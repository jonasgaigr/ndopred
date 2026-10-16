# Data tiers T2 and T3, and the choice of the trend estimate (review,
# Table 5 and section 7.3). T0, T1 and the effort layer are in trend.R.

# ------------------------------------------------------------------- T2

# Visits of the group around the taxon: one row per visit with its cell,
# year, whether the taxon was recorded and the list length (number of
# species of the group recorded on the visit).
.t2_visits <- function(taxon, effort, w, settings) {
  e <- effort[!effort$negative & effort$year >= w$comp_start & effort$year <= w$recent_end, , drop = FALSE]
  tc <- unique(e$cell[e$DRUH == taxon])
  if (!length(tc)) return(NULL)
  ij <- function(cells) do.call(rbind, lapply(strsplit(cells, "_", fixed = TRUE), as.numeric))
  t_ij <- ij(tc)
  all_cells <- unique(e$cell)
  a_ij <- ij(all_cells)
  b <- settings$t2_buffer_cells
  near <- vapply(seq_len(nrow(a_ij)), function(k) {
    any(abs(t_ij[, 1] - a_ij[k, 1]) <= b & abs(t_ij[, 2] - a_ij[k, 2]) <= b)
  }, logical(1))
  e <- e[e$cell %in% all_cells[near], , drop = FALSE]
  v <- stats::aggregate(list(det = e$DRUH == taxon), by = list(visit = e$visit), FUN = any)
  ll <- stats::aggregate(list(ll = e$DRUH), by = list(visit = e$visit), FUN = function(z) length(unique(z)))
  first <- e[!duplicated(e$visit), c("visit", "cell", "year")]
  merge(merge(first, v, by = "visit"), ll, by = "visit")
}

# T2: a multi-season occupancy-detection model (spOccupancy::tPGOcc) on the
# cells around the taxon: occupancy with a linear trend over years,
# detection with the visit's list length (review 7.3). The change is the
# posterior change in mean occupancy between the windows.
.t2_change <- function(taxon, effort, w, settings) {
  vis <- .t2_visits(taxon, effort, w, settings)
  if (is.null(vis) || !nrow(vis)) return(list(eligible = FALSE, n_repeat_sites = 0L))
  per <- stats::aggregate(list(n = vis$visit), by = list(cell = vis$cell, year = vis$year), FUN = length)
  rep_years <- stats::aggregate(list(y = per$n >= 2), by = list(cell = per$cell), FUN = sum)
  n_rep <- sum(rep_years$y >= 1)
  eligible <- n_rep >= settings$t2_min_sites &&
    length(unique(per$year[per$n >= 2])) >= settings$t2_min_years
  out <- list(eligible = eligible, n_repeat_sites = n_rep, n_visits = nrow(vis))
  if (!eligible || !isTRUE(settings$occupancy_model)) return(out)
  if (!requireNamespace("spOccupancy", quietly = TRUE)) {
    out$note <- "spOccupancy is not installed"
    return(out)
  }
  years <- seq(w$comp_start, w$recent_end)
  sites <- unique(vis$cell)
  K <- settings$t2_max_visits
  arrays <- .with_seed(settings$seed, {
    if (length(sites) > settings$t2_max_sites) sites <- sort(sample(sites, settings$t2_max_sites))
    y <- array(NA_real_, c(length(sites), length(years), K))
    ll <- array(NA_real_, dim(y))
    for (k in seq_len(nrow(per))) {
      s <- match(per$cell[k], sites)
      t <- match(per$year[k], years)
      if (is.na(s) || is.na(t)) next
      vv <- vis[vis$cell == per$cell[k] & vis$year == per$year[k], , drop = FALSE]
      if (nrow(vv) > K) vv <- vv[sample.int(nrow(vv), K), , drop = FALSE]
      y[s, t, seq_len(nrow(vv))] <- as.numeric(vv$det)
      ll[s, t, seq_len(nrow(vv))] <- log(vv$ll)
    }
    list(y = y, ll = ll, sites = sites)
  })
  y <- arrays$y
  ll <- arrays$ll
  sites <- arrays$sites
  yr <- matrix(as.numeric(scale(years)), length(sites), length(years), byrow = TRUE)
  llv <- ll[!is.na(ll)]
  ll <- (ll - mean(llv)) / max(stats::sd(llv), 1e-6)
  z0 <- apply(y, c(1, 2), function(a) if (all(is.na(a))) 0 else as.numeric(any(a > 0, na.rm = TRUE)))
  m <- settings$t2_mcmc
  fit <- tryCatch(.with_seed(settings$seed, spOccupancy::tPGOcc(
    occ.formula = ~ years, det.formula = ~ ll,
    data = list(y = y, occ.covs = list(years = yr), det.covs = list(ll = ll)),
    inits = list(beta = 0, alpha = 0, z = z0),
    n.batch = m$n.batch, batch.length = m$batch.length, n.burn = m$n.burn,
    n.thin = m$n.thin, n.chains = m$n.chains, verbose = FALSE
  )), error = function(e) e)
  if (inherits(fit, "error")) {
    out$note <- paste("occupancy model failed:", conditionMessage(fit))
    return(out)
  }
  # Mean occupancy probability over the sites, per posterior sample and year.
  psi <- apply(fit$psi.samples, c(1, 3), mean)
  in1 <- years >= w$comp_start & years <= w$comp_end
  in2 <- years >= w$recent_start & years <= w$recent_end
  change <- (rowMeans(psi[, in2, drop = FALSE]) / rowMeans(psi[, in1, drop = FALSE]) - 1) * 100
  out$change <- .q90(change)
  out$rhat <- suppressWarnings(max(unlist(fit$rhat), na.rm = TRUE))
  out$n_sites <- length(sites)
  if (is.finite(out$rhat) && out$rhat > 1.1) {
    out$note <- sprintf("the occupancy model has not converged (Rhat %.2f): more iterations needed", out$rhat)
  }
  out
}

# ------------------------------------------------------------------- T3

# The site of a record for T3: its NDOP locality.
.t3_site <- function(d) {
  if ("ID_LOKAL" %in% names(d)) as.character(d$ID_LOKAL) else as.character(d$NAZ_LOKAL)
}

# T3: an abundance index from standardized monitoring counts: a log-linear
# (quasi-)Poisson model of counts on site and year, TRIM-like, and the
# change over the gap between the window midpoints with a 90 % interval.
.t3_change <- function(occ, w, settings) {
  projs <- settings$monitoring_projects
  if (!length(projs) || !"PROJEKT" %in% names(occ) || !"POCET" %in% names(occ)) return(NULL)
  d <- if (inherits(occ, "sf")) sf::st_drop_geometry(occ) else occ
  d <- add_year_column(d)
  d$count <- suppressWarnings(as.numeric(as.character(d$POCET)))
  d$site <- .t3_site(d)
  neg <- if ("NEGATIV" %in% names(d)) suppressWarnings(as.numeric(d$NEGATIV)) %in% 1 else FALSE
  d$count[neg] <- 0   # a negative finding on a monitored site is a zero count
  d <- d[d$PROJEKT %in% projs & !is.na(d$count) & !is.na(d$year) & !is.na(d$site) &
           d$year >= w$comp_start & d$year <= w$recent_end, , drop = FALSE]
  # Only a site counted in two or more years tells about change; with a
  # site effect in the model, a site counted once carries no information on
  # the trend. Inventories and habitat mappings visit most sites once.
  years_per_site <- tapply(d$year, d$site, function(y) length(unique(y)))
  repeat_sites <- names(years_per_site)[years_per_site >= 2]
  n_all <- length(years_per_site)
  d <- d[d$site %in% repeat_sites, , drop = FALSE]
  out <- list(n_years = length(unique(d$year)), n_sites = length(repeat_sites),
              n_sites_all = n_all, sites = repeat_sites, rows = rownames(d))
  if (out$n_years < settings$t3_min_years || out$n_sites < settings$t3_min_sites) return(out)
  sy <- stats::aggregate(list(count = d$count), by = list(site = d$site, year = d$year), FUN = max)
  form <- if (length(unique(sy$site)) > 1) count ~ factor(site) + year else count ~ year
  fit <- tryCatch(stats::glm(form, family = stats::quasipoisson(), data = sy), error = function(e) NULL)
  if (is.null(fit) || !"year" %in% names(stats::coef(fit))) return(out)
  b <- stats::coef(fit)[["year"]]
  se <- sqrt(diag(stats::vcov(fit)))[["year"]]
  if (!is.finite(b) || !is.finite(se)) return(out)
  gap <- (w$recent_start + w$recent_end) / 2 - (w$comp_start + w$comp_end) / 2
  ch <- function(slope) (exp(slope * gap) - 1) * 100
  out$change <- c(min = ch(b - 1.645 * se), best = ch(b), max = ch(b + 1.645 * se))
  out$annual_rate <- b
  out
}

# ------------------------------------------------------- tier and estimate

#' Data Tier and Trend Estimate for One Taxon
#'
#' Classifies a taxon's data into the tiers of the review (Table 5) and
#' computes the trend estimate the tier allows:
#' \describe{
#'   \item{T3}{Counts from the monitoring projects (`monitoring_projects` in
#'     [trend_settings()]) at sites counted in two or more years, in enough
#'     years and sites: an abundance index, a
#'     log-linear model of counts on site and year. A potential basis for
#'     A2(b), and the only source of an estimated continuing decline (C1) and
#'     of an extreme-fluctuation diagnosis from records (Guidelines Table
#'     3.1, §4.7).}
#'   \item{T2}{Repeated visits of the group around the taxon (an effort layer
#'     from [ndop_effort()]): a multi-season occupancy-detection model
#'     ([spOccupancy::tPGOcc()]) with the visit's list length as the
#'     detection covariate, run when `occupancy_model = TRUE`. A trend in
#'     occupancy, not in mature individuals: basis (c) for A2 (review §7.3).}
#'   \item{T1}{An effort layer without the repeat visits T2 needs: relative
#'     occupancy on the cells recorded for the group in both windows, an
#'     explicitly labelled interim proxy.}
#'   \item{T0}{Records only: the raw change in AOO and in alpha-hull EOO
#'     (Guidelines §4.9; one point per occupied 2 x 2 km cell), a
#'     diagnostic, not a population trend. Its range comes from moving the
#'     comparison window `sensitivity_years` earlier and later (start-year
#'     sensitivity).}
#' }
#' A continuing decline is indicated only when the whole range of the change
#' (interval or start-year range) lies below `-decline_tolerance`, ruled out
#' when it lies above, and unknown otherwise.
#'
#' @param occ Records from [get_assessment_data()] (the included ones).
#' @param config Object from [ndopred_config()].
#' @param effort Optional effort layer for the taxon's group ([ndop_effort()]).
#' @param taxon The taxon name as in NDOP (needed for T2).
#' @return `trend_estimate()`: a list with `tier`, `tier_reason`, `change`
#'   (the chosen change in occupancy or abundance, min/best/max per cent),
#'   `eoo_change` (alpha-hull, T0 interval), `reduction`, `basis`, `source`,
#'   `decline_aoo`, `decline_eoo`, `sensitivity` and the per-tier details
#'   `t0`, `t1`, `t2`, `t3`. `data_tier()`: the tier and its reason.
#' @seealso [trend_settings()], [ndop_effort()]
#' @export
trend_estimate <- function(occ, config, effort = NULL, taxon = NA_character_) {
  s <- if (is.null(config$trend)) trend_settings() else config$trend
  w <- config$windows
  tol <- config$conventions$decline_tolerance
  occ <- add_year_column(occ)
  win <- function(a, b) {
    occ[!is.na(occ$year_min) & occ$year_min >= a & !is.na(occ$year_max) & occ$year_max <= b, ]
  }
  p1 <- win(w$comp_start, w$comp_end)
  p2 <- win(w$recent_start, w$recent_end)
  out <- list(tier = "T0", tier_reason = "records only: no effort layer for the group",
              change = NULL, eoo_change = NULL, reduction = NA_real_, basis = "c",
              source = NA_character_, decline_aoo = NA, decline_eoo = NA,
              sensitivity = NULL, t0 = NULL, t1 = NULL, t2 = NULL, t3 = NULL)
  if (!nrow(p1) || !nrow(p2)) {
    out$tier_reason <- "no records in one of the windows: no trend"
    return(out)
  }
  out$t0 <- tryCatch(.t0_change(occ, w, s), error = function(e) NULL)
  out$sensitivity <- out$t0
  if (!is.null(out$t0)) {
    out$change <- out$t0$aoo
    out$eoo_change <- out$t0$eoo
    out$source <- paste0("T0 diagnostic: raw AOO change between windows, not effort-corrected; ",
                         "range from comparison windows starting ", out$t0$starts[1], "-", out$t0$starts[2])
  }
  if (!is.null(effort) && nrow(effort)) {
    frac <- attr(effort, "sample_fraction")
    sampled <- if (is.null(frac)) "" else
      sprintf(" (effort layer from a random %.1f %% of cadastral units)", 100 * frac)
    out$t1 <- tryCatch(.t1_change(occ, effort, w, s), error = function(e) NULL)
    if (!is.null(out$t1$change)) {
      out$tier <- "T1"
      out$tier_reason <- paste0(sprintf("effort layer: %d cells recorded for the group in both windows",
                                 out$t1$n_benchmark), sampled)
      out$change <- out$t1$change
      out$source <- "T1 interim proxy: relative occupancy on cells recorded for the group in both windows"
    } else if (!is.null(out$t1)) {
      out$tier_reason <- sprintf(paste0("effort layer, but too few cells (%d recorded for the group in ",
                                        "both windows, %d with the taxon)"),
                                 out$t1$n_benchmark, out$t1$occupied[["comp"]])
    }
    if (!is.na(taxon)) {
      out$t2 <- tryCatch(.t2_change(taxon, effort, w, s), error = function(e) list(note = conditionMessage(e)))
      if (!is.null(out$t2$change)) {
        out$tier <- "T2"
        out$tier_reason <- paste0(sprintf("repeat visits at %d cells: occupancy-detection model",
                                   out$t2$n_repeat_sites), sampled)
        out$change <- out$t2$change
        out$source <- "T2 occupancy-detection model (a trend in occupancy, not in mature individuals)"
      } else if (isTRUE(out$t2$eligible) && !isTRUE(s$occupancy_model)) {
        out$tier_reason <- paste0(out$tier_reason, sprintf(
          "; T2 data (repeat visits at %d cells), occupancy model not run", out$t2$n_repeat_sites))
      }
    }
  }
  out$t3 <- tryCatch(.t3_change(occ, w, s), error = function(e) NULL)
  if (!is.null(out$t3$change)) {
    out$tier <- "T3"
    out$tier_reason <- sprintf("monitoring counts: %d sites counted in two or more years (of %d), %d years",
                               out$t3$n_sites, out$t3$n_sites_all, out$t3$n_years)
    out$change <- out$t3$change
    out$basis <- "b"
    out$source <- "T3 abundance index from standardized monitoring (log-linear model of counts)"
  }
  out$reduction <- .as_reduction(out$change)
  out$decline_aoo <- .decline_state(out$change, tol)
  out$decline_eoo <- .decline_state(out$eoo_change, tol)
  out
}

#' @rdname trend_estimate
#' @export
data_tier <- function(occ, config, effort = NULL, taxon = NA_character_) {
  te <- trend_estimate(occ, config, effort, taxon)
  list(tier = te$tier, reason = te$tier_reason)
}

# A short text for outputs: "-35 % (-48 to -20), T1".
.format_change <- function(change, tier) {
  if (is.null(change) || is.na(change[["best"]])) return(NA_character_)
  sprintf("%+.0f %% (%+.0f to %+.0f), %s", change[["best"]], change[["min"]], change[["max"]], tier)
}

.format_sensitivity <- function(sens) {
  if (is.null(sens) || is.na(sens$aoo[["best"]])) return(NA_character_)
  sprintf("AOO change %+.0f to %+.0f %% for comparison windows starting %d-%d",
          sens$aoo[["min"]], sens$aoo[["max"]], sens$starts[1], sens$starts[2])
}

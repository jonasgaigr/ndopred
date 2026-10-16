# The trend module: data tiers T0-T3 (review, Table 5) and the trend
# estimate each tier allows. Only an estimate from T2 or T3 data is a
# population trend; T0 and T1 give labelled diagnostics, which bulk mode may
# still use as indications (AOPK, D11), with "needs evidence" beside them.

#' Settings of the Trend Module
#'
#' Conventions for the data tiers and trend estimates (review, Table 5;
#' Guidelines v16 §3.2, §4.5, §4.7, §4.9). Each is a choice, not an IUCN
#' rule, and is stored with every run.
#'
#' @param reps Number of bootstrap samples for the T1 interval.
#' @param seed Seed of the resamplings (T1 bootstrap, T2 site sampling and
#'   MCMC), so results repeat exactly.
#' @param sensitivity_years The comparison window is also moved this many
#'   years earlier and later (start-year sensitivity; review §7.3). For T0
#'   the range of those changes is the interval of the change.
#' @param t1_min_cells,t1_min_occupied T1 needs at least this many cells
#'   recorded for the group in both windows, and the taxon in this many of
#'   them in the comparison window.
#' @param occupancy_model Logical. Fit the T2 occupancy-detection model
#'   (slow) where the data allow it. Otherwise T2 data are reported and the
#'   T1 indicator is used.
#' @param t2_min_sites,t2_min_years T2 needs at least `t2_min_sites` cells
#'   with two or more visits in a year, in at least `t2_min_years` years.
#' @param t2_buffer_cells Cells around the taxon's records that count as
#'   its area for the occupancy model.
#' @param t2_max_sites,t2_max_visits Caps on the model's size: sites are
#'   sampled down to `t2_max_sites`, visits per site and year to
#'   `t2_max_visits`.
#' @param t2_mcmc MCMC settings for [spOccupancy::tPGOcc()]: `n.batch`,
#'   `batch.length`, `n.burn`, `n.thin`, `n.chains`.
#' @param monitoring_projects Character vector of NDOP `PROJEKT` names whose
#'   counts follow a standardized protocol (data tier T3). Empty by default:
#'   AOPK names them.
#' @param t3_min_years,t3_min_sites T3 needs counts from those projects in
#'   at least this many years and sites.
#' @param alpha Alpha of the alpha-hull for the change in EOO (Guidelines
#'   §4.9).
#' @return A list of class `ndopred_trend_settings`.
#' @seealso [data_tier()], [trend_estimate()]
#' @export
trend_settings <- function(reps = 199, seed = 1, sensitivity_years = 2,
                           t1_min_cells = 30, t1_min_occupied = 5,
                           occupancy_model = FALSE, t2_min_sites = 20, t2_min_years = 5,
                           t2_buffer_cells = 5, t2_max_sites = 400, t2_max_visits = 6,
                           t2_mcmc = list(n.batch = 400, batch.length = 25, n.burn = 5000,
                                          n.thin = 5, n.chains = 2),
                           monitoring_projects = character(0),
                           t3_min_years = 5, t3_min_sites = 3, alpha = 2) {
  structure(as.list(environment()), class = "ndopred_trend_settings")
}

# Run `expr` with a fixed seed and restore the caller's random state.
.with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    } else assign(".Random.seed", old, envir = globalenv())
  })
  set.seed(seed)
  expr
}

# A range from resampled values: 5th, 50th and 95th percentiles (a 90 %
# interval, Guidelines 3.2.4).
.q90 <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) < 3) return(c(min = NA_real_, best = NA_real_, max = NA_real_))
  q <- stats::quantile(v, c(0.05, 0.5, 0.95), names = FALSE)
  c(min = q[1], best = q[2], max = q[3])
}

# A continuing decline from a range of percentage changes: TRUE when even
# the highest plausible change is a decline beyond the tolerance, FALSE when
# even the lowest is not, otherwise unknown.
.decline_state <- function(change, tolerance) {
  if (is.null(change) || is.na(change[["best"]])) return(NA)
  if (change[["max"]] < -tolerance) return(TRUE)
  if (change[["min"]] > -tolerance) return(FALSE)
  NA
}

# A reduction (positive per cent) from a range of changes.
.as_reduction <- function(change) {
  if (is.null(change) || is.na(change[["best"]])) return(NA_real_)
  c(min = max(0, -change[["max"]]), best = max(0, -change[["best"]]), max = max(0, -change[["min"]]))
}

.pct_change <- function(before, after) {
  if (is.na(before) || is.na(after) || before <= 0) return(NA_real_)
  (after - before) / before * 100
}

# ------------------------------------------------------------------- T0

# Point coordinates in EPSG:3035 (record centroids).
.xy3035 <- function(occ) {
  if (is.null(occ) || !nrow(occ)) return(matrix(numeric(0), 0, 2))
  occ <- occ[!sf::st_is_empty(occ), ]
  if (!nrow(occ)) return(matrix(numeric(0), 0, 2))
  g <- sf::st_geometry(occ)
  if (!identical(sf::st_crs(g)$epsg, 3035L)) g <- sf::st_transform(g, 3035)
  if (!all(sf::st_geometry_type(g) == "POINT")) g <- suppressWarnings(sf::st_centroid(g))
  sf::st_coordinates(g)[, 1:2, drop = FALSE]
}

# Occupied 2 x 2 km cells (km2) at a given grid origin.
.aoo_at <- function(xy, origin, grid = 2000) {
  if (!nrow(xy)) return(0)
  length(unique(paste(floor((xy[, 1] - origin[1]) / grid), floor((xy[, 2] - origin[2]) / grid)))) *
    (grid / 1000)^2
}

# The grid origin (of a 4 x 4 lattice) giving the fewest occupied cells,
# as calculate_aoo() does (Guidelines 4.10.2).
.min_origin <- function(xy, grid = 2000, n = 4) {
  off <- seq(0, grid, length.out = n + 1)[-(n + 1)]
  grid_o <- expand.grid(x = off, y = off)
  a <- vapply(seq_len(nrow(grid_o)), function(i) .aoo_at(xy, c(grid_o$x[i], grid_o$y[i]), grid), numeric(1))
  unlist(grid_o[which.min(a), ])
}

# One point per occupied 2 x 2 km cell (its centre): the alpha-hull then
# follows the range, not the density of recording.
.thin_cells <- function(xy, grid = 2000) {
  if (!nrow(xy)) return(xy)
  ij <- unique(cbind(floor(xy[, 1] / grid), floor(xy[, 2] / grid)))
  cbind(ij[, 1] * grid + grid / 2, ij[, 2] * grid + grid / 2)
}

# T0 with start-year sensitivity: the raw change in AOO (one grid origin
# shared by both windows) and in alpha-hull EOO (occupied cells; Guidelines
# 4.9) for the comparison window as set and moved up to `k` years earlier
# and later (review 7.3; Fox et al. 2019). The set window gives the best
# value; the moved ones give the range. Records only: a diagnostic, not a
# population trend (review, Table 5).
.t0_change <- function(occ, w, settings) {
  k <- max(0, if (is.null(settings$sensitivity_years)) 0 else settings$sensitivity_years)
  occ <- add_year_column(occ)
  win <- function(a, b) {
    .xy3035(occ[!is.na(occ$year_min) & occ$year_min >= a & !is.na(occ$year_max) & occ$year_max <= b, ])
  }
  x2 <- win(w$trend_start, w$trend_end)
  if (!nrow(x2)) return(NULL)
  h2 <- .alpha_hull_km2(.thin_cells(x2), settings$alpha)
  res <- lapply(seq(-k, k), function(off) {
    x1 <- win(w$comp_start + off, w$comp_end + off)
    if (!nrow(x1)) return(c(offset = off, aoo = NA, eoo = NA))
    o <- .min_origin(rbind(x1, x2))
    c(offset = off, aoo = .pct_change(.aoo_at(x1, o), .aoo_at(x2, o)),
      eoo = .pct_change(.alpha_hull_km2(.thin_cells(x1), settings$alpha), h2))
  })
  d <- as.data.frame(do.call(rbind, res))
  set <- d[d$offset == 0, ]
  rng3 <- function(v, best) {
    if (is.na(best)) return(c(min = NA_real_, best = NA_real_, max = NA_real_))
    c(min = min(v, na.rm = TRUE), best = best, max = max(v, na.rm = TRUE))
  }
  list(aoo = rng3(d$aoo, set$aoo), eoo = rng3(d$eoo, set$eoo), table = d,
       starts = c(w$comp_start - k, w$comp_start + k))
}

# --------------------------------------------------------- the effort layer

# The 2 x 2 km cell of each point, on a fixed grid (origin 0, 0) in
# EPSG:3035, shared by the taxon and the effort layer.
.cell_id <- function(x, y) paste(floor(x / 2000), floor(y / 2000), sep = "_")

.cells_of <- function(occ) {
  if (is.null(occ) || !nrow(occ)) return(character(0))
  occ <- occ[!sf::st_is_empty(occ), ]
  if (!nrow(occ)) return(character(0))
  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(sf::st_transform(occ, 3035))))
  .cell_id(xy[, 1], xy[, 2])
}

#' Recording Effort for a Taxonomic Group
#'
#' Data tiers T1 and T2 compare a taxon with the recording of its whole
#' group (review, Table 5): which 2 x 2 km cells the group was recorded in,
#' and which visits recorded which species. `ndop_effort()` downloads every
#' record of an NDOP taxonomic group for a span of years, year by year (and
#' month by month where a year is too large), and keeps per record only the
#' taxon name, date, year, 2 x 2 km cell (EPSG:3035, fixed origin), a visit
#' number (date, locality and observer; no names are kept), the negative
#' flag and the project. The layer is saved as RDS and read back with
#' `read_effort()` or `effort_layer()`. A download is resumable: years
#' already saved are not fetched again.
#'
#' Large groups (beetles, butterflies and moths, vascular plants, birds) hold
#' hundreds of thousands to millions of records; their first download takes
#' hours.
#'
#' @param group The group: an NDOP name (`KAT_TAX`, e.g. "Obojživelníci"),
#'   Latin (e.g. "Amphibia") or English name; see [generation_lengths()].
#' @param years The first and last year.
#' @param dir Folder for the layer; defaults to [default_snapshot_dir()].
#' @param download Function used to download one chunk (for tests).
#' @param units Optional character vector of NDOP cadastral units (see
#'   [cadastral_units()]): the layer then holds every record of the group in
#'   those units only, a random spatial sample. T1 compares occupancy on the
#'   cells recorded for the group in both windows, and T2 takes its sites and
#'   visits from the layer, so both stay within the sample. The units and
#'   their share are kept as attributes `sample_units` and
#'   `sample_fraction`.
#' @return `ndop_effort()` and `read_effort()`: a data frame of class
#'   `ndopred_effort` with attributes `group`, `years` and `downloaded`.
#'   `effort_layer()`: the newest layer for a group in `dir`, or `NULL`.
#' @seealso [trend_estimate()]
#' @export
ndop_effort <- function(group, years = c(2000, 2026), dir = default_snapshot_dir(),
                        download = NULL, units = NULL) {
  g <- generation_length_for(group)
  tab <- generation_lengths()
  lat <- tab$group_lat[match(g$group, tab$kat_tax)]
  if (is.na(lat)) stop("Unknown NDOP group: ", group, call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, sprintf("effort_%s_%d-%d.rds", gsub("[^A-Za-z]+", "_", lat), years[1], years[2]))
  done <- if (file.exists(path)) readRDS(path) else NULL
  have <- if (is.null(done)) integer(0) else attr(done, "years_done")
  default_dl <- is.null(download)
  if (default_dl) download <- function(lat, from, to, max_n = Inf) .download_chunk(lat, from, to, max_n, units)
  sample <- if (is.null(units)) NULL else
    list(units = units, fraction = length(units) / length(cadastral_units()))
  parts <- if (is.null(done)) list() else list(as.data.frame(done)[, setdiff(names(done), "visit"), drop = FALSE])
  # A small group, or a sample of units, comes in one request for the
  # whole span.
  if (!length(have) && default_dl) {
    whole <- download(lat, sprintf("1.1.%d", years[1]), sprintf("31.12.%d", years[2]),
                      max_n = if (is.null(units)) 20000 else Inf)
    if (!is.null(whole)) {
      if (nrow(whole)) parts[[1]] <- .effort_rows(whole)
      have <- seq(years[1], years[2])
      out <- .as_effort(parts, lat, g$group, years, have, sample)
      saveRDS(out, path)
    }
  }
  for (y in setdiff(seq(years[1], years[2]), have)) {
    chunk <- download(lat, sprintf("1.1.%d", y), sprintf("31.12.%d", y))
    complete <- !is.null(chunk)
    if (!complete) {
      # Too large or failed: month by month.
      ends <- c(31, 28 + (y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      months <- lapply(1:12, function(mo) {
        download(lat, sprintf("1.%d.%d", mo, y), sprintf("%d.%d.%d", ends[mo], mo, y))
      })
      complete <- !any(vapply(months, is.null, logical(1)))
      chunk <- do.call(rbind, months)
    }
    if (!complete) {
      # A year is done only when all of it came; it is fetched again next time.
      message("ndop_effort: ", lat, " ", y, " incomplete; will be retried on the next run")
      next
    }
    if (!is.null(chunk) && nrow(chunk)) parts[[length(parts) + 1]] <- .effort_rows(chunk)
    have <- c(have, y)
    out <- .as_effort(parts, lat, g$group, years, have, sample)
    saveRDS(out, path)
  }
  out <- .as_effort(parts, lat, g$group, years, have, sample)
  attr(out, "path") <- path
  out
}

#' @rdname ndop_effort
#' @param path Path of a saved layer.
#' @export
read_effort <- function(path) {
  out <- readRDS(path)
  if (!inherits(out, "ndopred_effort")) stop("Not an effort layer: ", path, call. = FALSE)
  attr(out, "path") <- path
  out
}

#' @rdname ndop_effort
#' @export
effort_layer <- function(group, dir = default_snapshot_dir()) {
  if (is.na(group) || !nzchar(group) || !dir.exists(dir)) return(NULL)
  g <- generation_length_for(group)
  tab <- generation_lengths()
  lat <- tab$group_lat[match(g$group, tab$kat_tax)]
  if (is.na(lat)) return(NULL)
  files <- sort(list.files(dir, pattern = paste0("^effort_", gsub("[^A-Za-z]+", "_", lat), "_.*\\.rds$"),
                           full.names = TRUE))
  if (!length(files)) return(NULL)
  read_effort(files[length(files)])
}

# One chunk from NDOP (rndop): NULL when the query fails or is too large.
# NULL when the request fails or holds more than `max_n` records.
.download_chunk <- function(lat, from, to, max_n = Inf, units = NULL) {
  p <- utils::capture.output(
    payload <- rndop::set_search_payload(rfKategorie = paste0("^", lat, "$"), rfDatumOd = from, rfDatumDo = to)
  )
  if (length(units)) payload$rfKatastr <- paste(units, collapse = " || ")
  # Two attempts: the NDOP server drops a request now and then.
  for (attempt in 1:2) {
    n <- .ndop_count(payload)
    if (is.na(n)) next
    if (n == 0) return(data.frame())
    if (n > max_n) return(NULL)
    tab <- tryCatch(suppressMessages(rndop::ndop_download(search_payload = payload)), error = function(e) NULL)
    if (is.data.frame(tab) && nrow(tab) > 0) return(tab)
  }
  NULL
}

# The number of records a search finds. rndop reads it from the text
# "zaznam ze N" and gives NA both for no records and for a failed request;
# here an empty result (the results table without a count) is 0, an
# invalid session (another login with the same account) logs in again, and
# anything else is NA.
.ndop_count <- function(payload) {
  url <- "https://portal23.nature.cz/nd/find.php?akce=seznam&opener=&vztazne_id=0"
  ua <- get("USER_AGENT", envir = asNamespace("rndop"))
  for (attempt in 1:2) {
    if (!exists("isop_loginhash", envir = globalenv()) ||
        is.na(get("isop_loginhash", envir = globalenv()))) {
      try(suppressMessages(rndop::isop_login()), silent = TRUE)
    }
    hash <- get0("isop_loginhash", envir = globalenv(), ifnotfound = NA)
    page <- tryCatch({
      for (i in 1:2) {   # as rndop: the second response carries the results
        res <- httr::POST(url, body = payload, config = httr::set_cookies(isop_loginhash = hash),
                          httr::user_agent(ua))
      }
      httr::content(res, as = "text", encoding = "UTF-8")
    }, error = function(e) NA_character_)
    if (is.na(page)) return(NA_real_)
    if (grepl("ISOP autentizace", page, fixed = TRUE)) {
      assign("isop_loginhash", NA, envir = globalenv())
      next
    }
    m <- regmatches(page, regexpr("z\u00e1znam ze [0-9]+", page))
    if (length(m)) return(as.numeric(sub("^.* ", "", m)))
    if (grepl("Hromadn\u00e1 validace", page, fixed = TRUE)) return(0)
    return(NA_real_)
  }
  NA_real_
}

# Keep only what effort needs; observers become part of an anonymous visit
# key and are not stored.
.effort_rows <- function(tab) {
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  x <- num(tab$X)
  y <- num(tab$Y)
  ok <- !is.na(x) & !is.na(y)
  tab <- tab[ok, , drop = FALSE]
  if (!nrow(tab)) return(NULL)
  pts <- sf::st_transform(sf::st_as_sf(data.frame(x = x[ok], y = y[ok]), coords = c("x", "y"), crs = 5514), 3035)
  xy <- sf::st_coordinates(pts)
  date <- num(tab$DATUM_OD)
  observer <- if ("AUTOR" %in% names(tab)) as.character(tab$AUTOR) else ""
  key <- paste(date, tab$ID_LOKAL, vapply(observer, .str_hash, numeric(1), USE.NAMES = FALSE))
  data.frame(
    ID_NALEZ = num(tab$ID_NALEZ),
    DRUH = as.character(tab$DRUH),
    date = date,
    year = floor(date / 10000),
    cell = .cell_id(xy[, 1], xy[, 2]),
    visit_key = key,
    negative = num(tab$NEGATIV) %in% 1,
    project = if ("PROJEKT" %in% names(tab)) as.character(tab$PROJEKT) else NA_character_,
    stringsAsFactors = FALSE
  )
}

# A short numeric code for a string (observer names are not kept).
.str_hash <- function(s) {
  v <- utf8ToInt(enc2utf8(if (is.na(s)) "" else s))
  if (!length(v)) return(0)
  sum(v * seq_along(v) * 31) %% 2147483647
}

.as_effort <- function(parts, lat, kat, years, have, sample = NULL) {
  d <- if (length(parts)) do.call(rbind, parts) else
    data.frame(ID_NALEZ = numeric(0), DRUH = character(0), date = numeric(0), year = numeric(0),
               cell = character(0), visit_key = character(0), negative = logical(0),
               project = character(0))
  # Records dated as ranges come in several chunks once.
  if (nrow(d) && any(!is.na(d$ID_NALEZ))) d <- d[!duplicated(d$ID_NALEZ) | is.na(d$ID_NALEZ), , drop = FALSE]
  # The anonymous visit key (date, locality, observer code) is kept, so a
  # layer can be extended; `visit` numbers the visits.
  d$visit <- as.integer(factor(d$visit_key))
  structure(d, class = c("ndopred_effort", "data.frame"), group = kat, group_lat = lat,
            sample_units = sample$units, sample_fraction = sample$fraction,
            years = years, years_done = sort(unique(have)),
            downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# ------------------------------------------------------------------- T1

# T1: relative occupancy on the cells recorded for the group in both
# windows (Isaac et al. 2014, "well-sampled sites"): the share of those
# cells holding the taxon, in each window, and its change; a bootstrap over
# the cells gives the interval.
.t1_change <- function(occ, effort, w, settings) {
  pos <- effort[!effort$negative, , drop = FALSE]
  in_w <- function(d, a, b) d$year >= a & d$year <= b
  surveyed1 <- unique(pos$cell[in_w(pos, w$comp_start, w$comp_end)])
  surveyed2 <- unique(pos$cell[in_w(pos, w$recent_start, w$recent_end)])
  bench <- intersect(surveyed1, surveyed2)
  occ <- add_year_column(occ)
  win <- function(a, b) occ[!is.na(occ$year_min) & occ$year_min >= a & !is.na(occ$year_max) & occ$year_max <= b, ]
  c1 <- intersect(unique(.cells_of(win(w$comp_start, w$comp_end))), bench)
  c2 <- intersect(unique(.cells_of(win(w$recent_start, w$recent_end))), bench)
  out <- list(n_benchmark = length(bench), occupied = c(comp = length(c1), recent = length(c2)))
  if (length(bench) < settings$t1_min_cells || length(c1) < settings$t1_min_occupied) {
    out$change <- NULL
    return(out)
  }
  o1 <- bench %in% c1
  o2 <- bench %in% c2
  best <- .pct_change(mean(o1), mean(o2))
  boots <- .with_seed(settings$seed, vapply(seq_len(max(settings$reps, 99)), function(r) {
    i <- sample.int(length(bench), replace = TRUE)
    .pct_change(mean(o1[i]), mean(o2[i]))
  }, numeric(1)))
  q <- .q90(boots)
  out$change <- c(min = q[["min"]], best = best, max = q[["max"]])
  out
}

#' NDOP Cadastral Units
#'
#' The cadastral units NDOP can filter by (from rndop), as the values a
#' search takes, e.g. "600016: Abertamy". Used to draw a random spatial
#' sample of a large group's effort layer ([ndop_effort()]).
#'
#' @return A character vector.
#' @export
cadastral_units <- function() {
  a <- get("area_list", envir = asNamespace("rndop"))
  a$payload_val[a$type == "KU"]
}

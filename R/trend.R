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
#' @return `ndop_effort()` and `read_effort()`: a data frame of class
#'   `ndopred_effort` with attributes `group`, `years` and `downloaded`.
#'   `effort_layer()`: the newest layer for a group in `dir`, or `NULL`.
#' @seealso [trend_estimate()]
#' @export
ndop_effort <- function(group, years = c(2000, 2026), dir = default_snapshot_dir(),
                        download = NULL) {
  g <- generation_length_for(group)
  tab <- generation_lengths()
  lat <- tab$group_lat[match(g$group, tab$kat_tax)]
  if (is.na(lat)) stop("Unknown NDOP group: ", group, call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, sprintf("effort_%s_%d-%d.rds", gsub("[^A-Za-z]+", "_", lat), years[1], years[2]))
  done <- if (file.exists(path)) readRDS(path) else NULL
  have <- if (is.null(done)) integer(0) else attr(done, "years_done")
  if (is.null(download)) download <- .download_chunk
  parts <- if (is.null(done)) list() else list(as.data.frame(done)[, setdiff(names(done), "visit"), drop = FALSE])
  for (y in setdiff(seq(years[1], years[2]), have)) {
    chunk <- download(lat, sprintf("1.1.%d", y), sprintf("31.12.%d", y))
    if (is.null(chunk)) {
      # Too large or failed: month by month.
      ends <- c(31, 28 + (y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      chunk <- do.call(rbind, lapply(1:12, function(mo) {
        download(lat, sprintf("1.%d.%d", mo, y), sprintf("%d.%d.%d", ends[mo], mo, y))
      }))
    }
    if (!is.null(chunk) && nrow(chunk)) parts[[length(parts) + 1]] <- .effort_rows(chunk)
    have <- c(have, y)
    out <- .as_effort(parts, lat, g$group, years, have)
    saveRDS(out, path)
  }
  out <- .as_effort(parts, lat, g$group, years, have)
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
.download_chunk <- function(lat, from, to) {
  p <- rndop::set_search_payload(rfKategorie = paste0("^", lat, "$"), rfDatumOd = from, rfDatumDo = to)
  n <- tryCatch(suppressMessages(rndop::ndop_download(search_payload = p, num_rec_only = TRUE)),
                error = function(e) NA)
  if (is.na(n)) return(NULL)
  if (n == 0) return(data.frame())
  tab <- tryCatch(suppressMessages(rndop::ndop_download(search_payload = p)), error = function(e) NULL)
  if (is.data.frame(tab)) tab else NULL
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

.as_effort <- function(parts, lat, kat, years, have) {
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

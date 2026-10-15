#' Calculate Spatial Trend (Recent Window vs. Preceding Window)
#'
#' Compares AOO and EOO between a recent window and an earlier comparison
#' window. The resulting change is what criterion A2(c) calls "a decline in
#' area of occupancy, extent of occurrence and/or quality of habitat", and
#' what criteria B1b(i)/B2b(ii) call a continuing decline in EOO/AOO.
#'
#' @section Window handling:
#' By default the two windows are contiguous and non-overlapping: the recent
#' window is the last \code{window_years} years, and the comparison window is
#' the \code{window_years} years immediately before it. The previous defaults
#' (recent 2010-2026 vs. comparison 2000-2016) overlapped by seven years,
#' which damped every measured change towards zero. Explicit
#' \code{recent_start}/\code{recent_end}/\code{comp_start}/\code{comp_end}
#' still override the derived windows.
#'
#' @section Comparable grid origins:
#' AOO for both periods is computed on a single shared grid origin. IUCN
#' Guidelines section 4.10.2 asks for the *minimum* estimate over grid
#' origins, but the minimising origin generally differs between periods; using
#' each period's own minimum would compare two different grids and manufacture
#' spurious change. The origin that minimises AOO over the pooled data of both
#' periods is chosen and then applied to each period.
#'
#' @param occurrence_data Data frame or sf object (must contain DATUM_OD, ROK
#'   or a 'year' column).
#' @param window_years Numeric. Length in years of each window (default 10).
#' @param year_now Numeric. Final year of the recent window: the reference
#'   year. Required unless `recent_end` is given; the system clock is never
#'   read.
#' @param recent_start,recent_end,comp_start,comp_end Numeric or NULL.
#'   Explicit window bounds; override the derived windows when supplied.
#' @param grid_size Numeric. AOO cell size in metres (default 2000).
#' @param alpha Numeric. The alpha of the alpha-hull used for the change in
#'   EOO (Guidelines 4.9; default 2), see [calculate_eoo_alpha()].
#' @return A list with the window definitions, AOO and EOO for both periods,
#'   \code{percent_change} (AOO, retained for backward compatibility),
#'   \code{aoo_percent_change}, \code{eoo_percent_change} (from alpha-hulls),
#'   \code{eoo_mcp_percent_change} (from the convex hulls, a diagnostic) and
#'   the record counts underlying each period. A change is \code{NA} when the
#'   earlier period has no records to compare against. These are raw
#'   changes: recording effort is not accounted for (data tier T0; see
#'   [data_tier()]).
#' @seealso [calculate_aoo()], [summarize_assessment()], [calculate_pop_metrics()]
#'   for the abundance-based counterpart.
#' @references IUCN Standards and Petitions Committee (2024), sections 4.6
#'   (continuing decline), 4.9 (comparing temporal EOO estimates) and 4.10.2.
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     x = c(4600000, 4700000, 4650000, 4600000, 4610000, 4605000),
#'     y = c(2900000, 2900000, 2990000, 2900000, 2900000, 2909000),
#'     # three records in the comparison window, three in the recent one
#'     DATUM_OD = c("20100615", "20110615", "20120615",
#'                  "20200615", "20210615", "20220615")
#'   ),
#'   coords = c("x", "y"), crs = 3035
#' )
#'
#' tr <- calculate_trend(pts, window_years = 10, year_now = 2025)
#' tr$period_1_range   # "2006 - 2015"
#' tr$period_2_range   # "2016 - 2025"
#' tr$aoo_percent_change
#' tr$eoo_percent_change
#' @export
calculate_trend <- function(occurrence_data,
                            window_years = 10,
                            year_now = NULL,
                            recent_start = NULL, recent_end = NULL,
                            comp_start = NULL, comp_end = NULL,
                            grid_size = 2000, alpha = 2) {

  # Derive contiguous, non-overlapping windows unless told otherwise. The
  # reference year is always explicit: no system clock in calculations.
  if (is.null(recent_end)) {
    if (is.null(year_now)) stop("Give `year_now` (the reference year) or `recent_end`.", call. = FALSE)
    recent_end <- year_now
  }
  if (is.null(recent_start)) recent_start <- recent_end - window_years + 1
  if (is.null(comp_end))     comp_end     <- recent_start - 1
  if (is.null(comp_start))   comp_start   <- comp_end - window_years + 1

  # Overlapping windows are allowed (the AOPK default, decision D5); the
  # records of the shared years count in both, which damps the change.

  occurrence_data <- add_year_column(occurrence_data)

  # A record belongs to a window only if its whole date range does.
  in_window <- function(a, b) {
    keep <- !is.na(occurrence_data$year_min) & occurrence_data$year_min >= a &
      !is.na(occurrence_data$year_max) & occurrence_data$year_max <= b
    occurrence_data[keep, , drop = FALSE]
  }
  p1_data <- in_window(comp_start, comp_end)
  p2_data <- in_window(recent_start, recent_end)

  # Shared grid origin, minimised over the pooled points of both windows.
  # Pooling two possibly-empty sf subsets recomputes a bounding box over zero
  # points, which emits benign min/max warnings; an empty window is normal.
  pooled <- suppressWarnings(rbind(p1_data, p2_data))
  shared_origin <- NULL
  if (nrow(pooled) > 0) {
    shared_origin <- suppressWarnings(
      calculate_aoo(pooled, grid_size = grid_size)$origin
    )
  }

  aoo_p1 <- suppressWarnings(
    calculate_aoo(p1_data, grid_size = grid_size, origin = shared_origin)$area_km2)
  aoo_p2 <- suppressWarnings(
    calculate_aoo(p2_data, grid_size = grid_size, origin = shared_origin)$area_km2)

  eoo_p1 <- calculate_eoo(p1_data)$area_km2
  eoo_p2 <- calculate_eoo(p2_data)$area_km2

  # IUCN 4.9: EOO is never smaller than AOO.
  eoo_p1 <- reconcile_eoo_aoo(eoo_p1, aoo_p1)
  eoo_p2 <- reconcile_eoo_aoo(eoo_p2, aoo_p2)

  pct_change <- function(before, after) {
    if (is.na(before) || is.na(after) || before <= 0) return(NA_real_)
    ((after - before) / before) * 100
  }

  aoo_change <- pct_change(aoo_p1, aoo_p2)
  eoo_mcp_change <- pct_change(eoo_p1, eoo_p2)
  # Guidelines 4.9: change in EOO is measured with an alpha-hull (alpha = 2),
  # less biased by outlying records than the convex hull of criterion B1.
  alpha_p1 <- calculate_eoo_alpha(p1_data, alpha)
  alpha_p2 <- calculate_eoo_alpha(p2_data, alpha)
  eoo_change <- pct_change(alpha_p1, alpha_p2)

  return(list(
    range = paste(comp_start, "-", recent_end),
    period_1_range = paste(comp_start, "-", comp_end),
    period_2_range = paste(recent_start, "-", recent_end),
    period_1_aoo = aoo_p1,
    period_2_aoo = aoo_p2,
    period_1_eoo = eoo_p1,
    period_2_eoo = eoo_p2,
    period_1_n = nrow(p1_data),
    period_2_n = nrow(p2_data),
    window_years = window_years,
    period_1_eoo_alpha = alpha_p1,
    period_2_eoo_alpha = alpha_p2,
    percent_change = aoo_change,
    aoo_percent_change = aoo_change,
    eoo_percent_change = eoo_change,
    eoo_mcp_percent_change = eoo_mcp_change
  ))
}

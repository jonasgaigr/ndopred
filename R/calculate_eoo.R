#' Calculate EOO with Temporal Filter
#'
#' Minimum convex polygon over the occurrence points, per IUCN Guidelines
#' (v16, 2024) section 4.9. Note that section 4.9 explicitly discourages
#' excluding discontinuities or disjunctions from EOO for criterion B1, which
#' is why no outlier trimming is applied here.
#'
#' @param occurrence_data sf object
#' @param year_start Numeric. Only records from this year onwards.
#' @param year_end Numeric. Only records up to and including this year.
#' @param date_col Character. The name of the date column (default is "DATUM_OD").
#' @param precision Numeric vector of positional precision in metres, or
#'   `NULL` to read `precision_m` / `CXPRESNOST` (0 = exact).
#' @param geometries Optional `sf` layer of record polygons and lines keyed by
#'   `ID_LOKAL` (see [calculate_aoo()]).
#' @section Bounds:
#' `area_km2_min` leaves out records less precise than 2 km and retained
#' less-credible records (`flag_less_credible`); `area_km2_max` is the hull of
#' every record's precision disc and polygon (Guidelines §3.2, §4.9). Both are
#' `NA` where fewer than three positions remain.
#' @return A list with \code{area_km2} (\code{NA} when fewer than 3 points are
#'   available, or when the hull is degenerate; callers should reconcile
#'   against AOO via \code{\link{reconcile_eoo_aoo}}), \code{area_km2_min},
#'   \code{area_km2_max}, and \code{geom}, the hull as an \code{sf} geometry.
#' @seealso [calculate_aoo()], [reconcile_eoo_aoo()]
#' @references IUCN Standards and Petitions Committee (2024), section 4.9.
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     x = c(4600000, 4700000, 4650000, 4620000),
#'     y = c(2900000, 2900000, 2990000, 2930000),
#'     DATUM_OD = c("20200615", "20210615", "20220615", "20120615")
#'   ),
#'   coords = c("x", "y"), crs = 3035
#' )
#' calculate_eoo(pts)$area_km2
#'
#' # Fewer than three points: NA, not zero. AOO then stands in for EOO.
#' calculate_eoo(pts[1:2, ])$area_km2
#' reconcile_eoo_aoo(calculate_eoo(pts[1:2, ])$area_km2, aoo_km2 = 8)
#' @export
calculate_eoo <- function(occurrence_data, year_start = NULL, date_col = "DATUM_OD",
                          year_end = NULL, precision = NULL, geometries = NULL) {

  # 1. Temporal Filter; records without a position cannot shape a hull.
  occurrence_data <- filter_years(occurrence_data, year_start, year_end, date_col)
  if (nrow(occurrence_data) > 0) {
    occurrence_data <- suppressWarnings(occurrence_data[!sf::st_is_empty(occurrence_data), ])
  }

  # IUCN section 4.9: EOO must never be smaller than AOO. With fewer than 3
  # points a convex hull has zero area, so EOO is returned as NA and
  # reconcile_eoo_aoo() substitutes AOO.
  if (nrow(occurrence_data) < 3) {
    bounds <- .eoo_bounds(occurrence_data, precision, geometries)
    return(list(area_km2 = NA_real_, area_km2_min = NA_real_,
                area_km2_max = bounds$max, geom = NULL))
  }

  # 2. Calculate Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  # 3. Calculate Area using the 'units' package to ensure safe conversion to km2
  area_units <- sf::st_area(hull)
  area_km2 <- as.numeric(units::set_units(area_units, "km^2"))
  bounds <- .eoo_bounds(occurrence_data, precision, geometries)

  # Three or more collinear points (or points inside a single cell) yield a
  # degenerate hull; treat it as "not computable" so AOO takes over.
  if (!is.finite(area_km2) || area_km2 <= 0) {
    return(list(area_km2 = NA_real_, area_km2_min = bounds$min,
                area_km2_max = bounds$max, geom = hull))
  }

  return(list(area_km2 = area_km2, area_km2_min = bounds$min,
              area_km2_max = bounds$max, geom = hull))
}

# Convex-hull area (km2) of a coordinate matrix in metres; NA if degenerate.
.hull_km2 <- function(xy) {
  xy <- unique(xy[stats::complete.cases(xy), , drop = FALSE])
  if (nrow(xy) < 3) return(NA_real_)
  h <- xy[grDevices::chull(xy), , drop = FALSE]
  a <- abs(sum(h[, 1] * c(h[-1, 2], h[1, 2]) - c(h[-1, 1], h[1, 1]) * h[, 2])) / 2 / 1e6
  if (is.finite(a) && a > 0) a else NA_real_
}

.eoo_bounds <- function(occ, precision = NULL, geometries = NULL) {
  if (nrow(occ) == 0) return(list(min = NA_real_, max = NA_real_))
  xy <- sf::st_coordinates(occ)[, 1:2, drop = FALSE]
  r <- .record_precision(occ, precision)
  # Lower bound: precise and credible positions only.
  less <- if ("flag_less_credible" %in% names(occ)) occ$flag_less_credible %in% TRUE else FALSE
  keep <- r <= 2000 & !less
  lo <- .hull_km2(xy[keep, , drop = FALSE])
  # Upper bound: each position widened to its precision disc (16 vertices,
  # circumscribing the circle), plus the vertices of record polygons.
  ang <- seq(0, 2 * pi, length.out = 17)[-17]
  rr <- r / cos(pi / 16)
  disc <- cbind(rep(xy[, 1], each = 16) + rep(rr, each = 16) * cos(ang),
                rep(xy[, 2], each = 16) + rep(rr, each = 16) * sin(ang))
  pts <- rbind(xy, disc)
  if (!is.null(geometries) && "ID_LOKAL" %in% names(occ) && nrow(geometries) > 0) {
    ids <- suppressWarnings(as.numeric(as.character(occ$ID_LOKAL)))
    gm <- geometries[geometries$ID_LOKAL %in% ids, ]
    if (nrow(gm)) {
      gm <- sf::st_transform(gm, sf::st_crs(occ))
      # Vertices geometry by geometry: st_coordinates() refuses a layer that
      # mixes points, lines and polygons.
      verts <- lapply(sf::st_geometry(gm), function(g) sf::st_coordinates(g)[, 1:2, drop = FALSE])
      pts <- rbind(pts, do.call(rbind, verts))
    }
  }
  list(min = lo, max = .hull_km2(pts))
}

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
#' @return A list with \code{area_km2} (\code{NA} when fewer than 3 points are
#'   available, or when the hull is degenerate; callers should reconcile
#'   against AOO via \code{\link{reconcile_eoo_aoo}}) and \code{geom}, the
#'   hull as an \code{sf} geometry.
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
                          year_end = NULL) {

  # 1. Temporal Filter
  occurrence_data <- filter_years(occurrence_data, year_start, year_end, date_col)

  # IUCN section 4.9: EOO must never be smaller than AOO. With fewer than 3
  # points a convex hull has zero area, so EOO is returned as NA and
  # reconcile_eoo_aoo() substitutes AOO.
  if (nrow(occurrence_data) < 3) {
    return(list(area_km2 = NA_real_, geom = NULL))
  }

  # 2. Calculate Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  # 3. Calculate Area using the 'units' package to ensure safe conversion to km2
  area_units <- sf::st_area(hull)
  area_km2 <- as.numeric(units::set_units(area_units, "km^2"))

  # Three or more collinear points (or points inside a single cell) yield a
  # degenerate hull; treat it as "not computable" so AOO takes over.
  if (!is.finite(area_km2) || area_km2 <= 0) {
    return(list(area_km2 = NA_real_, geom = hull))
  }

  return(list(area_km2 = area_km2, geom = hull))
}

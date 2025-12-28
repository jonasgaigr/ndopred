#' Calculate EOO with Temporal Filter
#' @param occurrence_data sf object
#' @param year_start Numeric. Only records from this year onwards.
#' @export
calculate_eoo <- function(occurrence_data, year_start = NULL) {

  # 1. Temporal Filter
  if (!is.null(year_start) && nrow(occurrence_data) > 0) {
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(y = as.numeric(substr(as.character(DATUM_OD), 1, 4))) %>%
      dplyr::filter(y >= year_start)
  }

  if (nrow(occurrence_data) < 3) return(list(area_km2 = 0, geom = NULL))

  # 2. Calculate Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  area <- as.numeric(sf::st_area(hull)) / 1e6
  return(list(area_km2 = area, geom = hull))
}

#' Calculate Extent of Occurrence (EOO)
#'
#' @param occurrence_data An sf object (points).
#'
#' @return A list containing the EOO area in km2 and the polygon geometry.
#' @export
calculate_eoo <- function(occurrence_data) {
  if (nrow(occurrence_data) < 3) {
    message("Fewer than 3 points. EOO cannot be calculated.")
    return(list(area_km2 = 0, geom = NULL))
  }

  # Calculate Minimum Convex Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  # Calculate area in km2 (st_area returns m2 for EPSG:3035)
  area_m2 <- sf::st_area(hull)
  area_km2 <- as.numeric(area_m2) / 1e6

  return(list(area_km2 = area_km2, geom = hull))
}

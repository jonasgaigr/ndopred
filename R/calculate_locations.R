#' Estimate Number of IUCN Locations
#'
#' @param occurrence_data An sf object.
#' @param threshold_dist Numeric. Distance in meters to cluster points (default 5000m).
#' @export
calculate_locations <- function(occurrence_data, threshold_dist = 5000) {
  if (nrow(occurrence_data) == 0) return(0)

  # Create a buffer around points and merge overlapping ones
  clusters <- sf::st_buffer(occurrence_data, dist = threshold_dist / 2) %>%
    sf::st_union() %>%
    sf::st_cast("POLYGON")

  # The number of resulting polygons is our "Location" count
  return(length(clusters))
}

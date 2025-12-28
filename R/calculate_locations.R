#' Calculate Number of Locations (IUCN Criterion B)
#'
#' @param occurrence_data An sf object of occurrences.
#' @param threshold_dist Numeric. Distance in meters to group points (default 5000).
#' @param year_start Numeric. Optional filter for starting year.
#' @return Numeric count of clusters (locations).
#' @export
calculate_locations <- function(occurrence_data, threshold_dist = 5000, year_start = NULL) {

  # 1. Temporal Filter (Harmonization)
  if (!is.null(year_start) && nrow(occurrence_data) > 0) {
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(y = as.numeric(substr(as.character(DATUM_OD), 1, 4))) %>%
      dplyr::filter(y >= year_start)
  }

  # Safety Check: If no recent data, locations = 0
  if (nrow(occurrence_data) == 0) {
    return(0)
  }

  # 2. Create Distance Matrix
  # Returns which points are within the threshold distance of each other
  dist_matrix <- sf::st_is_within_distance(occurrence_data, dist = threshold_dist)

  # 3. Use Graph Theory to find connected clusters
  # Requires igraph package
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for calculate_locations. Please install it.")
  }

  g <- igraph::graph_from_adj_list(dist_matrix)

  # Count the connected components (independent clusters)
  clusters <- igraph::components(g)$no

  return(clusters)
}

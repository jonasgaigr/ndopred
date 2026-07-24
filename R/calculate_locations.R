#' Calculate Number of Locations (IUCN Criterion B)
#'
#' @param occurrence_data An sf object of occurrences.
#' @param threshold_dist Numeric. Distance in metres to group points (default 5000).
#' @param year_start Numeric. Optional filter for starting year.
#' @param year_end Numeric. Optional filter for ending year.
#' @return Numeric count of clusters (locations).
#' @export
calculate_locations <- function(occurrence_data, threshold_dist = 5000, year_start = NULL, year_end = NULL) {

  # 1. Temporal Filter (Harmonisation)
  # Favour a pre-existing 'year' column to keep standardisation across the package
  if (!"year" %in% names(occurrence_data) && "DATUM_OD" %in% names(occurrence_data)) {
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(year = as.numeric(substr(as.character(DATUM_OD), 1, 4)))
  }

  # Apply strict temporal bounds if provided
  if (nrow(occurrence_data) > 0) {
    if (!is.null(year_start)) {
      occurrence_data <- occurrence_data %>% dplyr::filter(year >= year_start)
    }
    if (!is.null(year_end)) {
      occurrence_data <- occurrence_data %>% dplyr::filter(year <= year_end)
    }
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

#' Calculate Number of Locations (IUCN Criterion B)
#'
#' @section Important caveat:
#' IUCN Guidelines (v16, 2024) section 4.11 define a *location* as "a
#' geographically or ecologically distinct area in which a single threatening
#' event can rapidly affect all individuals of the taxon present", sized by
#' the most serious plausible threat. It is therefore a **threat-based**
#' concept that cannot be derived from occurrence records alone. The distance
#' clustering below is a purely spatial *surrogate* offered as a starting
#' point for expert review, not a guideline-compliant location count. In
#' particular, section 4.11 states that "in the absence of any plausible
#' threat for the taxon, the term 'location' cannot be used and the
#' subcriteria that refer to the number of locations will not be met" - a
#' judgement only an assessor can make.
#'
#' @param occurrence_data An sf object of occurrences.
#' @param threshold_dist Numeric. Distance in metres within which occurrences
#'   are treated as belonging to the same putative location (default 5000).
#' @param year_start Numeric. Optional filter for starting year.
#' @param year_end Numeric. Optional filter for ending year.
#' @param max_points Integer. Above this many distinct coordinates, points are
#'   aggregated to occupied-grid-cell centroids before clustering. The exact
#'   pairwise clustering is quadratic in the number of points and becomes
#'   intractable for well-recorded taxa (tens of thousands of NDOP records);
#'   aggregating to the AOO reference grid keeps the count tractable and also
#'   strips out recording-effort weighting.
#' @param grid_size Numeric. Cell size in metres used for that aggregation
#'   (default 2000, i.e. the IUCN AOO reference grid).
#' @return Numeric count of clusters (putative locations).
#' @seealso [calculate_aoo()], [summarize_assessment()] whose
#'   `severely_fragmented` argument covers the other half of criterion B(a).
#' @references IUCN Standards and Petitions Committee (2024), section 4.11.
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     # three points a few km apart, then two far away
#'     x = c(4600000, 4601000, 4602000, 4700000, 4701000),
#'     y = c(2900000, 2900000, 2900000, 2900000, 2900000),
#'     DATUM_OD = rep("20200615", 5)
#'   ),
#'   coords = c("x", "y"), crs = 3035
#' )
#' calculate_locations(pts)                       # 2 clusters at 5 km
#' calculate_locations(pts, threshold_dist = 200000)  # 1 cluster at 200 km
#' @export
calculate_locations <- function(occurrence_data, threshold_dist = 5000,
                                year_start = NULL, year_end = NULL,
                                max_points = 5000, grid_size = 2000) {

  # 1. Temporal Filter (shared year handling, see filter_years())
  if (!is.null(year_start) || !is.null(year_end)) {
    occurrence_data <- filter_years(occurrence_data, year_start, year_end)
  }

  # Safety Check: If no data, locations = 0
  if (nrow(occurrence_data) == 0) {
    return(0)
  }

  # 2. Reduce to distinct positions.
  # Exact de-duplication first: repeat visits to the same coordinate are the
  # dominant redundancy in NDOP and always fall in the same cluster, so
  # dropping them cannot change the component count.
  coords <- sf::st_coordinates(occurrence_data)[, 1:2, drop = FALSE]
  coords <- unique(coords)

  if (nrow(coords) > max_points) {
    # Fall back to occupied-grid-cell centroids (see @param max_points).
    cx <- floor(coords[, 1] / grid_size) * grid_size + grid_size / 2
    cy <- floor(coords[, 2] / grid_size) * grid_size + grid_size / 2
    coords <- unique(cbind(X = cx, Y = cy))
    warning(
      "More than ", max_points, " distinct coordinates; locations counted on ",
      grid_size / 1000, " km grid-cell centroids rather than raw points.",
      call. = FALSE
    )
  }

  if (nrow(coords) == 1) {
    return(1)
  }

  pts <- sf::st_as_sf(
    as.data.frame(coords),
    coords = colnames(coords)[1:2],
    crs = sf::st_crs(occurrence_data)
  )

  # 3. Create adjacency (which points lie within the threshold distance)
  dist_matrix <- sf::st_is_within_distance(pts, dist = threshold_dist)

  # 4. Use graph theory to find connected clusters
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for calculate_locations. Please install it.")
  }

  g <- igraph::graph_from_adj_list(dist_matrix)

  # Count the connected components (independent clusters)
  clusters <- igraph::components(g)$no

  return(clusters)
}

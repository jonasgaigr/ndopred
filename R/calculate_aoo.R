#' Calculate AOO with Temporal Filter
#' @param occurrence_data sf object
#' @param grid_size Numeric (default 2000 for IUCN)
#' @param year_start Numeric. Only records from this year onwards.
#' @export
calculate_aoo <- function(occurrence_data, grid_size = 2000, year_start = NULL) {

  # 1. Temporal Filter
  if (!is.null(year_start) && nrow(occurrence_data) > 0) {
    occurrence_data <- occurrence_data %>%
      dplyr::mutate(y = as.numeric(substr(as.character(DATUM_OD), 1, 4))) %>%
      dplyr::filter(y >= year_start)
  }

  if (nrow(occurrence_data) == 0) return(list(area_km2 = 0, geom = NULL))

  # 2. Grid Generation
  bbox <- sf::st_bbox(occurrence_data)
  rast_template <- terra::rast(
    xmin = floor(bbox$xmin / grid_size) * grid_size,
    xmax = ceiling(bbox$xmax / grid_size) * grid_size,
    ymin = floor(bbox$ymin / grid_size) * grid_size,
    ymax = ceiling(bbox$ymax / grid_size) * grid_size,
    res = grid_size, crs = "EPSG:3035"
  )

  aoo_raster <- terra::rasterize(terra::vect(occurrence_data), rast_template)
  aoo_polygons <- terra::as.polygons(aoo_raster) %>% sf::st_as_sf()

  area <- as.numeric(sum(sf::st_area(aoo_polygons))) / 1e6
  return(list(area_km2 = area, geom = aoo_polygons))
}

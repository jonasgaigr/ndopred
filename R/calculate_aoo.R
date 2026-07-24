#' Calculate AOO with Temporal Filter
#' @param occurrence_data sf object
#' @param grid_size Numeric (default 2000 for IUCN)
#' @param year_start Numeric. Only records from this year onwards.
#' @param date_col Character. The name of the date column (default is "DATUM_OD").
#' @export
calculate_aoo <- function(occurrence_data, grid_size = 2000, year_start = NULL, date_col = "DATUM_OD") {

  # 1. Temporal Filter
  if (!is.null(year_start) && nrow(occurrence_data) > 0) {
    if (!date_col %in% names(occurrence_data)) {
      stop(paste("Column", date_col, "not found in the dataset."))
    }

    occurrence_data <- occurrence_data %>%
      dplyr::mutate(y = as.numeric(substr(as.character(!!rlang::sym(date_col)), 1, 4))) %>%
      dplyr::filter(y >= year_start)
  }

  if (nrow(occurrence_data) == 0) return(list(area_km2 = 0, geom = NULL))

  # Extract dynamic CRS from the input data
  input_crs <- sf::st_crs(occurrence_data)$wkt

  if (is.na(input_crs)) {
    stop("Input occurrence_data must have a defined CRS.")
  }

  # 2. Grid Generation
  bbox <- sf::st_bbox(occurrence_data)
  rast_template <- terra::rast(
    xmin = floor(bbox$xmin / grid_size) * grid_size,
    xmax = ceiling(bbox$xmax / grid_size) * grid_size,
    ymin = floor(bbox$ymin / grid_size) * grid_size,
    ymax = ceiling(bbox$ymax / grid_size) * grid_size,
    res = grid_size,
    crs = input_crs
  )

  # 3. Rasterise and Extract Geometry
  aoo_raster <- terra::rasterize(terra::vect(occurrence_data), rast_template)
  aoo_polygons <- terra::as.polygons(aoo_raster) %>% sf::st_as_sf()

  # 4. Calculate Area using grid parameters (safest for AOO)
  # An IUCN cell is typically 2x2 km = 4 sq km.
  cell_area_km2 <- (grid_size / 1000)^2
  total_area_km2 <- nrow(aoo_polygons) * cell_area_km2

  return(list(area_km2 = total_area_km2, geom = aoo_polygons))
}

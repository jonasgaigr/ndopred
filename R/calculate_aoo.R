calculate_aoo <- function(occurrence_data, grid_size = 2000) {
  if (nrow(occurrence_data) == 0) return(list(area_km2 = 0, geom = NULL))

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

  # Optional: Clip AOO to Border (currently disabled)
  # cz_border <- giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
  #   sf::st_transform(3035)
  # aoo_polygons <- sf::st_intersection(aoo_polygons, cz_border)

  area <- as.numeric(sum(sf::st_area(aoo_polygons))) / 1e6
  return(list(area_km2 = area, geom = aoo_polygons))
}

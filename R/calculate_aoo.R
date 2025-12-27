#' Calculate Area of Occupancy (AOO)
#'
#' @param occurrence_data An sf object (points).
#' @param grid_size Numeric. Grid cell size in meters. Default 2000 (2km).
#'
#' @return A list containing the AOO area in km2 and the grid geometry.
#' @export
calculate_aoo <- function(occurrence_data, grid_size = 2000) {
  if (nrow(occurrence_data) == 0) return(list(area_km2 = 0, geom = NULL))

  # 1. Create a raster template covering the points
  # We use the bbox of the data and expand it slightly to align with the grid
  bbox <- sf::st_bbox(occurrence_data)

  # 2. Create a grid (raster) with 2km resolution
  # We use terra here because it's significantly faster for grid operations
  rast_template <- terra::rast(
    xmin = floor(bbox$xmin / grid_size) * grid_size,
    xmax = ceiling(bbox$xmax / grid_size) * grid_size,
    ymin = floor(bbox$ymin / grid_size) * grid_size,
    ymax = ceiling(bbox$ymax / grid_size) * grid_size,
    res = grid_size,
    crs = "EPSG:3035"
  )

  # 3. Rasterize the points (presence/absence)
  aoo_raster <- terra::rasterize(terra::vect(occurrence_data), rast_template, fun = "length")

  # 4. Count occupied cells and multiply by area (4 km2 per cell)
  occupied_cells <- terra::global(aoo_raster, "notNA")[[1]]
  area_km2 <- occupied_cells * (grid_size^2 / 1e6)

  # 5. Optional: Convert to polygons for visualization
  aoo_polygons <- terra::as.polygons(aoo_raster) %>% sf::st_as_sf()

  return(list(area_km2 = area_km2, geom = aoo_polygons))
}

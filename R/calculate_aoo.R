#' Calculate AOO with Temporal Filter
#' @param occurrence_data sf object
#' @param grid_size Numeric (default 2000 for IUCN)
#' @param year_start Numeric. Only records from this year onwards.
#' @param date_col Character. The name of the date column (default is "DATUM_OD").
#' @param n_shifts Integer. Number of grid-origin offsets tested per axis. IUCN
#'   Guidelines section 4.10.2: "If different grid locations (starting points
#'   of the grid) result in different AOO estimates, the minimum estimate
#'   should be used." Default 4 tests a 4x4 = 16 origin grid; use 1 to keep a
#'   single, unshifted origin (faster, but not guideline-compliant).
#' @export
calculate_aoo <- function(occurrence_data, grid_size = 2000, year_start = NULL,
                           date_col = "DATUM_OD", n_shifts = 4) {

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

  coords <- sf::st_coordinates(occurrence_data)
  px <- coords[, 1]
  py <- coords[, 2]

  # 2. Test multiple grid origins and keep the minimum occupied-cell count
  # (IUCN Guidelines 4.10.2: "If different grid locations ... result in
  # different AOO estimates, the minimum estimate should be used.").
  #
  # Cell membership is computed directly from point coordinates via floor()
  # division rather than via terra::rasterize(): floor() bins every point
  # unambiguously, whereas rasterize()'s half-open cell edges can silently
  # drop or misassign points that land exactly on a grid line (which happens
  # more than the "measure zero" intuition suggests, since projected
  # coordinates are often round numbers). This is also far cheaper than
  # rasterizing for every candidate origin.
  offsets <- ((seq_len(n_shifts) - 1) / n_shifts) * grid_size

  best_n_cells <- Inf
  best_origin <- c(x = offsets[1], y = offsets[1])

  for (ox in offsets) {
    for (oy in offsets) {
      cell_x <- floor((px - ox) / grid_size)
      cell_y <- floor((py - oy) / grid_size)
      # Integer hash instead of paste(): well within safe integer range for
      # any realistic grid-cell index, and noticeably faster for large n.
      cell_key <- cell_x * 1e7 + cell_y
      n_cells <- length(unique(cell_key))

      if (n_cells < best_n_cells) {
        best_n_cells <- n_cells
        best_origin <- c(x = ox, y = oy)
      }
    }
  }

  # 3. Build a raster/polygon representation at the winning origin, for
  # mapping purposes only - the area below uses best_n_cells, not this
  # geometry's feature count.
  bbox <- sf::st_bbox(occurrence_data)
  ox <- best_origin["x"]
  oy <- best_origin["y"]
  eps <- grid_size * 1e-6

  xmin <- floor((bbox$xmin - ox) / grid_size) * grid_size + ox
  ymin <- floor((bbox$ymin - oy) / grid_size) * grid_size + oy
  xmax <- xmin + max(1, ceiling((bbox$xmax - xmin + eps) / grid_size)) * grid_size
  ymax <- ymin + max(1, ceiling((bbox$ymax - ymin + eps) / grid_size)) * grid_size

  rast_template <- terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    res = grid_size, crs = input_crs
  )
  aoo_raster <- terra::rasterize(terra::vect(occurrence_data), rast_template)
  aoo_polygons <- terra::as.polygons(aoo_raster) %>% sf::st_as_sf()

  # 4. Calculate Area from the authoritative cell count (see note above).
  # An IUCN cell is typically 2x2 km = 4 sq km.
  cell_area_km2 <- (grid_size / 1000)^2
  total_area_km2 <- best_n_cells * cell_area_km2

  return(list(area_km2 = total_area_km2, geom = aoo_polygons))
}

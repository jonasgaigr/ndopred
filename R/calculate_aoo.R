#' Calculate AOO with Temporal Filter
#'
#' Counts occupied cells of a uniform grid, per IUCN Guidelines (v16, 2024)
#' section 4.10.2, equation 4.1: \code{AOO = no. occupied cells x area of an
#' individual cell}.
#'
#' @param occurrence_data sf object
#' @param grid_size Numeric. Cell size in map units (default 2000 m). IUCN
#'   Guidelines section 4.10.3: 4 km2 (2 x 2 km) cells are the *reference
#'   scale* for criteria B2 and D2, and estimates at other scales should be
#'   avoided. A warning is emitted if a different size is requested.
#' @param year_start Numeric. Only records from this year onwards.
#' @param year_end Numeric. Only records up to and including this year.
#' @param date_col Character. The name of the date column (default is "DATUM_OD").
#' @param n_shifts Integer. Number of grid-origin offsets tested per axis. IUCN
#'   Guidelines section 4.10.2: "If different grid locations (starting points
#'   of the grid) result in different AOO estimates, the minimum estimate
#'   should be used." Default 4 tests a 4x4 = 16 origin grid; use 1 to keep a
#'   single, unshifted origin (faster, but not guideline-compliant).
#' @param origin Numeric vector of length 2 (\code{c(x, y)}) or NULL. When
#'   supplied, the origin search is skipped and this grid origin is used
#'   verbatim. Required when comparing AOO between two time periods: the
#'   minimising origin generally differs between periods, so independently
#'   minimised estimates are not comparable and would manufacture spurious
#'   trends (see IUCN Guidelines section 4.9 on comparing temporal estimates).
#' @return A list with \code{area_km2}, \code{n_cells}, \code{origin} (the grid
#'   origin actually used, for reuse in a paired period) and \code{geom} (an
#'   \code{sf} polygon layer of the occupied cells, for mapping).
#' @seealso [calculate_eoo()], [reconcile_eoo_aoo()], [calculate_trend()]
#' @references IUCN Standards and Petitions Committee (2024), sections 4.10.2
#'   (grid method and minimum over origins) and 4.10.3 (2 x 2 km reference
#'   scale).
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     x = c(4600000, 4610000, 4620000, 4630000, 4640000),
#'     y = c(2900000, 2900300, 2899700, 2900500, 2899300),
#'     DATUM_OD = c("20200615", "20210615", "20220615", "20120615", "20130615")
#'   ),
#'   coords = c("x", "y"), crs = 3035
#' )
#'
#' # Five well-separated points -> five occupied 2x2 km cells -> 20 km2
#' calculate_aoo(pts)$area_km2
#'
#' # Restrict to a window
#' calculate_aoo(pts, year_start = 2020)$area_km2
#'
#' # Pin the origin so two periods stay comparable
#' shared <- calculate_aoo(pts)$origin
#' calculate_aoo(pts, year_start = 2020, origin = shared)$n_cells
#' @export
calculate_aoo <- function(occurrence_data, grid_size = 2000, year_start = NULL,
                          date_col = "DATUM_OD", n_shifts = 4, year_end = NULL,
                          origin = NULL) {

  if (!isTRUE(all.equal(grid_size, 2000))) {
    warning(
      "AOO requested at a ", grid_size / 1000, " km grid. IUCN Guidelines ",
      "section 4.10.3 sets 2 x 2 km as the reference scale for criteria B2 ",
      "and D2; results at other scales are not comparable with the criteria ",
      "thresholds.", call. = FALSE
    )
  }

  # 1. Temporal Filter
  occurrence_data <- filter_years(occurrence_data, year_start, year_end, date_col)

  if (nrow(occurrence_data) == 0) {
    return(list(area_km2 = 0, n_cells = 0, origin = origin, geom = NULL))
  }

  # Extract dynamic CRS from the input data
  input_crs <- sf::st_crs(occurrence_data)$wkt

  if (is.na(input_crs)) {
    stop("Input occurrence_data must have a defined CRS.")
  }

  coords <- sf::st_coordinates(occurrence_data)
  px <- coords[, 1]
  py <- coords[, 2]

  count_cells <- function(ox, oy) {
    # Cell membership is computed directly from point coordinates via floor()
    # division rather than via terra::rasterize(): floor() bins every point
    # unambiguously, whereas rasterize()'s half-open cell edges can silently
    # drop or misassign points that land exactly on a grid line (which happens
    # more than the "measure zero" intuition suggests, since projected
    # coordinates are often round numbers). This is also far cheaper than
    # rasterizing for every candidate origin.
    cell_x <- floor((px - ox) / grid_size)
    cell_y <- floor((py - oy) / grid_size)
    # Integer hash instead of paste(): well within safe integer range for
    # any realistic grid-cell index, and noticeably faster for large n.
    length(unique(cell_x * 1e7 + cell_y))
  }

  if (!is.null(origin)) {
    # 2a. Caller-pinned origin (paired-period comparisons).
    best_origin <- c(x = unname(origin[1]), y = unname(origin[2]))
    best_n_cells <- count_cells(best_origin["x"], best_origin["y"])
  } else {
    # 2b. Test multiple grid origins and keep the minimum occupied-cell count
    # (IUCN Guidelines 4.10.2).
    offsets <- ((seq_len(n_shifts) - 1) / n_shifts) * grid_size

    best_n_cells <- Inf
    best_origin <- c(x = offsets[1], y = offsets[1])

    for (ox in offsets) {
      for (oy in offsets) {
        n_cells <- count_cells(ox, oy)
        if (n_cells < best_n_cells) {
          best_n_cells <- n_cells
          best_origin <- c(x = ox, y = oy)
        }
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

  return(list(
    area_km2 = total_area_km2,
    n_cells = best_n_cells,
    origin = c(x = unname(ox), y = unname(oy)),
    geom = aoo_polygons
  ))
}

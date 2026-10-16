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
#' @param exact_max Integer. Up to this many distinct positions, the minimum
#'   over origins is also searched exactly (see the section below) and
#'   replaces the regular search's result where it is lower.
#' @section Exact minimum over origins:
#' The cell count changes only where an origin passes the position of a
#' point modulo the cell size, so trying those positions on both axes finds
#' the true minimum. The regular 4 x 4 search can miss it by a cell or two,
#' which matters near the CR and EN thresholds of B2; random jitter, as in
#' redlistr, can miss it as well. The exact search costs about the square
#' of the number of distinct positions, so it is limited to `exact_max`.
#' @param origin Numeric vector of length 2 (\code{c(x, y)}) or NULL. When
#'   supplied, the origin search is skipped and this grid origin is used
#'   verbatim. Required when comparing AOO between two time periods: the
#'   minimising origin generally differs between periods, so independently
#'   minimised estimates are not comparable and would manufacture spurious
#'   trends (see IUCN Guidelines section 4.9 on comparing temporal estimates).
#' @param precision Numeric vector of positional precision in metres, one per
#'   record, or `NULL` to read the `precision_m` or `CXPRESNOST` column (0
#'   means an exact position; gap analysis, decision D3).
#' @param geometries Optional `sf` layer of record polygons and lines with an
#'   `ID_LOKAL` column (from `attr(occ, "record_geometries")`). Records whose
#'   `ID_LOKAL` has one are placed by that geometry instead of a precision
#'   disc.
#' @section Positional uncertainty:
#' `area_km2` places every record in the cell containing its point. A record
#' whose precision disc (or polygon) reaches into other cells could lie in
#' any of them, so two bounds are added at the same grid origin (Guidelines
#' §3.2, §4.10.8):
#' * `area_km2_min`: uncertain records are put in a cell already occupied
#'   whenever they can reach one;
#' * `area_km2_max`: each uncertain record takes a cell no other record
#'   holds whenever it can reach one.
#' These bound the *observed* AOO only. For an under-recorded taxon the true
#' AOO can be larger still (§4.10.8).
#' @return A list with \code{area_km2}, \code{n_cells}, \code{area_km2_min},
#'   \code{area_km2_max}, \code{n_cells_min}, \code{n_cells_max},
#'   \code{n_uncertain} (records whose position spans more than one cell),
#'   \code{origin} (the grid origin actually used, for reuse in a paired
#'   period) and \code{geom} (an \code{sf} polygon layer of the occupied
#'   cells, for mapping).
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
                          origin = NULL, precision = NULL, geometries = NULL,
                          exact_max = 150) {

  if (!isTRUE(all.equal(grid_size, 2000))) {
    warning(
      "AOO requested at a ", grid_size / 1000, " km grid. IUCN Guidelines ",
      "section 4.10.3 sets 2 x 2 km as the reference scale for criteria B2 ",
      "and D2; results at other scales are not comparable with the criteria ",
      "thresholds.", call. = FALSE
    )
  }

  # 1. Temporal Filter; records without a position cannot occupy a cell.
  occurrence_data <- filter_years(occurrence_data, year_start, year_end, date_col)
  if (nrow(occurrence_data) > 0) {
    occurrence_data <- suppressWarnings(occurrence_data[!sf::st_is_empty(occurrence_data), ])
  }

  if (nrow(occurrence_data) == 0) {
    return(list(area_km2 = 0, n_cells = 0, area_km2_min = 0, area_km2_max = 0,
                n_cells_min = 0, n_cells_max = 0, n_uncertain = 0,
                origin = origin, geom = NULL))
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
    # For few distinct positions the minimum is found exactly; it replaces
    # the regular search only where it is strictly lower.
    if (n_shifts > 1) {
      ex <- .exact_origin(px, py, grid_size, exact_max)
      if (!is.null(ex) && ex$n_cells < best_n_cells) {
        best_n_cells <- ex$n_cells
        best_origin <- ex$origin
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

  # 5. Bounds from positional uncertainty, at the same origin.
  bounds <- .aoo_bounds(occurrence_data, px, py, unname(ox), unname(oy), grid_size,
                        .record_precision(occurrence_data, precision), geometries)
  n_min <- min(bounds$n_min, best_n_cells)
  n_max <- max(bounds$n_max, best_n_cells)

  return(list(
    area_km2 = total_area_km2,
    n_cells = best_n_cells,
    area_km2_min = n_min * cell_area_km2,
    area_km2_max = n_max * cell_area_km2,
    n_cells_min = n_min,
    n_cells_max = n_max,
    n_uncertain = bounds$n_uncertain,
    origin = c(x = unname(ox), y = unname(oy)),
    geom = aoo_polygons
  ))
}

# The exact minimum of occupied cells over grid origins. Along each axis the
# count is constant between consecutive positions of points modulo the
# cell size, so these positions are the only candidates; NULL above
# `max_points` distinct positions.
.exact_origin <- function(px, py, g, max_points) {
  xy <- unique(cbind(px, py))
  if (nrow(xy) > max_points) return(NULL)
  bx <- sort(unique(xy[, 1] %% g))
  by <- sort(unique(xy[, 2] %% g))
  # Cell rows for every candidate y origin at once (points x candidates).
  cy <- floor(outer(xy[, 2], by, "-") / g)
  best <- list(n_cells = Inf, origin = NULL)
  for (ox in bx) {
    keys <- floor((xy[, 1] - ox) / g) * 1e7 + cy
    n <- vapply(seq_along(by), function(j) length(unique(keys[, j])), integer(1))
    j <- which.min(n)
    if (n[j] < best$n_cells) best <- list(n_cells = n[j], origin = c(x = ox, y = by[j]))
  }
  best
}

# Positional precision per record in metres: an explicit vector, else the
# precision_m or CXPRESNOST column; missing values and 0 mean exact (D3).
.record_precision <- function(occ, precision = NULL) {
  n <- nrow(occ)
  r <- if (!is.null(precision)) {
    rep_len(as.numeric(precision), n)
  } else if ("precision_m" %in% names(occ)) {
    suppressWarnings(as.numeric(occ$precision_m))
  } else if ("CXPRESNOST" %in% names(occ)) {
    suppressWarnings(as.numeric(as.character(occ$CXPRESNOST)))
  } else {
    rep(0, n)
  }
  r[is.na(r) | r < 0] <- 0
  r
}

# Cells each record could lie in, and the minimum and maximum number of
# occupied cells over those choices (greedy; see calculate_aoo()).
.aoo_bounds <- function(occ, px, py, ox, oy, g, r, geometries = NULL) {
  key <- function(ix, iy) ix * 1e7 + iy
  cx <- floor((px - ox) / g)
  cy <- floor((py - oy) / g)
  own <- key(cx, cy)
  # A point record is certain when its precision disc lies inside its cell.
  ex <- (px - ox) - cx * g
  ey <- (py - oy) - cy * g
  certain <- pmin(ex, g - ex, ey, g - ey) >= r

  # Records with a polygon or line are placed by that geometry instead.
  geom_idx <- rep(NA_integer_, length(px))
  if (!is.null(geometries) && "ID_LOKAL" %in% names(occ) && nrow(geometries) > 0) {
    types <- as.character(sf::st_geometry_type(geometries))
    shaped <- !types %in% c("POINT", "MULTIPOINT")
    if (any(shaped)) {
      gm <- sf::st_transform(geometries[shaped, ], sf::st_crs(occ))
      geom_idx <- match(suppressWarnings(as.numeric(as.character(occ$ID_LOKAL))), gm$ID_LOKAL)
    }
  }

  # Records sharing a position and precision, or a geometry, are worked out
  # once: repeat visits to the same site are the norm in NDOP.
  sig <- ifelse(!is.na(geom_idx), paste0("g", geom_idx), paste(px, py, r))
  todo <- which(!duplicated(sig) & (!certain | !is.na(geom_idx)))
  cand <- vector("list", length(px))
  for (j in todo) {
    if (!is.na(geom_idx[j])) {
      shape <- sf::st_geometry(gm)[geom_idx[j]]
      bb <- sf::st_bbox(shape)
      ix <- floor((bb[["xmin"]] - ox) / g):floor((bb[["xmax"]] - ox) / g)
      iy <- floor((bb[["ymin"]] - oy) / g):floor((bb[["ymax"]] - oy) / g)
      cells <- expand.grid(ix = ix, iy = iy)
      rects <- lapply(seq_len(nrow(cells)), function(k) {
        x0 <- cells$ix[k] * g + ox
        y0 <- cells$iy[k] * g + oy
        sf::st_polygon(list(rbind(c(x0, y0), c(x0 + g, y0), c(x0 + g, y0 + g), c(x0, y0 + g), c(x0, y0))))
      })
      hit <- lengths(sf::st_intersects(sf::st_sfc(rects, crs = sf::st_crs(occ)), shape)) > 0
      cand[[j]] <- unique(c(own[j], key(cells$ix[hit], cells$iy[hit])))
      certain[j] <- length(cand[[j]]) == 1
    } else if (!certain[j]) {
      ix <- floor((px[j] - r[j] - ox) / g):floor((px[j] + r[j] - ox) / g)
      iy <- floor((py[j] - r[j] - oy) / g):floor((py[j] + r[j] - oy) / g)
      cells <- expand.grid(ix = ix, iy = iy)
      x0 <- cells$ix * g + ox
      y0 <- cells$iy * g + oy
      dx <- pmax(x0 - px[j], 0, px[j] - (x0 + g))
      dy <- pmax(y0 - py[j], 0, py[j] - (y0 + g))
      reach <- sqrt(dx^2 + dy^2) <= r[j]
      cand[[j]] <- key(cells$ix[reach], cells$iy[reach])
    }
  }
  # Copy the worked-out cells to the records that share them.
  done <- sig[todo]
  for (j in setdiff(which(!certain | !is.na(geom_idx)), todo)) {
    src <- todo[match(sig[j], done)]
    cand[[j]] <- cand[[src]]
    certain[j] <- certain[src]
  }

  fixed <- unique(own[certain])
  loose <- which(!certain)
  # Fewest candidates first, so the most constrained records choose first.
  loose <- loose[order(lengths(cand[loose]))]
  used_min <- fixed
  used_max <- fixed
  for (j in loose) {
    if (!any(cand[[j]] %in% used_min)) used_min <- c(used_min, own[j])
    free <- setdiff(cand[[j]], used_max)
    if (length(free)) used_max <- c(used_max, if (own[j] %in% free) own[j] else free[1])
  }
  list(n_min = length(unique(used_min)), n_max = length(unique(used_max)),
       n_uncertain = length(loose))
}

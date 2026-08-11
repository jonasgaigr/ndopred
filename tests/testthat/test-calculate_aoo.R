make_pts <- function(x, y, crs = 3035) {
  sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = crs)
}

test_that("AOO counts distinct 2x2km cells, not merged polygon features", {
  # Regression test: terra::rasterize() assigns a constant value to every
  # touched cell, and terra::as.polygons()'s default cell aggregation merges
  # all same-valued cells into a single feature regardless of adjacency.
  # calculate_aoo() must not derive area from nrow(as.polygons(...)), which
  # would collapse to a single cell's area for any non-empty input.
  pts <- make_pts(
    x = c(4600000, 4610000, 4620000, 4630000, 4640000),
    y = c(2900000, 2900300, 2899700, 2900500, 2899300)
  )
  res <- calculate_aoo(pts, grid_size = 2000, n_shifts = 1)
  expect_equal(res$area_km2, 20) # 5 well-separated cells x 4 km2
})

test_that("Repeated points within the same cell do not inflate AOO", {
  # 3 points cluster into one 2x2km cell, 2 more into a second, distinct cell.
  pts <- make_pts(
    x = c(4600100, 4600300, 4600300, 4610100, 4610300),
    y = c(2900100, 2900100, 2900300, 2900100, 2900300)
  )
  res <- calculate_aoo(pts, grid_size = 2000, n_shifts = 1)
  expect_equal(res$area_km2, 8) # 2 distinct cells x 4 km2, not 5 x 4
})

test_that("Single point yields exactly one cell", {
  pts <- make_pts(x = 4600000, y = 2900000)
  res <- calculate_aoo(pts, grid_size = 2000)
  expect_equal(res$area_km2, 4)
})

test_that("Grid-aligned coordinates are not dropped at raster edges", {
  # Regular lattice where every coordinate is an exact multiple of grid_size;
  # this previously caused boundary points to fall exactly on a raster edge
  # and silently drop out of the count.
  grid <- expand.grid(gx = 0:9, gy = 0:3)
  pts <- make_pts(
    x = 4600000 + grid$gx * 2000 + 1000,
    y = 2890000 + grid$gy * 2000 + 1000
  )
  res <- calculate_aoo(pts, grid_size = 2000)
  expect_equal(res$area_km2, 160) # 40 distinct cells x 4 km2
})

test_that("Minimum grid-origin AOO never exceeds a single origin (IUCN 4.10.2)", {
  set.seed(11)
  n <- 200
  pts <- make_pts(
    x = runif(n, 4622000, 4785000),
    y = runif(n, 2801000, 2965000)
  )
  res_min <- calculate_aoo(pts, grid_size = 2000, n_shifts = 4)
  res_single <- calculate_aoo(pts, grid_size = 2000, n_shifts = 1)
  expect_lte(res_min$area_km2, res_single$area_km2)
})

test_that("Non grid-aligned sparse points are counted correctly", {
  pts <- make_pts(
    x = c(4650000.37, 4700000.91, 4760000.13),
    y = c(2850000.22, 2900000.58, 2820000.71)
  )
  res <- calculate_aoo(pts, grid_size = 2000)
  expect_equal(res$area_km2, 12)
})

test_that("Empty occurrence data returns zero area", {
  pts <- suppressWarnings(sf::st_as_sf(
    data.frame(x = numeric(0), y = numeric(0)),
    coords = c("x", "y"), crs = 3035
  ))
  res <- calculate_aoo(pts, grid_size = 2000)
  expect_equal(res$area_km2, 0)
  expect_null(res$geom)
})

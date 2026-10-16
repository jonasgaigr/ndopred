# Cross-checks of the spatial metrics against redlistr, ConR and red
# (prompt section 9), and the benchmark table of compare_tools().

bench_points <- function(n, spread, seed = 5, x0 = 4600000, y0 = 2900000) {
  set.seed(seed)
  sf::st_as_sf(data.frame(x = x0 + stats::runif(n, 0, spread), y = y0 + stats::runif(n, 0, spread),
                          DATUM_OD = "20200615", DATUM_DO = "20200615"),
               coords = c("x", "y"), crs = 3035)
}

test_that("EOO equals redlistr's minimum convex polygon", {
  skip_if_not_installed("redlistr")
  for (spread in c(5000, 60000, 250000)) {
    pts <- bench_points(80, spread)
    ours <- calculate_eoo(pts)$area_km2
    theirs <- suppressMessages(redlistr::getEOO(pts))@EOO
    expect_equal(ours, theirs, tolerance = 1e-6)
  }
})

test_that("EOO agrees with ConR's equal-area hull within projection differences", {
  skip_if_not_installed("ConR")
  pts <- bench_points(80, 60000)
  ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
  XY <- data.frame(ddlat = ll[, 2], ddlon = ll[, 1], tax = "x")
  utils::capture.output(conr <- suppressMessages(ConR::EOO.computing(XY, show_progress = FALSE))$eoo)
  expect_lt(abs(conr - calculate_eoo(pts)$area_km2) / conr, 0.01)
})

test_that("AOO is never above redlistr's fixed or jittered grids by more than the search step", {
  skip_if_not_installed("redlistr")
  pts <- bench_points(120, 40000)
  ours <- calculate_aoo(pts)$n_cells
  theirs <- suppressWarnings(suppressMessages(
    redlistr::getAOO(pts, cell_size = 2000, bottom_1pct_rule = FALSE, n_jitter = 10)))@AOO
  # Both minimise over origins; a better origin may be found by either.
  expect_lte(abs(ours - theirs), max(2, 0.03 * theirs))
  # AOO on a single grid is an upper bound of the minimum over origins.
  fixed <- .aoo_at(sf::st_coordinates(pts), c(0, 0)) / 4
  expect_lte(ours, fixed)
})

test_that("the exact origin search is not beaten by redlistr's jitter", {
  skip_if_not_installed("redlistr")
  pts <- bench_points(40, 20000)
  theirs <- suppressWarnings(suppressMessages(
    redlistr::getAOO(pts, cell_size = 2000, bottom_1pct_rule = FALSE, n_jitter = 20)))@AOO
  expect_lte(calculate_aoo(pts)$n_cells, theirs)
})

test_that("compare_tools() tabulates ndopred and the tools on the same points", {
  cfg <- ndopred_config(reference_year = 2026)
  pts <- bench_points(40, 20000)
  tab <- compare_tools(pts, cfg, "Taxon x", tools = character(0))
  expect_equal(tab$Tool, c("ndopred (best estimate)", "ndopred (plausible maximum)"))
  expect_equal(tab$Points, c(40, 40))
  expect_equal(tab$EOO_diff_pct, c(0, 0))
  # Unknown decline: no category at the best estimate, EN as the plausible
  # maximum (EOO < 5,000 km2, AOO < 500 km2).
  expect_equal(tab$Category_B, c("LC or NT", "EN"))
  expect_match(tab$Criteria_B[2], "^B1ab")
})

test_that("compare_tools() matches ConR's category when ConR's locations meet ndopred's level", {
  skip_if_not_installed("ConR")
  cfg <- ndopred_config(reference_year = 2026)
  # Three clumps of 300 m on a triangle with 30 km sides: EOO about
  # 390 km2 and AOO 12-24 km2 (EN thresholds), three 10 km cells in ConR
  # (EN: at most 5 locations). ConR assumes decline; ndopred's plausible
  # maximum takes the unknown subcriteria as met.
  clump <- function(x, y, seed) bench_points(10, 300, seed, 4600000 + x, 2900000 + y)
  pts <- rbind(clump(0, 0, 1), clump(30000, 0, 2), clump(15000, 26000, 3))
  tab <- compare_tools(pts, cfg, "Taxon y", tools = "ConR")
  conr <- tab[tab$Tool == "ConR", ]
  high <- tab[tab$Tool == "ndopred (plausible maximum)", ]
  expect_equal(conr$Locations, 3)
  expect_equal(conr$Category_B, "EN")
  expect_equal(conr$Category_B, high$Category_B)
  expect_equal(conr$AOO_km2, high$AOO_km2)
})

test_that("compare_tools() reports a tool that cannot run instead of dropping it", {
  skip_if_not_installed("redlistr")
  cfg <- ndopred_config(reference_year = 2026)
  tab <- compare_tools(bench_points(2, 1000), cfg, "Taxon z", tools = "redlistr")
  expect_equal(nrow(tab), 3)
  expect_true(is.na(tab$AOO_km2[3]))
  expect_match(tab$Method[3], "not run")
})

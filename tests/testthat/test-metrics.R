make_occ <- function(x, y, years, crs = 3035, ...) {
  sf::st_as_sf(
    data.frame(x = x, y = y, ROK = years,
               DATUM_OD = sprintf("%04d0615", years), ...),
    coords = c("x", "y"), crs = crs
  )
}

# ---------------------------------------------------------------- extract_year

test_that("extract_year handles every NDOP date encoding seen in the wild", {
  expect_equal(extract_year(20200101), 2020)
  expect_equal(extract_year("20200101"), 2020)
  expect_equal(extract_year("2020-01-01"), 2020)
  expect_equal(extract_year("2020"), 2020)
  expect_equal(extract_year(as.Date("2020-01-01")), 2020)
  # Czech format: substr(x, 1, 4) used to yield "01.0" -> NA here.
  expect_equal(extract_year("01.01.2020"), 2020)
  expect_equal(extract_year("1/1/2020"), 2020)
  expect_true(is.na(extract_year("not a date")))
  expect_equal(extract_year(character(0)), numeric(0))
})

test_that("clean_dates keeps working through extract_year", {
  df <- data.frame(DATUM_OD = c("20200101", "01.01.1999", "junk"))
  expect_equal(clean_dates(df)$year, c(2020, 1999))
})

# --------------------------------------------------------------- calculate_eoo

test_that("EOO returns NA (not 0) when a hull cannot be built", {
  pts <- make_occ(c(4600000, 4600100), c(2900000, 2900100), c(2020, 2021))
  expect_true(is.na(calculate_eoo(pts)$area_km2))
  # Reconciliation then supplies AOO, per Guidelines 4.9.
  expect_equal(reconcile_eoo_aoo(calculate_eoo(pts)$area_km2, 8), 8)
})

test_that("EOO and AOO accept a closed year window", {
  pts <- make_occ(
    x = c(4600000, 4610000, 4620000, 4700000, 4710000, 4720000),
    y = c(2900000, 2901000, 2902000, 2950000, 2951000, 2952000),
    years = c(2001, 2002, 2003, 2021, 2022, 2023)
  )
  old <- suppressWarnings(calculate_aoo(pts, year_start = 2000, year_end = 2010))
  new <- suppressWarnings(calculate_aoo(pts, year_start = 2015, year_end = 2025))
  expect_equal(old$n_cells, 3)
  expect_equal(new$n_cells, 3)
})

# --------------------------------------------------------------- calculate_aoo

test_that("A pinned grid origin is honoured and reported back", {
  pts <- make_occ(c(4600000, 4610000), c(2900000, 2900100), c(2020, 2021))
  res <- suppressWarnings(calculate_aoo(pts, origin = c(500, 500)))
  expect_equal(unname(res$origin), c(500, 500))
})

test_that("A non-reference grid size warns", {
  pts <- make_occ(c(4600000, 4610000), c(2900000, 2900100), c(2020, 2021))
  expect_warning(calculate_aoo(pts, grid_size = 5000), "4.10.3")
  expect_silent(calculate_aoo(pts, grid_size = 2000))
})

# ------------------------------------------------------------- calculate_trend

test_that("Trend windows are equal length and adjacent by default", {
  pts <- make_occ(c(4600000, 4610000, 4620000), c(2900000, 2900100, 2900200),
                  c(2005, 2015, 2020))
  tr <- calculate_trend(pts, window_years = 10, year_now = 2025)
  expect_equal(tr$period_2_range, "2016 - 2025")
  expect_equal(tr$period_1_range, "2006 - 2015")
})

test_that("Trend compares both periods on one grid origin", {
  # Two identical clusters, one per period, offset by a whole number of
  # cells: AOO must come out unchanged. Minimising the origin separately per
  # period could report a spurious change.
  base_x <- c(4600000, 4600300, 4601900, 4603000)
  old <- make_occ(base_x, rep(2900000, 4), rep(2010, 4))
  new <- make_occ(base_x, rep(2900000, 4), rep(2020, 4))
  pts <- rbind(old, new)

  tr <- calculate_trend(pts, window_years = 10, year_now = 2025)
  expect_equal(tr$period_1_aoo, tr$period_2_aoo)
  expect_equal(tr$aoo_percent_change, 0)
})

test_that("Trend reports EOO change separately from AOO change", {
  pts <- rbind(
    make_occ(c(4600000, 4700000, 4650000), c(2900000, 2900000, 2990000), rep(2010, 3)),
    make_occ(c(4600000, 4610000, 4605000), c(2900000, 2900000, 2909000), rep(2020, 3))
  )
  tr <- calculate_trend(pts, window_years = 10, year_now = 2025)
  expect_true(tr$eoo_percent_change < -90)
  expect_false(identical(tr$eoo_percent_change, tr$aoo_percent_change))
})

test_that("Overlapping windows are flagged", {
  pts <- make_occ(c(4600000, 4610000, 4620000), c(2900000, 2900100, 2900200),
                  c(2005, 2015, 2020))
  expect_warning(
    calculate_trend(pts, recent_start = 2010, recent_end = 2026,
                    comp_start = 2000, comp_end = 2016),
    "overlap"
  )
})

# --------------------------------------------------------- calculate_locations

test_that("Locations cluster points within the threshold distance", {
  pts <- make_occ(
    x = c(4600000, 4601000, 4602000, 4700000, 4701000),
    y = c(2900000, 2900000, 2900000, 2900000, 2900000),
    years = rep(2020, 5)
  )
  expect_equal(calculate_locations(pts, threshold_dist = 5000), 2)
})

test_that("Repeat visits to one coordinate do not change the count", {
  pts <- make_occ(rep(4600000, 50), rep(2900000, 50), rep(2020, 50))
  expect_equal(calculate_locations(pts), 1)
})

test_that("Very large datasets fall back to grid centroids with a warning", {
  set.seed(3)
  n <- 600
  pts <- make_occ(runif(n, 4600000, 4900000), runif(n, 2800000, 3000000), rep(2020, n))
  expect_warning(calculate_locations(pts, max_points = 100), "grid-cell centroids")
  expect_gt(suppressWarnings(calculate_locations(pts, max_points = 100)), 0)
})

test_that("Empty input yields zero locations", {
  pts <- suppressWarnings(make_occ(numeric(0), numeric(0), numeric(0)))
  expect_equal(calculate_locations(pts), 0)
})

# ------------------------------------------------------- calculate_pop_metrics

pop_frame <- function(years, counts, lokal = "A", pocitano = "adulti") {
  data.frame(ROK = years, POCET = counts, POCITANO = pocitano,
             NAZ_LOKAL = lokal, stringsAsFactors = FALSE)
}

test_that("Missing abundance columns return NAs instead of erroring", {
  df <- data.frame(ROK = 2020:2024, NAZ_LOKAL = "A")
  res <- expect_silent(calculate_pop_metrics(df, year_now = 2025))
  expect_true(is.na(res$total_mature))
  expect_match(res$note, "abundance")
})

test_that("Decline is exponential, projected over the window, and floored", {
  # 20% per year for 10 observed years, assessed over a 10-year window.
  yrs <- 2016:2025
  df <- pop_frame(yrs, round(1000 * 0.8^(seq_along(yrs) - 1)))
  res <- calculate_pop_metrics(df, window_years = 10, year_now = 2025)
  # 1 - 0.8^10 = 89.3% reduction.
  expect_equal(res$decline_rate, -89.3, tolerance = 0.5)
  expect_gte(res$decline_rate, -100)
})

test_that("A monotonic crash is a reduction, not an extreme fluctuation", {
  yrs <- 2016:2025
  df <- pop_frame(yrs, round(1000 * 0.7^(seq_along(yrs) - 1)))
  res <- calculate_pop_metrics(df, window_years = 10, year_now = 2025)
  expect_gt(res$fluct_ratio, 10)     # the old max/min test would fire here
  expect_equal(res$fluct_reversals, 0)
  expect_false(res$extreme_fluctuation)
})

test_that("A genuine boom-bust series is diagnosed as extreme fluctuation", {
  yrs <- 2016:2025
  df <- pop_frame(yrs, c(1000, 50, 900, 40, 1100, 60, 950, 45, 1050, 55))
  res <- calculate_pop_metrics(df, window_years = 10, year_now = 2025)
  expect_gt(res$fluct_ratio, 10)
  expect_gte(res$fluct_reversals, 2)
  expect_true(res$extreme_fluctuation)
})

test_that("Pairs count as two mature individuals and sexes are summed", {
  df <- rbind(
    pop_frame(2025, 10, "A", "amplexus"),
    pop_frame(2025, 4, "B", "samci"),
    pop_frame(2025, 6, "B", "samice")
  )
  res <- calculate_pop_metrics(df, window_years = 10, year_now = 2025)
  expect_equal(res$total_mature, 20 + 10)
  expect_equal(res$max_subpop, 20)
})

test_that("Records outside the window are excluded", {
  df <- rbind(pop_frame(2000, 500, "A"), pop_frame(2024, 7, "A"))
  res <- calculate_pop_metrics(df, window_years = 10, year_now = 2025)
  expect_equal(res$total_mature, 7)
})

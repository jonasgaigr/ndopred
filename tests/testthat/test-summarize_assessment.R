test_that("Standard CR/EN/VU Logic works", {
  # Mock Inputs: CR Scenario (Trend > 80%)
  res <- summarize_assessment(
    species = "Test Species",
    eoo = list(area_km2 = 5000), aoo = list(area_km2 = 500), # Not CR spatial
    trend = list(percent_change = -90), # CR A
    locations = 10,
    pop_metrics = list(decline_rate = NA, fluct_ratio = NA, total_mature = NA, max_subpop = NA),
    evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res$result$Category, "CR")
  expect_match(res$result$Criteria, "A2")
})

test_that("Near Threatened (NT) is detected correctly", {
  # Create a full mock list to avoid NULL issues
  empty_pop <- list(decline_rate = NA, fluct_ratio = NA, total_mature = NA, max_subpop = NA)

  # Case A: NT via Criterion A (Decline 20-30%)
  res_a <- summarize_assessment(
    species = "NT A",
    eoo = list(area_km2 = 20000), aoo = list(area_km2 = 2000),
    trend = list(percent_change = -25),
    locations = 20,
    pop_metrics = empty_pop, evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res_a$result$Category, "NT")

  # Case B: NT via Criterion B
  res_b <- summarize_assessment(
    species = "NT B",
    eoo = list(area_km2 = 15000), aoo = list(area_km2 = 1500),
    trend = list(percent_change = 0),
    locations = 8,
    pop_metrics = empty_pop,
    evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res_b$result$Category, "NT")
})

test_that("Zero Occurrences (Absent in Window) defaults to DD", {
  empty_pop <- list(decline_rate = NA, fluct_ratio = NA, total_mature = NA, max_subpop = NA)
  res <- summarize_assessment(
    species = "Absent Species",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = -100),
    locations = 0, pop_metrics = empty_pop,
    evaluate_pop = FALSE, year_last = 2010, n_records = 10
  )
  expect_equal(res$result$Category, "DD")
  expect_match(res$result$Criteria, "Absent")
})

test_that("VU D2 requires Extant species", {
  empty_pop <- list(decline_rate = NA, fluct_ratio = NA, total_mature = NA, max_subpop = NA)
  # Case B: Invalid D2 (Zero area / Absent)
  res_invalid <- summarize_assessment(
    species = "D2 Invalid",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = 0), locations = 0,
    pop_metrics = empty_pop, evaluate_pop = FALSE, year_last = 2010, n_records = 10
  )
  expect_equal(res_invalid$result$Category, "DD")
})

test_that("Data Deficient (DD) triggers on low record count", {
  res <- summarize_assessment(
    species = "Rare Species",
    eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
    trend = list(percent_change = NA), locations = NA, pop_metrics = list(),
    evaluate_pop = FALSE, year_last = 2023,
    n_records = 2 # < 3 records
  )
  expect_equal(res$result$Category, "DD")
  expect_match(res$result$Criteria, "Insufficient Data")
})

test_that("Regionally Extinct (RE) triggers on time lag", {
  curr_year <- as.numeric(format(Sys.Date(), "%Y"))
  res <- summarize_assessment(
    species = "Ghost Species",
    eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
    trend = list(percent_change = NA), locations = NA, pop_metrics = list(),
    evaluate_pop = FALSE,
    year_last = curr_year - 60, # 60 years ago
    n_records = 10
  )
  expect_equal(res$result$Category, "RE")
})

test_that("Zero Occurrences (Absent in Window) defaults to DD", {
  # This tests the "Global Gate" we added: AOO = 0 means absent.
  res <- summarize_assessment(
    species = "Absent Species",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = -100), # Technically -100% trend
    locations = 0, pop_metrics = list(),
    evaluate_pop = FALSE, year_last = 2010, n_records = 10
  )

  # Should NOT be CR A2 (Decline) or CR B (Small Area).
  # It is extant=FALSE, so it skips A/B. Fallback is DD.
  expect_equal(res$result$Category, "DD")
  expect_match(res$result$Criteria, "Absent")
})

test_that("VU D2 requires Extant species", {
  # Case A: Valid D2 (Small area, alive)
  res_valid <- summarize_assessment(
    species = "D2 Valid",
    eoo = list(area_km2 = 10), aoo = list(area_km2 = 10), # < 20
    trend = list(percent_change = 0), locations = 3, # <= 5
    pop_metrics = list(), evaluate_pop = FALSE, year_last = 2023, n_records = 10
  )
  expect_equal(res_valid$result$Category, "VU")
  expect_match(res_valid$result$Criteria, "D2")

  # Case B: Invalid D2 (Zero area / Absent)
  res_invalid <- summarize_assessment(
    species = "D2 Invalid",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = 0), locations = 0,
    pop_metrics = list(), evaluate_pop = FALSE, year_last = 2010, n_records = 10
  )
  expect_false(res_invalid$result$Category == "VU")
  expect_equal(res_invalid$result$Category, "DD")
})

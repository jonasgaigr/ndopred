# Phase 0 gap tests (see methodology/gap_analysis.md).
#
# Each test asserts the IUCN-aligned behaviour for one problem found in the
# audit, so on the current code they FAIL. They are opt-in so that the default
# suite and R CMD check stay green until the fixes land:
#
#   Sys.setenv(NDOPRED_GAP_TESTS = "true"); devtools::test(filter = "gap-phase0")
#
# Test names start with the item number from section 4 of the agent prompt
# (GAP-01 ... GAP-15) or a new number for problems found during the audit
# (GAP-16 onwards). When a fix lands, its test moves to
# test-iucn-alignment.R, which runs in the regular suite.

skip_if_not(identical(Sys.getenv("NDOPRED_GAP_TESTS"), "true"),
            "Phase 0 gap tests are opt-in: set NDOPRED_GAP_TESTS=true")

# Fixtures are in helper-gap-fixtures.R; closed gaps have moved to
# test-iucn-alignment.R.

test_that("GAP-05b: the batch reduction window follows generation length", {
  # Criteria v3.1 A: "10 years or three generations, whichever is longer".
  local_mock_ndop()
  seen <- list()
  local_mocked_bindings(
    calculate_trend = function(occ, ...) {
      seen[[length(seen) + 1]] <<- list(...)
      list(percent_change = 0, eoo_percent_change = 0)
    },
    .package = "ndopred"
  )
  suppressMessages(batch_assess("Stable widespread sp", reference_year = gap_year, generation_length = 1))
  suppressMessages(batch_assess("Stable widespread sp", reference_year = gap_year, generation_length = 10))
  expect_false(identical(seen[[1]], seen[[2]]))
})

test_that("GAP-05c: past reductions (A2) are not capped at 100 years", {
  # Guidelines 5.2: "A maximum time cap is not applied to assessments based
  # on past reductions". app.R caps the A window at 100 for every A type.
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  shiny::testServer(gap_app_dir(), {
    session$setInputs(species_name = "Declining sp", window = 10,
                      gen_length = 40, use_pop = FALSE, run_calc = 1)
    expect_equal(raw_data()$a_window, 120)
  })
})

test_that("GAP-09c: total mature individuals is not a sum of maxima from different years", {
  # Site A had 100 adults in 2016 and 10 in 2025; site B had 10 in 2025.
  # The current count is 20; summing per-site maxima gives 110.
  occ <- data.frame(ROK = c(2016, 2025, 2025), POCET = c(100, 10, 10),
                    POCITANO = "adulti", NAZ_LOKAL = c("A", "A", "B"))
  res <- calculate_pop_metrics(occ, window_years = 10, year_now = 2025)
  expect_equal(res$total_mature, 20)
})

test_that("GAP-09d: alternating survey coverage is not an extreme fluctuation", {
  # Three sites counted in odd years, one site in even years: the yearly
  # total swings 30-fold because of which sites were visited (Guidelines 4.7:
  # be "reasonably certain" the change is in the total population).
  yrs <- 2016:2025
  occ <- do.call(rbind, lapply(yrs, function(y) {
    sites <- if (y %% 2 == 1) c("A", "B", "C") else "A"
    data.frame(ROK = y, POCET = if (y %% 2 == 1) 100 else 10,
               POCITANO = "adulti", NAZ_LOKAL = sites)
  }))
  res <- calculate_pop_metrics(occ, window_years = 10, year_now = 2025)
  expect_false(isTRUE(res$extreme_fluctuation))
})

# ----------------------------------------------------------- GAP-10: AOO

test_that("GAP-10a: positional precision changes AOO uncertainty", {
  # Guidelines 3.2 / 4.10.8: AOO needs a plausible range. A record with 3 km
  # positional error cannot be placed in one 2 x 2 km cell with certainty.
  mk <- function(prec) sf::st_as_sf(
    data.frame(x = c(4600500, 4610500), y = c(2900500, 2900500),
               CXPRESNOST = prec),
    coords = c("x", "y"), crs = 3035)
  expect_false(identical(calculate_aoo(mk(1))[c("area_km2", "n_cells")],
                         calculate_aoo(mk(3000))[c("area_km2", "n_cells")]))
})

test_that("GAP-10b: record geometries (lines/polygons) are requested, not just X/Y", {
  seen <- NULL
  testthat::local_mocked_bindings(
    ndop_download = function(...) { seen <<- list(...); NULL },
    .package = "rndop"
  )
  suppressMessages(get_assessment_data("Any sp"))
  expect_true(!is.null(seen$locations) && seen$locations > 0)
})

test_that("GAP-10d: observed AOO is reported as a lower bound with a range", {
  res <- gap_assess(eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400))
  expect_true(any(grepl("aoo.*(min|lower|max|upper)", names(res$result),
                        ignore.case = TRUE)))
})

# ---------------------------------------------- GAP-11: records are deleted

test_that("GAP-11a: no record is deleted; exclusions are flags", {
  testthat::local_mocked_bindings(ndop_download = function(...) gap_raw(),
                                  .package = "rndop")
  res <- get_assessment_data("Bombina bombina")
  expect_equal(nrow(res) + 0, nrow(gap_raw()) + 0)
})

test_that("GAP-11b: negative findings are kept as non-detections", {
  testthat::local_mocked_bindings(ndop_download = function(...) gap_raw(),
                                  .package = "rndop")
  res <- get_assessment_data("Bombina bombina")
  expect_true(2 %in% res$ID_NALEZ)
})

test_that("GAP-11c: a record validated as erroneous is flagged or excluded", {
  # VALIDACE = "chybný záznam" (erroneous record) is not read at all.
  testthat::local_mocked_bindings(ndop_download = function(...) gap_raw(),
                                  .package = "rndop")
  res <- get_assessment_data("Bombina bombina")
  expect_true(!(3 %in% res$ID_NALEZ) || "exclusion_reason" %in% names(res))
})

test_that("GAP-11d: records of a different taxon (hybrid) are flagged or excluded", {
  # rndop matches names with an unanchored grep: "Bombina bombina" also pulls
  # "Bombina bombina x variegata" (confirmed on the live ndop_list()).
  testthat::local_mocked_bindings(ndop_download = function(...) gap_raw(),
                                  .package = "rndop")
  res <- get_assessment_data("Bombina bombina")
  expect_true(!(7 %in% res$ID_NALEZ) || "exclusion_reason" %in% names(res))
})

test_that("GAP-11e: a record dated as a range carries its date precision", {
  # DATUM_OD 1990-01-01, DATUM_DO 2015-12-31 is filed under 1990 only.
  testthat::local_mocked_bindings(ndop_download = function(...) gap_raw(),
                                  .package = "rndop")
  res <- get_assessment_data("Bombina bombina")
  expect_true(any(grepl("date_prec|year_min|year_max|DATUM_DO", names(res))))
  expect_true(any(grepl("date_prec|year_min|year_max", names(add_year_column(res)))))
})

# --------------------------------------------------------- GAP-12: regional

test_that("GAP-12: regional eligibility and adjustment exist", {
  # Regional Guidelines v4.0 IV.2-IV.4: eligibility before (NA for
  # introduced/vagrant taxa), regional adjustment after.
  f <- names(formals(summarize_assessment))
  expect_true(any(grepl("eligib|native|regional|rescue", f)))
})

# ------------------------------------------------------ GAP-14: stale docs

test_that("GAP-14: README install instructions point at the real repository", {
  readme <- testthat::test_path("..", "..", "README.md")
  skip_if_not(file.exists(readme), "README not available (installed package)")
  expect_false(any(grepl("yourusername", readLines(readme, warn = FALSE))))
})

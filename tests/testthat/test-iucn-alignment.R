# Gaps from the Phase 0 audit (methodology/gap_analysis.md) that are now
# closed. Each test keeps its gap number; fixtures are in
# helper-gap-fixtures.R.

# -------------------------------------------------- GAP-01: one rule engine

test_that("GAP-01a: the expert panel and the engine agree on identical inputs", {
  # inst/shinyapp/app.R::expert_final() re-implements criterion C. Its C2
  # check uses the seeded a(i) checkbox at every level, ignoring the
  # level-specific caps (50/250/1000), so a taxon the engine puts in EN
  # C2a(i) is shown to the expert as CR before they have touched anything.
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  local_mocked_bindings(
    calculate_pop_metrics = function(...) list(
      total_mature = 200, max_subpop = 100, decline_rate = -30,
      decline_period_years = 10, extreme_fluctuation = FALSE, fluct_ratio = NA,
      n_records_used = 50L, n_records_counted = 50L, ignored_terms = integer(0)
    ),
    .package = "ndopred"
  )
  shiny::testServer(gap_app_dir(), {
    session$setInputs(species_name = "Stable widespread sp", window = 10,
                      gen_length = 1, use_pop = TRUE, run_calc = 1)
    expect_equal(expert_final()$category, auto_assessment()$result$Category)
  })
})

test_that("GAP-01b: no UI file carries its own category thresholds", {
  app_src <- readLines(file.path(gap_app_dir(), "app.R"), warn = FALSE)
  # Criterion A, B and D1 thresholds re-typed in the app.
  expect_false(any(grepl("red_val >= 80|t_vu <- if \\(type == \"B1\"\\) 20000|rv\\$pop_d1 < 50",
                         app_src)))
})

# ------------------------------------- GAP-02: clusters are not locations

test_that("GAP-02: B(a) resting on a cluster count says threat-defined locations are needed", {
  # Guidelines 4.11: locations are threat-defined; "in the absence of any
  # plausible threat ... the subcriteria that refer to the number of
  # locations will not be met". The pipeline passes 5 km clusters as the
  # location count; a proposal may use them, labelled as an indication.
  local_gap_ndop()
  res <- assess_species("Effort drop sp", recent_start = gap_year - 9,
                        recent_end = gap_year, comp_start = gap_year - 19,
                        comp_end = gap_year - 10)
  expect_match(res$Criteria, "B1a")          # the proposal itself is kept
  expect_true(needs(res$Needs_Evidence, "B\\(a\\)"))
})

# ------------------------- GAP-03: B(b) from a raw two-window comparison

test_that("GAP-03: B(b) from a raw window comparison says continuing decline needs evidence", {
  # Same range in both windows, five times fewer records recently. Observed
  # AOO and convex-hull EOO fall, and the engine asserts B(b)(i,ii).
  # Guidelines 4.10.8 (AOO depends on effort), 4.9 (convex hulls unsuitable
  # for temporal comparison), 4.6 (decline must be liable to continue).
  # The proposal may keep b(i,ii) as an indication, but must say so.
  local_gap_ndop()
  res <- assess_species("Effort drop sp", recent_start = gap_year - 9,
                        recent_end = gap_year, comp_start = gap_year - 19,
                        comp_end = gap_year - 10)
  expect_true(needs(res$Needs_Evidence, "B\\(b\\)"))
  expect_false(identical(res$Support, "evidence"))
})

# ---------------------------------------------------------- GAP-04: D2

test_that("GAP-04a: an unknown D2 threat is UNKNOWN, not LC", {
  # Three-valued logic: no threat information is not evidence of no threat.
  expect_false(identical(assess_criterion_d2(aoo_km2 = 10, locations = 3), "LC"))
})

test_that("GAP-04b: a restricted taxon with no threat information is not LC", {
  res <- gap_assess(eoo = list(area_km2 = 16), aoo = list(area_km2 = 16))
  expect_false(identical(res$result$Category, "LC"))
})

test_that("GAP-04c: VU D2 needs a specified threat, not a bare TRUE", {
  # Guidelines 8: the stochastic events or human activities "must be
  # specified in the justification for listing".
  res <- suppressWarnings(gap_assess(eoo = list(area_km2 = 16), aoo = list(area_km2 = 16),
                                     locations = 3, d2_threat = TRUE))
  expect_false(isTRUE(res$details$d2_flag))
  expect_false(res$result$Category == "VU")
})

test_that("GAP-04d: D2 resting on a cluster count says threat-defined locations are needed", {
  # A restricted taxon with a described threat: the D2 restriction comes from
  # 5 km clusters standing in for locations, so locations still need evidence.
  local_mock_ndop()
  res <- assess_species("Restricted stable sp", recent_start = gap_year - 9,
                        recent_end = gap_year, comp_start = gap_year - 19,
                        comp_end = gap_year - 10,
                        d2_threat = "development of the only known site")
  expect_true(needs(res$Needs_Evidence, "D2"))
})

# ------------------------------------------------ GAP-05: criterion A data

test_that("GAP-05a: A2c from a raw window comparison says the reduction needs evidence", {
  # Review section 7.1 / Table 5: a before/after count of cells is T0/T1 data.
  # The proposal may keep A2c as an indication, but must say so.
  local_gap_ndop()
  res <- assess_species("Effort drop sp", recent_start = gap_year - 9,
                        recent_end = gap_year, comp_start = gap_year - 19,
                        comp_end = gap_year - 10)
  expect_match(res$Criteria, "A2c")          # the proposal itself is kept
  expect_true(needs(res$Needs_Evidence, "A2"))
})

# ------------------------------------------------------------ GAP-06: RE

test_that("GAP-06: an RE proposal from a long gap lists survey evidence as needed", {
  # Regional Guidelines v4.0 III.14 / IV.3: RE needs "no reasonable doubt",
  # which depends on search effort; Guidelines 11.1-11.3. RE may be proposed
  # as a candidate, never without saying that surveys must be documented.
  res <- gap_assess(eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
                    trend = list(percent_change = NA),
                    year_last = gap_year - 60, n_records = 10)
  expect_true(needs(res$result$Needs_Evidence, "RE"))

  local_mock_ndop()
  ghost <- suppressMessages(batch_assess("Ghost sp", reference_year = gap_year))
  expect_true(needs(ghost$Needs_Evidence, "RE"))
})

# ------------------------------------------------------------ GAP-07: DD

test_that("GAP-07a: few records are not DD by default", {
  # Guidelines 10.3/10.4: DD needs all five criteria documented; 8.1: a taxon
  # known from very few records may be CR.
  res <- gap_assess(eoo = list(area_km2 = NA), aoo = list(area_km2 = 8),
                    n_records = 2)
  expect_false(identical(res$result$Category, "DD"))
})

test_that("GAP-07b: absence from the recent window is not DD by default", {
  res <- gap_assess(eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
                    trend = list(percent_change = -100),
                    year_last = gap_year - 12, n_records = 10)
  expect_false(identical(res$result$Category, "DD"))
})

test_that("GAP-07c: a taxon with no records at all is NE, not DD", {
  # Guidelines 10.4: "If all of the five criteria have not been considered,
  # DD cannot be used (the taxon must be categorized as NE)."
  local_mock_ndop()
  res <- suppressMessages(assess_species(
    "Unknown sp", recent_start = gap_year - 9, recent_end = gap_year,
    comp_start = gap_year - 19, comp_end = gap_year - 10
  ))
  expect_false(identical(res$Category, "DD"))
})

# ---------------------------------------------- GAP-08: missing data -> LC

test_that("GAP-08a: an LC proposal with an unknown trend says criterion A needs evidence", {
  # A widespread taxon may be proposed as LC (rapid-LC triage, review section
  # 3), but an unknown trend must not pass silently as "no reduction".
  res <- gap_assess(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
                    trend = list(percent_change = NA))
  expect_true(needs(res$result$Needs_Evidence, "\\bA[1-4]?\\b"))
})

test_that("GAP-08b: review taxon Y (EOO 1,700, AOO 84, nothing evidenced) is not LC", {
  # Review Table 6: "Z malého EOO/AOO NELZE sám odvodit EN; nutné doplnit
  # evidenci" - and equally it cannot be closed as LC.
  res <- gap_assess(eoo = list(area_km2 = 1700), aoo = list(area_km2 = 84),
                    trend = list(percent_change = NA), locations = NA)
  expect_false(identical(res$result$Category, "LC"))
})

# ------------------------------------------------------------ GAP-09: C

test_that("GAP-09a: C1 projections are capped at 100 years", {
  # Criteria v3.1 C1: "(up to a maximum of 100 years in the future)". With a
  # 40-year generation the VU period is 120 years uncapped, 100 capped; a 1%
  # per decade decline crosses 10% only on the uncapped period.
  res <- gap_assess(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
                    evaluate_pop = TRUE, generation_length = 40,
                    pop_metrics = list(total_mature = 5000, max_subpop = 1000,
                                       decline_rate = -1, decline_period_years = 10,
                                       extreme_fluctuation = FALSE))
  expect_false(grepl("C1", res$result$Criteria))
})

test_that("GAP-09b: C2a(i) from NDOP localities says subpopulations need evidence", {
  # Guidelines 4.2: subpopulations have little demographic or genetic
  # exchange. max_subpop is the largest per-locality count, an indication.
  res <- gap_assess(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
                    evaluate_pop = TRUE,
                    pop_metrics = list(total_mature = 240, max_subpop = 40,
                                       decline_rate = -20, decline_period_years = 10,
                                       extreme_fluctuation = FALSE))
  expect_match(res$result$Criteria, "C2a\\(i\\)")   # the proposal itself is kept
  expect_true(needs(res$result$Needs_Evidence, "C2a"))
})

test_that("GAP-10c: B2 always uses 2 x 2 km AOO, whatever the display grid", {
  # Guidelines 2.3.1 / 4.10.3: thresholds apply only to AOO at the reference
  # scale. Another cell size may be shown, but must not drive the proposal.
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  df2 <- gap_run_bulk("Stable widespread sp", cell_size = 2)$results
  df5 <- gap_run_bulk("Stable widespread sp", cell_size = 5)$results
  expect_equal(df5$`AOO nový (km2)`, df2$`AOO nový (km2)`)
  expect_equal(df5$`Kategorie (automatická)`, df2$`Kategorie (automatická)`)
})

test_that("GAP-13a: B1 and B2 together are written B1...+2...", {
  # Criteria v3.1 Annex 2: "VU B1ab(iii)+2ab(iii)".
  res <- gap_assess(eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400),
                    trend = list(percent_change = -10), locations = 4)
  expect_equal(res$result$Criteria_IUCN, "B1ab(ii)+2ab(ii)")
})

test_that("GAP-13b: review taxon X is EN B1ab(iii)+2ab(iii)", {
  res <- gap_assess(eoo = list(area_km2 = 1700), aoo = list(area_km2 = 84),
                    locations = 2, habitat_decline = TRUE)
  expect_equal(paste(res$result$Category, res$result$Criteria_IUCN),
               "EN B1ab(iii)+2ab(iii)")
})

test_that("GAP-13c: criterion D under CR and EN is written D, not D1", {
  # Annex 2: "CR D", "EN A2c; D"; Guidelines 8.2: "Critically Endangered: D".
  res <- gap_assess(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
                    locations = 40, evaluate_pop = TRUE,
                    pop_metrics = list(total_mature = 200, max_subpop = 200,
                                       decline_rate = NA, extreme_fluctuation = FALSE))
  expect_equal(paste(res$result$Category, res$result$Criteria_IUCN), "EN D")
})

test_that("GAP-13d: C2a(i) and a(ii) together are written C2a(i,ii)", {
  res <- gap_assess(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
                    locations = 40, evaluate_pop = TRUE,
                    pop_metrics = list(total_mature = 40, max_subpop = 40,
                                       decline_rate = -20, decline_period_years = 10,
                                       extreme_fluctuation = FALSE))
  expect_true(has_fixed(res$result$Criteria_IUCN, "C2a(i,ii)"))
})

test_that("GAP-13e: VU D1 and D2 together are written D1+2", {
  # Annex 2: "VU D1+2".
  res <- gap_assess(eoo = list(area_km2 = 12), aoo = list(area_km2 = 12),
                    locations = 3, d2_threat = "introduced predator on the only island",
                    evaluate_pop = TRUE,
                    pop_metrics = list(total_mature = 900, max_subpop = 900,
                                       decline_rate = NA, extreme_fluctuation = FALSE))
  expect_true(has_fixed(res$result$Criteria_IUCN, "D1+2"))
})

# ---------------------------------------- GAP-15: empty fields need reasons

test_that("GAP-15a: a download error is not reported as DD 'No records found'", {
  local_mocked_bindings(get_assessment_data = function(...) stop("timeout"),
                        plot_iucn = function(...) NULL, .package = "ndopred")
  df <- gap_run_bulk("Declining sp")$results
  expect_false(identical(df$`Kategorie (automatická)`, "DD"))
  expect_true(any(grepl("^issue_|důvod", names(df), ignore.case = TRUE)))
})

test_that("GAP-15b: an empty metric carries a reason code", {
  local_mock_ndop()
  local_mocked_bindings(calculate_eoo = function(...) stop("hull failed"),
                        plot_iucn = function(...) NULL, .package = "ndopred")
  df <- gap_run_bulk("Declining sp")$results
  expect_true(any(grepl("^issue_", names(df))))
})

test_that("GAP-15c: batch_assess keeps a row for a taxon that errored", {
  local_mock_ndop()
  real <- get_assessment_data
  local_mocked_bindings(
    get_assessment_data = function(sp, ...) if (sp == "Ghost sp") stop("timeout") else real(sp, ...),
    .package = "ndopred"
  )
  res <- suppressMessages(batch_assess(c("Stable widespread sp", "Ghost sp"), reference_year = gap_year))
  expect_equal(nrow(res), 2)
})

# ----------------------------------------------- new findings from the audit

test_that("GAP-16: bulk and single-taxon modes compute the same B2 AOO", {
  # The bulk app pins the recent window's AOO to the origin minimised over
  # both windows; assess_species() and app.R minimise over the recent window
  # alone (Guidelines 4.10.2: "the minimum estimate should be used").
  local_gap_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  bulk <- gap_run_bulk("Origin sp")$results$`AOO nový (km2)`
  single <- assess_species("Origin sp", recent_start = gap_year - 9,
                           recent_end = gap_year, comp_start = gap_year - 19,
                           comp_end = gap_year - 10)$AOO_km2
  expect_equal(bulk, single)
})

test_that("GAP-17: the assessment reference year is an explicit argument", {
  # summarize_assessment() reads Sys.Date() for the RE screen, so the same
  # inputs give different results in different years.
  expect_true("reference_year" %in% names(formals(summarize_assessment)))
  expect_true("reference_year" %in% names(formals(batch_assess)))
})

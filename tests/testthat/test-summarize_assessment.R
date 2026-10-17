empty_pop <- function() {
  list(decline_rate = NA, fluct_ratio = NA, extreme_fluctuation = FALSE,
       total_mature = NA, max_subpop = NA)
}

# summarize_assessment() takes an explicit reference year since 0.3.0; the
# scenarios below are set in 2026 unless a test says otherwise.
sa <- function(..., reference_year = 2026) {
  summarize_assessment(..., reference_year = reference_year)
}

# Expert evidence that nothing in B(b) or B(c) applies.
no_b_or_c <- list(decline = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE, v = FALSE),
                  fluctuation = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE))

test_that("Standard CR/EN/VU Logic works", {
  # Mock Inputs: CR Scenario (Trend > 80%)
  res <- sa(
    species = "Test Species",
    eoo = list(area_km2 = 5000), aoo = list(area_km2 = 500), # Not CR spatial
    trend = list(percent_change = -90), # CR A
    locations = 10,
    pop_metrics = empty_pop(),
    evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res$result$Category, "CR")
  expect_match(res$result$Criteria, "A2")
})

test_that("A1 thresholds are used when criterion A1 is selected", {
  args <- list(
    species = "Reversible decline",
    eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
    trend = list(percent_change = -85), locations = 40,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  # 85% reduction: CR under A2 (>=80), only EN under A1 (>=70, <90).
  expect_equal(do.call(sa, args)$result$Category, "CR")
  expect_equal(
    do.call(sa, c(args, list(a_criteria = "A1")))$result$Category,
    "EN"
  )
})

test_that("Near Threatened (NT) is detected correctly", {
  # Case A: NT via Criterion A (Decline 20-30%)
  res_a <- sa(
    species = "NT A",
    eoo = list(area_km2 = 20000), aoo = list(area_km2 = 2000),
    trend = list(percent_change = -25),
    locations = 20,
    pop_metrics = empty_pop(), evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res_a$result$Category, "NT")
  expect_match(res_a$result$Criteria, "nearly meets")

  # Case B: NT via Criterion B (area threshold met, only 1 of 3 conditions)
  res_b <- sa(
    species = "NT B",
    eoo = list(area_km2 = 15000), aoo = list(area_km2 = 1500),
    trend = list(percent_change = 0),
    locations = 8,
    pop_metrics = empty_pop(),
    evaluate_pop = FALSE, year_last = 2023, n_records = 50
  )
  expect_equal(res_b$result$Category, "NT")
})

test_that("NT criteria strings never read as met criteria", {
  # An NT taxon must not be reported as though it satisfied A2c: Near
  # Threatened is defined by proximity to VU, not by its own criteria.
  res <- sa(
    species = "NT wording",
    eoo = list(area_km2 = 30000), aoo = list(area_km2 = 3000),
    trend = list(percent_change = -22), locations = 40,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  expect_equal(res$result$Category, "NT")
  expect_false(grepl("^A2", res$result$Criteria))
  expect_match(res$result$Criteria, "nearly meets A2")
})

test_that("Restricted range with no threats is not Near Threatened", {
  # Guidelines 10.1 lists this exact case among those that should NOT be NT:
  # "meets the area requirements under criterion B for CR ... but is not
  # declining, not severely fragmented, there are no extreme fluctuations,
  # and there are no obvious threats." Those are statements of evidence.
  res <- sa(
    species = "Tiny but stable",
    eoo = list(area_km2 = 80), aoo = list(area_km2 = 8),
    trend = list(percent_change = 0), locations = 30,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50,
    severely_fragmented = FALSE, d2_threat = FALSE,
    evidence = c(no_b_or_c, list(reduction = 0))
  )
  expect_equal(res$result$Category, "LC")
  expect_equal(res$result$Support, "evidence")
})

test_that("Restricted range with nothing evidenced is flagged uncertain up to CR (D7)", {
  # The same taxon before anyone has looked: whether it is declining,
  # fragmented or threatened is unknown, so the plausible range reaches CR.
  # Unknown is not "no": the best estimate is proposed, flagged as uncertain
  # (AOPK, D7), and NT or DD is left to the assessor (Guidelines 10.1, 10.4).
  res <- sa(
    species = "Tiny, unknown",
    eoo = list(area_km2 = 80), aoo = list(area_km2 = 8),
    trend = list(percent_change = NA), locations = NA,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  expect_equal(res$result$Category, "LC")
  expect_equal(res$result$Category_Range, "LC-CR")
  expect_true(res$result$Uncertain)
  expect_match(res$result$Flags, "NT may be chosen")
  expect_match(res$result$Flags, "DD candidate")
  expect_match(res$result$Needs_Evidence, "B(a)", fixed = TRUE)
})

test_that("Criterion B needs two of three conditions", {
  base <- list(
    species = "B test",
    eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400),
    locations = 4, pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  # (a) few locations only -> 1 of 3 -> NT
  one <- do.call(sa, c(base, list(trend = list(percent_change = 0))))
  expect_equal(one$result$Category, "NT")

  # (a) few locations + (b) continuing decline in AOO -> 2 of 3 -> EN
  two <- do.call(sa, c(base, list(trend = list(percent_change = -5))))
  expect_equal(two$result$Category, "EN")
  expect_match(two$result$Criteria, "B1ab\\(ii\\)")
})

test_that("Criterion B subindices reflect only what was measured", {
  res <- sa(
    species = "Indices",
    eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400), locations = 4,
    trend = list(percent_change = -10, eoo_percent_change = -8),
    locations_trend = -20,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  # i (EOO), ii (AOO) and iv (locations) were measured; iii (habitat) was not.
  expect_equal(res$details$b_indices, c("i", "ii", "iv"))
  expect_false(grepl("iii", res$result$Criteria))

  # b(iii) only appears when the assessor asserts a habitat decline.
  res_hab <- sa(
    species = "Indices",
    eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400), locations = 4,
    trend = list(percent_change = -10), habitat_decline = TRUE,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  expect_true("iii" %in% res_hab$details$b_indices)
})

test_that("Severe fragmentation can satisfy condition (a) without few locations", {
  res <- sa(
    species = "Fragmented",
    eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400), locations = 40,
    trend = list(percent_change = -5), severely_fragmented = TRUE,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 50
  )
  expect_equal(res$result$Category, "EN")
  expect_match(res$result$Criteria, "B1ab")
})

test_that("A taxon gone from the current window is a DD candidate, not CR A2c", {
  # Recorded in the comparison window, absent from the current one: the raw
  # AOO change is -100 %, but with a window empty no trend can be estimated
  # (trend_estimate()), so it is no reduction under A. The 2026-10-17 bulk
  # run proposed nine such taxa as CR A2c, two of them on a single record.
  res <- sa(
    species = "Absent Species",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = -100),
    locations = 0, pop_metrics = empty_pop(),
    evaluate_pop = FALSE, year_last = 2010, n_records = 1
  )
  expect_equal(res$result$Category, "DD")
  expect_equal(res$result$Support, "candidate")
  expect_false(grepl("A2", res$result$Criteria))
  expect_match(res$result$Needs_Evidence, "current occurrence")
})

test_that("A taxon absent from the current window with nothing else is a DD candidate", {
  res <- sa(
    species = "Absent, no trend",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = NA),
    locations = 0, pop_metrics = empty_pop(),
    evaluate_pop = FALSE, year_last = 2010, n_records = 10
  )
  expect_equal(res$result$Category, "DD")
  expect_equal(res$result$Support, "candidate")
  expect_match(res$result$Criteria, "Absent")
})

test_that("Unknown AOO is a DD candidate, not a DD finding", {
  # Guidelines 10.3-10.4: DD is the assessor's decision; the engine only
  # flags a candidate and says what is missing.
  res <- sa(
    species = "Rare Species",
    eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
    trend = list(percent_change = NA), locations = NA, pop_metrics = list(),
    evaluate_pop = FALSE, year_last = 2023,
    n_records = 2
  )
  expect_equal(res$result$Category, "DD")
  expect_equal(res$result$Support, "candidate")
  expect_match(res$result$Criteria, "Inadequate information")
  expect_match(res$result$Needs_Evidence, "could not be computed")
})

test_that("An unknown number of locations does not crash the cascade", {
  # locations = NA previously produced sum(NA, ...) inside evaluate_b() and
  # failed with "missing value where TRUE/FALSE needed".
  res <- sa(
    species = "Unknown locations",
    eoo = list(area_km2 = 1000), aoo = list(area_km2 = 100),
    trend = list(percent_change = -5), locations = NA,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2024, n_records = 50
  )
  expect_true(res$result$Category %in% c("LC", "NT", "VU", "EN", "CR"))
  expect_false(res$details$loc_flag)
})

test_that("A long gap since the last record makes an RE candidate, not an RE finding", {
  # Regional Guidelines v4.0 III.14: RE needs "no reasonable doubt", which
  # depends on surveys; the year threshold is a labelled convention.
  res <- sa(
    species = "Ghost Species",
    eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
    trend = list(percent_change = NA), locations = NA, pop_metrics = list(),
    evaluate_pop = FALSE,
    year_last = 1966, n_records = 10, reference_year = 2026
  )
  expect_equal(res$result$Category, "RE")
  expect_equal(res$result$Support, "candidate")
  expect_true(res$details$re_screened)
  expect_match(res$result$Needs_Evidence, "RE: documented surveys")

  # The threshold is a parameter, not a hard-coded constant.
  res70 <- sa(
    species = "Ghost Species",
    eoo = list(area_km2 = 100), aoo = list(area_km2 = 20),
    trend = list(percent_change = 0), locations = 10, pop_metrics = empty_pop(),
    evaluate_pop = FALSE, year_last = 1966, n_records = 10,
    re_threshold_years = 70, reference_year = 2026
  )
  expect_false(res70$result$Category == "RE")

  # The same inputs in a different reference year give the same answer
  # only if the year is the same: the clock is never read.
  later <- sa(
    species = "Ghost Species",
    eoo = list(area_km2 = NA), aoo = list(area_km2 = NA),
    trend = list(percent_change = NA), locations = NA, pop_metrics = list(),
    evaluate_pop = FALSE, year_last = 1966, n_records = 10, reference_year = 2010
  )
  expect_false(later$result$Category == "RE")
})

test_that("D2 requires a specified plausible threat", {
  base <- list(
    species = "Restricted",
    eoo = list(area_km2 = 10), aoo = list(area_km2 = 10),
    trend = list(percent_change = 0), locations = 3,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2023, n_records = 10
  )

  # Guidelines section 8: meeting the AOO/location figures "is not
  # sufficient"; a plausible threat capable of driving the taxon to CR or EX
  # in a very short time is also required, and it must be specified.
  no_threat <- do.call(sa, base)
  expect_false(no_threat$result$Category == "VU")
  expect_true(no_threat$details$d2_candidate)
  expect_false(no_threat$details$d2_flag)
  expect_match(no_threat$result$Needs_Evidence, "D2: plausible threat not assessed")

  expect_warning(bare <- do.call(sa, c(base, list(d2_threat = TRUE))), "does not specify")
  expect_false(bare$details$d2_flag)
  expect_match(bare$result$Needs_Evidence, "describe the plausible threat")

  with_threat <- do.call(sa, c(base, list(d2_threat = "drainage of the only fen")))
  expect_equal(with_threat$result$Category, "VU")
  expect_match(with_threat$result$Criteria, "D2")
  expect_true(with_threat$details$d2_flag)

  # No plausible threat at all: D2 is not met, and neither are the location
  # subcriteria (Guidelines 4.11).
  none <- do.call(sa, c(base, list(d2_threat = FALSE)))
  expect_false(none$details$d2_candidate)
  expect_false(grepl("D2", none$result$Criteria))
})

test_that("A widespread stable species is not swept up by D2", {
  # EOO covers a quarter of Czechia, AOO 420 km2, no decline, no
  # fluctuations. Under the literal D2 reading this was automatically VU;
  # it should be NT (area threshold plus one of three B conditions,
  # Guidelines 10.1) with the D2 restriction flagged for expert review.
  res <- sa(
    species = "Widespread stable",
    eoo = list(area_km2 = 20904), aoo = list(area_km2 = 420),
    trend = list(percent_change = 0), locations = 5,
    pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2025, n_records = 250
  )
  expect_equal(res$result$Category, "NT")
  expect_false(res$details$d2_flag)
  expect_true(res$details$d2_candidate)
  expect_match(res$result$Criteria, "D2 candidate")
})

test_that("VU D2 still requires the taxon to be extant", {
  res_invalid <- sa(
    species = "D2 Invalid",
    eoo = list(area_km2 = 0), aoo = list(area_km2 = 0),
    trend = list(percent_change = 0), locations = 0,
    pop_metrics = empty_pop(), evaluate_pop = FALSE, year_last = 2010,
    n_records = 10, d2_threat = "drainage"
  )
  expect_false(res_invalid$result$Category == "VU")
  expect_equal(res_invalid$result$Category, "DD")
})

test_that("C2a(ii) uses the category-specific proportion thresholds", {
  # 92% of mature individuals in the largest subpopulation: enough for CR
  # (>=90%) but not for EN (>=95%) or VU (100%).
  pop <- list(decline_rate = -5, extreme_fluctuation = FALSE,
              total_mature = 200, max_subpop = 184, decline_period_years = 10)
  res_cr <- sa(
    species = "C2aii CR", eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
    trend = list(percent_change = 0), locations = 40, pop_metrics = pop,
    evaluate_pop = TRUE, year_last = 2023, n_records = 50
  )
  expect_equal(res_cr$result$Category, "CR")
  expect_match(res_cr$result$Criteria, "C2a\\(ii\\)")

  # Same proportion at an EN-sized population must not qualify under a(ii);
  # max_subpop of 1,840 also exceeds the EN a(i) cap of 250.
  pop_en <- list(decline_rate = -5, extreme_fluctuation = FALSE,
                 total_mature = 2000, max_subpop = 1840, decline_period_years = 10)
  res_en <- sa(
    species = "C2aii EN", eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
    trend = list(percent_change = 0), locations = 40, pop_metrics = pop_en,
    evaluate_pop = TRUE, year_last = 2023, n_records = 50
  )
  expect_false(grepl("C2a\\(ii\\)", res_en$result$Criteria))
})

test_that("C2 is not satisfied by a purely spatial decline", {
  # v3.1 criterion C2: "a continuing decline ... in numbers of mature
  # individuals". An AOO trend alone must not trigger it.
  pop <- list(decline_rate = NA, extreme_fluctuation = FALSE,
              total_mature = 200, max_subpop = 200, decline_period_years = 10)
  res <- sa(
    species = "Spatial only", eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
    trend = list(percent_change = -40), locations = 40, pop_metrics = pop,
    evaluate_pop = TRUE, year_last = 2023, n_records = 50
  )
  expect_false(grepl("C2", res$result$Criteria))
  # It is still EN under D1 (<250 mature individuals) and A2c.
  expect_true(grepl("D1", res$result$Criteria))
})

test_that("C1 scales the observed decline to each category's reference period", {
  # 25% decline measured over 10 years. CR C1 asks for 25% within 3 years
  # (1 generation), which the same annual rate does not reach; VU C1 asks for
  # 10% within 10 years, which it does.
  pop <- list(decline_rate = -25, extreme_fluctuation = FALSE,
              total_mature = 200, max_subpop = 200, decline_period_years = 10)
  res <- sa(
    species = "C1 scaling", eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
    trend = list(percent_change = 0), locations = 40, pop_metrics = pop,
    evaluate_pop = TRUE, year_last = 2023, n_records = 50, generation_length = 1
  )
  # Not CR C1: a 25%/10yr rate is only ~8.3% over 3 years.
  expect_false(grepl("C1", res$result$Criteria))
})

test_that("Small population NT is judged against the D1 threshold, not 10,000", {
  # Guidelines 10.1: "about 1,500 mature individuals" -> NT;
  # "more than 2,000 mature individuals" -> not NT.
  mk <- function(n) list(decline_rate = NA, extreme_fluctuation = FALSE,
                         total_mature = n, max_subpop = n, decline_period_years = 10)
  args <- list(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
               trend = list(percent_change = 0), locations = 40,
               evaluate_pop = TRUE, year_last = 2023, n_records = 500)

  res_nt <- do.call(sa,
                    c(list(species = "1500", pop_metrics = mk(1500)), args))
  expect_equal(res_nt$result$Category, "NT")
  expect_match(res_nt$result$Criteria, "small population")

  # The 10.1 example assumes nothing else is met or nearly met, i.e. that
  # there is no continuing decline. Given as evidence, 9,000 is LC...
  res_lc <- do.call(sa,
                    c(list(species = "9000", pop_metrics = mk(9000),
                           evidence = list(decline = c(v = FALSE))), args))
  expect_equal(res_lc$result$Category, "LC")

  # ...but with the decline unknown and every counted individual in one
  # locality, VU C2a(ii) is plausible: LC, flagged uncertain (D7; 10.1).
  res_unknown <- do.call(sa,
                         c(list(species = "9000?", pop_metrics = mk(9000)), args))
  expect_equal(res_unknown$result$Category, "LC")
  expect_true(res_unknown$result$Uncertain)
  expect_equal(res_unknown$result$Category_Range, "LC-VU")
  expect_match(res_unknown$result$Needs_Evidence, "C2: continuing decline")
})

test_that("Extreme fluctuations use the diagnosed flag, not a raw ratio", {
  # A steep monotonic decline has a large max/min ratio but is a reduction,
  # not a fluctuation (Guidelines 4.6, 4.7).
  pop <- list(decline_rate = -60, fluct_ratio = 30, extreme_fluctuation = FALSE,
              total_mature = 5000, max_subpop = 5000, decline_period_years = 10)
  res <- sa(
    species = "Monotonic crash",
    eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400), locations = 40,
    trend = list(percent_change = 0), pop_metrics = pop,
    evaluate_pop = TRUE, year_last = 2023, n_records = 500
  )
  expect_false(grepl("c\\(iv\\)", res$result$Criteria))
  expect_equal(res$details$c_indices, character(0))
})

test_that("D1 thresholds are applied on mature individuals", {
  mk <- function(n) list(decline_rate = NA, extreme_fluctuation = FALSE,
                         total_mature = n, max_subpop = n, decline_period_years = 10)
  args <- list(eoo = list(area_km2 = 50000), aoo = list(area_km2 = 5000),
               trend = list(percent_change = 0), locations = 40,
               evaluate_pop = TRUE, year_last = 2023, n_records = 500)
  expect_equal(do.call(sa, c(list(species = "a", pop_metrics = mk(40)), args))$result$Category, "CR")
  expect_equal(do.call(sa, c(list(species = "b", pop_metrics = mk(200)), args))$result$Category, "EN")
  expect_equal(do.call(sa, c(list(species = "c", pop_metrics = mk(900)), args))$result$Category, "VU")
})

test_that("evaluate_pop governs every use of the abundance data", {
  # The switch is a judgement about whether the counts are fit to be used at
  # all. A trend from thin counts is no more trustworthy than a total from
  # them, so criterion A basis (b) must be gated alongside C, D1, B(b)(v) and
  # B(c)(iv) - otherwise criterion A is driven by data the same call has just
  # declared unusable.
  pop <- list(decline_rate = -41.5, extreme_fluctuation = TRUE,
              total_mature = 40, max_subpop = 40, decline_period_years = 10)
  args <- list(
    species = "Abundance decline",
    eoo = list(area_km2 = 78669), aoo = list(area_km2 = 1788),
    trend = list(percent_change = -1, eoo_percent_change = -1),
    locations = 208, pop_metrics = pop,
    year_last = 2026, n_records = 23513
  )
  off <- do.call(sa, c(args, list(evaluate_pop = FALSE)))
  on  <- do.call(sa, c(args, list(evaluate_pop = TRUE)))

  # With the counts refused: no A2b, no b(v), no c(iv), no D1.
  expect_false(grepl("A2b", off$result$Criteria))
  expect_false("v" %in% off$details$b_indices)
  expect_equal(off$details$c_indices, character(0))
  expect_false(grepl("D1", off$result$Criteria))

  # With the counts accepted: all four become available.
  expect_true("v" %in% on$details$b_indices)
  expect_true("iv" %in% on$details$c_indices)
  expect_equal(on$result$Category, "CR")     # D1: fewer than 50 mature
  expect_match(on$result$Criteria, "D1")
})

test_that("Criterion A falls back to the spatial basis when counts are refused", {
  # A steep count-based decline plus a mild spatial one: with the counts
  # refused, criterion A rests on the spatial trend alone, i.e. basis (c).
  pop <- list(decline_rate = -85, extreme_fluctuation = FALSE,
              total_mature = 24541, max_subpop = 1000, decline_period_years = 10)
  args <- list(
    species = "Two signals",
    eoo = list(area_km2 = 78669), aoo = list(area_km2 = 5788),
    trend = list(percent_change = -35, eoo_percent_change = -30),
    locations = 208, pop_metrics = pop,
    year_last = 2026, n_records = 23513
  )
  off <- do.call(sa, c(args, list(evaluate_pop = FALSE)))
  on  <- do.call(sa, c(args, list(evaluate_pop = TRUE)))

  expect_equal(off$result$Category, "VU")    # 35% spatial decline
  expect_match(off$result$Criteria, "A2c")
  expect_equal(on$result$Category, "CR")     # 85% abundance decline dominates
  expect_match(on$result$Criteria, "A2b")
})

test_that("A sub-tolerance spatial wobble is not a continuing decline", {
  # -1.3% AOO change between two samples of an opportunistic database is
  # noise, not evidence of continuing decline (Guidelines 4.9 warns against
  # comparing convex-hull EOO estimates for exactly this reason).
  res <- sa(
    species = "Wobble", eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400),
    trend = list(percent_change = -1.3, eoo_percent_change = -3.4),
    locations = 4, pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2026, n_records = 500
  )
  expect_equal(res$details$b_indices, character(0))
  expect_equal(res$result$Category, "NT")  # area + few locations only

  strict <- sa(
    species = "Wobble", eoo = list(area_km2 = 4000), aoo = list(area_km2 = 400),
    trend = list(percent_change = -1.3, eoo_percent_change = -3.4),
    locations = 4, pop_metrics = empty_pop(), evaluate_pop = FALSE,
    year_last = 2026, n_records = 500, decline_tolerance = 0
  )
  expect_setequal(strict$details$b_indices, c("i", "ii"))
  expect_equal(strict$result$Category, "EN")
})

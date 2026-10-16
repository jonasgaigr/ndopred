# The rule engine, tested directly: every threshold boundary, every UNKNOWN
# path, the worked examples of Criteria v3.1 Annex 2 and Guidelines 8.2-8.3,
# and the properties the gap analysis requires.

cfg <- ndopred_config(2026)

propose <- function(..., indication = list(), evidence = list(), config = cfg,
                    evaluate_pop = TRUE) {
  defaults <- list(eoo_km2 = 50000, aoo_km2 = 5000, n_records = 100,
                   n_current = 50, year_last = 2026)
  args <- utils::modifyList(defaults, list(...))
  inp <- do.call(assessment_inputs, c(args, list(indication = indication, evidence = evidence)))
  categorise(assess_criteria(inp, config, evaluate_pop = evaluate_pop))
}

# Evidence that nothing else applies, so only the condition under test acts.
nothing <- list(severely_fragmented = FALSE, threat = FALSE,
                decline = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE, v = FALSE),
                fluctuation = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE))
with_nothing <- function(...) utils::modifyList(nothing, list(...))

# ------------------------------------------------------------- boundaries

test_that("A thresholds are inclusive (>=) at 80/50/30 and 90/70/50", {
  # Criteria v3.1 V A: "reduction of 80%" etc. means at least 80 %.
  a <- function(r, types = "A2") propose(evidence = with_nothing(reduction = r, reduction_types = types))$category
  expect_equal(a(80), "CR"); expect_equal(a(79.9), "EN")
  expect_equal(a(50), "EN"); expect_equal(a(49.9), "VU")
  expect_equal(a(30), "VU"); expect_equal(a(29.9), "NT")   # nt_margin_a convention
  expect_equal(a(19.9), "LC")
  expect_equal(a(90, "A1"), "CR"); expect_equal(a(89.9, "A1"), "EN")
  expect_equal(a(70, "A1"), "EN"); expect_equal(a(50, "A1"), "VU")
})

test_that("B area thresholds are strict (<)", {
  # Criteria v3.1 V B1/B2: "less than 100 km2" etc.
  b <- function(eoo, aoo) {
    propose(eoo_km2 = eoo, aoo_km2 = aoo,
            evidence = with_nothing(severely_fragmented = TRUE, decline = c(iii = TRUE)))$category
  }
  expect_equal(b(99.9, 50), "CR"); expect_equal(b(100, 50), "EN")
  expect_equal(b(4999, 600), "EN"); expect_equal(b(5000, 600), "VU")
  expect_equal(b(19999, 2500), "VU"); expect_equal(b(20000, 2500), "LC")
  expect_equal(b(50000, 9.9), "CR"); expect_equal(b(50000, 10), "EN")
  expect_equal(b(50000, 499), "EN"); expect_equal(b(50000, 500), "VU")
  expect_equal(b(50000, 1999), "VU"); expect_equal(b(50000, 2000), "LC")
})

test_that("B(a) location limits are inclusive (<=) at 1/5/10", {
  # "a single location" / "no more than five" / "no more than 10".
  # AOO 40 km2 keeps D2 out of it (not < 20 km2; D2 would need <= 5 locations).
  loc <- function(n) {
    propose(eoo_km2 = 50, aoo_km2 = 40,
            evidence = with_nothing(threat = "drainage", threat_locations = n,
                                    decline = c(iii = TRUE)))$category
  }
  expect_equal(loc(1), "CR"); expect_equal(loc(2), "EN")
  expect_equal(loc(5), "EN"); expect_equal(loc(6), "VU")
  expect_equal(loc(10), "VU"); expect_equal(loc(11), "NT")  # area + one condition
})

test_that("C and D population thresholds are strict (<), C2a caps inclusive", {
  d <- function(n) propose(evidence = with_nothing(mature = n))$category
  expect_equal(d(49), "CR"); expect_equal(d(50), "EN")
  expect_equal(d(249), "EN"); expect_equal(d(250), "VU")
  expect_equal(d(999), "VU"); expect_equal(d(1000), "NT")  # < nt_margin_c
  # C2a(i) at CR: no subpopulation larger than 50 -> 50 qualifies; 51 drops
  # to EN (where the cap is 250).
  c2 <- function(sub) {
    propose(evidence = with_nothing(mature = 240, largest_subpop = sub, prop_largest = 0.2,
                                    decline = c(v = TRUE)))
  }
  expect_equal(paste(c2(50)$category, c2(50)$criteria_iucn), "CR C2a(i)")
  expect_equal(c2(51)$category, "EN")
  # C2a(ii) at CR: at least 90 % in one subpopulation.
  share <- function(p) {
    propose(evidence = with_nothing(mature = 240, largest_subpop = 240 * p, prop_largest = p,
                                    decline = c(v = TRUE)))
  }
  expect_equal(share(0.90)$category, "CR")
  expect_match(share(0.90)$criteria_iucn, "C2a(ii)", fixed = TRUE)
  expect_false(share(0.89)$category == "CR")
})

test_that("C1 uses each category's period and caps it at 100 years", {
  # 25 % per decade is 25 % over VU's 10 years: VU C1 for 3,000 mature
  # (fewer than 10,000, not fewer than 2,500).
  p <- propose(evidence = with_nothing(mature = 3000, mature_decline = -25, mature_decline_years = 10))
  expect_equal(paste(p$category, p$criteria_iucn), "VU C1")
  # For 200 mature the same rate is 8.3 % over CR's 3 years and 13.4 % over
  # EN's 5: not C1 at CR or EN (the taxon is EN under D instead).
  q <- propose(evidence = with_nothing(mature = 200, mature_decline = -25, mature_decline_years = 10))
  expect_equal(paste(q$category, q$criteria_iucn), "EN D")
  # Generation length 40: VU period 120 years uncapped, 100 capped. A 1 % per
  # decade decline is 9.6 % over 100 years - below 10 %, so not C1.
  long <- ndopred_config(2026, generation_length = 40)
  q <- propose(config = long, evidence = with_nothing(mature = 5000, mature_decline = -1,
                                                      mature_decline_years = 10))
  expect_false(grepl("C1", q$criteria_iucn))
})

test_that("The D2 restriction is AOO < 20 km2 or <= 5 locations, with a threat", {
  d2 <- function(aoo, locs) {
    propose(eoo_km2 = 50000, aoo_km2 = aoo,
            evidence = list(threat = "flooding", threat_locations = locs))$criteria_iucn
  }
  expect_equal(d2(19.9, 30), "D2")
  expect_false(grepl("D2", d2(20, 30)))
  expect_equal(d2(800, 5), "D2")
  expect_false(grepl("D2", d2(800, 6)))
})

# ----------------------------------------------------------- UNKNOWN paths

test_that("Kleene logic: UNKNOWN is neither TRUE nor FALSE", {
  expect_true(ndopred:::.k_or(NA, TRUE))
  expect_false(ndopred:::.k_or(FALSE, FALSE))
  expect_true(is.na(ndopred:::.k_or(NA, FALSE)))
  expect_false(ndopred:::.resolve(ndopred:::.cond(NA, NA), "working"))
  expect_true(ndopred:::.resolve(ndopred:::.cond(NA, NA), "high"))
  expect_false(ndopred:::.resolve(ndopred:::.cond(FALSE, TRUE), "high"))  # evidence wins
})

test_that("Unknown B conditions leave a restricted taxon LC, flagged uncertain (D7)", {
  # The best estimate is proposed; the range across the threatened threshold
  # is flagged, and NT is left to the assessor (Guidelines 10.1, 10.4).
  p <- propose(eoo_km2 = 1700, aoo_km2 = 84)
  expect_equal(p$category, "LC")
  expect_equal(p$range, "LC-EN")
  expect_true(p$uncertain)
  expect_true(any(grepl("^Uncertain: plausible range LC-EN - NT may be chosen", p$flags)))
  expect_true(any(grepl("^B\\(a\\)", p$needs)))
  # The same taxon with all three conditions, and any reduction, evidenced
  # absent is LC, and nothing is left to evidence.
  q <- propose(eoo_km2 = 1700, aoo_km2 = 84, evidence = with_nothing(reduction = 0))
  expect_equal(q$category, "LC")
  expect_equal(q$support, "evidence")
  expect_false(q$uncertain)
  # With the reduction unknown, criterion A is the one thing still needed.
  r <- propose(eoo_km2 = 1700, aoo_km2 = 84, evidence = nothing)
  expect_equal(r$category, "LC")
  expect_equal(r$needs, "A: no reduction estimate (no records in the comparison window)")
})

test_that("An indication supports a proposal and is listed as needing evidence", {
  p <- propose(eoo_km2 = 4000, aoo_km2 = 400,
               indication = list(clusters = 3, decline = c(ii = TRUE)))
  expect_equal(p$category, "EN")
  expect_equal(p$support, "indication")
  expect_true(any(grepl("^B\\(a\\).*clusters", p$needs)))
  expect_true(any(grepl("^B\\(b\\)", p$needs)))
  # Evidence for the same conditions removes them from the list.
  q <- propose(eoo_km2 = 4000, aoo_km2 = 400,
               indication = list(clusters = 3, decline = c(ii = TRUE)),
               evidence = list(threat = "logging", threat_locations = 3, decline = c(ii = TRUE),
                               fluctuation = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE),
                               reduction = 0))
  expect_equal(q$category, "EN")
  expect_equal(q$support, "evidence")
})

test_that("A cluster count is not used once there is no plausible threat", {
  # Guidelines 4.11: without a plausible threat, location subcriteria fail.
  p <- propose(eoo_km2 = 4000, aoo_km2 = 400,
               indication = list(clusters = 3, decline = c(ii = TRUE)),
               evidence = list(threat = FALSE))
  expect_equal(p$category, "NT")
})

test_that("No records means NE; no current records and a long gap means an RE candidate", {
  expect_equal(propose(n_records = 0, n_current = 0)$category, "NE")
  re <- propose(n_current = 0, aoo_km2 = 0, year_last = 1960)
  expect_equal(re$category, "RE")
  expect_equal(re$support, "candidate")
  # The year threshold is a convention in the config.
  strict <- ndopred_config(2026, re_candidate_years = 80)
  expect_equal(propose(n_current = 0, aoo_km2 = 0, year_last = 1960, config = strict)$category, "DD")
})

# --------------------------------------------------------- worked examples

test_that("Criteria v3.1 Annex 2 examples are written as published", {
  vu_b <- propose(eoo_km2 = 15000, aoo_km2 = 1500,
                  evidence = with_nothing(threat = "drainage", threat_locations = 8,
                                          decline = c(iii = TRUE)))
  expect_equal(paste(vu_b$category, vu_b$criteria_iucn), "VU B1ab(iii)+2ab(iii)")

  en_b2 <- propose(eoo_km2 = 30000, aoo_km2 = 400,
                   evidence = with_nothing(decline = c(iii = TRUE), fluctuation = c(ii = TRUE)))
  expect_equal(paste(en_b2$category, en_b2$criteria_iucn), "EN B2b(iii)c(ii)")

  vu_d <- propose(eoo_km2 = 50000, aoo_km2 = 12,
                  evidence = list(mature = 900, threat = "volcano", threat_locations = 1))
  expect_match(vu_d$criteria_iucn, "D1+2", fixed = TRUE)

  en_a_d <- propose(evidence = with_nothing(reduction = 55, reduction_basis = "c", mature = 200))
  expect_equal(paste(en_a_d$category, en_a_d$criteria_iucn), "EN A2c; D")
})

test_that("Guidelines 8.2: fewer than 50 mature individuals is CR D", {
  p <- propose(eoo_km2 = 400, aoo_km2 = 40, evidence = with_nothing(mature = 30))
  expect_equal(paste(p$category, p$criteria_iucn), "CR D")
})

test_that("Guidelines 8.3: four predator-free islands with a specified threat is VU D2", {
  p <- propose(eoo_km2 = 30000, aoo_km2 = 60,
               evidence = list(threat = "accidental introduction of predators",
                               threat_locations = 4, severely_fragmented = FALSE,
                               decline = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE, v = FALSE),
                               fluctuation = c(i = FALSE, ii = FALSE, iii = FALSE, iv = FALSE)))
  expect_equal(paste(p$category, p$criteria_iucn), "VU D2")
})

test_that("The established criteria format is unchanged (gap analysis D10)", {
  p <- propose(eoo_km2 = 4000, aoo_km2 = 400,
               indication = list(clusters = 3, decline = c(ii = TRUE)))
  expect_equal(p$criteria, "B1ab(ii); B2ab(ii)")
  expect_equal(p$criteria_iucn, "B1ab(ii)+2ab(ii)")
  d <- propose(evidence = with_nothing(mature = 200))
  expect_equal(d$criteria, "D1")
  expect_equal(d$criteria_iucn, "D")
})

test_that("B is evaluated at every level, not just the one its area meets", {
  # EOO 50 km2 meets the CR area but has four locations: CR fails (a), EN
  # holds (Criteria v3.1 V: the EN area threshold is met too).
  p <- propose(eoo_km2 = 50, aoo_km2 = 40,
               evidence = with_nothing(threat = "drainage", threat_locations = 4,
                                       decline = c(iii = TRUE)))
  expect_equal(paste(p$category, p$criteria_iucn), "EN B1ab(iii)+2ab(iii)")
})

# -------------------------------------------------------------- properties

test_that("Turning an UNKNOWN into TRUE never lowers the proposal", {
  rank <- function(x) match(x, c("LC", "NT", "VU", "EN", "CR"))
  set.seed(7)
  for (k in 1:40) {
    eoo <- sample(c(50, 900, 4000, 15000, 40000), 1)
    aoo <- sample(c(8, 60, 400, 1500, 3000), 1)
    base_ev <- list(severely_fragmented = sample(c(NA, FALSE), 1),
                    decline = c(iii = sample(c(NA, FALSE), 1)))
    before <- propose(eoo_km2 = eoo, aoo_km2 = aoo, evidence = base_ev)
    for (flip in c("severely_fragmented", "iii")) {
      ev <- base_ev
      if (flip == "iii") ev$decline <- c(iii = TRUE) else ev$severely_fragmented <- TRUE
      after <- propose(eoo_km2 = eoo, aoo_km2 = aoo, evidence = ev)
      expect_gte(rank(after$category), rank(before$category))
    }
  }
})

test_that("The same inputs and configuration give the same proposal", {
  a <- propose(eoo_km2 = 4000, aoo_km2 = 400, indication = list(clusters = 3))
  b <- propose(eoo_km2 = 4000, aoo_km2 = 400, indication = list(clusters = 3))
  expect_identical(a, b)
  expect_equal(cfg$reference_year, 2026)
  expect_error(ndopred_config(), "reference_year")
})

test_that("The evidence table cites a source for every rule", {
  inp <- assessment_inputs(eoo_km2 = 4000, aoo_km2 = 400, n_records = 80, n_current = 30,
                           year_last = 2025, indication = list(clusters = 3))
  tab <- assess_criteria(inp, cfg)$table
  expect_true(all(nzchar(tab$ref)))
  expect_true(all(grepl("Criteria v3.1|Guidelines v16", tab$ref)))
  expect_setequal(unique(tab$criterion), c("A", "B1", "B2", "B", "C", "D", "E"))
})

# ------------------------------------------- AOPK decisions D5 and D7

test_that("Default windows are AOPK's: 2010-2026 against 2000-2016 (D5)", {
  w <- ndopred_config(2026)$windows
  expect_equal(c(w$recent_start, w$recent_end), c(2010, 2026))
  expect_equal(c(w$comp_start, w$comp_end), c(2000, 2016))
  # The trend compares the comparison window with the whole current window.
  expect_equal(c(w$trend_start, w$trend_end), c(2010, 2026))
  # Explicit bounds still win; a window cannot end before it starts.
  w2 <- ndopred_config(2025, recent_start = 2016, comp_start = 2006, comp_end = 2015)$windows
  expect_equal(c(w2$comp_start, w2$comp_end, w2$trend_start), c(2006, 2015, 2016))
  expect_error(ndopred_config(2026, comp_start = 2020, comp_end = 2010), "starts after it ends")
})

test_that("A threatened best estimate with LC in its range stays, flagged uncertain (D7)", {
  # Best AOO 1,990 km2 meets VU B2; with positional error up to 2,010 km2 the
  # area threshold may not be met, so LC is plausible too.
  inp <- assessment_inputs(eoo_km2 = 30000, aoo_km2 = 1990, aoo_km2_min = 1950, aoo_km2_max = 2010,
                           n_records = 200, n_current = 150, year_last = 2026,
                           evidence = list(severely_fragmented = TRUE, decline = c(iii = TRUE)))
  p <- categorise(assess_criteria(inp, cfg))
  expect_equal(p$category, "VU")
  expect_equal(p$criteria_iucn, "B2ab(iii)")
  expect_equal(p$range, "LC-VU")
  expect_true(p$uncertain)
  expect_true(any(grepl("Uncertain: plausible range LC-VU (Guidelines 3.2.4)", p$flags, fixed = TRUE)))
  # A range within the threatened categories is not flagged.
  inp2 <- assessment_inputs(eoo_km2 = 30000, aoo_km2 = 520, aoo_km2_min = 480, aoo_km2_max = 560,
                            n_records = 200, n_current = 150, year_last = 2026,
                            evidence = list(severely_fragmented = TRUE, decline = c(iii = TRUE)))
  q <- categorise(assess_criteria(inp2, cfg))
  expect_equal(q$range, "VU-EN")
  expect_false(q$uncertain)
  expect_equal(ndopred_config(2026)$attitude$proposal, "best estimate")
})

test_that("triage puts taxa that may be CR or EN first and never says LC", {
  tri <- function(eoo, aoo) {
    inp <- assessment_inputs(eoo_km2 = eoo, aoo_km2 = aoo, n_records = 50, n_current = 30,
                             year_last = 2025)
    p <- categorise(assess_criteria(inp, ndopred_config(2026)))
    .result_row("x", p, eoo, aoo, NA, NA)[, c("Triage_Priority", "Triage")]
  }
  expect_equal(unlist(tri(50, 8)), c(Triage_Priority = "1", Triage = "1: CR possible"))
  expect_equal(tri(3000, 300)$Triage, "1: EN possible")
  expect_equal(tri(15000, 1500)$Triage_Priority, 2L)
  wide <- tri(50000, 5000)
  expect_equal(wide$Triage_Priority, 4L)
  expect_match(wide$Triage, "LC candidate")
  expect_equal(.triage(list(category = "NT", range = "NT"))$priority, 3L)
  expect_equal(.triage(list(category = "LC", range = "LC-VU"))$priority, 2L)
  expect_equal(.triage(list(category = NA_character_))$priority, 0L)
  expect_true(is.na(.triage(list(category = "NA"))$priority))
  expect_true(is.na(.triage(list(category = "NE"))$priority))
  expect_equal(.triage(list(category = "RE"))$priority, 1L)
  expect_equal(.triage(list(category = "DD"))$priority, 2L)
})

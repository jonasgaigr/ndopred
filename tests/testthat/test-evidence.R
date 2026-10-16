# Phase 3: the evidence layer (prompt section 6), evidence ranges in the
# engine (Guidelines 3.2), the final category and its approval (AOPK D9).

# An assessment of a taxon with EOO 4,000 km2 and AOO 400 km2 (the EN area
# thresholds of B1 and B2 met) and the given evidence.
case <- function(ev, eoo = 4000, aoo = 400, n_current = 20) {
  ex <- evidence_for_engine(ev)
  inp <- assessment_inputs(eoo_km2 = eoo, aoo_km2 = aoo, n_records = 50, n_current = n_current,
                           year_last = 2026, evidence = ex$engine)
  a <- assess_criteria(inp, ndopred_config(2026))
  list(a = a, p = categorise(a), ex = ex)
}
final_of <- function(ev, ...) {
  k <- case(ev, ...)
  final_assessment(k$a, k$p, k$ex)
}

# ------------------------------------------------------------- the schema

test_that("Every field has a template row, and an empty template is no evidence", {
  tmpl <- evidence_template("Test sp")
  expect_setequal(tmpl$field, evidence_fields()$field)
  expect_equal(nrow(validate_evidence(tmpl)), 0)
})

test_that("Templates round-trip through CSV and XLSX", {
  dir <- withr::local_tempdir()
  ev <- evidence_of(ev_row("threat", "drainage of breeding ponds"),
                    ev_row("threat_locations", min = 2, best = 3, max = 5, basis = "b"))
  csv <- file.path(dir, "ev.csv")
  utils::write.csv(as.data.frame(ev), csv, row.names = FALSE, fileEncoding = "UTF-8")
  back <- read_evidence(csv)
  expect_equal(back$best, ev$best)
  expect_equal(back$value, ev$value)
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  xlsx <- file.path(dir, "template.xlsx")
  evidence_template("Test sp", file = xlsx)
  expect_equal(readxl::excel_sheets(xlsx), c("evidence", "fields"))
  writexl::write_xlsx(list(evidence = as.data.frame(ev)), xlsx)
  expect_equal(read_evidence(xlsx)$best, ev$best)
})

test_that("Validation names every problem with its row", {
  bad <- rbind(
    ev_row("threat", "x", author = ""),                        # no author
    ev_row("decline", "TRUE"),                                 # no subfield
    ev_row("threat_locations", best = 3),                      # no 4.11 option
    ev_row("mature_individuals", min = 900, best = 800, max = 1000),
    ev_row("prop_largest_subpopulation", best = 1.4),
    ev_row("criterion_e", "EN"),                               # no sources
    ev_row("record_exclude", subfield = "duplicate"),          # no record_id
    ev_row("no_such_field", "x"),
    ev_row("severely_fragmented", "maybe", date = "15.10.2026")
  )
  msg <- tryCatch(validate_evidence(bad), error = conditionMessage)
  for (k in c("row 1 \\(threat\\): author", "row 2 \\(decline\\): subfield",
              "row 3 \\(threat_locations\\): basis", "row 4 .*min <= best <= max",
              "row 5 .*between 0 and 1", "row 6 .*sources", "row 7 .*record_id",
              "row 8 .*unknown field", "row 9 .*YYYY-MM-DD", "row 9 .*TRUE or FALSE")) {
    expect_match(msg, k)
  }
})

test_that("The latest entry for a field is the one in force", {
  ev <- evidence_of(ev_row("severely_fragmented", "TRUE", date = "2026-01-10"),
                    ev_row("severely_fragmented", "FALSE", date = "2026-05-02"),
                    ev_row("severely_fragmented", "TRUE", date = "2025-12-01"))
  expect_false(evidence_for_engine(ev)$engine$severely_fragmented)
})

test_that("Evidence files are versioned, never overwritten", {
  dir <- withr::local_tempdir()
  ev <- evidence_of(ev_row("threat", "drainage"))
  p1 <- write_evidence(ev, dir)
  p2 <- write_evidence(evidence_of(ev_row("threat", "drainage and fish stocking")), dir)
  expect_false(identical(p1, p2))
  expect_length(list.files(dir), 2)
  expect_equal(read_evidence(latest_evidence(dir, "Test sp"))$value, "drainage and fish stocking")
})

# ------------------------------------------------------- engine evidence

test_that("Evidence becomes the engine's inputs", {
  ex <- evidence_for_engine(evidence_of(
    ev_row("threat", "drainage of breeding ponds"),
    ev_row("threat_locations", min = 2, best = 3, max = 5, basis = "b"),
    ev_row("decline", "ano", subfield = "iii"),
    ev_row("fluctuation", "FALSE", subfield = "i"),
    ev_row("mature_individuals", min = 400, best = 600, max = 900),
    ev_row("reduction", subfield = "A2+A4", best = 40, basis = "a,c"),
    ev_row("criterion_e", "VU", sources = "PVA, Novak 2025")
  ))$engine
  expect_equal(ex$threat, "drainage of breeding ponds")
  expect_equal(ex$threat_locations, c(min = 2, best = 3, max = 5))
  expect_equal(ex$decline, c(iii = TRUE))
  expect_equal(ex$fluctuation, c(i = FALSE))
  expect_equal(ex$reduction_types, c("A2", "A4"))
  expect_equal(ex$reduction_basis, "ac")
  expect_equal(ex$e_category, "VU")
  # No plausible threat outranks a described one; a D2 scenario stands in
  # for an undescribed threat.
  expect_false(evidence_for_engine(evidence_of(ev_row("threat", "x"),
                                               ev_row("no_plausible_threat", "TRUE")))$engine$threat)
  expect_equal(evidence_for_engine(evidence_of(ev_row("d2_scenario", "one dam failure")))$engine$threat,
               "one dam failure")
})

test_that("A1 needs reversible, understood and ceased causes; otherwise A2", {
  base <- ev_row("reduction", subfield = "A1", best = 60, basis = "a")
  ex <- evidence_for_engine(evidence_of(base))
  expect_equal(ex$engine$reduction_types, "A2")
  expect_match(ex$checks, "A1 needs")
  all3 <- evidence_of(base, ev_row("a1_causes", "TRUE", subfield = "reversible"),
                      ev_row("a1_causes", "TRUE", subfield = "understood"),
                      ev_row("a1_causes", "TRUE", subfield = "ceased"))
  expect_equal(evidence_for_engine(all3)$engine$reduction_types, "A1")
})

test_that("Evidence ranges widen the plausible range, not the proposal (Guidelines 3.2)", {
  ev <- function(locs) evidence_of(
    ev_row("threat", "drainage"), locs, ev_row("decline", "TRUE", subfield = "iii"),
    ev_row("severely_fragmented", "FALSE"),
    ev_row("fluctuation", "FALSE", subfield = "i"), ev_row("fluctuation", "FALSE", subfield = "ii"),
    ev_row("fluctuation", "FALSE", subfield = "iii"), ev_row("fluctuation", "FALSE", subfield = "iv"))
  point <- case(ev(ev_row("threat_locations", best = 7, basis = "all")))$p
  expect_equal(point$category, "VU")
  expect_equal(point$range, "VU")
  ranged <- case(ev(ev_row("threat_locations", min = 4, best = 7, max = 12, basis = "all")))$p
  expect_equal(ranged$category, "VU")
  expect_equal(ranged$range, "LC-EN")    # 12 locations: not VU; 4: EN
  expect_true(ranged$uncertain)
  # A reduction of 25-55 % (best 35 %) under A2.
  red <- case(evidence_of(ev_row("reduction", subfield = "A2", min = 25, best = 35, max = 55, basis = "a")),
              eoo = 50000, aoo = 5000)$p
  expect_equal(c(red$category, red$range), c("VU", "LC-EN"))
})

# ------------------------------------------------------- record evidence

test_that("An expert excludes a record by its NDOP ID; nothing is deleted", {
  local_mock_ndop()
  occ <- get_assessment_data("Restricted stable sp")
  id <- occ$ID_NALEZ[occ$include][1]
  ev <- evidence_of(ev_row("record_exclude", subfield = "misidentification", record_id = id,
                           taxon = "Restricted stable sp"),
                    ev_row("record_include", record_id = 99999, taxon = "Restricted stable sp"))
  ex <- evidence_for_engine(ev, "Restricted stable sp")
  out <- apply_record_evidence(occ, ex$records)
  expect_equal(nrow(out), nrow(occ))
  expect_false(out$include[out$ID_NALEZ == id])
  expect_match(out$exclusion_reason[out$ID_NALEZ == id], "^expert: misidentification \\(A. Expert, 2026-10-15\\)")
  expect_match(attr(out, "expert_record_notes"), "99999: not among")

  cfg <- ndopred_config(as.numeric(format(Sys.Date(), "%Y")))
  plain <- assess_taxon(occ, "Restricted stable sp", cfg)
  with_ev <- assess_taxon(occ, "Restricted stable sp", cfg, evidence = ev)
  expect_equal(with_ev$result$Records_Excluded, plain$result$Records_Excluded + 1)
  expect_match(with_ev$records$flag_expert[with_ev$records$ID_NALEZ == id], "misidentification")
})

test_that("A negative finding cannot be re-included", {
  occ <- data.frame(ID_NALEZ = 1:2, include = FALSE, flag_negative = c(TRUE, FALSE),
                    flag_no_coords = FALSE, exclusion_reason = c("negative finding", "VEROH 6"))
  recs <- data.frame(record_id = 1:2, action = "include", reason = "re-included",
                     author = "A", date = "2026-10-15", justification = "x")
  out <- apply_record_evidence(occ, recs)
  expect_equal(out$include, c(FALSE, TRUE))
  expect_match(attr(out, "expert_record_notes"), "record 1: a negative finding")
})

test_that("Generation-length evidence moves the comparison window", {
  local_mock_ndop()
  occ <- get_assessment_data("Stable widespread sp")
  ev <- evidence_of(ev_row("generation_length", best = 5, taxon = "Stable widespread sp"))
  # No time-depth limit here: with the default (2000) the window could not
  # move back from 2000-2016 (test-generation-lengths.R covers the limit).
  res <- assess_taxon(occ, "Stable widespread sp", ndopred_config(2026, time_depth_start = NA), evidence = ev)
  expect_equal(res$config$generation_length, 5)
  expect_equal(c(res$config$windows$comp_start, res$config$windows$comp_end), c(1995, 2011))
  expect_match(res$result$Generation_Length_Source, "expert evidence: A. Expert")
})

# ------------------------------------------------- final category, approval

test_that("EN claimed under B with only one of a/b/c evidenced is flagged", {
  fin <- final_of(evidence_of(
    ev_row("decline", "TRUE", subfield = "iii"),
    ev_row("final_category", "EN", basis = "EN B1ab(iii)", justification = "expert judgement"),
    approval()))
  expect_equal(fin$status, "approved; inconsistent with the evidence")
  expect_equal(fin$category, "EN")
  expect_true(any(grepl("EN is not supported by the evidence and data alone, which meet LC", fin$issues)))
  expect_true(any(grepl("B at EN: B1 and B2 below the threshold; 1 of \\(a\\), \\(b\\), \\(c\\) evidenced", fin$issues)))
})

test_that("A final category the evidence supports, approved, is final", {
  fin <- final_of(evidence_of(
    ev_row("threat", "drainage"), ev_row("threat_locations", best = 3, basis = "all"),
    ev_row("decline", "TRUE", subfield = "iii"),
    ev_row("final_category", "EN", basis = "EN B1ab(iii)+2ab(iii)", justification = "as proposed"),
    approval()))
  expect_equal(fin$status, "approved")
  expect_equal(fin$category, "EN")
  expect_length(fin$issues, 0)
  expect_equal(fin$evidence_only$criteria, "B1ab(iii)+2ab(iii)")
})

test_that("Without an approval on or after its date, a final category is not final (D9)", {
  ev <- list(ev_row("threat", "drainage"), ev_row("threat_locations", best = 3, basis = "all"),
             ev_row("decline", "TRUE", subfield = "iii"),
             ev_row("final_category", "EN", basis = "EN B1ab(iii)", justification = "x"))
  none <- final_of(do.call(evidence_of, ev))
  expect_equal(none$status, "awaiting approval")
  expect_true(is.na(none$category))
  stale <- final_of(do.call(evidence_of, c(ev, list(approval("2026-09-01")))))
  expect_equal(stale$status, "awaiting approval")
  expect_true(any(grepl("does not cover it", stale$notes)))
})

test_that("Final categories outside A-E are checked too", {
  dd <- final_of(evidence_of(
    ev_row("threat", "drainage"), ev_row("threat_locations", best = 3, basis = "all"),
    ev_row("decline", "TRUE", subfield = "iii"),
    ev_row("final_category", "DD", justification = "x"), approval()))
  expect_true(any(grepl("DD is not appropriate", dd$issues)))
  re <- final_of(evidence_of(ev_row("final_category", "RE", justification = "x"), approval()))
  expect_true(any(grepl("RE needs documented surveys", re$issues)))
  expect_true(any(grepl("records exist in the current window", re$issues)))
  na <- final_of(evidence_of(ev_row("final_category", "NA", justification = "x"), approval()))
  expect_true(any(grepl("NA needs a step-one reason", na$issues)))
  na_ok <- final_of(evidence_of(ev_row("origin", "introduced"),
                                ev_row("final_category", "NA", justification = "x"), approval()))
  expect_length(na_ok$issues, 0)
  d2 <- final_of(evidence_of(ev_row("final_category", "VU", basis = "VU D2", justification = "x"),
                             approval()), aoo = 400)
  expect_true(any(grepl("criterion D is not met at VU", d2$issues)))
})

test_that("Criteria letters are read from IUCN strings", {
  expect_equal(.criteria_letters("EN B1ab(ii,iii)+2ab(ii,iii)"), "B")
  expect_equal(.criteria_letters("VU A2c; B2ab(iii)"), c("A", "B"))
  expect_equal(.criteria_letters("CR D"), "D")
  expect_equal(.criteria_letters("VU D1+2"), "D")
  expect_equal(.criteria_letters("EN C2a(i)"), "C")
  expect_equal(.criteria_letters(""), character(0))
})

test_that("batch_assess takes one evidence table for many taxa", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  ev <- evidence_of(
    ev_row("final_category", "LC", justification = "widespread, stable", taxon = "Stable widespread sp"),
    approval_for("Stable widespread sp"),
    ev_row("final_category", "VU", basis = "VU D2", justification = "threat", taxon = "Restricted stable sp")
  )
  res <- suppressMessages(batch_assess(c("Stable widespread sp", "Restricted stable sp", "Declining sp"),
                                       reference_year = yn, evidence = ev))
  st <- stats::setNames(res$Final_Status, res$Species)
  expect_equal(unname(st["Stable widespread sp"]), "approved")
  expect_equal(unname(st["Restricted stable sp"]), "awaiting approval")
  expect_true(is.na(st["Declining sp"]))
})

test_that("Locations under Guidelines 4.11 option (a) are not used", {
  ev <- evidence_of(ev_row("threat", "drainage"), ev_row("decline", "TRUE", subfield = "iii"),
                    ev_row("threat_locations", best = 2, basis = "a"))
  k <- case(ev)
  expect_true(is.infinite(k$ex$engine$threat_locations))
  expect_match(k$ex$checks, "option a")
  # Two locations would give EN B with the decline; unused, B(a) is not met.
  expect_false(k$p$category %in% c("EN", "CR"))
  expect_equal(case(evidence_of(ev_row("threat", "drainage"), ev_row("decline", "TRUE", subfield = "iii"),
                                ev_row("threat_locations", best = 2, basis = "all")))$p$category, "EN")
})

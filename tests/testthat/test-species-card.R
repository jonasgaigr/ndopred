# Phase 3: the species card (prompt section 8).

test_that("The species card documents the proposal, final category, evidence and records", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  sp <- "Restricted stable sp"
  occ <- get_assessment_data(sp)
  id <- occ$ID_NALEZ[occ$include][1]
  ev <- evidence_of(
    ev_row("threat", "building on the only site", taxon = sp),
    ev_row("record_exclude", subfield = "misidentification", record_id = id, taxon = sp),
    ev_row("final_category", "VU", basis = "VU D2", justification = "single site", taxon = sp),
    approval_for(sp)
  )
  res <- assess_taxon(occ, sp, ndopred_config(yn), evidence = ev)
  f <- withr::local_tempfile(fileext = ".html")
  species_card(res, f)
  html <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (k in c(sp, "Proposal (ndopred)", "Final category", "B. Approver", "building on the only site",
              "expert: misidentification", "Criteria and the rules applied", "Open questions",
              "Regional step", "eligibility not documented",
              "Guidelines for Using the IUCN Red List Categories and Criteria v16")) {
    expect_match(html, k, fixed = TRUE)
  }
  expect_match(html, res$final$status, fixed = TRUE)
  expect_match(html, paste0("Used (ID_NALEZ)"), fixed = TRUE)
})

test_that("A card is written without evidence or records too", {
  inp <- assessment_inputs(eoo_km2 = 1700, aoo_km2 = 84, n_records = 40, n_current = 12, year_last = 2025)
  cfg <- ndopred_config(2026)
  a <- assess_criteria(inp, cfg)
  x <- list(result = data.frame(Species = "Example sp", EOO_km2 = 1700, AOO_km2 = 84),
            assessment = a, proposal = categorise(a), config = cfg)
  page <- species_card(x)
  html <- as.character(page)
  expect_match(html, "No final category recorded.", fixed = TRUE)
  expect_match(html, "Record list not available.", fixed = TRUE)
})

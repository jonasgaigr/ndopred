skip_if_not_installed("shiny")

app_dir <- function() {
  d <- system.file("shinyapp", package = "ndopred")
  if (!nzchar(d)) skip("shinyapp directory not found")
  d
}

test_that("The single-species app completes a run without erroring", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  out <- NULL
  shiny::testServer(app_dir(), {
    session$setInputs(species_name = "Declining sp", window = 10,
                      gen_length = 1, use_pop = FALSE, run_calc = 1)
    # raw_data() previously died here: the app called
    # calculate_trend(occ_all, window_years = ...), which was not an argument
    # of calculate_trend(), so every assessment failed with
    # "unused argument (window_years = 10)".
    out <<- list(data = raw_data(), auto = auto_assessment(),
                 expert = expert_final())
  })

  expect_gt(out$data$n_records, 0)
  expect_false(is.na(out$data$trend$percent_change))
  expect_true(out$auto$result$Category %in% c("CR", "EN", "VU", "NT", "LC", "DD", "RE"))
  expect_true(nchar(out$expert$category) > 0)
})

test_that("Criterion B uses the short current-distribution window", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  aoo <- list()
  for (gl in c(1, 30)) {
    shiny::testServer(app_dir(), {
      session$setInputs(species_name = "Declining sp", window = 10,
                        gen_length = gl, use_pop = FALSE, run_calc = 1)
      d <- raw_data()
      aoo[[as.character(gl)]] <<- list(aoo = d$aoo$area_km2,
                                       b = d$b_window, a = d$a_window)
    })
  }
  # Generation length drives the criterion A reduction window (3 generations,
  # capped at 100) but must not stretch the criterion B window: Guidelines 4.9
  # requires EOO/AOO to describe the CURRENT distribution.
  expect_equal(aoo[["1"]]$a, 10)
  expect_equal(aoo[["30"]]$a, 90)
  expect_equal(aoo[["1"]]$b, aoo[["30"]]$b)
  expect_equal(aoo[["1"]]$aoo, aoo[["30"]]$aoo)
})

test_that("D2 is not asserted for the expert", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  shiny::testServer(app_dir(), {
    session$setInputs(species_name = "Restricted stable sp", window = 10,
                      gen_length = 1, use_pop = FALSE, run_calc = 1)
    raw_data()
    auto_assessment()
    expect_true(rv$d2_candidate)   # the restriction is detected...
    # ...but VU D2 needs the expert to describe a plausible threat.
    expect_false(expert_final()$category == "VU")
    expect_true(any(grepl("D2", expert_final()$needs)))

    session$setInputs(threat_text = "drainage of the only site")
    expect_equal(expert_final()$category, "VU")
  })
})

test_that("The expert view equals the automated proposal until evidence is entered", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  shiny::testServer(app_dir(), {
    session$setInputs(species_name = "Declining sp", window = 10,
                      gen_length = 1, use_pop = FALSE, run_calc = 1)
    auto <- auto_assessment()$result
    expect_equal(expert_final()$category, auto$Category)
    expect_equal(expert_final()$criteria, auto$Criteria)
  })
})

test_that("A species with no recent records is screened, not silently listed", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  shiny::testServer(app_dir(), {
    session$setInputs(species_name = "Ghost sp", window = 10,
                      gen_length = 1, use_pop = FALSE, run_calc = 1)
    expect_equal(expert_final()$category, "RE")
    expect_match(expert_final()$criteria, "confirm with surveys")
  })
})

test_that("plot_iucn accepts the arguments both apps pass it", {
  # Regression: plot_iucn(..., window = n) used to fail with
  # "unused argument", so no map was ever produced by either app.
  local_mock_ndop()
  occ <- get_assessment_data("Declining sp")
  # Stub the basemap so the test does not reach the network.
  local_mocked_bindings(gisco_get_countries = function(...) stop("offline"),
                        .package = "giscoR")
  p <- plot_iucn("Declining sp", occ_data = occ, window = 10, year_now = 2025)
  expect_s3_class(p, "ggplot")

  # Window arithmetic reaches the legend, rather than a hard-coded "2000-2016".
  expect_match(p$labels$subtitle, "2016-2025")
  expect_match(p$labels$subtitle, "2006-2015")
})

test_that("The map draws overlapping windows (AOPK, D5)", {
  local_mock_ndop()
  occ <- get_assessment_data("Declining sp")
  local_mocked_bindings(gisco_get_countries = function(...) stop("offline"),
                        .package = "giscoR")
  w <- ndopred_config(2026)$windows
  p <- plot_iucn("Declining sp", occ_data = occ,
                 recent_start = w$recent_start, recent_end = w$recent_end,
                 comp_start = w$comp_start, comp_end = w$comp_end)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "2010-2026")
  expect_match(p$labels$subtitle, "2000-2016; only 2000-2009 grey", fixed = TRUE)
})

test_that("The expert workflow: evidence, a record excluded, a final category approved, a stored version", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  dir <- withr::local_tempdir()
  shiny::testServer(app_dir(), {
    session$setInputs(species_name = "Restricted stable sp", window = 10, use_pop = FALSE, run_calc = 1)
    raw_data()
    id <- raw_data()$occ$ID_NALEZ[raw_data()$occ$include][1]

    # Entries made before naming the assessor still preview, but cannot be saved.
    session$setInputs(threat_text = "building on the only site", just_b = "site visit 2025")
    expect_equal(expert_final()$category, "VU")
    session$setInputs(evidence_dir = dir, store_evidence = 1)
    expect_match(rv$status, "Not saved: enter the assessor's name")

    session$setInputs(assessor = "A. Expert", rec_ids = as.character(id),
                      rec_reason = "misidentification", rec_just = "photo shows another species",
                      rec_exclude = 1)
    rec <- expert_final()$assessment$records
    expect_false(rec$include[rec$ID_NALEZ == id])

    session$setInputs(final_cat = "VU", final_criteria = "VU D2", final_just = "single site, building plans")
    expect_equal(expert_final()$final$status, "awaiting approval")
    session$setInputs(appr_name = "B. Approver", appr_role = "taxonomist",
                      appr_group = "Beetle expert group", appr_date = Sys.Date(), add_approval = 1)
    # The D2 restriction rests on 5 km clusters, not threat-defined locations.
    expect_equal(expert_final()$final$status, "approved; inconsistent with the evidence")
    expect_true(any(grepl("criterion D is not met at VU", expert_final()$final$issues)))
    session$setInputs(threat_locations = 1, loc_option = "all")
    expect_equal(expert_final()$final$status, "approved")

    session$setInputs(store_evidence = 2)
    expect_match(rv$status, "^Saved: ")
    stored <- ndopred::read_evidence(sub("^Saved: ", "", rv$status))
    expect_setequal(stored$field, c("threat", "threat_locations", "record_exclude", "final_category", "approval"))
    expect_true(all(stored$author[stored$field != "approval"] == "A. Expert"))
    # The stored file is now the base: nothing is duplicated when saved again.
    expect_equal(nrow(evidence_table()), nrow(stored))
  })
})

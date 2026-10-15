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

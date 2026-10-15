skip_if_not_installed("shiny")

# R/app_bulk.R defines `server` at package level, so the bulk pipeline can be
# exercised for real rather than re-implemented in the test.

run_bulk <- function(species, window = 10, cell_size = 2) {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(TAXON = species, CELED = "Testidae", SKUPINA = "Brouci",
               ID_TAXON = seq_along(species), stringsAsFactors = FALSE),
    csv, row.names = FALSE, fileEncoding = "CP1250"
  )

  out <- NULL
  shiny::testServer(bulk_server, {
    session$setInputs(
      file1 = list(name = "in.csv", datapath = csv),
      window = window,
      cell_size = cell_size,
      run_bulk = 1
    )
    out <<- list(results = rv$results_df, log = rv$log)
  })
  out
}

test_that("the bulk pipeline produces one row per species with sane metrics", {
  local_mock_ndop()
  # giscoR reaches the network for the basemap; maps are not under test here.
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  res <- run_bulk(c("Declining sp", "Stable widespread sp", "Restricted stable sp"))
  df <- res$results

  expect_equal(nrow(df), 3)
  expect_true(all(c("Kategorie (automatická)", "Změna AOO (%)",
                    "D2 kandidát (nutno posoudit ohrožení)") %in% names(df)))

  # The comparison window is stated in the output, and is the same length as
  # the recent one rather than "everything before the cutoff".
  expect_match(df$`Období staré`[1], "^\\d{4}-\\d{4}$")
  span <- function(s) diff(as.numeric(strsplit(s, "-")[[1]]))
  expect_equal(span(df$`Období staré`[1]), span(df$`Období nové`[1]))

  declining <- df[df$Druh == "Declining sp", ]
  expect_lt(declining$`Změna AOO (%)`, -50)
  expect_true(declining$`Kategorie (automatická)` %in% c("CR", "EN"))
})

test_that("A stable widespread species is not reported as declining", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  df <- run_bulk("Stable widespread sp")$results
  # Same range in both windows: the previous unbounded "old = everything
  # before the cutoff" comparison made this look like a heavy loss.
  expect_lt(abs(df$`Změna AOO (%)`), 25)
  expect_true(df$`Kategorie (automatická)` %in% c("LC", "NT"))
})

test_that("A restricted stable species is flagged, not auto-listed as VU D2", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  df <- run_bulk("Restricted stable sp")$results
  expect_equal(df$`D2 kandidát (nutno posoudit ohrožení)`, "ano")
  expect_false(grepl("^D2", df$`Kriteria (automatická)`))
  expect_false(df$`Kategorie (automatická)` == "VU")
})

test_that("Criteria strings no longer need the b(iii) string surgery", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  df <- run_bulk(c("Declining sp", "Stable widespread sp"))$results
  # b(iii) is a habitat-decline claim that NDOP records cannot support, so it
  # is never emitted and never has to be stripped back out afterwards.
  expect_false(any(grepl("iii", df$`Kriteria (automatická)`)))
})

test_that("A misread CSV reports the problem instead of finding no species", {
  local_mock_ndop()
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("WRONG_HEADER", "Declining sp"), csv)

  log <- NULL
  shiny::testServer(bulk_server, {
    session$setInputs(file1 = list(name = "in.csv", datapath = csv),
                      window = 10, cell_size = 2, run_bulk = 1)
    log <<- rv$log
  })
  expect_true(any(grepl("TAXON", log)))
  expect_true(any(grepl("not found", log)))
})

test_that("A non-reference AOO grid is warned about in the log", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")

  res <- run_bulk("Stable widespread sp", cell_size = 5)
  expect_true(any(grepl("4.10.3", res$log, fixed = TRUE)))
})

test_that("The export says whether the proposal is uncertain (D7)", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  df <- run_bulk(c("Restricted stable sp", "Stable widespread sp"))$results
  expect_true(all(df$`Nejistý návrh` %in% c("ano", "ne")))
  # Restricted, threat unknown: the range reaches the threatened categories.
  expect_equal(df$`Nejistý návrh`[df$Druh == "Restricted stable sp"], "ano")
})

test_that("An uploaded evidence file reaches each taxon, with final-category columns", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  ev <- evidence_of(
    ev_row("final_category", "LC", justification = "widespread, stable", taxon = "Stable widespread sp"),
    approval_for("Stable widespread sp"),
    ev_row("threat", "building on the only site", taxon = "Restricted stable sp")
  )
  ev_file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(as.data.frame(ev), ev_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(TAXON = c("Stable widespread sp", "Restricted stable sp")), csv,
                   row.names = FALSE)
  out <- NULL
  shiny::testServer(bulk_server, {
    session$setInputs(file1 = list(name = "in.csv", datapath = csv),
                      evidence_file = list(name = "ev.csv", datapath = ev_file),
                      window = 10, cell_size = 2, run_bulk = 1)
    out <<- rv$results_df
  })
  st <- stats::setNames(out$`Stav finální kategorie`, out$Druh)
  expect_equal(unname(st["Stable widespread sp"]), "approved")
  expect_equal(out$`Kategorie (finální)`[out$Druh == "Stable widespread sp"], "LC")
  expect_equal(unname(st["Restricted stable sp"]), "no final category")
  # The described threat makes the restricted taxon VU D2.
  expect_equal(out$`Kategorie (automatická)`[out$Druh == "Restricted stable sp"], "VU")
})

test_that("The export carries the regional columns", {
  local_mock_ndop()
  local_mocked_bindings(plot_iucn = function(...) NULL, .package = "ndopred")
  df <- run_bulk("Stable widespread sp")$results
  cols <- c("Kategorie (regionální)", "Kritéria (regionální)", "Regionální krok",
            "Globální kategorie", "Podíl globální populace (%)")
  expect_true(all(cols %in% names(df)))
  expect_equal(df$`Kategorie (regionální)`, df$`Kategorie (automatická)`)   # no regional evidence
  expect_equal(df$`Podíl globální populace (%)`, "?")
})

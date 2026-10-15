# Fixtures for the IUCN-alignment tests: test-iucn-alignment.R (closed gaps,
# regular suite) and test-gap-phase0.R (open gaps, opt-in). See
# methodology/gap_analysis.md for the gap numbers.
#
# Output model decided 2026-10-15 (gap_analysis.md, D10/D11): a category is
# always proposed, and may rest on indications from the records (cluster
# counts, raw window-to-window change). What still needs evidence comes
# alongside it - `Needs_Evidence` in the R API, `Nutno doložit` in the bulk
# export - and names the subcriterion, e.g. "B(a)", "B(b)", "A2", "D2", "RE".
# `Criteria` keeps today's format; Annex 2 notation is in `Criteria_IUCN`.

# A needs-evidence string mentions `pattern`; NULL or NA counts as not.
needs <- function(x, pattern) isTRUE(any(grepl(pattern, x)))

# ---------------------------------------------------------------- fixtures

gap_year <- as.numeric(format(Sys.Date(), "%Y"))

# Extra synthetic taxa on top of helper-mock-ndop.R's mock_species().
gap_species <- function(name, year_now = gap_year) {
  recent <- seq(year_now - 9, year_now)
  comp <- seq(year_now - 19, year_now - 10)
  switch(
    name,
    # Same 20 x 20 km range in both windows; only recording effort drops
    # (200 records before, 40 now). Nothing ecological has changed.
    "Effort drop sp" = rbind(
      scatter(200, -700000, -1080000, 20000, comp, seed = 71),
      scatter(40, -700000, -1080000, 20000, recent, seed = 72)
    ),
    # Coordinates found by search: the recent window's minimum-origin AOO is
    # 3 cells, but on the origin minimised over both windows it is 4 cells.
    "Origin sp" = rbind(
      ndop_row(c(-703597, -697777, -694997, -702587, -704744, -697587),
               c(-1079664, -1076305, -1074522, -1084675, -1082721, -1080114),
               rep(year_now - 15, 6)),
      ndop_row(c(-702179, -699290, -702849, -703577),
               c(-1081350, -1075346, -1079341, -1075894),
               rep(year_now - 5, 4))
    ),
    mock_species(name, year_now)
  )
}

local_gap_ndop <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    ndop_download = function(species, ...) gap_species(species),
    .package = "rndop", .env = env
  )
}

gap_run_bulk <- function(species, window = 10, cell_size = 2) {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(TAXON = species, stringsAsFactors = FALSE),
                   csv, row.names = FALSE, fileEncoding = "CP1250")
  out <- NULL
  shiny::testServer(bulk_server, {
    session$setInputs(file1 = list(name = "in.csv", datapath = csv),
                      window = window, cell_size = cell_size, run_bulk = 1)
    out <<- list(results = rv$results_df, log = rv$log)
  })
  out
}

gap_app_dir <- function() {
  d <- system.file("shinyapp", package = "ndopred")
  if (!nzchar(d)) skip("shinyapp directory not found")
  d
}

gap_assess <- function(...) {
  base <- list(species = "Gap", pop_metrics = list(), evaluate_pop = FALSE,
               year_last = gap_year, n_records = 100,
               trend = list(percent_change = 0), locations = NA,
               reference_year = gap_year)
  args <- utils::modifyList(base, list(...))
  do.call(summarize_assessment, args)
}

# Mimics rndop::ndop_download() output, including fields ndopred ignores.
gap_raw <- function() {
  data.frame(
    ID_NALEZ = 1:7,
    DRUH = c(rep("Bombina bombina", 6), "Bombina bombina × variegata"),
    X = c(NA, -700000, -701000, -702000, -703000, -704000, -705000),
    Y = c(-1080000, -1080000, -1081000, -1082000, -1083000, -1084000, -1085000),
    DATUM_OD = c(20200601, 20200601, 20200601, 19900101, 20200601, 20200601, 20200601),
    DATUM_DO = c(20200601, 20200601, 20200601, 20151231, 20200601, 20200601, 20200601),
    NEGATIV = c(0, 1, 0, 0, 0, 0, 0),
    KATASTR = "KU",
    VEROH = c(0, 0, 0, 0, 0, 1, 0),
    VALIDACE = c("", "", "chybný záznam", "", "", "", ""),
    CXPRESNOST = c(10, 10, 10, 10, 3000, 10, 10),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------------- GAP-13: notation

# `Criteria` keeps today's format (D10); Annex 2 notation is `Criteria_IUCN`.
has_fixed <- function(x, s) isTRUE(any(grepl(s, x, fixed = TRUE)))

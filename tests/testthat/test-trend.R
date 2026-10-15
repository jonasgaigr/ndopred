# Phase 5: data tiers and trends (review, Table 5 and section 7), the
# alpha-hull for EOO change (Guidelines 4.9) and the minimum data quality of
# Guidelines Table 3.1.

cfg26 <- ndopred_config(2026)

# Records at the centres of 2 x 2 km cells (EPSG:3035), one per cell and year.
cell_records <- function(cells_x, year, cell_y = 1, ...) {
  sf::st_as_sf(data.frame(x = cells_x * 2000 + 1000, y = cell_y * 2000 + 1000,
                          DATUM_OD = sprintf("%d0615", year), include = TRUE, ...),
               coords = c("x", "y"), crs = 3035)
}

# ------------------------------------------------------------- alpha-hull

test_that("The alpha-hull leaves out the gap between patches (Guidelines 4.9)", {
  sq <- function(ox) cbind(ox + c(0, 1000, 0, 1000), c(0, 0, 1000, 1000))
  xy <- rbind(sq(0), sq(50000))
  expect_equal(.alpha_hull_km2(xy), 2)
  p <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = 3035)
  expect_equal(calculate_eoo(p)$area_km2, 51)
  expect_equal(calculate_eoo_alpha(p, thin = NULL), 2)
})

test_that("Thinned to occupied cells, recording density does not move the hull", {
  base <- cell_records(1:20, 2020)
  set.seed(9)
  dense <- rbind(base, sf::st_as_sf(data.frame(x = runif(300, 2000, 4000), y = runif(300, 2000, 4000),
                                               DATUM_OD = "20200615", include = TRUE),
                                    coords = c("x", "y"), crs = 3035))
  expect_equal(calculate_eoo_alpha(dense), calculate_eoo_alpha(base))
})

test_that("batch_assess takes a group's effort layer from a folder (T1)", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  spp <- c("Stable widespread sp", "Declining sp", "Restricted stable sp")
  group <- do.call(rbind, lapply(seq_along(spp), function(i) {
    d <- mock_species(spp[i])
    d$DRUH <- spp[i]
    d$ID_NALEZ <- seq_len(nrow(d)) + 10000 * i
    d$AUTOR <- "Observer"
    d
  }))
  eff <- .as_effort(list(.effort_rows(group)), "Coleoptera", "Brouci", c(yn - 30, yn), (yn - 30):yn)
  dir <- withr::local_tempdir()
  saveRDS(eff, file.path(dir, sprintf("effort_Coleoptera_%d-%d.rds", yn - 30, yn)))
  res <- suppressMessages(batch_assess("Stable widespread sp", reference_year = yn, effort_dir = dir))
  expect_equal(res$Data_Tier, "T1")
  expect_match(res$Trend_Source, "relative occupancy")
  plain <- suppressMessages(batch_assess("Stable widespread sp", reference_year = yn))
  expect_equal(plain$Data_Tier, "T0")
  expect_match(plain$Trend_Sensitivity, "for comparison windows starting")
})

# ---------------------------------------------------------- decline state

test_that("A continuing decline is indicated only when the whole range says so", {
  expect_true(.decline_state(c(min = -40, best = -30, max = -10), 5))
  expect_false(.decline_state(c(min = -3, best = 2, max = 8), 5))
  expect_true(is.na(.decline_state(c(min = -20, best = -8, max = 4), 5)))
  expect_equal(.as_reduction(c(min = -40, best = -30, max = -10)), c(min = 10, best = 30, max = 40))
})

# --------------------------------------------------------------------- T0

test_that("T0: the raw change with its start-year range, labelled a diagnostic", {
  occ <- rbind(cell_records(1:40, 2004), cell_records(1:40, 2005), cell_records(1:40, 2006),
               cell_records(1:20, 2020), cell_records(1:20, 2021))
  te <- trend_estimate(occ, cfg26)
  expect_equal(te$tier, "T0")
  expect_equal(te$change[["best"]], -50)
  expect_true(te$decline_aoo)
  expect_match(te$source, "T0 diagnostic")
  expect_equal(te$sensitivity$starts, c(1998, 2002))
  expect_equal(nrow(te$sensitivity$table), 5)
})

# --------------------------------------------------------------------- T1

# An effort layer: the group recorded in cells 1-100 in 2005 and in 2020.
effort_layer_fixture <- function(n = 100) {
  d <- data.frame(ID_NALEZ = seq_len(2 * n), DRUH = "Other sp", date = rep(c(20050615, 20200615), each = n),
                  year = rep(c(2005, 2020), each = n), cell = rep(paste(seq_len(n), 1, sep = "_"), 2),
                  negative = FALSE, project = NA_character_, visit = seq_len(2 * n))
  structure(d, class = c("ndopred_effort", "data.frame"), group = "Brouci", years = c(2000, 2026))
}

test_that("T1: relative occupancy on cells recorded for the group in both windows", {
  occ <- rbind(cell_records(1:50, 2005), cell_records(1:25, 2020))
  te <- trend_estimate(occ, cfg26, effort = effort_layer_fixture())
  expect_equal(te$tier, "T1")
  expect_equal(te$t1$n_benchmark, 100)
  expect_equal(te$change[["best"]], -50)
  expect_lt(te$change[["min"]], -50)
  expect_gt(te$change[["max"]], -50)
  expect_match(te$source, "T1 interim proxy")
  # Too few cells for T1: the tier stays T0 and says why.
  few <- trend_estimate(occ, cfg26, effort = effort_layer_fixture(10))
  expect_equal(few$tier, "T0")
  expect_match(few$tier_reason, "too few cells")
})

test_that("ndop_effort downloads year by year, resumes, and splits a failed year by month", {
  dir <- withr::local_tempdir()
  calls <- character(0)
  fake <- function(lat, from, to) {
    calls <<- c(calls, from)
    if (from == "1.1.2001") return(NULL)            # too large: month by month
    yr <- as.numeric(sub(".*\\.", "", from))
    data.frame(ID_NALEZ = length(calls), DRUH = "A b", X = -700000, Y = -1080000,
               DATUM_OD = yr * 10000 + 615, ID_LOKAL = 1, AUTOR = "Observer", NEGATIV = 0,
               PROJEKT = "P")
  }
  eff <- ndop_effort("Amphibia", years = c(2000, 2001), dir = dir, download = fake)
  expect_s3_class(eff, "ndopred_effort")
  expect_equal(attr(eff, "years_done"), c(2000, 2001))
  expect_equal(sum(startsWith(calls, "1.") & grepl("\\.2001$", calls)), 13)   # the year, then 12 months
  expect_false("AUTOR" %in% names(eff))                                     # no observer names kept
  n_before <- length(calls)
  again <- ndop_effort("Amphibia", years = c(2000, 2001), dir = dir, download = fake)
  expect_equal(length(calls), n_before)                                     # nothing fetched twice
  expect_equal(nrow(effort_layer("Obojživelníci", dir)), nrow(again))
})

# --------------------------------------------------------------------- T2

test_that("T2: repeat visits are counted; without the model T1 is used", {
  # 25 cells with two visits a year in 2005 and 2020; the taxon on some.
  n <- 25
  rows <- expand.grid(cell = seq_len(n), year = c(2005, 2020), v = 1:2)
  d <- data.frame(ID_NALEZ = seq_len(nrow(rows)), DRUH = "Other sp", date = rows$year * 10000 + 615 + rows$v,
                  year = rows$year, cell = paste(rows$cell, 1, sep = "_"), negative = FALSE,
                  project = NA_character_, visit = seq_len(nrow(rows)))
  taxon_rows <- d[d$cell %in% paste(1:12, 1, sep = "_") & rows$v == 1, ]
  taxon_rows$DRUH <- "A b"
  taxon_rows$ID_NALEZ <- taxon_rows$ID_NALEZ + 1000
  eff <- structure(rbind(d, taxon_rows), class = c("ndopred_effort", "data.frame"))
  occ <- rbind(cell_records(1:12, 2005), cell_records(1:12, 2020))
  cfg <- ndopred_config(2026, trend = trend_settings(t1_min_cells = 10, t2_min_sites = 15, t2_min_years = 2))
  te <- trend_estimate(occ, cfg, effort = eff, taxon = "A b")
  expect_true(te$t2$eligible)
  expect_equal(te$t2$n_repeat_sites, 17)   # cells within 5 cells of the taxon (t2_buffer_cells)
  expect_equal(te$tier, "T1")
  expect_match(te$tier_reason, "occupancy model not run")
})

test_that("T2: the occupancy model runs on repeat visits when asked", {
  skip_if_not_installed("spOccupancy")
  skip_on_cran()
  set.seed(4)
  n <- 40
  rows <- expand.grid(cell = seq_len(n), year = 2000:2026, v = 1:3)
  occupied <- rows$cell <= 20                                  # the taxon in 20 cells, all years
  det <- occupied & stats::runif(nrow(rows)) < 0.5
  base <- data.frame(ID_NALEZ = seq_len(nrow(rows)), DRUH = "Other sp",
                     date = rows$year * 10000 + 600 + rows$v, year = rows$year,
                     cell = paste(rows$cell, 1, sep = "_"), negative = FALSE, project = NA_character_,
                     visit = seq_len(nrow(rows)))
  tx <- base[det, ]
  tx$DRUH <- "A b"
  tx$ID_NALEZ <- tx$ID_NALEZ + 1e6
  eff <- structure(rbind(base, tx), class = c("ndopred_effort", "data.frame"))
  occ <- do.call(rbind, lapply(c(2005, 2020), function(y) cell_records(1:20, y)))
  cfg <- ndopred_config(2026, trend = trend_settings(
    occupancy_model = TRUE, t2_min_sites = 20, t2_min_years = 5,
    t2_mcmc = list(n.batch = 40, batch.length = 25, n.burn = 400, n.thin = 2, n.chains = 2)))
  te <- trend_estimate(occ, cfg, effort = eff, taxon = "A b")
  expect_equal(te$tier, "T2")
  expect_match(te$source, "occupancy-detection model")
  # A stable taxon: the interval of the change includes no change.
  expect_lt(te$change[["min"]], 0)
  expect_gt(te$change[["max"]], 0)
})

# --------------------------------------------------------------------- T3

monitoring_fixture <- function(rate = -0.05) {
  rows <- expand.grid(site = 1:5, year = 2000:2026)
  rows$POCET <- round(40 * exp(rate * (rows$year - 2000)))
  sf::st_as_sf(data.frame(x = 4600000 + rows$site * 5000, y = 2900000,
                          DATUM_OD = sprintf("%d0615", rows$year), POCET = rows$POCET,
                          POCITANO = "adulti", PROJEKT = "Monitoring X", ID_LOKAL = rows$site,
                          NAZ_LOKAL = paste("Site", rows$site), include = TRUE),
               coords = c("x", "y"), crs = 3035)
}

test_that("T3: standardized monitoring gives an abundance index with basis (b)", {
  occ <- monitoring_fixture()
  cfg <- ndopred_config(2026, trend = trend_settings(monitoring_projects = "Monitoring X"))
  te <- trend_estimate(occ, cfg)
  expect_equal(te$tier, "T3")
  expect_equal(te$basis, "b")
  # 5 % a year over the 10 years between the window midpoints: about -39 %.
  expect_equal(round(te$change[["best"]]), round((exp(-0.5) - 1) * 100))
  expect_true(te$decline_aoo)
  # Without listed projects the same counts are T0.
  expect_equal(trend_estimate(occ, cfg26)$tier, "T0")
})

test_that("C1 needs an estimated decline: only a consistent-effort series gives one", {
  occ <- monitoring_fixture(-0.15)
  t3 <- ndopred_config(2026, trend = trend_settings(monitoring_projects = "Monitoring X"))
  with_t3 <- assess_taxon(occ, "A b", t3, evaluate_pop = TRUE)
  without <- assess_taxon(occ, "A b", cfg26, evaluate_pop = TRUE)
  expect_true(with_t3$metrics$pop$decline_estimated)
  expect_false(without$metrics$pop$decline_estimated)
  expect_true(with_t3$assessment$scenarios$working$C$c1)
  expect_false(without$assessment$scenarios$working$C$c1)
  expect_equal(with_t3$result$Data_Tier, "T3")
})

# -------------------------------------------------- Guidelines Table 3.1

test_that("Evidence below the minimum quality of its criterion is not used (Table 3.1)", {
  q <- function(field, qual, ...) {
    r <- ev_row(field, ...)
    r$qualifier <- qual
    r
  }
  ex <- evidence_for_engine(evidence_of(
    q("decline", "suspected", "TRUE", subfield = "iii"),
    q("mature_individuals", "inferred", best = 150),
    q("largest_subpopulation", "estimated", best = 40)))
  expect_null(ex$engine$decline)
  expect_null(ex$engine$mature)
  expect_equal(ex$engine$largest_subpop[["best"]], 40)
  expect_true(any(grepl("decline \\(iii\\) is suspected, below the minimum quality inferred", ex$checks)))
  expect_true(any(grepl("mature_individuals is inferred, below the minimum quality estimated", ex$checks)))
  # The entries stay on record.
  expect_equal(nrow(ex$entries), 3)
})

test_that("The census range: most recent counts, latest year, per-locality maxima", {
  occ <- data.frame(ROK = c(2016, 2025, 2020), POCET = c(100, 10, 30),
                    POCITANO = "adulti", NAZ_LOKAL = c("A", "A", "B"))
  res <- calculate_pop_metrics(occ, window_years = 10, year_now = 2025)
  expect_equal(c(res$total_mature_min, res$total_mature, res$total_mature_max), c(10, 40, 130))
})

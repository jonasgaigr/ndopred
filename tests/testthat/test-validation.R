# Pilot validation (review section 9): blinded expert comparison and
# stability under alternative record filters.

proposals_fixture <- function() {
  data.frame(
    Species = c("A a", "B b", "C c", "D d", "E e", "F f", "G g"),
    Category = c("EN", "VU", "LC", "VU", "NT", "LC", "VU"),
    Regional_Category = c("EN", "NT°", "LC", "VU", "NT", "LC", NA),
    Category_Range = c("VU-EN", "NT-VU", "LC-EN", "VU", "LC-VU", "LC", "VU"),
    Uncertain = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

experts_fixture <- function() {
  data.frame(
    taxon = c("A a", "B b", "C c", "D d", "E e", "F f", "G g", "H h"),
    category = c("EN", "NT", "VU", "LC", "DD", "LC", "EN", "CR"),
    group = c("well", "well", "well", "well", "poor", "poor", "poor", "poor"),
    stringsAsFactors = FALSE
  )
}

test_that("outcomes separate false threatened from false non-threatened", {
  v <- validate_against_experts(proposals_fixture(), experts_fixture())
  t <- v$taxa
  expect_equal(t$outcome, c("exact", "exact", "false non-threatened", "false threatened",
                            "not comparable", "exact", "same status", "no proposal"))
  # The regional category is compared (degree sign dropped), falling back
  # to the category where the regional step is missing.
  expect_equal(t$proposed[1:2], c("EN", "NT"))
  expect_equal(t$proposed[7], "VU")
  # The plausible range covers the expert category for C c (LC-EN, VU).
  expect_equal(t$range_covers[1:4], c(TRUE, TRUE, TRUE, FALSE))
})

test_that("rates are computed per assessed status, overall and per group", {
  v <- validate_against_experts(proposals_fixture(), experts_fixture())
  s <- v$summary
  expect_equal(s$group, c("all", "poor", "well"))
  all <- s[s$group == "all", ]
  expect_equal(all$comparable, 6)
  expect_equal(all$false_threatened, 1)
  expect_equal(all$false_non_threatened, 1)
  # Assessed threatened: C c (VU), G g (EN), A a (EN) -> 2 of 3 proposed threatened.
  expect_equal(all$assessed_threatened, 3)
  expect_equal(all$sensitivity, round(2 / 3, 3))
  expect_equal(all$false_non_threatened_rate, round(1 / 3, 3))
  # Assessed non-threatened: B b (NT), D d (LC), F f (LC) -> D d proposed VU.
  expect_equal(all$specificity, round(2 / 3, 3))
  expect_equal(all$false_threatened_rate, round(1 / 3, 3))
  expect_equal(all$not_comparable, 2)
  well <- s[s$group == "well", ]
  expect_equal(c(well$false_threatened, well$false_non_threatened), c(1, 1))
  expect_equal(v$confusion["VU", "LC"], 1)
  expect_equal(v$confusion["(none)", "CR"], 1)
})

test_that("the plain category column can be compared instead", {
  v <- validate_against_experts(proposals_fixture(), experts_fixture(), column = "Category")
  expect_equal(v$taxa$proposed[2], "VU")
  expect_equal(v$taxa$outcome[2], "false threatened")
  expect_warning(validate_against_experts(proposals_fixture(), experts_fixture(),
                                          column = "Final_Category"), "not a blind")
})

test_that("the expert table is checked", {
  ex <- experts_fixture()
  ex$category[1] <- "VUL"
  expect_error(validate_against_experts(proposals_fixture(), ex), "unknown expert categories")
  ex <- experts_fixture()
  ex$taxon[2] <- "A a"
  expect_error(validate_against_experts(proposals_fixture(), ex), "twice")
  ex <- experts_fixture()
  ex$blinded <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  expect_warning(validate_against_experts(proposals_fixture(), ex), "not marked blinded")
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(experts_fixture(), path, row.names = FALSE)
  expect_equal(nrow(validate_against_experts(proposals_fixture(), path)$taxa), 8)
})

stability_fixture <- function() {
  set.seed(3)
  wide <- data.frame(x = 4500000 + stats::runif(700, 0, 300000),
                     y = 2850000 + stats::runif(700, 0, 200000), part = "wide")
  core <- data.frame(x = 4800000 + stats::runif(30, 0, 20000),
                     y = 2900000 + stats::runif(30, 0, 20000), part = "core")
  d <- rbind(wide, core)
  d$DATUM_OD <- "20200615"
  d$DATUM_DO <- "20200615"
  d$VEROH <- c(rep(0, 100), rep(1, 630))
  d$flag_literature <- c(rep(FALSE, 700), rep(TRUE, 3), rep(FALSE, 27))
  d$flag_imprecise <- FALSE
  d$include <- TRUE
  d$exclusion_reason <- NA_character_
  sf::st_as_sf(d, coords = c("x", "y"), crs = 3035)
}

test_that("filter variants drop the records they name", {
  occ <- stability_fixture()
  v <- filter_variants(drop_share = 0.1, seed = 1)
  expect_equal(sum(v$baseline(occ)), 0)
  expect_equal(sum(v$no_unvalidated(occ)), 100)
  expect_equal(sum(v$no_literature(occ)), 3)
  expect_equal(sum(v$precise_only(occ)), 0)
  expect_equal(v$data_loss(occ), v$data_loss(occ))
  expect_gt(sum(v$data_loss(occ)), 30)
})

test_that("filter_stability() reports where the category changes", {
  occ <- stability_fixture()
  cfg <- ndopred_config(reference_year = 2026, trend = trend_settings(reps = 9))
  # Severe fragmentation and a habitat decline: B is met wherever the area
  # thresholds are (no threat, so no VU D2).
  ev <- list(severely_fragmented = TRUE, decline = c(iii = TRUE))
  vars <- c(filter_variants()[c("baseline", "no_literature")],
            core_only = function(o) o$part == "wide")
  st <- filter_stability(occ, cfg, "Taxon s", variants = vars, evidence = ev)
  r <- st$results
  expect_equal(r$Variant, c("baseline", "no_literature", "core_only"))
  expect_equal(r$Records_Dropped, c(0, 3, 700))
  expect_equal(r$Category[c(1, 3)], c("LC", "EN"))
  expect_equal(r$changed, c(FALSE, FALSE, TRUE))
  expect_equal(r$status_changed, c(FALSE, FALSE, TRUE))
  expect_false(st$summary$stable)
  expect_equal(st$summary$changed_by, "core_only")
  expect_error(filter_stability(occ, cfg, variants = vars), "taxon")
})

test_that("region_variants() drops the records inside each region", {
  occ <- stability_fixture()
  box <- function(x0, x1) sf::st_as_sfc(sf::st_bbox(c(xmin = x0, xmax = x1, ymin = 2800000, ymax = 3100000),
                                                    crs = sf::st_crs(3035)))
  regions <- sf::st_sf(name = c("West", "East"), geometry = c(box(4400000, 4650000), box(4650000, 4900000)))
  rv <- region_variants(regions, "name")
  expect_equal(names(rv), c("without_West", "without_East"))
  x <- sf::st_coordinates(occ)[, 1]
  expect_equal(rv$without_West(occ), x < 4650000)
  expect_equal(sum(rv$without_West(occ)) + sum(rv$without_East(occ)), nrow(occ))
})

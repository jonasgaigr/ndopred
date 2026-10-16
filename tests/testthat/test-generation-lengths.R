# Generation lengths per NDOP taxonomic group (AOPK, 2026-10-15): a table
# with a placeholder of 1 year for every group, to be refined.

test_that("The table has one row per NDOP group, every value a placeholder of 1 year", {
  gl <- generation_lengths()
  expect_equal(nrow(gl), 58)
  expect_false(anyDuplicated(gl$ndop_code) > 0)
  expect_false(anyDuplicated(gl$group_lat) > 0)
  expect_true(all(gl$generation_length_years == 1))
  expect_true(all(gl$status == "placeholder"))
  expect_true(all(c("Amphibia", "Mammalia", "Lepidoptera", "Coleoptera") %in% gl$group_lat))
})

test_that("A group is found by its NDOP (Czech), Latin or English name", {
  expect_equal(generation_length_for("Obojživelníci")$group, "Obojživelníci")
  expect_equal(generation_length_for("amphibia")$group, "Obojživelníci")
  expect_equal(generation_length_for("Amphibians")$group, "Obojživelníci")
  hit <- generation_length_for("Brouci")
  expect_equal(hit$years, 1)
  expect_equal(hit$status, "placeholder")
  expect_match(hit$source, "group table: Brouci")
  # Not in the table, or unknown: 1 year, labelled as a default.
  miss <- generation_length_for("Nonexistent group")
  expect_equal(c(miss$years, miss$status), c(1, "default"))
  expect_equal(generation_length_for(NA)$status, "default")
})

test_that("A longer generation length moves the comparison window back (Criteria A2)", {
  w1 <- ndopred_config(2026, generation_length = 1)$windows
  expect_equal(c(w1$comp_start, w1$comp_end), c(2000, 2016))
  w3 <- ndopred_config(2026, generation_length = 3)$windows   # 3 x 3 < 10
  expect_equal(c(w3$comp_start, w3$comp_end), c(2000, 2016))
  w5 <- ndopred_config(2026, generation_length = 5, time_depth_start = NA)$windows   # 3 x 5 = 15
  expect_equal(c(w5$comp_start, w5$comp_end), c(1995, 2011))
  expect_equal(c(w5$recent_start, w5$recent_end), c(2010, 2026))
  expect_false(w5$time_depth_limited)
  # An explicit shift still wins.
  expect_equal(ndopred_config(2026, generation_length = 5, trend_shift = 10)$windows$comp_end, 2016)
})

test_that("The comparison window stops at the time-depth limit, and says so", {
  w <- ndopred_config(2026, generation_length = 17)$windows   # 3 x 17 = 51 years
  expect_equal(c(w$comp_start, w$comp_end), c(2000, 2016))
  expect_equal(w$a_period, 51)
  expect_true(w$time_depth_limited)
  w <- ndopred_config(2026, generation_length = 5, time_depth_start = 1990)$windows
  expect_equal(c(w$comp_start, w$comp_end), c(1995, 2011))   # room enough
  expect_false(w$time_depth_limited)
  w <- ndopred_config(2026, generation_length = 17, time_depth_start = 1990)$windows
  expect_equal(c(w$comp_start, w$comp_end), c(1990, 2006))
  expect_true(w$time_depth_limited)
  # Never shorter than the 10-year floor, and no flag without a longer period.
  w <- ndopred_config(2025, generation_length = 1)$windows
  expect_equal(c(w$comp_start, w$comp_end), c(1999, 2015))
  expect_false(w$time_depth_limited)
  # Explicit windows are the assessor's: not limited, not flagged.
  w <- ndopred_config(2026, generation_length = 17, trend_shift = 51)$windows
  expect_equal(w$comp_start, 1959)
  expect_false(w$time_depth_limited)
  # Evidence on the generation length re-derives the windows under the limit.
  up <- ndopred:::.update_config(ndopred_config(2026), list(generation_length = 20))
  expect_equal(up$windows$comp_start, 2000)
  expect_true(up$windows$time_depth_limited)
})

test_that("The refined set looks a taxon up by species, genus, family, then group", {
  r <- function(g, t) generation_length_for(g, taxon = t, set = "refined")
  bb <- r("Obojživelníci", "Bombina bombina")
  expect_equal(bb$level, "species")
  expect_equal(bb$status, "refined")
  expect_gt(bb$years, 3.3)
  # A synonym and an infraspecific name find the species row.
  expect_equal(r("Ptáci", "Sylvia communis")$level, "species")
  expect_equal(r("Obojživelníci", "Triturus cristatus s. l.")$years, r("Obojživelníci", "Triturus cristatus")$years)
  expect_match(r("Obojživelníci", "Triturus cristatus s. l.")$source, "binomial of")
  expect_equal(r("Plazi", "Elaphe longissima")$years, r("Plazi", "Zamenis longissimus")$years)
  # Genus rows, and family through the AOPK checklist.
  expect_equal(r("Cévnaté rostliny", "Tilia cordata")$level, "genus")
  q <- r("Cévnaté rostliny", "Quercus robur")
  expect_equal(c(q$level, q$years), c("family", "60"))
  expect_match(q$source, "Fagaceae, family of Quercus")
  expect_equal(r("Houby", "Boletus edulis")$years, 17)
  expect_equal(r("Brouci", "Melolontha melolontha")$level, "genus")
  # Homonyms stay in their group: Prunella is a bird and a plant.
  expect_equal(r("Ptáci", "Prunella modularis")$level, "species")
  expect_equal(r("Cévnaté rostliny", "Prunella vulgaris")$level, "family")
  # Group default, then the 1-year default.
  expect_equal(r("Brouci", "Carabus coriaceus")$level, "group")
  expect_equal(r("Neznámá", "Foo bar")[c("years", "status")], list(years = 1, status = "default"))
  # Every NDOP group has a refined default.
  cl <- generation_lengths_refined()$clades
  expect_true(all(generation_lengths()$group_lat %in% cl$group_lat[cl$rank == "group"]))
  expect_true(all(generation_lengths_refined()$species$generation_length_years > 0))
})

test_that("The placeholder set stays the default; the refined one is opt-in", {
  withr::local_options(ndopred.generation_lengths = NULL)
  withr::local_envvar(NDOPRED_GENERATION_LENGTH_SET = "")
  expect_equal(generation_length_set(), "placeholder")
  expect_equal(generation_length_for("Obojživelníci", taxon = "Bombina bombina")$years, 1)
  old <- generation_length_set("refined")
  expect_equal(old, "placeholder")
  expect_equal(generation_length_for("Obojživelníci", taxon = "Bombina bombina")$level, "species")
  generation_length_set(old)
  expect_equal(generation_length_set(), "placeholder")
  # Without the option, the environment variable decides.
  options(ndopred.generation_lengths = NULL)
  withr::local_envvar(NDOPRED_GENERATION_LENGTH_SET = "refined")
  expect_equal(generation_length_set(), "refined")
  expect_error(generation_length_set("other"))
})

test_that("A taxon whose criterion A period outreaches the data gets a time-depth note", {
  local_mock_ndop()
  res <- assess_species("Declining sp", recent_start = 2010, recent_end = 2026, generation_length = 17)
  expect_equal(res$Generation_Length, 17)
  expect_match(res$issue_trend, "time depth: criterion A period 51 years", fixed = TRUE)
  short <- assess_species("Declining sp", recent_start = 2010, recent_end = 2026, generation_length = 2)
  expect_false(isTRUE(grepl("time depth", short$issue_trend)))
})

test_that("assess_species takes the generation length from the taxon's group", {
  local_mock_ndop()
  tab <- generation_lengths()
  tab$generation_length_years[tab$kat_tax == "Brouci"] <- 5
  tab$status[tab$kat_tax == "Brouci"] <- "test value"
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(tab, path, row.names = FALSE, fileEncoding = "UTF-8")
  withr::local_envvar(NDOPRED_GENERATION_LENGTHS = path)

  res <- assess_species("Declining sp", recent_start = 2010, recent_end = 2026)
  expect_equal(res$Generation_Length, 5)
  expect_match(res$Generation_Length_Source, "group table: Brouci (test value", fixed = TRUE)
  # A value passed for the taxon wins over the table.
  given <- assess_species("Declining sp", recent_start = 2010, recent_end = 2026,
                          generation_length = 2)
  expect_equal(given$Generation_Length, 2)
  expect_equal(given$Generation_Length_Source, "given")
})

test_that("A malformed table is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(kat_tax = "X", group_lat = "X", group_en = "x",
                              generation_length_years = 0, status = "s", source = "s"),
                   path, row.names = FALSE)
  expect_error(generation_lengths(path), "positive")
  utils::write.csv(data.frame(kat_tax = "X"), path, row.names = FALSE)
  expect_error(generation_lengths(path), "lacks column")
})

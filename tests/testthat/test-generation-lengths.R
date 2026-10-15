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
  w5 <- ndopred_config(2026, generation_length = 5)$windows   # 3 x 5 = 15
  expect_equal(c(w5$comp_start, w5$comp_end), c(1995, 2011))
  expect_equal(c(w5$recent_start, w5$recent_end), c(2010, 2026))
  # An explicit shift still wins.
  expect_equal(ndopred_config(2026, generation_length = 5, trend_shift = 10)$windows$comp_end, 2016)
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

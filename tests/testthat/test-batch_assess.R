test_that("assess_species returns a one-row result frame", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))

  # Previously this errored with 'argument "pop_metrics" is missing, with no
  # default': assess_species() called summarize_assessment() with five
  # positional arguments, so batch_assess() produced nothing at all.
  res <- assess_species("Declining sp",
                        recent_start = yn - 9, recent_end = yn,
                        comp_start = yn - 19, comp_end = yn - 10)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Species, "Declining sp")
  expect_true(res$Category %in% c("CR", "EN", "VU", "NT", "LC", "DD", "RE"))
})

test_that("batch_assess binds every taxon into one table", {
  local_mock_ndop()
  res <- batch_assess(c("Declining sp", "Stable widespread sp", "Barely known sp"))

  expect_equal(nrow(res), 3)
  expect_setequal(res$Species,
                  c("Declining sp", "Stable widespread sp", "Barely known sp"))
  # Two records only -> DD, via the same screen the Shiny apps use rather
  # than the old "EX?" pseudo-category.
  expect_equal(res$Category[res$Species == "Barely known sp"], "DD")
})

test_that("batch_assess default windows do not overlap", {
  local_mock_ndop()
  expect_no_warning(batch_assess("Stable widespread sp"))
})

test_that("A taxon absent from the recent window is screened, not called EX?", {
  local_mock_ndop()
  res <- batch_assess("Ghost sp")
  expect_equal(res$Category, "RE")
})

test_that("assess_criterion_d2 requires a plausible threat", {
  expect_equal(assess_criterion_d2(10, 3), "LC")
  expect_equal(assess_criterion_d2(10, 3, plausible_threat = TRUE), "VU")
  expect_equal(assess_criterion_d2(5000, 40, plausible_threat = TRUE), "LC")
})

test_that("assess_criterion_a applies the A1 and A2 threshold sets", {
  expect_equal(assess_criterion_a(list(percent_change = -85)), "CR")
  expect_equal(assess_criterion_a(list(percent_change = -85), "A1"), "EN")
  expect_equal(assess_criterion_a(list(percent_change = -22)), "NT")
  expect_equal(assess_criterion_a(list(percent_change = 15)), "LC")
  expect_equal(assess_criterion_a(list(percent_change = NA)), "DD")
})

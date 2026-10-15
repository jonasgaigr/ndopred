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
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  res <- suppressMessages(batch_assess(
    c("Declining sp", "Stable widespread sp", "Barely known sp"), reference_year = yn
  ))

  expect_equal(nrow(res), 3)
  expect_setequal(res$Species,
                  c("Declining sp", "Stable widespread sp", "Barely known sp"))
  # Two records only: a proposal is still made (Guidelines 10.4: few records
  # are not a reason for DD), with its range and what needs evidence.
  barely <- res[res$Species == "Barely known sp", ]
  expect_false(barely$Category == "DD")
  expect_match(barely$Flags, "DD candidate")
  expect_true(nzchar(barely$Needs_Evidence))
  expect_true(all(res$Reference_Year == yn))
})

test_that("batch_assess needs an explicit reference year", {
  expect_error(batch_assess("Stable widespread sp"), "reference_year")
})

test_that("batch_assess default windows do not overlap", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  expect_no_warning(suppressMessages(batch_assess("Stable widespread sp", reference_year = yn)))
})

test_that("A taxon absent for decades is an RE candidate, not called EX?", {
  local_mock_ndop()
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  res <- suppressMessages(batch_assess("Ghost sp", reference_year = yn))
  expect_equal(res$Category, "RE")
  expect_equal(res$Support, "candidate")
})

test_that("assess_criterion_d2 requires a specified plausible threat", {
  expect_true(is.na(assess_criterion_d2(10, 3)))                 # threat unknown
  expect_equal(assess_criterion_d2(10, 3, plausible_threat = "drainage"), "VU")
  expect_equal(assess_criterion_d2(10, 3, plausible_threat = FALSE), "LC")
  expect_warning(res <- assess_criterion_d2(10, 3, plausible_threat = TRUE), "does not specify")
  expect_true(is.na(res))
  expect_equal(assess_criterion_d2(5000, 40, plausible_threat = "drainage"), "LC")
})

test_that("assess_criterion_a applies the A1 and A2 threshold sets", {
  expect_equal(assess_criterion_a(list(percent_change = -85)), "CR")
  expect_equal(assess_criterion_a(list(percent_change = -85), "A1"), "EN")
  expect_equal(assess_criterion_a(list(percent_change = -22)), "NT")
  expect_equal(assess_criterion_a(list(percent_change = 15)), "LC")
  # Unknown, not DD: DD is a decision about the whole assessment.
  expect_true(is.na(assess_criterion_a(list(percent_change = NA))))
})

test_that("batch_assess defaults to the AOPK windows (D5)", {
  seen <- NULL
  local_mocked_bindings(assess_species = function(sp, recent_start, recent_end,
                                                  comp_start, comp_end, ...) {
    seen <<- c(recent_start, recent_end, comp_start, comp_end)
    data.frame(Species = sp)
  })
  suppressMessages(batch_assess("Any sp", reference_year = 2026))
  expect_equal(seen, c(2010, 2026, 2000, 2016))
})

make_raw <- function(n_negative_na = FALSE) {
  # Mimics the shape returned by rndop::ndop_download(): one row is missing
  # coordinates, one is a flagged negative finding, one has no cadastral
  # unit, one is low-verification (VEROH 3 = dubious), one is fully clean.
  data.frame(
    X = c(NA, 4600000, 4610000, 4620000, 4630000),
    Y = c(2900000, 2900000, 2900100, 2900200, 2900300),
    NEGATIV = c(0, 1, 0, 0, if (n_negative_na) NA else 0),
    KATASTR = c("A", "A", "", "A", "A"),
    VEROH = c(1, 1, 1, 3, 1),
    DATUM_OD = c("20200101", "20200101", "20200101", "20200101", "20200101"),
    stringsAsFactors = FALSE
  )
}

test_that("filter_counts attribute traces records through each QC step", {
  testthat::local_mocked_bindings(
    ndop_download = function(...) make_raw(),
    .package = "rndop"
  )

  res <- get_assessment_data("Test species")
  fc <- attr(res, "filter_counts")

  expect_equal(fc$raw, 5)
  expect_equal(fc$coords, 4)        # drops the NA-coordinate row
  expect_equal(fc$non_negative, 3)  # drops the NEGATIV == 1 row
  expect_equal(fc$has_katastr, 2)   # drops the blank-KATASTR row
  expect_equal(fc$verified, 1)      # drops the VEROH == 3 row
  expect_equal(nrow(res), 1)
})

test_that("blank/NA NEGATIV is treated as non-negative, not dropped", {
  testthat::local_mocked_bindings(
    ndop_download = function(...) make_raw(n_negative_na = TRUE),
    .package = "rndop"
  )

  res <- get_assessment_data("Test species")
  fc <- attr(res, "filter_counts")

  # Same as above, but the last row's NEGATIV is NA instead of 0 - it must
  # still survive the non-negative step (NA means "not flagged", not
  # "unknown/exclude").
  expect_equal(fc$non_negative, 3)
})

test_that("Zero raw records produces a fully-zeroed filter_counts", {
  testthat::local_mocked_bindings(
    ndop_download = function(...) NULL,
    .package = "rndop"
  )

  res <- suppressWarnings(get_assessment_data("Nonexistent species"))
  fc <- attr(res, "filter_counts")

  expect_equal(fc$raw, 0)
  expect_equal(nrow(res), 0)
})

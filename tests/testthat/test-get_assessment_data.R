make_raw <- function(n_negative_na = FALSE) {
  # Mimics the shape returned by rndop::ndop_download() (S-JTSK coordinates):
  # one row is missing coordinates, one is a negative finding, one has no
  # cadastral unit, one is less credible (VEROH 3), one is clean.
  data.frame(
    ID_NALEZ = 1:5,
    X = c(NA, -700000, -701000, -702000, -703000),
    Y = c(-1080000, -1080000, -1080100, -1080200, -1080300),
    NEGATIV = c(0, 1, 0, 0, if (n_negative_na) NA else 0),
    KATASTR = c("A", "A", "", "A", "A"),
    VEROH = c(1, 1, 1, 3, 1),
    DATUM_OD = c("20200101", "20200101", "20200101", "20200101", "20200101"),
    stringsAsFactors = FALSE
  )
}

test_that("filter_counts traces records through each QC step, and nothing is deleted", {
  testthat::local_mocked_bindings(
    ndop_download = function(...) make_raw(),
    .package = "rndop"
  )

  res <- get_assessment_data("Test species")
  fc <- attr(res, "filter_counts")

  expect_equal(fc$raw, 5)
  expect_equal(fc$coords, 4)        # the NA-coordinate row
  expect_equal(fc$non_negative, 3)  # the NEGATIV == 1 row
  expect_equal(fc$has_katastr, 2)   # the blank-KATASTR row
  # VEROH 3 is one record in five (20 % > 10 %): kept and flagged (D8).
  expect_equal(fc$verified, 2)
  expect_equal(fc$included, 2)
  # Every record keeps its row; exclusions are flags with a reason.
  expect_equal(nrow(res), 5)
  expect_equal(sum(res$include), 2)
  expect_true(res$flag_less_credible[res$ID_NALEZ == 4])
  expect_true(res$include[res$ID_NALEZ == 4])
  expect_match(res$exclusion_reason[res$ID_NALEZ == 2], "negative finding")
})

test_that("VEROH 3 is excluded when it is at most 10 % of the taxon's records", {
  raw <- rbind(make_raw(), make_raw(), make_raw())
  raw$ID_NALEZ <- seq_len(nrow(raw))
  raw$VEROH <- 1
  raw$VEROH[1] <- 3                 # 1 of 15 = 7 %
  raw$X[1] <- -704000
  testthat::local_mocked_bindings(ndop_download = function(...) raw, .package = "rndop")
  res <- get_assessment_data("Test species")
  expect_false(res$include[1])
  expect_match(res$exclusion_reason[1], "VEROH 3")
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

test_that("A failed download is recorded, not passed off as zero records", {
  testthat::local_mocked_bindings(
    ndop_download = function(...) stop("Timeout was reached [login.nature.cz]"),
    .package = "rndop"
  )
  res <- suppressMessages(get_assessment_data("Any species"))
  expect_match(attr(res, "download_error"), "Timeout")
})

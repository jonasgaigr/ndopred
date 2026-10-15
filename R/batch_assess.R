#' Batch Assessment of Many Taxa
#'
#' Runs [assess_species()] over a list of taxa with one configuration. A
#' taxon that fails keeps its row: the category is `NA` and an `issue_*`
#' column says why, so no taxon silently drops out of the table.
#'
#' Windows default to the ten years ending in `reference_year`, compared with
#' the ten years before them.
#'
#' @param species_list Vector of scientific names.
#' @param reference_year Numeric. The assessment year (required; ndopred does
#'   not read the system clock).
#' @param recent_end,recent_start Numeric. The current window.
#' @param comp_end,comp_start Numeric. The comparison window.
#' @param snapshot_dir Optional folder in which every download is stored as
#'   an RDS snapshot, so the run can be repeated exactly; see
#'   [default_snapshot_dir()]. The file name is in the `Snapshot` column.
#' @param ... Passed to [assess_species()]: `evaluate_pop`, expert evidence
#'   and configuration.
#' @return A data frame with one row per input taxon.
#' @seealso [assess_species()], [run_bulk_app()] for the same screening with
#'   a UI, maps and a CSV export.
#' @examples
#' \dontrun{
#' batch_assess(c("Lucanus cervus", "Bombina bombina"), reference_year = 2025)
#'
#' # Explicit windows, with NDOP counts used for criteria C and D.
#' batch_assess(
#'   c("Cypripedium calceolus", "Pulsatilla vernalis"),
#'   reference_year = 2025, recent_start = 2016,
#'   comp_end = 2015, comp_start = 2006,
#'   evaluate_pop = TRUE
#' )
#' }
#' @export
batch_assess <- function(species_list,
                         reference_year,
                         recent_end = reference_year,
                         recent_start = recent_end - 9,
                         comp_end = recent_start - 1,
                         comp_start = comp_end - 9,
                         snapshot_dir = NULL,
                         ...) {
  if (missing(reference_year)) {
    stop("`reference_year` is required: ndopred no longer reads the system clock.",
         call. = FALSE)
  }
  if (recent_start <= comp_end) {
    warning("Recent and comparison windows overlap; measured change will be ",
            "damped towards zero.", call. = FALSE)
  }

  rows <- lapply(species_list, function(sp) {
    message(paste("--- Processing:", sp, "---"))
    tryCatch(
      assess_species(sp, recent_start = recent_start, recent_end = recent_end,
                     comp_start = comp_start, comp_end = comp_end,
                     reference_year = reference_year, snapshot_dir = snapshot_dir, ...),
      error = function(e) {
        message(paste("Error processing", sp, ":", conditionMessage(e)))
        failed <- .proposal(NA_character_, NA_character_, support = "none",
                            needs = "assessment failed - see issue_records",
                            range = NA_character_)
        row <- .result_row(sp, failed, NA, NA, NA, NA)
        row$Reference_Year <- reference_year
        cbind(row, .issue_columns(list(records = paste0("error: ", conditionMessage(e)))))
      }
    )
  })
  dplyr::bind_rows(rows)
}

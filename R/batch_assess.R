#' Batch Assessment of Many Taxa
#'
#' Runs [assess_species()] over a list of taxa with one configuration. A
#' taxon that fails keeps its row: the category is `NA` and an `issue_*`
#' column says why, so no taxon silently drops out of the table.
#'
#' Windows default to the AOPK convention (decision D5): the current window
#' is the 17 years ending in `reference_year` (2010-2026 for 2026), compared
#' with the same window ten years earlier (2000-2016), or three generations
#' earlier where that is longer. Each taxon's generation length comes from
#' its NDOP group in [generation_lengths()] unless `generation_length` is
#' passed. See [ndopred_config()].
#'
#' @param species_list Vector of scientific names.
#' @param reference_year Numeric. The assessment year (required; ndopred does
#'   not read the system clock).
#' @param recent_end,recent_start Numeric. The current window.
#' @param comp_end,comp_start Numeric or `NULL`. The comparison window;
#'   `NULL` derives it per taxon from the generation length.
#' @param evidence Optional expert evidence for any of the taxa: an evidence
#'   table or the path of an evidence file (see [evidence]). Each taxon uses
#'   its own rows; the final category columns report the checked final
#'   category and its approval.
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
#'   comp_start = 2006, comp_end = 2015,
#'   evaluate_pop = TRUE
#' )
#' }
#' @export
batch_assess <- function(species_list,
                         reference_year,
                         recent_end = reference_year,
                         recent_start = recent_end - 16,
                         comp_start = NULL,
                         comp_end = NULL,
                         snapshot_dir = NULL,
                         evidence = NULL,
                         ...) {
  if (missing(reference_year)) {
    stop("`reference_year` is required: ndopred no longer reads the system clock.",
         call. = FALSE)
  }

  if (is.character(evidence) && length(evidence) == 1) evidence <- read_evidence(evidence)
  if (!is.null(evidence)) evidence <- validate_evidence(evidence)

  rows <- lapply(species_list, function(sp) {
    message(paste("--- Processing:", sp, "---"))
    tryCatch(
      assess_species(sp, recent_start = recent_start, recent_end = recent_end,
                     comp_start = comp_start, comp_end = comp_end,
                     reference_year = reference_year, snapshot_dir = snapshot_dir,
                     evidence = evidence, ...),
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

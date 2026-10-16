#' Data Dictionary of the Export
#'
#' Every column of the bulk export: its name in [batch_assess()] (and
#' [assess_taxon()]) and in the bulk app's Czech export ([run_bulk_app()]),
#' what it holds, its values, the layer it belongs to and the IUCN section
#' behind it. The established Czech column names are unchanged; new
#' columns follow them (gap analysis, D10).
#'
#' @section Layers:
#' ndopred keeps apart what is calculated, what is evidenced, what is
#' proposed and what is approved:
#' * `input` and `run`: what the run was given, and its settings;
#' * `calculated`: measured from the records (EOO, AOO, counts);
#' * `indication`: derived from the records, but not evidence (clusters,
#'   raw changes, trend estimates);
#' * `parameter`: values the criteria need besides the records
#'   (generation length);
#' * `evidence`: entered by an assessor in the evidence file;
#' * `proposal`: the engine's proposal and what it rests on;
#' * `approved`: the final category and its state, set by the group experts;
#' * `diagnostic`: the reason for every empty value (`issue_*`).
#'
#' @section SIS fields:
#' `sis_field_candidate` names the field of an IUCN SIS export
#' (`allfields.csv`, as written by sRedList) a column could move to. The
#' mapping is a proposal: check it against the current SIS schema before
#' any transfer. Only an approved final category is a Red List category;
#' location counts for SIS come from the evidence file, not from the
#' clusters.
#'
#' @param path Path to a CSV with the same columns; defaults to the table
#'   shipped with the package (`inst/extdata/export_dictionary.csv`).
#' @return A data frame with `batch_column`, `app_column`, `layer`,
#'   `description`, `values`, `iucn_reference` and `sis_field_candidate`.
#'   A column only one of the two exports has is empty in the other.
#' @seealso [batch_assess()], [run_bulk_app()]
#' @examples
#' d <- export_dictionary()
#' d[d$layer == "approved", c("batch_column", "app_column")]
#' @export
export_dictionary <- function(path = system.file("extdata", "export_dictionary.csv",
                                                 package = "ndopred")) {
  if (!nzchar(path) || !file.exists(path)) stop("Export dictionary not found: ", path, call. = FALSE)
  d <- utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                       na.strings = character(0))
  need <- c("batch_column", "app_column", "layer", "description", "values",
            "iucn_reference", "sis_field_candidate")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop("Export dictionary lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  d[need]
}

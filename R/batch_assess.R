#' Batch IUCN Assessment (Harmonised Temporal Scale)
#'
#' Runs [assess_species()] over a list of taxa. Windows default to the last ten
#' years compared against the ten years immediately preceding them, so the two
#' periods are of equal length and do not overlap.
#'
#' @param species_list Vector of scientific names.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @param ... Further arguments passed to [assess_species()].
#' @return A data frame with one row per successfully processed taxon. Taxa
#'   that error are reported via [message()] and omitted, so the run continues.
#' @seealso [assess_species()] for a single taxon, [run_bulk_app()] for the
#'   same screening with a UI, maps and a CSV export.
#' @examples
#' \dontrun{
#' # Default windows: last ten years vs. the ten before them.
#' batch_assess(c("Lucanus cervus", "Bombina bombina"))
#'
#' # Explicit windows, with criteria C and D1 enabled.
#' batch_assess(
#'   c("Cypripedium calceolus", "Pulsatilla vernalis"),
#'   recent_end = 2025, recent_start = 2016,
#'   comp_end = 2015, comp_start = 2006,
#'   evaluate_pop = TRUE
#' )
#' }
#' @export
batch_assess <- function(species_list,
                         recent_end = as.numeric(format(Sys.Date(), "%Y")),
                         recent_start = recent_end - 9,
                         comp_end = recent_start - 1,
                         comp_start = comp_end - 9,
                         ...) {

  if (recent_start <= comp_end) {
    warning("Recent and comparison windows overlap; measured change will be ",
            "damped towards zero.", call. = FALSE)
  }

  results_list <- list()

  for (sp in species_list) {
    message(paste("--- Processing:", sp, "---"))

    res <- tryCatch({
      assess_species(
        sp,
        recent_start = recent_start,
        recent_end = recent_end,
        comp_start = comp_start,
        comp_end = comp_end,
        ...
      )
    }, error = function(e) {
      message(paste("Error processing", sp, ":", e$message))
      NULL
    })

    if (!is.null(res)) results_list[[sp]] <- res
  }

  if (length(results_list) == 0) {
    return(data.frame(
      Species = character(0), Category = character(0), Criteria = character(0),
      EOO_km2 = numeric(0), AOO_km2 = numeric(0), Locations = numeric(0),
      Trend_Perc = numeric(0), stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(results_list)
}

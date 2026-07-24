#' Batch IUCN Assessment (Harmonised Temporal Scale)
#'
#' @param species_list Vector of scientific names.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @export
batch_assess <- function(species_list,
                         recent_start = 2010, recent_end = 2026,
                         comp_start = 2000, comp_end = 2016) {

  results_list <- list()

  for (sp in species_list) {
    message(paste("--- Processing:", sp, "---"))

    tryCatch({
      # Delegate entirely to the harmonised single-species assessment function
      results_list[[sp]] <- assess_species(
        sp,
        recent_start = recent_start,
        recent_end = recent_end,
        comp_start = comp_start,
        comp_end = comp_end
      )
    }, error = function(e) {
      message(paste("Error processing", sp, ":", e$message))
    })
  }

  final_table <- dplyr::bind_rows(results_list)
  return(final_table)
}

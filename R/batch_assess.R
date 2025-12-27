#' Batch IUCN Assessment
#'
#' @param species_list Vector of scientific names.
#' @param window_years Numeric. Year window for trend (default 10).
#' @export
batch_assess <- function(species_list, window_years = 10) {

  results_list <- list()

  for (sp in species_list) {
    message(paste("--- Processing:", sp, "---"))

    # Wrap in tryCatch so one bad species name doesn't crash the whole loop
    tryCatch({
      occ <- get_assessment_data(sp)
      eoo <- calculate_eoo(occ)
      aoo <- calculate_aoo(occ)
      trend <- calculate_trend(occ, window_years = window_years)
      locs <- calculate_locations(occ)

      results_list[[sp]] <- summarize_assessment(sp, eoo, aoo, trend, locs)
    }, error = function(e) {
      message(paste("Error processing", sp, ":", e$message))
    })
  }

  # Combine list into one clean data frame
  final_table <- dplyr::bind_rows(results_list)
  return(final_table)
}

#' Batch IUCN Assessment (Harmonized Temporal Scale)
#'
#' @param species_list Vector of scientific names.
#' @param recent_years Numeric. Window for current status (default 10).
#' @export
batch_assess <- function(species_list, recent_years = 10) {

  results_list <- list()
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  start_year_recent <- current_year - recent_years

  for (sp in species_list) {
    message(paste("--- Processing:", sp, "---"))

    tryCatch({
      # 1. Get & Clean Data
      raw_occ <- get_assessment_data(sp)
      occ_all <- clean_dates(raw_occ)

      # 2. Filter for Current Status (Criterion B)
      # STRICTLY last 10 years for EOO, AOO, Locations
      occ_recent <- occ_all %>%
        dplyr::filter(year >= start_year_recent)

      if (nrow(occ_recent) == 0) {
        message(paste("  - No records in last", recent_years, "years!"))
        # Create empty result
        results_list[[sp]] <- data.frame(
          Species = sp, EOO_km2 = 0, AOO_km2 = 0, Locations = 0,
          Trend_Perc = NA, Category = "EX?", Note = "No recent data"
        )
        next
      }

      # 3. Compute Metrics
      # Spatial (Recent Only)
      eoo <- calculate_eoo(occ_recent)
      aoo <- calculate_aoo(occ_recent)
      locs <- calculate_locations(occ_recent)

      # Trend (Full Context)
      # This calculates change between (Now-20 to Now-10) and (Now-10 to Now)
      trend <- calculate_trend(occ_all, window_years = recent_years)

      # 4. Summarize
      results_list[[sp]] <- summarize_assessment(sp, eoo, aoo, trend, locs)

    }, error = function(e) {
      message(paste("Error processing", sp, ":", e$message))
    })
  }

  final_table <- dplyr::bind_rows(results_list)
  return(final_table)
}

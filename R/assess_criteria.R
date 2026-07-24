#' Evaluate Criterion A (Population Reduction)
#'
#' @param trend_res Result from calculate_trend()
#' @return Suggested category (character)
#' @export
assess_criterion_a <- function(trend_res) {
  if (is.na(trend_res$percent_change)) return("DD") # Data Deficient

  # IUCN A2/A4 Thresholds (Decline is a negative change)
  # If percent_change is -80 (80% decline) -> CR
  decline <- -1 * trend_res$percent_change

  if (decline >= 80) return("CR")
  if (decline >= 50) return("EN")
  if (decline >= 30) return("VU")

  # Note: A positive trend (growth) or small decline is LC
  return("LC")
}

#' Evaluate Criterion D2 (Restricted AOO/Locations)
#'
#' @param aoo_km2 Numeric AOO
#' @param locations Numeric Location count
#' @return "VU" or "LC"
#' @export
assess_criterion_d2 <- function(aoo_km2, locations) {
  # IUCN D2 Thresholds: AOO < 20 km2 or Locations <= 5
  if (aoo_km2 < 20 || locations <= 5) {
    return("VU")
  } else {
    return("LC")
  }
}

#' Run Full IUCN Assessment for a Single Species (Harmonised Pipeline)
#'
#' @param sp Character. Species scientific name.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @export
assess_species <- function(sp, recent_start, recent_end, comp_start, comp_end) {

  # 1. Get & Clean Data
  raw_occ <- get_assessment_data(sp)
  occ_all <- clean_dates(raw_occ)

  # 2. Filter for Current Status (Criterion B)
  occ_recent <- occ_all %>%
    dplyr::filter(year >= recent_start & year <= recent_end)

  if (nrow(occ_recent) == 0) {
    # Original exact fallback structure preserved
    return(data.frame(
      Species = sp, EOO_km2 = 0, AOO_km2 = 0, Locations = 0,
      Trend_Perc = NA, Category = "EX?", Note = "No recent data",
      stringsAsFactors = FALSE
    ))
  }

  # 3. Compute Metrics
  # Spatial (Recent Only)
  eoo <- calculate_eoo(occ_recent)
  aoo <- calculate_aoo(occ_recent)
  locs <- calculate_locations(occ_recent, year_start = NULL, year_end = NULL)

  # Trend (Full Context)
  trend <- calculate_trend(
    occ_all,
    recent_start = recent_start, recent_end = recent_end,
    comp_start = comp_start, comp_end = comp_end
  )

  # 4. Summarise
  # Keeping the exact argument structure from the original batch_assess.R
  return(summarize_assessment(sp, eoo, aoo, trend, locs))
}

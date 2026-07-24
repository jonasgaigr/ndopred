#' Calculate Trend (Comparing Recent vs Comparison Window)
#'
#' @param occurrence_data Data frame (must contain DATUM_OD or 'year' col)
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @export
calculate_trend <- function(occurrence_data,
                            recent_start = 2010, recent_end = 2026,
                            comp_start = 2000, comp_end = 2016) {

  # Ensure dates are cleaned
  if (!"year" %in% names(occurrence_data)) {
    occurrence_data <- ndopred::clean_dates(occurrence_data)
  }

  # Filter data for both periods
  p1_data <- occurrence_data %>% dplyr::filter(year >= comp_start & year <= comp_end)
  p2_data <- occurrence_data %>% dplyr::filter(year >= recent_start & year <= recent_end)

  # Calculate AOO for both periods to derive trend
  # (Passing year_start = NULL because we pre-filtered the data above)
  aoo_p1 <- ndopred::calculate_aoo(p1_data, year_start = NULL)$area_km2
  aoo_p2 <- ndopred::calculate_aoo(p2_data, year_start = NULL)$area_km2

  # Calculate percentage change
  perc_change <- if(!is.na(aoo_p1) && aoo_p1 > 0) {
    ((aoo_p2 - aoo_p1) / aoo_p1) * 100
  } else {
    NA
  }

  return(list(
    range = paste(comp_start, "-", recent_end),
    period_1_range = paste(comp_start, "-", comp_end),
    period_2_range = paste(recent_start, "-", recent_end),
    period_1_aoo = aoo_p1,
    period_2_aoo = aoo_p2,
    percent_change = perc_change
  ))
}

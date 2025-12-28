#' Calculate Trend (Comparing Recent vs Previous Window)
#'
#' @param occurrence_data Data frame (must contain DATUM_OD or 'year' col)
#' @param window_years Numeric (default 10)
#' @export
calculate_trend <- function(occurrence_data, window_years = 10) {

  # Ensure dates are cleaned
  if (!"year" %in% names(occurrence_data)) {
    occurrence_data <- clean_dates(occurrence_data)
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Define Periods
  # P2 = Recent (e.g., 2015-2025)
  # P1 = Previous (e.g., 2005-2015)
  p2_start <- current_year - window_years
  p1_start <- p2_start - window_years

  p1_data <- occurrence_data %>% dplyr::filter(year >= p1_start & year < p2_start)
  p2_data <- occurrence_data %>% dplyr::filter(year >= p2_start)

  # Calculate AOO for both periods
  aoo_p1 <- calculate_aoo(p1_data)$area_km2
  aoo_p2 <- calculate_aoo(p2_data)$area_km2

  perc_change <- if(aoo_p1 > 0) {
    ((aoo_p2 - aoo_p1) / aoo_p1) * 100
  } else {
    NA
  }

  return(list(
    range = paste(p1_start, "-", current_year),
    period_1_range = paste(p1_start, "-", p2_start - 1),
    period_2_range = paste(p2_start, "-", current_year),
    period_1_aoo = aoo_p1,
    period_2_aoo = aoo_p2,
    percent_change = perc_change
  ))
}

#' Calculate AOO Trend (Criterion A proxy)
#'
#' @param occurrence_data An sf object.
#' @param window_years Numeric. The period to compare (default 10 years).
#' @export
calculate_trend <- function(occurrence_data, window_years = 10) {

  # 1. Handle YYYYMMDD format
  # We convert to character first, then parse as Year-Month-Day
  data_with_years <- occurrence_data %>%
    dplyr::mutate(
      date_clean = lubridate::ymd(as.character(DATUM_OD)),
      obs_year = lubridate::year(date_clean)
    ) %>%
    dplyr::filter(!is.na(obs_year))

  # 2. Get the actual time range for the message
  year_min <- min(data_with_years$obs_year, na.rm = TRUE)
  year_max <- max(data_with_years$obs_year, na.rm = TRUE)
  message(paste("Successfully parsed records from year", year_min, "to", year_max))

  # 3. Define Periods (Based on the last available year)
  p2_start <- year_max - window_years + 1
  p1_start <- p2_start - window_years

  p1_data <- data_with_years %>% dplyr::filter(obs_year >= p1_start & obs_year < p2_start)
  p2_data <- data_with_years %>% dplyr::filter(obs_year >= p2_start)

  # 4. Calculate AOO using our existing function
  aoo_p1 <- calculate_aoo(p1_data)$area_km2
  aoo_p2 <- calculate_aoo(p2_data)$area_km2

  perc_change <- if(aoo_p1 > 0) {
    ((aoo_p2 - aoo_p1) / aoo_p1) * 100
  } else {
    NA
  }

  return(list(
    range = paste(year_min, "-", year_max),
    period_1_aoo = aoo_p1,
    period_2_aoo = aoo_p2,
    percent_change = perc_change
  ))
}

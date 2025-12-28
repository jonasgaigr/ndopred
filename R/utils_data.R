#' Clean and Parse NDOP Dates
#'
#' @param df Data frame with DATUM_OD column
#' @return Data frame with added 'year' column
#' @export
clean_dates <- function(df) {
  df %>%
    dplyr::mutate(
      # Parse YYYYMMDD integers to Date objects
      date_obj = lubridate::ymd(as.character(DATUM_OD)),
      # Extract Year
      year = lubridate::year(date_obj)
    ) %>%
    # Remove rows where year couldn't be parsed
    dplyr::filter(!is.na(year))
}

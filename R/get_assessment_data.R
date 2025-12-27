#' Get and Clean NDOP Data for IUCN Assessment
#'
#' @param species_name Character. Scientific name of the species.
#' @param year_start Numeric. Optional filter for starting year.
#'
#' @return An sf object of occurrences.
#' @export
get_assessment_data <- function(species_name, year_start = NULL) {

  # 1. Download
  raw_data <- rndop::ndop_download(species_name)

  # 2. Handle X/Y coordinates (S-JTSK / EPSG:5514)
  # In NDOP, X and Y are usually S-JTSK
  cleaned_sf <- raw_data %>%
    dplyr::filter(!is.na(X), !is.na(Y)) %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
    # Transform to LAEA Europe (EPSG:3035) for accurate area calculations
    sf::st_transform(crs = 3035)

  # 3. Filter by date (using the DATUM_OD column found in your names)
  if (!is.null(year_start)) {
    cleaned_sf <- cleaned_sf %>%
      dplyr::filter(lubridate::year(as.Date(DATUM_OD)) >= year_start)
  }

  return(cleaned_sf)
}

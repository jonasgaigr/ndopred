#' Get and Clean NDOP Data
#' @export
get_assessment_data <- function(species_name, year_start = NULL) {

  # 1. Download
  raw_data <- tryCatch({
    rndop::ndop_download(species_name)
  }, error = function(e) return(NULL))

  # 2. Check if data actually exists before doing anything else
  if (is.null(raw_data) || !is.data.frame(raw_data) || nrow(raw_data) == 0) {
    message(paste("Zero records for:", species_name))
    # Return empty sf with correct CRS
    return(sf::st_as_sf(data.frame(X=numeric(0), Y=numeric(0), DATUM_OD=character(0)),
                        coords = c("X", "Y"), crs = 5514) %>%
             sf::st_transform(3035))
  }

  # 3. Clean and Transform
  cleaned_sf <- raw_data %>%
    dplyr::filter(!is.na(X), !is.na(Y)) %>%
    dplyr::filter(NEGATIV == 0) %>%
    dplyr::filter(KATASTR != "") %>%
    dplyr::filter(VEROH != 3 & VEROH != 9)

  # Handle case where filtering out negatives leaves 0 rows
  if (nrow(cleaned_sf) == 0) {
    return(sf::st_as_sf(data.frame(X=numeric(0), Y=numeric(0), DATUM_OD=character(0)),
                        coords = c("X", "Y"), crs = 5514) %>%
             sf::st_transform(3035))
  }

  cleaned_sf <- cleaned_sf %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
    sf::st_transform(crs = 3035)

  # 4. Date Filter
  if (!is.null(year_start) && nrow(cleaned_sf) > 0) {
    # Convert DATUM_OD safely
    cleaned_sf <- cleaned_sf %>%
      dplyr::mutate(y = as.numeric(substr(as.character(DATUM_OD), 1, 4))) %>%
      dplyr::filter(y >= year_start) %>%
      dplyr::select(-y)
  }

  return(cleaned_sf)
}

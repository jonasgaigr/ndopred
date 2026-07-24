#' Calculate EOO with Temporal Filter
#' @param occurrence_data sf object
#' @param year_start Numeric. Only records from this year onwards.
#' @param date_col Character. The name of the date column (default is "DATUM_OD").
#' @export
calculate_eoo <- function(occurrence_data, year_start = NULL, date_col = "DATUM_OD") {

  # 1. Temporal Filter
  if (!is.null(year_start) && nrow(occurrence_data) > 0) {

    # Ensure the date column exists to prevent obscure errors
    if (!date_col %in% names(occurrence_data)) {
      stop(paste("Column", date_col, "not found in the dataset."))
    }

    occurrence_data <- occurrence_data %>%
      dplyr::mutate(y = as.numeric(substr(as.character(!!rlang::sym(date_col)), 1, 4))) %>%
      dplyr::filter(y >= year_start)
  }

  # IUCN Edge Case: EOO cannot be 0 if species exists. It should default to AOO.
  # (Placeholder for AOO logic)
  if (nrow(occurrence_data) < 3) {
    warning("Fewer than 3 records found. EOO cannot be calculated via convex hull. Defaulting to AOO is recommended.")
    return(list(area_km2 = NA, geom = NULL))
  }

  # 2. Calculate Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  # 3. Calculate Area using the 'units' package to ensure safe conversion to km2
  area_units <- sf::st_area(hull)
  area_km2 <- as.numeric(units::set_units(area_units, "km^2"))

  return(list(area_km2 = area_km2, geom = hull))
}

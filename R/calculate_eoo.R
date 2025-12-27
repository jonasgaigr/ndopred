calculate_eoo <- function(occurrence_data) {
  if (nrow(occurrence_data) < 3) return(list(area_km2 = 0, geom = NULL))

  # 1. Calculate Hull
  hull <- sf::st_convex_hull(sf::st_union(occurrence_data))

  # Optional: Clip to Border (currently disabled)
  # cz_border <- giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
  #   sf::st_transform(3035)
  # hull <- sf::st_intersection(hull, cz_border)

  area <- as.numeric(sf::st_area(hull)) / 1e6
  return(list(area_km2 = area, geom = hull))
}

#' Plot IUCN Assessment Map
#'
#' @param species_name Character.
#' @param occ_data An existing sf object (optional). If NULL, it will download.
#' @param window Numeric.
#' @export
plot_iucn <- function(species_name, occ_data = NULL, window = 10) {

  # 1. Use provided data or download if missing
  occ_all <- if (!is.null(occ_data)) occ_data else get_assessment_data(species_name)

  if (nrow(occ_all) == 0) return(NULL)

  cutoff_year <- as.numeric(format(Sys.Date(), "%Y")) - window

  # 2. Add Status column
  occ_all <- occ_all %>%
    dplyr::mutate(
      # Safely handle year extraction
      year = as.numeric(substr(as.character(DATUM_OD), 1, 4)),
      Status = ifelse(year >= cutoff_year, "Recent", "Historical")
    )

  # 3. Calculate Metrics for the plot (Recent Only)
  eoo <- calculate_eoo(occ_all, year_start = cutoff_year)
  aoo <- calculate_aoo(occ_all, year_start = cutoff_year)

  # 4. Transform to S-JTSK for plotting
  cz_border <- giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
    sf::st_transform(5514)

  occ_5514 <- sf::st_transform(occ_all, 5514)
  eoo_5514 <- if(!is.null(eoo$geom)) sf::st_transform(eoo$geom, 5514) else NULL
  aoo_5514 <- if(!is.null(aoo$geom)) sf::st_transform(aoo$geom, 5514) else NULL

  # 5. ggplot
  ggplot() +
    geom_sf(data = cz_border, fill = "gray99", color = "gray85") +
    {if(!is.null(eoo_5514)) geom_sf(data = eoo_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")} +
    {if(!is.null(aoo_5514)) geom_sf(data = aoo_5514, fill = "blue", alpha = 0.3, color = NA)} +
    geom_sf(data = occ_5514, aes(color = Status), alpha = 0.7, size = 1.5) +
    scale_color_manual(values = c("Historical" = "gray70", "Recent" = "black")) +
    coord_sf(datum = 5514) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = species_name, subtitle = "Red: EOO | Blue: AOO")
}

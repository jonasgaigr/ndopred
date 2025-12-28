#' Plot IUCN Assessment Map in S-JTSK with Temporal Context
#'
#' @param species_name Character name of species.
#' @param window Numeric. The number of years for the "Recent" period (default 10).
#' @export
plot_iucn <- function(species_name, window = 10) {
  # 1. Get all data
  occ_all <- get_assessment_data(species_name)

  # Calculate cutoff
  cutoff_year <- as.numeric(format(Sys.Date(), "%Y")) - window

  # 2. Prepare Temporal Layers
  # We parse the year from DATUM_OD as we did in calculation functions
  occ_all <- occ_all %>%
    dplyr::mutate(
      year = as.numeric(substr(as.character(DATUM_OD), 1, 4)),
      Status = ifelse(year >= cutoff_year, "Recent", "Historical")
    )

  # 3. Calculate Metrics (STRICTLY Recent Only)
  eoo <- calculate_eoo(occ_all, year_start = cutoff_year)
  aoo <- calculate_aoo(occ_all, year_start = cutoff_year)

  # 4. Get Czech Border
  cz_border <- giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
    sf::st_transform(5514)

  # 5. Transform all to S-JTSK (5514) for plotting
  occ_5514 <- sf::st_transform(occ_all, 5514)
  eoo_5514 <- if(!is.null(eoo$geom)) sf::st_transform(eoo$geom, 5514) else NULL
  aoo_5514 <- if(!is.null(aoo$geom)) sf::st_transform(aoo$geom, 5514) else NULL

  # 6. Create Plot
  library(ggplot2)

  ggplot() +
    # Background Czech Border
    geom_sf(data = cz_border, fill = "gray99", color = "gray85") +

    # EOO Hull (Recent Only)
    {if(!is.null(eoo_5514)) geom_sf(data = eoo_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")} +

    # AOO Grids (Recent Only)
    {if(!is.null(aoo_5514)) geom_sf(data = aoo_5514, fill = "blue", alpha = 0.3, color = NA)} +

    # Points: Historical in Light Grey, Recent in Black
    geom_sf(data = occ_5514, aes(color = Status), alpha = 0.7, size = 1.5) +

    scale_color_manual(values = c("Historical" = "gray70", "Recent" = "black")) +

    coord_sf(datum = 5514) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      title = paste("IUCN Assessment:", species_name),
      subtitle = paste("Recent window:", cutoff_year, "-", format(Sys.Date(), "%Y"),
                       "| Red: Recent EOO | Blue: Recent AOO"),
      caption = "Base map: giscoR | Data: NDOP",
      color = "Record Status"
    )
}

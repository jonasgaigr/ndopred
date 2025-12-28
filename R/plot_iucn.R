#' Plot IUCN Assessment Map in S-JTSK
#'
#' @param species_name Character name of species.
#' @export
plot_iucn <- function(species_name) {
  # 1. Get data and metrics
  occ <- get_assessment_data(species_name)
  eoo <- calculate_eoo(occ)
  aoo <- calculate_aoo(occ)

  # 2. Get Czech Border from giscoR (resolution "03" is fine for background)
  cz_border <- giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
    sf::st_transform(5514)

  # 3. Transform assessment layers to S-JTSK (5514)
  occ_5514 <- sf::st_transform(occ, 5514)
  eoo_5514 <- if(!is.null(eoo$geom)) sf::st_transform(eoo$geom, 5514) else NULL
  aoo_5514 <- if(!is.null(aoo$geom)) sf::st_transform(aoo$geom, 5514) else NULL

  # 4. Create Plot
  library(ggplot2)

  ggplot() +
    # Background Border
    geom_sf(data = cz_border, fill = "gray98", color = "gray80") +
    # EOO Hull (Full, unclipped)
    {if(!is.null(eoo_5514)) geom_sf(data = eoo_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")} +
    # AOO Grids (Full, unclipped)
    {if(!is.null(aoo_5514)) geom_sf(data = aoo_5514, fill = "blue", alpha = 0.3, color = NA)} +
    # Points
    geom_sf(data = occ_5514, alpha = 0.6, size = 1, color = "black") +
    coord_sf(datum = 5514) +
    theme_minimal() +
    labs(
      title = paste("IUCN Assessment:", species_name),
      subtitle = "S-JTSK Projection | Red: EOO Hull | Blue: AOO Grids",
      caption = "Base map: giscoR | Data: NDOP"
    )
}

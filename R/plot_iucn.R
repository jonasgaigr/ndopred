#' @export
plot_iucn <- function(species_name) {
  occ <- get_assessment_data(species_name)
  eoo <- calculate_eoo(occ)
  aoo <- calculate_aoo(occ)

  cz_border <- RCzechia::republika() %>% sf::st_transform(5514)

  occ_5514 <- sf::st_transform(occ, 5514)
  # Plotting the full unclipped geometries
  eoo_5514 <- if(!is.null(eoo$geom)) sf::st_transform(eoo$geom, 5514) else NULL
  aoo_5514 <- if(!is.null(aoo$geom)) sf::st_transform(aoo$geom, 5514) else NULL

  library(ggplot2)
  ggplot() +
    geom_sf(data = cz_border, fill = "gray98", color = "gray80") +
    {if(!is.null(eoo_5514)) geom_sf(data = eoo_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")} +
    {if(!is.null(aoo_5514)) geom_sf(data = aoo_5514, fill = "blue", alpha = 0.3, color = NA)} +
    geom_sf(data = occ_5514, alpha = 0.6, size = 1, color = "black") +
    coord_sf(datum = 5514) +
    theme_minimal() +
    labs(
      title = paste("IUCN Metrics (Unclipped):", species_name),
      subtitle = "Red = EOO Hull | Blue = AOO Grids | BG = Czech Republic"
    )
}

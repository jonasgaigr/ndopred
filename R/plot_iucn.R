#' Plot IUCN Assessment Map
#'
#' @param species_name Character.
#' @param occ_data An existing sf object (optional). If NULL, it will download.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @export
plot_iucn <- function(species_name, occ_data = NULL,
                      recent_start = 2010, recent_end = 2026,
                      comp_start = 2000, comp_end = 2016) {

  occ_all <- if (!is.null(occ_data)) occ_data else ndopred::get_assessment_data(species_name)
  if (is.null(occ_all) || nrow(occ_all) == 0) return(NULL)

  # CRITICAL FIX 1: Inherit the patched ROK column from app_bulk if available.
  # Fallback to DATUM_OD only if running independently.
  if ("ROK" %in% names(occ_all)) {
    occ_all$year <- occ_all$ROK
  } else if ("DATUM_OD" %in% names(occ_all)) {
    occ_all$year <- suppressWarnings(as.numeric(substr(as.character(occ_all$DATUM_OD), 1, 4)))
  } else {
    occ_all$year <- NA
  }

  occ_all <- occ_all %>%
    dplyr::mutate(
      Status = dplyr::case_when(
        year >= recent_start & year <= recent_end ~ "Recent",
        year >= comp_start & year <= comp_end ~ "Comparison",
        TRUE ~ "Historical"
      ),
      Status = factor(Status, levels = c("Recent", "Comparison", "Historical"))
    )

  occ_recent <- occ_all %>% dplyr::filter(Status == "Recent")
  occ_comp <- occ_all %>% dplyr::filter(Status == "Comparison")

  eoo_recent <- if(nrow(occ_recent) >= 3) ndopred::calculate_eoo(occ_recent, year_start = NULL) else list(geom = NULL)
  aoo_recent <- if(nrow(occ_recent) > 0) ndopred::calculate_aoo(occ_recent, year_start = NULL) else list(geom = NULL)

  eoo_comp <- if(nrow(occ_comp) >= 3) ndopred::calculate_eoo(occ_comp, year_start = NULL) else list(geom = NULL)
  aoo_comp <- if(nrow(occ_comp) > 0) ndopred::calculate_aoo(occ_comp, year_start = NULL) else list(geom = NULL)

  # CRITICAL FIX 2: Check for valid sf class to prevent st_transform crashes
  if (!inherits(occ_all, "sf") || is.na(sf::st_crs(occ_all))) {
    warning(paste("occ_data for", species_name, "lacks sf geometry. Map skipped."))
    return(NULL)
  }

  occ_5514 <- sf::st_transform(occ_all, 5514)

  # CRITICAL FIX 3: Safe API Call. If the web request fails, it returns NULL instead of crashing the loop.
  cz_border <- tryCatch({
    giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>% sf::st_transform(5514)
  }, error = function(e) NULL)

  eoo_recent_5514 <- if(!is.null(eoo_recent$geom)) sf::st_transform(eoo_recent$geom, 5514) else NULL
  aoo_recent_5514 <- if(!is.null(aoo_recent$geom)) sf::st_transform(aoo_recent$geom, 5514) else NULL
  eoo_comp_5514 <- if(!is.null(eoo_comp$geom)) sf::st_transform(eoo_comp$geom, 5514) else NULL
  aoo_comp_5514 <- if(!is.null(aoo_comp$geom)) sf::st_transform(aoo_comp$geom, 5514) else NULL

  p <- ggplot2::ggplot()
  if (!is.null(cz_border)) {
    p <- p + ggplot2::geom_sf(data = cz_border, fill = "gray99", color = "gray85")
  }

  if(!is.null(eoo_comp_5514)) p <- p + ggplot2::geom_sf(data = eoo_comp_5514, fill = "blue", alpha = 0.05, color = "blue", linetype = "dashed")
  if(!is.null(aoo_comp_5514)) p <- p + ggplot2::geom_sf(data = aoo_comp_5514, fill = "blue", alpha = 0.3, color = NA)
  if(!is.null(eoo_recent_5514)) p <- p + ggplot2::geom_sf(data = eoo_recent_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")
  if(!is.null(aoo_recent_5514)) p <- p + ggplot2::geom_sf(data = aoo_recent_5514, fill = "red", alpha = 0.3, color = NA)

  p <- p +
    ggplot2::geom_sf(data = occ_5514, ggplot2::aes(color = Status), alpha = 0.7, size = 1.5) +
    ggplot2::scale_color_manual(values = c("Recent" = "red", "Comparison" = "blue", "Historical" = "gray70")) +
    ggplot2::coord_sf(datum = sf::st_crs(5514)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = species_name,
      subtitle = sprintf("Recent (%s-%s) | Comp (%s-%s) | Pre-%s",
                         recent_start, recent_end, comp_start, comp_end, comp_start)
    )

  return(p)
}

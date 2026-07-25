#' Plot IUCN Assessment Map
#'
#' @param species_name Character.
#' @param occ_data An existing sf object (optional). If NULL, it will download.
#' @param recent_start Numeric. Start year for recent data (default 2010).
#' @param recent_end Numeric. End year for recent data (default 2026).
#' @param comp_start Numeric. Start year for comparison data (default 2000).
#' @param comp_end Numeric. End year for comparison data (default 2016).
#' @export
plot_iucn <- function(species_name, occ_data = NULL,
                      recent_start = 2010, recent_end = 2026,
                      comp_start = 2000, comp_end = 2016) {

  # 1. Use provided data or download if missing
  occ_all <- if (!is.null(occ_data)) occ_data else ndopred::get_assessment_data(species_name)

  if (is.null(occ_all) || nrow(occ_all) == 0) return(NULL)

  # 2. Add Status column based on precise windows (SAFE DATE PARSING)
  occ_all <- occ_all %>%
    dplyr::mutate(
      year = if ("ROK" %in% names(.)) {
        as.numeric(ROK)
      } else if ("DATUM_OD" %in% names(.)) {
        suppressWarnings(as.numeric(substr(as.character(DATUM_OD), 1, 4)))
      } else {
        NA_real_
      },
      Status = dplyr::case_when(
        !is.na(year) & year >= recent_start & year <= recent_end ~ "Recent (New)",
        !is.na(year) & year >= comp_start & year <= comp_end ~ "Comparison (2000-2016)",
        TRUE ~ "Historical (<2000)"
      ),
      # Set explicit factor levels to control plotting order (Historical bottom, Recent top)
      Status = factor(Status, levels = c("Historical (<2000)", "Comparison (2000-2016)", "Recent (New)"))
    ) %>%
    # Ensure points are ordered so Recent is plotted last (on top)
    dplyr::arrange(Status)

  # 3. Filter datasets
  occ_recent <- occ_all %>% dplyr::filter(Status == "Recent (New)")
  occ_comp <- occ_all %>% dplyr::filter(Status == "Comparison (2000-2016)")

  # 4. Calculate Metrics safely
  safe_calc <- function(func, data) {
    if (nrow(data) == 0) return(list(geom = NULL))
    res <- tryCatch(func(data, year_start = NULL), error = function(e) list(geom = NULL))
    if (!is.list(res) || !("geom" %in% names(res))) return(list(geom = NULL))
    return(res)
  }

  eoo_recent <- if(nrow(occ_recent) >= 3) safe_calc(ndopred::calculate_eoo, occ_recent) else list(geom = NULL)
  aoo_recent <- if(nrow(occ_recent) > 0) safe_calc(ndopred::calculate_aoo, occ_recent) else list(geom = NULL)

  eoo_comp <- if(nrow(occ_comp) >= 3) safe_calc(ndopred::calculate_eoo, occ_comp) else list(geom = NULL)
  aoo_comp <- if(nrow(occ_comp) > 0) safe_calc(ndopred::calculate_aoo, occ_comp) else list(geom = NULL)

  # 5. Transform to S-JTSK for plotting (EPSG:5514)
  cz_border <- tryCatch({
    giscoR::gisco_get_countries(country = "Czech Republic", resolution = "03") %>%
      sf::st_transform(5514)
  }, error = function(e) NULL)

  occ_5514 <- tryCatch(sf::st_transform(occ_all, 5514), error = function(e) NULL)

  if (is.null(occ_5514)) {
    warning("Geometry missing or invalid. Cannot plot.")
    return(NULL)
  }

  eoo_recent_5514 <- if(!is.null(eoo_recent$geom)) sf::st_transform(eoo_recent$geom, 5514) else NULL
  aoo_recent_5514 <- if(!is.null(aoo_recent$geom)) sf::st_transform(aoo_recent$geom, 5514) else NULL

  eoo_comp_5514 <- if(!is.null(eoo_comp$geom)) sf::st_transform(eoo_comp$geom, 5514) else NULL
  aoo_comp_5514 <- if(!is.null(aoo_comp$geom)) sf::st_transform(aoo_comp$geom, 5514) else NULL

  # 6. ggplot construction with custom shapes & colors matching requirements
  p <- ggplot2::ggplot()

  if (!is.null(cz_border)) {
    p <- p + ggplot2::geom_sf(data = cz_border, fill = "gray99", color = "gray85")
  }

  # Comparison EOO/AOO Layers (Blue or neutral styling as per your app design)
  if(!is.null(eoo_comp_5514)) p <- p + ggplot2::geom_sf(data = eoo_comp_5514, fill = "blue", alpha = 0.05, color = "blue", linetype = "dashed")
  if(!is.null(aoo_comp_5514)) p <- p + ggplot2::geom_sf(data = aoo_comp_5514, fill = "blue", alpha = 0.3, color = NA)

  # Recent EOO/AOO Layers (Red)
  if(!is.null(eoo_recent_5514)) p <- p + ggplot2::geom_sf(data = eoo_recent_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")
  if(!is.null(aoo_recent_5514)) p <- p + ggplot2::geom_sf(data = aoo_recent_5514, fill = "red", alpha = 0.3, color = NA)

  # Data Points mapping shapes (21 = hollow circle for historical, 19 = solid dot for comparison and recent)
  p <- p +
    ggplot2::geom_sf(
      data = occ_5514,
      ggplot2::aes(color = Status, fill = Status, shape = Status),
      alpha = 0.8,
      size = 1.8,
      stroke = 0.8
    ) +
    ggplot2::scale_shape_manual(values = c(
      "Historical (<2000)" = 21,         # Hollow circle
      "Comparison (2000-2016)" = 19,     # Solid circle
      "Recent (New)" = 19                # Solid circle
    )) +
    ggplot2::scale_color_manual(values = c(
      "Historical (<2000)" = "gray50",
      "Comparison (2000-2016)" = "gray40",
      "Recent (New)" = "black"           # Top layer: Black
    )) +
    ggplot2::scale_fill_manual(values = c(
      "Historical (<2000)" = NA,         # Empty (transparent fill inside hollow circle)
      "Comparison (2000-2016)" = "gray40",
      "Recent (New)" = "black"
    )) +
    ggplot2::coord_sf(datum = sf::st_crs(5514)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = species_name,
      subtitle = sprintf("Recent (%s-%s): Black | Comp (%s-%s): Grey | Hist (<%s): Empty Circles",
                         recent_start, recent_end, comp_start, comp_end, comp_start)
    )

  return(p)
}

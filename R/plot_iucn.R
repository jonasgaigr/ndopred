#' Plot IUCN Assessment Map
#'
#' Draws the recent and comparison occurrence windows with their EOO hulls and
#' AOO grids over the Czech border.
#'
#' @param species_name Character.
#' @param occ_data An existing sf object (optional). If NULL, it will download.
#' @param window Numeric. Length in years of each window. When supplied (and
#'   the explicit bounds below are not), the recent window is the last
#'   `window` years up to `year_now` and the comparison window is the `window`
#'   years immediately before it - matching [calculate_trend()].
#' @param year_now Numeric. Final year of the recent window.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @param grid_size Numeric. AOO cell size in metres (default 2000).
#' @return A `ggplot` object, or `NULL` if there is nothing to draw.
#' @details
#' Recent records are drawn as filled black points with a red EOO hull and AOO
#' grid; comparison records as grey points with a blue hull and grid;
#' historical records as open circles. Both periods' AOO grids share one grid
#' origin so the cells are directly comparable. The Czech border comes from
#' \pkg{giscoR}; if that call fails the map is drawn without it.
#' @seealso [calculate_eoo()], [calculate_aoo()], [calculate_trend()]
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     x = c(-740000, -700000, -650000, -600000, -730000, -690000),
#'     y = c(-1040000, -1080000, -1100000, -1150000, -1050000, -1090000),
#'     DATUM_OD = c("20200615", "20210615", "20220615",
#'                  "20100615", "20110615", "19950615")
#'   ),
#'   coords = c("x", "y"), crs = 5514
#' )
#'
#' \donttest{
#' # Windows derived from `window` and `year_now`, matching the assessment.
#' p <- plot_iucn("Example species", occ_data = pts,
#'                window = 10, year_now = 2025)
#' }
#' @export
plot_iucn <- function(species_name, occ_data = NULL,
                      window = 10,
                      year_now = as.numeric(format(Sys.Date(), "%Y")),
                      recent_start = NULL, recent_end = NULL,
                      comp_start = NULL, comp_end = NULL,
                      grid_size = 2000) {

  # Both Shiny apps called this as plot_iucn(..., window = <n>), which was not
  # an argument of the function: every map generation failed with "unused
  # argument (window = ...)" and was swallowed by the surrounding tryCatch.
  # `window` is now the primary interface and drives the same windows the
  # assessment uses, so the map and the numbers can no longer disagree.
  if (is.null(recent_end))   recent_end   <- year_now
  if (is.null(recent_start)) recent_start <- recent_end - window + 1
  if (is.null(comp_end))     comp_end     <- recent_start - 1
  if (is.null(comp_start))   comp_start   <- comp_end - window + 1

  lbl_recent <- paste0("Recent (", recent_start, "-", recent_end, ")")
  lbl_comp   <- paste0("Comparison (", comp_start, "-", comp_end, ")")
  lbl_hist   <- paste0("Historical (<", comp_start, ")")

  # 1. Use provided data or download if missing
  occ_all <- if (!is.null(occ_data)) occ_data else get_assessment_data(species_name)

  if (is.null(occ_all) || nrow(occ_all) == 0) return(NULL)

  # 2. Add Status column based on the assessment windows
  occ_all <- add_year_column(occ_all)
  occ_all <- occ_all %>%
    dplyr::mutate(
      Status = dplyr::case_when(
        !is.na(.data$year) & .data$year >= recent_start & .data$year <= recent_end ~ lbl_recent,
        !is.na(.data$year) & .data$year >= comp_start & .data$year <= comp_end ~ lbl_comp,
        TRUE ~ lbl_hist
      ),
      # Explicit factor levels control plotting order (historical bottom,
      # recent top)
      Status = factor(.data$Status, levels = c(lbl_hist, lbl_comp, lbl_recent))
    ) %>%
    dplyr::arrange(.data$Status)

  # 3. Filter datasets
  occ_recent <- occ_all %>% dplyr::filter(.data$Status == lbl_recent)
  occ_comp <- occ_all %>% dplyr::filter(.data$Status == lbl_comp)

  # 4. Calculate Metrics safely, on a grid origin shared between the two
  # periods so the drawn AOO cells are directly comparable.
  shared_origin <- NULL
  pooled <- suppressWarnings(rbind(occ_recent, occ_comp))
  if (nrow(pooled) > 0) {
    shared_origin <- tryCatch(
      suppressWarnings(calculate_aoo(pooled, grid_size = grid_size)$origin),
      error = function(e) NULL
    )
  }

  safe_hull <- function(data) {
    if (nrow(data) < 3) return(list(geom = NULL))
    tryCatch(calculate_eoo(data), error = function(e) list(geom = NULL))
  }
  safe_grid <- function(data) {
    if (nrow(data) == 0) return(list(geom = NULL))
    tryCatch(
      suppressWarnings(calculate_aoo(data, grid_size = grid_size, origin = shared_origin)),
      error = function(e) list(geom = NULL)
    )
  }

  eoo_recent <- safe_hull(occ_recent)
  aoo_recent <- safe_grid(occ_recent)
  eoo_comp <- safe_hull(occ_comp)
  aoo_comp <- safe_grid(occ_comp)

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

  to_5514 <- function(g) if (!is.null(g)) sf::st_transform(g, 5514) else NULL
  eoo_recent_5514 <- to_5514(eoo_recent$geom)
  aoo_recent_5514 <- to_5514(aoo_recent$geom)
  eoo_comp_5514 <- to_5514(eoo_comp$geom)
  aoo_comp_5514 <- to_5514(aoo_comp$geom)

  # 6. ggplot construction
  p <- ggplot2::ggplot()

  if (!is.null(cz_border)) {
    p <- p + ggplot2::geom_sf(data = cz_border, fill = "gray99", color = "gray85")
  }

  # Comparison EOO/AOO layers
  if (!is.null(eoo_comp_5514)) p <- p + ggplot2::geom_sf(data = eoo_comp_5514, fill = "blue", alpha = 0.05, color = "blue", linetype = "dashed")
  if (!is.null(aoo_comp_5514)) p <- p + ggplot2::geom_sf(data = aoo_comp_5514, fill = "blue", alpha = 0.3, color = NA)

  # Recent EOO/AOO layers
  if (!is.null(eoo_recent_5514)) p <- p + ggplot2::geom_sf(data = eoo_recent_5514, fill = "red", alpha = 0.05, color = "red", linetype = "dashed")
  if (!is.null(aoo_recent_5514)) p <- p + ggplot2::geom_sf(data = aoo_recent_5514, fill = "red", alpha = 0.3, color = NA)

  shape_vals <- stats::setNames(c(21, 19, 19), c(lbl_hist, lbl_comp, lbl_recent))
  color_vals <- stats::setNames(c("gray50", "gray40", "black"), c(lbl_hist, lbl_comp, lbl_recent))
  fill_vals  <- stats::setNames(c(NA, "gray40", "black"), c(lbl_hist, lbl_comp, lbl_recent))

  p <- p +
    ggplot2::geom_sf(
      data = occ_5514,
      ggplot2::aes(color = .data$Status, fill = .data$Status, shape = .data$Status),
      alpha = 0.8,
      size = 1.8,
      stroke = 0.8
    ) +
    ggplot2::scale_shape_manual(values = shape_vals, drop = FALSE) +
    ggplot2::scale_color_manual(values = color_vals, drop = FALSE) +
    ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE, na.value = NA) +
    ggplot2::coord_sf(datum = sf::st_crs(5514)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = species_name,
      # Wrapped onto two lines: a single line overflowed the plot width at the
      # 8x6in size both apps save at, and the last window was clipped.
      subtitle = sprintf(
        "%s: black points, red AOO grid + EOO hull\n%s: grey points, blue AOO grid + EOO hull | %s: open circles",
        lbl_recent, lbl_comp, lbl_hist
      ),
      color = NULL, fill = NULL, shape = NULL
    )

  return(p)
}

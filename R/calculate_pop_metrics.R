#' Calculate Population Metrics for IUCN Criterion C/D
#' Handles missing ROK column and maps NAZ_LOKAL to LOKALITA.
#'
#' @param occ_data Dataframe of occurrences
#' @param window_years Number of recent years to consider
#' @return List with total_mature, decline_rate, and max_subpop
calculate_pop_metrics <- function(occ_data, window_years = 10) {

  # 1. Standardize Data Structure (Handle missing columns)
  df <- if(inherits(occ_data, "sf")) sf::st_drop_geometry(occ_data) else occ_data

  # Ensure 'ROK' exists
  if (!"ROK" %in% names(df)) {
    if ("DATUM_OD" %in% names(df)) {
      df$ROK <- as.numeric(format(as.Date(df$DATUM_OD), "%Y"))
    } else {
      # Fallback or return empty if year is impossible to find
      warning("Could not find year column (ROK or DATUM_OD).")
      return(list(total_mature = NA, decline_rate = NA, max_subpop = NA))
    }
  }

  # Ensure 'LOKALITA' exists (Rename NAZ_LOKAL or ID_LOKAL)
  if (!"LOKALITA" %in% names(df)) {
    if ("NAZ_LOKAL" %in% names(df)) {
      df <- dplyr::rename(df, LOKALITA = NAZ_LOKAL)
    } else if ("ID_LOKAL" %in% names(df)) {
      df <- dplyr::rename(df, LOKALITA = ID_LOKAL)
    } else {
      warning("Could not find locality column (NAZ_LOKAL).")
      return(list(total_mature = NA, decline_rate = NA, max_subpop = NA))
    }
  }

  mature_terms <- get_mature_terms()
  cutoff_year <- as.numeric(format(Sys.Date(), "%Y")) - window_years

  # 2. Filter for Mature Individuals in Recent Window
  clean_pop <- df %>%
    dplyr::filter(
      ROK >= cutoff_year,
      POCITANO %in% mature_terms,
      !is.na(POCET) & POCET > 0
    ) %>%
    # Handle counting pairs (mating) as 2 individuals
    dplyr::mutate(final_count = dplyr::case_when(
      POCITANO %in% c("amplexus", "kopulace") ~ as.numeric(POCET) * 2,
      TRUE ~ as.numeric(POCET)
    ))

  if (nrow(clean_pop) == 0) {
    return(list(total_mature = NA, decline_rate = NA, max_subpop = NA))
  }

  # 3. Aggregate by LOKALITA per Year
  # Use MAX count in a year to represent that locality's presence for that year
  loc_year_summary <- clean_pop %>%
    dplyr::group_by(LOKALITA, ROK) %>%
    dplyr::summarise(max_local_count = max(final_count, na.rm = TRUE), .groups = "drop")

  # 4. Metric: Total Mature Individuals (Sum of best counts per locality)
  loc_maxima <- loc_year_summary %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::summarise(best_count = max(max_local_count, na.rm = TRUE), .groups = "drop")

  total_mature <- sum(loc_maxima$best_count, na.rm = TRUE)

  # 5. Metric: Max Subpopulation (Largest single locality count)
  max_subpop <- max(loc_maxima$best_count, na.rm = TRUE)

  # 6. Metric: Decline Rate (Linear trend of yearly totals)
  yearly_totals <- loc_year_summary %>%
    dplyr::group_by(ROK) %>%
    dplyr::summarise(total_year = sum(max_local_count, na.rm = TRUE), .groups = "drop")

  decline_rate <- NA
  if (nrow(yearly_totals) >= 3) {
    model <- tryCatch(lm(total_year ~ ROK, data = yearly_totals), error = function(e) NULL)
    if (!is.null(model)) {
      pred_start <- predict(model, newdata = data.frame(ROK = min(yearly_totals$ROK)))
      pred_end <- predict(model, newdata = data.frame(ROK = max(yearly_totals$ROK)))

      if (pred_start > 0) {
        change <- ((pred_end - pred_start) / pred_start) * 100
        decline_rate <- round(change, 1)
      }
    }
  }

  return(list(
    total_mature = total_mature,
    decline_rate = decline_rate,
    max_subpop = max_subpop
  ))
}

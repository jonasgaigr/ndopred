#' Calculate Population Metrics for IUCN Criterion C/D
#' Handles missing ROK column, maps NAZ_LOKAL, and correctly sums sexes.
#'
#' @param occ_data Dataframe of occurrences
#' @param window_years Number of recent years to consider
#' @return List with total_mature, decline_rate, and max_subpop
calculate_pop_metrics <- function(occ_data, window_years = 10) {

  # 1. Standardize Data Structure
  df <- if(inherits(occ_data, "sf")) sf::st_drop_geometry(occ_data) else occ_data

  # Ensure 'ROK' exists
  if (!"ROK" %in% names(df)) {
    if ("DATUM_OD" %in% names(df)) {
      df$ROK <- as.numeric(format(as.Date(df$DATUM_OD), "%Y"))
    } else {
      warning("Could not find year column (ROK or DATUM_OD).")
      return(list(total_mature = NA, decline_rate = NA, max_subpop = NA))
    }
  }

  # Ensure 'LOKALITA' exists
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

  # Define Sex-specific categories
  male_terms <- c("samci", "vychovaní samci")
  female_terms <- c("samice")

  cutoff_year <- as.numeric(format(Sys.Date(), "%Y")) - window_years

  # 2. Filter and Pre-process counts
  clean_pop <- df %>%
    dplyr::filter(
      ROK >= cutoff_year,
      POCITANO %in% mature_terms,
      !is.na(POCET) & POCET > 0
    ) %>%
    dplyr::mutate(
      # Multiply pairs by 2
      final_count = dplyr::case_when(
        POCITANO %in% c("amplexus", "kopulace") ~ as.numeric(POCET) * 2,
        TRUE ~ as.numeric(POCET)
      ),
      # Categorize record type
      type_cat = dplyr::case_when(
        POCITANO %in% male_terms ~ "M",
        POCITANO %in% female_terms ~ "F",
        TRUE ~ "General"
      )
    )

  if (nrow(clean_pop) == 0) {
    return(list(total_mature = NA, decline_rate = NA, max_subpop = NA))
  }

  # 3. Aggregate by LOKALITA per Year
  # Logic: For a given Year/Locality, we take the max Males and max Females seen
  # and add them. We compare this sum to the max "General" (adults/imaga) seen.
  # We take the higher of the two numbers.

  loc_year_summary <- clean_pop %>%
    dplyr::group_by(LOKALITA, ROK) %>%
    dplyr::summarise(
      max_m = max(final_count[type_cat == "M"], 0, na.rm = TRUE),
      max_f = max(final_count[type_cat == "F"], 0, na.rm = TRUE),
      max_gen = max(final_count[type_cat == "General"], 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate best estimate for that year/locality
    dplyr::mutate(
      sum_sexes = max_m + max_f,
      max_local_count = pmax(sum_sexes, max_gen)
    )

  # 4. Metric: Total Mature Individuals (Sum of best counts per locality)
  # We assume different localities are distinct subpopulations
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

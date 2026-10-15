#' Calculate Population Metrics for IUCN Criteria C and D
#'
#' Derives a minimum observed count of mature individuals, a decline rate, the
#' largest single-site count, and an extreme-fluctuation diagnosis from NDOP
#' abundance fields.
#'
#' @section What these numbers are (and are not):
#' NDOP records are opportunistic observations, not censuses. The totals
#' returned here are therefore **minimum observed counts of mature
#' individuals**, not estimates of population size in the sense of IUCN
#' Guidelines section 4.3, and `max_subpop` uses *locality* as a surrogate for
#' *subpopulation* (section 4.2). Both will usually be severe underestimates,
#' and both are sensitive to recording effort. They are intended to seed
#' expert review in the single-species app, and must be confirmed or replaced
#' by the assessor before criteria C or D1 are applied.
#'
#' @section Life-stage vocabulary coverage:
#' Only records whose `POCITANO` (what was counted) names a reproductive stage
#' are used. On live NDOP data that is often a minority of the counted
#' records - the unqualified term `"jedinci"` ("individuals") is the single
#' commonest value and is deliberately *not* in [get_mature_terms()], because
#' it does not assert that the individuals were mature (Guidelines 4.3). The
#' returned `ignored_terms` and `n_records_counted`/`n_records_used` make that
#' shortfall visible so it cannot silently shrink `total_mature`; pass
#' `mature_terms` to override the vocabulary for a given taxon.
#'
#' @param occ_data Dataframe or sf object of occurrences.
#' @param window_years Number of recent years to consider.
#' @param year_now Numeric. Final year of the window: the reference year
#'   (required; the system clock is never read).
#' @param mature_terms Character vector of `POCITANO` values to treat as
#'   mature individuals. Defaults to [get_mature_terms()].
#' @param consistent_effort Logical. Whether the counts come from a
#'   consistent-effort series (standardized monitoring, data tier T3). Only
#'   then are extreme fluctuations diagnosed (Guidelines 4.7: be reasonably
#'   certain the change is in the population, not in recording) and is the
#'   decline rate an *estimated* continuing decline usable for C1
#'   (Guidelines Table 3.1). Otherwise `extreme_fluctuation` is `NA` and
#'   `decline_estimated` is `FALSE`.
#' @section The census:
#' `total_mature` is the sum over localities of each locality's most recent
#' count in the window, not a sum of maxima from different years. Its range:
#' `total_mature_min` counts only the latest year with counts,
#' `total_mature_max` sums each locality's maximum in the window (Guidelines
#' 3.2, 4.3). `max_subpop` is the largest most-recent locality count.
#' @return A list with `total_mature` (and `total_mature_min`,
#'   `total_mature_max`), `decline_rate`, `decline_estimated`, `max_subpop`,
#'   `fluct_ratio`, `fluct_reversals`, `extreme_fluctuation`,
#'   `decline_period_years`, `n_years`, `n_localities`, `n_records_counted`,
#'   `n_records_used` and `ignored_terms`. Always check `n_records_used`
#'   against `n_records_counted` before trusting `total_mature`.
#' @seealso [get_mature_terms()] for the vocabulary, [summarize_assessment()]
#'   whose `evaluate_pop` argument decides whether criteria C and D1 are
#'   applied at all.
#' @references IUCN Standards and Petitions Committee (2024), sections 4.3
#'   (mature individuals), 4.5.1 (exponential decline), 4.6 and 4.7
#'   (continuing decline vs. extreme fluctuation).
#' @examples
#' # A locality censused for ten years, declining ~25% a year.
#' yrs <- 2016:2025
#' occ <- data.frame(
#'   ROK = yrs,
#'   POCET = round(400 * 0.75^(seq_along(yrs) - 1)),
#'   POCITANO = "adulti",
#'   NAZ_LOKAL = "Site A"
#' )
#' pm <- calculate_pop_metrics(occ, window_years = 10, year_now = 2025)
#' pm$total_mature
#' pm$decline_rate          # ~ -94%, projected over the 10-year window
#' pm$extreme_fluctuation   # NA: opportunistic counts diagnose no fluctuation
#'
#' # A boom-bust series of the same magnitude, from a consistent-effort
#' # series, IS an extreme fluctuation.
#' boom <- data.frame(
#'   ROK = yrs, POCITANO = "adulti", NAZ_LOKAL = "Site A",
#'   POCET = c(1000, 50, 900, 40, 1100, 60, 950, 45, 1050, 55)
#' )
#' calculate_pop_metrics(boom, year_now = 2025, consistent_effort = TRUE)$extreme_fluctuation
#'
#' # Unqualified life stages are skipped, and the shortfall is reported.
#' mixed <- data.frame(
#'   ROK = 2025, POCET = 20, POCITANO = c("adulti", "jedinci", "larvy"),
#'   NAZ_LOKAL = "Site A"
#' )
#' res <- calculate_pop_metrics(mixed, year_now = 2025)
#' res$n_records_used       # 1 of 3
#' res$ignored_terms
#' @export
calculate_pop_metrics <- function(occ_data, window_years = 10,
                                  year_now,
                                  mature_terms = get_mature_terms(),
                                  consistent_effort = FALSE) {
  if (missing(year_now) || is.null(year_now) || is.na(year_now)) {
    stop("`year_now` (the reference year) is required.", call. = FALSE)
  }

  empty_result <- function(reason = NA_character_) {
    list(total_mature = NA_real_, total_mature_min = NA_real_, total_mature_max = NA_real_,
         decline_rate = NA_real_, decline_estimated = FALSE, max_subpop = NA_real_,
         fluct_ratio = NA_real_, fluct_reversals = NA_integer_,
         extreme_fluctuation = NA, decline_period_years = window_years,
         n_years = 0L, n_localities = 0L, n_records_counted = 0L,
         n_records_used = 0L, ignored_terms = integer(0), note = reason)
  }

  # 1. Standardize Data Structure
  df <- if (inherits(occ_data, "sf")) sf::st_drop_geometry(occ_data) else occ_data
  if (is.null(df) || nrow(df) == 0) return(empty_result("no records"))

  # Ensure 'ROK' exists
  if (!"ROK" %in% names(df)) {
    if ("DATUM_OD" %in% names(df)) {
      df$ROK <- extract_year(df$DATUM_OD)
    } else {
      warning("Could not find year column (ROK or DATUM_OD).", call. = FALSE)
      return(empty_result("no year column"))
    }
  }
  df$ROK <- suppressWarnings(as.numeric(as.character(df$ROK)))

  # Abundance fields are optional in some NDOP exports. Missing them is a
  # normal outcome (criteria C/D simply cannot be automated), not an error --
  # the previous version let dplyr::filter() fail on an absent POCITANO
  # column, which took the whole single-species app down with it.
  if (!all(c("POCITANO", "POCET") %in% names(df))) {
    return(empty_result("no abundance columns (POCITANO / POCET)"))
  }

  # Ensure 'LOKALITA' exists
  if (!"LOKALITA" %in% names(df)) {
    if ("NAZ_LOKAL" %in% names(df)) {
      df <- dplyr::rename(df, LOKALITA = "NAZ_LOKAL")
    } else if ("ID_LOKAL" %in% names(df)) {
      df <- dplyr::rename(df, LOKALITA = "ID_LOKAL")
    } else {
      warning("Could not find locality column (NAZ_LOKAL).", call. = FALSE)
      return(empty_result("no locality column"))
    }
  }

  male_terms <- c("samci", "vychovan\u00ed samci")
  female_terms <- c("samice")
  cutoff_year <- year_now - window_years + 1

  df$.pocet_num <- suppressWarnings(as.numeric(as.character(df$POCET)))

  # Audit how much of the countable data the vocabulary actually reaches, so
  # a partial match cannot quietly become a population estimate.
  in_window <- !is.na(df$ROK) & df$ROK >= cutoff_year & df$ROK <= year_now &
    !is.na(df$.pocet_num) & df$.pocet_num > 0
  counted_terms <- as.character(df$POCITANO[in_window])
  counted_terms[is.na(counted_terms) | !nzchar(trimws(counted_terms))] <- "(blank)"
  n_records_counted <- length(counted_terms)
  ignored_tab <- table(counted_terms[!counted_terms %in% mature_terms])
  ignored_terms <- sort(stats::setNames(as.integer(ignored_tab), names(ignored_tab)),
                        decreasing = TRUE)

  # 2. Filter and Pre-process counts
  clean_pop <- df %>%
    dplyr::filter(
      !is.na(.data$ROK), .data$ROK >= cutoff_year, .data$ROK <= year_now,
      .data$POCITANO %in% mature_terms,
      !is.na(.data$.pocet_num), .data$.pocet_num > 0
    ) %>%
    dplyr::mutate(
      # Records of pairs represent two mature individuals
      final_count = dplyr::case_when(
        .data$POCITANO %in% c("amplexus", "kopulace") ~ .data$.pocet_num * 2,
        TRUE ~ .data$.pocet_num
      ),
      # Categorize record type
      type_cat = dplyr::case_when(
        .data$POCITANO %in% male_terms ~ "M",
        .data$POCITANO %in% female_terms ~ "F",
        TRUE ~ "General"
      )
    )

  if (nrow(clean_pop) == 0) {
    res <- empty_result("no countable mature-stage records in window")
    res$n_records_counted <- n_records_counted
    res$ignored_terms <- ignored_terms
    return(res)
  }

  # 3. Aggregate by LOKALITA per Year
  # Sum sexes, compare to general adults -> best estimate for locality/year
  loc_year_summary <- clean_pop %>%
    dplyr::group_by(.data$LOKALITA, .data$ROK) %>%
    dplyr::summarise(
      max_m = max(c(0, .data$final_count[.data$type_cat == "M"]), na.rm = TRUE),
      max_f = max(c(0, .data$final_count[.data$type_cat == "F"]), na.rm = TRUE),
      max_gen = max(c(0, .data$final_count[.data$type_cat == "General"]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      max_local_count = pmax((.data$max_m + .data$max_f), .data$max_gen)
    )

  # 4. Minimum observed count of mature individuals: each locality's most
  # recent count, not maxima from different years summed (Guidelines 4.3).
  loc_counts <- loc_year_summary %>%
    dplyr::group_by(.data$LOKALITA) %>%
    dplyr::summarise(latest = .data$max_local_count[which.max(.data$ROK)],
                     peak = max(.data$max_local_count, na.rm = TRUE),
                     .groups = "drop")
  last_year <- max(loc_year_summary$ROK)

  total_mature <- sum(loc_counts$latest, na.rm = TRUE)
  total_mature_min <- sum(loc_year_summary$max_local_count[loc_year_summary$ROK == last_year])
  total_mature_max <- sum(loc_counts$peak, na.rm = TRUE)

  # 5. Metric: largest single-locality count (subpopulation surrogate)
  max_subpop <- max(loc_counts$latest, na.rm = TRUE)

  # 6. Yearly totals underpinning trend and fluctuation
  yearly_totals <- loc_year_summary %>%
    dplyr::group_by(.data$ROK) %>%
    dplyr::summarise(total_year = sum(.data$max_local_count, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::arrange(.data$ROK)

  n_years <- nrow(yearly_totals)

  # --- DECLINE RATE ---
  # IUCN Guidelines section 4.5.1: where the proportional rate of decline is
  # taken to be constant, reduction over a period is
  #   1 - (observed change)^(assessment period / observed period).
  # Fitting log(N) ~ year is the regression form of that same exponential
  # model, and is less sensitive to a single anomalous end year than reading
  # off the raw first and last totals. The previous implementation fitted an
  # unconstrained *linear* model and took its endpoints, which could report
  # reductions beyond 100% (section 4.5.1 floors projected abundance at zero)
  # and reported change over the observed span rather than over the
  # assessment window.
  decline_rate <- NA_real_
  decline_period_years <- window_years
  if (n_years >= 3) {
    fit <- tryCatch(
      stats::lm(log(total_year) ~ ROK, data = yearly_totals),
      error = function(e) NULL
    )
    if (!is.null(fit) && is.finite(stats::coef(fit)[["ROK"]])) {
      annual_rate <- stats::coef(fit)[["ROK"]]
      change <- (exp(annual_rate * decline_period_years) - 1) * 100
      # A reduction cannot exceed 100%.
      decline_rate <- round(max(change, -100), 1)
    }
  }

  # --- EXTREME FLUCTUATIONS ---
  # IUCN Guidelines section 4.7: variation "greater than one order of
  # magnitude" AND a *recurring* pattern of increases and decreases; section
  # 4.6 adds that "an observed decline should not be considered as a
  # fluctuation unless there is evidence for this". A bare max/min ratio --
  # the previous test -- labels any steep monotonic decline (and any gap in
  # recording effort) an extreme fluctuation, so reversals are required too.
  # Opportunistic counts change with recording effort, so they diagnose
  # nothing: only a consistent-effort series does (Guidelines 4.7).
  fluct_ratio <- NA_real_
  fluct_reversals <- NA_integer_
  extreme_fluctuation <- NA
  if (n_years >= 3) {
    min_val <- min(yearly_totals$total_year)
    max_val <- max(yearly_totals$total_year)
    fluct_ratio <- if (min_val > 0) max_val / min_val else Inf

    signs <- sign(diff(yearly_totals$total_year))
    signs <- signs[signs != 0]
    fluct_reversals <- if (length(signs) > 1) sum(diff(signs) != 0) else 0L

    if (consistent_effort) {
      extreme_fluctuation <- isTRUE(fluct_ratio >= 10) &&
        fluct_reversals >= 2 && n_years >= 5
    }
  }

  return(list(
    total_mature = total_mature,
    total_mature_min = total_mature_min,
    total_mature_max = total_mature_max,
    decline_rate = decline_rate,
    # Guidelines Table 3.1: C1 needs an estimated decline.
    decline_estimated = consistent_effort && !is.na(decline_rate),
    max_subpop = max_subpop,
    fluct_ratio = if (is.finite(fluct_ratio)) round(fluct_ratio, 1) else fluct_ratio,
    fluct_reversals = as.integer(fluct_reversals),
    extreme_fluctuation = extreme_fluctuation,
    decline_period_years = decline_period_years,
    n_years = as.integer(n_years),
    n_localities = nrow(loc_counts),
    n_records_counted = n_records_counted,
    n_records_used = nrow(clean_pop),
    ignored_terms = ignored_terms,
    note = if (consistent_effort) NA_character_ else
      "opportunistic counts: extreme fluctuations not diagnosed, decline not an estimate (Guidelines 4.7, Table 3.1)"
  ))
}

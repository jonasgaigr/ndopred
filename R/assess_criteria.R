#' Evaluate Criterion A (Population Reduction)
#'
#' @param trend_res Result from [calculate_trend()].
#' @param a_criteria Character. `"A1"` uses the 90/70/50 thresholds (causes
#'   reversible, understood and ceased); anything else uses the A2/A3/A4
#'   thresholds of 80/50/30.
#' @return Suggested category (character).
#' @seealso [summarize_assessment()], which applies criterion A inside the
#'   full cascade alongside B, C and D.
#' @references IUCN (2012), criterion A; Guidelines v16 section 5.
#' @examples
#' assess_criterion_a(list(percent_change = -85))          # "CR" under A2
#' assess_criterion_a(list(percent_change = -85), "A1")     # "EN" under A1
#' assess_criterion_a(list(percent_change = -22))          # "NT": 20-29%
#' assess_criterion_a(list(percent_change = 15))           # increase -> LC
#' assess_criterion_a(list(percent_change = NA))           # "DD"
#' @export
assess_criterion_a <- function(trend_res, a_criteria = "A2") {
  if (is.null(trend_res$percent_change) || is.na(trend_res$percent_change)) {
    return("DD") # Data Deficient
  }

  # Decline is a negative change: -80 means an 80% reduction.
  decline <- -1 * trend_res$percent_change
  if (decline <= 0) return("LC")

  if (identical(a_criteria, "A1")) {
    if (decline >= 90) return("CR")
    if (decline >= 70) return("EN")
    if (decline >= 50) return("VU")
  } else {
    if (decline >= 80) return("CR")
    if (decline >= 50) return("EN")
    if (decline >= 30) return("VU")
  }

  # Guidelines 10.1: a 20-25% reduction is given as a justified NT listing.
  if (decline >= 20) return("NT")
  return("LC")
}

#' Evaluate Criterion D2 (Restricted AOO/Locations)
#'
#' IUCN Guidelines (v16, 2024) section 8 is explicit that the AOO and location
#' figures under D2 are indicators rather than thresholds, and that "simply
#' meeting the suggested (or any other) threshold for AOO or number of
#' locations is not sufficient": D2 also requires a plausible threat capable
#' of driving the taxon to CR or EX in a very short time. Literal application
#' of the numbers alone is called out in the same section as a cause of
#' "massive over-listing", so `plausible_threat` must be asserted explicitly.
#'
#' @param aoo_km2 Numeric AOO.
#' @param locations Numeric location count.
#' @param plausible_threat Logical. Has the assessor identified a plausible
#'   threat that could drive the taxon to CR or EX in a very short time?
#' @return `"VU"` if D2 is met, otherwise `"LC"`.
#' @seealso [summarize_assessment()], whose `d2_threat` argument does the same
#'   job inside the full cascade and additionally reports `d2_candidate` for
#'   restricted taxa awaiting a threat assessment.
#' @references IUCN Standards and Petitions Committee (2024), section 8.
#' @examples
#' # A small range alone is not enough...
#' assess_criterion_d2(aoo_km2 = 10, locations = 3)
#'
#' # ...but a small range plus a plausible threat is VU D2.
#' assess_criterion_d2(aoo_km2 = 10, locations = 3, plausible_threat = TRUE)
#'
#' # A widespread taxon does not qualify even with a threat.
#' assess_criterion_d2(aoo_km2 = 5000, locations = 40, plausible_threat = TRUE)
#' @export
assess_criterion_d2 <- function(aoo_km2, locations, plausible_threat = FALSE) {
  restricted <- (!is.na(aoo_km2) && aoo_km2 < 20) ||
    (!is.na(locations) && locations <= 5)

  if (!restricted) return("LC")
  if (isTRUE(plausible_threat)) return("VU")
  return("LC")
}

#' Run Full IUCN Assessment for a Single Species (Harmonised Pipeline)
#'
#' Downloads NDOP records for one taxon and runs the whole chain: quality
#' control, spatial metrics for the recent window, trend against the
#' comparison window, optional abundance metrics, and the criteria cascade.
#'
#' Criterion B metrics describe the **current** distribution and so use only
#' `recent_start`–`recent_end`; the trend compares that window against
#' `comp_start`–`comp_end` (see [calculate_trend()]).
#'
#' @param sp Character. Species scientific name.
#' @param recent_start Numeric. Start year for recent data.
#' @param recent_end Numeric. End year for recent data.
#' @param comp_start Numeric. Start year for comparison data.
#' @param comp_end Numeric. End year for comparison data.
#' @param evaluate_pop Logical. Apply criteria C and D1 (vertebrates, plants).
#' @param ... Further arguments passed to [summarize_assessment()], e.g.
#'   `d2_threat` or `severely_fragmented`.
#' @return A one-row data frame: `Species`, `Category`, `Criteria`, `EOO_km2`,
#'   `AOO_km2`, `Locations`, `Trend_Perc`.
#' @seealso [batch_assess()] for many taxa, [run_app()] for the interactive
#'   expert workflow, [summarize_assessment()] for the cascade itself.
#' @examples
#' \dontrun{
#' # Last ten years against the ten before them.
#' assess_species("Lucanus cervus",
#'                recent_start = 2016, recent_end = 2025,
#'                comp_start = 2006, comp_end = 2015)
#'
#' # A taxon with usable counts, with criteria C and D1 enabled.
#' assess_species("Bombina bombina",
#'                recent_start = 2016, recent_end = 2025,
#'                comp_start = 2006, comp_end = 2015,
#'                evaluate_pop = TRUE)
#' }
#' @export
assess_species <- function(sp, recent_start, recent_end, comp_start, comp_end,
                           evaluate_pop = FALSE, ...) {

  # 1. Get & Clean Data
  raw_occ <- get_assessment_data(sp)
  occ_all <- clean_dates(raw_occ)

  n_records <- nrow(occ_all)
  year_last <- if (n_records > 0) suppressWarnings(max(occ_all$year, na.rm = TRUE)) else NA_real_
  if (!is.finite(year_last)) year_last <- NA_real_

  empty_row <- function(category, note) {
    data.frame(
      Species = sp, Category = category, Criteria = note,
      EOO_km2 = NA_real_, AOO_km2 = NA_real_, Locations = NA_real_,
      Trend_Perc = NA_real_, stringsAsFactors = FALSE
    )
  }

  if (n_records == 0) return(empty_row("DD", "No usable NDOP records"))

  # 2. Filter for current status (criterion B uses the current distribution -
  # Guidelines 4.9 - not the whole reduction window).
  occ_recent <- occ_all %>%
    dplyr::filter(.data$year >= recent_start & .data$year <= recent_end)

  if (nrow(occ_recent) == 0) {
    # No recent records: hand the decision to the RE/DD screens rather than
    # inventing an "EX?" pseudo-category outside the IUCN scheme.
    return(summarize_assessment(
      sp,
      eoo = list(area_km2 = NA_real_), aoo = list(area_km2 = 0),
      trend = list(percent_change = NA_real_), locations = 0,
      pop_metrics = list(), evaluate_pop = evaluate_pop,
      year_last = year_last, n_records = n_records, ...
    )$result)
  }

  # 3. Compute metrics
  eoo <- calculate_eoo(occ_recent)
  aoo <- calculate_aoo(occ_recent)
  locs <- calculate_locations(occ_recent)

  trend <- calculate_trend(
    occ_all,
    recent_start = recent_start, recent_end = recent_end,
    comp_start = comp_start, comp_end = comp_end
  )

  pop <- if (evaluate_pop) {
    calculate_pop_metrics(occ_all, window_years = recent_end - recent_start + 1,
                          year_now = recent_end)
  } else {
    list()
  }

  # 4. Summarise
  summarize_assessment(
    sp, eoo, aoo, trend, locs, pop,
    evaluate_pop = evaluate_pop,
    year_last = year_last, n_records = n_records,
    locations_trend = NULL, ...
  )$result
}

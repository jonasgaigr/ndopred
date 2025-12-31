#' Summarize Assessment with Strict IUCN Cascade Logic
#'
#' Applies strict "2 of 3" subcriteria rules for Criterion B and integrates
#' population metrics for D2 and Criterion B(c) fluctuations.
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2`.
#' @param aoo List containing `area_km2`.
#' @param trend List containing `percent_change`.
#' @param locations Numeric. Number of locations.
#' @param pop_metrics List containing `decline_rate` and `fluct_ratio`.
#' @return A data frame with the automated category and criteria.
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics) {

  eoo_val <- eoo$area_km2
  aoo_val <- aoo$area_km2
  locs_val <- locations

  # Safe numeric conversions for binary flags
  # Using suppressWarnings to handle cases where input might be "NA" string
  trend_val <- suppressWarnings(as.numeric(trend$percent_change))
  pop_decline <- suppressWarnings(as.numeric(pop_metrics$decline_rate))
  fluct_ratio <- suppressWarnings(as.numeric(pop_metrics$fluct_ratio))

  # Determine active sub-criteria flags (Binary)
  # b: Continuing Decline (either AOO/EOO trend or Pop trend)
  has_decline <- (!is.na(trend_val) && trend_val < 0) ||
    (!is.na(pop_decline) && pop_decline < 0)

  # c: Extreme Fluctuations (> 10x)
  has_fluct <- (!is.na(fluct_ratio) && fluct_ratio > 10)

  # --- CASCADE LOGIC ---
  # Internal function to evaluate B1 (EOO) or B2 (AOO) levels
  evaluate_b <- function(area_val, type) {
    t_cr <- if(type=="B1") 100 else 10
    t_en <- if(type=="B1") 5000 else 500
    t_vu <- if(type=="B1") 20000 else 2000

    # Check CR
    if (area_val < t_cr) {
      met_a <- (locs_val == 1)
      # Must meet 2 of 3: (a) Locs, (b) Decline, (c) Fluctuation
      if (sum(met_a, has_decline, has_fluct) >= 2) {
        code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
        return(list(cat="CR", code=code))
      }
    }
    # Check EN
    if (area_val < t_en) {
      met_a <- (locs_val <= 5)
      if (sum(met_a, has_decline, has_fluct) >= 2) {
        code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
        return(list(cat="EN", code=code))
      }
    }
    # Check VU
    if (area_val < t_vu) {
      met_a <- (locs_val <= 10)
      if (sum(met_a, has_decline, has_fluct) >= 2) {
        code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
        return(list(cat="VU", code=code))
      }
    }
    return(list(cat="LC", code=""))
  }

  b1_res <- evaluate_b(eoo_val, "B1")
  b2_res <- evaluate_b(aoo_val, "B2")

  # Rank and Select Highest Category
  cats <- c("LC", "NT", "VU", "EN", "CR")
  get_rank <- function(x) match(x, cats)

  final_cat <- "LC"
  final_crit <- c()

  # Apply B1
  if (get_rank(b1_res$cat) > get_rank(final_cat)) {
    final_cat <- b1_res$cat
    final_crit <- c(b1_res$code)
  }

  # Apply B2
  if (get_rank(b2_res$cat) > get_rank(final_cat)) {
    final_cat <- b2_res$cat
    final_crit <- c(b2_res$code)
  } else if (get_rank(b2_res$cat) == get_rank(final_cat) && b2_res$cat != "LC") {
    final_crit <- c(final_crit, b2_res$code)
  }

  # Apply D2 Criteria (Only valid if category is VU or lower)
  is_d2 <- (aoo_val < 20 || locs_val <= 5)
  if (is_d2) {
    if (get_rank(final_cat) < get_rank("VU")) {
      final_cat <- "VU"
      final_crit <- c("D2")
    } else if (final_cat == "VU") {
      final_crit <- c(final_crit, "D2")
    }
  }

  return(data.frame(
    Species = species,
    Category = final_cat,
    Criteria = paste(unique(final_crit), collapse = "; "),
    EOO_km2 = round(eoo_val, 2),
    AOO_km2 = round(aoo_val, 2),
    Locations = locations,
    Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1)),
    Flags = "None",
    stringsAsFactors = FALSE
  ))
}

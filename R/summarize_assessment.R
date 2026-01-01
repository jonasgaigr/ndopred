#' Summarize Assessment with Strict IUCN Cascade Logic
#'
#' Evaluates Criteria A and B using both spatial and population trends.
#' Returns raw boolean flags for UI filtering.
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2`.
#' @param aoo List containing `area_km2`.
#' @param trend List containing `percent_change`.
#' @param locations Numeric. Number of locations.
#' @param pop_metrics List containing `decline_rate`, `fluct_ratio`, and `total_mature`.
#' @return A list containing `result` (display dataframe) and `details` (boolean flags).
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics) {

  eoo_val <- eoo$area_km2
  aoo_val <- aoo$area_km2
  locs_val <- locations

  # Safe conversions
  trend_val <- suppressWarnings(as.numeric(trend$percent_change))
  pop_decline <- suppressWarnings(as.numeric(pop_metrics$decline_rate))
  fluct_ratio <- suppressWarnings(as.numeric(pop_metrics$fluct_ratio))

  # --- 1. DETECT SUB-CRITERIA FLAGS (Data Level) ---

  # B(b) Continuing Decline Flags
  b_indices <- character(0)

  # Spatial Decline (Trend < 0) -> triggers:
  # (i) EOO, (ii) AOO, (iii) Habitat Quality (Inferred)
  if (!is.na(trend_val) && trend_val < 0) {
    b_indices <- c(b_indices, "i", "ii", "iii")
  }

  # Population Decline -> triggers (v) Mature Individuals
  if (!is.na(pop_decline) && pop_decline < 0) {
    b_indices <- c(b_indices, "v")
  }

  has_decline <- length(b_indices) > 0

  # B(c) Extreme Fluctuations Flags
  c_indices <- character(0)
  if (!is.na(fluct_ratio) && fluct_ratio > 10) {
    c_indices <- c(c_indices, "iv") # (iv) is Mature Indiv for Fluctuation
  }
  has_fluct <- length(c_indices) > 0

  # B(a) Severe Fragmentation / Locations
  # Note: Specific thresholds (1, 5, 10) are checked in evaluate_b
  loc_flag <- (locs_val <= 10)

  # --- 2. EVALUATE B CRITERIA (Geographic Range) ---
  evaluate_b <- function(area_val, type) {
    t_cr <- if(type=="B1") 100 else 10
    t_en <- if(type=="B1") 5000 else 500
    t_vu <- if(type=="B1") 20000 else 2000

    cat <- "LC"; code <- ""; met_subs <- FALSE

    if (!is.na(area_val)) {
      if (area_val < t_cr)      { curr_cat <- "CR"; thresh_loc <- 1 }
      else if (area_val < t_en) { curr_cat <- "EN"; thresh_loc <- 5 }
      else if (area_val < t_vu) { curr_cat <- "VU"; thresh_loc <- 10 }
      else                      { curr_cat <- "LC"; thresh_loc <- 0 }

      if (curr_cat != "LC") {
        met_a <- (locs_val <= thresh_loc)

        # IUCN Rule: Must meet 2 of 3 conditions (a, b, c)
        conditions_met <- sum(met_a, has_decline, has_fluct)

        if (conditions_met >= 2) {
          cat <- curr_cat
          # Code generation (includes ALL detected flags, filtering happens in UI)
          sub_str <- ""
          if (met_a) sub_str <- paste0(sub_str, "a")
          if (has_decline) sub_str <- paste0(sub_str, "b(", paste(sort(unique(b_indices)), collapse=","), ")")
          if (has_fluct) sub_str <- paste0(sub_str, "c(", paste(sort(unique(c_indices)), collapse=","), ")")

          code <- paste0(type, sub_str)
          met_subs <- TRUE
        }
      }
    }
    return(list(cat=cat, code=code, met_subs=met_subs))
  }

  b1_res <- evaluate_b(eoo_val, "B1")
  b2_res <- evaluate_b(aoo_val, "B2")

  # --- 3. EVALUATE A CRITERIA (Population Reduction) ---
  # We evaluate both Spatial (A2c) and Population (A2a/b) trends

  get_a_cat <- function(val) {
    if (is.na(val)) return("LC")
    val <- abs(val) # Reduction is positive magnitude
    if (val >= 80) return("CR")
    if (val >= 50) return("EN")
    if (val >= 30) return("VU")
    return("LC")
  }

  # A2 based on Spatial data (c)
  cat_a_spatial <- "LC"
  if (!is.na(trend_val) && trend_val < 0) cat_a_spatial <- get_a_cat(trend_val)

  # A2 based on Population data (b) - assuming Index of Abundance
  cat_a_pop <- "LC"
  if (!is.na(pop_decline) && pop_decline < 0) cat_a_pop <- get_a_cat(pop_decline)

  # Select highest threat for A
  cats <- c("LC", "NT", "VU", "EN", "CR")
  get_rank <- function(x) match(x, cats)

  cat_A <- "LC"; code_A <- ""; a_type <- character(0); a_basis <- character(0)

  if (get_rank(cat_a_pop) >= get_rank(cat_a_spatial) && cat_a_pop != "LC") {
    cat_A <- cat_a_pop
    a_type <- "A2"
    a_basis <- c("b") # Population index
    code_A <- "A2b"
  } else if (cat_a_spatial != "LC") {
    cat_A <- cat_a_spatial
    a_type <- "A2"
    a_basis <- c("c") # Decline in AOO/EOO/Habitat
    code_A <- "A2c"
  }

  # --- 4. AGGREGATE RESULTS ---
  final_cat <- "LC"
  final_crit <- character(0)

  # Compare B1
  if (get_rank(b1_res$cat) > get_rank(final_cat)) { final_cat <- b1_res$cat; final_crit <- c(b1_res$code) }

  # Compare B2
  if (get_rank(b2_res$cat) > get_rank(final_cat)) { final_cat <- b2_res$cat; final_crit <- c(b2_res$code) }
  else if (get_rank(b2_res$cat) == get_rank(final_cat) && b2_res$cat != "LC") { final_crit <- c(final_crit, b2_res$code) }

  # Compare A
  if (get_rank(cat_A) > get_rank(final_cat)) { final_cat <- cat_A; final_crit <- c(code_A) }
  else if (get_rank(cat_A) == get_rank(final_cat) && cat_A != "LC") { final_crit <- c(final_crit, code_A) }

  # D2 Check (Restricted Area)
  is_d2 <- (aoo_val < 20 || locs_val <= 5)
  d2_active <- FALSE
  if (is_d2) {
    if (get_rank(final_cat) < get_rank("VU")) { final_cat <- "VU"; final_crit <- c("D2"); d2_active <- TRUE }
    else if (final_cat == "VU") { final_crit <- c(final_crit, "D2"); d2_active <- TRUE }
  }

  crit_string <- paste(unique(final_crit), collapse="; ")
  if(crit_string == "") crit_string <- "None"

  list(
    result = data.frame(
      Species = species,
      Category = final_cat,
      Criteria = crit_string,
      EOO_km2 = round(eoo_val, 2),
      AOO_km2 = round(aoo_val, 2),
      Locations = locs_val,
      Trend_Perc = ifelse(is.na(trend_val), "NA", round(trend_val, 1)),
      stringsAsFactors = FALSE
    ),
    details = list(
      a_type = a_type,
      a_basis = a_basis,
      b_indices = b_indices,
      c_indices = c_indices,
      loc_flag = loc_flag,
      d2_flag = d2_active
    )
  )
}

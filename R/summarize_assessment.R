#' Summarize Assessment with Strict IUCN Cascade Logic
#'
#' Applies strict "2 of 3" subcriteria rules for Criterion B and integrates
#' population metrics for D2 and Criterion B(c) fluctuations. Returns both
#' a display-ready summary and raw boolean flags for UI pre-filling.
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2`.
#' @param aoo List containing `area_km2`.
#' @param trend List containing `percent_change`.
#' @param locations Numeric. Number of locations.
#' @param pop_metrics List containing `decline_rate`, `fluct_ratio`, and `total_mature`.
#' @return A list containing `result` (data.frame for display) and `details` (list of flags for UI).
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics) {

  eoo_val <- eoo$area_km2
  aoo_val <- aoo$area_km2
  locs_val <- locations

  # Safe numeric conversions
  trend_val <- suppressWarnings(as.numeric(trend$percent_change))
  pop_decline <- suppressWarnings(as.numeric(pop_metrics$decline_rate))
  fluct_ratio <- suppressWarnings(as.numeric(pop_metrics$fluct_ratio))
  total_mature <- suppressWarnings(as.numeric(pop_metrics$total_mature))

  # --- 1. DETECT SUB-CRITERIA FLAGS (Source of Truth) ---

  # Sub-criterion (b): Continuing decline
  # Logic: If spatial trend < 0 -> assume EOO(i) and AOO(ii) decline
  # Logic: If pop trend < 0 -> assume Mature Indiv(v) decline
  b_indices <- character(0)
  if (!is.na(trend_val) && trend_val < 0) b_indices <- c(b_indices, "i", "ii")
  if (!is.na(pop_decline) && pop_decline < 0) b_indices <- c(b_indices, "v")
  has_decline <- length(b_indices) > 0

  # Sub-criterion (c): Extreme fluctuations
  c_indices <- character(0)
  if (!is.na(fluct_ratio) && fluct_ratio > 10) c_indices <- c(c_indices, "iv") # iv = Mature individuals
  has_fluct <- length(c_indices) > 0

  # Sub-criterion (a): Severe fragmentation / Locations
  # We track the specific location thresholds inside evaluate_b,
  # but we export this generic flag for the UI pre-fill.
  loc_flag <- (locs_val <= 10)

  # --- 2. EVALUATE B CRITERIA ---
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
        conditions_met <- sum(met_a, has_decline, has_fluct)

        if (conditions_met >= 2) {
          cat <- curr_cat
          # Construct code string: e.g., B1ab(i,ii)
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

  # --- 3. EVALUATE A CRITERIA ---
  cat_A <- "LC"; code_A <- ""
  a_type <- character(0)
  if (!is.na(trend_val) && trend_val <= -30) {
    # Assuming A2 for observed historical decline
    a_type <- "A2"
    cat_A <- dplyr::case_when(trend_val <= -80 ~ "CR", trend_val <= -50 ~ "EN", TRUE ~ "VU")
    code_A <- paste0("A2b") # Defaulting to 'b' (index of abundance)
  }

  # --- 4. AGGREGATE RESULTS ---
  cats <- c("LC", "NT", "VU", "EN", "CR")
  get_rank <- function(x) match(x, cats)

  final_cat <- "LC"
  final_crit <- character(0)

  # Compare B
  if (get_rank(b1_res$cat) > get_rank(final_cat)) { final_cat <- b1_res$cat; final_crit <- c(b1_res$code) }
  if (get_rank(b2_res$cat) > get_rank(final_cat)) { final_cat <- b2_res$cat; final_crit <- c(b2_res$code) }
  else if (get_rank(b2_res$cat) == get_rank(final_cat) && b2_res$cat != "LC") { final_crit <- c(final_crit, b2_res$code) }

  # Compare A
  if (get_rank(cat_A) > get_rank(final_cat)) { final_cat <- cat_A; final_crit <- c(code_A) }
  else if (get_rank(cat_A) == get_rank(final_cat) && cat_A != "LC") { final_crit <- c(final_crit, code_A) }

  # D2 Check
  is_d2 <- (aoo_val < 20 || locs_val <= 5)
  d2_active <- FALSE
  if (is_d2) {
    if (get_rank(final_cat) < get_rank("VU")) { final_cat <- "VU"; final_crit <- c("D2"); d2_active <- TRUE }
    else if (final_cat == "VU") { final_crit <- c(final_crit, "D2"); d2_active <- TRUE }
  }

  crit_string <- paste(unique(final_crit), collapse="; ")
  if(crit_string == "") crit_string <- "None"

  # --- 5. RETURN STRUCTURED OBJECT ---
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
      b_indices = b_indices,
      c_indices = c_indices,
      loc_flag = loc_flag,
      d2_flag = d2_active
    )
  )
}

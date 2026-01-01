#' Summarize Assessment with Strict IUCN Cascade Logic
#'
#' Evaluates Criteria A-E, plus Pre-Assessment for RE/DD.
#' FIX: Prevents CR/EN/VU assignment if species is absent (AOO=0).
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2`.
#' @param aoo List containing `area_km2`.
#' @param trend List containing `percent_change`.
#' @param locations Numeric. Number of locations.
#' @param pop_metrics List containing `decline_rate`, `fluct_ratio`, `total_mature`, and `max_subpop`.
#' @param evaluate_pop Logical. If FALSE, ignores Criteria C, D1, and pop-dependent subcriteria.
#' @param year_last Numeric. The most recent year observed in the dataset.
#' @param n_records Numeric. Total number of records in the dataset.
#' @return A list containing `result` (display dataframe) and `details` (boolean flags).
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics,
                                 evaluate_pop = TRUE, year_last = NA, n_records = NA) {

  # --- 0. PRE-ASSESSMENT CHECKS (RE & DD) ---
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Check Regionally Extinct (RE)
  if (!is.na(year_last) && (current_year - year_last) > 50) {
    return(list(
      result = data.frame(
        Species = species, Category = "RE",
        Criteria = paste0("Last recorded: ", year_last),
        EOO_km2 = NA, AOO_km2 = NA, Locations = NA, Trend_Perc = NA,
        stringsAsFactors = FALSE
      ),
      details = list(a_type=character(0), a_basis=character(0), b_indices=character(0),
                     c_indices=character(0), loc_flag=FALSE, c1=FALSE, c2_ai=FALSE,
                     c2_aii=FALSE, c2_b=FALSE, d1_flag=FALSE, d2_flag=FALSE)
    ))
  }

  # Check Data Deficient (DD) - Global poverty
  if (!is.na(n_records) && n_records < 3) {
    return(list(
      result = data.frame(
        Species = species, Category = "DD",
        Criteria = paste0("Insufficient Data (n=", n_records, ")"),
        EOO_km2 = NA, AOO_km2 = NA, Locations = NA, Trend_Perc = NA,
        stringsAsFactors = FALSE
      ),
      details = list(a_type=character(0), a_basis=character(0), b_indices=character(0),
                     c_indices=character(0), loc_flag=FALSE, c1=FALSE, c2_ai=FALSE,
                     c2_aii=FALSE, c2_b=FALSE, d1_flag=FALSE, d2_flag=FALSE)
    ))
  }

  # --- SETUP VARIABLES ---
  eoo_val <- eoo$area_km2
  aoo_val <- aoo$area_km2
  locs_val <- locations
  trend_val <- suppressWarnings(as.numeric(trend$percent_change))
  pop_decline <- suppressWarnings(as.numeric(pop_metrics$decline_rate))
  fluct_ratio <- suppressWarnings(as.numeric(pop_metrics$fluct_ratio))
  total_mature <- suppressWarnings(as.numeric(pop_metrics$total_mature))
  max_subpop   <- suppressWarnings(as.numeric(pop_metrics$max_subpop))

  prop_largest <- if(!is.na(max_subpop) && !is.na(total_mature) && total_mature > 0) max_subpop/total_mature else 0

  # --- 1. DETECT FLAGS ---
  has_decline_any <- (!is.na(trend_val) && trend_val < 0)
  if (evaluate_pop && !is.na(pop_decline) && pop_decline < 0) has_decline_any <- TRUE

  has_fluct <- (evaluate_pop && !is.na(fluct_ratio) && fluct_ratio > 10)

  b_indices <- character(0)
  if (!is.na(trend_val) && trend_val < 0) b_indices <- c(b_indices, "i", "ii", "iii")
  if (evaluate_pop && !is.na(pop_decline) && pop_decline < 0) b_indices <- c(b_indices, "v")
  has_b_decline <- length(b_indices) > 0

  c_indices_b <- character(0)
  if (has_fluct) c_indices_b <- c(c_indices_b, "iv")

  loc_flag <- (locs_val <= 10)

  # *** GLOBAL GATE: IS SPECIES EXTANT IN WINDOW? ***
  # If AOO is 0, we treat it as "Absent in window".
  # This prevents A/B criteria from triggering on "0 < Threshold".
  is_extant <- (!is.na(aoo_val) && aoo_val > 0)

  # --- 2. EVALUATE B CRITERIA ---
  evaluate_b <- function(area_val, type) {
    if (!is_extant) return(list(cat="LC", code="")) # Skip B if absent

    t_cr <- if(type=="B1") 100 else 10; t_en <- if(type=="B1") 5000 else 500; t_vu <- if(type=="B1") 20000 else 2000
    cat <- "LC"; code <- ""

    if (!is.na(area_val)) {
      if (area_val < t_cr)      { curr_cat <- "CR"; thresh_loc <- 1 }
      else if (area_val < t_en) { curr_cat <- "EN"; thresh_loc <- 5 }
      else if (area_val < t_vu) { curr_cat <- "VU"; thresh_loc <- 10 }
      else                      { curr_cat <- "LC"; thresh_loc <- 0 }

      if (curr_cat != "LC") {
        met_a_specific <- (locs_val <= thresh_loc)
        cond_sum <- sum(met_a_specific, has_b_decline, has_fluct)

        if (cond_sum >= 2) {
          cat <- curr_cat
          sub_str <- ""
          if (met_a_specific) sub_str <- paste0(sub_str, "a")
          if (has_b_decline) sub_str <- paste0(sub_str, "b(", paste(sort(unique(b_indices)), collapse=","), ")")
          if (has_fluct) sub_str <- paste0(sub_str, "c(", paste(sort(unique(c_indices_b)), collapse=","), ")")
          code <- paste0(type, sub_str)
        } else if (cond_sum == 1) {
          cat <- "NT"
          code <- paste0(type, " (close)")
        }
      }
    }
    return(list(cat=cat, code=code))
  }

  b1_res <- evaluate_b(eoo_val, "B1")
  b2_res <- evaluate_b(aoo_val, "B2")

  # --- 3. EVALUATE A CRITERIA ---
  get_a_cat <- function(val) {
    if (is.na(val)) return("LC")
    val <- abs(val)
    if (val >= 80) return("CR")
    if (val >= 50) return("EN")
    if (val >= 30) return("VU")
    if (val >= 20) return("NT")
    return("LC")
  }

  cat_A <- "LC"; code_A <- ""; a_type <- character(0); a_basis <- character(0)

  # Only evaluate A if extant (prevents "100% decline to zero" from triggering CR for an extinct species)
  if (is_extant) {
    cat_a_spatial <- if(!is.na(trend_val) && trend_val < 0) get_a_cat(trend_val) else "LC"
    cat_a_pop <- "LC"
    if (evaluate_pop && !is.na(pop_decline) && pop_decline < 0) cat_a_pop <- get_a_cat(pop_decline)

    cats <- c("LC", "NT", "VU", "EN", "CR"); get_rank <- function(x) match(x, cats)

    if (get_rank(cat_a_pop) >= get_rank(cat_a_spatial) && cat_a_pop != "LC") {
      cat_A <- cat_a_pop; a_type <- "A2"; a_basis <- "b"; code_A <- "A2b"
    } else if (cat_a_spatial != "LC") {
      cat_A <- cat_a_spatial; a_type <- "A2"; a_basis <- "c"; code_A <- "A2c"
    }
  }

  # --- 4. EVALUATE C CRITERIA ---
  cat_C <- "LC"; code_C <- ""
  c1_flag <- FALSE; c2_ai_flag <- FALSE; c2_aii_flag <- FALSE; c2_b_flag <- FALSE

  if (evaluate_pop && !is.na(total_mature)) {
    evaluate_c_level <- function(thresh_pop, thresh_c1_decline, thresh_subpop) {
      if (total_mature < thresh_pop) {
        if (!is.na(pop_decline) && abs(pop_decline) >= thresh_c1_decline) return(list(met=TRUE, type="1"))
        if (has_decline_any) {
          is_ai  <- (!is.na(max_subpop) && max_subpop <= thresh_subpop)
          is_aii <- (!is.na(prop_largest) && prop_largest >= 0.95)
          is_b   <- has_fluct
          if (is_ai || is_aii || is_b) {
            code_parts <- ""
            if (is_ai) code_parts <- paste0(code_parts, "a(i)")
            if (is_aii) code_parts <- paste0(code_parts, if(is_ai)","else"", "a(ii)")
            if (is_b) code_parts <- paste0(code_parts, "b")
            return(list(met=TRUE, type=paste0("2", code_parts), flags=c(ai=is_ai, aii=is_aii, b=is_b)))
          }
        }
        return(list(met=FALSE, near=TRUE))
      }
      return(list(met=FALSE, near=FALSE))
    }

    c_cr <- evaluate_c_level(250, 25, 50)
    if (c_cr$met) {
      cat_C <- "CR"; code_C <- paste0("C", c_cr$type)
      if(grepl("1", c_cr$type)) c1_flag <- TRUE
      if(!is.null(c_cr$flags)) { c2_ai_flag<-c_cr$flags['ai']; c2_aii_flag<-c_cr$flags['aii']; c2_b_flag<-c_cr$flags['b'] }
    } else {
      c_en <- evaluate_c_level(2500, 20, 250)
      if (c_en$met) {
        cat_C <- "EN"; code_C <- paste0("C", c_en$type)
        if(grepl("1", c_en$type)) c1_flag <- TRUE
        if(!is.null(c_en$flags)) { c2_ai_flag<-c_en$flags['ai']; c2_aii_flag<-c_en$flags['aii']; c2_b_flag<-c_en$flags['b'] }
      } else {
        c_vu <- evaluate_c_level(10000, 10, 1000)
        if (c_vu$met) {
          cat_C <- "VU"; code_C <- paste0("C", c_vu$type)
          if(grepl("1", c_vu$type)) c1_flag <- TRUE
          if(!is.null(c_vu$flags)) { c2_ai_flag<-c_vu$flags['ai']; c2_aii_flag<-c_vu$flags['aii']; c2_b_flag<-c_vu$flags['b'] }
        } else if (c_vu$near || c_en$near || c_cr$near) {
          cat_C <- "NT"
        }
      }
    }
  }

  # --- 5. AGGREGATE RESULTS ---
  final_cat <- "LC"; final_crit <- character(0)

  update_cat <- function(new_cat, new_code) {
    if (get_rank(new_cat) > get_rank(final_cat)) {
      final_cat <<- new_cat; final_crit <<- c(new_code)
    } else if (get_rank(new_cat) == get_rank(final_cat) && new_cat != "LC" && new_cat != "NT") {
      final_crit <<- c(final_crit, new_code)
    }
  }

  update_cat(cat_A, code_A)
  update_cat(b1_res$cat, b1_res$code)
  update_cat(b2_res$cat, b2_res$code)
  update_cat(cat_C, code_C)

  # D2 Check - REQUIRES EXTANT
  is_d2 <- (is_extant && (aoo_val < 20 || locs_val <= 5))
  d2_active <- FALSE
  if (is_d2) {
    if (get_rank(final_cat) < get_rank("VU")) { final_cat <- "VU"; final_crit <- c("D2"); d2_active <- TRUE }
    else if (final_cat == "VU") { final_crit <- c(final_crit, "D2"); d2_active <- TRUE }
  }

  # D1 Check
  d1_active <- FALSE
  if (evaluate_pop && !is.na(total_mature) && total_mature > 0) { # Requires mature > 0
    if (total_mature < 50) cat_d1 <- "CR"
    else if (total_mature < 250) cat_d1 <- "EN"
    else if (total_mature < 1000) cat_d1 <- "VU"
    else cat_d1 <- "LC"

    if (cat_d1 != "LC") {
      if (get_rank(cat_d1) > get_rank(final_cat)) { final_cat <- cat_d1; final_crit <- c("D1"); d1_active<-TRUE }
      else if (get_rank(cat_d1) == get_rank(final_cat)) { final_crit <- c(final_crit, "D1"); d1_active<-TRUE }
    }
  }

  # *** CATCH-ALL: IF NOT EXTANT (AOO=0) AND NOT RE, THEN DD ***
  if (!is_extant && final_cat == "LC") {
    final_cat <- "DD"
    final_crit <- "Absent in recent window"
  }

  crit_string <- paste(unique(final_crit), collapse="; ")
  if(crit_string == "") crit_string <- "None"

  list(
    result = data.frame(
      Species = species, Category = final_cat, Criteria = crit_string,
      EOO_km2 = round(eoo_val, 2), AOO_km2 = round(aoo_val, 2), Locations = locs_val,
      Trend_Perc = ifelse(is.na(trend_val), "NA", round(trend_val, 1)), stringsAsFactors = FALSE
    ),
    details = list(
      a_type = a_type, a_basis = a_basis,
      b_indices = b_indices, c_indices = c_indices_b, loc_flag = loc_flag,
      c1 = c1_flag, c2_ai = c2_ai_flag, c2_aii = c2_aii_flag, c2_b = c2_b_flag,
      d1_flag = d1_active, d2_flag = d2_active
    )
  )
}

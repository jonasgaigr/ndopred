#' Summarize Assessment with Strict IUCN Cascade Logic
#'
#' Evaluates criteria A-D from automatable metrics, plus screening checks for
#' Regionally Extinct (RE) and Data Deficient (DD). Thresholds follow IUCN Red
#' List Categories and Criteria v3.1 (2012); interpretation follows the
#' Guidelines for Using the IUCN Red List Categories and Criteria v16 (2024).
#'
#' @section What this function will not decide for you:
#' Three subcriteria depend on judgements that occurrence records cannot
#' supply, so they are inputs rather than outputs here:
#' * **B(a) severe fragmentation** (Guidelines 4.8) - pass `severely_fragmented`.
#' * **B(b)(iii) habitat decline** (Guidelines 4.6) - pass `habitat_decline`.
#'   NDOP records carry no habitat information, so this is never inferred.
#' * **D2 plausible threat** (Guidelines 8) - pass `d2_threat`. Section 8 is
#'   explicit that meeting the AOO/location thresholds "is not sufficient",
#'   and that literal interpretation "results in massive over-listing". When
#'   `d2_threat` is FALSE the taxon is only *flagged* as a D2 candidate
#'   (`details$d2_candidate`) and its category is not raised.
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2`.
#' @param aoo List containing `area_km2`.
#' @param trend List containing `percent_change` (change in AOO, per cent) and
#'   optionally `eoo_percent_change`.
#' @param locations Numeric. Number of locations (may be NA).
#' @param pop_metrics List containing `decline_rate`, `extreme_fluctuation`,
#'   `total_mature`, `max_subpop` and optionally `decline_period_years`.
#' @param evaluate_pop Logical. Whether the abundance data in `pop_metrics`
#'   is fit to be used at all. It governs **every** use of that data, not just
#'   criteria C and D1:
#'   * criteria C and D1 (population size);
#'   * criterion A basis (b), the abundance-derived reduction — with
#'     `evaluate_pop = FALSE` criterion A rests on the spatial trend alone,
#'     i.e. basis (c);
#'   * criterion B(b)(v), continuing decline in mature individuals;
#'   * criterion B(c)(iv), extreme fluctuations in mature individuals.
#'
#'   NDOP counts are opportunistic (Guidelines 4.10.8), so a trend derived
#'   from them is not more reliable than a total derived from them. Set this
#'   FALSE whenever the counts are too thin or too unrepresentative to stand
#'   behind — see `n_records_used` / `n_records_counted` from
#'   [calculate_pop_metrics()].
#' @param year_last Numeric. The most recent year observed in the dataset.
#' @param n_records Numeric. Total number of records in the dataset.
#' @param a_criteria Character vector of A criteria to apply. Only `"A1"`
#'   changes the thresholds (90/70/50 instead of 80/50/30); default `"A2"`.
#' @param eoo_trend Numeric. Per cent change in EOO, if measured separately.
#' @param locations_trend Numeric. Per cent change in the number of locations.
#' @param severely_fragmented Logical. Expert judgement, criterion B(a).
#' @param habitat_decline Logical. Expert judgement, criterion B(b)(iii).
#' @param d2_threat Logical. Expert-confirmed plausible threat for D2.
#' @param generation_length Numeric. Generation length in years, used to scale
#'   the C1 reference periods (1/2/3 generations for CR/EN/VU).
#' @param decline_tolerance Numeric, per cent. Measured changes smaller than
#'   this are not asserted as a continuing decline. Criteria B and C2 accept a
#'   continuing decline "at any rate" (Guidelines 4.6), but a trend estimated
#'   by resampling opportunistic records carries sampling noise, and
#'   Guidelines 4.9 warns specifically that convex-hull EOO estimates are
#'   "less suitable as a method for comparing two or more temporal estimates
#'   ... for assessing reductions or continuing declines". Without this guard
#'   a 1% wobble between two samples reads as a continuing decline and can
#'   push an otherwise stable taxon into a threatened category. Set to 0 for
#'   the literal "any rate" reading.
#' @param re_threshold_years Numeric. Years since the last record after which
#'   the taxon is *screened* as Regionally Extinct. See the note on
#'   `details$re_screened` below.
#' @return A list of two elements:
#' \describe{
#'   \item{`result`}{A one-row data frame: `Species`, `Category`, `Criteria`,
#'     `EOO_km2`, `AOO_km2`, `Locations`, `Trend_Perc`. For a Near Threatened
#'     taxon `Criteria` reads "nearly meets ..." rather than a criteria code,
#'     because NT is defined by proximity to Vulnerable and not by criteria of
#'     its own (Guidelines 10.1).}
#'   \item{`details`}{Flags consumed by the Shiny expert-review panel:
#'     `a_type`, `a_basis`, `b_indices`, `c_indices`, `loc_flag`, `c1`,
#'     `c2_ai`, `c2_aii`, `c2_b`, `d1_flag`, `d2_flag`, `d2_candidate`,
#'     `re_screened` and `nt_reasons`.}
#' }
#' @seealso [calculate_eoo()], [calculate_aoo()], [calculate_locations()],
#'   [calculate_trend()], [calculate_pop_metrics()] for the inputs;
#'   [assess_species()] to run the whole chain for one taxon.
#' @references
#' IUCN (2012), *Categories and Criteria v3.1* — the thresholds.
#'
#' IUCN Standards and Petitions Committee (2024), *Guidelines* v16 — sections
#' 4.6, 4.7, 4.9, 8, 10.1, 10.3 and 11.1 as cited inline.
#' @examples
#' # Minimal inputs: a taxon with a restricted range and a measured decline.
#' res <- summarize_assessment(
#'   species     = "Example species",
#'   eoo         = list(area_km2 = 4000),
#'   aoo         = list(area_km2 = 400),
#'   trend       = list(percent_change = -30, eoo_percent_change = -25),
#'   locations   = 4,
#'   pop_metrics = list(),
#'   evaluate_pop = FALSE,
#'   year_last   = 2025,
#'   n_records   = 120
#' )
#' res$result$Category   # "EN": area threshold + (a) few locations + (b) decline
#' res$result$Criteria
#'
#' # Restricted but stable and unthreatened: NOT Near Threatened (Guidelines
#' # 10.1 lists this case among those that should not be listed).
#' summarize_assessment(
#'   species = "Small but stable", eoo = list(area_km2 = 80),
#'   aoo = list(area_km2 = 8), trend = list(percent_change = 0),
#'   locations = 30, pop_metrics = list(), evaluate_pop = FALSE,
#'   year_last = 2025, n_records = 60
#' )$result$Category     # "LC"
#'
#' # Criterion D2 needs a plausible threat, not just a small range.
#' args <- list(
#'   species = "Restricted", eoo = list(area_km2 = 10),
#'   aoo = list(area_km2 = 10), trend = list(percent_change = 0),
#'   locations = 3, pop_metrics = list(), evaluate_pop = FALSE,
#'   year_last = 2025, n_records = 40
#' )
#' do.call(summarize_assessment, args)$details$d2_candidate          # TRUE
#' do.call(summarize_assessment, args)$result$Category               # not VU
#' do.call(summarize_assessment, c(args, list(d2_threat = TRUE)))$result$Category
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics,
                                 evaluate_pop = TRUE, year_last = NA, n_records = NA,
                                 a_criteria = NULL,
                                 eoo_trend = NULL, locations_trend = NULL,
                                 severely_fragmented = FALSE,
                                 habitat_decline = FALSE,
                                 d2_threat = FALSE,
                                 generation_length = 1,
                                 decline_tolerance = 5,
                                 re_threshold_years = 50) {

  # --- 1. SAFE NUMERIC HELPER (Prevents crashes on empty data) ---
  safe_num <- function(x) {
    if (is.null(x)) return(NA_real_)
    val <- suppressWarnings(as.numeric(x))
    if (length(val) == 0) return(NA_real_)
    return(val[1])
  }
  is_true <- function(x) isTRUE(as.logical(x)[1])

  empty_details <- function(extra = list()) {
    utils::modifyList(
      list(a_type = character(0), a_basis = character(0), b_indices = character(0),
           c_indices = character(0), loc_flag = FALSE, c1 = FALSE, c2_ai = FALSE,
           c2_aii = FALSE, c2_b = FALSE, d1_flag = FALSE, d2_flag = FALSE,
           d2_candidate = FALSE, re_screened = FALSE, nt_reasons = character(0)),
      extra
    )
  }

  # --- 2. SETUP VARIABLES ---
  eoo_val <- safe_num(eoo$area_km2)
  aoo_val <- safe_num(aoo$area_km2)
  locs_val <- safe_num(locations)

  # IUCN Guidelines section 4.9: EOO must never be smaller than AOO, and
  # falls back to AOO when it cannot be computed directly (e.g. fewer than
  # 3 occurrence points for a convex hull). Centralised here so every caller
  # of summarize_assessment() (the Shiny bulk app and the batch_assess()
  # pipeline alike) gets a consistent, guideline-compliant value.
  eoo_val <- reconcile_eoo_aoo(eoo_val, aoo_val)

  aoo_trend_val <- safe_num(trend$percent_change)
  eoo_trend_val <- if (!is.null(eoo_trend)) safe_num(eoo_trend) else safe_num(trend$eoo_percent_change)
  locs_trend_val <- safe_num(locations_trend)

  pop_decline <- safe_num(pop_metrics$decline_rate)
  pop_decline_period <- safe_num(pop_metrics$decline_period_years)
  if (is.na(pop_decline_period) || pop_decline_period <= 0) pop_decline_period <- 10
  total_mature <- safe_num(pop_metrics$total_mature)
  max_subpop   <- safe_num(pop_metrics$max_subpop)

  # Extreme fluctuations (Guidelines 4.7) require a recurring pattern of
  # increases and decreases, which calculate_pop_metrics() now diagnoses.
  # A bare max/min ratio is accepted only as a legacy fallback.
  has_fluct <- FALSE
  if (evaluate_pop) {
    if (!is.null(pop_metrics$extreme_fluctuation)) {
      has_fluct <- is_true(pop_metrics$extreme_fluctuation)
    } else {
      fr <- safe_num(pop_metrics$fluct_ratio)
      has_fluct <- (!is.na(fr) && fr >= 10)
    }
  }

  # --- 3. PRE-ASSESSMENT SCREENS (RE & DD) ---
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Regionally Extinct screen.
  # NOTE ON PROVENANCE: neither IUCN (2012) Categories and Criteria v3.1 nor
  # the Guidelines v16 (2024) define a years-since-last-record rule for RE;
  # RE is defined in the Guidelines for Application of IUCN Red List Criteria
  # at Regional and National Levels v4.0, which is not among the documents in
  # methodology/. Guidelines section 11.1 is emphatic that extinction listings
  # require exhaustive surveys and warns against the "Romeo Error". This is
  # therefore a *screening* threshold for expert follow-up, not a
  # determination, and the number of years is a parameter, not a constant.
  if (!is.na(year_last) && (current_year - year_last) > re_threshold_years) {
    return(list(
      result = data.frame(
        Species = species, Category = "RE",
        Criteria = paste0("Screen: no record for >", re_threshold_years,
                          " years (last ", year_last, ") - confirm with surveys"),
        EOO_km2 = NA_real_, AOO_km2 = NA_real_, Locations = NA_real_,
        Trend_Perc = NA_real_,
        stringsAsFactors = FALSE
      ),
      details = empty_details(list(re_screened = TRUE))
    ))
  }

  # Data Deficient (Guidelines 10.3)
  if (is.na(n_records) || n_records < 3 || is.na(aoo_val)) {
    return(list(
      result = data.frame(
        Species = species, Category = "DD",
        Criteria = "Inadequate information to estimate distribution",
        EOO_km2 = NA_real_, AOO_km2 = NA_real_, Locations = NA_real_,
        Trend_Perc = NA_real_,
        stringsAsFactors = FALSE
      ),
      details = empty_details()
    ))
  }

  # --- 4. DERIVED METRICS ---
  prop_largest <- NA_real_
  if (!is.na(max_subpop) && !is.na(total_mature) && total_mature > 0) {
    prop_largest <- max_subpop / total_mature
  }

  cats <- c("LC", "NT", "VU", "EN", "CR")
  get_rank <- function(x) match(x, cats)

  # A measured change counts as a continuing decline only once it clears the
  # sampling-noise tolerance (see @param decline_tolerance).
  tol <- if (is.na(decline_tolerance)) 0 else abs(decline_tolerance)
  declining <- function(x) !is.na(x) && x < 0 && abs(x) >= tol

  # Continuing decline in mature individuals (criteria B(b)(v), C2).
  # Criterion C2 requires "a continuing decline ... in numbers of mature
  # individuals" (v3.1) - a purely spatial trend does not satisfy it, which
  # the previous has_decline_any flag allowed.
  #
  # Gated on evaluate_pop, like every other use of the abundance data. That
  # switch is a judgement about whether the counts are fit to be used at all,
  # and a trend computed from thin or unrepresentative counts is no more
  # trustworthy than a total computed from them - less so, if anything, since
  # it is confounded by recording effort (Guidelines 4.10.8). Letting an
  # abundance trend through while refusing the abundance total would drive
  # criterion A(b) from data the same function has just declared unusable.
  pop_declining <- (evaluate_pop && declining(pop_decline))

  # Criterion B(b): continuing decline, indexed by what was actually measured.
  # (i) EOO and (ii) AOO are now driven by their own trends - the previous
  # version asserted (i) from an AOO trend, and asserted (iii) habitat
  # decline, which NDOP records cannot evidence at all.
  b_indices <- character(0)
  if (declining(eoo_trend_val))  b_indices <- c(b_indices, "i")
  if (declining(aoo_trend_val))  b_indices <- c(b_indices, "ii")
  if (is_true(habitat_decline))  b_indices <- c(b_indices, "iii")
  if (declining(locs_trend_val)) b_indices <- c(b_indices, "iv")
  if (pop_declining)             b_indices <- c(b_indices, "v")
  has_b_decline <- length(b_indices) > 0

  c_indices_b <- character(0)
  if (has_fluct) c_indices_b <- c(c_indices_b, "iv")

  loc_flag <- (!is.na(locs_val) && locs_val <= 10)
  is_extant <- (!is.na(aoo_val) && aoo_val > 0)
  nt_reasons <- character(0)

  # --- 5. EVALUATE B CRITERIA ---
  evaluate_b <- function(area_val, type) {
    if (!is_extant) return(list(cat = "LC", code = "", nt = NULL))

    t_cr <- if (type == "B1") 100 else 10
    t_en <- if (type == "B1") 5000 else 500
    t_vu <- if (type == "B1") 20000 else 2000
    cat <- "LC"; code <- ""; nt <- NULL

    if (!is.na(area_val)) {
      if (area_val < t_cr)      { curr_cat <- "CR"; thresh_loc <- 1 }
      else if (area_val < t_en) { curr_cat <- "EN"; thresh_loc <- 5 }
      else if (area_val < t_vu) { curr_cat <- "VU"; thresh_loc <- 10 }
      else                      { curr_cat <- "LC"; thresh_loc <- 0 }

      if (curr_cat != "LC") {
        met_a_specific <- is_true(severely_fragmented) ||
          (!is.na(locs_val) && locs_val <= thresh_loc)
        cond_sum <- sum(met_a_specific, has_b_decline, has_fluct)

        if (cond_sum >= 2) {
          sub_str <- ""
          if (met_a_specific) sub_str <- paste0(sub_str, "a")
          if (has_b_decline) sub_str <- paste0(sub_str, "b(", paste(sort(unique(b_indices)), collapse = ","), ")")
          if (has_fluct) sub_str <- paste0(sub_str, "c(", paste(sort(unique(c_indices_b)), collapse = ","), ")")
          cat <- curr_cat
          code <- paste0(type, sub_str)
        } else if (cond_sum == 1) {
          # Guidelines 10.1: "meets the area requirements under criterion B
          # for threatened ... and is declining, but ... occurs at many more
          # than 10 locations, and there are no extreme fluctuations" -> NT.
          cat <- "NT"
          nt <- paste0("nearly meets ", type, " (area threshold met, only 1 of 3 conditions)")
        }
        # cond_sum == 0 is deliberately NOT Near Threatened. Guidelines 10.1
        # lists "meets the area requirements under criterion B for CR ... but
        # is not declining, not severely fragmented, there are no extreme
        # fluctuations, and there are no obvious threats" among the cases that
        # should NOT be listed as NT. The previous code returned NT here and
        # cited section 10.1 for doing so.
      }
    }
    return(list(cat = cat, code = code, nt = nt))
  }

  b1_res <- evaluate_b(eoo_val, "B1")
  b2_res <- evaluate_b(aoo_val, "B2")
  nt_reasons <- c(nt_reasons, b1_res$nt, b2_res$nt)

  # --- 6. EVALUATE A CRITERIA ---
  use_a1 <- !is.null(a_criteria) && "A1" %in% a_criteria
  a_label <- if (use_a1) "A1" else "A2"
  get_a_cat <- function(val) {
    if (is.na(val)) return("LC")
    val <- abs(val)
    if (use_a1) {
      if (val >= 90) return("CR")
      if (val >= 70) return("EN")
      if (val >= 50) return("VU")
    } else {
      if (val >= 80) return("CR")
      if (val >= 50) return("EN")
      if (val >= 30) return("VU")
    }
    # Guidelines 10.1: "Population has declined by an estimated 20-25% in the
    # last three generations" is given as a justified NT listing.
    if (val >= 20) return("NT")
    return("LC")
  }

  cat_A <- "LC"; code_A <- ""; a_type <- character(0); a_basis <- character(0)

  if (is_extant) {
    # Basis (c): "a decline in area of occupancy, extent of occurrence and/or
    # quality of habitat". Basis (b): "an index of abundance appropriate to
    # the taxon". Take the more severe of the two measured declines.
    spatial_decline <- suppressWarnings(min(c(aoo_trend_val, eoo_trend_val), na.rm = TRUE))
    if (!is.finite(spatial_decline)) spatial_decline <- NA_real_

    cat_a_spatial <- if (!is.na(spatial_decline) && spatial_decline < 0) get_a_cat(spatial_decline) else "LC"
    cat_a_pop <- "LC"
    if (pop_declining) cat_a_pop <- get_a_cat(pop_decline)

    if (get_rank(cat_a_pop) >= get_rank(cat_a_spatial) && cat_a_pop != "LC") {
      cat_A <- cat_a_pop; a_type <- a_label; a_basis <- "b"
    } else if (cat_a_spatial != "LC") {
      cat_A <- cat_a_spatial; a_type <- a_label; a_basis <- "c"
    }

    if (cat_A == "NT") {
      nt_reasons <- c(nt_reasons, paste0("nearly meets ", a_label, a_basis, " (20-29% reduction)"))
      code_A <- ""
    } else if (cat_A != "LC") {
      code_A <- paste0(a_label, a_basis)
    }
  }

  # --- 7. EVALUATE C CRITERIA ---
  cat_C <- "LC"; code_C <- ""
  c1_flag <- FALSE; c2_ai_flag <- FALSE; c2_aii_flag <- FALSE; c2_b_flag <- FALSE

  # C1 reference periods differ by category: 25% in 3 years or 1 generation
  # (CR), 20% in 5 years or 2 generations (EN), 10% in 10 years or 3
  # generations (VU) - whichever is longer in each case. The observed decline
  # is measured over pop_decline_period years, so it is re-projected onto each
  # category's own period at a constant proportional rate (Guidelines 4.6:
  # "Rates of continuing decline ... may be estimated from data over shorter
  # time frames"). The previous code compared one 10-year rate against all
  # three thresholds, so a 25% decline over a decade was read as CR.
  gl <- if (is.na(generation_length) || generation_length <= 0) 1 else generation_length
  decline_over <- function(period_years) {
    if (is.na(pop_decline) || pop_decline >= 0) return(NA_real_)
    ratio <- 1 + pop_decline / 100
    if (ratio <= 0) return(100)
    annual <- ratio^(1 / pop_decline_period)
    (1 - annual^period_years) * 100
  }

  if (evaluate_pop && !is.na(total_mature)) {
    evaluate_c_level <- function(thresh_pop, thresh_c1_decline, c1_period,
                                 thresh_subpop, thresh_prop) {
      if (total_mature < thresh_pop) {
        obs_decline <- decline_over(c1_period)
        if (!is.na(obs_decline) && obs_decline >= thresh_c1_decline) {
          return(list(met = TRUE, type = "1", near = FALSE))
        }
        if (pop_declining) {
          is_ai  <- (!is.na(max_subpop) && max_subpop <= thresh_subpop)
          is_aii <- (!is.na(prop_largest) && prop_largest >= thresh_prop)
          is_b   <- has_fluct
          if (is_ai || is_aii || is_b) {
            code_parts <- ""
            if (is_ai) code_parts <- paste0(code_parts, "a(i)")
            if (is_aii) code_parts <- paste0(code_parts, if (is_ai) "," else "", "a(ii)")
            if (is_b) code_parts <- paste0(code_parts, "b")
            return(list(met = TRUE, type = paste0("2", code_parts), near = FALSE,
                        flags = c(ai = is_ai, aii = is_aii, b = is_b)))
          }
        }
        # Guidelines 10.1 gives "about 1,500 mature individuals" as NT and
        # "more than 2,000 mature individuals" as not NT, so proximity is
        # judged against twice the D1 threshold - not against the full
        # criterion C ceiling of 10,000, which made almost every counted
        # taxon Near Threatened.
        return(list(met = FALSE, near = (total_mature < 2000)))
      }
      return(list(met = FALSE, near = FALSE))
    }

    # thresh_pop, C1 %, C1 period, C2a(i) subpop cap, C2a(ii) proportion
    c_cr <- evaluate_c_level(250,   25, max(3,  1 * gl), 50,   0.90)
    c_en <- evaluate_c_level(2500,  20, max(5,  2 * gl), 250,  0.95)
    c_vu <- evaluate_c_level(10000, 10, max(10, 3 * gl), 1000, 1.00)

    apply_c <- function(res, category) {
      cat_C <<- category; code_C <<- paste0("C", res$type)
      if (identical(res$type, "1")) c1_flag <<- TRUE
      if (!is.null(res$flags)) {
        c2_ai_flag <<- unname(res$flags["ai"])
        c2_aii_flag <<- unname(res$flags["aii"])
        c2_b_flag <<- unname(res$flags["b"])
      }
    }

    if (c_cr$met) apply_c(c_cr, "CR")
    else if (c_en$met) apply_c(c_en, "EN")
    else if (c_vu$met) apply_c(c_vu, "VU")
    else if (isTRUE(c_vu$near)) {
      cat_C <- "NT"
      nt_reasons <- c(nt_reasons, "small population (<2,000 mature individuals)")
    }
  }

  # --- 8. AGGREGATE RESULTS ---
  final_cat <- "LC"; final_crit <- character(0)

  update_cat <- function(new_cat, new_code) {
    if (get_rank(new_cat) > get_rank(final_cat)) {
      final_cat <<- new_cat
      final_crit <<- if (nzchar(new_code)) new_code else character(0)
    } else if (get_rank(new_cat) == get_rank(final_cat) && new_cat != "LC" && nzchar(new_code)) {
      final_crit <<- c(final_crit, new_code)
    }
  }

  update_cat(cat_A, code_A)
  update_cat(b1_res$cat, b1_res$code)
  update_cat(b2_res$cat, b2_res$code)
  update_cat(cat_C, code_C)

  # D1 (very small population)
  d1_active <- FALSE
  if (evaluate_pop && !is.na(total_mature) && total_mature > 0) {
    if (total_mature < 50) cat_d1 <- "CR"
    else if (total_mature < 250) cat_d1 <- "EN"
    else if (total_mature < 1000) cat_d1 <- "VU"
    else cat_d1 <- "LC"

    if (cat_d1 != "LC") {
      if (get_rank(cat_d1) > get_rank(final_cat)) {
        final_cat <- cat_d1; final_crit <- "D1"; d1_active <- TRUE
      } else if (get_rank(cat_d1) == get_rank(final_cat)) {
        final_crit <- c(final_crit, "D1"); d1_active <- TRUE
      }
    }
  }

  # D2 (very restricted population). Guidelines section 8: "simply meeting the
  # suggested (or any other) threshold for AOO or number of locations is not
  # sufficient. It is necessary that this restriction makes the species
  # capable of becoming CR or EX within a very short time, because of the
  # effects of human activities or stochastic events." Without an
  # expert-confirmed plausible threat the taxon is only flagged as a
  # candidate; its category is not raised.
  d2_candidate <- is_extant &&
    ((!is.na(aoo_val) && aoo_val < 20) || (!is.na(locs_val) && locs_val <= 5))
  d2_active <- FALSE
  if (d2_candidate && is_true(d2_threat)) {
    if (get_rank(final_cat) < get_rank("VU")) {
      final_cat <- "VU"; final_crit <- "D2"; d2_active <- TRUE
    } else if (final_cat == "VU") {
      final_crit <- c(final_crit, "D2"); d2_active <- TRUE
    }
  }

  # *** CATCH-ALL: IF NOT EXTANT (AOO=0) AND NOT RE, THEN DD ***
  if (!is_extant && final_cat == "LC") {
    final_cat <- "DD"
    final_crit <- "Absent in recent window"
  }

  if (final_cat == "NT") {
    final_crit <- unique(nt_reasons)
  }
  # Surface an unconfirmed D2 candidate wherever it has not been applied, so
  # the assessor sees the restriction even when the category was set by
  # another criterion.
  if (d2_candidate && !d2_active && final_cat %in% c("LC", "NT")) {
    final_crit <- c(final_crit, "D2 candidate - confirm plausible threat")
  }

  crit_string <- paste(unique(final_crit), collapse = "; ")
  if (crit_string == "") crit_string <- "None"

  list(
    result = data.frame(
      Species = species, Category = final_cat, Criteria = crit_string,
      EOO_km2 = round(eoo_val, 2), AOO_km2 = round(aoo_val, 2),
      Locations = locs_val,
      Trend_Perc = round(aoo_trend_val, 1), stringsAsFactors = FALSE
    ),
    details = list(
      a_type = a_type, a_basis = a_basis,
      b_indices = b_indices, c_indices = c_indices_b, loc_flag = loc_flag,
      c1 = c1_flag, c2_ai = c2_ai_flag, c2_aii = c2_aii_flag, c2_b = c2_b_flag,
      d1_flag = d1_active, d2_flag = d2_active, d2_candidate = d2_candidate,
      re_screened = FALSE, nt_reasons = unique(nt_reasons)
    )
  )
}

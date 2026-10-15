#' Evaluate Criterion A (Population Reduction) on Its Own
#'
#' Deprecated: criterion A is evaluated with the others by [assess_criteria()].
#' Kept for existing scripts; uses the engine's thresholds.
#'
#' @param trend_res List with `percent_change` (negative = decline), e.g.
#'   from [calculate_trend()].
#' @param a_criteria Character. `"A1"` uses the 90/70/50 thresholds (causes
#'   reversible, understood and ceased); anything else uses the A2/A3/A4
#'   thresholds of 80/50/30 (Criteria v3.1 V A).
#' @param nt_margin Numeric, per cent. *Convention*: see
#'   [ndopred_config()].
#' @return `"CR"`, `"EN"`, `"VU"`, `"NT"` or `"LC"`; `NA` when the change is
#'   unknown. (It used to return `"DD"`, but a criterion cannot be Data
#'   Deficient: DD is a decision about the whole assessment, Guidelines
#'   §10.4.)
#' @seealso [assess_criteria()]
#' @references IUCN (2012), criterion A; Guidelines v16 section 5.
#' @examples
#' assess_criterion_a(list(percent_change = -85))          # "CR" under A2
#' assess_criterion_a(list(percent_change = -85), "A1")     # "EN" under A1
#' assess_criterion_a(list(percent_change = -22))          # "NT": 20-29%
#' assess_criterion_a(list(percent_change = 15))           # increase -> LC
#' assess_criterion_a(list(percent_change = NA))           # NA: unknown
#' @export
assess_criterion_a <- function(trend_res, a_criteria = "A2", nt_margin = 20) {
  change <- suppressWarnings(as.numeric(trend_res$percent_change)[1])
  if (length(change) == 0 || is.na(change)) return(NA_character_)
  decline <- -change
  if (decline <= 0) return("LC")
  type <- if (identical(a_criteria, "A1")) "A1" else "A2"
  hit <- .LEVELS[decline >= .A_THRESH[[type]][.LEVELS]]
  if (length(hit)) return(hit[1])
  if (decline >= nt_margin) "NT" else "LC"
}

#' Evaluate Criterion D2 (Restricted AOO or Locations) on Its Own
#'
#' Deprecated: D2 is evaluated with the other criteria by
#' [assess_criteria()]. Kept for existing scripts.
#'
#' Guidelines v16 §8: the AOO and location figures under D2 are indicators,
#' not thresholds, and "simply meeting the suggested (or any other)
#' threshold for AOO or number of locations is not sufficient". D2 also
#' needs a specified plausible threat able to drive the taxon to CR or EX in
#' a very short time.
#'
#' @param aoo_km2 Numeric AOO (2 x 2 km).
#' @param locations Numeric location count.
#' @param plausible_threat A description of the threat (character). `FALSE`
#'   records that there is none. `NA` (default) is unknown. A bare `TRUE` is
#'   not a specified threat and counts as unknown.
#' @return `"VU"` if D2 is met, `"LC"` if it is not, `NA` if the restriction
#'   is present but the threat is unknown.
#' @seealso [assess_criteria()]
#' @references IUCN Standards and Petitions Committee (2024), section 8.
#' @examples
#' # A small range alone is not enough: the answer is unknown...
#' assess_criterion_d2(aoo_km2 = 10, locations = 3)
#'
#' # ...a small range plus a specified threat is VU D2...
#' assess_criterion_d2(aoo_km2 = 10, locations = 3,
#'                     plausible_threat = "drainage of the only fen")
#'
#' # ...and with no plausible threat it is not met.
#' assess_criterion_d2(aoo_km2 = 10, locations = 3, plausible_threat = FALSE)
#' @export
assess_criterion_d2 <- function(aoo_km2, locations, plausible_threat = NA) {
  restricted <- (!is.na(aoo_km2) && aoo_km2 < 20) ||
    (!is.na(locations) && locations <= 5)
  if (!restricted) return("LC")
  if (isTRUE(plausible_threat)) {
    warning("`plausible_threat = TRUE` does not specify a threat (Guidelines 8); ",
            "describe it. Treated as unknown.", call. = FALSE)
  }
  threat <- .threat_state(plausible_threat)
  if (is.na(threat)) return(NA_character_)
  if (threat) "VU" else "LC"
}

#' Run the Full Assessment for a Single Species
#'
#' Downloads NDOP records for one taxon and runs the shared pipeline
#' ([assess_taxon()]): quality control, spatial metrics for the current
#' window, the trend against the comparison window, optional abundance
#' metrics, and the rule engine.
#'
#' @param sp Character. Species scientific name.
#' @param recent_start,recent_end Numeric. The current window (criterion B).
#'   `recent_end` is also the reference year unless `reference_year` is
#'   passed in `...`.
#' @param comp_start,comp_end Numeric or `NULL`. The comparison window for
#'   the trend. `NULL` moves the current window back by the criterion A
#'   period: 10 years, or three generations when longer (see
#'   [ndopred_config()]).
#' @param group Optional taxonomic group, used for the generation length
#'   when the records carry no `KAT_TAX`.
#' @param evaluate_pop Logical. Use NDOP counts as indications for A(b),
#'   B(b)(v), B(c)(iv), C and D.
#' @param snapshot_dir Optional folder to store the NDOP download in (see
#'   [ndop_snapshot()], [default_snapshot_dir()]).
#' @param snapshot Optional stored snapshot (object or path) to assess
#'   instead of downloading.
#' @param ... Expert evidence (`evidence`: an evidence table or file, see
#'   [evidence]; or the older loose arguments `d2_threat`, `threat_locations`,
#'   `severely_fragmented`, `habitat_decline`, `a_criteria`) and
#'   configuration (`generation_length`, `decline_tolerance`,
#'   `re_threshold_years`, `reference_year`); see [summarize_assessment()]
#'   and [ndopred_config()]. Without `generation_length`, the value for the
#'   taxon's NDOP group comes from [generation_lengths()].
#' @return A one-row data frame: the columns of [summarize_assessment()]'s
#'   `result`, plus `Reference_Year` and `issue_*` reason codes.
#' @seealso [batch_assess()] for many taxa, [run_app()] for the interactive
#'   expert workflow, [assess_taxon()] for the pipeline.
#' @examples
#' \dontrun{
#' # AOPK windows: 2010-2026 against 2000-2016 (generation length from the
#' # group table; three generations over 10 years move the comparison back).
#' assess_species("Lucanus cervus", recent_start = 2010, recent_end = 2026)
#'
#' # With the assessor's evidence on the threat and locations.
#' assess_species("Bombina bombina",
#'                recent_start = 2016, recent_end = 2025,
#'                comp_start = 2006, comp_end = 2015,
#'                d2_threat = "drainage of breeding ponds", threat_locations = 8)
#' }
#' @export
assess_species <- function(sp, recent_start, recent_end, comp_start = NULL, comp_end = NULL,
                           evaluate_pop = FALSE, snapshot_dir = NULL, snapshot = NULL,
                           group = NA_character_, ...) {
  args <- list(...)
  if (is.character(args$evidence) && length(args$evidence) == 1) args$evidence <- read_evidence(args$evidence)
  occ <- get_assessment_data(sp, snapshot = snapshot, snapshot_dir = snapshot_dir)
  if (is.null(args$generation_length)) {
    gl <- .taxon_generation_length(occ, group)
    args$generation_length <- gl$years
    args$generation_length_source <- gl$source
  }
  config <- .config_from_args(
    args,
    reference_year = if (is.null(args$reference_year)) recent_end else args$reference_year,
    recent_start = recent_start, recent_end = recent_end,
    comp_start = comp_start, comp_end = comp_end
  )
  assess_taxon(occ, sp, config, evaluate_pop = evaluate_pop,
               evidence = .evidence_from_args(args))$result
}

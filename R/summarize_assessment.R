#' Summarize an Assessment from Pre-computed Metrics
#'
#' Runs the rule engine ([assess_criteria()], [categorise()]) on metrics
#' computed elsewhere and returns the proposal in ndopred's established
#' shape. Kept for existing scripts; new code should use [assess_taxon()],
#' or [assessment_inputs()] with [assess_criteria()] and [categorise()].
#'
#' @section Evidence and indications:
#' Metrics computed from NDOP records are *indications*: the cluster count
#' passed as `locations`, the AOO/EOO changes in `trend`, and everything in
#' `pop_metrics`. They may support the proposed category, and every one that
#' does is listed in `Needs_Evidence`. Expert judgements are *evidence*:
#' `severely_fragmented`, `habitat_decline`, `d2_threat`, `threat_locations`
#' and anything in `evidence`. Their defaults are `NA` (unknown), never
#' `FALSE`.
#'
#' @param species Character string. Species name.
#' @param eoo List containing `area_km2` (EOO of the current window).
#' @param aoo List containing `area_km2` (AOO of the current window, 2 x 2 km).
#' @param trend List containing `percent_change` (change in AOO, per cent)
#'   and optionally `eoo_percent_change`.
#' @param locations Numeric. Number of 5 km clusters, used as an
#'   *indication* of the number of locations.
#' @param pop_metrics List from [calculate_pop_metrics()].
#' @param reference_year Numeric. The assessment year (required; see
#'   [ndopred_config()]).
#' @param evaluate_pop Logical. Whether NDOP counts may be used at all, as
#'   indications for A(b), B(b)(v), B(c)(iv), C and D.
#' @param year_last Numeric. The most recent year observed in the dataset.
#' @param n_records Numeric. Total number of records in the dataset.
#' @param a_criteria Character vector of A subcriteria judged to apply.
#'   `"A1"` uses the 90/70/50 thresholds and needs the causes to be
#'   reversible, understood and ceased (Guidelines §5); default `"A2"`.
#' @param eoo_trend Numeric. Per cent change in EOO, if measured separately.
#' @param locations_trend Numeric. Per cent change in the cluster count.
#' @param severely_fragmented Logical evidence for B(a) (Guidelines §4.8).
#' @param habitat_decline Logical evidence for B(b)(iii) (Guidelines §4.6).
#'   NDOP records carry no habitat information, so this is never inferred.
#' @param d2_threat The most serious plausible threat, as a description
#'   (character). `FALSE` records that there is no plausible threat, so the
#'   location subcriteria and D2 are not met (Guidelines §4.11, §8). A bare
#'   `TRUE` is not a specified threat and is treated as unknown.
#' @param threat_locations Numeric. Number of threat-defined locations
#'   (evidence, Guidelines §4.11).
#' @param evidence List of further expert evidence; see
#'   [assessment_inputs()].
#' @param generation_length Numeric. Generation length in years.
#' @param decline_tolerance Numeric, per cent. See [ndopred_config()].
#' @param re_threshold_years Numeric. Years without records after which a
#'   taxon with no current records is an RE candidate (a convention; see
#'   [ndopred_config()]).
#' @return A list of three elements:
#' \describe{
#'   \item{`result`}{A one-row data frame: `Species`, `Category` (the
#'     proposed category), `Criteria` (established format), `EOO_km2`,
#'     `AOO_km2`, `Locations`, `Trend_Perc`, `Criteria_IUCN` (Annex 2
#'     notation), `Support`, `Needs_Evidence`, `Category_Range`, `Flags`.}
#'   \item{`details`}{Flags used by the apps: `a_type`, `a_basis`,
#'     `b_indices`, `c_indices`, `loc_flag`, `c1`, `c2_ai`, `c2_aii`,
#'     `c2_b`, `d1_flag`, `d2_flag`, `d2_candidate`, `re_screened`,
#'     `nt_reasons`.}
#'   \item{`assessment`}{The [assess_criteria()] object, including the
#'     evidence table, and `proposal`, the [categorise()] object.}
#' }
#' @seealso [assess_taxon()] for the whole pipeline from records.
#' @references
#' IUCN (2012), *Categories and Criteria v3.1*. IUCN Standards and Petitions
#' Committee (2024), *Guidelines* v16.
#' @examples
#' res <- summarize_assessment(
#'   species     = "Example species",
#'   eoo         = list(area_km2 = 4000),
#'   aoo         = list(area_km2 = 400),
#'   trend       = list(percent_change = -30, eoo_percent_change = -25),
#'   locations   = 4,
#'   pop_metrics = list(),
#'   reference_year = 2025,
#'   evaluate_pop = FALSE,
#'   year_last   = 2025,
#'   n_records   = 120
#' )
#' res$result$Category        # "EN", proposed from indications
#' res$result$Criteria_IUCN   # "A2c; B1ab(i,ii)+2ab(i,ii)"
#' res$result$Needs_Evidence  # what would turn the indications into evidence
#'
#' # With the assessor's evidence the same taxon needs less:
#' res2 <- summarize_assessment(
#'   species = "Example species", eoo = list(area_km2 = 4000),
#'   aoo = list(area_km2 = 400), trend = list(percent_change = -30),
#'   locations = 4, pop_metrics = list(), reference_year = 2025,
#'   evaluate_pop = FALSE, year_last = 2025, n_records = 120,
#'   d2_threat = "drainage", threat_locations = 3, habitat_decline = TRUE
#' )
#' res2$result$Needs_Evidence
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics,
                                 reference_year,
                                 evaluate_pop = TRUE, year_last = NA, n_records = NA,
                                 a_criteria = NULL,
                                 eoo_trend = NULL, locations_trend = NULL,
                                 severely_fragmented = NA,
                                 habitat_decline = NA,
                                 d2_threat = NA,
                                 threat_locations = NA,
                                 evidence = list(),
                                 generation_length = 1,
                                 decline_tolerance = 5,
                                 re_threshold_years = 50) {
  cfg <- ndopred_config(reference_year, generation_length = generation_length,
                        decline_tolerance = decline_tolerance,
                        re_candidate_years = re_threshold_years)
  num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) == 0) NA_real_ else v[1]
  }
  aoo_val <- num(aoo$area_km2)
  # Guidelines 4.9: EOO is never smaller than AOO, and falls back to AOO
  # when a hull cannot be built.
  eoo_val <- reconcile_eoo_aoo(num(eoo$area_km2), aoo_val)
  aoo_trend <- num(trend$percent_change)
  eoo_change <- if (!is.null(eoo_trend)) num(eoo_trend) else num(trend$eoo_percent_change)

  ev <- utils::modifyList(list(
    reduction_types = if (is.null(a_criteria)) "A2" else a_criteria,
    severely_fragmented = severely_fragmented,
    threat = d2_threat,
    threat_locations = threat_locations
  ), evidence[setdiff(names(evidence), "decline")])
  # habitat_decline is B(b)(iii); an explicit entry in evidence$decline wins.
  ev$decline <- if (is.null(evidence$decline)) logical(0) else evidence$decline
  if (!"iii" %in% names(ev$decline)) ev$decline <- c(ev$decline, iii = as.logical(habitat_decline)[1])
  if (isTRUE(d2_threat)) {
    warning("`d2_threat = TRUE` does not specify a threat (Guidelines 8); describe it, ",
            "e.g. d2_threat = \"drainage of the only site\". Treated as unknown.", call. = FALSE)
  }

  inputs <- .inputs_from_metrics(
    eoo_km2 = eoo_val, aoo_km2 = aoo_val, n_records = num(n_records),
    n_current = NA, year_last = num(year_last),
    aoo_change = aoo_trend, eoo_change = eoo_change,
    locs_change = num(locations_trend), clusters = num(locations),
    pop_metrics = pop_metrics, evaluate_pop = evaluate_pop,
    tolerance = cfg$conventions$decline_tolerance, evidence = ev
  )
  assessment <- assess_criteria(inputs, cfg, evaluate_pop = evaluate_pop)
  proposal <- categorise(assessment)

  list(
    result = .result_row(species, proposal, eoo_val, aoo_val, num(locations), aoo_trend),
    details = .legacy_details(assessment, proposal),
    assessment = c(assessment, list(proposal = proposal))
  )
}

# The flag list both apps have always read from summarize_assessment().
.legacy_details <- function(assessment, proposal) {
  W <- assessment$scenarios$working
  inp <- assessment$inputs
  resolved <- function(ev, ind) {
    k <- names(ev)
    keep <- vapply(k, function(i) .resolve(.cond(ev[[i]], ind[[i]]), "working"), logical(1))
    k[keep]
  }
  in_parts <- function(p, code) !is.null(proposal$parts[[p]]) && code %in% proposal$parts[[p]]$legacy
  clusters <- inp$indication$clusters
  tl <- suppressWarnings(as.numeric(inp$evidence$threat_locations))
  list(
    a_type = if (!is.null(proposal$parts$A)) W$A$types else character(0),
    a_basis = if (!is.null(proposal$parts$A)) W$A$basis else character(0),
    b_indices = resolved(inp$evidence$decline, inp$indication$decline),
    c_indices = resolved(inp$evidence$fluctuation, inp$indication$fluctuation),
    loc_flag = (!is.na(tl) && tl <= 10) || (!is.na(clusters) && clusters <= 10),
    c1 = W$C$c1, c2_ai = W$C$ai, c2_aii = W$C$aii, c2_b = W$C$b,
    d1_flag = in_parts("D", "D1"),
    d2_flag = in_parts("D", "D2"),
    d2_candidate = W$D2$candidate,
    re_screened = isTRUE(proposal$re_candidate),
    nt_reasons = proposal$nt_reasons,
    support = proposal$support,
    needs = proposal$needs,
    range = proposal$range,
    flags = proposal$flags
  )
}

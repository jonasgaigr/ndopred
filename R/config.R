#' Assessment Run Configuration
#'
#' Everything an assessment run depends on, in one object that is stored with
#' every output, so that a bulk export or a species assessment can be
#' regenerated exactly. Nothing in the rule engine reads the system clock:
#' the reference year is always explicit.
#'
#' @section Conventions:
#' Arguments documented as a *convention* are not IUCN rules. They are
#' choices for the national list where the Guidelines leave a judgement
#' open, and outputs label them as such. Change them here, never in code.
#'
#' @param reference_year Numeric. The assessment year. The current window
#'   ends in this year.
#' @param window Numeric. *Convention (AOPK, decision D5).* Length in years
#'   of the current window, which ends in `reference_year`: EOO, AOO and
#'   locations for criterion B, and the recent side of the trend. Guidelines
#'   v16 §4.9: EOO and AOO describe the *current* distribution. The default
#'   of 17 gives 2010-2026 for reference year 2026.
#' @param trend_shift Numeric. *Convention (AOPK, decision D5).* The
#'   comparison window for the trend is the current window moved this many
#'   years back: 2000-2016 against 2010-2026 by default. The windows overlap
#'   when `trend_shift < window`; their midpoints are `trend_shift` years
#'   apart, matching the 10-year period of criterion A (Criteria v3.1 A2).
#'   The shared years damp the measured change (see the vignette).
#' @param recent_start,recent_end,comp_start,comp_end Numeric or `NULL`.
#'   Explicit window bounds; override the windows derived from
#'   `reference_year`, `window` and `trend_shift`. The trend always compares
#'   the comparison window with the current window.
#' @param generation_length Numeric. Generation length in years (Criteria
#'   v3.1 III.4; Guidelines §4.4). Scales the C1 periods.
#' @param decline_tolerance Numeric, per cent. *Convention.* A measured
#'   change smaller than this is not read as an indication of continuing
#'   decline, because a trend resampled from opportunistic records carries
#'   noise (Guidelines §4.9, §4.10.8). Criteria B and C2 accept a decline "at
#'   any rate" (§4.6): set 0 for the literal reading.
#' @param nt_margin_a Numeric, per cent. *Convention.* A reduction from which
#'   a taxon not meeting criterion A is proposed as NT. Guidelines §10.1 gives
#'   "an estimated 20-25% in the last three generations" as an NT example.
#' @param nt_margin_c Numeric. *Convention.* Mature individuals below which a
#'   taxon not meeting criteria C/D is proposed as NT. Guidelines §10.1 gives
#'   "about 1,500" as NT and "more than 2,000" as not NT.
#' @param re_candidate_years Numeric. *Convention.* Years since the last
#'   record after which a taxon with no current records is proposed as an
#'   RE candidate. Regional Guidelines v4.0 III.14 leaves any time limit to
#'   the regional Red List authority; RE itself needs documented surveys.
#' @param d2_aoo_km2,d2_locations Numeric. The indicative restriction of VU
#'   D2: AOO "typically less than 20 km2" or "typically five or fewer"
#'   locations (Criteria v3.1 V, VU D2). Guidelines §8: indicators, not
#'   strict thresholds.
#' @return An object of class `ndopred_config`.
#' @seealso [assess_criteria()], [categorise()], [assess_taxon()]
#' @examples
#' cfg <- ndopred_config(reference_year = 2025)
#' cfg$windows
#'
#' # AOPK defaults (D5): 2010-2026 against 2000-2016
#' ndopred_config(2026)$windows
#'
#' # Explicit windows, e.g. 2016-2025 against 2006-2015
#' ndopred_config(2025, recent_start = 2016, comp_start = 2006, comp_end = 2015)$windows
#' @export
ndopred_config <- function(reference_year,
                           window = 17,
                           trend_shift = 10,
                           recent_start = NULL, recent_end = NULL,
                           comp_start = NULL, comp_end = NULL,
                           generation_length = 1,
                           decline_tolerance = 5,
                           nt_margin_a = 20,
                           nt_margin_c = 2000,
                           re_candidate_years = 50,
                           d2_aoo_km2 = 20,
                           d2_locations = 5) {
  if (missing(reference_year) || length(reference_year) != 1 ||
      is.na(suppressWarnings(as.numeric(reference_year)))) {
    stop("`reference_year` is required: ndopred no longer reads the system ",
         "clock, so every run states its assessment year.", call. = FALSE)
  }
  reference_year <- as.numeric(reference_year)
  window <- max(1, as.numeric(window))
  trend_shift <- max(1, as.numeric(trend_shift))
  gl <- suppressWarnings(as.numeric(generation_length))
  if (length(gl) != 1 || is.na(gl) || gl <= 0) gl <- 1

  if (is.null(recent_end)) recent_end <- reference_year
  if (is.null(recent_start)) recent_start <- recent_end - window + 1
  # The trend compares the comparison window with the current window; by
  # default the comparison window is the current one moved `trend_shift`
  # years back (AOPK, D5).
  if (is.null(comp_end)) comp_end <- recent_end - trend_shift
  if (is.null(comp_start)) comp_start <- recent_start - trend_shift
  if (comp_start > comp_end || recent_start > recent_end) {
    stop("A window starts after it ends: current ", recent_start, "-", recent_end,
         ", comparison ", comp_start, "-", comp_end, ".", call. = FALSE)
  }

  structure(
    list(
      reference_year = reference_year,
      windows = list(
        recent_start = recent_start, recent_end = recent_end,
        trend_start = recent_start, trend_end = recent_end,
        comp_start = comp_start, comp_end = comp_end
      ),
      generation_length = gl,
      # AOPK, decision D7: the proposal is the best estimate, the whole
      # plausible range is reported (dispute tolerance 0, Guidelines §3.2.3)
      # and a range on both sides of the threatened threshold is flagged.
      attitude = list(
        proposal = "best estimate",
        dispute_tolerance = 0,
        uncertain_when = "plausible range includes a non-threatened and a threatened category"
      ),
      conventions = list(
        decline_tolerance = abs(decline_tolerance),
        nt_margin_a = nt_margin_a,
        nt_margin_c = nt_margin_c,
        re_candidate_years = re_candidate_years,
        d2_aoo_km2 = d2_aoo_km2,
        d2_locations = d2_locations
      ),
      versions = list(
        ndopred = as.character(utils::packageVersion("ndopred")),
        criteria = "IUCN Red List Categories and Criteria v3.1, 2nd ed. (2012)",
        guidelines = "Guidelines for Using the IUCN Red List Categories and Criteria v16 (2024)",
        regional = "Guidelines for Application of IUCN Red List Criteria at Regional and National Levels v4.0 (2012)"
      )
    ),
    class = "ndopred_config"
  )
}

#' @export
print.ndopred_config <- function(x, ...) {
  w <- x$windows
  cat("<ndopred_config> reference year", x$reference_year, "\n")
  cat("  current window (B):", w$recent_start, "-", w$recent_end, "\n")
  cat("  trend:", w$comp_start, "-", w$comp_end, "vs", w$trend_start, "-", w$trend_end, "\n")
  cat("  generation length:", x$generation_length, "years\n")
  cat("  proposal:", x$attitude$proposal, "- flagged uncertain when the", x$attitude$uncertain_when, "\n")
  cat("  conventions:", paste(names(x$conventions), unlist(x$conventions), sep = " = ", collapse = ", "), "\n")
  invisible(x)
}

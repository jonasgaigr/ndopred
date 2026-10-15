# The rule engine: one implementation of criteria A-E, used by every entry
# point (assess_taxon(), batch_assess(), both Shiny apps and the deprecated
# summarize_assessment()). No other file may hold thresholds or category
# logic (gap_analysis.md, item 1).
#
# Conditions carry two layers (gap_analysis.md section 8, decision D11):
#   ev  - evidence: TRUE / FALSE / NA from an expert or an evidenced
#         estimate. NA means UNKNOWN and is never read as FALSE.
#   ind - indication: TRUE / FALSE / NA from a record-based proxy (cluster
#         count, raw window-to-window change, NDOP locality counts).
# Every criterion is evaluated in two scenarios:
#   working - the proposal and the low end of the plausible range:
#             evidence where it exists, else the indication;
#   high    - the high end: evidence where it exists, else assumed TRUE
#             (only where the taxon's own data meet the criterion's gate).
# Whether a condition rests on evidence or on an indication is reported
# separately (support, needs), not through the range. Parameter min/max
# bounds widen the range from Phase 2 on (Guidelines 3.2).
# .resolve() is the only place where an unknown is read as FALSE, and only
# for the working scenario.

# ------------------------------------------------------------- thresholds

.CATS <- c("LC", "NT", "VU", "EN", "CR")
.LEVELS <- c("CR", "EN", "VU")

# Criteria v3.1 V, criterion A: A1 90/70/50 %; A2, A3, A4 80/50/30 %.
.A_THRESH <- list(
  A1 = c(CR = 90, EN = 70, VU = 50),
  A2 = c(CR = 80, EN = 50, VU = 30),
  A3 = c(CR = 80, EN = 50, VU = 30),
  A4 = c(CR = 80, EN = 50, VU = 30)
)
# Criteria v3.1 V, B1 extent of occurrence and B2 area of occupancy (km2).
.B_AREA <- list(
  B1 = c(CR = 100, EN = 5000, VU = 20000),
  B2 = c(CR = 10, EN = 500, VU = 2000)
)
# Criteria v3.1 V, B(a): "a single location" / "no more than five" / "no
# more than 10 locations".
.B_LOC <- c(CR = 1, EN = 5, VU = 10)
# Criteria v3.1 V, criterion C: fewer than 250 / 2,500 / 10,000 mature.
.C_POP <- c(CR = 250, EN = 2500, VU = 10000)
# C1: 25 % in 3 years or 1 generation, 20 % in 5 years or 2 generations,
# 10 % in 10 years or 3 generations, whichever is longer, "up to a maximum
# of 100 years in the future".
.C1_DECLINE <- c(CR = 25, EN = 20, VU = 10)
.C1_YEARS <- c(CR = 3, EN = 5, VU = 10)
.C1_GENS <- c(CR = 1, EN = 2, VU = 3)
.C1_CAP <- 100
# C2a(i): no subpopulation larger than 50 / 250 / 1,000; C2a(ii): at least
# 90 % / 95 % / all mature individuals in one subpopulation.
.C2_SUBPOP <- c(CR = 50, EN = 250, VU = 1000)
.C2_SHARE <- c(CR = 0.90, EN = 0.95, VU = 1.00)
# Criteria v3.1 V, criterion D: CR D < 50, EN D < 250, VU D1 < 1,000.
.D_POP <- c(CR = 50, EN = 250, VU = 1000)

.rank <- function(x) match(x, .CATS)
.max_cat <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("LC")
  .CATS[max(.rank(x))]
}

# ------------------------------------------------------- condition helpers

.cond <- function(ev = NA, ind = NA) list(ev = as.logical(ev)[1], ind = as.logical(ind)[1])

.resolve <- function(x, scenario) {
  if (!is.na(x$ev)) return(x$ev)
  switch(scenario,
         working = !is.na(x$ind) && x$ind,
         high = TRUE)
}

.support <- function(x) {
  if (!is.na(x$ev)) "evidence" else if (!is.na(x$ind)) "indication" else "none"
}

# Known to be TRUE (an UNKNOWN is not).
.yes <- function(x) length(x) == 1 && !is.na(x) && x

# Kleene OR: TRUE if any is TRUE, FALSE if all are FALSE, otherwise UNKNOWN.
.k_or <- function(...) {
  v <- as.logical(c(...))
  if (any(!is.na(v) & v)) return(TRUE)
  if (length(v) && all(!is.na(v) & !v)) return(FALSE)
  NA
}

# A quantity from evidence, else from the indication. (`scenario` is kept
# for the parameter bounds of Phase 2.)
.pick <- function(ev, ind, scenario) {
  ev <- suppressWarnings(as.numeric(ev)[1])
  ind <- suppressWarnings(as.numeric(ind)[1])
  if (!is.na(ev)) return(list(value = ev, support = "evidence"))
  if (!is.na(ind)) return(list(value = ind, support = "indication"))
  list(value = NA_real_, support = "none")
}

# Decline over `period` years from a percentage change measured over
# `measured` years, at a constant proportional rate (Guidelines 4.5.1).
.decline_over <- function(change, measured, period) {
  if (is.na(change) || change >= 0) return(NA_real_)
  ratio <- 1 + change / 100
  if (ratio <= 0) return(100)
  (1 - ratio^(period / measured)) * 100
}

# The threat entry: a description (character) means a plausible threat has
# been specified; FALSE means the assessor found no plausible threat; NA is
# unknown. A bare TRUE is not a specified threat (Guidelines 8).
.threat_state <- function(threat) {
  if (is.character(threat) && length(threat) && !is.na(threat[1]) && nzchar(trimws(threat[1]))) return(TRUE)
  if (identical(threat, FALSE)) return(FALSE)
  NA
}

# ------------------------------------------------------------------ inputs

#' Build Inputs for the Rule Engine
#'
#' Collects what the engine needs about one taxon in three layers: measured
#' **data** (EOO, AOO, record counts), **indications** derived from the
#' records (cluster counts, raw window-to-window changes, NDOP counts), and
#' **evidence** supplied by an expert. The proposal uses evidence where it
#' exists and the indication otherwise, and reports which it used
#' (gap analysis, decision D11).
#'
#' @param eoo_km2,aoo_km2 Numeric. EOO and AOO of the current window, with
#'   EOO already reconciled to be at least AOO (Guidelines §4.9). AOO on the
#'   2 x 2 km reference grid (§4.10.3).
#' @param n_records Numeric. Usable records of the taxon (all years).
#' @param n_current Numeric. Records in the current window. `0` means the
#'   taxon has no current records.
#' @param year_last Numeric. Year of the most recent record.
#' @param indication Named list of record-based proxies; see Details.
#' @param evidence Named list of expert evidence; see Details.
#' @details
#' `indication` entries: `reduction` (per cent decline, positive),
#' `reduction_basis` ("b" or "c"), `reduction_source` (text), `clusters`
#' (5 km cluster count), `decline` (named logical vector `i`-`v`, criterion
#' B(b)), `fluctuation` (named logical vector `i`-`iv`, B(c)), `mature`,
#' `mature_decline` (per cent change, negative = decline) and
#' `mature_decline_years`, `largest_subpop`, `prop_largest`.
#'
#' `evidence` entries: the same population entries plus `reduction`,
#' `reduction_types` (e.g. `"A2"`, `c("A2", "A4")`), `reduction_basis`,
#' `severely_fragmented`, `threat` (a description of the most serious
#' plausible threat; `FALSE` if there is none), `threat_locations`,
#' `decline`, `fluctuation` and `e_category`.
#' @return A list of class `ndopred_inputs`.
#' @seealso [assess_criteria()], [categorise()]
#' @examples
#' inp <- assessment_inputs(
#'   eoo_km2 = 1700, aoo_km2 = 84, n_records = 60, n_current = 25,
#'   year_last = 2025,
#'   evidence = list(threat = "drainage of the two remaining fens",
#'                   threat_locations = 2,
#'                   decline = c(iii = TRUE))
#' )
#' categorise(assess_criteria(inp, ndopred_config(2025)))$category   # "EN"
#' @export
assessment_inputs <- function(eoo_km2 = NA, aoo_km2 = NA, n_records = NA,
                              n_current = NA, year_last = NA,
                              indication = list(), evidence = list()) {
  flags_b <- c(i = NA, ii = NA, iii = NA, iv = NA, v = NA)
  flags_c <- c(i = NA, ii = NA, iii = NA, iv = NA)
  merge_flags <- function(default, given) {
    if (is.null(given) || !length(given)) return(default)
    k <- intersect(names(given), names(default))
    default[k] <- as.logical(unlist(given[k]))
    default
  }
  ind <- utils::modifyList(list(
    reduction = NA_real_, reduction_basis = NA_character_,
    reduction_source = NA_character_, clusters = NA_real_,
    mature = NA_real_, mature_decline = NA_real_, mature_decline_years = NA_real_,
    largest_subpop = NA_real_, prop_largest = NA_real_
  ), indication[setdiff(names(indication), c("decline", "fluctuation"))])
  ind$decline <- merge_flags(flags_b, indication$decline)
  ind$fluctuation <- merge_flags(flags_c, indication$fluctuation)

  undescribed <- isTRUE(evidence$threat)
  ev <- utils::modifyList(list(
    reduction = NA_real_, reduction_types = "A2", reduction_basis = NA_character_,
    severely_fragmented = NA, threat = NA, threat_locations = NA_real_,
    mature = NA_real_, mature_decline = NA_real_, mature_decline_years = NA_real_,
    largest_subpop = NA_real_, prop_largest = NA_real_, e_category = NA_character_
  ), evidence[setdiff(names(evidence), c("decline", "fluctuation"))])
  if (undescribed) ev$threat <- NA
  ev$decline <- merge_flags(flags_b, evidence$decline)
  ev$fluctuation <- merge_flags(flags_c, evidence$fluctuation)
  # B(b)(v) and C2 share the continuing decline in mature individuals, and
  # B(c)(iv) and C2b the extreme fluctuation in mature individuals.

  num <- function(x) suppressWarnings(as.numeric(x)[1])
  aoo <- num(aoo_km2)
  n_cur <- num(n_current)
  # TRUE: current records exist; FALSE: none; NA: unknown (AOO not computed).
  extant <- if (!is.na(n_cur)) n_cur > 0 else if (!is.na(aoo)) aoo > 0 else NA
  structure(list(
    eoo_km2 = num(eoo_km2), aoo_km2 = aoo, n_records = num(n_records),
    n_current = n_cur, year_last = num(year_last), extant = extant,
    indication = ind, evidence = ev, threat_undescribed = undescribed
  ), class = "ndopred_inputs")
}

# ------------------------------------------------------------- criterion A

.eval_A <- function(inp, cfg, scenario) {
  ev <- inp$evidence
  ind <- inp$indication
  p <- .pick(ev$reduction, ind$reduction, scenario)
  basis <- if (p$support == "evidence") ev$reduction_basis else ind$reduction_basis
  if (is.na(basis) || !nzchar(basis)) basis <- if (p$support == "evidence") "a" else "c"
  types <- intersect(ev$reduction_types, names(.A_THRESH))
  if (!length(types)) types <- "A2"
  out <- list(level = NA_character_, types = character(0), basis = basis,
              value = p$value, support = p$support,
              source = if (p$support == "indication") ind$reduction_source else "expert evidence")
  if (is.na(p$value)) return(out)
  lv <- vapply(types, function(t) {
    hit <- .LEVELS[p$value >= .A_THRESH[[t]][.LEVELS]]
    if (length(hit)) hit[1] else NA_character_
  }, character(1))
  if (all(is.na(lv))) return(out)
  out$level <- .max_cat(lv)
  out$types <- types[!is.na(lv) & lv == out$level]
  out
}

# ------------------------------------------------------------- criterion B

# Subcriterion (a) at one level: severely fragmented (evidence only) or
# known to exist at no more than x threat-defined locations (evidence), with
# the 5 km cluster count as an indication. Guidelines 4.11: with no
# plausible threat the location subcriteria are not met.
.a_cond <- function(inp, level) {
  ev <- inp$evidence
  no_threat <- identical(.threat_state(ev$threat), FALSE)
  loc_known <- !is.na(suppressWarnings(as.numeric(ev$threat_locations)))
  loc_ev <- if (no_threat) FALSE else if (loc_known) as.numeric(ev$threat_locations) <= .B_LOC[[level]] else NA
  ind <- if (no_threat || loc_known || is.na(inp$indication$clusters)) NA else inp$indication$clusters <= .B_LOC[[level]]
  .cond(.k_or(ev$severely_fragmented, loc_ev), ind)
}

.sub_conds <- function(ev_flags, ind_flags) {
  out <- lapply(names(ev_flags), function(k) .cond(ev_flags[[k]], ind_flags[[k]]))
  stats::setNames(out, names(ev_flags))
}

.eval_B <- function(inp, cfg, scenario) {
  b <- .sub_conds(inp$evidence$decline, inp$indication$decline)
  c <- .sub_conds(inp$evidence$fluctuation, inp$indication$fluctuation)
  res <- list()
  for (sub in c("B1", "B2")) {
    area <- if (sub == "B1") inp$eoo_km2 else inp$aoo_km2
    out <- list(evaluable = .yes(inp$extant) && !is.na(area), area = area, gate = NA_character_,
                level = NA_character_, a = FALSE, b = character(0), c = character(0),
                max_count = 0L, conds = list(b = b, c = c))
    if (!out$evaluable) { res[[sub]] <- out; next }
    gates <- .LEVELS[area < .B_AREA[[sub]][.LEVELS]]
    if (length(gates)) out$gate <- gates[1]
    # Two of (a), (b), (c) at the highest level whose area threshold is met;
    # (a) depends on the level, so lower levels are tried in turn.
    for (L in gates) {
      a <- .a_cond(inp, L)
      a_s <- .resolve(a, scenario)
      b_s <- vapply(b, .resolve, logical(1), scenario = scenario)
      c_s <- vapply(c, .resolve, logical(1), scenario = scenario)
      n <- as.integer(a_s) + any(b_s) + any(c_s)
      out$max_count <- max(out$max_count, n)
      if (n >= 2) {
        out$level <- L
        out$a <- a_s
        out$a_cond <- a
        out$b <- names(b_s)[b_s]
        out$c <- names(c_s)[c_s]
        break
      }
    }
    if (is.na(out$level) && length(gates)) out$a_cond <- .a_cond(inp, gates[1])
    res[[sub]] <- out
  }
  res
}

# ---------------------------------------------------------- criteria C, D

.pop_values <- function(inp, scenario, use_pop) {
  ev <- inp$evidence
  ind <- inp$indication
  from_ind <- function(x) if (use_pop) x else NA
  rate <- .pick(ev$mature_decline, from_ind(ind$mature_decline), scenario)
  years <- if (rate$support == "evidence") ev$mature_decline_years else ind$mature_decline_years
  years <- suppressWarnings(as.numeric(years)[1])
  if (is.na(years) || years <= 0) years <- 10
  list(
    N = .pick(ev$mature, from_ind(ind$mature), scenario),
    rate = rate, years = years,
    subpop = .pick(ev$largest_subpop, from_ind(ind$largest_subpop), scenario),
    share = .pick(ev$prop_largest, from_ind(ind$prop_largest), scenario),
    declining = .cond(ev$decline[["v"]], from_ind(inp$indication$decline[["v"]])),
    fluct = .cond(ev$fluctuation[["iv"]], from_ind(inp$indication$fluctuation[["iv"]]))
  )
}

.eval_C <- function(inp, cfg, scenario, use_pop) {
  pv <- .pop_values(inp, scenario, use_pop)
  out <- c(pv, list(level = NA_character_, c1 = FALSE, c2 = FALSE,
                    ai = FALSE, aii = FALSE, b = FALSE, evaluated = !is.na(pv$N$value)))
  if (!out$evaluated) return(out)
  gl <- cfg$generation_length
  for (L in .LEVELS) {
    if (pv$N$value >= .C_POP[[L]]) next
    period <- min(.C1_CAP, max(.C1_YEARS[[L]], .C1_GENS[[L]] * gl))
    dec_c1 <- .decline_over(pv$rate$value, pv$years, period)
    c1 <- !is.na(dec_c1) && dec_c1 >= .C1_DECLINE[[L]]
    declining <- .resolve(pv$declining, scenario)
    ai <- !is.na(pv$subpop$value) && pv$subpop$value <= .C2_SUBPOP[[L]]
    aii <- !is.na(pv$share$value) && pv$share$value >= .C2_SHARE[[L]]
    b <- .resolve(pv$fluct, scenario)
    c2 <- declining && (ai || aii || b)
    if (c1 || c2) {
      out[c("level", "c1", "c2", "ai", "aii", "b")] <- list(L, c1, c2, c2 && ai, c2 && aii, c2 && b)
      return(out)
    }
  }
  out
}

.eval_D <- function(inp, cfg, scenario, use_pop) {
  N <- .pop_values(inp, scenario, use_pop)$N
  hit <- if (is.na(N$value)) character(0) else .LEVELS[N$value < .D_POP[.LEVELS]]
  list(level = if (length(hit)) hit[1] else NA_character_, N = N)
}

# VU D2: AOO typically < 20 km2 or typically <= 5 locations (Criteria v3.1
# V), AND a specified plausible threat able to drive the taxon to CR or EX
# in a very short time (Guidelines 8). An unknown threat is never assumed in
# the proposal; Guidelines 8 warns that literal use of the figures "results
# in massive over-listing".
.eval_D2 <- function(inp, cfg, scenario) {
  conv <- cfg$conventions
  ev <- inp$evidence
  threat <- .threat_state(ev$threat)
  out <- list(met = FALSE, restricted = FALSE, by_aoo = FALSE, loc = NULL, threat = threat,
              candidate = FALSE)
  if (!.yes(inp$extant)) return(out)
  by_aoo <- !is.na(inp$aoo_km2) && inp$aoo_km2 < conv$d2_aoo_km2
  loc_known <- !is.na(suppressWarnings(as.numeric(ev$threat_locations)))
  loc <- .cond(
    if (identical(threat, FALSE)) FALSE else if (loc_known) as.numeric(ev$threat_locations) <= conv$d2_locations else NA,
    if (identical(threat, FALSE) || loc_known || is.na(inp$indication$clusters)) NA else inp$indication$clusters <= conv$d2_locations
  )
  # The restriction itself is never assumed: an unknown location count
  # without an indication counts as not restricted, in every scenario.
  by_loc <- if (!is.na(loc$ev)) loc$ev else !is.na(loc$ind) && loc$ind
  restricted <- by_aoo || by_loc
  threat_s <- .resolve(.cond(threat, NA), scenario)
  out$restricted <- restricted
  out$by_aoo <- by_aoo
  out$loc <- loc
  out$met <- restricted && threat_s
  out$candidate <- restricted && is.na(threat)
  out
}

.eval_E <- function(inp) {
  e <- inp$evidence$e_category
  list(level = if (!is.null(e) && length(e) && !is.na(e[1]) && e[1] %in% .LEVELS) e[1] else NA_character_)
}

# --------------------------------------------------------------- the engine

#' Evaluate a Taxon Against Criteria A-E
#'
#' The single rule engine of ndopred. Every criterion is evaluated in two
#' scenarios: the *working* one behind the proposal (evidence where it
#' exists, otherwise the indication from the records), and *high*, where
#' every condition without evidence is assumed met wherever the taxon's own
#' data meet the criterion's gate (an area threshold, the D2 restriction, a
#' small population). The two give the plausible range of categories.
#' Pass the result to [categorise()] for the proposed category.
#'
#' @param inputs Object from [assessment_inputs()].
#' @param config Object from [ndopred_config()].
#' @param evaluate_pop Logical. Whether NDOP counts may serve as indications
#'   for criteria A(b), B(b)(v), B(c)(iv), C and D. Expert evidence on
#'   population is used either way.
#' @return An object of class `ndopred_assessment` with the scenarios and a
#'   tidy evidence table (`$table`): one row per criterion x subcriterion x
#'   level, with the value used, threshold, evidence and indication states,
#'   support and the rule's source.
#' @seealso [categorise()], [assessment_inputs()]
#' @references IUCN (2012) Categories and Criteria v3.1, section V; IUCN
#'   Standards and Petitions Committee (2024) Guidelines v16, sections 4-10.
#' @examples
#' inp <- assessment_inputs(eoo_km2 = 4000, aoo_km2 = 400, n_records = 80,
#'                          n_current = 30, year_last = 2025,
#'                          indication = list(clusters = 3, decline = c(ii = TRUE)))
#' a <- assess_criteria(inp, ndopred_config(2025))
#' head(a$table)
#' @export
assess_criteria <- function(inputs, config, evaluate_pop = TRUE) {
  stopifnot(inherits(inputs, "ndopred_inputs"), inherits(config, "ndopred_config"))
  scen <- c("working", "high")
  scenarios <- lapply(scen, function(s) list(
    A = .eval_A(inputs, config, s),
    B = .eval_B(inputs, config, s),
    C = .eval_C(inputs, config, s, evaluate_pop),
    D = .eval_D(inputs, config, s, evaluate_pop),
    D2 = .eval_D2(inputs, config, s),
    E = .eval_E(inputs)
  ))
  names(scenarios) <- scen
  out <- structure(list(inputs = inputs, config = config, evaluate_pop = evaluate_pop,
                        scenarios = scenarios),
                   class = "ndopred_assessment")
  out$table <- .evidence_table(out)
  out
}

.scenario_category <- function(s) {
  .max_cat(c(s$A$level, s$B$B1$level, s$B$B2$level, s$C$level, s$D$level,
             if (s$D2$met) "VU", s$E$level))
}

# ---------------------------------------------------------- evidence table

.evidence_table <- function(assessment) {
  inp <- assessment$inputs
  w <- assessment$scenarios$working
  st <- function(x) if (is.na(x)) "UNKNOWN" else if (x) "TRUE" else "FALSE"
  rows <- list()
  add <- function(criterion, sub, level, value, threshold, evidence, indication, used, support, ref) {
    rows[[length(rows) + 1]] <<- data.frame(
      criterion = criterion, subcriterion = sub, level = level,
      value = as.character(value), threshold = as.character(threshold),
      evidence = evidence, indication = indication, used = used,
      support = support, ref = ref, stringsAsFactors = FALSE
    )
  }
  A <- w$A
  for (t in intersect(inp$evidence$reduction_types, names(.A_THRESH))) for (L in .LEVELS) {
    met <- !is.na(A$value) && A$value >= .A_THRESH[[t]][[L]]
    add("A", t, L, round(A$value, 1), paste0(">= ", .A_THRESH[[t]][[L]], " %"),
        if (A$support == "evidence") st(met) else "UNKNOWN",
        if (A$support == "indication") st(met) else "NA",
        st(if (is.na(A$value)) NA else met), A$support,
        "Criteria v3.1 V A; Guidelines v16 s5")
  }
  for (sub in c("B1", "B2")) {
    B <- w$B[[sub]]
    for (L in .LEVELS) {
      gate <- if (B$evaluable) B$area < .B_AREA[[sub]][[L]] else NA
      add(sub, "area", L, round(B$area, 2), paste0("< ", .B_AREA[[sub]][[L]], " km2"),
          "data", "NA", st(gate), if (B$evaluable) "data" else "none",
          "Criteria v3.1 V B; Guidelines v16 s4.9-4.10")
      if (!isFALSE(gate) && B$evaluable) {
        a <- .a_cond(inp, L)
        add(sub, "a", L, inp$indication$clusters, paste0("<= ", .B_LOC[[L]], " locations or severely fragmented"),
            st(a$ev), st(a$ind), st(.resolve(a, "working")), .support(a),
            "Criteria v3.1 V B(a); Guidelines v16 s4.8, s4.11")
      }
    }
  }
  b <- .sub_conds(inp$evidence$decline, inp$indication$decline)
  for (k in names(b)) add("B", paste0("b(", k, ")"), "any", "", "continuing decline",
                          st(b[[k]]$ev), st(b[[k]]$ind), st(.resolve(b[[k]], "working")),
                          .support(b[[k]]), "Criteria v3.1 V B(b); Guidelines v16 s4.6")
  cc <- .sub_conds(inp$evidence$fluctuation, inp$indication$fluctuation)
  for (k in names(cc)) add("B", paste0("c(", k, ")"), "any", "", "extreme fluctuation",
                           st(cc[[k]]$ev), st(cc[[k]]$ind), st(.resolve(cc[[k]], "working")),
                           .support(cc[[k]]), "Criteria v3.1 V B(c); Guidelines v16 s4.7")
  C <- w$C
  for (L in .LEVELS) add("C", "population", L, C$N$value, paste0("< ", .C_POP[[L]], " mature"),
                         if (C$N$support == "evidence") st(C$N$value < .C_POP[[L]]) else "UNKNOWN",
                         if (C$N$support == "indication") st(C$N$value < .C_POP[[L]]) else "NA",
                         st(if (is.na(C$N$value)) NA else C$N$value < .C_POP[[L]]), C$N$support,
                         "Criteria v3.1 V C; Guidelines v16 s4.3, s7")
  D <- w$D
  for (L in .LEVELS) add("D", if (L == "VU") "D1" else "D", L, D$N$value, paste0("< ", .D_POP[[L]], " mature"),
                         if (D$N$support == "evidence") st(D$N$value < .D_POP[[L]]) else "UNKNOWN",
                         if (D$N$support == "indication") st(D$N$value < .D_POP[[L]]) else "NA",
                         st(if (is.na(D$N$value)) NA else D$N$value < .D_POP[[L]]), D$N$support,
                         "Criteria v3.1 V D; Guidelines v16 s8")
  D2 <- w$D2
  add("D", "D2 restriction", "VU", round(inp$aoo_km2, 2), "AOO < 20 km2 or <= 5 locations (indicative)",
      if (D2$by_aoo) "TRUE" else if (!is.null(D2$loc)) st(D2$loc$ev) else "UNKNOWN",
      if (!is.null(D2$loc)) st(D2$loc$ind) else "NA", st(D2$restricted),
      if (D2$by_aoo) "data" else if (!is.null(D2$loc)) .support(D2$loc) else "none",
      "Criteria v3.1 V D2; Guidelines v16 s8")
  add("D", "D2 threat", "VU", "", "specified plausible threat", st(D2$threat), "NA",
      st(D2$met), if (is.na(D2$threat)) "none" else "evidence", "Guidelines v16 s8, s8.3")
  add("E", "quantitative analysis", "any", w$E$level, "documented analysis",
      if (is.na(w$E$level)) "UNKNOWN" else "TRUE", "NA", st(!is.na(w$E$level)),
      if (is.na(w$E$level)) "none" else "evidence", "Criteria v3.1 V E; Guidelines v16 s9")
  do.call(rbind, rows)
}

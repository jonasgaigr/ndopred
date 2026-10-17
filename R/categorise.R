#' Propose a Category, with Its Criteria, Support and Evidence Gaps
#'
#' Turns an [assess_criteria()] result into the proposal shown in both apps
#' and the bulk export. Bulk mode always proposes a category; what still
#' needs evidence comes alongside it, never instead of it (gap analysis,
#' decisions D10 and D11).
#'
#' @section Rules:
#' * The proposal is the highest category met in the working scenario
#'   (Guidelines §2.3). All criteria met at that category are listed.
#' * A best estimate of LC is proposed as NT when a criterion is nearly met
#'   (`nt_margin_a`, B area threshold with one of three conditions,
#'   `nt_margin_c`; Guidelines §10.1).
#' * Uncertainty never moves the proposal (AOPK, decision D7). A proposal
#'   whose plausible range holds both a non-threatened category (LC, NT) and
#'   a threatened one (VU, EN, CR) is flagged **uncertain**. That is the case
#'   in which Guidelines §10.1 says a taxon "can be classified as NT, unless
#'   the best estimate is VU (or EN)", and §10.4 leaves the choice to the
#'   assessor's risk tolerance.
#' * A range from LC to CR adds a DD flag (Guidelines §10.1, §10.4). DD is
#'   proposed only as a candidate for a taxon with records but none in the
#'   current window and nothing else to go on.
#' * A taxon with no current records whose last record is older than
#'   `re_candidate_years` is proposed as an RE *candidate*, with surveys
#'   listed as needed (Regional Guidelines v4.0 III.14; Guidelines §11.3).
#' * A taxon with no usable records is NE (Guidelines §10.4).
#'
#' @param assessment Object from [assess_criteria()].
#' @return A list of class `ndopred_proposal`:
#' \describe{
#'   \item{`category`}{Proposed category.}
#'   \item{`criteria`}{Criteria behind it in ndopred's established format
#'     (`"B1ab(ii); B2ab(ii)"`, `"D1"`, `"nearly meets ..."`).}
#'   \item{`criteria_iucn`}{The same in IUCN Annex 2 notation
#'     (`"B1ab(ii)+2ab(ii)"`, `"D"` under CR/EN, `"D1+2"`). Empty for
#'     non-threatened proposals.}
#'   \item{`support`}{`"evidence"` when nothing behind the proposal rests on
#'     an indication or an unknown, `"indication"` otherwise, `"candidate"`
#'     for RE/DD candidates, `"none"` for NE.}
#'   \item{`needs`}{Character vector: what still needs evidence, each item
#'     starting with its subcriterion, e.g. `"B(a): ..."`.}
#'   \item{`range`}{Plausible range, e.g. `"LC-EN"`.}
#'   \item{`uncertain`}{`TRUE` when the range holds both a non-threatened
#'     and a threatened category.}
#'   \item{`flags`}{Character vector of flags: uncertainty and candidates.}
#' }
#' @seealso [assess_criteria()]
#' @references IUCN Standards and Petitions Committee (2024) Guidelines v16,
#'   sections 2.3, 3.2, 10.1, 10.3, 10.4, 11; IUCN (2012) Regional
#'   Guidelines v4.0, III.14.
#' @examples
#' # Review taxon Y: EOO 1,700 km2, AOO 84 km2, nothing evidenced.
#' inp <- assessment_inputs(eoo_km2 = 1700, aoo_km2 = 84, n_records = 40,
#'                          n_current = 12, year_last = 2025)
#' p <- categorise(assess_criteria(inp, ndopred_config(2025)))
#' p$category   # "LC": nothing behind B is known
#' p$range      # "LC-EN": unevidenced B conditions could give EN
#' p$uncertain  # TRUE, and p$flags says NT may be chosen (Guidelines 10.1)
#' p$needs
#' @export
categorise <- function(assessment) {
  stopifnot(inherits(assessment, "ndopred_assessment"))
  inp <- assessment$inputs
  cfg <- assessment$config
  conv <- cfg$conventions
  S <- assessment$scenarios
  W <- S$working

  if (!is.na(inp$n_records) && inp$n_records == 0) {
    return(.proposal("NE", "No usable NDOP records", support = "none",
                     needs = "NE: no usable NDOP records - not evaluated (Guidelines 10.4)"))
  }

  cat_w <- .scenario_category(W)
  cat_l <- .scenario_category(S$low)
  cat_h <- .scenario_category(S$high)

  if (!.yes(inp$extant)) {
    gap <- cfg$reference_year - inp$year_last
    if (!is.na(gap) && gap > conv$re_candidate_years) {
      return(.proposal(
        "RE",
        sprintf("Screen: no record for >%s years (last %s) - confirm with surveys",
                conv$re_candidate_years, inp$year_last),
        support = "candidate",
        needs = paste0("RE: documented surveys leaving no reasonable doubt that the taxon ",
                       "has gone (Regional Guidelines III.14; Guidelines 11.3)"),
        flags = "RE candidate", re = TRUE
      ))
    }
    if (.rank(cat_w) < .rank("VU")) {
      unknown <- is.na(inp$extant)
      return(.proposal(
        "DD",
        if (unknown) "Inadequate information to estimate distribution" else "Absent in recent window",
        support = "candidate",
        needs = c(if (unknown) "DD: AOO/EOO could not be computed" else
                    "DD: current occurrence and search effort (no records in the current window)",
                  .needs_a_unknown(W, inp)),
        flags = "DD candidate"
      ))
    }
  }

  # The best estimate, and NT only where a criterion is nearly met. A range
  # alone does not move the proposal: choosing NT or DD on uncertainty is the
  # assessor's call (AOPK, D7; Guidelines §10.1, §10.4).
  proposal <- cat_w
  nt_reasons <- character(0)
  if (proposal == "LC") {
    nt_reasons <- .near_reasons(W, cfg)
    if (length(nt_reasons)) proposal <- "NT"
  }

  parts <- if (.rank(proposal) >= .rank("VU")) .parts_at(W, proposal) else list()
  needs <- .needs(assessment, proposal, parts)

  hi <- .max_cat(c(cat_h, proposal))
  lo <- if (.rank(cat_l) <= .rank(proposal)) cat_l else proposal
  # Uncertain: the plausible range holds a non-threatened and a threatened
  # category, the case Guidelines §10.1 and §10.4 leave to risk tolerance.
  uncertain <- .rank(lo) < .rank("VU") && .rank(hi) >= .rank("VU")
  flags <- character(0)
  if (uncertain) {
    flags <- c(flags, sprintf("Uncertain: plausible range %s-%s%s", lo, hi,
                              switch(proposal,
                                     LC = " - NT may be chosen (Guidelines 10.1)",
                                     NT = " (Guidelines 10.1)",
                                     " (Guidelines 3.2.4)")))
  }
  if (cat_l == "LC" && cat_h == "CR") flags <- c(flags, "DD candidate (plausible range LC-CR, Guidelines 10.1, 10.4)")
  if (W$D2$candidate) flags <- c(flags, "D2 candidate")

  legacy <- if (length(parts)) {
    paste(unlist(lapply(parts, `[[`, "legacy")), collapse = "; ")
  } else if (length(nt_reasons)) {
    paste(unique(nt_reasons), collapse = "; ")
  } else ""
  if (W$D2$candidate && proposal %in% c("LC", "NT")) {
    legacy <- paste(c(if (nzchar(legacy)) legacy, "D2 candidate - confirm plausible threat"), collapse = "; ")
  }
  if (!nzchar(legacy)) legacy <- "None"

  .proposal(
    proposal, legacy,
    iucn = .iucn_string(parts),
    support = if (length(needs)) "indication" else "evidence",
    needs = needs,
    range = if (lo == hi) lo else paste0(lo, "-", hi),
    flags = flags, nt = nt_reasons, low = cat_l, high = cat_h, parts = parts,
    uncertain = uncertain
  )
}

.proposal <- function(category, criteria, iucn = "", support = "indication",
                      needs = character(0), range = category, flags = character(0),
                      nt = character(0), low = NA_character_, high = NA_character_,
                      parts = list(), re = FALSE, uncertain = FALSE) {
  structure(list(
    category = category, criteria = criteria, criteria_iucn = iucn,
    support = support, needs = unique(needs), range = range,
    uncertain = uncertain,
    flags = unique(flags), nt_reasons = unique(nt), low = low, high = high,
    parts = parts, re_candidate = re
  ), class = "ndopred_proposal")
}

#' @export
print.ndopred_proposal <- function(x, ...) {
  cat("Proposed:", x$category, if (nzchar(x$criteria_iucn)) x$criteria_iucn, "\n")
  cat("Criteria (established format):", x$criteria, "\n")
  cat("Support:", x$support, "| range:", x$range, if (identical(x$uncertain, TRUE)) "| UNCERTAIN", "\n")
  if (length(x$flags)) cat("Flags:", paste(x$flags, collapse = "; "), "\n")
  if (length(x$needs)) cat("Needs evidence:\n", paste0("  - ", x$needs, collapse = "\n"), "\n")
  invisible(x)
}

# ------------------------------------------------------------- near-misses

.near_reasons <- function(W, cfg) {
  conv <- cfg$conventions
  out <- character(0)
  for (sub in c("B1", "B2")) {
    B <- W$B[[sub]]
    if (!is.na(B$gate) && is.na(B$level) && B$max_count == 1) {
      out <- c(out, paste0("nearly meets ", sub, " (area threshold met, only 1 of 3 conditions)"))
    }
  }
  A <- W$A
  if (!is.na(A$value) && is.na(A$level) && A$value >= conv$nt_margin_a) {
    t <- intersect(A$types, names(.A_THRESH))
    t <- if (length(t)) t[1] else "A2"
    out <- c(out, sprintf("nearly meets %s%s (%g-%g%% reduction)", t, A$basis,
                          conv$nt_margin_a, .A_THRESH[[t]][["VU"]] - 1))
  }
  C <- W$C
  if (C$evaluated && is.na(C$level) && C$N$value < conv$nt_margin_c) {
    out <- c(out, sprintf("small population (<%s mature individuals)",
                          format(conv$nt_margin_c, big.mark = ",")))
  }
  out
}

# ------------------------------------------------------- criteria strings

.b_sub <- function(B) {
  paste0(if (B$a) "a",
         if (length(B$b)) paste0("b(", paste(sort(B$b), collapse = ","), ")"),
         if (length(B$c)) paste0("c(", paste(sort(B$c), collapse = ","), ")"))
}

# Criteria met at `level` in the working scenario, each with its string in
# ndopred's established format and in Criteria v3.1 Annex 2 notation.
.parts_at <- function(W, level) {
  parts <- list()
  A <- W$A
  if (identical(A$level, level)) {
    nums <- sub("A", "", A$types)
    parts$A <- list(legacy = paste0(A$types[1], A$basis),
                    iucn = paste0("A", paste0(nums, A$basis, collapse = "+")))
  }
  b_legacy <- character(0)
  b_iucn <- character(0)
  for (sub in c("B1", "B2")) {
    B <- W$B[[sub]]
    if (identical(B$level, level)) {
      b_legacy <- c(b_legacy, paste0(sub, .b_sub(B)))
      b_iucn <- c(b_iucn, paste0(sub("B", "", sub), .b_sub(B)))
    }
  }
  if (length(b_legacy)) parts$B <- list(legacy = b_legacy, iucn = paste0("B", paste(b_iucn, collapse = "+")))
  C <- W$C
  if (identical(C$level, level)) {
    a_iucn <- if (C$ai && C$aii) "a(i,ii)" else if (C$ai) "a(i)" else if (C$aii) "a(ii)" else ""
    a_legacy <- paste0(if (C$ai) "a(i)", if (C$ai && C$aii) ",", if (C$aii) "a(ii)")
    c2 <- paste0("2", a_iucn, if (C$b) "b")
    parts$C <- list(
      legacy = if (C$c1) "C1" else paste0("C2", a_legacy, if (C$b) "b"),
      iucn = paste0("C", paste(c(if (C$c1) "1", if (C$c2) c2), collapse = "+"))
    )
  }
  d_met <- identical(W$D$level, level)
  d2_met <- level == "VU" && W$D2$met
  if (d_met || d2_met) {
    iucn <- if (level != "VU") "D" else if (d_met && d2_met) "D1+2" else if (d_met) "D1" else "D2"
    parts$D <- list(legacy = c(if (d_met) "D1", if (d2_met) "D2"), iucn = iucn)
  }
  if (identical(W$E$level, level)) parts$E <- list(legacy = "E", iucn = "E")
  parts
}

.iucn_string <- function(parts) {
  if (!length(parts)) return("")
  paste(vapply(parts, `[[`, character(1), "iucn"), collapse = "; ")
}

# ---------------------------------------------------- what needs evidence

# No trend without records in both windows (trend_estimate()); a taxon gone
# from the current window is not read as a 100 % reduction.
.needs_a_unknown <- function(W, inp) {
  if (!is.na(W$A$value)) return(character(0))
  empty <- if (identical(inp$extant, FALSE)) "current" else "comparison"
  sprintf("A: no reduction estimate (no records in the %s window)", empty)
}

.needs <- function(assessment, proposal, parts) {
  inp <- assessment$inputs
  S <- assessment$scenarios
  W <- S$working
  H <- S$high
  out <- character(0)
  ind_src <- inp$indication$reduction_source
  prop_rank <- .rank(proposal)

  # Criterion A
  src <- if (is.na(ind_src)) "the records" else ind_src
  out <- c(out, .needs_a_unknown(W, inp))
  # What the reduction still needs depends on the data tier behind it
  # (trend_estimate()): T0/T1 are diagnostics, T2 an occupancy trend whose
  # link to the population must be shown (review 7.3), T3 an index to confirm.
  tier <- inp$indication$data_tier
  a_need <- switch(if (is.null(tier) || is.na(tier)) "T0" else tier,
                   T3 = "an estimate from standardized monitoring - confirm it as evidence",
                   T2 = "a trend in occupancy - document its link to the population (basis c)",
                   "needs an effort-corrected or monitored estimate")
  if (!is.null(parts$A) && W$A$support == "indication") {
    out <- c(out, sprintf("%s: reduction from %s - %s", paste(W$A$types, collapse = "+"), src, a_need))
  } else if (is.na(W$A$level) && W$A$support == "indication" && prop_rank < .rank("VU")) {
    # A non-threatened proposal that rests on "no reduction" from the same
    # proxy is no better evidenced than a threatened one.
    out <- c(out, sprintf("A: no qualifying reduction indicated by %s - %s", src, a_need))
  }

  # Criterion B: indications behind a B listing, and unknowns that could
  # raise the category.
  if (identical(inp$extant, FALSE)) out <- c(out, "B: current occurrence (no records in the current window)")
  if (is.na(inp$extant)) out <- c(out, "B: AOO/EOO could not be computed")
  for (sub in c("B1", "B2")) {
    B <- W$B[[sub]]
    Bh <- H$B[[sub]]
    if (.yes(inp$extant) && !B$evaluable) out <- c(out, paste0(sub, ": area could not be computed"))
    if (!B$evaluable || is.na(B$gate)) next
    behind <- !is.null(parts$B) && identical(B$level, proposal)
    could_raise <- !is.na(Bh$level) && .rank(Bh$level) > max(prop_rank, if (is.na(B$level)) 0 else .rank(B$level))
    b_ev <- vapply(B$conds$b, function(x) !is.na(x$ev) && x$ev, logical(1))
    b_ind <- vapply(B$conds$b, function(x) is.na(x$ev) && !is.na(x$ind) && x$ind, logical(1))
    c_ev <- vapply(B$conds$c, function(x) !is.na(x$ev) && x$ev, logical(1))
    c_ind <- vapply(B$conds$c, function(x) is.na(x$ev) && !is.na(x$ind) && x$ind, logical(1))
    item_a_ind <- "B(a): severe fragmentation or threat-defined locations (5 km clusters used as an indication)"
    item_b_ind <- "B(b): continuing decline (raw window-to-window change or counts used as an indication)"
    item_c_ind <- "B(c): extreme fluctuations (opportunistic counts used as an indication)"
    # Indications that the listing behind the proposal rests on.
    if (behind) {
      if (B$a && is.na(B$a_cond$ev)) out <- c(out, item_a_ind)
      if (any(b_ind[B$b])) out <- c(out, item_b_ind)
      if (any(c_ind[B$c])) out <- c(out, item_c_ind)
    }
    # Conditions without evidence that could complete a higher B listing.
    if (could_raise) {
      raise <- sprintf(" - could give %s %s", Bh$level, sub)
      a_top <- .a_cond(inp, Bh$level)
      if (is.na(a_top$ev)) {
        out <- c(out, paste0(if (!is.na(a_top$ind) && a_top$ind) item_a_ind else
          "B(a): severe fragmentation or threat-defined locations unknown", raise))
      }
      if (!any(b_ev)) out <- c(out, paste0(if (any(b_ind)) item_b_ind else "B(b): continuing decline unknown", raise))
      if (!any(c_ev)) out <- c(out, paste0(if (any(c_ind)) item_c_ind else "B(c): extreme fluctuations unknown", raise))
    }
  }

  # Criteria C and D: indications behind a C listing, and unknowns that
  # could raise the category.
  C <- W$C
  Ch <- H$C
  c_behind <- !is.null(parts$C)
  c_raise <- !is.na(Ch$level) && .rank(Ch$level) > max(prop_rank, if (is.na(C$level)) 0 else .rank(C$level))
  if (c_behind || c_raise) {
    if (C$N$support == "indication") out <- c(out, "C: number of mature individuals (NDOP minimum observed count used)")
    if (C$c1 && C$rate$support == "indication") out <- c(out, "C1: estimated continuing decline (opportunistic counts used)")
    uses_subpop <- (C$ai && C$subpop$support == "indication") || (C$aii && C$share$support == "indication") ||
      (c_raise && (Ch$ai || Ch$aii) && (C$subpop$support == "indication" || C$share$support == "indication"))
    if (uses_subpop) out <- c(out, "C2a: subpopulations (NDOP localities used as subpopulations)")
    if (is.na(C$declining$ev)) {
      out <- c(out, if (!is.na(C$declining$ind)) {
        "C2: continuing decline in mature individuals (opportunistic counts used as an indication)"
      } else "C2: continuing decline in mature individuals unknown")
    }
    if ((C$b || (c_raise && Ch$b)) && is.na(C$fluct$ev)) {
      out <- c(out, "C2b: extreme fluctuations in mature individuals (not evidenced)")
    }
  }
  if (!is.null(parts$D) && identical(W$D$level, proposal) && W$D$N$support == "indication") {
    out <- c(out, "D: number of mature individuals (NDOP minimum observed count used)")
  }

  # Criterion D2 can only give VU, so it matters only below VU or when it is
  # one of the criteria behind a VU proposal.
  D2 <- W$D2
  d2_behind <- !is.null(parts$D) && "D2" %in% parts$D$legacy
  if (inp$threat_undescribed) out <- c(out, "D2: describe the plausible threat (Guidelines 8 requires it to be specified)")
  if (prop_rank < .rank("VU") || d2_behind) {
    if (D2$candidate) out <- c(out, "D2: plausible threat not assessed (Guidelines 8)")
    if ((d2_behind || D2$candidate) && !D2$by_aoo && !is.null(D2$loc) && is.na(D2$loc$ev)) {
      out <- c(out, "D2: threat-defined locations (5 km clusters used as an indication)")
    }
  }
  .merge_raises(unique(out))
}

# "X - could give VU B1" and "X - could give EN B2" become one item,
# "X - could give VU B1, EN B2".
.merge_raises <- function(items) {
  marker <- " - could give "
  has <- grepl(marker, items, fixed = TRUE)
  if (sum(has) < 2) return(items)
  base <- sub(paste0(marker, ".*$"), "", items)
  target <- ifelse(has, sub(paste0("^.*", marker), "", items), NA_character_)
  out <- character(0)
  for (b in unique(base)) {
    k <- base == b
    t <- unique(stats::na.omit(target[k]))
    out <- c(out, if (length(t) && all(has[k])) paste0(b, marker, paste(t, collapse = ", ")) else items[k])
  }
  unique(out)
}

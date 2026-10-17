#' Check the Final Category Against the Evidence, and Its Approval
#'
#' The proposal is the engine's; the final category is the assessor's, and
#' it may depart from the proposal only with a justification (prompt section
#' 6). This function re-checks a final category against what the evidence
#' and the measured data support on their own, without the indications from
#' the records, and records whether it is approved. Approval is by group
#' experts, conservationists and taxonomists trained in Red List assessment
#' (AOPK, decision D9); a final category without an approval given on or
#' after its date is not reported as final.
#'
#' @section Checks:
#' \itemize{
#'   \item A threatened category above what the evidence supports, e.g. EN
#'     claimed under B with only one of (a), (b), (c) evidenced (Criteria
#'     v3.1 B; Guidelines §2.3).
#'   \item A criterion named in the criteria string that the evidence does
#'     not meet at that category.
#'   \item A category below a threatened category the evidence meets.
#'   \item DD where the evidence meets a threatened category (§10.3).
#'   \item RE without documented surveys, with current records, or with the
#'     last record before 1500 AD (Regional Guidelines III.14; §11.3); EX and
#'     EW at national level.
#'   \item NA without a step-one reason, or another category for a taxon
#'     step one makes Not Applicable (Regional Guidelines IV.2).
#' }
#' The comparison includes the regional step: what the evidence supports is
#' adjusted by the step-three answers ([regional_assessment()]), so a final
#' category one step below it after a documented rescue effect is
#' consistent, and is shown with the degree sign (VU\eqn{^\circ}).
#'
#' @param assessment Object from [assess_criteria()], with the evidence.
#' @param proposal Object from [categorise()].
#' @param ex The result of [evidence_for_engine()].
#' @param taxon Optional taxon name, for the list of introduced taxa
#'   ([introduced_taxa()]); by default the taxon of the evidence when it is
#'   about one taxon.
#' @return A list of class `ndopred_final`: `category` (the final category
#'   when approved, else `NA`), `display` (the same with the degree sign when
#'   the regional step adjusted it), `claimed`, `criteria`, `status` ("no
#'   final category", "awaiting approval", "approved" or "approved;
#'   inconsistent with the evidence"), `issues`, `notes`, `evidence_only`
#'   (category, criteria and regional category the evidence and data support
#'   alone) and `approvals`.
#' @seealso [evidence_for_engine()], [assess_taxon()]
#' @export
final_assessment <- function(assessment, proposal, ex, taxon = NULL) {
  if (is.null(taxon)) {
    tx <- unique(ex$entries$taxon)
    taxon <- if (length(tx) == 1) tx else NA_character_
  }
  fin <- ex$final
  eo <- .evidence_only(assessment)
  eo_cat <- .scenario_category(eo$scenarios$working)
  eo_parts <- if (.rank(eo_cat) >= .rank("VU")) .parts_at(eo$scenarios$working, eo_cat) else list()
  eo_str <- .iucn_string(eo_parts)
  threatened <- c("CR", "EN", "VU")
  step2 <- if (eo_cat %in% threatened) paste0(eo_cat, " ", eo_str) else eo_cat
  # The regional step applied to what the evidence supports (RG IV.2, IV.4).
  reg <- regional_assessment(eo_cat, eo_str, ex, assessment$config, taxon)
  el <- reg$eligibility
  adj <- reg$adjustment
  sup_cat <- reg$category
  supported <- if (!el$eligible) {
    paste0("NA (", el$reason, ")")
  } else if (adj$adjusted) {
    paste0(sup_cat, " (", step2, ", ", utils::tail(adj$trail, 1), ")")
  } else step2

  out <- structure(list(
    category = NA_character_, display = NA_character_, claimed = NA_character_,
    criteria = NA_character_, status = "no final category", issues = character(0),
    notes = ex$checks,
    evidence_only = list(category = eo_cat, criteria = eo_str, regional = reg$display),
    approvals = ex$approvals
  ), class = "ndopred_final")
  if (is.null(fin)) return(out)

  fcat <- fin$category
  out$claimed <- fcat
  out$criteria <- fin$criteria
  issue <- function(...) out$issues <<- c(out$issues, paste0(...))
  note <- function(...) out$notes <<- c(out$notes, paste0(...))
  if (!identical(fcat, proposal$category)) {
    note("departs from the proposal (", proposal$category, "); justification: ", fin$justification)
  }
  eo_threat <- eo_cat %in% threatened
  ladder <- c("LC", "NT", "VU", "EN", "CR")
  # The step-two category behind a final category the regional step moved:
  # criteria are met at that level.
  fcat_step2 <- fcat
  if (el$eligible && adj$adjusted && identical(fcat, sup_cat)) {
    k <- if (adj$direction == "down") adj$steps else -adj$steps
    fcat_step2 <- ladder[min(length(ladder), max(1, match(fcat, ladder) + k))]
  }

  if (!el$eligible) {
    if (fcat != "NA") issue("step one: ", el$reason, "; the taxon is Not Applicable")
  } else if (fcat == "NA") {
    issue("NA needs a step-one reason: introduced, vagrant, occasional breeder, recent colonizer ",
          "or below the regional filter (RG IV.2)")
  } else if (fcat %in% threatened) {
    if (!sup_cat %in% ladder || .rank(fcat) > .rank(sup_cat)) {
      issue(fcat, " is not supported by the evidence and data alone, which meet ", supported)
      issue(.b_detail(eo, fcat_step2))
    }
    for (L in .criteria_letters(fin$criteria)) {
      if (!.letter_met(eo$scenarios$working, L, fcat_step2)) {
        issue("criterion ", L, " is not met at ", fcat_step2, " by the evidence")
      }
    }
    if (sup_cat %in% ladder && .rank(fcat) < .rank(sup_cat)) {
      issue("the evidence meets ", supported, "; a lower category needs a documented reason")
    }
  } else if (fcat %in% c("NT", "LC")) {
    if (sup_cat %in% threatened) issue("the evidence meets ", supported, "; ", fcat, " contradicts it")
  } else if (fcat == "DD") {
    if (eo_threat) issue("DD is not appropriate: the evidence meets ", step2, " (Guidelines 10.3)")
    note("DD needs the available data and the reason each of A-E cannot be applied documented (Guidelines 10.4)")
  } else if (fcat == "RE") {
    if (!nrow(ex$surveys)) issue("RE needs documented surveys (Regional Guidelines III.14; Guidelines 11.3)")
    if (.yes(assessment$inputs$extant)) issue("RE, but records exist in the current window")
    yl <- assessment$inputs$year_last
    if (!is.na(yl) && yl < 1500) issue("RE should not normally pre-date 1500 AD (Regional Guidelines III.14)")
  } else if (fcat %in% c("EX", "EW")) {
    issue(fcat, " is a global category; a taxon gone from the region is RE (Regional Guidelines)")
  }
  regional_mark <- el$eligible && adj$adjusted && identical(fcat, sup_cat)
  if (regional_mark) note("regional step: ", paste(adj$trail, collapse = "; "))

  ap <- ex$approvals
  approved <- nrow(ap) > 0 && any(as.Date(ap$date) >= as.Date(fin$date))
  if (!approved) {
    out$status <- "awaiting approval"
    if (nrow(ap)) note("approval dated before the final category (", fin$date, ") does not cover it")
    return(out)
  }
  out$category <- fcat
  out$display <- if (regional_mark) paste0(fcat, .DEGREE) else fcat
  out$status <- if (length(out$issues)) "approved; inconsistent with the evidence" else "approved"
  out
}

#' @export
print.ndopred_final <- function(x, ...) {
  cat("Final:", if (is.na(x$claimed)) "-" else paste(x$claimed, x$criteria), "|", x$status, "\n")
  cat("Evidence alone supports:", x$evidence_only$category, x$evidence_only$criteria,
      "| after the regional step:", x$evidence_only$regional, "\n")
  if (length(x$issues)) cat("Issues:\n", paste0("  - ", x$issues, collapse = "\n"), "\n")
  if (length(x$notes)) cat("Notes:\n", paste0("  - ", x$notes, collapse = "\n"), "\n")
  invisible(x)
}

# The assessment without indications: what evidence and measured data
# (areas, counts) support on their own.
.evidence_only <- function(assessment) {
  inp <- assessment$inputs
  inp$indication <- assessment_inputs()$indication
  assess_criteria(inp, assessment$config, assessment$evaluate_pop)
}

# Criteria letters named in a criteria string, e.g. "EN B1ab(iii); C2a(i)".
.criteria_letters <- function(criteria) {
  if (is.null(criteria) || is.na(criteria) || !nzchar(criteria)) return(character(0))
  s <- gsub("\\([^)]*\\)", "", criteria)               # drop subcriteria in brackets
  s <- gsub("\\b(CR|EN|VU|NT|LC|DD|RE)\\b", "", s)     # and the category
  unique(regmatches(s, gregexpr("(?<![a-z])[A-E](?=[0-9]|$|[^a-zA-Z])", s, perl = TRUE))[[1]])
}

# Whether criterion `letter` is met at `level` or above in a scenario.
.letter_met <- function(s, letter, level) {
  at_least <- function(x) !is.na(x) && .rank(x) >= .rank(level)
  switch(EXPR = letter,
         A = at_least(s$A$level),
         B = at_least(s$B$B1$level) || at_least(s$B$B2$level),
         C = at_least(s$C$level),
         D = at_least(s$D$level) || (level == "VU" && s$D2$met),
         E = at_least(s$E$level),
         FALSE)
}

# Criterion B at one level in the evidence-only scenario: the area
# thresholds and how many of (a), (b), (c) are evidenced.
.b_detail <- function(eo, level) {
  inp <- eo$inputs
  W <- eo$scenarios$working
  gate <- c(B1 = !is.na(W$B$B1$area) && W$B$B1$area < .B_AREA$B1[[level]],
            B2 = !is.na(W$B$B2$area) && W$B$B2$area < .B_AREA$B2[[level]])
  a <- .a_cond(inp, level)$ev
  n <- sum(c(isTRUE(a),
             any(inp$evidence$decline %in% TRUE),
             any(inp$evidence$fluctuation %in% TRUE)))
  areas <- if (any(gate)) paste(names(gate)[gate], collapse = " and ") else "neither area"
  sprintf("B at %s: %s below the threshold; %d of (a), (b), (c) evidenced (two needed)", level, areas, n)
}

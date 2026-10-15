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
#'   \item RE without documented surveys, or with current records (Regional
#'     Guidelines III.14; §11.3); EX and EW at national level.
#'   \item NA without eligibility evidence (introduced, or a vagrant or
#'     visitor; Regional Guidelines).
#' }
#'
#' @param assessment Object from [assess_criteria()], with the evidence.
#' @param proposal Object from [categorise()].
#' @param ex The result of [evidence_for_engine()].
#' @return A list of class `ndopred_final`: `category` (the final category
#'   when approved, else `NA`), `claimed`, `criteria`, `status` ("no final
#'   category", "awaiting approval", "approved" or "approved; inconsistent
#'   with the evidence"), `issues`, `notes`, `evidence_only` (category and
#'   criteria the evidence and data support alone) and `approvals`.
#' @seealso [evidence_for_engine()], [assess_taxon()]
#' @export
final_assessment <- function(assessment, proposal, ex) {
  fin <- ex$final
  eo <- .evidence_only(assessment)
  eo_cat <- .scenario_category(eo$scenarios$working)
  eo_parts <- if (.rank(eo_cat) >= .rank("VU")) .parts_at(eo$scenarios$working, eo_cat) else list()
  eo_str <- .iucn_string(eo_parts)
  supported <- if (.rank(eo_cat) >= .rank("VU")) paste0(eo_cat, " ", eo_str) else eo_cat

  out <- structure(list(
    category = NA_character_, claimed = NA_character_, criteria = NA_character_,
    status = "no final category", issues = character(0), notes = ex$checks,
    evidence_only = list(category = eo_cat, criteria = eo_str),
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
  threatened <- c("CR", "EN", "VU")
  eo_threat <- eo_cat %in% threatened

  if (fcat %in% threatened) {
    if (.rank(fcat) > .rank(eo_cat)) {
      issue(fcat, " is not supported by the evidence and data alone, which meet ", supported)
      issue(.b_detail(eo, fcat))
    }
    for (L in .criteria_letters(fin$criteria)) {
      if (!.letter_met(eo$scenarios$working, L, fcat)) {
        issue("criterion ", L, " is not met at ", fcat, " by the evidence")
      }
    }
    if (.rank(fcat) < .rank(eo_cat)) {
      issue("the evidence meets ", supported, "; a lower category needs a documented reason ",
            "(e.g. the regional adjustment)")
    }
  } else if (fcat %in% c("NT", "LC")) {
    if (eo_threat) issue("the evidence meets ", supported, "; ", fcat, " contradicts it")
  } else if (fcat == "DD") {
    if (eo_threat) issue("DD is not appropriate: the evidence meets ", supported, " (Guidelines 10.3)")
    note("DD needs the available data and the reason each of A-E cannot be applied documented (Guidelines 10.4)")
  } else if (fcat == "RE") {
    if (!nrow(ex$surveys)) issue("RE needs documented surveys (Regional Guidelines III.14; Guidelines 11.3)")
    if (.yes(assessment$inputs$extant)) issue("RE, but records exist in the current window")
  } else if (fcat %in% c("EX", "EW")) {
    issue(fcat, " is a global category; a taxon gone from the region is RE (Regional Guidelines)")
  } else if (fcat == "NA") {
    el <- ex$eligibility
    eligible_na <- isTRUE(el$origin == "introduced") ||
      isTRUE(el$occurrence_status %in% c("vagrant", "visitor"))
    if (!eligible_na) issue("NA needs eligibility evidence: introduced, or a vagrant or visitor (Regional Guidelines)")
  }

  ap <- ex$approvals
  approved <- nrow(ap) > 0 && any(as.Date(ap$date) >= as.Date(fin$date))
  if (!approved) {
    out$status <- "awaiting approval"
    if (nrow(ap)) note("approval dated before the final category (", fin$date, ") does not cover it")
    return(out)
  }
  out$category <- fcat
  out$status <- if (length(out$issues)) "approved; inconsistent with the evidence" else "approved"
  out
}

#' @export
print.ndopred_final <- function(x, ...) {
  cat("Final:", if (is.na(x$claimed)) "-" else paste(x$claimed, x$criteria), "|", x$status, "\n")
  cat("Evidence alone supports:", x$evidence_only$category, x$evidence_only$criteria, "\n")
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

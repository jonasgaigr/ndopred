# The regional step: IUCN (2012) Guidelines for Application of IUCN Red List
# Criteria at Regional and National Levels, Version 4.0 (hereafter RG).
# Step one decides whether the regional population is assessed (RG IV.2,
# Annex 3); step two is the rule engine; step three adjusts the preliminary
# category for populations outside the region (RG IV.4, Figure 3, Table 1).

# Categories that "cannot logically be up- or downlisted" (RG IV.4).
.NOT_ADJUSTABLE <- c("EX", "EW", "RE", "DD", "NE", "NA")
.DEGREE <- "\u00b0"   # RG V.3: "a degree sign after the category (VU\u00b0)"

# yes / likely -> TRUE; no / unlikely -> FALSE; do not know -> NA (RG Fig. 3).
.answer <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (!length(x) || is.na(x[1])) return(NA)
  if (x[1] %in% c("yes", "likely")) return(TRUE)
  if (x[1] %in% c("no", "unlikely")) return(FALSE)
  NA
}

#' Regional Step One: Is the Regional Population Assessed?
#'
#' Follows the flowchart of the Regional Guidelines v4.0 (Annex 3; section
#' IV.2). The categorisation applies "only to wild populations inside their
#' natural range and to populations resulting from benign introductions".
#' Not assessed, and Not Applicable (NA): introduced taxa, vagrants,
#' occasional breeders that regularly become regionally extinct, colonizers
#' that have not yet reproduced in the region for `colonizer_years`
#' consecutive years ("typically for at least 10"), and taxa below an
#' optional filter on the share of the global population. As in the
#' flowchart, an answer the assessor does not know leads to assessment.
#'
#' @param eligibility A list with `origin` (native, introduced, reintroduced,
#'   uncertain), `occurrence_status` (resident, breeding, visitor, vagrant,
#'   occasional_breeder, uncertain), `reproduction_since` (first year of
#'   established reproduction in the region) and `global_share` (per cent of
#'   the global population in the region); missing entries are unknown.
#' @param config Object from [ndopred_config()]: the reference year and the
#'   conventions `colonizer_years` and `regional_filter_share`.
#' @return A list: `eligible` (logical), `population` ("breeding" or
#'   "visiting"), `reason` and `documented` (whether eligibility evidence was
#'   given).
#' @seealso [regional_adjustment()], [regional_assessment()]
#' @examples
#' cfg <- ndopred_config(2026)
#' regional_eligibility(list(origin = "introduced"), cfg)$eligible          # FALSE
#' regional_eligibility(list(reproduction_since = 2021), cfg)$reason        # recent colonizer
#' regional_eligibility(list(), cfg)$reason                                 # assessed, not documented
#' @export
regional_eligibility <- function(eligibility, config) {
  el <- if (is.null(eligibility)) list() else eligibility
  get <- function(k) {
    v <- el[[k]]
    if (is.null(v) || !length(v) || is.na(v[1]) || !nzchar(as.character(v[1]))) NA else v[1]
  }
  origin <- get("origin")
  status <- get("occurrence_status")
  since <- suppressWarnings(as.numeric(get("reproduction_since")))
  share <- suppressWarnings(as.numeric(get("global_share")))
  conv <- config$conventions
  documented <- !all(is.na(c(origin, status, since, share)))
  population <- if (identical(status, "visitor")) "visiting" else "breeding"
  out <- function(eligible, reason) {
    list(eligible = eligible, population = population, reason = reason, documented = documented)
  }
  if (identical(origin, "introduced")) {
    return(out(FALSE, "introduced: not a wild population within its natural range (RG IV.2)"))
  }
  if (identical(status, "vagrant")) {
    return(out(FALSE, "vagrant: \"Vagrant taxa should NOT be assessed\" (RG IV.2)"))
  }
  if (identical(status, "occasional_breeder")) {
    return(out(FALSE, "occasional breeder that regularly becomes regionally extinct (RG IV.2)"))
  }
  years <- if (is.na(since)) NA else config$reference_year - since + 1
  if (!is.na(years) && years < conv$colonizer_years) {
    return(out(FALSE, sprintf(paste0("recent colonizer: reproducing in the region for %d years, ",
                                     "fewer than %d (RG IV.2)"), years, conv$colonizer_years)))
  }
  filt <- conv$regional_filter_share
  if (!is.null(filt) && !is.na(filt) && !is.na(share) && share < filt) {
    return(out(FALSE, sprintf("below the regional filter: %s %% of the global population, under %s %% (RG IV.2)",
                              share, filt)))
  }
  reason <- if (!documented) {
    "assessed: eligibility not documented (assumed wild, native and established)"
  } else {
    paste0("assessed: ", population, " population",
           if (identical(origin, "reintroduced")) "; reintroduced, assessed once reproducing without direct support (RG IV.2)" else "")
  }
  out(TRUE, reason)
}

#' Regional Step Three: Adjust the Preliminary Category
#'
#' The procedure of the Regional Guidelines v4.0, Figure 3 and Table 1, for
#' populations outside the region. Breeding populations: (3a) significant
#' immigration of propagules likely to reproduce in the region? (3b) is the
#' immigration expected to decrease? (3c) is the regional population a
#' sink? Visiting populations: (3d) conditions outside the region
#' deteriorating? (3e) conditions within the region deteriorating? (3f) can
#' the breeding population rescue the regional population should it decline?
#' Answers are yes, likely, no, unlikely or unknown; "If it is unknown
#' whether or not extra-regional populations influence the extinction risk
#' of the regional population, the category from step two should be kept
#' unaltered". A downlisting is "normally" one step; two may be appropriate
#' for expanding populations at the edge of their range or very small,
#' non-isolated regions. EX, EW, RE, DD, NE and NA are never adjusted. An
#' adjusted category carries a degree sign (VU\eqn{^\circ}), which is equivalent to the
#' same category (RG V.3).
#'
#' @param category The preliminary category (step two).
#' @param answers Named character vector or list of answers, names `3a` to
#'   `3f`.
#' @param population "breeding" or "visiting".
#' @param steps Number of categories to move (default 1).
#' @param global_criteria Optional global criteria string: a taxon listed
#'   globally under D is not expected to rescue a visiting population (RG
#'   IV.4, box 3f).
#' @return A list: `category`, `display` (with the degree sign when
#'   adjusted), `adjusted`, `direction` ("down", "up" or "none"), `steps`
#'   and `trail` (the answers that decided it).
#' @examples
#' regional_adjustment("EN", c("3a" = "yes", "3b" = "no"))$display   # "VU" with a degree sign
#' regional_adjustment("EN", c("3a" = "unknown"))$display            # "EN"
#' @export
regional_adjustment <- function(category, answers = character(0), population = "breeding",
                                steps = 1, global_criteria = NA_character_) {
  raw <- function(k) if (k %in% names(answers)) as.character(answers[[k]]) else NA_character_
  a <- function(k) .answer(raw(k))
  ans <- function(k) {
    v <- raw(k)
    paste0(k, " ", if (is.na(v) || !nzchar(v)) "not answered" else v)
  }
  steps <- suppressWarnings(as.integer(steps))
  if (is.na(steps) || steps < 1) steps <- 1L
  none <- function(trail) list(category = category, display = category, adjusted = FALSE,
                               direction = "none", steps = 0L, trail = trail)
  if (is.na(category) || category %in% .NOT_ADJUSTABLE) {
    return(none(paste(category, "is never adjusted (RG IV.4)")))
  }
  direction <- "none"
  if (population == "visiting") {
    trail <- c(ans("3d"), ans("3e"), ans("3f"))
    rescue <- isFALSE(a("3d")) && isFALSE(a("3e")) && isTRUE(a("3f"))
    if (rescue && !is.na(global_criteria) && grepl("\\bD", global_criteria)) {
      return(none(c(trail, "no downlisting: a taxon listed globally under D cannot be expected to rescue (RG IV.4)")))
    }
    if (rescue) direction <- "down"
  } else {
    trail <- ans("3a")
    if (isTRUE(a("3a"))) {
      trail <- c(trail, ans("3b"))
      if (isFALSE(a("3b"))) {
        direction <- "down"
      } else {
        trail <- c(trail, ans("3c"))
        if (isTRUE(a("3c"))) direction <- "up"
      }
    }
  }
  if (direction == "none") return(none(c(trail, "no change")))
  ladder <- c("LC", "NT", "VU", "EN", "CR")
  from <- match(category, ladder)
  to <- if (direction == "down") max(1L, from - steps) else min(length(ladder), from + steps)
  moved <- abs(to - from)
  if (!moved) return(none(c(trail, sprintf("no change: %s cannot move %s", category, direction))))
  new <- ladder[to]
  list(category = new, display = paste0(new, .DEGREE), adjusted = TRUE, direction = direction,
       steps = as.integer(moved),
       trail = c(trail, sprintf("%s %d step%s from %s", if (direction == "down") "downlisted" else "uplisted",
                                moved, if (moved > 1) "s" else "", category)))
}

#' The Regional Step for One Taxon
#'
#' Combines step one ([regional_eligibility()]) and step three
#' ([regional_adjustment()]) around the category from step two, and carries
#' the fields a regional Red List publishes beside it: the global category
#' and the share of the global population in the region, `"?"` when unknown
#' (Regional Guidelines v4.0, V.4).
#'
#' @param category,criteria The category and criteria from step two.
#' @param ex The result of [evidence_for_engine()], or `NULL` (no regional
#'   evidence: assessed, no adjustment).
#' @param config Object from [ndopred_config()].
#' @return A list: `eligibility`, `adjustment`, `category` (final regional
#'   category without the degree sign), `display`, `criteria`, `global`
#'   (category and criteria) and `global_share`.
#' @export
regional_assessment <- function(category, criteria, ex, config) {
  reg <- if (is.null(ex)) list() else ex$regional
  el <- regional_eligibility(if (is.null(ex)) list() else ex$eligibility, config)
  glob <- if (is.null(reg$global)) list(category = NA_character_, criteria = NA_character_) else reg$global
  share <- if (is.null(reg$global_share)) NA_real_ else reg$global_share
  if (!el$eligible) {
    adj <- list(category = "NA", display = "NA", adjusted = FALSE, direction = "none", steps = 0L,
                trail = "not assessed (step one)")
  } else {
    adj <- regional_adjustment(category, if (is.null(reg$answers)) character(0) else reg$answers,
                               el$population, if (is.null(reg$steps)) 1 else reg$steps, glob$criteria)
  }
  crit <- if (!el$eligible || !adj$category %in% c("CR", "EN", "VU")) "" else criteria
  list(eligibility = el, adjustment = adj, category = adj$category, display = adj$display,
       criteria = crit, global = glob,
       global_share = if (is.na(share)) "?" else format(share))
}

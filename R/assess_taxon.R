#' Compute the Metrics for One Taxon
#'
#' Everything the engine needs from the records of one taxon, for the
#' windows in `config`. Used by [assess_taxon()] and by both Shiny apps, so
#' the two modes compute identical numbers from identical inputs.
#'
#' @section Which AOO is which:
#' * `aoo_new` is the **criterion B2 value**: the current window on the
#'   2 x 2 km reference grid (Guidelines §4.10.3), minimised over grid
#'   origins for that window alone (§4.10.2).
#' * `aoo_old` is the comparison window's AOO, computed the same way.
#' * `aoo_change` comes from [calculate_trend()], which puts both trend
#'   windows on one shared grid origin so the change is not an artefact of
#'   two different grids.
#' * `aoo_display` is the current window on `display_grid_size`, for maps
#'   and display only; it never reaches the criteria.
#'
#' @param occ An `sf` object from [get_assessment_data()].
#' @param config Object from [ndopred_config()].
#' @param display_grid_size Numeric. Cell size in metres for `aoo_display`.
#' @param mature_terms Passed to [calculate_pop_metrics()].
#' @param effort Optional effort layer for the taxon's group
#'   ([ndop_effort()]), for data tiers T1 and T2.
#' @param taxon Optional taxon name as in NDOP (for T2).
#' @section Trends:
#' `aoo_change` and `eoo_change` are the raw changes of [calculate_trend()]
#' (EOO by alpha-hull), kept for display. The trend the engine uses is
#' `trend_estimate` ([trend_estimate()]): the data tier and the estimate it
#' allows, with its interval and start-year sensitivity.
#' @return A list of metrics plus `issues`, a named list giving the reason
#'   for every value that could not be computed, and `failed` (a reason)
#'   when no assessment is possible at all, e.g. after a failed download.
#' @seealso [assess_taxon()]
#' @export
taxon_metrics <- function(occ, config, display_grid_size = 2000,
                          mature_terms = get_mature_terms(), effort = NULL,
                          taxon = NA_character_) {
  stopifnot(inherits(config, "ndopred_config"))
  w <- config$windows
  issues <- list()
  note <- function(code, msg) {
    issues[[code]] <<- paste(c(issues[[code]], msg), collapse = "; ")
  }
  capture <- function(code, expr) {
    tryCatch(expr, error = function(e) {
      note(code, paste0("calculation error: ", conditionMessage(e)))
      NULL
    })
  }
  pct <- function(before, after) {
    if (is.na(before) || is.na(after) || before <= 0) return(NA_real_)
    (after - before) / before * 100
  }

  # Attributes do not survive subsetting, so read them first.
  dl <- attr(occ, "download_error")
  geoms <- attr(occ, "record_geometries")
  snapshot <- attr(occ, "snapshot")
  n_raw <- if (is.null(occ)) 0L else nrow(occ)
  # Only included records count; excluded ones keep their row upstream
  # (flag_records(); gap analysis, item 11).
  if (n_raw > 0 && "include" %in% names(occ)) {
    excluded <- occ$exclusion_reason[!occ$include]
    occ <- suppressWarnings(occ[occ$include %in% TRUE, , drop = FALSE])
  } else {
    excluded <- character(0)
  }
  n_records <- if (is.null(occ)) 0L else nrow(occ)
  out <- list(
    n_records = n_records, n_raw = n_raw, n_excluded = length(excluded),
    exclusions = if (length(excluded)) sort(table(excluded), decreasing = TRUE) else integer(0),
    n_current = 0L, n_comp = 0L, year_last = NA_real_,
    aoo_new = NA_real_, aoo_new_min = NA_real_, aoo_new_max = NA_real_,
    cells_new = NA_real_, aoo_display = NA_real_,
    eoo_new = NA_real_, eoo_new_min = NA_real_, eoo_new_max = NA_real_,
    clusters_new = NA_real_,
    aoo_old = NA_real_, eoo_old = NA_real_, clusters_old = NA_real_,
    aoo_change = NA_real_, eoo_change = NA_real_, locs_change = NA_real_,
    trend = NULL, trend_estimate = NULL, pop = NULL, windows = w,
    display_grid_size = display_grid_size,
    snapshot = if (is.null(snapshot$path)) NA_character_ else basename(snapshot$path)
  )
  if (!is.null(dl)) {
    note("download", dl)
    out$failed <- paste0("download failed: ", dl)
  }
  if (n_records == 0) {
    note("records", if (!is.null(dl)) "download failed" else if (n_raw > 0) {
      sprintf("all %d records excluded (see exclusions)", n_raw)
    } else "no usable NDOP records")
    out$issues <- issues
    return(out)
  }

  occ <- add_year_column(occ)
  yl <- suppressWarnings(max(occ$year_max, na.rm = TRUE))
  out$year_last <- if (is.finite(yl)) yl else NA_real_
  in_window <- function(a, b) .window_records(occ, a, b)
  recent <- in_window(w$recent_start, w$recent_end)
  comp <- in_window(w$comp_start, w$comp_end)
  out$n_current <- nrow(recent)
  out$n_comp <- nrow(comp)

  if (nrow(recent) == 0) {
    out$aoo_new <- out$aoo_new_min <- out$aoo_new_max <- 0
    out$cells_new <- 0
    out$aoo_display <- 0
    msg <- sprintf("no records in the current window %s-%s", w$recent_start, w$recent_end)
    note("eoo", msg)
    note("locations", msg)
  } else {
    a <- capture("aoo", calculate_aoo(recent, grid_size = 2000, geometries = geoms))
    if (!is.null(a)) {
      out$aoo_new <- a$area_km2
      out$aoo_new_min <- a$area_km2_min
      out$aoo_new_max <- a$area_km2_max
      out$cells_new <- a$n_cells
    }
    e <- capture("eoo", calculate_eoo(recent, geometries = geoms))
    eoo <- if (is.null(e)) NA_real_ else e$area_km2
    if (is.null(e) || is.na(eoo)) {
      if (is.null(issues$eoo)) note("eoo", "hull not computable (fewer than 3 distinct points); AOO stands in (Guidelines 4.9)")
    }
    out$eoo_new <- reconcile_eoo_aoo(eoo, out$aoo_new)
    # Each bound reconciled with the matching AOO bound (Guidelines 4.9).
    out$eoo_new_min <- reconcile_eoo_aoo(if (is.null(e)) NA_real_ else e$area_km2_min, out$aoo_new_min)
    out$eoo_new_max <- reconcile_eoo_aoo(if (is.null(e)) NA_real_ else e$area_km2_max, out$aoo_new_max)
    if (!is.na(out$eoo_new_min) && !is.na(out$eoo_new)) out$eoo_new_min <- min(out$eoo_new_min, out$eoo_new)
    if (!is.na(out$eoo_new_max) && !is.na(out$eoo_new)) out$eoo_new_max <- max(out$eoo_new_max, out$eoo_new)
    cl <- capture("locations", suppressWarnings(calculate_locations(recent)))
    if (!is.null(cl)) out$clusters_new <- cl
    if (isTRUE(all.equal(display_grid_size, 2000))) {
      out$aoo_display <- out$aoo_new
    } else {
      d <- capture("aoo", suppressWarnings(calculate_aoo(recent, grid_size = display_grid_size)))
      if (!is.null(d)) out$aoo_display <- d$area_km2
    }
  }

  if (nrow(comp) == 0) {
    note("trend", sprintf("no records in the comparison window %s-%s", w$comp_start, w$comp_end))
  } else {
    a <- capture("aoo", calculate_aoo(comp, grid_size = 2000))
    if (!is.null(a)) out$aoo_old <- a$area_km2
    e <- capture("eoo", calculate_eoo(comp))
    out$eoo_old <- reconcile_eoo_aoo(if (is.null(e)) NA_real_ else e$area_km2, out$aoo_old)
    cl <- capture("locations", suppressWarnings(calculate_locations(comp)))
    if (!is.null(cl)) out$clusters_old <- cl
  }

  tr <- capture("trend", calculate_trend(
    occ, recent_start = w$trend_start, recent_end = w$trend_end,
    comp_start = w$comp_start, comp_end = w$comp_end
  ))
  if (!is.null(tr)) {
    out$trend <- tr
    out$aoo_change <- tr$aoo_percent_change
    out$eoo_change <- tr$eoo_percent_change
    if (tr$period_2_n == 0 && nrow(comp) > 0) {
      note("trend", sprintf("no records in the trend window %s-%s", w$trend_start, w$trend_end))
    }
  }
  clusters_trend <- if (w$trend_start == w$recent_start) out$clusters_new else {
    tw <- in_window(w$trend_start, w$trend_end)
    if (nrow(tw)) capture("locations", suppressWarnings(calculate_locations(tw))) else NA_real_
  }
  out$locs_change <- pct(out$clusters_old, if (is.null(clusters_trend)) NA_real_ else clusters_trend)

  te <- capture("trend", trend_estimate(occ, config, effort, taxon))
  if (!is.null(te)) out$trend_estimate <- te

  # Counts from standardized monitoring (T3) are a consistent-effort series:
  # only they give an estimated decline (C1) and a fluctuation diagnosis.
  t3 <- !is.null(te) && identical(te$tier, "T3")
  pop_occ <- if (t3) occ[occ$PROJEKT %in% config$trend$monitoring_projects, , drop = FALSE] else occ
  pop <- capture("population", calculate_pop_metrics(
    pop_occ, window_years = w$trend_end - w$trend_start + 1, year_now = w$trend_end,
    mature_terms = mature_terms, consistent_effort = t3
  ))
  if (!is.null(pop)) {
    out$pop <- pop
    if (length(pop$note) == 1 && !is.na(pop$note)) note("population", pop$note)
  }
  out$issues <- issues
  out
}

#' Assess One Taxon: Records In, Proposal Out
#'
#' The pipeline shared by [assess_species()], [batch_assess()], the bulk app
#' and the single-taxon app: [taxon_metrics()], then [assess_criteria()] and
#' [categorise()]. With an evidence table ([validate_evidence()]), record
#' entries are applied as flags first ([apply_record_evidence()]), a taxon's
#' generation length or current window adjusts the config, and the final
#' category is checked ([final_assessment()]).
#'
#' @param occ An `sf` object from [get_assessment_data()]. With `metrics`,
#'   used only for the record list and when evidence changes the records or
#'   windows.
#' @param species Character. Taxon name.
#' @param config Object from [ndopred_config()].
#' @param evaluate_pop Logical. Whether NDOP counts may be used as
#'   indications for criteria A(b), B(b)(v), B(c)(iv), C and D.
#' @param evidence Expert evidence: an evidence table (see [evidence]) or
#'   the engine's evidence list (see [assessment_inputs()]).
#' @param display_grid_size Numeric. Cell size in metres of the display AOO.
#' @param metrics Optional result of [taxon_metrics()], to reuse.
#' @param effort Optional effort layer of the taxon's group ([ndop_effort()]),
#'   for the trend tiers T1 and T2 ([trend_estimate()]).
#' @return A list: `result` (one-row data frame, see [summarize_assessment()]
#'   for the columns, plus `Reference_Year`, the generation length, the final
#'   category columns and `issue_*` reason codes), `metrics`, `assessment`,
#'   `proposal`, `config`, `final` ([final_assessment()], or `NULL` without an
#'   evidence table), `evidence` ([evidence_for_engine()], or `NULL`) and
#'   `records` (record IDs with `include` and the exclusion reason).
#' @seealso [assess_species()], [batch_assess()]
#' @export
assess_taxon <- function(occ = NULL, species, config, evaluate_pop = FALSE,
                         evidence = list(), display_grid_size = 2000, metrics = NULL,
                         effort = NULL) {
  ex <- NULL
  notes <- character(0)
  if (is.data.frame(evidence)) {
    ex <- evidence_for_engine(evidence, species)
    # A table without rows for this taxon is no evidence for it.
    if (!nrow(ex$entries)) ex <- NULL
    evidence <- list()
  }
  if (!is.null(ex)) {
    recompute <- FALSE
    if (nrow(ex$records)) {
      if (is.null(occ)) {
        notes <- c(notes, "record evidence not applied: no records passed")
      } else {
        occ <- apply_record_evidence(occ, ex$records)
        notes <- c(notes, attr(occ, "expert_record_notes"))
        recompute <- TRUE
      }
    }
    if (length(ex$config)) {
      config <- .update_config(config, ex$config)
      recompute <- TRUE
    }
    if (recompute && !is.null(occ)) metrics <- NULL
    evidence <- ex$engine
  }
  if (is.null(metrics)) {
    metrics <- taxon_metrics(occ, config, display_grid_size, effort = effort, taxon = species)
  }
  if (!is.null(metrics$failed)) {
    # No proposal without data: an error is never turned into DD or NE.
    proposal <- .proposal(NA_character_, NA_character_, support = "none",
                          needs = paste0("no proposal: ", metrics$failed, " - rerun this taxon"),
                          range = NA_character_)
    assessment <- NULL
  } else {
    inputs <- .inputs_from_metrics(
      eoo_km2 = metrics$eoo_new, aoo_km2 = metrics$aoo_new,
      n_records = metrics$n_records, n_current = metrics$n_current,
      year_last = metrics$year_last,
      aoo_change = metrics$aoo_change, eoo_change = metrics$eoo_change,
      locs_change = metrics$locs_change, clusters = metrics$clusters_new,
      pop_metrics = metrics$pop, evaluate_pop = evaluate_pop,
      tolerance = config$conventions$decline_tolerance, evidence = evidence,
      eoo_km2_min = metrics$eoo_new_min, eoo_km2_max = metrics$eoo_new_max,
      aoo_km2_min = metrics$aoo_new_min, aoo_km2_max = metrics$aoo_new_max,
      trend_est = metrics$trend_estimate
    )
    assessment <- assess_criteria(inputs, config, evaluate_pop = evaluate_pop)
    proposal <- categorise(assessment)
  }
  # The regional step (Regional Guidelines v4.0): step one may make the
  # taxon Not Applicable; step three adjusts the category from step two.
  regional <- if (is.na(proposal$category)) NULL else
    regional_assessment(proposal$category, proposal$criteria_iucn, ex, config)
  if (!is.null(regional) && !regional$eligibility$eligible) {
    proposal <- .proposal("NA", paste0("Not applicable: ", regional$eligibility$reason),
                          support = "evidence", range = "NA",
                          flags = "not assessed: step one of the regional procedure")
  }
  row <- .result_row(species, proposal, metrics$eoo_new, metrics$aoo_new,
                     metrics$clusters_new, metrics$aoo_change,
                     eoo_range = c(min = metrics$eoo_new_min, max = metrics$eoo_new_max),
                     aoo_range = c(min = metrics$aoo_new_min, max = metrics$aoo_new_max))
  row$Reference_Year <- config$reference_year
  te <- metrics$trend_estimate
  row$Data_Tier <- if (is.null(te)) NA_character_ else te$tier
  row$Trend_Estimate <- if (is.null(te)) NA_character_ else .format_change(te$change, te$tier)
  row$Trend_Source <- if (is.null(te)) NA_character_ else
    paste(stats::na.omit(c(te$tier_reason, te$source)), collapse = "; ")
  row$Trend_Sensitivity <- if (is.null(te)) NA_character_ else .format_sensitivity(te$sensitivity)
  row$Generation_Length <- config$generation_length
  row$Generation_Length_Source <- if (is.null(config$generation_length_source)) NA_character_ else
    config$generation_length_source
  final <- if (!is.null(ex) && !is.null(assessment)) final_assessment(assessment, proposal, ex) else NULL
  if (!is.null(final)) final$notes <- c(final$notes, notes)
  opt <- function(x) if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) NA_character_ else x[1]
  row$Regional_Category <- if (is.null(regional)) NA_character_ else regional$display
  row$Regional_Criteria <- if (is.null(regional)) NA_character_ else opt(regional$criteria)
  row$Regional_Step <- if (is.null(regional)) NA_character_ else
    paste(c(regional$eligibility$reason, regional$adjustment$trail), collapse = "; ")
  row$Global_Category <- if (is.null(regional)) NA_character_ else
    opt(trimws(paste(c(regional$global$category, regional$global$criteria)[
      !is.na(c(regional$global$category, regional$global$criteria))], collapse = " ")))
  row$Global_Population_Share <- if (is.null(regional)) NA_character_ else regional$global_share
  row$Final_Category <- if (is.null(final)) NA_character_ else final$display
  row$Final_Claimed <- if (is.null(final)) NA_character_ else final$claimed
  row$Final_Status <- if (is.null(final)) NA_character_ else final$status
  row$Final_Issues <- if (is.null(final)) NA_character_ else paste(final$issues, collapse = "; ")
  row$Records_Excluded <- if (is.null(metrics$n_excluded)) 0L else metrics$n_excluded
  row$Snapshot <- if (is.null(metrics$snapshot)) NA_character_ else metrics$snapshot
  row <- cbind(row, .issue_columns(metrics$issues))
  list(result = row, metrics = metrics, assessment = assessment,
       proposal = proposal, config = config, final = final, evidence = ex,
       regional = regional, records = .record_list(occ))
}

# A record belongs to a window only if its whole date range does
# (`occ` with the columns of add_year_column()).
.window_records <- function(occ, a, b) {
  keep <- !is.na(occ$year_min) & occ$year_min >= a & !is.na(occ$year_max) & occ$year_max <= b
  suppressWarnings(occ[keep, , drop = FALSE])
}

# The records behind an assessment, for the species card: IDs, dates and
# whether each was used, with the reason when not.
.record_list <- function(occ) {
  if (is.null(occ) || !nrow(occ)) return(NULL)
  d <- if (inherits(occ, "sf")) sf::st_drop_geometry(occ) else as.data.frame(occ)
  keep <- intersect(c("ID_NALEZ", "DRUH", "year_min", "year_max", "precision_m", "geom_type",
                      "include", "exclusion_reason", "flag_expert"), names(d))
  d[, keep, drop = FALSE]
}

.ISSUE_FIELDS <- c("download", "records", "aoo", "eoo", "locations", "trend", "population")

.issue_columns <- function(issues) {
  vals <- lapply(.ISSUE_FIELDS, function(k) {
    v <- issues[[k]]
    if (is.null(v) || !nzchar(v)) NA_character_ else v
  })
  stats::setNames(as.data.frame(vals, stringsAsFactors = FALSE), paste0("issue_", .ISSUE_FIELDS))
}

# Expert evidence from the loose arguments assess_species() and
# batch_assess() have always passed through `...`.
.evidence_from_args <- function(args) {
  # An evidence table (one file for many taxa) is passed on whole; each
  # taxon's rows are picked in assess_taxon().
  if (is.data.frame(args$evidence)) return(args$evidence)
  ev <- if (is.null(args$evidence)) list() else args$evidence
  if (!is.null(args$a_criteria)) ev$reduction_types <- args$a_criteria
  if (!is.null(args$severely_fragmented)) ev$severely_fragmented <- args$severely_fragmented
  if (!is.null(args$threat_locations)) ev$threat_locations <- args$threat_locations
  if (!is.null(args$d2_threat)) {
    if (isTRUE(args$d2_threat)) {
      warning("`d2_threat = TRUE` does not specify a threat (Guidelines 8); describe it. ",
              "Treated as unknown.", call. = FALSE)
    }
    ev$threat <- args$d2_threat
  }
  if (!is.null(args$habitat_decline)) {
    dec <- if (is.null(ev$decline)) logical(0) else ev$decline
    if (!"iii" %in% names(dec)) dec <- c(dec, iii = as.logical(args$habitat_decline)[1])
    ev$decline <- dec
  }
  ev
}

.config_from_args <- function(args, ...) {
  keep <- intersect(names(args), c("generation_length", "generation_length_source", "trend_shift",
                                   "decline_tolerance", "nt_margin_a",
                                   "nt_margin_c", "re_candidate_years", "d2_aoo_km2", "d2_locations",
                                   "colonizer_years", "regional_filter_share", "trend"))
  cfg <- args[keep]
  if (!is.null(args$re_threshold_years)) cfg$re_candidate_years <- args$re_threshold_years
  do.call(ndopred_config, c(list(...), cfg))
}

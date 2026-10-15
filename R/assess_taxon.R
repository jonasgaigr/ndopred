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
#' @return A list of metrics plus `issues`, a named list giving the reason
#'   for every value that could not be computed, and `failed` (a reason)
#'   when no assessment is possible at all, e.g. after a failed download.
#' @seealso [assess_taxon()]
#' @export
taxon_metrics <- function(occ, config, display_grid_size = 2000,
                          mature_terms = get_mature_terms()) {
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
    trend = NULL, pop = NULL, windows = w, display_grid_size = display_grid_size,
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
  # A record belongs to a window only if its whole date range does.
  in_window <- function(a, b) {
    keep <- !is.na(occ$year_min) & occ$year_min >= a & !is.na(occ$year_max) & occ$year_max <= b
    suppressWarnings(occ[keep, , drop = FALSE])
  }
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

  pop <- capture("population", calculate_pop_metrics(
    occ, window_years = w$trend_end - w$trend_start + 1, year_now = w$trend_end,
    mature_terms = mature_terms
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
#' [categorise()].
#'
#' @param occ An `sf` object from [get_assessment_data()]. Ignored when
#'   `metrics` is given.
#' @param species Character. Taxon name.
#' @param config Object from [ndopred_config()].
#' @param evaluate_pop Logical. Whether NDOP counts may be used as
#'   indications for criteria A(b), B(b)(v), B(c)(iv), C and D.
#' @param evidence List of expert evidence; see [assessment_inputs()].
#' @param display_grid_size Numeric. Cell size in metres of the display AOO.
#' @param metrics Optional result of [taxon_metrics()], to reuse.
#' @return A list: `result` (one-row data frame, see [summarize_assessment()]
#'   for the columns, plus `Reference_Year` and `issue_*` reason codes),
#'   `metrics`, `assessment`, `proposal` and `config`.
#' @seealso [assess_species()], [batch_assess()]
#' @export
assess_taxon <- function(occ = NULL, species, config, evaluate_pop = FALSE,
                         evidence = list(), display_grid_size = 2000, metrics = NULL) {
  if (is.null(metrics)) metrics <- taxon_metrics(occ, config, display_grid_size)
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
      aoo_km2_min = metrics$aoo_new_min, aoo_km2_max = metrics$aoo_new_max
    )
    assessment <- assess_criteria(inputs, config, evaluate_pop = evaluate_pop)
    proposal <- categorise(assessment)
  }
  row <- .result_row(species, proposal, metrics$eoo_new, metrics$aoo_new,
                     metrics$clusters_new, metrics$aoo_change,
                     eoo_range = c(min = metrics$eoo_new_min, max = metrics$eoo_new_max),
                     aoo_range = c(min = metrics$aoo_new_min, max = metrics$aoo_new_max))
  row$Reference_Year <- config$reference_year
  row$Records_Excluded <- if (is.null(metrics$n_excluded)) 0L else metrics$n_excluded
  row$Snapshot <- if (is.null(metrics$snapshot)) NA_character_ else metrics$snapshot
  row <- cbind(row, .issue_columns(metrics$issues))
  list(result = row, metrics = metrics, assessment = assessment,
       proposal = proposal, config = config)
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
  keep <- intersect(names(args), c("generation_length", "decline_tolerance", "nt_margin_a",
                                   "nt_margin_c", "re_candidate_years", "d2_aoo_km2", "d2_locations"))
  cfg <- args[keep]
  if (!is.null(args$re_threshold_years)) cfg$re_candidate_years <- args$re_threshold_years
  do.call(ndopred_config, c(list(...), cfg))
}

# Turns computed metrics into engine inputs. Everything derived from the
# records is an *indication*; only the `evidence` list carries evidence.
# Shared by assess_taxon() and the deprecated summarize_assessment(), so the
# two cannot read the same numbers differently.
.inputs_from_metrics <- function(eoo_km2, aoo_km2, n_records, n_current, year_last,
                                 aoo_change, eoo_change, locs_change, clusters,
                                 pop_metrics, evaluate_pop, tolerance, evidence = list(),
                                 eoo_km2_min = NA, eoo_km2_max = NA,
                                 aoo_km2_min = NA, aoo_km2_max = NA,
                                 trend_est = NULL) {
  num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) == 0) NA_real_ else v[1]
  }
  # A measured change indicates a continuing decline only once it clears the
  # sampling-noise tolerance (ndopred_config(decline_tolerance)); an unknown
  # change indicates nothing either way.
  declining <- function(x) if (is.na(x)) NA else (x < 0 && abs(x) >= tolerance)

  aoo_change <- num(aoo_change)
  eoo_change <- num(eoo_change)
  pm <- if (is.null(pop_metrics)) list() else pop_metrics
  pop_rate <- if (evaluate_pop) num(pm$decline_rate) else NA_real_
  total <- if (evaluate_pop) num(pm$total_mature) else NA_real_
  biggest <- if (evaluate_pop) num(pm$max_subpop) else NA_real_
  fluct <- NA
  if (evaluate_pop) {
    if (!is.null(pm$extreme_fluctuation)) {
      fluct <- as.logical(pm$extreme_fluctuation)[1]
    } else if (!is.na(num(pm$fluct_ratio))) {
      fluct <- num(pm$fluct_ratio) >= 10
    }
  }

  # Criterion A indication: the larger of the spatial decline (basis c) and
  # the count-based decline (basis b). The count-based one counts only past
  # the tolerance, and wins ties on category, as before. With a trend
  # estimate (trend_estimate()), the spatial decline is its tiered estimate
  # with its interval; otherwise the raw change.
  te <- trend_est
  has_te <- !is.null(te) && !is.null(te$change) && !is.na(te$change[["best"]])
  if (has_te) {
    spatial <- .as_reduction(te$change)
    spatial_basis <- te$basis
    spatial_source <- te$source
  } else {
    spatial <- suppressWarnings(min(c(aoo_change, eoo_change), na.rm = TRUE))
    spatial <- if (is.finite(spatial)) max(0, -spatial) else NA_real_
    spatial_basis <- "c"
    spatial_source <- "raw AOO/EOO change between windows (not effort-corrected)"
  }
  pop_red <- if (isTRUE(declining(pop_rate))) -pop_rate else NA_real_
  a_level <- function(v) {
    v <- .range3(v)[["best"]]
    if (is.na(v)) return(0L)
    hit <- which(v >= .A_THRESH$A2[.LEVELS])
    if (length(hit)) 4L - hit[1] else 0L
  }
  use_pop <- !is.na(pop_red) && (is.na(.range3(spatial)[["best"]]) || a_level(pop_red) >= a_level(spatial))
  reduction <- if (use_pop) pop_red else spatial

  indication <- list(
    reduction = reduction,
    reduction_basis = if (use_pop) "b" else spatial_basis,
    reduction_source = if (use_pop) "opportunistic NDOP counts (minimum observed)" else spatial_source,
    data_tier = if (has_te) te$tier else NA_character_,
    clusters = num(clusters),
    # B(b)(i), (ii): with a trend estimate, a decline only where its whole
    # interval says so (trend_estimate()); otherwise the raw change.
    decline = c(i = if (has_te) te$decline_eoo else declining(eoo_change),
                ii = if (has_te) te$decline_aoo else declining(aoo_change),
                iv = declining(num(locs_change)),
                v = if (evaluate_pop) declining(pop_rate) else NA),
    fluctuation = c(iv = fluct),
    # The census range (calculate_pop_metrics()): most recent counts, with
    # the latest year and the per-locality maxima as bounds.
    mature = if (is.na(total)) NA_real_ else c(min = num(pm$total_mature_min), best = total,
                                               max = num(pm$total_mature_max)),
    mature_decline_estimated = evaluate_pop && isTRUE(pm$decline_estimated),
    mature_decline = pop_rate,
    mature_decline_years = num(pm$decline_period_years),
    largest_subpop = biggest,
    prop_largest = if (!is.na(total) && total > 0 && !is.na(biggest)) biggest / total else NA_real_
  )
  assessment_inputs(eoo_km2 = eoo_km2, aoo_km2 = aoo_km2, n_records = n_records,
                    n_current = n_current, year_last = year_last,
                    indication = indication, evidence = evidence,
                    eoo_km2_min = eoo_km2_min, eoo_km2_max = eoo_km2_max,
                    aoo_km2_min = aoo_km2_min, aoo_km2_max = aoo_km2_max)
}

# One-row result shared by summarize_assessment(), assess_taxon(),
# assess_species() and batch_assess().
.result_row <- function(species, proposal, eoo_km2, aoo_km2, locations, trend_perc,
                        eoo_range = NULL, aoo_range = NULL) {
  na_or <- function(x, f) if (is.null(x) || length(x) == 0 || is.na(x[1])) NA_real_ else f(x[1])
  r2 <- function(v) round(v, 2)
  bound <- function(rng, k, fallback) if (is.null(rng)) na_or(fallback, r2) else na_or(rng[[k]], r2)
  data.frame(
    Species = species,
    Category = proposal$category,
    Criteria = proposal$criteria,
    EOO_km2 = na_or(eoo_km2, r2),
    AOO_km2 = na_or(aoo_km2, r2),
    # Bounds from positional uncertainty (Guidelines 3.2, 4.10.8): observed
    # AOO is itself a lower bound on the true AOO.
    EOO_km2_min = bound(eoo_range, "min", eoo_km2),
    EOO_km2_max = bound(eoo_range, "max", eoo_km2),
    AOO_km2_min = bound(aoo_range, "min", aoo_km2),
    AOO_km2_max = bound(aoo_range, "max", aoo_km2),
    Locations = na_or(locations, as.numeric),
    Trend_Perc = na_or(trend_perc, function(v) round(v, 1)),
    Criteria_IUCN = proposal$criteria_iucn,
    Support = proposal$support,
    Needs_Evidence = paste(proposal$needs, collapse = "; "),
    Category_Range = if (is.na(proposal$range)) NA_character_ else proposal$range,
    # AOPK, D7: the range holds a non-threatened and a threatened category.
    Uncertain = if (is.na(proposal$category)) NA else identical(proposal$uncertain, TRUE),
    Flags = paste(proposal$flags, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

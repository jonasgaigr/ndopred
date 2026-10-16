#' Compare Proposals With Another List of Categories
#'
#' Sets the proposals beside the categories of another list, such as the
#' current national Red List or categories assigned by experts, and sorts
#' the taxa by where they agree and where they do not. **Neither side is
#' taken as the truth.** Experts often depart from the IUCN criteria (AOPK,
#' D17), and a proposal rests on what the records show and the evidence
#' given. A disagreement is therefore a question to settle against the
#' criteria and the evidence. It is not an error of the proposal, and it
#' is not an error of the list.
#'
#' @section Outcomes:
#' For taxa where both sides give CR, EN, VU, NT or LC:
#' * `exact`: the same category;
#' * `same status`: both threatened (CR, EN, VU) or both not (NT, LC);
#' * `proposal threatened only`: the proposal threatened, the list NT or LC;
#' * `reference threatened only`: the list threatened, the proposal NT or LC.
#'
#' Any other pair (DD, NE, NA, RE, EX, EW) is `not comparable`, and a taxon
#' the proposals lack is `no proposal`; both are kept in the cross table. A
#' degree sign (regional adjustment) is ignored. `range_covers` says whether
#' the list's category lies inside the proposal's plausible range.
#'
#' An expert's final category is checked against the criteria by
#' [final_assessment()], which flags where it goes beyond what the evidence
#' supports.
#'
#' @param proposals A data frame from [batch_assess()] (or rows of
#'   [assess_taxon()]), with `Species`, `Category` and optionally
#'   `Regional_Category`, `Category_Range` and `Uncertain`.
#' @param reference A data frame, or the path to a CSV file, with one row
#'   per taxon: `taxon` and `category`, and optionally `group` (to summarise
#'   by group) and `criteria`.
#' @param column The proposal column compared: `"Regional_Category"` (the
#'   category after the regional step, falling back to `Category`),
#'   `"Category"` or `"Final_Category"`.
#' @return A list: `taxa` (one row per reference taxon with both categories,
#'   the outcome and `range_covers`), `summary` (counts and shares, overall
#'   and per `group`), and `cross` (a table of proposed against reference
#'   categories).
#' @seealso [filter_stability()], [final_assessment()],
#'   `inst/validation/check_proposals.R`
#' @export
compare_categories <- function(proposals, reference,
                               column = c("Regional_Category", "Category", "Final_Category")) {
  column <- match.arg(column)
  reference <- .read_reference(reference)
  if (!"Species" %in% names(proposals)) stop("`proposals` needs a Species column", call. = FALSE)
  idx <- match(reference$taxon, proposals$Species)
  col <- function(nm) if (nm %in% names(proposals)) as.character(proposals[[nm]])[idx] else
    rep(NA_character_, length(idx))
  proposed <- col(column)
  if (column == "Regional_Category") proposed <- ifelse(is.na(proposed), col("Category"), proposed)
  proposed <- .plain_category(proposed)
  listed <- .plain_category(reference$category)
  range <- col("Category_Range")
  taxa <- data.frame(
    taxon = reference$taxon,
    group = reference$group,
    proposed = proposed,
    reference = listed,
    outcome = .outcome(proposed, listed),
    range = range,
    range_covers = .range_covers(range, listed),
    uncertain = as.logical(col("Uncertain")),
    stringsAsFactors = FALSE
  )
  taxa$outcome[is.na(idx)] <- "no proposal"
  groups <- list(all = taxa)
  g <- taxa$group
  if (any(!is.na(g))) groups <- c(groups, split(taxa[!is.na(g), ], g[!is.na(g)]))
  summary <- do.call(rbind, lapply(names(groups), function(g) .comparison_summary(groups[[g]], g)))
  lv <- c("CR", "EN", "VU", "NT", "LC", "DD", "NE", "NA", "RE", "EW", "EX")
  lev <- function(x) factor(ifelse(is.na(x), "(none)", x),
                            levels = c(lv, setdiff(unique(stats::na.omit(x)), lv), "(none)"))
  cross <- table(proposed = lev(taxa$proposed), reference = lev(taxa$reference))
  list(taxa = taxa, summary = summary, cross = cross)
}

.THREAT <- c("CR", "EN", "VU")
.NON_THREAT <- c("NT", "LC")

.read_reference <- function(reference) {
  if (is.character(reference) && length(reference) == 1) {
    reference <- utils::read.csv(reference, stringsAsFactors = FALSE, na.strings = "",
                                 fileEncoding = "UTF-8")
  }
  reference <- as.data.frame(reference, stringsAsFactors = FALSE)
  miss <- setdiff(c("taxon", "category"), names(reference))
  if (length(miss)) stop("reference table lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(reference$taxon)) {
    stop("reference table has taxa twice: ",
         paste(unique(reference$taxon[duplicated(reference$taxon)]), collapse = ", "), call. = FALSE)
  }
  known <- c(.CATS, "DD", "NE", "NA", "RE", "EW", "EX")
  bad <- setdiff(stats::na.omit(.plain_category(reference$category)), known)
  if (length(bad)) stop("unknown reference categories: ", paste(bad, collapse = ", "), call. = FALSE)
  if (!"group" %in% names(reference)) reference$group <- NA_character_
  reference
}

# Category codes without the degree sign of a regional adjustment or a
# (PE)/(PEW) tag.
.plain_category <- function(x) {
  x <- trimws(gsub("\u00b0", "", as.character(x), fixed = TRUE))
  x <- sub("[(].*$", "", x)
  x[!nzchar(x)] <- NA_character_
  x
}

.outcome <- function(proposed, listed) {
  ok <- proposed %in% c(.THREAT, .NON_THREAT) & listed %in% c(.THREAT, .NON_THREAT)
  p_thr <- proposed %in% .THREAT
  l_thr <- listed %in% .THREAT
  ifelse(!ok, "not comparable",
         ifelse(proposed == listed, "exact",
                ifelse(p_thr == l_thr, "same status",
                       ifelse(p_thr, "proposal threatened only", "reference threatened only"))))
}

.range_covers <- function(range, assessed) {
  vapply(seq_along(range), function(i) {
    r <- range[i]
    a <- assessed[i]
    if (is.na(r) || is.na(a)) return(NA)
    ends <- strsplit(r, "-", fixed = TRUE)[[1]]
    if (!all(ends %in% .CATS) || !a %in% .CATS) return(a %in% ends)
    lo <- .rank(ends[1])
    hi <- .rank(ends[length(ends)])
    .rank(a) >= min(lo, hi) && .rank(a) <= max(lo, hi)
  }, logical(1))
}

# Counts and shares only: with neither side the truth, there is no
# sensitivity or specificity to report.
.comparison_summary <- function(t, group) {
  comp <- t[t$outcome %in% c("exact", "same status", "proposal threatened only",
                             "reference threatened only"), ]
  share <- function(k, n) if (n) round(k / n, 3) else NA_real_
  data.frame(
    group = group,
    taxa = nrow(t),
    comparable = nrow(comp),
    exact = sum(comp$outcome == "exact"),
    same_status = sum(comp$outcome %in% c("exact", "same status")),
    proposal_threatened_only = sum(comp$outcome == "proposal threatened only"),
    reference_threatened_only = sum(comp$outcome == "reference threatened only"),
    share_exact = share(sum(comp$outcome == "exact"), nrow(comp)),
    share_same_status = share(sum(comp$outcome %in% c("exact", "same status")), nrow(comp)),
    range_covers = share(sum(t$range_covers %in% TRUE), sum(!is.na(t$range_covers))),
    not_comparable = sum(t$outcome %in% c("not comparable", "no proposal")),
    stringsAsFactors = FALSE
  )
}

#' Record Filters for the Stability Test
#'
#' Alternative record filters for [filter_stability()], after the review
#' (section 9): drop unvalidated records, historical (literature and
#' collection) sources, imprecise positions, part of the data at random,
#' or one region at a time ([region_variants()]). Each variant is a function
#' of the flagged records ([get_assessment_data()]) returning `TRUE` for
#' the records it drops in addition to the baseline exclusions.
#'
#' @param drop_share Share of records dropped at random in `data_loss`.
#' @param seed Seed for `data_loss`.
#' @return A named list of functions: `baseline` (drops nothing),
#'   `no_unvalidated` (`VEROH = 0`), `no_less_credible` (`VEROH = 3`),
#'   `no_literature` (literature or collection records, `flag_literature`),
#'   `precise_only` (precision worse than 2 km, `flag_imprecise`) and
#'   `data_loss`.
#' @seealso [filter_stability()], [region_variants()]
#' @export
filter_variants <- function(drop_share = 0.1, seed = 1) {
  veroh <- function(occ) suppressWarnings(as.numeric(as.character(.col_of(occ, "VEROH"))))
  list(
    baseline = function(occ) rep(FALSE, nrow(occ)),
    no_unvalidated = function(occ) veroh(occ) %in% 0,
    no_less_credible = function(occ) veroh(occ) %in% 3,
    no_literature = function(occ) .col_of(occ, "flag_literature") %in% TRUE,
    precise_only = function(occ) .col_of(occ, "flag_imprecise") %in% TRUE,
    data_loss = function(occ) .with_seed(seed, stats::runif(nrow(occ)) < drop_share)
  )
}

#' Leave-One-Region-Out Filters
#'
#' One variant per region, dropping the records inside it, to test whether
#' a proposal rests on one part of the country (review section 9).
#'
#' @param regions An `sf` polygon layer, e.g. the regions (kraje) from
#'   `giscoR::gisco_get_nuts(country = "CZ", nuts_level = 3)`.
#' @param name_col The column with the region names.
#' @return A named list of variants (`without_<name>`) for
#'   [filter_stability()].
#' @export
region_variants <- function(regions, name_col) {
  stopifnot(inherits(regions, "sf"), name_col %in% names(regions))
  vars <- lapply(seq_len(nrow(regions)), function(i) {
    region <- sf::st_geometry(regions)[i]
    function(occ) {
      inside <- lengths(suppressMessages(sf::st_intersects(
        sf::st_transform(sf::st_geometry(occ), sf::st_crs(region)), region))) > 0
      inside
    }
  })
  stats::setNames(vars, paste0("without_", gsub("[^A-Za-z0-9]+", "_", regions[[name_col]])))
}

#' Stability of Proposals Under Alternative Record Filters
#'
#' Reassesses each taxon with each record filter of `variants` and reports
#' where the proposed category, or the threat status, changes (review
#' section 9). Records dropped by a variant are marked excluded with the
#' reason `"variant: <name>"`; the snapshot is not changed.
#'
#' @param occ An `sf` object from [get_assessment_data()], or a named list
#'   of them (names are the taxa).
#' @param config Object from [ndopred_config()].
#' @param taxon Taxon name when `occ` is a single object.
#' @param variants A named list of filters ([filter_variants()],
#'   [region_variants()]); one should be `baseline`.
#' @param ... Passed to [assess_taxon()] (e.g. `evidence`, `effort`).
#' @return A list: `results` (one row per taxon and variant: records used,
#'   records dropped by the variant, category, criteria, range,
#'   `changed` (category differs from the baseline) and `status_changed`
#'   (threatened against not threatened)), and `summary` (per taxon:
#'   the categories seen, `stable`, `status_stable` and the variants that
#'   changed the category).
#' @seealso [compare_categories()]
#' @export
filter_stability <- function(occ, config, taxon = NULL, variants = filter_variants(), ...) {
  if (inherits(occ, "sf")) {
    if (is.null(taxon)) stop("`taxon` is needed for a single taxon", call. = FALSE)
    occ <- stats::setNames(list(occ), taxon)
  }
  if (is.null(names(variants)) || any(!nzchar(names(variants)))) {
    stop("`variants` must be a named list", call. = FALSE)
  }
  base <- if ("baseline" %in% names(variants)) "baseline" else names(variants)[1]
  rows <- list()
  for (sp in names(occ)) {
    o <- occ[[sp]]
    if (!"include" %in% names(o)) o$include <- TRUE
    if (!"exclusion_reason" %in% names(o)) o$exclusion_reason <- NA_character_
    for (v in names(variants)) {
      drop <- as.logical(variants[[v]](o)) %in% TRUE
      ov <- o
      newly <- drop & ov$include %in% TRUE
      ov$include[drop] <- FALSE
      ov$exclusion_reason[newly] <- paste0("variant: ", v)
      res <- tryCatch(assess_taxon(ov, sp, config, ...)$result, error = function(e) {
        data.frame(Species = sp, Category = NA_character_, Criteria_IUCN = NA_character_,
                   Category_Range = NA_character_, Flags = conditionMessage(e), stringsAsFactors = FALSE)
      })
      rows[[length(rows) + 1]] <- data.frame(
        Taxon = sp, Variant = v,
        Records_Used = sum(ov$include %in% TRUE),
        Records_Dropped = sum(newly),
        Category = res$Category,
        Criteria_IUCN = res$Criteria_IUCN,
        Category_Range = res$Category_Range,
        stringsAsFactors = FALSE
      )
    }
  }
  results <- do.call(rbind, rows)
  ref <- results[results$Variant == base, c("Taxon", "Category")]
  base_cat <- ref$Category[match(results$Taxon, ref$Taxon)]
  thr <- function(x) ifelse(is.na(x), NA, x %in% .THREAT)
  results$changed <- !unname(mapply(identical, results$Category, base_cat))
  results$status_changed <- !unname(mapply(identical, thr(results$Category), thr(base_cat)))
  summary <- do.call(rbind, lapply(split(results, factor(results$Taxon, levels = names(occ))), function(r) {
    data.frame(
      Taxon = r$Taxon[1],
      Baseline = r$Category[r$Variant == base][1],
      Categories = paste(unique(ifelse(is.na(r$Category), "(none)", r$Category)), collapse = ", "),
      stable = !any(r$changed),
      status_stable = !any(r$status_changed),
      changed_by = paste(r$Variant[r$changed], collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary) <- NULL
  list(results = results, summary = summary)
}

.col_of <- function(occ, nm) {
  if (nm %in% names(occ)) {
    if (inherits(occ, "sf")) sf::st_drop_geometry(occ)[[nm]] else occ[[nm]]
  } else rep(NA, nrow(occ))
}

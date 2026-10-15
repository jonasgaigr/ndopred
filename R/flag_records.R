#' Flag NDOP Records Without Deleting Any
#'
#' Adds one logical `flag_*` column per test, the first `exclusion_reason`
#' that applies, an `include` column, the record's date range (`year_min`,
#' `year_max`) and its positional precision (`precision_m`). No record is
#' removed: downstream metrics use `include`, and excluded records stay
#' available, e.g. negative findings as non-detections.
#'
#' @section Exclusion rules:
#' Decided with AOPK on 2026-10-15 (gap analysis, decisions D2, D3, D8, D13):
#' \describe{
#'   \item{Taxon}{A record whose `DRUH` differs from the queried name is
#'     another taxon (hybrid, *s. l.*, subspecies) and is excluded. Names are
#'     matched exactly.}
#'   \item{Credibility (`VEROH`)}{0 = not yet validated (used; a proposed
#'     validation mark in `VALIDACE` is only flagged, the process being
#'     unfinished); 1 = credible (used); 3 = less credible; 6 = erroneous,
#'     to be deleted; 9 = erroneous, to be corrected (both excluded).
#'     Records with 3 are excluded unless more than
#'     `less_credible_share` of the taxon's records carry 3, in which case
#'     they are kept and flagged (a convention).}
#'   \item{Negative findings}{`NEGATIV = 1` is excluded from presence
#'     metrics and kept as a non-detection.}
#'   \item{Position}{No coordinates (hidden or missing), no cadastral unit
#'     (probably outside Czechia), or coordinates outside the Czech bounding
#'     box: excluded. `CXPRESNOST` is the positional precision in metres; 0
#'     means exact.}
#'   \item{Date}{An unparseable date is excluded. A date range is kept with
#'     `year_min` and `year_max`; time windows use the whole range.}
#' }
#' Literature or collection records and suspected duplicates (same taxon,
#' date and position) are flagged, never excluded.
#'
#' @param raw Data frame from `rndop::ndop_download()`.
#' @param species Character. The queried taxon name, or `NULL` to skip the
#'   taxon check.
#' @param less_credible_share Numeric. *Convention*: the share of `VEROH = 3`
#'   records above which they are kept (flagged) rather than excluded.
#' @return `raw` with the added columns. Attributes `less_credible_share`
#'   (the taxon's observed share of `VEROH = 3`) and
#'   `less_credible_retained`.
#' @seealso [get_assessment_data()]
#' @examples
#' raw <- data.frame(
#'   ID_NALEZ = 1:4, DRUH = c("A b", "A b", "A b", "A b x c"),
#'   X = c(-700000, NA, -701000, -702000), Y = -1080000,
#'   DATUM_OD = 20200601, DATUM_DO = c(20200601, 20200601, 20200601, 20211231),
#'   NEGATIV = c(0, 0, 1, 0), KATASTR = "KU", VEROH = c(1, 0, 0, 1),
#'   CXPRESNOST = c(10, 0, 0, 3000)
#' )
#' f <- flag_records(raw, species = "A b")
#' f[, c("ID_NALEZ", "include", "exclusion_reason")]
#' @export
flag_records <- function(raw, species = NULL, less_credible_share = 0.10) {
  n <- nrow(raw)
  col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA, n)
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  chr <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }

  x <- num(col("X"))
  y <- num(col("Y"))
  druh <- chr(col("DRUH"))
  veroh <- num(col("VEROH"))
  validace <- chr(col("VALIDACE"))
  negativ <- num(col("NEGATIV"))
  katastr <- col("KATASTR")
  precision <- num(col("CXPRESNOST"))
  zdroj <- chr(col("ZDROJ"))

  year_min <- extract_year(col("DATUM_OD"))
  if ("ROK" %in% names(raw)) {
    rok <- num(raw$ROK)
    year_min[is.na(year_min)] <- rok[is.na(year_min)]
  }
  year_max <- extract_year(col("DATUM_DO"))
  year_max[is.na(year_max)] <- year_min[is.na(year_max)]

  flags <- list(
    flag_taxon_mismatch = if (!is.null(species) && "DRUH" %in% names(raw)) {
      nzchar(druh) & druh != trimws(species)
    } else rep(FALSE, n),
    flag_erroneous = !is.na(veroh) & veroh %in% c(6, 9),
    flag_less_credible = !is.na(veroh) & veroh == 3,
    flag_validation_proposed = nzchar(validace) & (is.na(veroh) | veroh == 0),
    flag_negative = !is.na(negativ) & negativ == 1,
    flag_no_coords = is.na(x) | is.na(y),
    flag_no_cadastre = !is.na(katastr) & trimws(as.character(katastr)) == "",
    # S-JTSK / Krovak East North bounding box of Czechia, with a margin.
    flag_coords_suspect = !(is.na(x) | is.na(y)) &
      (x == 0 | y == 0 | x < -910000 | x > -420000 | y < -1240000 | y > -920000),
    flag_date_unknown = is.na(year_min),
    flag_date_range = !is.na(year_min) & !is.na(year_max) & year_max != year_min,
    flag_imprecise = !is.na(precision) & precision > 2000,
    # A heuristic: an author list in capitals followed by a year, the form
    # NDOP uses for publications (e.g. "NOVAK J. (2008) ...").
    flag_literature = grepl("[A-Z\u00c1\u010c\u010e\u00c9\u011a\u00cd\u0147\u00d3\u0158\u0160\u0164\u00da\u016e\u00dd\u017d]{2,}.*\\(\\d{4}\\)", zdroj) &
      !grepl("Vlastn\u00ed n\u00e1lezy|iNaturalist", zdroj),
    flag_duplicate = rep(FALSE, n)
  )
  # Suspected duplicates: same taxon, start date and position (to 10 m);
  # the first occurrence is kept unflagged.
  key <- paste(druh, col("DATUM_OD"), round(x, -1), round(y, -1))
  flags$flag_duplicate <- !flags$flag_no_coords & duplicated(key)

  own <- !flags$flag_taxon_mismatch
  share <- if (any(own)) mean(flags$flag_less_credible[own]) else 0
  retain_lc <- share > less_credible_share

  reasons <- cbind(
    ifelse(flags$flag_taxon_mismatch, paste0("different taxon (DRUH: ", druh, ")"), NA),
    ifelse(flags$flag_erroneous, paste0("VEROH ", veroh, ": erroneous record"), NA),
    ifelse(flags$flag_less_credible & !retain_lc,
           sprintf("VEROH 3: less credible (%.1f%% of the taxon's records)", 100 * share), NA),
    ifelse(flags$flag_negative, "negative finding (kept as a non-detection)", NA),
    ifelse(flags$flag_no_coords, "no coordinates (hidden or missing)", NA),
    ifelse(flags$flag_no_cadastre, "no cadastral unit (probably outside Czechia)", NA),
    ifelse(flags$flag_coords_suspect, "coordinates outside Czechia or zero", NA),
    ifelse(flags$flag_date_unknown, "date not parseable", NA)
  )
  first <- if (n == 0) character(0) else
    apply(reasons, 1, function(r) { r <- r[!is.na(r)]; if (length(r)) r[1] else NA_character_ })

  out <- raw
  for (k in names(flags)) out[[k]] <- flags[[k]]
  out$year_min <- year_min
  out$year_max <- year_max
  out$precision_m <- ifelse(is.na(precision), 0, precision)
  out$exclusion_reason <- unname(first)
  out$include <- is.na(out$exclusion_reason)
  attr(out, "less_credible_share") <- share
  attr(out, "less_credible_retained") <- retain_lc
  out
}

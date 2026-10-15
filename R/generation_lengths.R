#' Generation Lengths by Taxonomic Group
#'
#' The table ndopred takes a generation length from when the assessor gives
#' none: one row per NDOP taxonomic group (the `KAT_TAX` of the records).
#' Generation length (Criteria v3.1 III.4; Guidelines §4.4) sets the period
#' of criterion A, "10 years or three generations, whichever is longer", and
#' the C1 periods. ndopred moves the comparison window back by that period
#' (see [ndopred_config()]).
#'
#' @section Status:
#' AOPK decided on 2026-10-15 that every group starts at 1 year, a
#' placeholder to be refined group by group (`status = "placeholder"`).
#' Any generation length up to 3.3 years gives the same periods as 1 year:
#' criterion A uses 10 years until three generations exceed it, and C1 uses
#' 3, 5 and 10 years until one, two and three generations exceed them. Groups
#' holding longer-lived taxa (most vertebrate groups, large bivalves, woody
#' plants) are where the refinement matters. A value given for one taxon
#' (single-taxon app, `generation_length` argument) always wins.
#'
#' @param path Path to a CSV with the same columns. Defaults to the
#'   `NDOPRED_GENERATION_LENGTHS` environment variable, else the table shipped
#'   with the package (`inst/extdata/generation_lengths.csv`).
#' @return `generation_lengths()`: a data frame with `ndop_code` (NDOP group
#'   code), `group_lat` and `kat_tax` (the group's Latin and Czech names in
#'   NDOP), `group_en`, `generation_length_years`, `status`, `source` and
#'   `note`.
#' @seealso [ndopred_config()]
#' @examples
#' gl <- generation_lengths()
#' head(gl[, c("kat_tax", "group_en", "generation_length_years", "status")])
#' generation_length_for("Amphibia")
#' @export
generation_lengths <- function(path = Sys.getenv("NDOPRED_GENERATION_LENGTHS")) {
  if (is.null(path) || !nzchar(path)) {
    path <- system.file("extdata", "generation_lengths.csv", package = "ndopred")
  }
  if (!file.exists(path)) stop("Generation-length table not found: ", path, call. = FALSE)
  tab <- utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  need <- c("kat_tax", "group_lat", "group_en", "generation_length_years", "status", "source")
  miss <- setdiff(need, names(tab))
  if (length(miss)) {
    stop("Generation-length table lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  gl <- suppressWarnings(as.numeric(tab$generation_length_years))
  if (any(is.na(gl) | gl <= 0)) {
    stop("Generation lengths must be positive numbers (rows: ",
         paste(which(is.na(gl) | gl <= 0), collapse = ", "), ").", call. = FALSE)
  }
  tab$generation_length_years <- gl
  attr(tab, "path") <- path
  tab
}

#' @rdname generation_lengths
#' @param group Character. A group as in NDOP (`KAT_TAX`, Czech), or its
#'   Latin or English name; matched case-insensitively.
#' @param table The table, from `generation_lengths()`.
#' @return `generation_length_for()`: a list with `years`, `group` (the
#'   matched `KAT_TAX`), `status` and `source`. A group not in the table gets
#'   1 year with status `"default"`.
#' @export
generation_length_for <- function(group, table = generation_lengths()) {
  key <- function(x) tolower(trimws(as.character(x)))
  g <- if (length(group)) key(group[1]) else NA_character_
  hit <- if (is.na(g) || !nzchar(g)) integer(0) else {
    which(key(table$kat_tax) == g | key(table$group_lat) == g | key(table$group_en) == g)
  }
  if (!length(hit)) {
    return(list(years = 1, group = if (length(group)) as.character(group[1]) else NA_character_,
                status = "default",
                source = "default 1 year: group not in the generation-length table"))
  }
  r <- table[hit[1], ]
  list(years = r$generation_length_years, group = r$kat_tax, status = r$status,
       source = sprintf("group table: %s (%s; %s)", r$kat_tax, r$status, r$source))
}

# The taxon's NDOP group: the most frequent KAT_TAX among its own records
# (records of other taxa returned by the query do not count).
.taxon_group <- function(occ) {
  if (is.null(occ) || !nrow(occ) || !"KAT_TAX" %in% names(occ)) return(NA_character_)
  own <- if ("flag_taxon_mismatch" %in% names(occ)) !(occ$flag_taxon_mismatch %in% TRUE) else TRUE
  v <- as.character(occ$KAT_TAX[own])
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}

# Generation length for one taxon: from its records' group, else the group
# given with the input (e.g. SKUPINA in the bulk sheet), else the default.
.taxon_generation_length <- function(occ, group_hint = NA_character_, table = generation_lengths()) {
  grp <- .taxon_group(occ)
  if (is.na(grp)) grp <- group_hint
  generation_length_for(grp, table)
}

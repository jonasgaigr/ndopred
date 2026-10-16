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
#' (single-taxon app, `generation_length` argument) always wins. The refined
#' values proposed to replace the placeholder are in
#' [generation_lengths_refined()] (opt-in, `set = "refined"`).
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
#' @param taxon Character. The taxon's name, used by the refined set only.
#' @param set `"placeholder"` (the group table above) or `"refined"` (the
#'   proposal of [generation_lengths_refined()]). Defaults to
#'   [generation_length_set()].
#' @return `generation_length_for()`: a list with `years`, `group` (the
#'   matched `KAT_TAX`), `status`, `source` and `level` (`"group"`, or for
#'   the refined set the row that matched: `"species"`, `"genus"`,
#'   `"family"` or `"group"`). A group not in the table gets 1 year with
#'   status `"default"`.
#' @export
generation_length_for <- function(group, table = generation_lengths(), taxon = NA_character_,
                                  set = generation_length_set()) {
  set <- match.arg(set, c("placeholder", "refined"))
  hit <- .group_row(group, table)
  if (set == "refined") return(.refined_generation_length(taxon, group, table, hit))
  if (!length(hit)) {
    return(list(years = 1, group = if (length(group)) as.character(group[1]) else NA_character_,
                status = "default", level = "default",
                source = "default 1 year: group not in the generation-length table"))
  }
  r <- table[hit[1], ]
  list(years = r$generation_length_years, group = r$kat_tax, status = r$status, level = "group",
       source = sprintf("group table: %s (%s; %s)", r$kat_tax, r$status, r$source))
}

.gl_key <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))

.group_row <- function(group, table) {
  g <- if (length(group)) .gl_key(group[1]) else NA_character_
  if (is.na(g) || !nzchar(g)) return(integer(0))
  which(.gl_key(table$kat_tax) == g | .gl_key(table$group_lat) == g | .gl_key(table$group_en) == g)
}

#' Refined Generation Lengths (Proposal)
#'
#' The proposed replacement for the 1-year placeholder of
#' [generation_lengths()], built by `methodology/generation_lengths/build.R`
#' (sources and method in the README there). Until AOPK adopts it, it is
#' used only when asked for: `generation_length_set("refined")`, the
#' `ndopred.generation_lengths` option, the `NDOPRED_GENERATION_LENGTH_SET`
#' environment variable, or the choice in the bulk app.
#'
#' A taxon is looked up in this order, within its NDOP group: the species
#' table (name or synonym, then the binomial of an infraspecific name), a
#' genus row, the family of its genus (AOPK checklist for plants, fungi,
#' lichens and bryophytes; the species table for vertebrates), and the
#' group default. Generation lengths of up to 3.3 years give the same
#' periods as 1 year.
#'
#' @return `generation_lengths_refined()`: a list with `species` (vertebrates
#'   and Habitats Directive taxa, one row each), `clades` (group, family and
#'   genus rows) and `genus_family` (genus to family by group).
#'   `generation_length_set()`: the set in use; with `set`, it is changed for
#'   the session and the previous one returned invisibly.
#' @seealso [generation_lengths()], [generation_length_for()]
#' @examples
#' gl <- generation_lengths_refined()
#' head(gl$species[, c("taxon", "generation_length_years", "method")])
#' generation_length_for("Obojživelníci", taxon = "Bombina bombina", set = "refined")
#' generation_length_for("Houby", taxon = "Boletus edulis", set = "refined")
#' @export
generation_lengths_refined <- function() {
  if (!is.null(.gl_cache$refined)) return(.gl_cache$refined)
  rd <- function(f) {
    p <- system.file("extdata", f, package = "ndopred")
    if (!nzchar(p)) stop("Refined generation-length table not found: ", f, call. = FALSE)
    utils::read.csv(p, fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  }
  out <- list(species = rd("generation_lengths_species.csv"),
              clades = rd("generation_lengths_clades.csv"),
              genus_family = rd("generation_lengths_genus_family.csv"))
  out$species$synonyms[is.na(out$species$synonyms)] <- ""
  .gl_cache$refined <- out
  out
}

.gl_cache <- new.env(parent = emptyenv())

.gl_paren <- function(...) {
  x <- c(...)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) paste0(" (", paste(x, collapse = "; "), ")") else ""
}

#' @rdname generation_lengths_refined
#' @param set `"placeholder"` or `"refined"`.
#' @export
generation_length_set <- function(set) {
  if (!missing(set)) {
    set <- match.arg(set, c("placeholder", "refined"))
    old <- options(ndopred.generation_lengths = set)
    return(invisible(if (is.null(old[[1]])) "placeholder" else old[[1]]))
  }
  s <- getOption("ndopred.generation_lengths")
  if (is.null(s)) s <- Sys.getenv("NDOPRED_GENERATION_LENGTH_SET", "placeholder")
  if (!nzchar(s)) s <- "placeholder"
  match.arg(s, c("placeholder", "refined"))
}

.refined_generation_length <- function(taxon, group, table, hit) {
  ref <- generation_lengths_refined()
  lat <- if (length(hit)) table$group_lat[hit[1]] else NA_character_
  kat <- if (length(hit)) table$kat_tax[hit[1]] else if (length(group)) as.character(group[1]) else NA_character_
  ans <- function(years, level, what) {
    list(years = years, group = kat, status = "refined", level = level,
         source = sprintf("refined table, %s: %s", level, what))
  }
  in_grp <- function(d) if (is.na(lat)) rep(TRUE, nrow(d)) else d$group_lat == lat
  t <- if (length(taxon)) .gl_key(taxon[1]) else NA_character_
  if (!is.na(t) && nzchar(t)) {
    sp <- ref$species[in_grp(ref$species), ]
    syn <- strsplit(.gl_key(sp$synonyms), "; ", fixed = TRUE)
    find <- function(n) which(.gl_key(sp$taxon) == n | vapply(syn, function(s) n %in% s, logical(1)))
    words <- strsplit(t, " ", fixed = TRUE)[[1]]
    tries <- unique(c(t, if (length(words) > 2) paste(words[1:2], collapse = " ")))
    for (n in tries) {
      i <- find(n)
      if (length(i) == 1) {
        r <- sp[i, ]
        what <- paste0(r$taxon, if (n != t) paste0(", binomial of ", taxon[1]) else "",
                       .gl_paren(r$method, r$source))
        return(ans(r$generation_length_years, "species", what))
      }
    }
    genus <- words[1]
    genus_shown <- sub(" .*", "", gsub("[[:space:]]+", " ", trimws(as.character(taxon[1]))))
    cl <- ref$clades[in_grp(ref$clades), ]
    one <- function(i) if (length(unique(cl$generation_length_years[i])) == 1) i[1] else integer(0)
    i <- one(which(cl$rank == "genus" & .gl_key(cl$clade) == genus))
    if (length(i)) return(ans(cl$generation_length_years[i], "genus",
                              paste0(cl$clade[i], .gl_paren(cl$basis[i], cl$note[i]))))
    gf <- ref$genus_family[in_grp(ref$genus_family) & .gl_key(ref$genus_family$genus) == genus, ]
    if (length(unique(gf$family)) == 1) {
      i <- one(which(cl$rank == "family" & .gl_key(cl$clade) == .gl_key(gf$family[1])))
      if (length(i)) return(ans(cl$generation_length_years[i], "family",
                                paste0(cl$clade[i], ", family of ", genus_shown, .gl_paren(cl$basis[i], cl$note[i]))))
    }
  }
  if (!is.na(lat)) {
    i <- which(ref$clades$group_lat == lat & ref$clades$rank == "group")
    if (length(i)) {
      r <- ref$clades[i[1], ]
      nm <- if (is.na(r$kat_tax)) r$group_lat else r$kat_tax
      return(ans(r$generation_length_years, "group", paste0(nm, .gl_paren(r$basis, r$note))))
    }
  }
  list(years = 1, group = kat, status = "default", level = "default",
       source = "default 1 year: taxon and group not in the refined tables")
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
# The refined set also looks the taxon up by name: the name queried, else
# the most frequent name among its own records.
.taxon_generation_length <- function(occ, group_hint = NA_character_, table = generation_lengths(),
                                     taxon = NA_character_, set = generation_length_set()) {
  grp <- .taxon_group(occ)
  if (is.na(grp)) grp <- group_hint
  if ((is.na(taxon[1]) || !nzchar(taxon[1])) && !is.null(occ) && nrow(occ) && "DRUH" %in% names(occ)) {
    own <- if ("flag_taxon_mismatch" %in% names(occ)) !(occ$flag_taxon_mismatch %in% TRUE) else TRUE
    v <- as.character(occ$DRUH[own])
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v)) taxon <- names(sort(table(v), decreasing = TRUE))[1]
  }
  generation_length_for(grp, table, taxon = taxon, set = set)
}

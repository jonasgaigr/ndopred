# Check the proposals for a list of taxa: their stability under alternative
# record filters, and optionally how they compare with another list of
# categories.
#
# For each taxon the script
#   1. proposes a category exactly as bulk mode does (assess_species() on
#      the latest stored NDOP snapshot, with the group's effort layer);
#   2. reassesses it under alternative record filters (filter_stability():
#      without unvalidated, less credible, literature and collection, or
#      imprecise records, with 10 % of the records dropped at random, and
#      optionally without one region at a time) and reports what changes
#      (review section 9);
#   3. with --compare, sets the proposals beside another list of categories
#      (compare_categories()), such as the current national Red List.
#      Neither side is taken as the truth: experts often depart from the
#      IUCN criteria (AOPK, D17), so every disagreement is a question to
#      settle against the criteria and the evidence.
#
# The taxa (CSV, UTF-8) have one row per taxon: `taxon` (the name exactly
# as in NDOP) and optionally `group`. A --compare list has `taxon` and
# `category`, and optionally `group`.
#
# Usage (from a shell):
#   Rscript check_proposals.R --taxa taxa.csv --year REFERENCE_YEAR
#       [--compare list.csv] [--out DIR] [--dir SNAPSHOT_DIR] [--regions]
#
# --regions adds leave-one-region-out filters for the 14 regions (kraje,
# NUTS 3) from giscoR, which downloads them once. Outputs in --out
# (default check_proposals/): proposals.csv, stability.csv,
# stability_summary.csv and, with --compare, comparison.csv,
# comparison_summary.csv and comparison_cross.csv. They hold categories,
# areas and counts only; the snapshots in --dir hold exact coordinates and
# observer names and stay outside the repository.

library(ndopred)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}
taxa_path <- opt("--taxa", NA)
if (is.na(taxa_path)) stop("give the taxa: --taxa taxa.csv")
year <- as.integer(opt("--year", NA))
if (is.na(year)) stop("give the reference year: --year 2026")
compare_path <- opt("--compare", NA)
out <- opt("--out", "check_proposals")
dir <- opt("--dir", default_snapshot_dir())
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write <- function(x, file) {
  utils::write.csv(x, file.path(out, file), row.names = FALSE, fileEncoding = "UTF-8")
}

taxa <- unique(utils::read.csv(taxa_path, stringsAsFactors = FALSE, na.strings = "",
                               fileEncoding = "UTF-8")$taxon)

# 1. Proposals, as in bulk mode (current window year-16 to year; the
# comparison window follows from the generation length).
occs <- list()
proposals <- do.call(rbind, lapply(taxa, function(sp) {
  message("proposal: ", sp)
  path <- latest_snapshot(sp, dir, download = TRUE)
  if (is.na(path)) {
    message("  no snapshot and the download failed")
    return(NULL)
  }
  occs[[sp]] <<- get_assessment_data(sp, snapshot = path)
  assess_species(sp, recent_start = year - 16, recent_end = year, snapshot = path,
                 effort_dir = dir)
}))
write(proposals, "proposals.csv")

# 2. Stability under alternative record filters.
variants <- filter_variants()
if ("--regions" %in% args) {
  regions <- giscoR::gisco_get_nuts(country = "CZ", nuts_level = 3, resolution = "01")
  variants <- c(variants, region_variants(regions, "NAME_LATN"))
}
group_of <- function(occ) {
  v <- as.character(occ$KAT_TAX)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) names(sort(table(v), decreasing = TRUE))[1] else NA_character_
}
stab <- lapply(names(occs), function(sp) {
  message("stability: ", sp)
  occ <- occs[[sp]]
  grp <- group_of(occ)
  gl <- generation_length_for(grp, taxon = sp)
  cfg <- ndopred_config(reference_year = year, generation_length = gl$years,
                        generation_length_source = gl$source)
  filter_stability(occ, cfg, sp, variants = variants, effort = effort_layer(grp, dir))
})
stab_summary <- do.call(rbind, lapply(stab, `[[`, "summary"))
write(do.call(rbind, lapply(stab, `[[`, "results")), "stability.csv")
write(stab_summary, "stability_summary.csv")

old <- options(width = 200)
cat("\nStability under alternative filters:\n")
print(stab_summary, row.names = FALSE)

# 3. Optionally, the comparison with another list.
if (!is.na(compare_path)) {
  v <- compare_categories(proposals, compare_path)
  write(v$taxa, "comparison.csv")
  write(v$summary, "comparison_summary.csv")
  write(as.data.frame.matrix(v$cross), "comparison_cross.csv")
  cat("\nComparison with", basename(compare_path), "(neither side taken as the truth):\n")
  print(v$summary, row.names = FALSE)
  cat("\nRows: proposed; columns: the list.\n")
  print(v$cross)
}
options(old)
message("written to: ", normalizePath(out))

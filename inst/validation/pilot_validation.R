# Pilot validation against blinded expert assessments (review section 9).
#
# Two pilot groups are recommended: one well recorded (e.g. butterflies)
# and one poorly recorded (e.g. saproxylic beetles). For each taxon in the
# expert table the script
#   1. proposes a category exactly as bulk mode does (assess_species() on
#      the latest stored NDOP snapshot, with the group's effort layer),
#   2. compares the proposals with the expert categories
#      (validate_against_experts(): false threatened and false
#      non-threatened counted separately, overall and per group),
#   3. reassesses each taxon under alternative record filters
#      (filter_stability(): unvalidated records, less credible records,
#      literature and collection records, imprecise positions, 10 % data
#      loss, and optionally one region at a time).
#
# The expert table (CSV, UTF-8) has one row per taxon:
#   taxon     name exactly as in NDOP
#   category  the expert's category (CR, EN, VU, NT, LC, DD, RE, NA, ...)
#   group     the pilot group, e.g. "butterflies" and "saproxylic beetles"
#   assessor  who assessed it (optional)
#   blinded   TRUE when the assessor did not see the proposal (optional,
#             strongly recommended)
#
# Usage (from a shell):
#   Rscript pilot_validation.R --experts experts.csv --year REFERENCE_YEAR
#       [--out DIR] [--dir SNAPSHOT_DIR] [--regions]
#
# --regions adds leave-one-region-out filters for the 14 regions (kraje,
# NUTS 3) from giscoR, which downloads them once. Outputs in --out
# (default pilot_validation/): proposals.csv, comparison.csv, summary.csv,
# confusion.csv, stability.csv and stability_summary.csv. They hold
# categories, areas and counts only; the snapshots in --dir hold exact
# coordinates and observer names and stay outside the repository.

library(ndopred)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}
experts_path <- opt("--experts", NA)
if (is.na(experts_path)) stop("give the expert table: --experts experts.csv")
out <- opt("--out", "pilot_validation")
dir <- opt("--dir", default_snapshot_dir())
year <- as.integer(opt("--year", NA))
if (is.na(year)) stop("give the reference year: --year 2026")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write <- function(x, file) {
  utils::write.csv(x, file.path(out, file), row.names = FALSE, fileEncoding = "UTF-8")
}

experts <- utils::read.csv(experts_path, stringsAsFactors = FALSE, na.strings = "",
                           fileEncoding = "UTF-8")
taxa <- unique(experts$taxon)

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

# 2. Comparison with the blinded expert categories.
v <- validate_against_experts(proposals, experts)
write(v$taxa, "comparison.csv")
write(v$summary, "summary.csv")
write(as.data.frame.matrix(v$confusion), "confusion.csv")

# 3. Stability under alternative record filters.
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
  gl <- generation_length_for(grp)
  cfg <- ndopred_config(reference_year = year, generation_length = gl$years,
                        generation_length_source = gl$source)
  filter_stability(occ, cfg, sp, variants = variants, effort = effort_layer(grp, dir))
})
write(do.call(rbind, lapply(stab, `[[`, "results")), "stability.csv")
write(do.call(rbind, lapply(stab, `[[`, "summary")), "stability_summary.csv")

old <- options(width = 200)
cat("\nComparison with the expert assessments:\n")
print(v$summary, row.names = FALSE)
cat("\nConfusion (rows: proposed, columns: assessed):\n")
print(v$confusion)
cat("\nStability under alternative filters:\n")
print(do.call(rbind, lapply(stab, `[[`, "summary")), row.names = FALSE)
options(old)
message("written to: ", normalizePath(out))

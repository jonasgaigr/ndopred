# Benchmark ndopred against ConR, redlistr and red on the pilot taxa.
#
# For each taxon the latest stored NDOP snapshot is used (or one is
# downloaded and stored), its current-window records are run through
# ndopred and the three tools, and the table of compare_tools() is written
# to a CSV file. See ?compare_tools for what each column compares.
#
# Usage (from a shell):
#   Rscript benchmark_tools.R [--out FILE] [--dir SNAPSHOT_DIR]
#                             [--year REFERENCE_YEAR] [TAXON ...]
#
# Defaults: --out benchmark_tools.csv, --dir default_snapshot_dir(),
# --year the current year, and the pilot taxa below. The snapshots hold
# exact coordinates and observer names: keep --dir outside the repository.
# The CSV holds only areas and counts.

library(ndopred)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}
flags <- which(args %in% c("--out", "--dir", "--year"))
taxa <- if (length(flags)) args[-c(flags, flags + 1)] else args
if (!length(taxa)) {
  taxa <- c("Onthophagus medius",   # dung beetle, few recent records
            "Protaetia cuprea",     # rose chafer, recorded widely
            "Bombina bombina")      # fire-bellied toad, well monitored
}
out <- opt("--out", "benchmark_tools.csv")
dir <- opt("--dir", default_snapshot_dir())
year <- as.integer(opt("--year", format(Sys.Date(), "%Y")))
cfg <- ndopred_config(reference_year = year)

rows <- lapply(taxa, function(sp) {
  message("benchmark: ", sp)
  res <- tryCatch({
    path <- latest_snapshot(sp, dir, download = TRUE)
    if (is.na(path)) stop("no snapshot and the download failed")
    occ <- get_assessment_data(sp, snapshot = path)
    compare_tools(occ, cfg, sp)
  }, error = function(e) {
    message("  failed: ", conditionMessage(e))
    NULL
  })
  res
})
tab <- do.call(rbind, rows)
utils::write.csv(tab, out, row.names = FALSE, fileEncoding = "UTF-8")

old <- options(width = 200)
print(tab[, c("Taxon", "Tool", "Points", "EOO_km2", "AOO_km2", "Locations",
              "Category_B", "EOO_diff_pct", "AOO_diff_pct")], row.names = FALSE)
options(old)
message("written: ", normalizePath(out))

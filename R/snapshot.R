#' Download and Store an NDOP Snapshot
#'
#' Downloads the records of one taxon with their record geometries
#' (points, lines and polygons, `rndop::ndop_download(locations = 2)`) and
#' keeps them exactly as downloaded, with the query, the download time, the
#' rndop and ndopred versions and the latest `DATI_INSERT`. Snapshots are
#' never edited: quality flags are added downstream by [flag_records()].
#'
#' @section Where snapshots are kept:
#' Snapshots hold exact (including hidden) coordinates and observer names, so
#' they stay outside the package repository. The default folder is
#' `Documents/ndop_snapshots` ([default_snapshot_dir()]; gap analysis,
#' decision D12).
#'
#' @param species Character. Taxon name exactly as in NDOP.
#' @param dir Character. Folder for the RDS file.
#' @param save Logical. Write the snapshot to `dir`.
#' @return An object of class `ndop_snapshot`: `table` (the records),
#'   `geometries` (the record geometries, or `NULL`) and `meta`, which
#'   includes `path` when saved and `download_error` when the download
#'   failed.
#' @seealso [read_ndop_snapshot()], [get_assessment_data()]
#' @examples
#' \dontrun{
#' snap <- ndop_snapshot("Onthophagus medius")
#' snap$meta$path
#' occ <- get_assessment_data("Onthophagus medius", snapshot = snap$meta$path)
#' }
#' @export
ndop_snapshot <- function(species, dir = default_snapshot_dir(), save = TRUE) {
  started <- Sys.time()
  dl <- tryCatch(rndop::ndop_download(species, locations = 2), error = function(e) e)
  table <- NULL
  geometries <- NULL
  error <- NULL
  if (inherits(dl, "error")) {
    error <- conditionMessage(dl)
  } else if (is.data.frame(dl)) {
    table <- dl
  } else if (is.list(dl) && length(dl) >= 1 && is.data.frame(dl[[1]])) {
    table <- dl[[1]]
    if (length(dl) >= 2) geometries <- dl[[2]]
  }
  dati <- if (!is.null(table) && "DATI_INSERT" %in% names(table)) {
    suppressWarnings(max(as.numeric(table$DATI_INSERT), na.rm = TRUE))
  } else NA_real_
  snap <- structure(list(
    table = table,
    geometries = geometries,
    meta = list(
      query = species,
      downloaded = format(started, "%Y-%m-%dT%H:%M:%S%z"),
      rndop = as.character(utils::packageVersion("rndop")),
      ndopred = as.character(utils::packageVersion("ndopred")),
      n_records = if (is.null(table)) 0L else nrow(table),
      max_dati_insert = if (is.finite(dati)) dati else NA_real_,
      download_error = error,
      path = NULL
    )
  ), class = "ndop_snapshot")
  if (save && is.null(error) && !is.null(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(dir, sprintf("%s_%s.rds", gsub("[^A-Za-z0-9]+", "_", species),
                                   format(started, "%Y%m%dT%H%M%S")))
    snap$meta$path <- path
    saveRDS(snap, path)
  }
  snap
}

#' Read a Stored NDOP Snapshot
#'
#' @param path Path to an RDS file written by [ndop_snapshot()].
#' @return The `ndop_snapshot` object.
#' @seealso [ndop_snapshot()]
#' @export
read_ndop_snapshot <- function(path) {
  snap <- readRDS(path)
  if (!inherits(snap, "ndop_snapshot")) stop("Not an ndop_snapshot: ", path, call. = FALSE)
  snap$meta$path <- path
  snap
}

#' Default Folder for NDOP Snapshots
#'
#' `Documents/ndop_snapshots` in the user's profile (gap analysis, decision
#' D12). Set the environment variable `NDOPRED_SNAPSHOT_DIR` to use another
#' folder.
#'
#' @return A path; the folder is created when the first snapshot is saved.
#' @export
default_snapshot_dir <- function() {
  env <- Sys.getenv("NDOPRED_SNAPSHOT_DIR")
  if (nzchar(env)) return(env)
  profile <- Sys.getenv("USERPROFILE")
  docs <- if (nzchar(profile)) file.path(profile, "Documents") else file.path(path.expand("~"), "Documents")
  file.path(docs, "ndop_snapshots")
}

#' The Latest Stored Snapshot of a Taxon
#'
#' Finds the most recent snapshot file of a taxon written by
#' [ndop_snapshot()], so that a rerun uses exactly the same records.
#'
#' @param species Character. Taxon name exactly as in NDOP.
#' @param dir Character. Folder of the snapshots.
#' @param download Logical. Download and store a snapshot when there is
#'   none.
#' @return The path, or `NA` when there is no snapshot (and none was
#'   downloaded, or the download failed).
#' @export
latest_snapshot <- function(species, dir = default_snapshot_dir(), download = FALSE) {
  stem <- gsub("[^A-Za-z0-9]+", "_", species)
  files <- list.files(dir, paste0("^", stem, "_[0-9]{8}T[0-9]{6}[.]rds$"), full.names = TRUE)
  if (length(files)) return(sort(files, decreasing = TRUE)[1])
  if (!download) return(NA_character_)
  path <- ndop_snapshot(species, dir = dir)$meta$path
  if (is.null(path)) NA_character_ else path
}

#' @export
print.ndop_snapshot <- function(x, ...) {
  m <- x$meta
  cat("<ndop_snapshot>", m$query, "-", m$n_records, "records, downloaded", m$downloaded, "\n")
  if (!is.null(m$path)) cat("  file:", m$path, "\n")
  if (!is.null(m$download_error)) cat("  download error:", m$download_error, "\n")
  invisible(x)
}

# The record geometries as one sf layer (EPSG:5514): `ID_LOKAL` and geometry.
# rndop returns a list of layers keyed by `idx_nd_lok`, which equals the
# table's ID_LOKAL.
.snapshot_geometries <- function(geometries) {
  if (is.null(geometries)) return(NULL)
  layers <- if (inherits(geometries, "sf")) list(geometries) else geometries
  layers <- lapply(Filter(function(l) inherits(l, "sf") && nrow(l) > 0, layers), function(l) {
    id <- names(l)[tolower(names(l)) %in% c("idx_nd_lok", "id_lokal")][1]
    if (is.na(id)) return(NULL)
    out <- sf::st_sf(ID_LOKAL = suppressWarnings(as.numeric(as.character(l[[id]]))),
                     geometry = sf::st_geometry(l))
    if (is.na(sf::st_crs(out))) sf::st_crs(out) <- 5514
    out
  })
  layers <- Filter(Negate(is.null), layers)
  if (!length(layers)) return(NULL)
  layers <- lapply(layers, sf::st_transform, crs = 5514)
  out <- do.call(rbind, layers)
  out[!duplicated(out$ID_LOKAL), ]
}

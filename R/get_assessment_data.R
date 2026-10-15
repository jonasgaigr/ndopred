#' Get NDOP Records for Assessment, Flagged
#'
#' Takes an NDOP snapshot ([ndop_snapshot()]) or reads a stored one, flags
#' every record ([flag_records()]) and returns them as points. **No record
#' is deleted**: excluded records keep their row with `include = FALSE` and
#' an `exclusion_reason`, and the metrics use only `include == TRUE`.
#'
#' Coordinates are read as S-JTSK / Krovak East North (EPSG:5514), NDOP's
#' native CRS, and transformed to ETRS89-LAEA (EPSG:3035) so that areas come
#' out in equal-area metres. Records without coordinates get an empty point.
#'
#' @param species_name Character. Scientific name exactly as in NDOP; records
#'   of other taxa returned by the query are flagged (gap analysis, D13).
#' @param year_start Numeric. Optional: keep only records from this year on.
#' @param snapshot Optional `ndop_snapshot` object or the path to one. When
#'   given, nothing is downloaded.
#' @param snapshot_dir Optional folder. When given (and `snapshot` is not),
#'   the download is stored there as an RDS snapshot; see
#'   [default_snapshot_dir()].
#' @param less_credible_share Passed to [flag_records()].
#' @return An `sf` object with every record, the `flag_*` columns,
#'   `exclusion_reason`, `include`, `year_min`, `year_max`, `precision_m` and
#'   `geom_type`. Attributes:
#'   \describe{
#'     \item{`filter_counts`}{Records left after each successive test
#'       (`raw`, `coords`, `non_negative`, `has_katastr`, `verified`) and
#'       `included`.}
#'     \item{`exclusions`}{A count of records per exclusion reason.}
#'     \item{`record_geometries`}{The record points, lines and polygons
#'       (EPSG:3035) keyed by `ID_LOKAL`, used for precision-aware AOO.}
#'     \item{`snapshot`}{The snapshot's metadata (query, download time,
#'       versions, file).}
#'     \item{`download_error`}{The error message, if the download failed.}
#'   }
#'
#' @section Credentials:
#' NDOP requires a login. Set `NDOP_USER` and `NDOP_PWD` in `~/.Renviron`, or
#' call `rndop::isop_login()` once. A failed download returns no records and
#' sets the `download_error` attribute.
#'
#' @seealso [flag_records()], [ndop_snapshot()], [taxon_metrics()]
#' @examples
#' \dontrun{
#' occ <- get_assessment_data("Lucanus cervus", snapshot_dir = default_snapshot_dir())
#' table(occ$include)
#' attr(occ, "exclusions")
#'
#' # Rerun later on exactly the same records
#' occ2 <- get_assessment_data("Lucanus cervus",
#'                             snapshot = attr(occ, "snapshot")$path)
#' }
#' @export
get_assessment_data <- function(species_name, year_start = NULL, snapshot = NULL,
                                snapshot_dir = NULL, less_credible_share = 0.10) {

  empty_sf <- function() {
    # An empty coordinate set makes sf compute a bounding box over zero
    # points, which emits benign min/max warnings.
    suppressWarnings(
      sf::st_as_sf(data.frame(X = numeric(0), Y = numeric(0), DATUM_OD = character(0),
                              include = logical(0)),
                   coords = c("X", "Y"), crs = 5514) %>%
        sf::st_transform(3035)
    )
  }

  # 1. The snapshot: given, stored, or downloaded now.
  snap <- if (inherits(snapshot, "ndop_snapshot")) {
    snapshot
  } else if (is.character(snapshot)) {
    read_ndop_snapshot(snapshot)
  } else {
    ndop_snapshot(species_name, dir = snapshot_dir, save = !is.null(snapshot_dir))
  }
  raw_data <- snap$table
  download_error <- snap$meta$download_error

  if (is.null(raw_data) || !is.data.frame(raw_data) || nrow(raw_data) == 0) {
    message(paste(if (is.null(download_error)) "Zero records for:" else "Download failed for:",
                  species_name))
    result <- empty_sf()
    attr(result, "filter_counts") <- list(
      raw = 0, coords = 0, non_negative = 0, has_katastr = 0, verified = 0, included = 0
    )
    attr(result, "snapshot") <- snap$meta
    if (!is.null(download_error)) attr(result, "download_error") <- download_error
    return(result)
  }

  # 2. Flags, never deletion (gap analysis, item 11).
  flagged <- flag_records(raw_data, species = species_name,
                          less_credible_share = less_credible_share)
  f <- flagged
  verified_out <- f$flag_erroneous | (f$flag_less_credible & !attr(f, "less_credible_retained"))
  coords_ok <- !f$flag_no_coords
  filter_counts <- list(
    raw = nrow(f),
    coords = sum(coords_ok),
    non_negative = sum(coords_ok & !f$flag_negative),
    has_katastr = sum(coords_ok & !f$flag_negative & !f$flag_no_cadastre),
    verified = sum(coords_ok & !f$flag_negative & !f$flag_no_cadastre & !verified_out),
    included = sum(f$include)
  )

  # 3. Record geometries, joined by ID_LOKAL (rndop's idx_nd_lok).
  geoms <- .snapshot_geometries(snap$geometries)
  flagged$geom_type <- NA_character_
  if (!is.null(geoms) && "ID_LOKAL" %in% names(flagged)) {
    types <- as.character(sf::st_geometry_type(geoms))
    flagged$geom_type <- types[match(suppressWarnings(as.numeric(flagged$ID_LOKAL)), geoms$ID_LOKAL)]
  }
  flagged$geom_type[is.na(flagged$geom_type) & coords_ok] <- "POINT"

  # Records without coordinates get an empty point rather than being dropped.
  cleaned_sf <- suppressWarnings(
    sf::st_as_sf(flagged, coords = c("X", "Y"), crs = 5514, na.fail = FALSE,
                 remove = FALSE) %>%
      sf::st_transform(crs = 3035)
  )

  # 4. Optional date filter (a subset the caller asked for).
  if (!is.null(year_start) && nrow(cleaned_sf) > 0) {
    cleaned_sf <- filter_years(cleaned_sf, year_start = year_start)
  }

  attr(cleaned_sf, "filter_counts") <- filter_counts
  attr(cleaned_sf, "exclusions") <- table(f$exclusion_reason[!f$include])
  attr(cleaned_sf, "record_geometries") <- if (is.null(geoms)) NULL else sf::st_transform(geoms, 3035)
  attr(cleaned_sf, "snapshot") <- snap$meta
  attr(cleaned_sf, "less_credible_share") <- attr(f, "less_credible_share")
  cleaned_sf
}

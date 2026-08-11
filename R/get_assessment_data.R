#' Get and Clean NDOP Data
#'
#' @param species_name Character. Scientific name to query.
#' @param year_start Numeric. Optional filter for starting year.
#' @return An sf object of cleaned occurrences. It always carries a
#'   \code{filter_counts} attribute - a named list (\code{raw}, \code{coords},
#'   \code{non_negative}, \code{has_katastr}, \code{verified}) recording how
#'   many records survived each quality-control step, so a species that ends
#'   up with zero usable records can be traced back to its cause (e.g. no NDOP
#'   records at all, vs. records excluded for missing coordinates or low
#'   verification status) instead of a single opaque "no records" outcome.
#' @export
get_assessment_data <- function(species_name, year_start = NULL) {

  empty_sf <- function() {
    # An empty coordinate set makes sf::st_transform() compute a bounding box
    # over zero points, which emits benign but noisy min/max warnings - this
    # is an expected, common outcome (a species simply has no usable
    # records), not something worth surfacing to the user.
    suppressWarnings(
      sf::st_as_sf(data.frame(X = numeric(0), Y = numeric(0), DATUM_OD = character(0)),
                   coords = c("X", "Y"), crs = 5514) %>%
        sf::st_transform(3035)
    )
  }

  # 1. Download
  raw_data <- tryCatch({
    rndop::ndop_download(species_name)
  }, error = function(e) return(NULL))

  # 2. Check if data actually exists before doing anything else
  if (is.null(raw_data) || !is.data.frame(raw_data) || nrow(raw_data) == 0) {
    message(paste("Zero records for:", species_name))
    result <- empty_sf()
    attr(result, "filter_counts") <- list(
      raw = 0, coords = 0, non_negative = 0, has_katastr = 0, verified = 0
    )
    return(result)
  }

  n_raw <- nrow(raw_data)

  # 3. Clean and Transform, tracking survivors at each QC step for diagnostics.
  # NEGATIV uses is.na(...) | ... == 0 rather than a bare `== 0`: a blank/NA
  # value means the field was never populated (i.e. not flagged as a negative
  # finding), not that the record's status is unknown, so it should not be
  # silently dropped alongside genuine negative-finding records.
  step_coords <- dplyr::filter(raw_data, !is.na(X), !is.na(Y))
  step_negativ <- dplyr::filter(step_coords, is.na(NEGATIV) | NEGATIV == 0)
  step_katastr <- dplyr::filter(step_negativ, KATASTR != "")
  step_veroh <- dplyr::filter(step_katastr, VEROH != 3 & VEROH != 9)

  filter_counts <- list(
    raw = n_raw,
    coords = nrow(step_coords),
    non_negative = nrow(step_negativ),
    has_katastr = nrow(step_katastr),
    verified = nrow(step_veroh)
  )

  cleaned_sf <- step_veroh

  # Handle case where filtering leaves 0 rows
  if (nrow(cleaned_sf) == 0) {
    result <- empty_sf()
    attr(result, "filter_counts") <- filter_counts
    return(result)
  }

  cleaned_sf <- cleaned_sf %>%
    sf::st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
    sf::st_transform(crs = 3035)

  # 4. Date Filter
  if (!is.null(year_start) && nrow(cleaned_sf) > 0) {
    # Convert DATUM_OD safely
    cleaned_sf <- cleaned_sf %>%
      dplyr::mutate(y = as.numeric(substr(as.character(DATUM_OD), 1, 4))) %>%
      dplyr::filter(y >= year_start) %>%
      dplyr::select(-y)
  }

  attr(cleaned_sf, "filter_counts") <- filter_counts
  return(cleaned_sf)
}

#' Clean and Parse NDOP Dates
#'
#' Adds a numeric `year` column parsed from `DATUM_OD` and drops rows whose
#' date could not be parsed.
#'
#' @param df Data frame with a `DATUM_OD` column.
#' @return The input with a numeric `year` column added, minus unparseable
#'   rows.
#' @seealso [extract_year()] for the parser, [filter_years()] to subset a
#'   window without dropping the rest.
#' @examples
#' df <- data.frame(DATUM_OD = c("20200101", "01.01.1999", "junk"))
#' clean_dates(df)
#' @export
clean_dates <- function(df) {
  df %>%
    dplyr::mutate(
      year = extract_year(.data$DATUM_OD)
    ) %>%
    # Remove rows where year couldn't be parsed
    dplyr::filter(!is.na(.data$year))
}

#' Extract a Four-Digit Year from Heterogeneous NDOP Date Values
#'
#' NDOP exports have appeared with several date encodings depending on the
#' export route and locale: integer/character \code{YYYYMMDD}, ISO
#' \code{YYYY-MM-DD}, Czech \code{DD.MM.YYYY}, and true \code{Date} objects.
#' The package previously relied on \code{substr(x, 1, 4)} throughout, which
#' silently produces \code{NA} (or, worse, a plausible-looking wrong number)
#' for the Czech format. Centralising the parsing here keeps every temporal
#' filter in the package consistent.
#'
#' @param x A vector of dates (Date, character or numeric).
#' @return A numeric vector of years, \code{NA} where no year could be parsed.
#' @seealso [clean_dates()], [filter_years()]
#' @examples
#' extract_year(20200101)             # NDOP integer YYYYMMDD
#' extract_year("2020-01-01")         # ISO
#' extract_year("01.01.2020")         # Czech
#' extract_year(as.Date("2020-01-01"))
#' extract_year(c("20200101", "junk", NA))
#' @export
extract_year <- function(x) {
  if (length(x) == 0) return(numeric(0))
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.numeric(format(x, "%Y")))
  }

  chr <- trimws(as.character(x))
  year <- rep(NA_real_, length(chr))

  # Leading year: "20200101", "2020-01-01", "2020/01/01", "2020"
  lead <- regmatches(chr, regexpr("^(1[6-9]|2[0-9])[0-9]{2}", chr))
  has_lead <- grepl("^(1[6-9]|2[0-9])[0-9]{2}", chr)
  year[has_lead] <- as.numeric(lead)

  # Trailing year: "01.01.2020", "1/1/2020"
  trail_idx <- !has_lead & grepl("(1[6-9]|2[0-9])[0-9]{2}$", chr)
  if (any(trail_idx)) {
    trail <- regmatches(chr[trail_idx], regexpr("(1[6-9]|2[0-9])[0-9]{2}$", chr[trail_idx]))
    year[trail_idx] <- as.numeric(trail)
  }

  year
}

#' Add a Standardised 'year' Column to Occurrence Data
#'
#' Prefers an existing \code{ROK} column, falling back to parsing
#' \code{DATUM_OD}. Used by the temporal filters in \code{calculate_eoo()},
#' \code{calculate_aoo()} and \code{calculate_locations()} so that all three
#' agree on which records belong to a given window.
#'
#' @param data Data frame or sf object of occurrences.
#' @param date_col Character. Name of the date column to fall back on.
#' @return The input with a numeric \code{year} column added (or refreshed).
#' @keywords internal
add_year_column <- function(data, date_col = "DATUM_OD") {
  if ("year" %in% names(data) && is.numeric(data$year)) return(data)

  if ("ROK" %in% names(data)) {
    yr <- suppressWarnings(as.numeric(as.character(data$ROK)))
    if (date_col %in% names(data)) {
      fallback <- extract_year(data[[date_col]])
      yr[is.na(yr)] <- fallback[is.na(yr)]
    }
    data$year <- yr
    return(data)
  }

  if (date_col %in% names(data)) {
    data$year <- extract_year(data[[date_col]])
    return(data)
  }

  stop(paste0("Neither 'ROK' nor '", date_col, "' found in the dataset; ",
              "cannot apply a temporal filter."))
}

#' Filter Occurrence Data to a Year Window
#'
#' @param data Data frame or sf object of occurrences.
#' @param year_start Numeric or NULL. Inclusive lower bound.
#' @param year_end Numeric or NULL. Inclusive upper bound.
#' @param date_col Character. Name of the date column to fall back on.
#' @return The filtered input.
#' @keywords internal
filter_years <- function(data, year_start = NULL, year_end = NULL,
                         date_col = "DATUM_OD") {
  if (is.null(year_start) && is.null(year_end)) return(data)
  if (nrow(data) == 0) return(data)

  data <- add_year_column(data, date_col = date_col)

  keep <- !is.na(data$year)
  if (!is.null(year_start)) keep <- keep & data$year >= year_start
  if (!is.null(year_end))   keep <- keep & data$year <= year_end

  # Subsetting an sf object down to zero features recomputes a bounding box
  # over no points, which emits benign min/max warnings. A window with no
  # records is an ordinary outcome here, so keep the noise out of the log.
  suppressWarnings(data[keep, , drop = FALSE])
}

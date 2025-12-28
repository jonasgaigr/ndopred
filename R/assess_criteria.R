#' Evaluate Criterion A (Population Reduction)
#'
#' @param trend_res Result from calculate_trend()
#' @return Suggested category (character)
#' @export
assess_criterion_a <- function(trend_res) {
  if (is.na(trend_res$percent_change)) return("DD") # Data Deficient

  # IUCN A2/A4 Thresholds (Decline is a negative change)
  # If percent_change is -80 (80% decline) -> CR
  decline <- -1 * trend_res$percent_change

  if (decline >= 80) return("CR")
  if (decline >= 50) return("EN")
  if (decline >= 30) return("VU")

  # Note: A positive trend (growth) or small decline is LC
  return("LC")
}

#' Evaluate Criterion D2 (Restricted AOO/Locations)
#'
#' @param aoo_km2 Numeric AOO
#' @param locations Numeric Location count
#' @return "VU" or "LC"
#' @export
assess_criterion_d2 <- function(aoo_km2, locations) {
  # IUCN D2 Thresholds: AOO < 20 km2 or Locations <= 5
  if (aoo_km2 < 20 || locations <= 5) {
    return("VU")
  } else {
    return("LC")
  }
}

#' Summarize Red List Assessment
#'
#' @param species Name of the species
#' @param eoo EOO result list
#' @param aoo AOO result list
#' @export
summarize_assessment <- function(species, eoo, aoo) {

  # Determine category for B1 (EOO)
  cat_b1 <- dplyr::case_when(
    eoo$area_km2 < 100 ~ "CR",
    eoo$area_km2 < 5000 ~ "EN",
    eoo$area_km2 < 20000 ~ "VU",
    TRUE ~ "LC"
  )

  # Determine category for B2 (AOO)
  cat_b2 <- dplyr::case_when(
    aoo$area_km2 < 10 ~ "CR",
    aoo$area_km2 < 500 ~ "EN",
    aoo$area_km2 < 2000 ~ "VU",
    TRUE ~ "LC"
  )

  res <- data.frame(
    Species = species,
    EOO_km2 = round(eoo$area_km2, 1),
    AOO_km2 = round(aoo$area_km2, 1),
    Category_B1 = cat_b1,
    Category_B2 = cat_b2,
    Final_Preliminary_Category = ifelse(
      match(cat_b1, c("LC", "VU", "EN", "CR")) > match(cat_b2, c("LC", "VU", "EN", "CR")),
      cat_b1, cat_b2
    )
  )

  return(res)
}

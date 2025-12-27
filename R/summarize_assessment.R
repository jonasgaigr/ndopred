summarize_assessment <- function(species, eoo, aoo, trend, locations) {

  # 1. Determine Initial Category based on Area Thresholds
  raw_cat_b1 <- dplyr::case_when(
    eoo$area_km2 < 100 ~ "CR",
    eoo$area_km2 < 5000 ~ "EN",
    eoo$area_km2 < 20000 ~ "VU",
    TRUE ~ "LC"
  )

  raw_cat_b2 <- dplyr::case_when(
    aoo$area_km2 < 10 ~ "CR",
    aoo$area_km2 < 500 ~ "EN",
    aoo$area_km2 < 2000 ~ "VU",
    TRUE ~ "LC"
  )

  # 2. Apply the "Location" Constraint (Sub-criterion a)
  # Thresholds: CR <= 1 location, EN <= 5 locations, VU <= 10 locations
  final_cat_b1 <- dplyr::case_when(
    raw_cat_b1 == "CR" & locations <= 1 ~ "CR",
    raw_cat_b1 %in% c("CR", "EN") & locations <= 5 ~ "EN",
    raw_cat_b1 %in% c("CR", "EN", "VU") & locations <= 10 ~ "VU",
    TRUE ~ "LC"
  )

  final_cat_b2 <- dplyr::case_when(
    raw_cat_b2 == "CR" & locations <= 1 ~ "CR",
    raw_cat_b2 %in% c("CR", "EN") & locations <= 5 ~ "EN",
    raw_cat_b2 %in% c("CR", "EN", "VU") & locations <= 10 ~ "VU",
    TRUE ~ "LC"
  )

  # 3. Final Category Selection
  levels <- c("LC", "VU", "EN", "CR")
  final_cat <- levels[max(match(c(final_cat_b1, final_cat_b2), levels))]

  return(data.frame(
    Species = species,
    EOO_km2 = round(eoo$area_km2),
    AOO_km2 = round(aoo$area_km2),
    Locations = locations,
    Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1)),
    Category = final_cat,
    Note = ifelse(final_cat != "LC" & final_cat != raw_cat_b2, "Downgraded due to Location count", "Criteria met")
  ))
}

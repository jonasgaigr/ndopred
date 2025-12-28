#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations) {

  # 1. Check for Regional Extinction
  if (aoo$area_km2 == 0) {
    return(data.frame(
      Species = species, EOO_km2 = 0, AOO_km2 = 0, Locations = 0,
      Trend_Perc = "NA", Category = "RE", Note = "No recent records found."
    ))
  }

  # 2. Determine Category based on Area (B1, B2)
  raw_cat_b1 <- dplyr::case_when(eoo$area_km2 < 100 ~ "CR", eoo$area_km2 < 5000 ~ "EN", eoo$area_km2 < 20000 ~ "VU", TRUE ~ "LC")
  raw_cat_b2 <- dplyr::case_when(aoo$area_km2 < 10 ~ "CR", aoo$area_km2 < 500 ~ "EN", aoo$area_km2 < 2000 ~ "VU", TRUE ~ "LC")

  # 3. Apply Location Constraint (B2a)
  final_cat_b1 <- dplyr::case_when(raw_cat_b1 == "CR" & locations <= 1 ~ "CR", raw_cat_b1 %in% c("CR", "EN") & locations <= 5 ~ "EN", raw_cat_b1 %in% c("CR", "EN", "VU") & locations <= 10 ~ "VU", TRUE ~ "LC")
  final_cat_b2 <- dplyr::case_when(raw_cat_b2 == "CR" & locations <= 1 ~ "CR", raw_cat_b2 %in% c("CR", "EN") & locations <= 5 ~ "EN", raw_cat_b2 %in% c("CR", "EN", "VU") & locations <= 10 ~ "VU", TRUE ~ "LC")

  # 4. Preliminary Category
  levels <- c("LC", "VU", "EN", "CR")
  final_cat <- levels[max(match(c(final_cat_b1, final_cat_b2), levels))]

  # 5. Add Criterion D2 (The Safety Net)
  # If it was LC but is very restricted, move to VU
  note_text <- "Criteria met"
  if (final_cat == "LC" && (aoo$area_km2 < 20 || locations <= 5)) {
    final_cat <- "VU"
    note_text <- "Triggered VU under Criterion D2 (restricted range)"
  }

  return(data.frame(
    Species = species, EOO_km2 = round(eoo$area_km2), AOO_km2 = round(aoo$area_km2),
    Locations = locations,
    Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1)),
    Category = final_cat, Note = note_text
  ))
}

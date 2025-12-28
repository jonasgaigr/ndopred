#' Summarize Assessment with IUCN Labeling
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations) {

  # 1. Evaluate B1 (EOO) and B2 (AOO)
  cat_b1 <- case_when(eoo$area_km2 < 100 ~ "CR", eoo$area_km2 < 5000 ~ "EN", eoo$area_km2 < 20000 ~ "VU", TRUE ~ "LC")
  cat_b2 <- case_when(aoo$area_km2 < 10 ~ "CR", aoo$area_km2 < 500 ~ "EN", aoo$area_km2 < 2000 ~ "VU", TRUE ~ "LC")

  # 2. Check for "Continuing Decline" (Criterion b)
  # If our trend calculation shows a negative slope, we can trigger 'b'
  has_decline <- !is.na(trend$percent_change) && trend$percent_change < 0

  # 3. Build Criterion Strings
  crit_parts <- c()

  # B1 Logic
  if (cat_b1 != "LC" && locations <= 10) {
    loc_code <- case_when(locations <= 1 ~ "a", locations <= 5 ~ "a", TRUE ~ "a")
    decline_code <- if(has_decline) "b(ii)" else ""
    crit_parts <- c(crit_parts, paste0("B1", loc_code, decline_code))
  }

  # B2 Logic
  if (cat_b2 != "LC" && locations <= 10) {
    loc_code <- case_when(locations <= 1 ~ "a", locations <= 5 ~ "a", TRUE ~ "a")
    decline_code <- if(has_decline) "b(ii)" else ""
    crit_parts <- c(crit_parts, paste0("B2", loc_code, decline_code))
  }

  # 4. Criterion D2 (Very restricted)
  cat_d <- "LC"
  if (aoo$area_km2 < 20 || locations <= 5) {
    cat_d <- "VU"
    crit_parts <- c(crit_parts, "D2")
  }

  # 5. Final Category
  levels <- c("LC", "VU", "EN", "CR")
  final_cat <- levels[max(match(c(cat_b1, cat_b2, cat_d), levels))]

  # If B triggers, but location count is too high, it might downgrade to LC
  # UNLESS D2 or Trend (A) saves it.
  if (locations > 10 && final_cat != "VU") final_cat <- "LC"

  return(data.frame(
    Species = species,
    Category = final_cat,
    Criteria = paste(unique(crit_parts), collapse = "; "),
    EOO_km2 = round(eoo$area_km2, 2),
    AOO_km2 = round(aoo$area_km2, 2),
    Locations = locations,
    Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1))
  ))
}

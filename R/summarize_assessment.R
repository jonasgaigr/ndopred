#' Summarize Assessment with IUCN Labeling and Flags
#' @export
summarize_assessment <- function(species, eoo, aoo, trend, locations) {

  # 1. Evaluate B1 (EOO) and B2 (AOO) thresholds
  cat_b1 <- dplyr::case_when(
    eoo$area_km2 < 100 ~ "CR",
    eoo$area_km2 < 5000 ~ "EN",
    eoo$area_km2 < 20000 ~ "VU",
    TRUE ~ "LC"
  )

  cat_b2 <- dplyr::case_when(
    aoo$area_km2 < 10 ~ "CR",
    aoo$area_km2 < 500 ~ "EN",
    aoo$area_km2 < 2000 ~ "VU",
    TRUE ~ "LC"
  )

  # 2. Check for "Continuing Decline" (Criterion b)
  # We use the trend slope as a proxy for 'b(ii)' (decline in AOO)
  has_decline <- !is.na(trend$percent_change) && trend$percent_change < 0

  # 3. Build Criterion B Strings
  # Note: IUCN requires (a) AND (b) to be met for B
  crit_parts <- c()

  if (cat_b1 != "LC" && locations <= 10) {
    decline_code <- if(has_decline) "b(ii)" else ""
    crit_parts <- c(crit_parts, paste0("B1a", decline_code))
  }

  if (cat_b2 != "LC" && locations <= 10) {
    decline_code <- if(has_decline) "b(ii)" else ""
    crit_parts <- c(crit_parts, paste0("B2a", decline_code))
  }

  # 4. Criterion D2 (Very restricted range/locations)
  cat_d <- "LC"
  if (aoo$area_km2 < 20 || locations <= 5) {
    cat_d <- "VU"
    crit_parts <- c(crit_parts, "D2")
  }

  # 5. Criterion A Flagging (Expert confirmation required)
  a_flag <- "None"
  if (!is.na(trend$percent_change)) {
    a_flag <- dplyr::case_when(
      trend$percent_change <= -80 ~ "Potentially CR (A2)",
      trend$percent_change <= -50 ~ "Potentially EN (A2)",
      trend$percent_change <= -30 ~ "Potentially VU (A2)",
      TRUE ~ "None"
    )
  }

  # 6. Final Category Determination (Hierarchical)
  cat_levels <- c("LC", "VU", "EN", "CR")
  # Filter only the categories triggered by Criterion B or D
  triggered_cats <- c(
    if(locations <= 10) cat_b1,
    if(locations <= 10) cat_b2,
    cat_d
  )

  final_cat <- if(length(triggered_cats) > 0) {
    cat_levels[max(match(triggered_cats, cat_levels))]
  } else {
    "LC"
  }

  # Final result data frame
  return(data.frame(
    Species = species,
    Category = final_cat,
    Criteria = paste(unique(crit_parts), collapse = "; "),
    EOO_km2 = round(eoo$area_km2, 2),
    AOO_km2 = round(aoo$area_km2, 2),
    Locations = locations,
    Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1)),
    Flags = a_flag,
    stringsAsFactors = FALSE
  ))
}

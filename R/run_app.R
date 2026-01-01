#' Run the NDOP Red List Assessment Dashboard
#'
#' Launches the interactive Shiny application for species assessment.
#' This dashboard allows experts to calculate spatial metrics, population trends,
#' and apply IUCN criteria (A-E) with manual override capabilities.
#'
#' @return No return value. Side effect: Launches the Shiny app.
#' @export
run_app <- function() {
  # Locate the 'shinyapp' folder inside the installed package
  app_dir <- system.file("shinyapp", package = "ndopred")

  # Check if directory exists (vital for debugging installation issues)
  if (app_dir == "") {
    stop("The 'shinyapp' directory was not found. Try re-installing `ndopred`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}

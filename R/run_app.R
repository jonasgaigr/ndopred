#' Run the NDOP Single-Species Assessment Dashboard
#'
#' Launches the interactive expert dashboard: one taxon at a time, with the
#' distribution map and every metric rendered as the assessor works through
#' criteria A to E.
#'
#' @details
#' Enter a scientific name and press **Run Assessment**. The app downloads
#' NDOP records, computes the metrics and shows an *automated baseline*
#' category, then lets the assessor confirm or change every input behind it.
#' The **Expert Final Verdict** panel updates live as they do.
#'
#' The three tabs are:
#' \describe{
#'   \item{Spatial & Metrics}{The map (recent, comparison and historical
#'     records, with EOO hulls and AOO grids) plus the automated category and
#'     a metrics table.}
#'   \item{Expert Review}{One panel per criterion. Criterion A takes the type
#'     (A1–A4), a reduction percentage and its basis; criterion B takes severe
#'     fragmentation, the location count, and the decline and fluctuation
#'     subindices; C and D take counts and the D2 threat confirmation; E takes
#'     a quantitative extinction probability. A **Force Category Override**
#'     selector enforces DD/RE/EW/EX regardless of the metrics.}
#'   \item{Trend & History}{The raw trend and population objects, including
#'     the window definitions actually used.}
#' }
#'
#' @section Two windows, two purposes:
#' *Current-distribution window* drives EOO, AOO and locations — criterion B
#' describes the current distribution (Guidelines 4.9). *Generation length*
#' drives the criterion A reduction window, which is ten years or three
#' generations, whichever is longer, capped at 100 (Guidelines 4.4, 5.2). The
#' app shows the resulting reduction window beneath the input.
#'
#' @section Things the app will not decide for you:
#' Criteria C and D1 are auto-enabled only for vertebrates and plants *and*
#' only when the abundance records behind `total_mature` are a substantial
#' share of the counted records — NDOP counts are minimum observed values, not
#' population estimates. The Expert Review panel shows how many counted
#' records were used and which life stages were skipped. Criterion D2 is never
#' pre-ticked: a restricted range is flagged, but VU D2 requires the assessor
#' to affirm a plausible threat (Guidelines 8).
#'
#' @section Credentials:
#' NDOP requires a login. Set `NDOP_USER` and `NDOP_PWD` in `~/.Renviron`, or
#' run `rndop::isop_login()` once, before starting the app. Without them every
#' species returns Data Deficient.
#'
#' @param ... Passed to [shiny::runApp()], e.g. `port` or `launch.browser`.
#' @return No return value. Side effect: launches the Shiny application.
#' @seealso [run_bulk_app()] for screening a whole spreadsheet;
#'   [assess_species()] for the same pipeline without a UI.
#' @examples
#' if (interactive()) {
#'   run_app()
#'
#'   # On a fixed port, without opening a browser
#'   run_app(port = 4321, launch.browser = FALSE)
#' }
#' @export
run_app <- function(...) {
  # Locate the 'shinyapp' folder inside the installed package
  app_dir <- system.file("shinyapp", package = "ndopred")

  # Check if directory exists (vital for debugging installation issues)
  if (app_dir == "") {
    stop("The 'shinyapp' directory was not found. Try re-installing `ndopred`.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}

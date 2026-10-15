#' ndopred: Automated IUCN Red List Assessment from NDOP Data
#'
#' Turns occurrence records from NDOP (Nálezová databáze ochrany přírody, the
#' Czech Nature Conservation Agency's species occurrence database) into IUCN
#' Red List metrics and categories, for national and regional Red List work.
#'
#' @section The two applications:
#' The package ships two complementary Shiny dashboards:
#'
#' * [run_bulk_app()] — **bulk screening**. Takes a spreadsheet of taxa and
#'   evaluates every one of them on the criteria that can be automated from
#'   occurrence records alone (those driven by EOO, AOO and locations). Writes
#'   one row per taxon plus a distribution map per taxon.
#' * [run_app()] — **single-species expert assessment**. Works through one
#'   taxon at a time with maps and metrics rendered as the assessor proceeds,
#'   letting them confirm, adjust or override each subcriterion.
#'
#' The bulk pass narrows a long list to the taxa that need attention; the
#' single-species app is where a defensible category is actually settled.
#'
#' @section The pipeline:
#' \describe{
#'   \item{Fetch}{[get_assessment_data()] downloads and quality-filters
#'     records, recording how many survived each step.}
#'   \item{Spatial metrics}{[calculate_eoo()], [calculate_aoo()],
#'     [calculate_locations()], reconciled by [reconcile_eoo_aoo()].}
#'   \item{Change over time}{[calculate_trend()] compares the recent window
#'     against the equal-length window before it.}
#'   \item{Abundance}{[calculate_pop_metrics()] derives minimum observed
#'     counts, decline rate and fluctuation diagnosis.}
#'   \item{Categorise}{[summarize_assessment()] applies criteria A–D.}
#'   \item{Map}{[plot_iucn()].}
#' }
#' [assess_species()] and [batch_assess()] run the whole chain headlessly.
#'
#' @section What is deliberately not automated:
#' Occurrence records cannot evidence every subcriterion, so these are
#' arguments to [summarize_assessment()] rather than inferences:
#'
#' \tabular{lll}{
#'   **Judgement** \tab **Argument** \tab **Why** \cr
#'   Severe fragmentation, B(a) \tab `severely_fragmented` \tab Guidelines 4.8 \cr
#'   Habitat decline, B(b)(iii) \tab `habitat_decline` \tab NDOP holds no habitat data \cr
#'   Plausible threat for D2 \tab `d2_threat` \tab Guidelines 8: the AOO/location figures alone are "not sufficient" \cr
#'   Criterion E \tab — \tab Requires a quantitative extinction-risk analysis
#' }
#'
#' Two further limits apply to national work. First, **no regional
#' adjustment** is made: the *Guidelines for Application of IUCN Red List
#' Criteria at Regional and National Levels v4.0* — which define RE and the
#' downgrade/upgrade step for immigration from neighbouring populations — are
#' not implemented, so the package produces a global-style category from Czech
#' data and the `RE` output is a screen for survey follow-up, not a
#' determination. Second, NDOP is **opportunistic**: counts are minimum
#' observed values rather than population estimates (Guidelines 4.3), and
#' spatial metrics are sensitive to recording effort (Guidelines 4.10.8).
#'
#' @section Credentials:
#' NDOP requires a login. Set `NDOP_USER` and `NDOP_PWD` in `~/.Renviron`, or
#' call `rndop::isop_login()` once, before using any function that downloads
#' data.
#'
#' @references
#' IUCN (2012). *IUCN Red List Categories and Criteria: Version 3.1*, 2nd
#' edition. Gland, Switzerland and Cambridge, UK: IUCN.
#'
#' IUCN Standards and Petitions Committee (2024). *Guidelines for Using the
#' IUCN Red List Categories and Criteria*, Version 16.
#' \url{https://www.iucnredlist.org/documents/RedListGuidelines.pdf}
#'
#' @seealso `vignette("methodology", package = "ndopred")` for the mapping of
#'   each rule to its guideline section.
#' @keywords internal
"_PACKAGE"

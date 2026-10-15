#' User Interface for the Bulk Assessor
#'
#' Not called directly; see [run_bulk_app()].
#'
#' @return A `shiny.tag.list` describing the bulk assessor page.
#' @keywords internal
bulk_ui <- function() fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("NDOP Bulk Assessor: Automated IUCN Metrics"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      p(tags$small("The CSV must contain a TAXON column; CELED, SKUPINA and ID_TAXON are optional.")),

      # The default is only a starting value for the form; the run uses the
      # year shown here and records it in every row.
      numericInput("reference_year", "Reference year:",
                   value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, step = 1),
      numericInput("window", "Window Length (Years):", value = 10, min = 1),
      p(tags$small(paste(
        "The current window is the last N years up to the reference year; the",
        "comparison window is the N years immediately before it."
      ))),
      numericInput("cell_size", "Display grid for maps (km):", value = 2, min = 1),
      p(tags$small(paste(
        "Criteria B2 and D2 always use AOO on the 2 x 2 km reference grid",
        "(IUCN Guidelines 4.10.3). Another size changes the maps and the",
        "display AOO column only."
      ))),

      actionButton("run_bulk", "Run Bulk Assessment", class = "btn-primary", style = "width: 100%;"),
      hr(),

      uiOutput("download_ui")
    ),

    mainPanel(
      h4("Processing Log & Preview"),
      verbatimTextOutput("log_text"),
      tableOutput("preview_table")
    )
  )
)

# Export column names. The established ones are unchanged (gap analysis,
# D10); the new ones follow them. Escaped so the R code stays ASCII.
.BULK_COLS <- c(
  group = "Taxonomick\u00e1 skupina",
  family = "\u010cele\u010f",
  taxon = "Druh",
  category = "Kategorie (automatick\u00e1)",
  criteria = "Kriteria (automatick\u00e1)",
  period_old = "Obdob\u00ed star\u00e9",
  period_new = "Obdob\u00ed nov\u00e9",
  aoo_old = "AOO star\u00fd (km2)",
  aoo_new = "AOO nov\u00fd (km2)",
  eoo_old = "EOO star\u00fd (km2)",
  eoo_new = "EOO nov\u00fd (km2)",
  locs_old = "Po\u010det lokalit star\u00fd",
  locs_new = "Po\u010det lokalit nov\u00fd",
  cells_new = "Grid nov\u00fd (po\u010det bun\u011bk)",
  aoo_change = "Zm\u011bna AOO (%)",
  eoo_change = "Zm\u011bna EOO (%)",
  n_total = "Z\u00e1znam\u016f celkem",
  n_new = "Z\u00e1znam\u016f nov\u00e9 obdob\u00ed",
  year_last = "Posledn\u00ed rok",
  d2 = "D2 kandid\u00e1t (nutno posoudit ohro\u017een\u00ed)",
  card = "Karta druhu",
  criteria_iucn = "Krit\u00e9ria (IUCN z\u00e1pis)",
  support = "Opora n\u00e1vrhu",
  needs = "Nutno dolo\u017eit",
  range = "Rozsah kategori\u00ed",
  flags = "P\u0159\u00edznaky",
  aoo_display = "AOO nov\u00fd, zvolen\u00e1 m\u0159\u00ed\u017eka (km2)",
  reference_year = "Referen\u010dn\u00ed rok"
)

.SUPPORT_CS <- c(
  evidence = "dolo\u017eeno",
  indication = "indikace z n\u00e1lezov\u00fdch dat",
  candidate = "kandid\u00e1t",
  none = "nehodnoceno"
)

# One export row from an assess_taxon() result.
.bulk_row <- function(taxon, meta) {
  r <- taxon$result
  m <- taxon$metrics
  w <- taxon$config$windows
  d2 <- taxon$assessment$scenarios$working$D2$candidate
  vals <- list(
    group = meta$group, family = meta$family, taxon = r$Species,
    category = r$Category, criteria = r$Criteria,
    period_old = paste0(w$comp_start, "-", w$comp_end),
    period_new = paste0(w$recent_start, "-", w$recent_end),
    aoo_old = m$aoo_old, aoo_new = m$aoo_new,
    eoo_old = m$eoo_old, eoo_new = m$eoo_new,
    locs_old = m$clusters_old, locs_new = m$clusters_new,
    cells_new = m$cells_new,
    aoo_change = m$aoo_change, eoo_change = m$eoo_change,
    n_total = m$n_records, n_new = m$n_current, year_last = m$year_last,
    d2 = if (is.null(d2)) NA_character_ else if (d2) "ano" else "ne",
    card = meta$card,
    criteria_iucn = r$Criteria_IUCN,
    support = unname(.SUPPORT_CS[r$Support]),
    needs = r$Needs_Evidence, range = r$Category_Range, flags = r$Flags,
    aoo_display = m$aoo_display,
    reference_year = r$Reference_Year
  )
  vals <- lapply(vals, function(v) if (is.null(v) || length(v) == 0) NA else v[1])
  row <- as.data.frame(vals, stringsAsFactors = FALSE)
  names(row) <- .BULK_COLS[names(vals)]
  cbind(row, r[grepl("^issue_", names(r))])
}

#' Server Logic for the Bulk Assessor
#'
#' Not called directly; see [run_bulk_app()]. Exposed as a named function so
#' the pipeline can be driven end-to-end by [shiny::testServer()].
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Called for its side effects on `output`.
#' @keywords internal
bulk_server <- function(input, output, session) {

  rv <- reactiveValues(
    results_df = NULL,
    maps_zip_path = NULL,
    config = NULL,
    log = character(0)
  )

  log_msg <- function(msg) {
    rv$log <- c(rv$log, paste(Sys.time(), "-", msg))
  }

  observeEvent(input$run_bulk, {
    req(input$file1)

    sp_col <- "TAXON"

    # Read the CSV, preferring whichever separator actually yields the
    # expected TAXON column. read.csv2() on a comma-separated file silently
    # returns a single glued-together column.
    df_input <- tryCatch({
      candidates <- list(
        tryCatch(utils::read.csv2(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "CP1250"),
                 error = function(e) NULL),
        tryCatch(utils::read.csv(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "CP1250"),
                 error = function(e) NULL),
        tryCatch(utils::read.csv2(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                 error = function(e) NULL),
        tryCatch(utils::read.csv(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                 error = function(e) NULL)
      )
      ok <- Filter(function(d) !is.null(d) && sp_col %in% names(d), candidates)
      if (length(ok) > 0) ok[[1]] else Filter(Negate(is.null), candidates)[[1]]
    }, error = function(e) {
      log_msg(paste("Error reading CSV:", e$message))
      NULL
    })

    if (is.null(df_input)) return(invisible(NULL))

    if (!sp_col %in% names(df_input)) {
      log_msg(paste0("Column '", sp_col, "' not found. Columns present: ",
                     paste(names(df_input), collapse = ", "),
                     ". Check the separator and encoding of the uploaded file."))
      return(invisible(NULL))
    }

    species_list <- unique(trimws(df_input[[sp_col]]))
    species_list <- species_list[species_list != "" & !is.na(species_list)]

    if (length(species_list) == 0) {
      log_msg("No species names found in the TAXON column.")
      return(invisible(NULL))
    }

    win <- max(1, as.numeric(input$window))
    # The form supplies the reference year; without one (e.g. under
    # testServer) the current year is used and logged, and the run records it.
    ref_year <- suppressWarnings(as.numeric(input$reference_year))
    if (length(ref_year) == 0 || is.na(ref_year)) ref_year <- as.numeric(format(Sys.Date(), "%Y"))
    config <- ndopred_config(reference_year = ref_year, window = win)
    rv$config <- config
    w <- config$windows

    cell_km <- as.numeric(input$cell_size)
    grid_size_m <- cell_km * 1000
    if (!isTRUE(all.equal(cell_km, 2))) {
      log_msg(paste0("Display grid set to ", cell_km, " km: maps and the display AOO column only. ",
                     "Criteria B2/D2 use the 2 x 2 km reference grid (IUCN Guidelines 4.10.3)."))
    }

    log_msg(paste0("Found ", length(species_list), " unique species. Reference year ",
                   config$reference_year, "; current window ", w$recent_start, "-", w$recent_end,
                   "; comparison window ", w$comp_start, "-", w$comp_end, "."))

    temp_dir <- tempdir()
    map_dir <- file.path(temp_dir, "ndop_maps")
    if (dir.exists(map_dir)) unlink(map_dir, recursive = TRUE)
    dir.create(map_dir)

    results_list <- list()

    # Look up an optional per-species attribute from the uploaded sheet once.
    lookup <- function(col, sp) {
      if (!col %in% names(df_input)) return(NA_character_)
      hit <- df_input[trimws(df_input[[sp_col]]) == sp, col, drop = TRUE]
      hit <- hit[!is.na(hit)]
      if (length(hit) == 0) return(NA_character_)
      as.character(hit[1])
    }

    withProgress(message = 'Running Bulk Assessment', value = 0, {

      for (i in seq_along(species_list)) {
        sp <- species_list[i]
        incProgress(1 / length(species_list), detail = paste("Processing", sp))
        log_msg(paste("Fetching data for:", sp))

        id_taxon <- lookup("ID_TAXON", sp)
        meta <- list(
          group = lookup("SKUPINA", sp),
          family = lookup("CELED", sp),
          card = if (!is.na(id_taxon) && id_taxon != "") {
            paste0("https://portal.nature.cz/w/druh-", id_taxon)
          } else NA_character_
        )

        occ_raw <- tryCatch(ndopred::get_assessment_data(sp), error = function(e) e)
        if (inherits(occ_raw, "error")) {
          log_msg(paste("Download failed for", sp, ":", conditionMessage(occ_raw)))
          metrics <- ndopred::taxon_metrics(NULL, config)
          metrics$issues$download <- conditionMessage(occ_raw)
          metrics$issues$records <- "download failed"
          metrics$failed <- paste0("download failed: ", conditionMessage(occ_raw))
          taxon <- ndopred::assess_taxon(species = sp, config = config, metrics = metrics)
          occ_raw <- NULL
        } else {
          if (!is.null(attr(occ_raw, "download_error"))) {
            log_msg(paste("Download failed for", sp, ":", attr(occ_raw, "download_error")))
          } else if (nrow(occ_raw) == 0) {
            fc <- attr(occ_raw, "filter_counts")
            log_msg(if (!is.null(fc) && fc$raw > 0) {
              paste0("No usable records for: ", sp, " (raw=", fc$raw, ", with coords=", fc$coords,
                     ", non-negative=", fc$non_negative, ", with katastr=", fc$has_katastr,
                     ", verified=", fc$verified, ")")
            } else paste("No records found for:", sp))
          }
          if (is.na(meta$group) && "KAT_TAX" %in% names(occ_raw) && nrow(occ_raw) > 0) {
            meta$group <- as.character(occ_raw$KAT_TAX[1])
          }
          if (is.na(meta$family) && "CELED" %in% names(occ_raw) && nrow(occ_raw) > 0) {
            meta$family <- as.character(occ_raw$CELED[1])
          }
          taxon <- tryCatch(
            ndopred::assess_taxon(occ_raw, sp, config, evaluate_pop = FALSE,
                                  display_grid_size = grid_size_m),
            error = function(e) {
              log_msg(paste("ERROR in automated category for", sp, ":", conditionMessage(e)))
              metrics <- ndopred::taxon_metrics(NULL, config)
              metrics$issues$records <- paste0("assessment error: ", conditionMessage(e))
              metrics$failed <- paste0("assessment error: ", conditionMessage(e))
              ndopred::assess_taxon(species = sp, config = config, metrics = metrics)
            }
          )
        }
        results_list[[i]] <- .bulk_row(taxon, meta)

        # --- MAP GENERATION ---
        if (!is.null(occ_raw) && nrow(occ_raw) > 0) {
          map_file <- file.path(map_dir, paste0(gsub("[^A-Za-z0-9]", "_", sp), "_map.png"))
          tryCatch({
            p <- ndopred::plot_iucn(
              species_name = sp, occ_data = occ_raw,
              window = win, year_now = config$reference_year,
              grid_size = grid_size_m
            )
            if (!is.null(p)) {
              ggplot2::ggsave(filename = map_file, plot = p, width = 8, height = 6,
                              dpi = 150, bg = "white")
            }
          }, error = function(e) {
            log_msg(paste("Map generation failed for", sp, ":", e$message))
          })
        }
      }

      rv$results_df <- do.call(rbind, results_list)

      zip_file <- file.path(temp_dir, "ndop_maps.zip")
      if (file.exists(zip_file)) unlink(zip_file)

      map_files <- list.files(map_dir, full.names = TRUE)
      if (length(map_files) > 0) {
        zip::zipr(zipfile = zip_file, files = map_files)
        rv$maps_zip_path <- zip_file
        log_msg("Maps successfully zipped.")
      } else {
        log_msg("No maps were generated.")
      }

      log_msg("Bulk assessment complete.")
    })
  })

  output$log_text <- renderText({
    paste(rv$log, collapse = "\n")
  })

  output$preview_table <- renderTable({
    req(rv$results_df)
    utils::head(rv$results_df, 10)
  })

  output$download_ui <- renderUI({
    req(rv$results_df)
    tagList(
      downloadButton("dl_csv", "Download Results (CSV)", class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
      if (!is.null(rv$maps_zip_path)) {
        downloadButton("dl_maps", "Download Maps (ZIP)", class = "btn-info", style = "width: 100%;")
      }
    )
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("NDOP_Bulk_Assessment_", if (is.null(rv$config)) "" else rv$config$reference_year, ".csv")
    },
    content = function(file) {
      utils::write.table(rv$results_df, file,
                         sep = ",",
                         dec = ".",
                         row.names = FALSE,
                         fileEncoding = "CP1250",
                         quote = TRUE)
    }
  )

  output$dl_maps <- downloadHandler(
    filename = function() {
      paste0("NDOP_Maps_", if (is.null(rv$config)) "" else rv$config$reference_year, ".zip")
    },
    content = function(file) {
      file.copy(rv$maps_zip_path, file)
    }
  )
}

#' Run the NDOP Bulk Assessment Dashboard
#'
#' Screens a whole spreadsheet of taxa and writes one row per taxon plus a
#' distribution map per taxon.
#'
#' @details
#' Upload a CSV with a `TAXON` column (scientific names). `CELED` (family),
#' `SKUPINA` (taxonomic group) and `ID_TAXON` are optional; `ID_TAXON` is used
#' to build a link to the species' card on <https://portal.nature.cz>. The
#' separator and encoding are detected by trying `read.csv2`/`read.csv` with
#' CP1250 and UTF-8 and keeping whichever yields a `TAXON` column.
#'
#' Each taxon goes through the same pipeline as [assess_species()] and the
#' single-taxon app ([assess_taxon()]) with NDOP counts not used
#' (`evaluate_pop = FALSE`).
#'
#' @section The export:
#' Every row has a **proposed category** (`Kategorie (automatická)`) and the
#' criteria behind it in the established format (`Kriteria (automatická)`).
#' Alongside them: `Kritéria (IUCN zápis)` (IUCN Annex 2 notation),
#' `Opora návrhu` (whether the proposal is evidenced or rests on indications
#' from the records), `Nutno doložit` (what still needs evidence),
#' `Rozsah kategorií` (plausible range), `Příznaky` (candidate flags) and
#' `issue_*` columns giving the reason for every empty value.
#'
#' @section Credentials:
#' NDOP requires a login. Set `NDOP_USER` and `NDOP_PWD` in `~/.Renviron`, or
#' run `rndop::isop_login()` once, before starting the app. A failed download
#' leaves the category empty and is explained in `issue_download`.
#'
#' @param ... Passed to [shiny::runApp()], e.g. `port` or `launch.browser`.
#' @return No return value. Side effect: launches the Shiny application.
#' @seealso [run_app()] for the single-species expert dashboard;
#'   [batch_assess()] for the same pipeline without a UI.
#' @examples
#' if (interactive()) {
#'   run_bulk_app()
#' }
#' @import shiny
#' @export
run_bulk_app <- function(...) {
  # 100 MB upload cap, restored on exit so launching the app does not leave a
  # global option changed behind it.
  old <- options(shiny.maxRequestSize = 100 * 1024^2)
  on.exit(options(old), add = TRUE)

  shiny::runApp(shiny::shinyApp(bulk_ui(), bulk_server), ...)
}

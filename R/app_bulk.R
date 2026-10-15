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

      numericInput("window", "Window Length (Years):", value = 10, min = 1),
      p(tags$small(paste(
        "The recent window is the last N years; the comparison window is the",
        "N years immediately before it. Both windows are the same length, so",
        "the change between them is a like-for-like comparison."
      ))),
      numericInput("cell_size", "AOO Grid Size (km):", value = 2, min = 1),
      p(tags$small(paste(
        "IUCN Guidelines 4.10.3: 2 x 2 km is the reference scale for criteria",
        "B2 and D2. Other scales are not comparable with the thresholds."
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
    log = character(0)
  )

  log_msg <- function(msg) {
    rv$log <- c(rv$log, paste(Sys.time(), "-", msg))
  }

  observeEvent(input$run_bulk, {
    req(input$file1)

    sp_col <- "TAXON"
    celed_col <- "CELED"
    group_col <- "SKUPINA"
    id_taxon_col <- "ID_TAXON"

    # Read the CSV, preferring whichever separator actually yields the
    # expected TAXON column. read.csv2() on a comma-separated file silently
    # returns a single glued-together column, which used to surface only as
    # "Found 0 unique species" with no explanation.
    df_input <- tryCatch({
      candidates <- list(
        tryCatch(read.csv2(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "CP1250"),
                 error = function(e) NULL),
        tryCatch(read.csv(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "CP1250"),
                 error = function(e) NULL),
        tryCatch(read.csv2(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                 error = function(e) NULL),
        tryCatch(read.csv(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
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

    # --- Assessment windows -------------------------------------------------
    # The recent and comparison windows are of equal length and adjacent. The
    # previous version compared the last N years against *all* earlier records
    # combined, which meant a 10-year window was pitted against a century of
    # accumulated recording. That comparison manufactures an apparent decline
    # for almost any taxon and systematically inflated threat categories.
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    win <- max(1, as.numeric(input$window))
    recent_end <- current_year
    recent_start <- recent_end - win + 1
    comp_end <- recent_start - 1
    comp_start <- comp_end - win + 1

    grid_size_m <- input$cell_size * 1000
    grid_area <- input$cell_size^2
    if (!isTRUE(all.equal(input$cell_size, 2))) {
      log_msg(paste0("WARNING: AOO grid set to ", input$cell_size,
                     " km. IUCN Guidelines 4.10.3 require 2 x 2 km for B2/D2; ",
                     "categories derived at other scales are not comparable."))
    }

    log_msg(paste0("Found ", length(species_list), " unique species. Recent window ",
                   recent_start, "-", recent_end, "; comparison window ",
                   comp_start, "-", comp_end, "."))

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

        input_celed <- lookup(celed_col, sp)
        input_group <- lookup(group_col, sp)
        input_id_taxon <- lookup(id_taxon_col, sp)

        karta_druhu_val <- if (!is.na(input_id_taxon) && input_id_taxon != "") {
          paste0("https://portal.nature.cz/w/druh-", input_id_taxon)
        } else {
          NA_character_
        }

        occ_raw <- tryCatch(ndopred::get_assessment_data(sp), error = function(e) NULL)

        res_row <- data.frame(
          `Taxonomická skupina` = input_group,
          `Čeleď` = input_celed,
          Druh = sp,
          `Kategorie (automatická)` = NA_character_,
          `Kriteria (automatická)` = NA_character_,
          `Období staré` = paste0(comp_start, "-", comp_end),
          `Období nové` = paste0(recent_start, "-", recent_end),
          `AOO starý (km2)` = NA_real_,
          `AOO nový (km2)` = NA_real_,
          `EOO starý (km2)` = NA_real_,
          `EOO nový (km2)` = NA_real_,
          `Počet lokalit starý` = NA_integer_,
          `Počet lokalit nový` = NA_integer_,
          `Grid nový (počet buněk)` = NA_integer_,
          `Změna AOO (%)` = NA_real_,
          `Změna EOO (%)` = NA_real_,
          `Záznamů celkem` = NA_integer_,
          `Záznamů nové období` = NA_integer_,
          `Poslední rok` = NA_integer_,
          `D2 kandidát (nutno posoudit ohrožení)` = NA_character_,
          `Karta druhu` = karta_druhu_val,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        if (!is.null(occ_raw) && nrow(occ_raw) > 0) {

          # Standardise the year column once. extract_year() copes with the
          # YYYYMMDD, ISO and Czech DD.MM.YYYY encodings that have all been
          # seen in NDOP exports; the previous substr(x, 1, 4) silently
          # produced NA for the last of these.
          occ_raw$year <- if ("ROK" %in% names(occ_raw)) {
            suppressWarnings(as.numeric(as.character(occ_raw$ROK)))
          } else {
            rep(NA_real_, nrow(occ_raw))
          }
          if ("DATUM_OD" %in% names(occ_raw)) {
            from_date <- ndopred::extract_year(occ_raw$DATUM_OD)
            gaps <- is.na(occ_raw$year)
            occ_raw$year[gaps] <- from_date[gaps]
          }
          occ_raw$ROK <- occ_raw$year

          if (is.na(res_row$`Taxonomická skupina`) && "KAT_TAX" %in% names(occ_raw)) {
            res_row$`Taxonomická skupina` <- as.character(occ_raw$KAT_TAX[1])
          }
          if (is.na(res_row$`Čeleď`) && "CELED" %in% names(occ_raw)) {
            res_row$`Čeleď` <- as.character(occ_raw$CELED[1])
          }

          has_year <- !is.na(occ_raw$year)
          occ_old <- occ_raw[has_year & occ_raw$year >= comp_start & occ_raw$year <= comp_end, ]
          occ_new <- occ_raw[has_year & occ_raw$year >= recent_start & occ_raw$year <= recent_end, ]

          # A single grid origin shared by both periods. Minimising the origin
          # separately for each period (IUCN 4.10.2) gives each period its own
          # grid, and the two AOO values would no longer be comparable.
          pooled <- suppressWarnings(rbind(occ_old, occ_new))
          shared_origin <- NULL
          if (nrow(pooled) > 0) {
            shared_origin <- tryCatch(
              suppressWarnings(ndopred::calculate_aoo(pooled, grid_size = grid_size_m)$origin),
              error = function(e) NULL
            )
          }

          safe_num <- function(expr, default = NA_real_) {
            val <- tryCatch(expr, error = function(e) NULL)
            if (is.null(val) || length(val) == 0) return(default)
            suppressWarnings(as.numeric(val[[1]]))
          }

          aoo_old <- safe_num(suppressWarnings(ndopred::calculate_aoo(
            occ_old, grid_size = grid_size_m, origin = shared_origin))$area_km2)
          aoo_new <- safe_num(suppressWarnings(ndopred::calculate_aoo(
            occ_new, grid_size = grid_size_m, origin = shared_origin))$area_km2)
          eoo_old <- safe_num(ndopred::calculate_eoo(occ_old)$area_km2)
          eoo_new <- safe_num(ndopred::calculate_eoo(occ_new)$area_km2)
          locs_old <- safe_num(suppressWarnings(ndopred::calculate_locations(occ_old)))
          locs_new <- safe_num(suppressWarnings(ndopred::calculate_locations(occ_new)))

          # IUCN Guidelines 4.9 (EOO >= AOO): shared with summarize_assessment()
          # below so the displayed columns and the automated category always
          # agree, and with assess_species()/batch_assess() for a consistent
          # non-Shiny batch pipeline.
          eoo_old <- ndopred::reconcile_eoo_aoo(eoo_old, aoo_old)
          eoo_new <- ndopred::reconcile_eoo_aoo(eoo_new, aoo_new)

          res_row$`AOO starý (km2)` <- aoo_old
          res_row$`AOO nový (km2)` <- aoo_new
          res_row$`EOO starý (km2)` <- eoo_old
          res_row$`EOO nový (km2)` <- eoo_new
          res_row$`Počet lokalit starý` <- locs_old
          res_row$`Počet lokalit nový` <- locs_new
          res_row$`Záznamů celkem` <- nrow(occ_raw)
          res_row$`Záznamů nové období` <- nrow(occ_new)

          if (!is.na(aoo_new)) {
            res_row$`Grid nový (počet buněk)` <- aoo_new / grid_area
          }

          pct <- function(before, after) {
            if (is.na(before) || is.na(after) || before <= 0) return(NA_real_)
            ((after - before) / before) * 100
          }
          aoo_change <- pct(aoo_old, aoo_new)
          eoo_change <- pct(eoo_old, eoo_new)
          locs_change <- pct(locs_old, locs_new)
          res_row$`Změna AOO (%)` <- aoo_change
          res_row$`Změna EOO (%)` <- eoo_change

          y_last <- suppressWarnings(max(occ_raw$year, na.rm = TRUE))
          if (!is.finite(y_last)) y_last <- NA
          res_row$`Poslední rok` <- y_last

          sum_obj <- tryCatch({
            ndopred::summarize_assessment(
              species = sp,
              eoo = list(area_km2 = eoo_new),
              aoo = list(area_km2 = aoo_new),
              trend = list(percent_change = aoo_change,
                           eoo_percent_change = eoo_change),
              locations = locs_new,
              locations_trend = locs_change,
              pop_metrics = list(),
              # Criteria C and D1 need mature-individual counts, which this
              # bulk pass does not compute; they are left to the
              # single-species app.
              evaluate_pop = FALSE,
              year_last = y_last,
              n_records = nrow(occ_raw)
            )
          }, error = function(e) {
            log_msg(paste("ERROR in automated category for", sp, ":", e$message))
            NULL
          })

          if (!is.null(sum_obj)) {
            res_row$`Kategorie (automatická)` <- sum_obj$result$Category
            # b(iii) (habitat decline) is no longer emitted by the engine at
            # all, so the string-surgery that used to strip "iii" from the
            # criteria code afterwards is gone with it.
            res_row$`Kriteria (automatická)` <- sum_obj$result$Criteria
            res_row$`D2 kandidát (nutno posoudit ohrožení)` <-
              if (isTRUE(sum_obj$details$d2_candidate)) "ano" else "ne"
          } else {
            res_row$`Kategorie (automatická)` <- "DD"
            res_row$`Kriteria (automatická)` <- "Inadequate information"
          }

          # --- MAP GENERATION ---
          map_file <- file.path(map_dir, paste0(gsub("[^A-Za-z0-9]", "_", sp), "_map.png"))
          tryCatch({
            p <- ndopred::plot_iucn(
              species_name = sp,
              occ_data = occ_raw,
              window = win,
              year_now = recent_end,
              grid_size = grid_size_m
            )

            if (!is.null(p)) {
              ggplot2::ggsave(
                filename = map_file,
                plot = p,
                width = 8,
                height = 6,
                dpi = 150,
                bg = "white"
              )
            }
          }, error = function(e) {
            log_msg(paste("Map generation failed for", sp, ":", e$message))
          })

        } else {
          fc <- if (!is.null(occ_raw)) attr(occ_raw, "filter_counts") else NULL
          if (!is.null(fc) && fc$raw > 0) {
            log_msg(paste0(
              "No usable records for: ", sp,
              " (raw=", fc$raw,
              ", with coords=", fc$coords,
              ", non-negative=", fc$non_negative,
              ", with katastr=", fc$has_katastr,
              ", verified=", fc$verified, ")"
            ))
          } else {
            log_msg(paste("No records found for:", sp))
          }
          res_row$`Kategorie (automatická)` <- "DD"
          res_row$`Kriteria (automatická)` <- "No records found"
          res_row$`Záznamů celkem` <- 0L
        }

        results_list[[i]] <- res_row
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
    head(rv$results_df, 10)
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
    filename = function() { paste0("NDOP_Bulk_Assessment_", Sys.Date(), ".csv") },
    content = function(file) {
      write.table(rv$results_df, file,
                  sep = ",",
                  dec = ".",
                  row.names = FALSE,
                  fileEncoding = "CP1250",
                  quote = TRUE)
    }
  )

  output$dl_maps <- downloadHandler(
    filename = function() { paste0("NDOP_Maps_", Sys.Date(), ".zip") },
    content = function(file) {
      file.copy(rv$maps_zip_path, file)
    }
  )
}

#' Run the NDOP Bulk Assessment Dashboard
#'
#' Screens a whole spreadsheet of taxa on the criteria that can be automated
#' from occurrence records alone, and writes one row per taxon plus a
#' distribution map per taxon.
#'
#' @details
#' Upload a CSV with a `TAXON` column (scientific names). `CELED` (family),
#' `SKUPINA` (taxonomic group) and `ID_TAXON` are optional; `ID_TAXON` is used
#' to build a link to the species' card on <https://portal.nature.cz>. The
#' separator and encoding are detected by trying `read.csv2`/`read.csv` with
#' CP1250 and UTF-8 and keeping whichever yields a `TAXON` column.
#'
#' For each taxon the app downloads NDOP records via [get_assessment_data()],
#' computes EOO, AOO and locations for the recent window and for the
#' equal-length window immediately before it, and runs the metrics through
#' [summarize_assessment()] with `evaluate_pop = FALSE` — criteria C and D1
#' need a population estimate, which this pass does not attempt. Restricted
#' taxa are flagged in a `D2 kandidát` column for the assessor rather than
#' listed as VU (see [summarize_assessment()] on criterion D2).
#'
#' Results download as CSV; maps download as a ZIP of PNGs.
#'
#' @section Credentials:
#' NDOP requires a login. Set `NDOP_USER` and `NDOP_PWD` in `~/.Renviron`, or
#' run `rndop::isop_login()` once, before starting the app. Without them every
#' taxon returns "No records found".
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

library(shiny)
library(shinythemes)
library(dplyr)
library(zip)
# library(ndopred) # Ensure your package is loaded

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("NDOP Bulk Assessor: Automated IUCN Metrics"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      p(tags$small("The CSV must contain a column named 'Species' or 'Druh'.")),

      numericInput("window", "Recent Time Window (Years):", value = 10, min = 1),
      numericInput("cell_size", "AOO Grid Size (km):", value = 2, min = 1),

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

server <- function(input, output, session) {

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

    df_input <- tryCatch({
      temp <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      if(ncol(temp) == 1 && grepl(";", temp[1,1])) {
        read.csv2(input$file1$datapath, stringsAsFactors = FALSE)
      } else {
        temp
      }
    }, error = function(e) {
      log_msg("Error reading CSV.")
      return(NULL)
    })

    req(df_input)

    sp_col <- grep("species|druh|taxon", names(df_input), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(sp_col)) {
      log_msg("Could not find a column named 'Species' or 'Druh' in the CSV.")
      return()
    }

    species_list <- unique(trimws(df_input[[sp_col]]))
    species_list <- species_list[species_list != "" & !is.na(species_list)]

    log_msg(paste("Found", length(species_list), "unique species. Starting assessment..."))

    temp_dir <- tempdir()
    map_dir <- file.path(temp_dir, "ndop_maps")
    if (dir.exists(map_dir)) unlink(map_dir, recursive = TRUE)
    dir.create(map_dir)

    results_list <- list()
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    recent_cutoff <- current_year - input$window

    withProgress(message = 'Running Bulk Assessment', value = 0, {

      for (i in seq_along(species_list)) {
        sp <- species_list[i]
        incProgress(1/length(species_list), detail = paste("Processing", sp))
        log_msg(paste("Fetching data for:", sp))

        occ_raw <- tryCatch(ndopred::get_assessment_data(sp), error = function(e) NULL)

        res_row <- data.frame(
          `Taxonomická skupina` = NA_character_,
          `Čeleď` = NA_character_,
          Druh = sp,
          `Kategorie (automatická)` = NA_character_,
          `AOO starý (km2)` = NA_real_,
          `AOO nový (km2)` = NA_real_,
          `EOO starý (km2)` = NA_real_,
          `EOO nový (km2)` = NA_real_,
          `Počet lokalit starý` = NA_integer_,
          `Počet lokalit nový` = NA_integer_,
          `2x2 grid nový (počet)` = NA_integer_,
          `Pokles AOO (%)` = NA_real_,
          `Pokles EOO (%)` = NA_real_,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        if (!is.null(occ_raw) && nrow(occ_raw) > 0) {

          # 1. The Ultimate Date Extractor (Legacy DB Patch)
          # Locate columns ignoring case to prevent API variance crashes
          col_rok <- grep("^ROK$", names(occ_raw), ignore.case = TRUE, value = TRUE)
          col_datum <- grep("^DATUM_OD$", names(occ_raw), ignore.case = TRUE, value = TRUE)

          # Initialise ROK if completely missing, otherwise force coercion to numeric
          if (length(col_rok) == 0) {
            occ_raw$ROK <- NA_real_
            col_rok <- "ROK"
          } else {
            occ_raw[[col_rok[1]]] <- suppressWarnings(as.numeric(as.character(occ_raw[[col_rok[1]]])))
          }

          # Extract historic years from DATUM_OD using the first 4 characters
          if (length(col_datum) > 0) {
            extracted_years <- suppressWarnings(
              as.numeric(substr(as.character(occ_raw[[col_datum[1]]]), 1, 4))
            )

            nas_in_rok <- is.na(occ_raw[[col_rok[1]]])
            occ_raw[[col_rok[1]]][nas_in_rok] <- extracted_years[nas_in_rok]
          }

          # Standardise the final column name to exactly "ROK" for downstream logic
          names(occ_raw)[names(occ_raw) == col_rok[1]] <- "ROK"

          if ("KAT_TAX" %in% names(occ_raw)) res_row$`Taxonomická skupina` <- occ_raw$KAT_TAX[1]
          if ("CELED" %in% names(occ_raw)) res_row$`Čeleď` <- occ_raw$CELED[1]

          # The subsets will now successfully inherit the patched historic records
          occ_old <- occ_raw[!is.na(occ_raw$ROK) & occ_raw$ROK < recent_cutoff, ]
          occ_new <- occ_raw[!is.na(occ_raw$ROK) & occ_raw$ROK >= recent_cutoff, ]

          safe_metric <- function(calc_func, data, ...) {
            if (nrow(data) == 0) return(0)
            res <- tryCatch(calc_func(data, ...), error = function(e) NA)

            if (is.list(res)) {
              if ("area_km2" %in% names(res)) return(as.numeric(res$area_km2))
              if ("n_locations" %in% names(res)) return(as.numeric(res$n_locations))
              if (length(res) > 0) return(as.numeric(res[[1]]))
            } else if (is.numeric(res) || is.character(res)) {
              return(suppressWarnings(as.numeric(res)))
            }
            return(NA)
          }

          grid_size_m <- input$cell_size * 1000

          res_row$`AOO starý (km2)` <- safe_metric(ndopred::calculate_aoo, occ_old, grid_size = grid_size_m)
          res_row$`AOO nový (km2)` <- safe_metric(ndopred::calculate_aoo, occ_new, grid_size = grid_size_m)
          res_row$`EOO starý (km2)` <- safe_metric(ndopred::calculate_eoo, occ_old)
          res_row$`EOO nový (km2)` <- safe_metric(ndopred::calculate_eoo, occ_new)
          res_row$`Počet lokalit starý` <- safe_metric(ndopred::calculate_locations, occ_old)
          res_row$`Počet lokalit nový` <- safe_metric(ndopred::calculate_locations, occ_new)

          enforce_iucn_logic <- function(eoo, aoo) {
            if (is.na(aoo) || aoo == 0) return(eoo)
            if (is.na(eoo) || eoo < aoo) return(aoo)
            return(eoo)
          }

          res_row$`EOO starý (km2)` <- enforce_iucn_logic(res_row$`EOO starý (km2)`, res_row$`AOO starý (km2)`)
          res_row$`EOO nový (km2)` <- enforce_iucn_logic(res_row$`EOO nový (km2)`, res_row$`AOO nový (km2)`)

          grid_area <- input$cell_size^2
          if (!is.na(res_row$`AOO nový (km2)`)) {
            res_row$`2x2 grid nový (počet)` <- res_row$`AOO nový (km2)` / grid_area
          }

          trend_res <- tryCatch(
            ndopred::calculate_trend(
              occ_raw,
              recent_start = recent_cutoff,
              recent_end = current_year,
              comp_start = recent_cutoff - input$window,
              comp_end = recent_cutoff - 1
            ),
            error = function(e) list()
          )

          t_val <- NA
          if ("percent_change" %in% names(trend_res)) t_val <- trend_res$percent_change
          if ("aoo_change" %in% names(trend_res)) res_row$`Pokles AOO (%)` <- trend_res$aoo_change else res_row$`Pokles AOO (%)` <- t_val
          if ("eoo_change" %in% names(trend_res)) res_row$`Pokles EOO (%)` <- trend_res$eoo_change

          y_last <- suppressWarnings(max(occ_raw$ROK, na.rm = TRUE))
          if (is.infinite(y_last)) y_last <- NA

          sum_obj <- tryCatch({
            safe_locs <- if(is.na(res_row$`Počet lokalit nový`)) 0 else res_row$`Počet lokalit nový`

            ndopred::summarize_assessment(
              species = sp,
              eoo = list(area_km2 = res_row$`EOO nový (km2)`),
              aoo = list(area_km2 = res_row$`AOO nový (km2)`),
              trend = list(percent_change = t_val),
              locations = safe_locs,
              pop_metrics = list(decline_rate = NA, fluct_ratio = NA, total_mature = NA, max_subpop = NA),
              evaluate_pop = FALSE,
              year_last = y_last,
              n_records = nrow(occ_raw)
            )
          }, error = function(e) {
            log_msg(paste("ERROR in automated category for", sp, ":", e$message))
            return(NULL)
          })

          if (!is.null(sum_obj)) {
            res_row$`Kategorie (automatická)` <- sum_obj$result$Category
          }

          map_file <- file.path(map_dir, paste0(gsub("[^A-Za-z0-9]", "_", sp), "_map.png"))
          tryCatch({
            png(map_file, width = 1000, height = 800, res = 150)
            ndopred::plot_iucn(sp, occ_raw, window = input$window)
            dev.off()
          }, error = function(e) {
            log_msg(paste("Map generation failed for", sp, ":", e$message))
            if(file.exists(map_file)) unlink(map_file)
          })

        } else {
          log_msg(paste("No records found for:", sp))
          res_row$`Kategorie (automatická)` <- "DD"
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

shinyApp(ui, server)

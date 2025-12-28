library(shiny)
library(ndopred)
library(ggplot2)
library(sf)
library(dplyr)

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("NDOP Red List Assessor v0.1"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Pulsatilla patens"),
      numericInput("window", "Recent Window (Years):", value = 10),
      actionButton("run_calc", "Run Assessment", class = "btn-primary", style="width: 100%"),
      hr(),

      # --- NEW: Population Criteria Toggle ---
      h4("Assessment Settings"),
      checkboxInput("use_pop", "Apply Pop. Size Criteria (C/D)", value = FALSE),
      p(tags$small("Auto-selected for Vertebrates & Plants.")),
      hr(),

      h4("Expert Final Verdict"),
      uiOutput("live_category_ui"),
      textAreaInput("justification", "Expert Notes:", rows = 3),
      actionButton("save_assessment", "Finalize & Save", class = "btn-success", style="width: 100%")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Spatial & Metrics",
                 fluidRow(
                   column(8, plotOutput("map_plot")),
                   column(4,
                          h4("Automated Results"),
                          uiOutput("status_ui"),
                          tableOutput("metrics_table"))
                 )
        ),
        tabPanel("Expert Review",
                 uiOutput("verification_panel")
        ),
        tabPanel("Trend & History",
                 verbatimTextOutput("trend_text"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Helper: Rank Categories for automated comparison
  get_cat_rank <- function(cat) {
    match(cat, c("DD", "LC", "NT", "VU", "EN", "CR", "RE", "EX"))
  }

  # Helper: Define Groups that use Population Criteria by default
  #
  get_pop_groups <- function() {
    c(
      # Vertebrates
      "Ryby a mihule", "Obojživelníci", "Plazi", "Ptáci", "Letouni", "Savci",
      # Plants
      "Cévnaté rostliny"
    )
  }

  # --- 1. DATA FETCHING & METRIC CALCULATION (Heavy Lifting) ---
  # This runs only when "Run Assessment" is clicked.
  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {

      # A. Download
      occ_raw <- get_assessment_data(input$species_name)

      if (nrow(occ_raw) == 0) {
        return(NULL) # Handle in downstream reactive
      }

      # B. Data Cleaning: Create 'ROK' from 'DATUM_OD'
      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all) && "DATUM_OD" %in% names(occ_all)) {
        occ_all$ROK <- as.numeric(format(as.Date(occ_all$DATUM_OD), "%Y"))
      }

      # C. Metrics
      incProgress(0.3, message = "Calculating spatial metrics...")
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window
      eoo <- calculate_eoo(occ_all, year_start = cutoff)
      aoo <- calculate_aoo(occ_all, year_start = cutoff)
      locs <- calculate_locations(occ_all, year_start = cutoff)
      trend <- calculate_trend(occ_all, window_years = input$window)

      incProgress(0.6, message = "Calculating population metrics...")
      pop_metrics <- calculate_pop_metrics(occ_all, window_years = input$window)

      # Determine Taxon Group (Look for KAT_TAX, SKUPINA, or OBLAST)
      taxon_group <- "Unknown"
      if ("KAT_TAX" %in% names(occ_all)) taxon_group <- unique(occ_all$KAT_TAX)[1]

      list(
        eoo = eoo, aoo = aoo, locs = locs, trend = trend,
        pop = pop_metrics, occ_all = occ_all,
        taxon_group = taxon_group, species = input$species_name
      )
    })
  })

  # --- 2. INTELLIGENT DEFAULT SETTING ---
  # Watch the downloaded data and update the checkbox based on taxonomy
  observeEvent(raw_data(), {
    req(raw_data())
    data <- raw_data()

    # Check if group is in the "Yes" list
    should_use_pop <- data$taxon_group %in% get_pop_groups()

    # Update the UI
    updateCheckboxInput(session, "use_pop", value = should_use_pop)
  })

  # --- 3. AUTOMATED ASSESSMENT LOGIC ---
  # This runs when data changes OR when the user toggles the checkbox
  assessment_results <- reactive({
    data <- raw_data()

    # Handle empty/null data
    if (is.null(data)) {
      return(list(res = data.frame(Species=input$species_name, Category="DD", Note="No records",
                                   EOO_km2=0, AOO_km2=0, Locations=0, Criteria="", Flags="None", Trend_Perc=NA),
                  trend = list(percent_change = NA), occ_all = data.frame(),
                  pop = list(total_mature=NA, decline_rate=NA, max_subpop=NA)))
    }

    # 1. Base Summary (Criteria A, B, D2)
    res <- summarize_assessment(data$species, data$eoo, data$aoo, data$trend, data$locs)

    # 2. EXTENDED AUTOMATION (C, D1, NT)
    # Controlled by the checkbox input$use_pop

    new_crit <- res$Criteria
    current_rank <- get_cat_rank(res$Category)
    pop <- data$pop

    # Only evaluate C and D1 if the checkbox is checked
    if (input$use_pop) {

      # --- D1 (Very Small Pop) ---
      if (!is.na(pop$total_mature)) {
        N <- pop$total_mature
        cat_d1 <- dplyr::case_when(N < 50 ~ "CR", N < 250 ~ "EN", N < 1000 ~ "VU", TRUE ~ "LC")

        if (get_cat_rank(cat_d1) > current_rank) {
          res$Category <- cat_d1
          current_rank <- get_cat_rank(cat_d1)
          new_crit <- paste(new_crit, "D1", sep=";")
        }
      }

      # --- C (Small Pop + Decline) ---
      if (!is.na(pop$total_mature) && !is.na(pop$decline_rate)) {
        N <- pop$total_mature
        Decline <- pop$decline_rate

        if (N < 10000 && Decline <= -10) {
          cat_c <- dplyr::case_when(N < 250 ~ "CR", N < 2500 ~ "EN", TRUE ~ "VU")
          if (get_cat_rank(cat_c) > current_rank) {
            res$Category <- cat_c
            current_rank <- get_cat_rank(cat_c)
            new_crit <- paste(new_crit, "C1", sep=";")
          } else if (get_cat_rank(cat_c) == current_rank) {
            new_crit <- paste(new_crit, "C1", sep=";")
          }
        }
      }
    } # End Population Logic

    # --- NT (Near Threatened) Logic ---
    if (res$Category %in% c("LC", "DD")) {
      is_nt <- FALSE
      nt_reason <- c()

      # NT A: Decline ~20-30%
      if (!is.na(res$Trend_Perc) && abs(as.numeric(res$Trend_Perc)) >= 20 && abs(as.numeric(res$Trend_Perc)) < 30) {
        is_nt <- TRUE; nt_reason <- c(nt_reason, "A")
      }

      # NT B: Close to thresholds
      if (res$EOO_km2 > 0 && res$EOO_km2 < 30000 && res$Locations <= 15) {
        is_nt <- TRUE; nt_reason <- c(nt_reason, "B")
      }

      # NT D1: Pop ~1000-2000 (Only if Pop criteria enabled)
      if (input$use_pop && !is.na(pop$total_mature)) {
        if (pop$total_mature >= 1000 && pop$total_mature < 2000) {
          is_nt <- TRUE; nt_reason <- c(nt_reason, "D1")
        }
      }

      if (is_nt) {
        res$Category <- "NT"
        res$Criteria <- paste("Close to:", paste(nt_reason, collapse="+"))
      }
    }

    # Final String Cleanup
    res$Criteria <- gsub("^;|;$", "", gsub(";;", ";", new_crit))
    if(res$Criteria == "") res$Criteria <- "None"

    list(res = res, trend = data$trend, occ_all = data$occ_all, pop = data$pop)
  })

  # --- 4. EXPERT PANEL & OUTPUTS ---
  output$verification_panel <- renderUI({
    req(assessment_results())
    res <- assessment_results()$res
    pop <- assessment_results()$pop

    curr_trend <- if(!is.na(res$Trend_Perc)) paste0(round(abs(res$Trend_Perc), 1), "%") else "NA"
    pop_str <- if(!is.na(pop$total_mature)) paste(pop$total_mature) else "Unknown"
    decline_str <- if(!is.na(pop$decline_rate)) paste0(pop$decline_rate, "%") else "Unknown"

    tagList(
      fluidRow(
        # --- CRITERION A ---
        column(4, wellPanel(
          h4("A. Population Reduction"),
          p(tags$small("Automated Trend: ", curr_trend)),
          checkboxGroupInput("check_a_type", "Criterion Type:", inline = TRUE,
                             choiceNames = list(tags$span("A1"), tags$span("A2"), tags$span("A3"), tags$span("A4")),
                             choiceValues = c("A1", "A2", "A3", "A4"),
                             selected = if(!is.na(res$Trend_Perc) && as.numeric(res$Trend_Perc) <= -30) "A2"
          ),
          numericInput("manual_a_trend", "Manual Reduction %:", value = NA, min = 0, max = 100),
          checkboxGroupInput("a_basis", "Basis:", inline = TRUE, choices = c("a","b","c","d","e"), selected = "b")
        )),

        # --- CRITERION B ---
        column(4, wellPanel(
          h4("B. Geographic Range"),
          p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1)))),
          checkboxInput("check_loc", "a) Fragmented / Low Locs", value = (res$Locations <= 10)),
          checkboxInput("check_b_decline", "b) Continuing Decline", value = (!is.na(res$Trend_Perc) && as.numeric(res$Trend_Perc) < 0)),
          checkboxInput("check_b_fluct", "c) Extreme Fluctuations", value = FALSE)
        )),

        # --- CRITERION C ---
        column(4, wellPanel(
          h4("C. Small Pop & Decline"),
          p(tags$small(paste0("Calc. Mature: ", pop_str))),
          numericInput("pop_size_c", "Mature Individuals:", value = pop$total_mature, min = 0),
          checkboxInput("check_c1", "C1. Rate of Decline", value = (!is.na(pop$decline_rate) && pop$decline_rate <= -10)),
          checkboxInput("check_c2", "C2. Subpops", value = FALSE),
          # UI feedback if criteria disabled
          if(!input$use_pop) p(style="color:red; font-size:0.8em;", "Pop. criteria currently disabled in settings.")
        ))
      ),
      fluidRow(
        # --- CRITERION D ---
        column(4, wellPanel(
          h4("D. Very Small or Restricted"),
          numericInput("pop_size_d1", "Mature Individuals (D1):", value = pop$total_mature, min = 0),
          hr(),
          checkboxInput("check_d2", "D2. Restricted AOO/Locs (VU)", value = (res$AOO_km2 < 20 || res$Locations <= 5)),
          if(!input$use_pop) p(style="color:red; font-size:0.8em;", "D1 currently disabled in settings.")
        )),

        # --- CRITERION E ---
        column(4, wellPanel(
          h4("E. Quantitative Analysis"),
          selectInput("check_e", "Probability of Extinction:", choices = c("None", "CR", "EN", "VU"))
        ))
      )
    )
  })

  # --- 5. EXPERT FINAL VERDICT LOGIC ---
  expert_final <- reactive({
    req(assessment_results())
    if (is.null(input$check_loc)) return(list(category = "LC", criteria = ""))

    res <- assessment_results()$res
    triggered_cats <- "LC"
    expert_codes <- c()

    # --- A ---
    if (length(input$check_a_type) > 0) {
      trend_val <- if (!is.na(input$manual_a_trend)) input$manual_a_trend else abs(as.numeric(res$Trend_Perc))
      a_cat <- if ("A1" %in% input$check_a_type) {
        dplyr::case_when(trend_val >= 90 ~ "CR", trend_val >= 70 ~ "EN", trend_val >= 50 ~ "VU", TRUE ~ "LC")
      } else {
        dplyr::case_when(trend_val >= 80 ~ "CR", trend_val >= 50 ~ "EN", trend_val >= 30 ~ "VU", TRUE ~ "LC")
      }
      if (a_cat != "LC") {
        triggered_cats <- c(triggered_cats, a_cat)
        expert_codes <- c(expert_codes, paste0(input$check_a_type[1], paste(input$a_basis, collapse="")))
      }
    }

    # --- B ---
    sub_count <- sum(isTRUE(input$check_loc), isTRUE(input$check_b_decline), isTRUE(input$check_b_fluct))
    if (sub_count >= 2) {
      b1_cat <- dplyr::case_when(res$EOO_km2 < 100 ~ "CR", res$EOO_km2 < 5000 ~ "EN", res$EOO_km2 < 20000 ~ "VU", TRUE ~ "LC")
      b2_cat <- dplyr::case_when(res$AOO_km2 < 10 ~ "CR", res$AOO_km2 < 500 ~ "EN", res$AOO_km2 < 2000 ~ "VU", TRUE ~ "LC")
      sub_str <- paste0(if(input$check_loc) "a", if(input$check_b_decline) "b(ii)", if(input$check_b_fluct) "c")
      if (b1_cat != "LC") { triggered_cats <- c(triggered_cats, b1_cat); expert_codes <- c(expert_codes, paste0("B1", sub_str)) }
      if (b2_cat != "LC") { triggered_cats <- c(triggered_cats, b2_cat); expert_codes <- c(expert_codes, paste0("B2", sub_str)) }
    }

    # --- C ---
    if (!is.na(input$pop_size_c) && input$pop_size_c < 10000) {
      if (isTRUE(input$check_c1) || isTRUE(input$check_c2)) {
        c_cat <- dplyr::case_when(input$pop_size_c < 250 ~ "CR", input$pop_size_c < 2500 ~ "EN", TRUE ~ "VU")
        triggered_cats <- c(triggered_cats, c_cat); expert_codes <- c(expert_codes, "C")
      }
    }

    # --- D ---
    if (!is.na(input$pop_size_d1)) {
      d1_cat <- dplyr::case_when(input$pop_size_d1 <= 50 ~ "CR", input$pop_size_d1 <= 250 ~ "EN", input$pop_size_d1 <= 1000 ~ "VU", TRUE ~ "LC")
      if (d1_cat != "LC") { triggered_cats <- c(triggered_cats, d1_cat); expert_codes <- c(expert_codes, "D1") }
    }
    if (isTRUE(input$check_d2)) { triggered_cats <- c(triggered_cats, "VU"); expert_codes <- c(expert_codes, "D2") }

    # --- E ---
    if (input$check_e != "None") { triggered_cats <- c(triggered_cats, input$check_e); expert_codes <- c(expert_codes, "E") }

    # Final Rank
    cat_levels <- c("LC", "NT", "VU", "EN", "CR") # Added NT to ranking
    final_cat <- cat_levels[max(match(triggered_cats, cat_levels), na.rm = TRUE)]

    return(list(category = final_cat, criteria = paste(unique(expert_codes), collapse = "; ")))
  })

  # --- OUTPUTS ---
  output$live_category_ui <- renderUI({
    exp <- expert_final()
    color <- if(exp$category %in% c("CR", "EN", "VU")) "#f2dede" else if(exp$category == "NT") "#fcf8e3" else "#dff0d8"
    div(style = paste0("background-color:", color, "; padding: 10px; border-radius: 5px; text-align: center; border: 1px solid #ccc;"),
        h2(exp$category, style="margin:0; font-weight: bold;"), p(tags$small(exp$criteria)))
  })

  output$status_ui <- renderUI({
    res <- assessment_results()$res
    div(style = "background-color:#eee; padding: 15px; border-radius: 5px; border: 1px solid;",
        h3(res$Category, style="margin-top:0;"), p(strong("Automated Criteria: "), res$Criteria))
  })

  output$metrics_table <- renderTable({
    data <- assessment_results(); res <- data$res; pop <- data$pop; exp <- expert_final()
    mat_val <- if(!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unknown"
    data.frame("Metric" = c("Taxon", "Expert Category", "Expert Criteria", "EOO", "AOO", "Locs", "Mature Individuals"),
               "Value" = c(res$Species, exp$category, exp$criteria, base::format(res$EOO_km2, nsmall=1),
                           base::format(res$AOO_km2, nsmall=1), as.character(res$Locations), mat_val))
  })

  output$map_plot <- renderPlot({
    data <- assessment_results()
    plot_iucn(data$res$Species, occ_data = data$occ_all, window = input$window)
  })
}

shinyApp(ui, server)

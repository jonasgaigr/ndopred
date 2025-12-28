library(shiny)
library(ndopred)
library(ggplot2)
library(sf)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("NDOP Red List Assessor v0.1"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Euoniticellus fulvus"),
      numericInput("window", "Recent Window (Years):", value = 10),
      actionButton("run_calc", "Run Assessment", class = "btn-primary", style="width: 100%"),
      hr(),
      h4("Expert Final Verdict"),
      # This will now display the LIVE recalculated category based on checkboxes
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
                 fluidRow(
                   column(12,
                          h4("Criterion Tickboxes (Pre-filled by Automation)"),
                          uiOutput("verification_panel")
                   )
                 )
        ),
        tabPanel("Trend & History",
                 verbatimTextOutput("trend_text"))
      )
    )
  )
)

server <- function(input, output, session) {

  # --- 1. Automated Calculation ---
  assessment_results <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {
      occ_all <- get_assessment_data(input$species_name)
      if (nrow(occ_all) == 0) {
        return(list(res = data.frame(Species=input$species_name, Category="DD", Note="No records",
                                     EOO_km2=0, AOO_km2=0, Locations=0, Criteria="", Flags="None"),
                    trend = list(percent_change = NA), occ_all = occ_all))
      }
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window
      eoo <- calculate_eoo(occ_all, year_start = cutoff)
      aoo <- calculate_aoo(occ_all, year_start = cutoff)
      locs <- calculate_locations(occ_all, year_start = cutoff)
      trend <- calculate_trend(occ_all, window_years = input$window)
      res <- summarize_assessment(input$species_name, eoo, aoo, trend, locs)
      list(res = res, trend = trend, occ_all = occ_all)
    })
  })

  # --- 2. Dynamic Tickboxes ---
  output$verification_panel <- renderUI({
    data <- assessment_results()
    res <- data$res

    # Logic to determine automated pre-fill states
    is_b1 <- grepl("B1", res$Criteria)
    is_b2 <- grepl("B2", res$Criteria)
    is_decline <- grepl("b\\(ii\\)", res$Criteria)
    is_d2 <- grepl("D2", res$Criteria)
    is_crit_a <- res$Flags != "None"

    tagList(
      fluidRow(
        column(6,
               wellPanel(
                 h4("Criterion A: Population Reduction"),
                 p(tags$small("Trend based on last 10 years of data.")),
                 checkboxInput("check_a", paste("A2: Significant Decline?", res$Flags), value = is_crit_a),

                 hr(),
                 h4("Criterion B: Geographic Range"),
                 checkboxGroupInput("check_b_type", "Thresholds met:",
                                    choices = c("B1: EOO < 20,000 km²" = "B1",
                                                "B2: AOO < 2,000 km²" = "B2"),
                                    selected = c(if(is_b1) "B1", if(is_b2) "B2")),
                 checkboxInput("check_loc", "a: Severely Fragmented / Low Locations (≤10)", value = (res$Locations <= 10)),
                 p(tags$strong("b: Continuing Decline in:")),
                 checkboxInput("check_b_ii", "(ii) Area of occupancy (AOO)", value = is_decline),
                 checkboxInput("check_b_iii", "(iii) Area, extent and/or quality of habitat", value = FALSE),
                 checkboxInput("check_b_iv", "(iv) Number of locations or subpopulations", value = FALSE),
                 checkboxInput("check_b_v", "(v) Number of mature individuals", value = FALSE)
               )
        ),
        column(6,
               wellPanel(
                 h4("Criterion C: Small Population & Decline"),
                 p(tags$small("Requires count of mature individuals.")),
                 checkboxInput("check_c", "C: < 10,000 mature individuals + decline", value = FALSE),

                 hr(),
                 h4("Criterion D: Very Small or Restricted"),
                 checkboxInput("check_d1", "D1: < 1,000 mature individuals (VU/EN/CR)", value = FALSE),
                 checkboxInput("check_d2", "D2: Very restricted AOO (<20 km²) or Locations (≤5)", value = is_d2),

                 hr(),
                 h4("Criterion E: Quantitative Analysis"),
                 checkboxInput("check_e", "E: Statistical Extinction Risk (PVA)", value = FALSE)
               )
        )
      )
    )
  })

  # --- 3. Reactive Recalculation ---
  # This recalculates the category LIVE as the expert clicks
  expert_final <- reactive({
    req(assessment_results())
    res <- assessment_results()$res

    # 1. Initialize logic variables
    cats <- "LC"
    expert_codes <- c()

    # --- Criterion A ---
    if (isTRUE(input$check_a)) {
      a_cat <- dplyr::case_when(
        res$Trend_Perc <= -80 ~ "CR",
        res$Trend_Perc <= -50 ~ "EN",
        res$Trend_Perc <= -30 ~ "VU",
        TRUE ~ "LC"
      )
      if (a_cat != "LC") {
        cats <- c(cats, a_cat)
        expert_codes <- c(expert_codes, "A2")
      }
    }

    # --- Criterion B ---
    # Fix the '||' error by checking if "B1" or "B2" is in the selected vector
    b1_selected <- "B1" %in% input$check_b_type
    b2_selected <- "B2" %in% input$check_b_type

    # Check for any 'b' sub-criteria
    b_sub_codes <- c(
      if(isTRUE(input$check_b_ii)) "ii",
      if(isTRUE(input$check_b_iii)) "iii",
      if(isTRUE(input$check_b_iv)) "iv",
      if(isTRUE(input$check_b_v)) "v"
    )

    if ((b1_selected || b2_selected) && isTRUE(input$check_loc) && length(b_sub_codes) > 0) {
      sub_str <- paste0("ab(", paste(b_sub_codes, collapse = ","), ")")

      if (b1_selected) {
        b1_cat <- dplyr::case_when(res$EOO_km2 < 100 ~ "CR", res$EOO_km2 < 5000 ~ "EN", TRUE ~ "VU")
        cats <- c(cats, b1_cat)
        expert_codes <- c(expert_codes, paste0("B1", sub_str))
      }
      if (b2_selected) {
        b2_cat <- dplyr::case_when(res$AOO_km2 < 10 ~ "CR", res$AOO_km2 < 500 ~ "EN", TRUE ~ "VU")
        cats <- c(cats, b2_cat)
        expert_codes <- c(expert_codes, paste0("B2", sub_str))
      }
    }

    # --- Criterion D ---
    if (isTRUE(input$check_d2)) {
      cats <- c(cats, "VU")
      expert_codes <- c(expert_codes, "D2")
    }

    # 2. Final Hierarchical Category
    levels <- c("DD", "LC", "VU", "EN", "CR", "RE", "EX")
    final_cat <- levels[max(base::match(cats, levels), na.rm = TRUE)]

    # 3. Final Criteria String
    final_criteria <- if(length(expert_codes) > 0) paste(unique(expert_codes), collapse = "; ") else ""

    return(list(category = final_cat, criteria = final_criteria))
  })

  # --- 4. Live Category Display ---
  output$live_category_ui <- renderUI({
    cat <- expert_final()
    color <- if(cat %in% c("CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    div(style = paste0("background-color:", color, "; padding: 10px; border-radius: 5px; text-align: center; border: 1px solid #ccc;"),
        h2(cat, style="margin:0; font-weight: bold;"))
  })

  # (Previous outputs remain same, ensure they use assessment_results())
  output$map_plot <- renderPlot({
    data <- assessment_results()
    plot_iucn(data$res$Species, occ_data = data$occ_all, window = input$window)
  })

  output$status_ui <- renderUI({
    res <- assessment_results()$res
    color <- if(res$Category %in% c("RE", "CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    div(style = paste0("background-color:", color, "; padding: 15px; border-radius: 5px; border: 1px solid;"),
        h3(res$Category, style="margin-top:0;"),
        p(strong("Note: "), res$Note))
  })

  output$metrics_table <- renderTable({
    data <- assessment_results()
    res <- data$res
    exp <- expert_final() # Get the reactive expert data

    data.frame(
      "Component" = c(
        "Taxon",
        "Automated Category",
        "Expert Category",
        "Automated Criteria",
        "Expert Criteria",
        "EOO (km2)",
        "AOO (km2)",
        "Locations"
      ),
      "Value" = c(
        as.character(res$Species),
        as.character(res$Category),
        as.character(exp$category),
        as.character(res$Criteria),
        as.character(exp$criteria),
        base::format(res$EOO_km2, nsmall = 2),
        base::format(res$AOO_km2, nsmall = 2),
        base::format(res$Locations, nsmall = 0)
      )
    )
  }, striped = TRUE, spacing = 'm', width = '100%')

  # Update the Live Category UI in the sidebar
  output$live_category_ui <- renderUI({
    exp <- expert_final()
    color <- if(exp$category %in% c("CR", "EN", "VU")) "#f2dede" else "#dff0d8"

    div(style = paste0("background-color:", color, "; padding: 10px; border-radius: 5px; text-align: center; border: 1px solid #ccc;"),
        h2(exp$category, style="margin:0; font-weight: bold;"),
        p(tags$small(exp$criteria))) # Show the code (e.g. B2ab(ii)) under the big Category
  })

  output$trend_text <- renderPrint({ assessment_results()$trend })
}

shinyApp(ui, server)

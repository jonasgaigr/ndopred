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

    tagList(
      wellPanel(
        h5("Criterion B: Geographic Range"),
        checkboxInput("check_b1", "B1: EOO Threshold met", value = grepl("B1", res$Criteria)),
        checkboxInput("check_b2", "B2: AOO Threshold met", value = grepl("B2", res$Criteria)),
        checkboxInput("check_loc", "a: Low Locations / Fragmented", value = (res$Locations <= 10)),
        checkboxInput("check_b_decline", "b(ii): Continuing Decline in AOO", value = grepl("b\\(ii\\)", res$Criteria)),
        checkboxInput("check_b_habitat", "b(iii): Decline in Habitat Quality", value = FALSE),

        hr(),
        h5("Criterion A & D"),
        checkboxInput("check_a", paste("A2: Apply Trend Logic?", res$Flags), value = (res$Flags != "None")),
        checkboxInput("check_d2", "D2: Very Restricted (VU)", value = grepl("D2", res$Criteria))
      )
    )
  })

  # --- 3. Reactive Recalculation ---
  # This recalculates the category LIVE as the expert clicks
  expert_final <- reactive({
    req(assessment_results())
    res <- assessment_results()$res

    # Start with LC
    cats <- "LC"

    # Criterion B Logic: (Threshold) AND (Locations) AND (Decline)
    b_threshold <- input$check_b1 || input$check_b2
    if (b_threshold && input$check_loc && (input$check_b_decline || input$check_b_habitat)) {
      b_cat <- if(input$check_b1) {
        dplyr::case_when(res$EOO_km2 < 100 ~ "CR", res$EOO_km2 < 5000 ~ "EN", TRUE ~ "VU")
      } else {
        dplyr::case_when(res$AOO_km2 < 10 ~ "CR", res$AOO_km2 < 500 ~ "EN", TRUE ~ "VU")
      }
      cats <- c(cats, b_cat)
    }

    # Criterion A Logic (if expert confirms)
    if (isTRUE(input$check_a)) {
      a_cat <- dplyr::case_when(
        res$Trend_Perc <= -80 ~ "CR",
        res$Trend_Perc <= -50 ~ "EN",
        res$Trend_Perc <= -30 ~ "VU",
        TRUE ~ "LC"
      )
      cats <- c(cats, a_cat)
    }

    # Criterion D2
    if (isTRUE(input$check_d2)) cats <- c(cats, "VU")

    # Hierarchical Pick
    levels <- c("LC", "VU", "EN", "CR")
    final <- levels[max(match(cats, levels))]
    return(final)
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
    res <- assessment_results()$res
    data.frame(
      "Component" = c("Taxon", "Calculated", "Criteria", "EOO", "AOO", "Locs", "Trend"),
      "Value" = c(res$Species, res$Category, res$Criteria, res$EOO_km2, res$AOO_km2, res$Locations, res$Trend_Perc)
    )
  })

  output$trend_text <- renderPrint({ assessment_results()$trend })
}

shinyApp(ui, server)

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
      selectInput("final_cat", "Category Override:",
                  choices = c("Auto", "EX", "RE", "CR", "EN", "VU", "LC", "DD")),
      textAreaInput("justification", "Notes:", rows = 3)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Spatial & Metrics",
                 fluidRow(
                   column(8, plotOutput("map_plot")),
                   column(4,
                          h4("Automated Results"),
                          uiOutput("status_ui"), # Fixed custom status display
                          tableOutput("metrics_table"))
                 )
        ),
        tabPanel("Trend & History",
                 verbatimTextOutput("trend_text"))
      )
    )
  )
)

server <- function(input, output, session) {

  # 1. THE SINGLE SOURCE OF TRUTH
  # This block runs ONCE per button click.
  assessment_results <- eventReactive(input$run_calc, {
    req(input$species_name)

    withProgress(message = 'Accessing NDOP...', value = 0, {
      # Download
      occ_all <- get_assessment_data(input$species_name)
      incProgress(0.4, message = "Processing spatial data...")

      if (nrow(occ_all) == 0) {
        return(list(
          res = data.frame(Species=input$species_name, Category="DD", Note="No records found.",
                           EOO_km2=0, AOO_km2=0, Locations=0),
          trend = list(percent_change = NA),
          occ_all = occ_all
        ))
      }

      # Calculations
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window
      eoo <- calculate_eoo(occ_all, year_start = cutoff)
      aoo <- calculate_aoo(occ_all, year_start = cutoff)
      locs <- calculate_locations(occ_all, year_start = cutoff)
      trend <- calculate_trend(occ_all, window_years = input$window)

      res <- summarize_assessment(input$species_name, eoo, aoo, trend, locs)
      incProgress(0.6, message = "Finalizing UI...")

      # Return the list with all objects
      list(res = res, trend = trend, occ_all = occ_all)
    })
  })

  # --- OUTPUTS (All calling assessment_results() now) ---

  output$map_plot <- renderPlot({
    # Access the reactive list
    data <- assessment_results()
    # Use the shared occ_all data to prevent a 2nd download
    plot_iucn(data$res$Species, occ_data = data$occ_all, window = input$window)
  })

  output$status_ui <- renderUI({
    data <- assessment_results()
    res <- data$res

    # Visual styling based on Category
    color <- if(res$Category %in% c("RE", "CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    text_color <- if(res$Category %in% c("RE", "CR", "EN", "VU")) "#a94442" else "#3c763d"

    div(style = paste0("background-color:", color, "; color:", text_color,
                       "; padding: 15px; border-radius: 5px; border: 1px solid;"),
        h3(res$Category, style="margin-top:0;"),
        p(strong("Note: "), res$Note))
  })

  output$metrics_table <- renderTable({
    data <- assessment_results()
    res <- data$res

    data.frame(
      "Assessment Component" = c(
        "Taxon",
        "Calculated Category",
        "Criteria String",
        "EOO (km2)",
        "AOO (km2)",
        "Locations",
        "Trend (10y %)",
        "Criterion A Flag"
      ),
      "Value" = c(
        as.character(res$Species),
        as.character(res$Category),
        as.character(res$Criteria),
        base::format(res$EOO_km2, nsmall = 2),
        base::format(res$AOO_km2, nsmall = 2),
        base::format(res$Locations, nsmall = 0),
        as.character(res$Trend_Perc),
        as.character(res$Flags)
      )
    )
  }, striped = TRUE, spacing = 'm', width = '100%')

  output$trend_text <- renderPrint({
    data <- assessment_results()
    data$trend
  })
}

shinyApp(ui, server)

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

  # 1. Create a reactive that ONLY updates when the button is clicked
  assessment_data <- eventReactive(input$run_calc, {
    req(input$species_name)

    # Everything inside here happens ONLY when button is pressed
    withProgress(message = 'Processing...', value = 0, {

      # Fetch All Data
      occ_all <- get_assessment_data(input$species_name)
      incProgress(0.3)

      # Define Window
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window
      occ_recent <- get_assessment_data(input$species_name, year_start = cutoff)
      incProgress(0.3)

      # Perform Logic
      if (nrow(occ_recent) == 0) {
        note <- if(nrow(occ_all) > 0) "Historical only." else "No records found."
        res <- data.frame(Species=input$species_name, EOO_km2=0, AOO_km2=0, Locations=0,
                          Trend_Perc="NA", Category="RE", Note=note)
        trend <- list(percent_change = NA)
      } else {
        eoo <- calculate_eoo(occ_all, year_start = cutoff)
        aoo <- calculate_aoo(occ_all, year_start = cutoff)
        locs <- calculate_locations(occ_all, year_start = cutoff)
        trend <- calculate_trend(occ_all, window_years = input$window)
        res <- summarize_assessment(input$species_name, eoo, aoo, trend, locs)
      }
      incProgress(0.4)

      # Return a list containing everything we need
      list(res = res, trend = trend, occ_recent = occ_recent)
    })
  })

  # --- OUTPUTS ---

  output$map_plot <- renderPlot({
    # Access the reactive result. If button hasn't been clicked, this stops here.
    data <- assessment_data()

    # We use the species name from the result to ensure it matches the calculation
    plot_iucn(data$res$Species, window = input$window)
  })

  output$status_ui <- renderUI({
    data <- assessment_data()
    res <- data$res

    color <- if(res$Category %in% c("RE", "CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    text_color <- if(res$Category %in% c("RE", "CR", "EN", "VU")) "#a94442" else "#3c763d"

    div(style = paste0("background-color:", color, "; color:", text_color,
                       "; padding: 15px; border-radius: 5px; border: 1px solid;"),
        h3(res$Category),
        p(res$Note))
  })

  output$metrics_table <- renderTable({
    data <- assessment_data()
    data$res[, c("Species", "Category", "EOO_km2", "AOO_km2", "Locations")]
  })

  output$trend_text <- renderPrint({
    data <- assessment_data()
    data$trend
  })
}

shinyApp(ui, server)

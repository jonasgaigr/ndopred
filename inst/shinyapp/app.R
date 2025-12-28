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

  # --- 2. Dynamic Verification Panel ---
  output$verification_panel <- renderUI({
    res <- assessment_results()$res

    tagList(
      fluidRow(
        column(3, wellPanel(
          h4("A. Population Reduction"),
          checkboxGroupInput("check_a_type", "Criteria Type:", inline = TRUE,
                             choices = c("A1", "A2", "A3", "A4"),
                             selected = if(as.numeric(res$Trend_Perc) <= -30) "A2"),
          checkboxGroupInput("a_basis", "Basis (a-e):", inline = TRUE,
                             choices = c("a","b","c","d","e"), selected = "b")
        )),
        column(3, wellPanel(
          h4("B. Geographic Range"),
          p(tags$small("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1))),
          checkboxInput("check_loc", "a. Fragmented / Low Locs", value = (res$Locations <= 10)),
          checkboxInput("check_b_decline", "b. Continuing Decline", value = (as.numeric(res$Trend_Perc) < 0)),
          checkboxInput("check_b_fluct", "c. Extreme Fluctuations", value = FALSE)
        )),
        column(3, wellPanel(
          h4("C. Small Pop & Decline"),
          numericInput("pop_size", "Mature Individuals:", value = NA, min = 0),
          checkboxInput("check_c1", "C1. Rate of decline met?", value = FALSE),
          checkboxInput("check_c2a1", "C2a(i). Subpop ≤ 1000", value = FALSE),
          checkboxInput("check_c2a2", "C2a(ii). 90-100% in one", value = FALSE),
          checkboxInput("check_c2b", "C2b. Extreme fluctuations", value = FALSE)
        )),
        column(3, wellPanel(
          h4("D & E. Restricted/Quant"),
          checkboxInput("check_d2", "D2. Very Restricted (VU)", value = (res$AOO_km2 < 20 || res$Locations <= 5)),
          hr(),
          selectInput("check_e", "E. Probability of Extinction:",
                      choices = c("None", "CR (≥50% in 10y)", "EN (≥20% in 20y)", "VU (≥10% in 100y)"))
        ))
      )
    )
  })

  # --- 3. Reactive Recalculation (The IUCN Logic Engine) ---
  expert_final <- reactive({
    req(assessment_results())
    res <- assessment_results()$res
    triggered_cats <- "LC"
    expert_codes <- c()

    # --- CRITERION A ---
    if (length(input$check_a_type) > 0) {
      trend_val <- abs(as.numeric(res$Trend_Perc))
      # Tiered thresholds: A1 (90/70/50) vs A2-4 (80/50/30)
      a_cat <- if("A1" %in% input$check_a_type) {
        dplyr::case_when(trend_val >= 90 ~ "CR", trend_val >= 70 ~ "EN", trend_val >= 50 ~ "VU", TRUE ~ "LC")
      } else {
        dplyr::case_when(trend_val >= 80 ~ "CR", trend_val >= 50 ~ "EN", trend_val >= 30 ~ "VU", TRUE ~ "LC")
      }
      if (a_cat != "LC") {
        triggered_cats <- c(triggered_cats, a_cat)
        expert_codes <- c(expert_codes, paste0(input$check_a_type[1], paste(input$a_basis, collapse="")))
      }
    }

    # --- CRITERION B: Must meet Threshold AND 2 of {a, b, c} ---
    if (sum(c(input$check_loc, input$check_b_decline, input$check_b_fluct)) >= 2) {
      if (res$EOO_km2 < 20000) {
        b1_cat <- dplyr::case_when(res$EOO_km2 < 100 ~ "CR", res$EOO_km2 < 5000 ~ "EN", TRUE ~ "VU")
        triggered_cats <- c(triggered_cats, b1_cat); expert_codes <- c(expert_codes, "B1")
      }
      if (res$AOO_km2 < 2000) {
        b2_cat <- dplyr::case_when(res$AOO_km2 < 10 ~ "CR", res$AOO_km2 < 500 ~ "EN", TRUE ~ "VU")
        triggered_cats <- c(triggered_cats, b2_cat); expert_codes <- c(expert_codes, "B2")
      }
    }

    # --- CRITERION C ---
    if (!is.na(input$pop_size) && input$pop_size < 10000) {
      c_cat <- dplyr::case_when(input$pop_size < 250 ~ "CR", input$pop_size < 2500 ~ "EN", TRUE ~ "VU")
      if (input$check_c1 || input$check_c2a1 || input$check_c2a2 || input$check_c2b) {
        triggered_cats <- c(triggered_cats, c_cat); expert_codes <- c(expert_codes, "C")
      }
    }

    # --- CRITERION D ---
    if (!is.na(input$pop_size) && input$pop_size <= 1000) {
      d1_cat <- dplyr::case_when(input$pop_size <= 50 ~ "CR", input$pop_size <= 250 ~ "EN", TRUE ~ "VU")
      triggered_cats <- c(triggered_cats, d1_cat); expert_codes <- c(expert_codes, "D1")
    }
    if (isTRUE(input$check_d2)) { triggered_cats <- c(triggered_cats, "VU"); expert_codes <- c(expert_codes, "D2") }

    # --- CRITERION E ---
    if (input$check_e != "None") {
      e_cat <- dplyr::case_when(grepl("CR", input$check_e) ~ "CR", grepl("EN", input$check_e) ~ "EN", TRUE ~ "VU")
      triggered_cats <- c(triggered_cats, e_cat); expert_codes <- c(expert_codes, "E")
    }

    cat_levels <- c("DD", "LC", "VU", "EN", "CR", "RE", "EX")
    final_cat <- cat_levels[max(base::match(triggered_cats, cat_levels), na.rm = TRUE)]
    return(list(category = final_cat, criteria = paste(unique(expert_codes), collapse = "; ")))
  })

  # --- 4. Outputs ---
  output$live_category_ui <- renderUI({
    exp <- expert_final()
    color <- if(exp$category %in% c("CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    div(style = paste0("background-color:", color, "; padding: 10px; border-radius: 5px; text-align: center; border: 1px solid #ccc;"),
        h2(exp$category, style="margin:0; font-weight: bold;"),
        p(tags$small(exp$criteria)))
  })

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
    data <- assessment_results(); res <- data$res; exp <- expert_final()
    data.frame(
      "Component" = c("Taxon", "Auto Category", "Expert Category", "Expert Criteria", "EOO", "AOO", "Locs"),
      "Value" = c(res$Species, res$Category, exp$category, exp$criteria,
                  base::format(res$EOO_km2, nsmall=1), base::format(res$AOO_km2, nsmall=1), res$Locations)
    )
  }, striped = TRUE, spacing = 'm', width = '100%')

  output$trend_text <- renderPrint({ assessment_results()$trend })
}

shinyApp(ui, server)

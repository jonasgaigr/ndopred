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

# ------------------------------------------------------------------------------
# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Helper: Rank Categories for automated comparison
  get_cat_rank <- function(cat) {
    # Higher index = Higher threat
    match(cat, c("DD", "LC", "VU", "EN", "CR", "RE", "EX"))
  }

  # --- 1. Automated Calculation & Logic Integration ---
  assessment_results <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {

      # 1. Download Data
      occ_raw <- get_assessment_data(input$species_name)

      if (nrow(occ_raw) == 0) {
        return(list(res = data.frame(Species=input$species_name, Category="DD", Note="No records",
                                     EOO_km2=0, AOO_km2=0, Locations=0, Criteria="", Flags="None", Trend_Perc=NA),
                    trend = list(percent_change = NA), occ_all = occ_raw,
                    pop = list(total_mature=NA, decline_rate=NA, max_subpop=NA)))
      }

      # 2. Data Cleaning: Create 'ROK' from 'DATUM_OD'
      # Critical fix for "object ROK not found" error
      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all) && "DATUM_OD" %in% names(occ_all)) {
        occ_all$ROK <- as.numeric(format(as.Date(occ_all$DATUM_OD), "%Y"))
      }

      # 3. Spatial Metrics
      incProgress(0.3, message = "Calculating spatial metrics...")
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window
      eoo <- calculate_eoo(occ_all, year_start = cutoff)
      aoo <- calculate_aoo(occ_all, year_start = cutoff)
      locs <- calculate_locations(occ_all, year_start = cutoff)
      trend <- calculate_trend(occ_all, window_years = input$window)

      # 4. Population Metrics (Criterion C/D)
      incProgress(0.6, message = "Calculating population metrics...")
      # Assumes calculate_pop_metrics is loaded in the environment
      pop_metrics <- calculate_pop_metrics(occ_all, window_years = input$window)

      # 5. Base Summary (B and D2 logic from package)
      res <- summarize_assessment(input$species_name, eoo, aoo, trend, locs)

      # ---------------------------------------------------------
      # 6. EXTENDED AUTOMATION: Integrate Criterion C & D1
      # ---------------------------------------------------------
      new_crit <- res$Criteria
      current_rank <- get_cat_rank(res$Category)

      # A. Check D1 (Very Small Population)
      if (!is.na(pop_metrics$total_mature)) {
        N <- pop_metrics$total_mature
        cat_d1 <- dplyr::case_when(N < 50 ~ "CR", N < 250 ~ "EN", N < 1000 ~ "VU", TRUE ~ "LC")

        if (get_cat_rank(cat_d1) > current_rank) {
          res$Category <- cat_d1
          current_rank <- get_cat_rank(cat_d1)
          new_crit <- paste(new_crit, "D1", sep=";")
        }
      }

      # B. Check C (Small Pop + Decline)
      # We assume C1 applies if decline rate is significant (<= -10%)
      if (!is.na(pop_metrics$total_mature) && !is.na(pop_metrics$decline_rate)) {
        N <- pop_metrics$total_mature
        Decline <- pop_metrics$decline_rate

        # Must meet Thresholds (<10k) AND Decline
        if (N < 10000 && Decline <= -10) {
          cat_c <- dplyr::case_when(N < 250 ~ "CR", N < 2500 ~ "EN", TRUE ~ "VU")

          if (get_cat_rank(cat_c) > current_rank) {
            res$Category <- cat_c
            current_rank <- get_cat_rank(cat_c)
            new_crit <- paste(new_crit, "C1", sep=";")
          } else if (get_cat_rank(cat_c) == current_rank) {
            # Append criteria if same level
            new_crit <- paste(new_crit, "C1", sep=";")
          }
        }
      }

      # Clean up criteria string
      res$Criteria <- gsub("^;|;$", "", gsub(";;", ";", new_crit))
      if(res$Criteria == "") res$Criteria <- "None"

      list(res = res, trend = trend, occ_all = occ_all, pop = pop_metrics)
    })
  })

  # --- 2. Expert Panel UI ---
  output$verification_panel <- renderUI({
    req(assessment_results())
    res <- assessment_results()$res
    pop <- assessment_results()$pop

    # Format strings
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
                             choiceNames = list(
                               tags$span("A1", title = "Causes clearly reversible AND understood AND ceased (90/70/50%)."),
                               tags$span("A2", title = "Causes NOT ceased OR NOT understood OR NOT reversible (80/50/30%)."),
                               tags$span("A3", title = "Projected future reduction."),
                               tags$span("A4", title = "Time period includes both past and future.")
                             ),
                             choiceValues = c("A1", "A2", "A3", "A4"),
                             selected = if(!is.na(res$Trend_Perc) && as.numeric(res$Trend_Perc) <= -30) "A2"
          ),

          numericInput("manual_a_trend", "Manual Reduction % (Override):", value = NA, min = 0, max = 100),

          checkboxGroupInput("a_basis", "Basis (a-e):", inline = TRUE,
                             choiceNames = list(
                               tags$span("a", title = "Direct observation"),
                               tags$span("b", title = "Index of abundance"),
                               tags$span("c", title = "Decline in AOO/EOO/Habitat"),
                               tags$span("d", title = "Exploitation"),
                               tags$span("e", title = "Biotic effects")
                             ),
                             choiceValues = c("a","b","c","d","e"), selected = "b"
          )
        )),

        # --- CRITERION B ---
        column(4, wellPanel(
          h4("B. Geographic Range"),
          p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1)))),

          checkboxInput("check_loc",
                        tags$span("a) Fragmented / Low Locs", title = "Severely fragmented OR Locations ≤ 10 (VU), 5 (EN), 1 (CR)"),
                        value = (res$Locations <= 10)),

          checkboxInput("check_b_decline",
                        tags$span("b) Continuing Decline", title = "Decline in EOO, AOO, Habitat, Locations, or Individuals"),
                        value = (!is.na(res$Trend_Perc) && as.numeric(res$Trend_Perc) < 0)),

          checkboxInput("check_b_fluct",
                        tags$span("c) Extreme Fluctuations", title = ">1 order of magnitude fluctuation"),
                        value = FALSE),

          p(tags$small("Rule: Must meet EOO/AOO threshold AND 2 of 3 subcriteria."))
        )),

        # --- CRITERION C ---
        column(4, wellPanel(
          h4("C. Small Pop & Decline"),
          p(tags$small(paste0("Calc. Mature: ", pop_str, " | C1 Trend: ", decline_str))),

          numericInput("pop_size_c", "Mature Individuals (Override):",
                       value = pop$total_mature, min = 0),

          p(tags$small("Thresholds: <250 CR | <2,500 EN | <10,000 VU")),

          checkboxInput("check_c1",
                        tags$span("C1. Rate of Decline", title = "Decline >10% (VU), >20% (EN), or >25% (CR) in 3 gen/10yrs"),
                        value = (!is.na(pop$decline_rate) && pop$decline_rate <= -10)),

          checkboxInput("check_c2",
                        tags$span("C2. Subpops or Fluctuations", title = "Small subpopulations or % in one subpop or fluctuations"),
                        value = FALSE)
        ))
      ),
      fluidRow(
        # --- CRITERION D ---
        column(4, wellPanel(
          h4("D. Very Small or Restricted"),

          # D1: Uses population count
          numericInput("pop_size_d1", "Mature Individuals (D1):",
                       value = pop$total_mature, min = 0),
          p(tags$small("D1 Thresholds: ≤50 CR | ≤250 EN | ≤1000 VU")),
          hr(),

          # D2: Restricted Area
          checkboxInput("check_d2",
                        tags$span("D2. Restricted AOO/Locs (VU)", title = "AOO < 20 km² or Locations ≤ 5. Triggers VU only."),
                        value = (res$AOO_km2 < 20 || res$Locations <= 5))
        )),

        # --- CRITERION E ---
        column(4, wellPanel(
          h4("E. Quantitative Analysis"),
          selectInput("check_e", "Probability of Extinction:",
                      choices = c("None", "CR", "EN", "VU"))
        ))
      )
    )
  })

  # --- 3. Reactive Logic: IUCN Rules ---
  expert_final <- reactive({
    req(assessment_results())
    # Critical: Wait for UI to render to avoid null errors
    if (is.null(input$check_loc)) return(list(category = "LC", criteria = ""))

    res <- assessment_results()$res
    triggered_cats <- "LC"
    expert_codes <- c()

    # --- Criterion A ---
    if (length(input$check_a_type) > 0) {
      trend_val <- if (!is.na(input$manual_a_trend)) input$manual_a_trend else abs(as.numeric(res$Trend_Perc))

      a_cat <- if ("A1" %in% input$check_a_type) {
        dplyr::case_when(trend_val >= 90 ~ "CR", trend_val >= 70 ~ "EN", trend_val >= 50 ~ "VU", TRUE ~ "LC")
      } else {
        dplyr::case_when(trend_val >= 80 ~ "CR", trend_val >= 50 ~ "EN", trend_val >= 30 ~ "VU", TRUE ~ "LC")
      }

      if (a_cat != "LC") {
        triggered_cats <- c(triggered_cats, a_cat)
        basis_str <- paste(input$a_basis, collapse="")
        expert_codes <- c(expert_codes, paste0(input$check_a_type[1], basis_str))
      }
    }

    # --- Criterion B ---
    sub_count <- sum(isTRUE(input$check_loc), isTRUE(input$check_b_decline), isTRUE(input$check_b_fluct))
    if (sub_count >= 2) {
      b1_cat <- dplyr::case_when(res$EOO_km2 < 100 ~ "CR", res$EOO_km2 < 5000 ~ "EN", res$EOO_km2 < 20000 ~ "VU", TRUE ~ "LC")
      b2_cat <- dplyr::case_when(res$AOO_km2 < 10 ~ "CR", res$AOO_km2 < 500 ~ "EN", res$AOO_km2 < 2000 ~ "VU", TRUE ~ "LC")

      sub_str <- paste0(if(input$check_loc) "a", if(input$check_b_decline) "b(ii)", if(input$check_b_fluct) "c")
      if (b1_cat != "LC") { triggered_cats <- c(triggered_cats, b1_cat); expert_codes <- c(expert_codes, paste0("B1", sub_str)) }
      if (b2_cat != "LC") { triggered_cats <- c(triggered_cats, b2_cat); expert_codes <- c(expert_codes, paste0("B2", sub_str)) }
    }

    # --- Criterion C ---
    if (!is.na(input$pop_size_c) && input$pop_size_c < 10000) {
      if (isTRUE(input$check_c1) || isTRUE(input$check_c2)) {
        c_cat <- dplyr::case_when(input$pop_size_c < 250 ~ "CR", input$pop_size_c < 2500 ~ "EN", TRUE ~ "VU")
        triggered_cats <- c(triggered_cats, c_cat)
        expert_codes <- c(expert_codes, "C")
      }
    }

    # --- Criterion D ---
    if (!is.na(input$pop_size_d1)) {
      d1_cat <- dplyr::case_when(input$pop_size_d1 <= 50 ~ "CR", input$pop_size_d1 <= 250 ~ "EN", input$pop_size_d1 <= 1000 ~ "VU", TRUE ~ "LC")
      if (d1_cat != "LC") { triggered_cats <- c(triggered_cats, d1_cat); expert_codes <- c(expert_codes, "D1") }
    }
    if (isTRUE(input$check_d2)) {
      triggered_cats <- c(triggered_cats, "VU")
      expert_codes <- c(expert_codes, "D2")
    }

    # --- Criterion E ---
    if (input$check_e != "None") {
      triggered_cats <- c(triggered_cats, input$check_e)
      expert_codes <- c(expert_codes, "E")
    }

    # Final Verdict
    cat_levels <- c("LC", "VU", "EN", "CR")
    final_cat <- cat_levels[max(match(triggered_cats, cat_levels), na.rm = TRUE)]
    return(list(category = final_cat, criteria = paste(unique(expert_codes), collapse = "; ")))
  })

  # --- Outputs ---
  output$live_category_ui <- renderUI({
    exp <- expert_final()
    color <- if(exp$category %in% c("CR", "EN", "VU")) "#f2dede" else "#dff0d8"
    div(style = paste0("background-color:", color, "; padding: 10px; border-radius: 5px; text-align: center; border: 1px solid #ccc;"),
        h2(exp$category, style="margin:0; font-weight: bold;"),
        p(tags$small(exp$criteria)))
  })

  output$status_ui <- renderUI({
    res <- assessment_results()$res
    div(style = "background-color:#eee; padding: 15px; border-radius: 5px; border: 1px solid;",
        h3(res$Category, style="margin-top:0;"), p(strong("Automated Criteria: "), res$Criteria))
  })

  output$metrics_table <- renderTable({
    data <- assessment_results(); res <- data$res; pop <- data$pop; exp <- expert_final()

    # Safely handle NA for table display
    mat_val <- if(!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unknown"

    data.frame(
      "Metric" = c("Taxon", "Expert Category", "Expert Criteria", "EOO", "AOO", "Locs", "Mature Individuals"),
      "Value" = c(res$Species, exp$category, exp$criteria,
                  base::format(res$EOO_km2, nsmall=1),
                  base::format(res$AOO_km2, nsmall=1),
                  as.character(res$Locations),
                  mat_val)
    )
  })

  output$map_plot <- renderPlot({
    data <- assessment_results()
    plot_iucn(data$res$Species, occ_data = data$occ_all, window = input$window)
  })
}

shinyApp(ui, server)

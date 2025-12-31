library(shiny)
library(ggplot2)
library(sf)
library(dplyr)

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),

  # Initialize Bootstrap Tooltips
  tags$head(
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip();
      });
    "))
  ),

  titlePanel("NDOP Red List Assessor v0.6"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Codocera ferruginea"),
      numericInput("window", "Recent Window (Years):", value = 10),
      actionButton("run_calc", "Run Assessment", class = "btn-primary", style="width: 100%"),
      hr(),

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

  # --- 1. LOCAL DEFINITION OF AUTOMATED FUNCTION (Your provided code) ---
  summarize_assessment <- function(species, eoo, aoo, trend, locations, pop_metrics) {

    eoo_val <- eoo$area_km2
    aoo_val <- aoo$area_km2
    locs_val <- locations

    # Safe numeric conversions for binary flags
    trend_val <- suppressWarnings(as.numeric(trend$percent_change))
    pop_decline <- suppressWarnings(as.numeric(pop_metrics$decline_rate))
    fluct_ratio <- suppressWarnings(as.numeric(pop_metrics$fluct_ratio))

    # Determine active sub-criteria flags (Binary)
    has_decline <- (!is.na(trend_val) && trend_val < 0) ||
      (!is.na(pop_decline) && pop_decline < 0)

    has_fluct <- (!is.na(fluct_ratio) && fluct_ratio > 10)

    # --- CASCADE LOGIC ---
    evaluate_b <- function(area_val, type) {
      t_cr <- if(type=="B1") 100 else 10
      t_en <- if(type=="B1") 5000 else 500
      t_vu <- if(type=="B1") 20000 else 2000

      if (area_val < t_cr) {
        met_a <- (locs_val == 1)
        if (sum(met_a, has_decline, has_fluct) >= 2) {
          code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
          return(list(cat="CR", code=code))
        }
      }
      if (area_val < t_en) {
        met_a <- (locs_val <= 5)
        if (sum(met_a, has_decline, has_fluct) >= 2) {
          code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
          return(list(cat="EN", code=code))
        }
      }
      if (area_val < t_vu) {
        met_a <- (locs_val <= 10)
        if (sum(met_a, has_decline, has_fluct) >= 2) {
          code <- paste0(type, if(met_a)"a"else"", if(has_decline)"b(ii)"else"", if(has_fluct)"c"else"")
          return(list(cat="VU", code=code))
        }
      }
      return(list(cat="LC", code=""))
    }

    b1_res <- evaluate_b(eoo_val, "B1")
    b2_res <- evaluate_b(aoo_val, "B2")

    cats <- c("LC", "NT", "VU", "EN", "CR")
    get_rank <- function(x) match(x, cats)

    final_cat <- "LC"; final_crit <- c()

    if (get_rank(b1_res$cat) > get_rank(final_cat)) { final_cat <- b1_res$cat; final_crit <- c(b1_res$code) }

    if (get_rank(b2_res$cat) > get_rank(final_cat)) {
      final_cat <- b2_res$cat; final_crit <- c(b2_res$code)
    } else if (get_rank(b2_res$cat) == get_rank(final_cat) && b2_res$cat != "LC") {
      final_crit <- c(final_crit, b2_res$code)
    }

    is_d2 <- (aoo_val < 20 || locs_val <= 5)
    if (is_d2) {
      if (get_rank(final_cat) < get_rank("VU")) { final_cat <- "VU"; final_crit <- c("D2") }
      else if (final_cat == "VU") { final_crit <- c(final_crit, "D2") }
    }

    return(data.frame(
      Species = species, Category = final_cat, Criteria = paste(unique(final_crit), collapse = "; "),
      EOO_km2 = round(eoo_val, 2), AOO_km2 = round(aoo_val, 2), Locations = locations,
      Trend_Perc = ifelse(is.na(trend$percent_change), "NA", round(trend$percent_change, 1)),
      Flags = "None", stringsAsFactors = FALSE
    ))
  }

  # --- 2. HELPER FUNCTIONS ---
  get_cat_rank <- function(cat) match(cat, c("DD", "LC", "NT", "VU", "EN", "CR", "RE", "EX"))
  get_pop_groups <- function() c("Ryby a mihule", "Obojživelníci", "Plazi", "Ptáci", "Letouni", "Savci", "Cévnaté rostliny")

  tooltip_span <- function(label, text) {
    tags$span(label, `data-toggle` = "tooltip", `data-placement` = "top", title = text, style = "border-bottom: 1px dotted #777; cursor: help; margin-right: 5px;")
  }

  get_val <- function(obj, field = "val") {
    if (is.list(obj) && field %in% names(obj)) return(obj[[field]])
    if (is.list(obj) && "area_km2" %in% names(obj)) return(obj[["area_km2"]])
    if (is.list(obj) && "n_locations" %in% names(obj)) return(obj[["n_locations"]])
    if (is.list(obj) && "percent_change" %in% names(obj)) return(obj[["percent_change"]])
    return(obj)
  }

  # --- 3. EXPERT STATE ---
  rv <- reactiveValues(
    a_type = character(0), a_basis = "b", manual_trend = NA,
    loc = FALSE,
    b_subs = character(0), c_subs = character(0),
    pop_c = NA, c1 = FALSE, c2 = FALSE,
    pop_d1 = NA, d2 = FALSE,
    e_cat = "None"
  )

  # --- 4. DATA FETCHING ---
  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {

      occ_raw <- tryCatch(ndopred::get_assessment_data(input$species_name), error = function(e) NULL)
      if (is.null(occ_raw) || nrow(occ_raw) == 0) return(NULL)

      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all) && "DATUM_OD" %in% names(occ_all)) {
        occ_all$ROK <- as.numeric(format(as.Date(occ_all$DATUM_OD), "%Y"))
      }

      incProgress(0.3, message = "Calculating metrics...")
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window

      list(
        eoo = calculate_eoo(occ_all, year_start = cutoff),
        aoo = calculate_aoo(occ_all, year_start = cutoff),
        locs = calculate_locations(occ_all, year_start = cutoff),
        trend = calculate_trend(occ_all, window_years = input$window),
        pop = calculate_pop_metrics(occ_all, window_years = input$window),
        occ_all = occ_all,
        taxon_group = if ("KAT_TAX" %in% names(occ_all)) unique(occ_all$KAT_TAX)[1] else "Unknown",
        species = input$species_name,
        window_used = input$window
      )
    })
  })

  # --- 5. DEFAULTS & STATE SYNC ---
  observeEvent(raw_data(), {
    req(raw_data())
    data <- raw_data()

    # Determines if this group uses population criteria by default
    is_pop_group <- data$taxon_group %in% get_pop_groups()
    updateCheckboxInput(session, "use_pop", value = is_pop_group)

    # Extract Values Safely
    trend_val <- suppressWarnings(as.numeric(get_val(data$trend, "percent_change")))
    locs_val  <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))
    aoo_val   <- suppressWarnings(as.numeric(get_val(data$aoo, "area_km2")))
    pop_decline <- suppressWarnings(as.numeric(data$pop$decline_rate))
    fluct_ratio <- suppressWarnings(as.numeric(data$pop$fluct_ratio))

    # --- PRE-FILL LOGIC ---
    b_sel <- c()

    # 1. Spatial Decline -> implies i(EOO) and ii(AOO)
    if (!is.na(trend_val) && trend_val < 0) {
      b_sel <- c(b_sel, "i", "ii")
    }

    # 2. Population Decline -> implies v(Mature)
    # CRITICAL FIX: Only add 'v' if it is a population group!
    if (is_pop_group && !is.na(pop_decline) && pop_decline < 0) {
      b_sel <- c(b_sel, "v")
    }

    c_sel <- c()
    # 3. Fluctuation -> implies iv(Mature) if pop group
    if (is_pop_group && !is.na(fluct_ratio) && fluct_ratio > 10) {
      c_sel <- c(c_sel, "iv")
    }

    # Prevent UI from wiping calculated defaults
    freezeReactiveValue(input, "check_b_subs")
    freezeReactiveValue(input, "check_c_subs")
    freezeReactiveValue(input, "check_loc")

    # Update Source of Truth
    rv$a_type <- if(!is.na(trend_val) && trend_val <= -30) "A2" else character(0)
    rv$manual_trend <- NA
    rv$loc <- (locs_val <= 10)
    rv$b_subs <- unique(b_sel)
    rv$c_subs <- unique(c_sel)
    rv$pop_c <- data$pop$total_mature
    rv$c1 <- (!is.na(pop_decline) && pop_decline <= -10)
    rv$c2 <- (length(c_sel) > 0)
    rv$pop_d1 <- data$pop$total_mature
    rv$d2 <- (aoo_val < 20 || locs_val <= 5)
    rv$e_cat <- "None"
  })

  # --- 6. UI OBSERVERS ---
  observeEvent(input$check_a_type, { rv$a_type <- input$check_a_type }, ignoreNULL = FALSE)
  observeEvent(input$manual_a_trend, { rv$manual_trend <- input$manual_a_trend }, ignoreNULL = FALSE)
  observeEvent(input$check_loc, { rv$loc <- input$check_loc })
  observeEvent(input$check_b_subs, { rv$b_subs <- input$check_b_subs }, ignoreNULL = FALSE)
  observeEvent(input$check_c_subs, { rv$c_subs <- input$check_c_subs }, ignoreNULL = FALSE)
  observeEvent(input$pop_size_c, { rv$pop_c <- input$pop_size_c })
  observeEvent(input$check_c1, { rv$c1 <- input$check_c1 })
  observeEvent(input$check_c2, { rv$c2 <- input$check_c2 })
  observeEvent(input$pop_size_d1, { rv$pop_d1 <- input$pop_size_d1 })
  observeEvent(input$check_d2, { rv$d2 <- input$check_d2 })
  observeEvent(input$check_e, { rv$e_cat <- input$check_e })

  # --- 7. AUTOMATED RESULTS DISPLAY ---
  assessment_results <- reactive({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    # Extract numeric locations for your specific function
    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))

    res <- summarize_assessment(data$species, data$eoo, data$aoo, data$trend, locs_numeric, data$pop)

    # Extended Logic for C/D1 (only if use_pop is TRUE)
    current_rank <- get_cat_rank(res$Category)
    new_crit <- res$Criteria
    pop <- data$pop
    if (input$use_pop) {
      if (!is.na(pop$total_mature)) {
        cat_d1 <- dplyr::case_when(pop$total_mature < 50 ~ "CR", pop$total_mature < 250 ~ "EN", pop$total_mature < 1000 ~ "VU", TRUE ~ "LC")
        if (get_cat_rank(cat_d1) > current_rank) { res$Category <- cat_d1; current_rank <- get_cat_rank(cat_d1); new_crit <- paste(new_crit, "D1", sep=";") }
      }
    }

    trend_val <- suppressWarnings(as.numeric(get_val(data$trend, "percent_change")))
    eoo_val <- suppressWarnings(as.numeric(get_val(data$eoo, "area_km2")))
    aoo_val <- suppressWarnings(as.numeric(get_val(data$aoo, "area_km2")))

    if (res$Category %in% c("LC", "DD")) {
      is_nt <- FALSE; nt_notes <- c()
      if (!is.na(trend_val) && abs(trend_val) >= 20 && abs(trend_val) < 30) { is_nt <- TRUE; nt_notes <- c(nt_notes, "A") }
      if ((eoo_val < 20000 || aoo_val < 2000) && locs_numeric <= 15) { is_nt <- TRUE; nt_notes <- c(nt_notes, "B") }
      if (is_nt) { res$Category <- "NT"; res$Criteria <- paste("Close to:", paste(unique(nt_notes), collapse="+")) }
    }
    res$Criteria <- gsub("^;|;$", "", gsub(";;", ";", new_crit)); if(res$Criteria == "") res$Criteria <- "None"
    list(res=res, trend=data$trend, occ_all=data$occ_all, pop=pop, window_used=data$window_used)
  })

  # --- 8. EXPERT FINAL VERDICT ---
  expert_final <- reactive({
    req(assessment_results())

    eoo_v <- suppressWarnings(as.numeric(get_val(raw_data()$eoo, "area_km2")))
    aoo_v <- suppressWarnings(as.numeric(get_val(raw_data()$aoo, "area_km2")))
    locs_v <- suppressWarnings(as.numeric(get_val(raw_data()$locs, "n_locations")))

    triggered_cats <- "LC"; expert_codes <- c()

    # A
    tr_val <- if(!is.na(rv$manual_trend)) rv$manual_trend else abs(suppressWarnings(as.numeric(get_val(raw_data()$trend, "percent_change"))))
    cat_A <- "LC"; code_A <- ""
    if (!is.na(tr_val) && length(rv$a_type) > 0) {
      cat_A <- if("A1" %in% rv$a_type) dplyr::case_when(tr_val>=90~"CR", tr_val>=70~"EN", tr_val>=50~"VU", TRUE~"LC")
      else dplyr::case_when(tr_val>=80~"CR", tr_val>=50~"EN", tr_val>=30~"VU", TRUE~"LC")
      if (cat_A != "LC") code_A <- paste0(rv$a_type[1], paste(input$a_basis, collapse=""))
    }

    # B (Logic)
    # FORCE IGNORE 'v' if input$use_pop is FALSE
    active_b_subs <- rv$b_subs
    active_c_subs <- rv$c_subs
    if (!input$use_pop) {
      active_b_subs <- setdiff(active_b_subs, "v") # Remove "Mature Individuals"
      active_c_subs <- setdiff(active_c_subs, "iv") # Remove "Mature Individuals"
    }

    build_sub_str <- function() {
      s <- ""
      if (length(active_b_subs) > 0) s <- paste0(s, "b(", paste(sort(active_b_subs), collapse=","), ")")
      if (length(active_c_subs) > 0) s <- paste0(s, "c(", paste(sort(active_c_subs), collapse=","), ")")
      return(s)
    }
    has_b <- (length(active_b_subs) > 0); has_c <- (length(active_c_subs) > 0)

    eval_b_expert <- function(area, type) {
      t_cr <- if(type=="B1") 100 else 10; t_en <- if(type=="B1") 5000 else 500; t_vu <- if(type=="B1") 20000 else 2000
      check_level <- function(thresh_locs) {
        met_a <- (rv$loc && locs_v <= thresh_locs)
        if (sum(met_a, has_b, has_c) >= 2) return(paste0(type, if(met_a)"a"else"", build_sub_str()))
        return(NULL)
      }
      if (area < t_cr) { r <- check_level(1); if(!is.null(r)) return(list(cat="CR", code=r)) }
      if (area < t_en) { r <- check_level(5); if(!is.null(r)) return(list(cat="EN", code=r)) }
      if (area < t_vu) { r <- check_level(10); if(!is.null(r)) return(list(cat="VU", code=r)) }
      return(list(cat="LC", code=""))
    }
    res_b1 <- eval_b_expert(eoo_v, "B1"); res_b2 <- eval_b_expert(aoo_v, "B2")

    # C, D, E (Only apply if use_pop is TRUE)
    cat_C <- "LC"; code_C <- ""
    cat_D1 <- "LC"; code_D1 <- ""
    if (input$use_pop) {
      if (!is.na(rv$pop_c) && rv$pop_c < 10000 && (rv$c1 || rv$c2)) {
        cat_C <- dplyr::case_when(rv$pop_c < 250 ~ "CR", rv$pop_c < 2500 ~ "EN", TRUE ~ "VU")
        code_C <- "C"
      }
      if (!is.na(rv$pop_d1)) {
        cat_D1 <- dplyr::case_when(rv$pop_d1<=50~"CR", rv$pop_d1<=250~"EN", rv$pop_d1<=1000~"VU", TRUE~"LC")
        if (cat_D1 != "LC") code_D1 <- "D1"
      }
    }

    cat_D2 <- if(rv$d2) "VU" else "LC"; code_D2 <- if(rv$d2) "D2" else ""
    cat_E <- rv$e_cat; code_E <- if(cat_E!="None") "E" else ""; if (cat_E == "None") cat_E <- "LC"

    # Rank
    rank_c <- c("LC", "NT", "VU", "EN", "CR"); get_r <- function(x) match(x, rank_c)
    ranks <- c(get_r(cat_A), get_r(res_b1$cat), get_r(res_b2$cat), get_r(cat_C), get_r(cat_D1), get_r(cat_E))
    max_rank <- max(ranks, na.rm=T)
    if (rv$d2 && max_rank <= get_r("VU")) max_rank <- max(max_rank, get_r("VU"))

    final_cat <- rank_c[max_rank]; final_codes <- c()
    if (code_A != "" && get_r(cat_A) == max_rank) final_codes <- c(final_codes, code_A)
    if (res_b1$code != "" && get_r(res_b1$cat) >= max_rank) final_codes <- c(final_codes, res_b1$code)
    if (res_b2$code != "" && get_r(res_b2$cat) >= max_rank) final_codes <- c(final_codes, res_b2$code)
    if (code_C != "" && get_r(cat_C) == max_rank) final_codes <- c(final_codes, code_C)
    if (code_D1 != "" && get_r(cat_D1) == max_rank) final_codes <- c(final_codes, code_D1)
    if (code_D2 != "" && final_cat == "VU") final_codes <- c(final_codes, code_D2)
    if (code_E != "" && get_r(cat_E) == max_rank) final_codes <- c(final_codes, code_E)

    return(list(category = final_cat, criteria = paste(unique(final_codes), collapse = "; ")))
  })

  # --- 9. UI RENDER ---
  output$verification_panel <- renderUI({
    req(assessment_results())
    res <- assessment_results()$res; pop <- assessment_results()$pop
    t_val <- suppressWarnings(as.numeric(get_val(res, "Trend_Perc")))
    curr_trend <- if(!is.na(t_val)) paste0(round(abs(t_val), 1), "%") else "NA"
    pop_str <- if(!is.na(pop$total_mature)) paste(pop$total_mature) else "Unknown"

    tagList(
      tags$script("$('[data-toggle=\"tooltip\"]').tooltip();"),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"), p(tags$small("Auto: ", curr_trend)),
                            checkboxGroupInput("check_a_type", "Type:", inline=T, choices=c("A1","A2","A3","A4"), selected=rv$a_type),
                            numericInput("manual_a_trend", "Manual %:", value=rv$manual_trend),
                            checkboxGroupInput("a_basis", "Basis:", inline=T, choices=c("a","b","c","d","e"), selected="b"))),

        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1)))),
                            checkboxInput("check_loc", tooltip_span("a) Frag/Locs", "Severe Frag OR Locs <= 10"), value=rv$loc),

                            tags$div(style="border-top: 1px solid #ccc; margin-top:5px; padding-top:5px;",
                                     tags$label("b) Continuing Decline:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_b_subs", label=NULL, inline=TRUE,
                                                        choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Habitat"), tooltip_span("iv", "Locs"), tooltip_span("v", "Mature")),
                                                        choiceValues = c("i", "ii", "iii", "iv", "v"), selected=rv$b_subs)),

                            tags$div(style="border-top: 1px solid #ccc; margin-top:5px; padding-top:5px;",
                                     tags$label("c) Extreme Fluctuations:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label=NULL, inline=TRUE,
                                                        choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Locs"), tooltip_span("iv", "Mature")),
                                                        choiceValues = c("i", "ii", "iii", "iv"), selected=rv$c_subs))
        )),

        column(4, wellPanel(h4("C. Small Pop"), if(input$use_pop) tagList(p(tags$small(paste0("Est: ", pop_str))),
                                                                          numericInput("pop_size_c", "Mature:", value=rv$pop_c),
                                                                          checkboxInput("check_c1", "C1. Decline", value=rv$c1),
                                                                          checkboxInput("check_c2", "C2. Sub/Fluct", value=rv$c2)) else p(style="color:#999", "Disabled")))
      ),
      fluidRow(
        column(4, wellPanel(h4("D. Very Small"), if(input$use_pop) numericInput("pop_size_d1", "D1 Mature:", value=rv$pop_d1) else p(style="color:#999", "D1 Disabled"),
                            checkboxInput("check_d2", "D2. Restricted", value=rv$d2))),
        column(4, wellPanel(h4("E. Quantitative"), selectInput("check_e", "Extinction Prob:", choices=c("None","CR","EN","VU"), selected=rv$e_cat)))
      )
    )
  })

  output$live_category_ui <- renderUI({
    exp <- expert_final(); color <- if(exp$category %in% c("CR","EN","VU")) "#f2dede" else if(exp$category=="NT") "#fcf8e3" else "#dff0d8"
    div(style=paste0("background-color:",color,"; padding:10px; border-radius:5px; text-align:center; border:1px solid #ccc;"), h2(exp$category, style="margin:0; font-weight:bold;"), p(tags$small(exp$criteria)))
  })
  output$status_ui <- renderUI({ res <- assessment_results()$res; div(style="background-color:#eee; padding:15px; border-radius:5px; border:1px solid;", h3(res$Category, style="margin-top:0;"), p(strong("Automated: "), res$Criteria)) })
  output$metrics_table <- renderTable({
    data <- assessment_results(); res <- data$res; pop <- data$pop; exp <- expert_final(); mat_val <- if(!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unknown"
    data.frame(Metric=c("Taxon","Expert Category","Expert Criteria","EOO","AOO","Locs","Mature","Fluctuation"), Value=c(res$Species, exp$category, exp$criteria, base::format(res$EOO_km2, nsmall=1), base::format(res$AOO_km2, nsmall=1), as.character(res$Locations), mat_val, if(!is.na(pop$fluct_ratio)) paste0(pop$fluct_ratio,"x") else "-"))
  })
  output$map_plot <- renderPlot({ d <- assessment_results(); plot_iucn(d$res$Species, occ_data=d$occ_all, window=d$window_used) })
}

shinyApp(ui, server)

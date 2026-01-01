library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(shinythemes)
# library(ndopred) # Ensure package is loaded

# ------------------------------------------------------------------------------
# UI DEFINITION
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),

  tags$head(
    tags$script(HTML("
      $(function () {
        $('body').tooltip({selector: '[data-toggle=\"tooltip\"]'});
      });
    "))
  ),

  titlePanel("NDOP Red List Assessor v0.1"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Onthophagus medius"),
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
                   column(8, plotOutput("map_plot", height = "500px")),
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

  # --- 1. HELPER FUNCTIONS ---
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

  # --- 2. REACTIVE STATE (Expert Overrides) ---
  rv <- reactiveValues(
    a_type = character(0), a_basis = "b", manual_trend = NA,
    loc = FALSE,
    b_subs = character(0), c_subs = character(0),
    pop_c = NA, c1 = FALSE, c2 = FALSE,
    pop_d1 = NA, d2 = FALSE,
    e_cat = "None",
    auto_res_str = ""
  )

  # --- 3. DATA FETCHING ---
  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {

      occ_raw <- tryCatch(ndopred::get_assessment_data(input$species_name), error = function(e) NULL)

      if (is.null(occ_raw) || nrow(occ_raw) == 0) {
        showNotification("No data found for this species.", type = "error")
        return(NULL)
      }

      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all) && "DATUM_OD" %in% names(occ_all)) {
        occ_all$ROK <- as.numeric(format(as.Date(occ_all$DATUM_OD), "%Y"))
      }

      incProgress(0.3, message = "Calculating metrics...")
      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - input$window

      eoo_res <- ndopred::calculate_eoo(occ_all, year_start = cutoff)
      aoo_res <- ndopred::calculate_aoo(occ_all, year_start = cutoff)
      locs_res <- ndopred::calculate_locations(occ_all, year_start = cutoff)
      trend_res <- ndopred::calculate_trend(occ_all, window_years = input$window)
      pop_res <- ndopred::calculate_pop_metrics(occ_all, window_years = input$window)

      list(
        eoo = eoo_res,
        aoo = aoo_res,
        locs = locs_res,
        trend = trend_res,
        pop = pop_res,
        occ_all = occ_all,
        taxon_group = if ("KAT_TAX" %in% names(occ_all)) unique(occ_all$KAT_TAX)[1] else "Unknown",
        species = input$species_name,
        window_used = input$window
      )
    })
  })

  # --- 4. PRE-FILL LOGIC (1:1 Sync) ---
  observeEvent(raw_data(), {
    req(raw_data())
    data <- raw_data()

    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))
    summary_obj <- ndopred::summarize_assessment(
      species = data$species, eoo = data$eoo, aoo = data$aoo, trend = data$trend,
      locations = locs_numeric, pop_metrics = data$pop
    )

    # 1. Detect Pop Group
    is_pop <- data$taxon_group %in% get_pop_groups()
    updateCheckboxInput(session, "use_pop", value = is_pop)

    # 2. Update Expert State from Details
    dets <- summary_obj$details
    rv$a_type <- dets$a_type
    rv$manual_trend <- NA
    rv$a_basis <- "b"
    rv$loc <- dets$loc_flag

    # --- CRITICAL FIX: FILTER INDICES ON LOAD ---
    # We strictly filter out 'v' and 'iv' from the expert state
    # if the species is not in a pop group.

    b_init <- dets$b_indices
    c_init <- dets$c_indices
    if (!is_pop) {
      b_init <- setdiff(b_init, "v")
      c_init <- setdiff(c_init, "iv")
    }

    rv$b_subs <- b_init
    rv$c_subs <- c_init

    rv$pop_c <- data$pop$total_mature
    rv$c1 <- (!is.na(data$pop$decline_rate) && data$pop$decline_rate <= -10)
    rv$c2 <- (length(dets$c_indices) > 0)
    rv$pop_d1 <- data$pop$total_mature
    rv$d2 <- dets$d2_flag
    rv$e_cat <- "None"
  })

  # --- 5. LOGIC HELPERS ---

  get_active_indices <- function(indices, type="decline") {
    active <- indices
    if (!input$use_pop) {
      if (type == "decline") active <- setdiff(active, "v")       # v = Mature
      if (type == "fluct")   active <- setdiff(active, "iv")      # iv = Mature
    }
    return(active)
  }

  # --- 6. AUTOMATED RESULTS DISPLAY ---
  # Re-calculates string for "Automated Results" box based on current Toggle state
  assessment_display <- reactive({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))

    # Get raw object (contains ALL flags including i,ii,iii,v)
    sum_obj <- ndopred::summarize_assessment(
      species = data$species, eoo = data$eoo, aoo = data$aoo, trend = data$trend,
      locations = locs_numeric, pop_metrics = data$pop
    )
    res <- sum_obj$result
    dets <- sum_obj$details

    # --- RECONSTRUCT AUTOMATED STRING BASED ON UI TOGGLE ---
    final_crit <- character(0)

    # 1. Filter indices based on toggle
    b_inds <- get_active_indices(dets$b_indices, "decline")
    c_inds <- get_active_indices(dets$c_indices, "fluct")

    has_b <- length(b_inds) > 0
    has_c <- length(c_inds) > 0
    has_a <- dets$loc_flag

    # 2. Re-evaluate B-Criteria levels with filtered indices
    recalc_b <- function(area, type) {
      t_cr <- if(type=="B1") 100 else 10; t_en <- if(type=="B1") 5000 else 500; t_vu <- if(type=="B1") 20000 else 2000

      curr_cat <- "LC"
      if (!is.na(area)) {
        if (area < t_cr) curr_cat <- "CR"
        else if (area < t_en) curr_cat <- "EN"
        else if (area < t_vu) curr_cat <- "VU"
      }

      if (curr_cat != "LC") {
        # Check if 2 of 3 conditions are met (using FILTERED flags)
        if (sum(has_a, has_b, has_c) >= 2) {
          sub_str <- ""
          if (has_a) sub_str <- paste0(sub_str, "a")
          if (has_b) sub_str <- paste0(sub_str, "b(", paste(sort(b_inds), collapse=","), ")")
          if (has_c) sub_str <- paste0(sub_str, "c(", paste(sort(c_inds), collapse=","), ")")
          return(list(cat=curr_cat, code=paste0(type, sub_str)))
        }
      }
      return(list(cat="LC", code=""))
    }

    b1_new <- recalc_b(data$eoo$area_km2, "B1")
    b2_new <- recalc_b(data$aoo$area_km2, "B2")

    # 3. Determine Highest Category (Automated)
    cats <- c("LC", "NT", "VU", "EN", "CR"); get_rank <- function(x) match(x, cats)
    final_cat <- "LC"

    # Compare B1
    if (get_rank(b1_new$cat) > get_rank(final_cat)) { final_cat <- b1_new$cat; final_crit <- c(b1_new$code) }

    # Compare B2
    if (get_rank(b2_new$cat) > get_rank(final_cat)) { final_cat <- b2_new$cat; final_crit <- c(b2_new$code) }
    else if (get_rank(b2_new$cat) == get_rank(final_cat) && b2_new$cat != "LC") { final_crit <- c(final_crit, b2_new$code) }

    # D2 Check (Spatial only)
    if (dets$d2_flag) {
      if (get_rank(final_cat) < get_rank("VU")) { final_cat <- "VU"; final_crit <- c("D2") }
      else if (final_cat == "VU") final_crit <- c(final_crit, "D2")
    }

    # If using Pop, check D1
    if (input$use_pop && !is.na(data$pop$total_mature)) {
      mat <- data$pop$total_mature
      cat_d1 <- dplyr::case_when(mat < 50 ~ "CR", mat < 250 ~ "EN", mat < 1000 ~ "VU", TRUE ~ "LC")
      if (get_rank(cat_d1) > get_rank(final_cat)) { final_cat <- cat_d1; final_crit <- c("D1") }
      else if (get_rank(cat_d1) == get_rank(final_cat) && cat_d1 != "LC") final_crit <- c(final_crit, "D1")
    }

    str_out <- paste(unique(final_crit), collapse="; ")
    if(str_out == "") str_out <- "None"

    # Update the display object
    res$Category <- final_cat
    res$Criteria <- str_out

    return(list(res=res, dets=dets, data=data))
  })

  # --- 7. EXPERT FINAL VERDICT ---
  expert_final <- reactive({
    req(raw_data())
    data <- raw_data()

    eoo_v <- suppressWarnings(as.numeric(get_val(data$eoo, "area_km2")))
    aoo_v <- suppressWarnings(as.numeric(get_val(data$aoo, "area_km2")))

    # A. Reduction
    tr_val <- if(!is.na(rv$manual_trend)) rv$manual_trend else abs(suppressWarnings(as.numeric(get_val(data$trend, "percent_change"))))
    cat_A <- "LC"; code_A <- ""
    if (!is.na(tr_val) && length(rv$a_type) > 0) {
      cat_A <- if("A1" %in% rv$a_type) dplyr::case_when(tr_val>=90~"CR", tr_val>=70~"EN", tr_val>=50~"VU", TRUE~"LC")
      else dplyr::case_when(tr_val>=80~"CR", tr_val>=50~"EN", tr_val>=30~"VU", TRUE~"LC")
      if (cat_A != "LC") code_A <- paste0(rv$a_type[1], paste(sort(unique(rv$a_basis)), collapse=""))
    }

    # B. Geographic Range
    # Use filtered indices
    b_valid <- get_active_indices(rv$b_subs, "decline")
    c_valid <- get_active_indices(rv$c_subs, "fluct")

    has_b <- (length(b_valid) > 0)
    has_c <- (length(c_valid) > 0)

    eval_b_expert <- function(area, type) {
      t_cr <- if(type=="B1") 100 else 10; t_en <- if(type=="B1") 5000 else 500; t_vu <- if(type=="B1") 20000 else 2000

      check_level <- function() {
        met_a <- rv$loc
        if (sum(met_a, has_b, has_c) >= 2) {
          sub_str <- ""
          if (met_a) sub_str <- paste0(sub_str, "a")
          if (has_b) sub_str <- paste0(sub_str, "b(", paste(sort(b_valid), collapse=","), ")")
          if (has_c) sub_str <- paste0(sub_str, "c(", paste(sort(c_valid), collapse=","), ")")
          return(paste0(type, sub_str))
        }
        return(NULL)
      }

      if (area < t_cr) { r <- check_level(); if(!is.null(r)) return(list(cat="CR", code=r)) }
      if (area < t_en) { r <- check_level(); if(!is.null(r)) return(list(cat="EN", code=r)) }
      if (area < t_vu) { r <- check_level(); if(!is.null(r)) return(list(cat="VU", code=r)) }
      return(list(cat="LC", code=""))
    }

    res_b1 <- eval_b_expert(eoo_v, "B1")
    res_b2 <- eval_b_expert(aoo_v, "B2")

    # C, D, E Logic
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
    add_code <- function(c_code, c_cat) { if (c_code != "" && get_r(c_cat) >= max_rank) return(c_code) else return(NULL) }

    final_codes <- c(final_codes, add_code(code_A, cat_A), add_code(res_b1$code, res_b1$cat), add_code(res_b2$code, res_b2$cat),
                     add_code(code_C, cat_C), add_code(code_D1, cat_D1), add_code(code_E, cat_E))
    if (code_D2 != "" && final_cat == "VU") final_codes <- c(final_codes, code_D2)

    crit_out <- paste(unique(final_codes), collapse = "; ")
    if (crit_out == "") crit_out <- "None"

    return(list(category = final_cat, criteria = crit_out))
  })

  # --- 8. UI OUTPUTS ---

  output$map_plot <- renderPlot({
    req(raw_data())
    d <- raw_data()
    tryCatch({
      ndopred::plot_iucn(d$species, occ_data=d$occ_all, window=d$window_used)
    }, error = function(e) {
      plot(1,1, type="n", axes=FALSE, xlab="", ylab="")
      text(1,1, paste("Map Error:", e$message), col="red")
    })
  })

  output$verification_panel <- renderUI({
    req(assessment_display())
    res <- assessment_display()$res

    # VISUAL STYLING FOR DISABLED ELEMENTS
    style_disabled <- "color: #999; text-decoration: line-through; cursor: not-allowed;"

    lbl_b_v <- if(input$use_pop) tooltip_span("v", "Mature Individuals") else tags$span("v (Pop Only)", style=style_disabled)
    lbl_c_iv <- if(input$use_pop) tooltip_span("iv", "Mature Individuals") else tags$span("iv (Pop Only)", style=style_disabled)

    tagList(
      div(style="margin-bottom:15px; background:#f9f9f9; padding:10px; border-left: 5px solid #337ab7;",
          tags$small(strong("Automated Baseline:")),
          h4(paste0(res$Category, ": ", res$Criteria), style="margin-top:0; color:#337ab7;")
      ),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"),
                            checkboxGroupInput("check_a_type", "Type:", inline=T, choices=c("A1","A2","A3","A4"), selected=rv$a_type),
                            numericInput("manual_a_trend", "Manual %:", value=rv$manual_trend),
                            checkboxGroupInput("a_basis", "Basis:", inline=T, choices=c("a","b","c","d","e"), selected=rv$a_basis))),

        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1)))),
                            checkboxInput("check_loc", tooltip_span("a) Frag/Locs", "Severe Frag OR Locs <= threshold"), value=rv$loc),

                            tags$div(style="border-top: 1px solid #ccc; margin-top:5px; padding-top:5px;",
                                     tags$label("b) Continuing Decline:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_b_subs", label=NULL, inline=TRUE,
                                                        choiceNames = list(
                                                          tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"),
                                                          tooltip_span("iii", "Habitat"), tooltip_span("iv", "Locs"),
                                                          lbl_b_v
                                                        ),
                                                        choiceValues = c("i", "ii", "iii", "iv", "v"), selected=rv$b_subs)),

                            tags$div(style="border-top: 1px solid #ccc; margin-top:5px; padding-top:5px;",
                                     tags$label("c) Extreme Fluctuations:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label=NULL, inline=TRUE,
                                                        choiceNames = list(
                                                          tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"),
                                                          tooltip_span("iii", "Locs"),
                                                          lbl_c_iv
                                                        ),
                                                        choiceValues = c("i", "ii", "iii", "iv"), selected=rv$c_subs))
        )),

        column(4, wellPanel(h4("C. Small Pop"),
                            if(input$use_pop) tagList(
                              numericInput("pop_size_c", "Mature:", value=rv$pop_c),
                              checkboxInput("check_c1", "C1. Decline", value=rv$c1),
                              checkboxInput("check_c2", "C2. Sub/Fluct", value=rv$c2)
                            ) else div(style="color:#999; font-style:italic;", "Disabled (Enable in Settings)"))
        )
      ),
      fluidRow(
        column(4, wellPanel(h4("D. Very Small"),
                            if(input$use_pop) numericInput("pop_size_d1", "D1 Mature:", value=rv$pop_d1) else div(style="color:#999; font-style:italic;", "D1 Disabled"),
                            checkboxInput("check_d2", "D2. Restricted", value=rv$d2))),
        column(4, wellPanel(h4("E. Quantitative"), selectInput("check_e", "Extinction Prob:", choices=c("None","CR","EN","VU"), selected=rv$e_cat)))
      )
    )
  })

  output$live_category_ui <- renderUI({
    exp <- expert_final()
    color <- if(exp$category %in% c("CR","EN","VU")) "#f2dede" else if(exp$category=="NT") "#fcf8e3" else "#dff0d8"
    div(style=paste0("background-color:",color,"; padding:10px; border-radius:5px; text-align:center; border:1px solid #ccc;"),
        h2(exp$category, style="margin:0; font-weight:bold;"),
        p(tags$small(exp$criteria))
    )
  })

  output$status_ui <- renderUI({
    res <- assessment_display()$res
    div(style="background-color:#eee; padding:15px; border-radius:5px; border:1px solid;",
        h3(res$Category, style="margin-top:0;"),
        p(strong("Automated: "), res$Criteria)
    )
  })

  output$metrics_table <- renderTable({
    data <- assessment_display()
    res <- data$res
    pop <- data$data$pop
    exp <- expert_final()
    mat_val <- if(!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unknown"

    data.frame(Metric=c("Taxon","Expert Category","Expert Criteria","EOO","AOO","Locs","Mature","Fluctuation"),
               Value=c(res$Species, exp$category, exp$criteria,
                       base::format(res$EOO_km2, nsmall=1),
                       base::format(res$AOO_km2, nsmall=1),
                       as.character(res$Locations),
                       mat_val,
                       if(!is.na(pop$fluct_ratio)) paste0(pop$fluct_ratio,"x") else "-"))
  })
}

shinyApp(ui, server)

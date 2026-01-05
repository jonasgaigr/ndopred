library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(shinythemes)
# library(ndopred)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(tags$script(HTML("$(function () { $('body').tooltip({selector: '[data-toggle=\"tooltip\"]'}); });"))),
  titlePanel("NDOP Red List Assessor v0.1.1"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Onthophagus medius"),
      numericInput("window", "Recent Window (Years):", value = 10),
      numericInput("gen_length", "Generation Length (Years):", value = 1, min = 1),

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
        tabPanel("Spatial & Metrics", fluidRow(column(8, plotOutput("map_plot", height = "500px")), column(4, h4("Automated Results"), uiOutput("status_ui"), tableOutput("metrics_table")))),
        tabPanel("Expert Review", uiOutput("verification_panel")),
        tabPanel("Trend & History", verbatimTextOutput("trend_text"))
      )
    )
  )
)

server <- function(input, output, session) {
  get_cat_rank <- function(cat) match(cat, c("DD", "LC", "NT", "VU", "EN", "CR", "RE", "EX", "EW"))
  get_pop_groups <- function() c("Ryby a mihule", "Obojživelníci", "Plazi", "Ptáci", "Letouni", "Savci", "Cévnaté rostliny")
  tooltip_span <- function(label, text) tags$span(label, `data-toggle` = "tooltip", `data-placement` = "top", title = text, style = "border-bottom: 1px dotted #777; cursor: help; margin-right: 5px;")
  get_val <- function(obj, field = "val") { if (is.list(obj) && field %in% names(obj)) return(obj[[field]]); if (is.list(obj) && "area_km2" %in% names(obj)) return(obj[["area_km2"]]); if (is.list(obj) && "n_locations" %in% names(obj)) return(obj[["n_locations"]]); if (is.list(obj) && "percent_change" %in% names(obj)) return(obj[["percent_change"]]); return(obj) }

  rv <- reactiveValues(a_type=character(0), a_basis="b", manual_trend=NA, loc=FALSE, b_subs=character(0), c_subs=character(0), pop_c=NA, c1=FALSE, c2_ai=FALSE, c2_aii=FALSE, c2_b=FALSE, pop_d1=NA, d2=FALSE, e_cat="None", manual_cat="None")

  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {
      occ_raw <- tryCatch(ndopred::get_assessment_data(input$species_name), error = function(e) NULL)

      # Handle 0 records gracefully for DD
      if (is.null(occ_raw) || nrow(occ_raw) == 0) {
        return(list(
          eoo=list(area_km2=NA), aoo=list(area_km2=NA), locs=NA,
          trend=list(percent_change=NA),
          pop=list(decline_rate=NA, fluct_ratio=NA, total_mature=NA, max_subpop=NA),
          occ_all=data.frame(ROK=numeric(0)),
          taxon_group="Unknown", species=input$species_name, window_used=input$window
        ))
      }

      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all) && "DATUM_OD" %in% names(occ_all)) occ_all$ROK <- as.numeric(format(as.Date(occ_all$DATUM_OD), "%Y"))
      incProgress(0.3, message = "Calculating metrics...")

      # --- COMPLIANCE FIX: Section 4.4 (100-year Cap) ---
      # "10 years or 3 generations, whichever is longer, up to a maximum of 100 years"
      calc_gen_window <- 3 * input$gen_length
      assess_window <- min(100, max(10, calc_gen_window))

      cutoff <- as.numeric(format(Sys.Date(), "%Y")) - assess_window

      eoo_res <- ndopred::calculate_eoo(occ_all, year_start = cutoff)
      aoo_res <- ndopred::calculate_aoo(occ_all, year_start = cutoff)
      locs_res <- ndopred::calculate_locations(occ_all, year_start = cutoff)
      trend_res <- ndopred::calculate_trend(occ_all, window_years = assess_window)
      pop_res <- ndopred::calculate_pop_metrics(occ_all, window_years = assess_window)

      list(eoo=eoo_res, aoo=aoo_res, locs=locs_res, trend=trend_res, pop=pop_res, occ_all=occ_all, taxon_group=if("KAT_TAX"%in%names(occ_all)) unique(occ_all$KAT_TAX)[1] else "Unknown", species=input$species_name, window_used=assess_window)
    })
  })

  observeEvent(raw_data(), {
    req(raw_data()); data <- raw_data()
    is_pop <- data$taxon_group %in% get_pop_groups()
    updateCheckboxInput(session, "use_pop", value = is_pop)
    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))

    y_last <- if(!is.null(data$occ_all$ROK) && length(data$occ_all$ROK) > 0) max(data$occ_all$ROK, na.rm=T) else NA
    n_rec  <- nrow(data$occ_all)

    sum_obj <- ndopred::summarize_assessment(species=data$species, eoo=data$eoo, aoo=data$aoo, trend=data$trend, locations=locs_numeric, pop_metrics=data$pop, evaluate_pop=is_pop, year_last=y_last, n_records=n_rec)

    dets <- sum_obj$details
    rv$a_type <- dets$a_type

    # --- FIX CRASH: Robust Trend Extraction ---
    # Safely extract values, defaulting to NA if list is empty or field missing
    t_spatial <- suppressWarnings(as.numeric(get_val(data$trend, "percent_change")))
    if(length(t_spatial) == 0) t_spatial <- NA

    t_pop <- NA
    if(!is.null(data$pop) && "decline_rate" %in% names(data$pop)) {
      t_pop <- suppressWarnings(as.numeric(data$pop$decline_rate))
    }
    if(length(t_pop) == 0) t_pop <- NA

    # Pick the most severe decline (lowest negative number)
    t_init <- NA
    if (!is.na(t_spatial) && !is.na(t_pop)) t_init <- min(t_spatial, t_pop)
    else if (!is.na(t_spatial)) t_init <- t_spatial
    else if (!is.na(t_pop)) t_init <- t_pop

    rv$manual_trend <- if(!is.na(t_init)) t_init else NA

    rv$a_basis <- if(length(dets$a_basis)>0) dets$a_basis else "b"
    rv$loc <- dets$loc_flag

    b_init <- dets$b_indices; c_init <- dets$c_indices
    if (!is_pop) { b_init <- setdiff(b_init, "v"); c_init <- setdiff(c_init, "iv") }
    rv$b_subs <- b_init; rv$c_subs <- c_init
    rv$pop_c <- data$pop$total_mature; rv$c1 <- as.logical(dets$c1); rv$c2_ai <- as.logical(dets$c2_ai); rv$c2_aii <- as.logical(dets$c2_aii); rv$c2_b <- as.logical(dets$c2_b)
    rv$pop_d1 <- data$pop$total_mature; rv$d2 <- dets$d2_flag; rv$e_cat <- "None"; rv$manual_cat <- "None"
  })

  observeEvent(input$check_a_type, { rv$a_type <- input$check_a_type }, ignoreNULL=F)
  observeEvent(input$manual_a_trend, { rv$manual_trend <- input$manual_a_trend }, ignoreNULL=F)
  observeEvent(input$a_basis, { rv$a_basis <- input$a_basis }, ignoreNULL=F)
  observeEvent(input$check_loc, { rv$loc <- input$check_loc })
  observeEvent(input$check_b_subs, { rv$b_subs <- input$check_b_subs }, ignoreNULL=F)
  observeEvent(input$check_c_subs, { rv$c_subs <- input$check_c_subs }, ignoreNULL=F)
  observeEvent(input$pop_size_c, { rv$pop_c <- input$pop_size_c })
  observeEvent(input$check_c1, { rv$c1 <- input$check_c1 })
  observeEvent(input$check_c2_ai, { rv$c2_ai <- input$check_c2_ai })
  observeEvent(input$check_c2_aii, { rv$c2_aii <- input$check_c2_aii })
  observeEvent(input$check_c2_b, { rv$c2_b <- input$check_c2_b })
  observeEvent(input$pop_size_d1, { rv$pop_d1 <- input$pop_size_d1 })
  observeEvent(input$check_d2, { rv$d2 <- input$check_d2 })
  observeEvent(input$check_e, { rv$e_cat <- input$check_e })
  observeEvent(input$manual_cat_override, { rv$manual_cat <- input$manual_cat_override })

  assessment_display <- reactive({
    data <- raw_data(); if(is.null(data)) return(NULL)
    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))
    y_last <- if(!is.null(data$occ_all$ROK) && length(data$occ_all$ROK) > 0) max(data$occ_all$ROK, na.rm=T) else NA
    n_rec  <- nrow(data$occ_all)
    sum_obj <- ndopred::summarize_assessment(species=data$species, eoo=data$eoo, aoo=data$aoo, trend=data$trend, locations=locs_numeric, pop_metrics=data$pop, evaluate_pop=input$use_pop, year_last=y_last, n_records=n_rec)
    return(list(res=sum_obj$result, dets=sum_obj$details, data=data))
  })

  expert_final <- reactive({
    req(raw_data());
    if (!is.null(rv$manual_cat) && rv$manual_cat != "None") return(list(category = rv$manual_cat, criteria = "Expert Override"))

    data <- raw_data()

    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    y_last <- if(!is.null(data$occ_all$ROK) && length(data$occ_all$ROK) > 0) max(data$occ_all$ROK, na.rm=T) else NA
    n_rec  <- nrow(data$occ_all)

    # RE Pre-Check (Section 11.2)
    if (!is.na(y_last) && (current_year - y_last) > 50) return(list(category="RE", criteria=paste0("Last recorded: ", y_last)))

    eoo_v <- suppressWarnings(as.numeric(get_val(data$eoo, "area_km2")))
    aoo_v <- suppressWarnings(as.numeric(get_val(data$aoo, "area_km2")))

    # DD Pre-Check (Section 10.3)
    if (n_rec < 3 || is.na(aoo_v)) return(list(category="DD", criteria="Inadequate information"))

    tr_val <- if(!is.na(rv$manual_trend)) rv$manual_trend else 0
    is_extant <- (!is.na(aoo_v) && aoo_v > 0) || (!is.na(tr_val) && tr_val != 0)

    cat_A <- "LC"; code_A <- ""
    # Criterion A Harmonized: Use manual trend if negative (Decline)
    if (is_extant && !is.na(tr_val) && tr_val < 0 && length(rv$a_type) > 0) {
      red_val <- abs(tr_val)
      cat_A <- if("A1"%in%rv$a_type) dplyr::case_when(red_val>=90~"CR", red_val>=70~"EN", red_val>=50~"VU", red_val>=20~"NT", TRUE~"LC") else dplyr::case_when(red_val>=80~"CR", red_val>=50~"EN", red_val>=30~"VU", red_val>=20~"NT", TRUE~"LC")
      if (cat_A != "LC" && cat_A != "NT") code_A <- paste0(rv$a_type[1], paste(sort(unique(rv$a_basis)), collapse=""))
    }

    b_valid <- if(!input$use_pop) setdiff(rv$b_subs, "v") else rv$b_subs
    c_valid <- if(!input$use_pop) setdiff(rv$c_subs, "iv") else rv$c_subs
    has_b <- (length(b_valid) > 0); has_c <- (length(c_valid) > 0)

    locs_numeric <- suppressWarnings(as.numeric(get_val(data$locs, "n_locations")))

    eval_b <- function(area, type) {
      if (!is.na(area) && area > 0) {
        t_cr <- if(type=="B1") 100 else 10; t_en <- if(type=="B1") 5000 else 500; t_vu <- if(type=="B1") 20000 else 2000

        if (area < t_cr) curr="CR" else if(area < t_en) curr="EN" else if(area < t_vu) curr="VU" else curr="LC"

        if (curr!="LC") {
          thresh_loc <- if(curr=="CR") 1 else if(curr=="EN") 5 else 10
          met_a <- (rv$loc && !is.na(locs_numeric) && locs_numeric <= thresh_loc)

          cond_sum <- sum(met_a, has_b, has_c)

          if (cond_sum >= 2) {
            s <- paste0(if(met_a)"a"else"", if(has_b)paste0("b(",paste(sort(b_valid),collapse=","),")")else"", if(has_c)paste0("c(",paste(sort(c_valid),collapse=","),")")else"")
            return(list(cat=curr, code=paste0(type, s)))
          } else if (cond_sum == 1) {
            return(list(cat="NT", code=""))
          } else if (cond_sum == 0 && (curr == "CR" || curr == "EN")) {
            # Section 10.1: NT Restricted
            return(list(cat="NT", code=""))
          }
        }
      }
      return(list(cat="LC", code=""))
    }
    res_b1 <- eval_b(eoo_v, "B1"); res_b2 <- eval_b(aoo_v, "B2")

    cat_C <- "LC"; code_C <- ""
    if (input$use_pop && !is.na(rv$pop_c)) {
      eval_c <- function(t_pop, t_c1, t_sub) {
        if (rv$pop_c < t_pop) {
          if (rv$c1) return(list(met=TRUE, type="1"))
          if (rv$c2_ai || rv$c2_aii || rv$c2_b) {
            flags <- c(); if(rv$c2_ai) flags<-c(flags,"a(i)"); if(rv$c2_aii) flags<-c(flags,"a(ii)"); if(rv$c2_b) flags<-c(flags,"b")
            return(list(met=TRUE, type=paste0("2", paste(flags, collapse=""))))
          }
          return(list(met=FALSE, near=TRUE))
        }
        return(list(met=FALSE, near=FALSE))
      }
      r_cr <- eval_c(250, NA, NA); r_en <- eval_c(2500, NA, NA); r_vu <- eval_c(10000, NA, NA)
      if(r_cr$met) { cat_C<-"CR"; code_C<-paste0("C", r_cr$type) }
      else if(r_en$met) { cat_C<-"EN"; code_C<-paste0("C", r_en$type) }
      else if(r_vu$met) { cat_C<-"VU"; code_C<-paste0("C", r_vu$type) }
      else if(r_vu$near) { cat_C<-"NT" }
    }

    cat_D1 <- "LC"; code_D1 <- ""
    if (input$use_pop && !is.na(rv$pop_d1)) {
      cat_D1 <- dplyr::case_when(rv$pop_d1<50~"CR", rv$pop_d1<250~"EN", rv$pop_d1<1000~"VU", TRUE~"LC")
      if (cat_D1 != "LC") code_D1 <- "D1"
    }

    cat_D2 <- "LC"; code_D2 <- ""
    if (is_extant && rv$d2) { cat_D2 <- "VU"; code_D2 <- "D2" }

    cat_E <- rv$e_cat; code_E <- if(cat_E!="None") "E" else ""; if (cat_E == "None") cat_E <- "LC"

    rank_c <- c("LC","NT","VU","EN","CR"); get_r <- function(x) match(x, rank_c)
    ranks <- c(get_r(cat_A), get_r(res_b1$cat), get_r(res_b2$cat), get_r(cat_C), get_r(cat_D1), get_r(cat_E))
    max_rank <- max(ranks, na.rm=T)
    if (rv$d2 && max_rank <= get_r("VU") && is_extant) max_rank <- max(max_rank, get_r("VU"))

    final_cat <- rank_c[max_rank]; final_codes <- c()
    add_code <- function(c_code, c_cat) { if (c_code != "" && get_r(c_cat) >= max_rank) return(c_code) else return(NULL) }
    final_codes <- c(final_codes, add_code(code_A, cat_A), add_code(res_b1$code, res_b1$cat), add_code(res_b2$code, res_b2$cat),
                     add_code(code_C, cat_C), add_code(code_D1, cat_D1), add_code(code_E, cat_E))
    if (code_D2 != "" && final_cat == "VU") final_codes <- c(final_codes, code_D2)

    if (!is_extant && final_cat == "LC" && is.na(rv$manual_trend)) {
      final_cat <- "DD"
    }

    return(list(category = final_cat, criteria = paste(unique(final_codes), collapse = "; ")))
  })

  output$map_plot <- renderPlot({ req(raw_data()); d<-raw_data(); tryCatch({ndopred::plot_iucn(d$species, occ_data=d$occ_all, window=d$window_used)}, error=function(e){plot(1,1,type="n");text(1,1,"Map Error")}) })

  output$verification_panel <- renderUI({
    req(assessment_display()); res <- assessment_display()$res; data_raw <- raw_data()

    t_spatial <- suppressWarnings(as.numeric(get_val(data_raw$trend, "percent_change")))
    # Safe pop extraction for UI text
    t_pop <- NA
    if(!is.null(data_raw$pop) && "decline_rate" %in% names(data_raw$pop)) t_pop <- suppressWarnings(as.numeric(data_raw$pop$decline_rate))

    txt_s <- if(!is.na(t_spatial)) paste0(round(t_spatial, 1), "% (Spatial)") else "NA (Spatial)"
    txt_p <- if(!is.na(t_pop)) paste0(round(t_pop, 1), "% (Pop)") else "NA (Pop)"

    style_disabled <- "color: #999; text-decoration: line-through; cursor: not-allowed;"
    lbl_b_v <- if(input$use_pop) tooltip_span("v", "Mature Individuals") else tags$span("v (Pop Only)", style=style_disabled)
    lbl_c_iv <- if(input$use_pop) tooltip_span("iv", "Mature Individuals") else tags$span("iv (Pop Only)", style=style_disabled)

    tagList(
      div(style="margin-bottom:15px; background:#f9f9f9; padding:10px; border-left: 5px solid #337ab7;",
          tags$small(strong("Automated Baseline:")), h4(paste0(res$Category, ": ", res$Criteria), style="margin-top:0; color:#337ab7;")),
      div(style="margin-bottom:15px; border:1px solid #d6d6d6; padding:10px; background: #fff;",
          h5(strong("Force Category Override"), style="margin-top:0;"),
          selectInput("manual_cat_override", label=NULL, choices=c("None", "DD", "RE", "EW", "EX"), selected=rv$manual_cat, width="100%"),
          p(tags$small("Select to strictly enforce DD/RE/EW/EX regardless of calculated metrics."), style="color:#777;")),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"),
                            p(tags$small(strong(txt_s), " | ", strong(txt_p), style="color:#337ab7;")),
                            checkboxGroupInput("check_a_type", "Type:", inline=T, choiceNames=list(tooltip_span("A1","Reversible"), tooltip_span("A2","Irreversible"), tooltip_span("A3","Projected"), tooltip_span("A4","Past+Future")), choiceValues=c("A1","A2","A3","A4"), selected=rv$a_type),
                            numericInput("manual_a_trend", "Manual Override %:", value=rv$manual_trend),
                            checkboxGroupInput("a_basis", "Basis:", inline=T, choiceNames=list(tooltip_span("a","Obs"),tooltip_span("b","Index"),tooltip_span("c","AOO/EOO"),tooltip_span("d","Exploit"),tooltip_span("e","Intro")), choiceValues=c("a","b","c","d","e"), selected=rv$a_basis))),
        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall=1), " | AOO: ", base::format(res$AOO_km2, nsmall=1)))),
                            checkboxInput("check_loc", tooltip_span("a) Frag/Locs", "Severe Frag OR Locs <= 1/5/10"), value=rv$loc),
                            tags$div(style="margin-top:5px;", tags$label("b) Decline:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_b_subs", label=NULL, inline=T, choiceNames=list(tooltip_span("i","EOO"), tooltip_span("ii","AOO"), tooltip_span("iii","Hab"), tooltip_span("iv","Locs"), lbl_b_v), choiceValues=c("i","ii","iii","iv","v"), selected=rv$b_subs)),
                            tags$div(style="margin-top:5px;", tags$label("c) Fluctuation:", style="font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label=NULL, inline=T, choiceNames=list(tooltip_span("i","EOO"), tooltip_span("ii","AOO"), tooltip_span("iii","Locs"), lbl_c_iv), choiceValues=c("i","ii","iii","iv"), selected=rv$c_subs))
        )),
        column(4, wellPanel(h4("C. Small Pop"),
                            if(input$use_pop) tagList(
                              numericInput("pop_size_c", "Mature:", value=rv$pop_c),
                              checkboxInput("check_c1", tooltip_span("C1. Rapid Decline", "Decline >= 10%(VU)/20%(EN)/25%(CR)"), value=rv$c1),
                              tags$label("C2. Decline +:", style="font-weight:bold; display:block;"),
                              div(style="margin-left:10px;",
                                  checkboxInput("check_c2_ai", tooltip_span("a(i) Small Subs", "Subpop < 50/250/1000"), value=rv$c2_ai),
                                  checkboxInput("check_c2_aii", tooltip_span("a(ii) % One Sub", "90-100% in one sub"), value=rv$c2_aii),
                                  checkboxInput("check_c2_b", tooltip_span("b. Fluctuations", "Extreme fluctuations"), value=rv$c2_b)
                              )
                            ) else div(style="color:#999; font-style:italic;", "Disabled (Enable in Settings)"))
        )
      ),
      fluidRow(
        column(4, wellPanel(h4("D. Very Small"), if(input$use_pop) numericInput("pop_size_d1", "D1 Mature:", value=rv$pop_d1) else div(style="color:#999;", "D1 Disabled"), checkboxInput("check_d2", tooltip_span("D2. Restricted", "AOO < 20 or Locs <= 5"), value=rv$d2))),
        column(4, wellPanel(h4("E. Quantitative"), selectInput("check_e", "Extinction Prob:", choices=c("None","CR","EN","VU"), selected=rv$e_cat)))
      )
    )
  })

  output$live_category_ui <- renderUI({ exp<-expert_final(); div(style=paste0("background-color:", if(exp$category%in%c("CR","EN","VU"))"#f2dede" else if(exp$category=="NT") "#fcf8e3" else "#dff0d8", "; padding:10px; border:1px solid #ccc; text-align:center;"), h2(exp$category, style="margin:0;"), p(tags$small(exp$criteria))) })
  output$status_ui <- renderUI({ res<-assessment_display()$res; div(style="background-color:#eee; padding:15px; border:1px solid;", h3(res$Category, style="margin-top:0;"), p(strong("Automated: "), res$Criteria)) })
  output$metrics_table <- renderTable({ data<-assessment_display(); res<-data$res; pop<-data$data$pop; exp<-expert_final(); data.frame(Metric=c("Taxon","Expert Cat","Expert Crit","EOO","AOO","Locs","Mature","Fluct"), Value=c(res$Species, exp$category, exp$criteria, base::format(res$EOO_km2, nsmall=1), base::format(res$AOO_km2, nsmall=1), as.character(res$Locations), if(!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unk", if(!is.na(pop$fluct_ratio)) paste0(pop$fluct_ratio,"x") else "-")) })
}
shinyApp(ui, server)

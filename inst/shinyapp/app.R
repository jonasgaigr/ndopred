library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(shinythemes)

# The app holds no thresholds or category logic of its own: the automated
# proposal and the expert's view both come from ndopred::assess_taxon(), the
# expert's view with the panel's entries passed as evidence. Untouched
# entries are UNKNOWN, so the two agree until the expert changes something.

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(tags$script(HTML("$(function () { $('body').tooltip({selector: '[data-toggle=\"tooltip\"]'}); });"))),
  titlePanel(paste0("NDOP Red List Assessor v", as.character(utils::packageVersion("ndopred")))),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Onthophagus medius"),
      numericInput("reference_year", "Reference year:",
                   value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, step = 1),
      numericInput("window", "Current-distribution window (years):", value = 17, min = 1),
      p(tags$small("Criterion B (EOO, AOO, locations) uses this window: IUCN Guidelines 4.9 requires the *current* distribution. AOPK default: 17 years (2010-2026 for 2026).")),
      numericInput("gen_length", "Generation length (years; empty = NDOP group table):", value = NA, min = 0.1),
      uiOutput("window_note"),

      actionButton("run_calc", "Run Assessment", class = "btn-primary", style = "width: 100%"),
      hr(),
      h4("Assessment Settings"),
      checkboxInput("use_pop", "Use NDOP counts as indications (A2b, B b(v)/c(iv), C, D)", value = FALSE),
      p(tags$small("Auto-selected for vertebrates and plants when enough counted records carry a mature life stage.")),
      hr(),
      h4("Expert view"),
      uiOutput("live_category_ui"),
      textAreaInput("justification", "Expert Notes:", rows = 3),
      actionButton("save_assessment", "Finalize & Save", class = "btn-success", style = "width: 100%")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spatial & Metrics", fluidRow(column(8, plotOutput("map_plot", height = "500px")), column(4, h4("Automated proposal"), uiOutput("status_ui"), tableOutput("metrics_table")))),
        tabPanel("Expert Review", uiOutput("verification_panel")),
        tabPanel("Evidence table", tableOutput("evidence_table")),
        tabPanel("Trend & History", verbatimTextOutput("trend_text"))
      )
    )
  )
)

server <- function(input, output, session) {
  get_pop_groups <- function() c("Ryby a mihule", "Obojživelníci", "Plazi", "Ptáci", "Letouni", "Savci", "Cévnaté rostliny")
  tooltip_span <- function(label, text) tags$span(label, `data-toggle` = "tooltip", `data-placement` = "top", title = text, style = "border-bottom: 1px dotted #777; cursor: help; margin-right: 5px;")
  num_or_na <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0) NA_real_ else v[1] }
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  tri <- function(x) switch(x %||% "unknown", yes = TRUE, no = FALSE, NA)
  fmt <- function(x, d = 1) if (is.null(x) || length(x) == 0 || is.na(x)) "-" else format(round(x, d), nsmall = 0)

  # The form default is only a starting value; the run records the year used.
  reference_year <- reactive({
    y <- num_or_na(input$reference_year)
    if (is.na(y)) as.numeric(format(Sys.Date(), "%Y")) else y
  })

  # IUCN Guidelines 4.4 / 5.2: criterion A is measured over 10 years or three
  # generations, whichever is longer. (The 100-year cap applies to projected
  # reductions only; removing it for past reductions is Phase 5 work.)
  reduction_window_for <- function(gl) min(100, max(10, 3 * gl))

  # The generation length typed in, else the value for the taxon's NDOP
  # group (generation_lengths(); AOPK placeholder of 1 year until refined).
  generation_length_used <- function(occ) {
    gl_in <- num_or_na(input$gen_length)
    if (!is.na(gl_in) && gl_in > 0) return(list(years = gl_in, source = "entered by the assessor"))
    group <- if (!is.null(occ) && "KAT_TAX" %in% names(occ) && nrow(occ) > 0) {
      names(sort(table(as.character(occ$KAT_TAX)), decreasing = TRUE))[1]
    } else NA_character_
    tryCatch(ndopred::generation_length_for(group),
             error = function(e) list(years = 1, source = paste("default 1 year:", conditionMessage(e))))
  }

  output$window_note <- renderUI({
    d <- tryCatch(raw_data(), error = function(e) NULL)
    txt <- if (is.null(d)) {
      "Criterion A reduction window: 10 years or 3 generations, whichever is longer (capped at 100)."
    } else paste0("Generation length ", d$config$generation_length, " years (",
                  d$config$generation_length_source, "); criterion A reduction window ",
                  d$a_window, " years.")
    tags$small(style = "color:#337ab7;", paste(txt, "The trend compares the current window",
                                               "with the same window moved back by that period."))
  })

  rv <- reactiveValues(d2_candidate = FALSE)

  # AOPK, D7: a range holding both non-threatened and threatened categories.
  uncertain_note <- function(u) {
    if (isTRUE(u)) " | UNCERTAIN: the range crosses the threatened threshold (Guidelines 10.1)" else ""
  }

  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {
      b_window <- max(1, input$window %||% 17)
      occ_raw <- tryCatch(ndopred::get_assessment_data(input$species_name), error = function(e) e)
      gl <- generation_length_used(if (inherits(occ_raw, "error")) NULL else occ_raw)
      a_window <- reduction_window_for(gl$years)
      # AOPK, D5: the comparison window is the current one moved back by the
      # criterion A period (10 years unless three generations are longer).
      config <- ndopred::ndopred_config(reference_year = reference_year(), window = b_window,
                                        trend_shift = a_window, generation_length = gl$years,
                                        generation_length_source = gl$source)
      incProgress(0.3, message = "Calculating metrics...")
      if (inherits(occ_raw, "error")) {
        metrics <- ndopred::taxon_metrics(NULL, config)
        metrics$issues$download <- conditionMessage(occ_raw)
        metrics$failed <- paste0("download failed: ", conditionMessage(occ_raw))
        occ_raw <- NULL
      } else {
        metrics <- ndopred::taxon_metrics(occ_raw, config)
      }
      occ_all <- if (is.null(occ_raw) || nrow(occ_raw) == 0) data.frame(ROK = numeric(0)) else occ_raw

      list(metrics = metrics, config = config, occ_all = occ_all,
           eoo = list(area_km2 = metrics$eoo_new), aoo = list(area_km2 = metrics$aoo_new),
           locs = metrics$clusters_new, trend = metrics$trend,
           pop = metrics$pop %||% list(),
           taxon_group = if ("KAT_TAX" %in% names(occ_all) && nrow(occ_all)) unique(occ_all$KAT_TAX)[1] else "Unknown",
           species = input$species_name,
           b_window = b_window, a_window = a_window, year_now = config$reference_year,
           n_records = metrics$n_records, year_last = metrics$year_last)
    })
  })

  # The automated proposal: no expert evidence.
  auto_assessment <- reactive({
    data <- raw_data()
    req(data)
    ndopred::assess_taxon(species = data$species, config = data$config,
                          evaluate_pop = isTRUE(input$use_pop), metrics = data$metrics)
  })

  observeEvent(auto_assessment(), {
    a <- auto_assessment()
    rv$d2_candidate <- isTRUE(a$assessment$scenarios$working$D2$candidate)
  })

  # Criteria C and D need a population estimate. NDOP gives a minimum
  # observed count, so the counts are switched on automatically only when
  # the count basis is substantial.
  count_basis_ok <- function(pop) {
    used <- pop$n_records_used %||% 0
    counted <- pop$n_records_counted %||% 0
    isTRUE(used >= 10) && (counted == 0 || used / counted >= 0.5)
  }

  observeEvent(raw_data(), {
    data <- raw_data(); req(data)
    updateCheckboxInput(
      session, "use_pop",
      value = (data$taxon_group %in% get_pop_groups()) && count_basis_ok(data$pop)
    )
  }, priority = 10)

  count_audit_ui <- function(pop) {
    counted <- pop$n_records_counted %||% 0
    used <- pop$n_records_used %||% 0
    if (counted == 0) {
      return(div(style = "font-size:90%; color:#777;", "No counted records in window."))
    }
    ign <- pop$ignored_terms %||% integer(0)
    pct <- round(100 * used / counted)
    div(
      style = paste0("font-size:90%; margin-bottom:8px; padding:6px; border-left:4px solid ",
                     if (pct >= 50) "#5bc0de" else "#f0ad4e", "; background:#fbfbfb;"),
      tags$b(sprintf("Counts used: %d of %d records (%d%%)", used, counted, pct)),
      if (length(ign) > 0) tags$div(
        style = "color:#8a6d3b; margin-top:3px;",
        "Skipped life stages: ",
        paste(sprintf("%s (%d)", names(ign), as.integer(ign)), collapse = ", ")
      )
    )
  }

  # The expert's entries as evidence. Nothing here is seeded from the
  # records: an untouched entry is UNKNOWN and the proposal keeps using the
  # indication behind it.
  expert_evidence <- reactive({
    ev <- list()
    red <- num_or_na(input$manual_a_trend)
    if (!is.na(red)) {
      ev$reduction <- max(0, -red)
      ev$reduction_basis <- paste(sort(input$a_basis %||% character(0)), collapse = "")
    }
    if (length(input$check_a_type)) ev$reduction_types <- input$check_a_type
    ev$severely_fragmented <- tri(input$sf)
    threat_txt <- trimws(input$threat_text %||% "")
    ev$threat <- if (isTRUE(input$no_threat)) FALSE else if (nzchar(threat_txt)) threat_txt else NA
    ev$threat_locations <- num_or_na(input$threat_locations)
    flags <- function(confirmed, none, keys) {
      out <- stats::setNames(rep(NA, length(keys)), keys)
      if (isTRUE(none)) out[] <- FALSE
      out[intersect(confirmed %||% character(0), keys)] <- TRUE
      out
    }
    ev$decline <- flags(input$check_b_subs, input$no_decline, c("i", "ii", "iii", "iv", "v"))
    ev$fluctuation <- flags(input$check_c_subs, input$no_fluct, c("i", "ii", "iii", "iv"))
    ev$mature <- num_or_na(input$pop_mature)
    ev$mature_decline <- num_or_na(input$pop_change)
    ev$mature_decline_years <- num_or_na(input$pop_change_years)
    ev$largest_subpop <- num_or_na(input$pop_largest)
    share <- num_or_na(input$pop_share)
    ev$prop_largest <- if (is.na(share)) NA_real_ else share / 100
    if (!is.null(input$check_e) && input$check_e != "None") ev$e_category <- input$check_e
    ev
  })

  expert_final <- reactive({
    data <- raw_data(); req(data)
    override <- input$manual_cat_override %||% "None"
    if (override != "None") {
      return(list(category = override, criteria = "Expert Override", criteria_iucn = "",
                  support = "expert", needs = character(0), range = override, uncertain = FALSE))
    }
    a <- ndopred::assess_taxon(species = data$species, config = data$config,
                               evaluate_pop = isTRUE(input$use_pop),
                               evidence = expert_evidence(), metrics = data$metrics)
    r <- a$result
    list(category = r$Category, criteria = r$Criteria, criteria_iucn = r$Criteria_IUCN,
         support = r$Support, needs = a$proposal$needs, range = r$Category_Range,
         uncertain = isTRUE(r$Uncertain), assessment = a)
  })

  output$map_plot <- renderPlot({
    d <- raw_data(); req(d)
    tryCatch(
      with(d$config$windows,
           ndopred::plot_iucn(d$species, occ_data = d$occ_all,
                              recent_start = recent_start, recent_end = recent_end,
                              comp_start = comp_start, comp_end = comp_end)),
      error = function(e) { plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = ""); text(1, 1, paste("Map error:", conditionMessage(e))) }
    )
  })

  output$trend_text <- renderPrint({
    d <- raw_data(); req(d)
    w <- d$config$windows
    cat("Species:", d$species, "\n")
    cat("Records:", d$n_records, " Last year:", d$year_last, " Reference year:", d$year_now, "\n\n")
    cat("Criterion B window (current distribution):", w$recent_start, "-", w$recent_end, "\n")
    cat("Trend:", w$comp_start, "-", w$comp_end, "vs", w$trend_start, "-", w$trend_end, "\n\n")
    if (length(d$metrics$issues)) {
      cat("--- Reasons for missing values ---\n")
      for (k in names(d$metrics$issues)) cat(k, ":", d$metrics$issues[[k]], "\n")
      cat("\n")
    }
    cat("--- Spatial trend ---\n")
    str(d$trend)
    cat("\n--- Population metrics (minimum observed counts) ---\n")
    str(d$pop)
  })

  output$evidence_table <- renderTable({
    e <- expert_final()
    if (is.null(e$assessment$assessment)) return(NULL)
    e$assessment$assessment$table
  })

  needs_ui <- function(needs) {
    if (!length(needs)) return(NULL)
    div(style = "font-size:90%; color:#8a6d3b; text-align:left;",
        tags$b("Needs evidence:"), tags$ul(lapply(needs, tags$li)))
  }

  output$verification_panel <- renderUI({
    a <- auto_assessment(); req(a)
    data <- raw_data()
    m <- data$metrics
    ind <- if (is.null(a$assessment)) list() else a$assessment$inputs$indication
    ind_b <- ind$decline %||% logical(0)
    shown <- function(x) paste(names(x)[!is.na(x) & x], collapse = ", ")

    tagList(
      div(style = "margin-bottom:15px; background:#f9f9f9; padding:10px; border-left: 5px solid #337ab7;",
          tags$small(strong("Automated proposal:")),
          h4(paste0(a$result$Category, ": ", a$result$Criteria), style = "margin-top:0; color:#337ab7;"),
          p(tags$small(paste0("Support: ", a$result$Support, " | range: ", a$result$Category_Range, uncertain_note(a$result$Uncertain))))),
      p(tags$small("Every entry below starts as unknown. Entries are evidence: they replace the indication from the records for that condition.",
                   style = "color:#777;")),
      div(style = "margin-bottom:15px; border:1px solid #d6d6d6; padding:10px; background: #fff;",
          h5(strong("Force Category Override"), style = "margin-top:0;"),
          selectInput("manual_cat_override", label = NULL, choices = c("None", "DD", "RE", "EW", "EX"), selected = input$manual_cat_override %||% "None", width = "100%"),
          p(tags$small("Enforces DD/RE/EW/EX regardless of the criteria."), style = "color:#777;")),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"),
                            p(tags$small(sprintf("From the records: %s %% AOO, %s %% EOO change; reduction indication %s %% (%s)",
                                                 fmt(m$aoo_change), fmt(m$eoo_change), fmt(ind$reduction), ind$reduction_source %||% "-"),
                                         style = "color:#337ab7;")),
                            numericInput("manual_a_trend", "Evidenced change % (negative = decline):", value = NA),
                            checkboxGroupInput("check_a_type", "Type:", inline = TRUE, choiceNames = list(tooltip_span("A1", "Reversible, understood and ceased (90/70/50%)"), tooltip_span("A2", "Past; may not have ceased (80/50/30%)"), tooltip_span("A3", "Projected (80/50/30%)"), tooltip_span("A4", "Past + future (80/50/30%)")), choiceValues = c("A1", "A2", "A3", "A4"), selected = input$check_a_type %||% "A2"),
                            checkboxGroupInput("a_basis", "Basis:", inline = TRUE, choiceNames = list(tooltip_span("a", "Direct observation"), tooltip_span("b", "Index of abundance"), tooltip_span("c", "AOO/EOO/habitat quality"), tooltip_span("d", "Exploitation"), tooltip_span("e", "Introduced taxa etc.")), choiceValues = c("a", "b", "c", "d", "e"), selected = input$a_basis))),
        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(sprintf("EOO %s km2 | AOO %s km2 | %s clusters (5 km; not locations)",
                                                 fmt(m$eoo_new), fmt(m$aoo_new), fmt(m$clusters_new, 0)))),
                            selectInput("sf", tooltip_span("a) Severely fragmented", "Guidelines 4.8"), choices = c(unknown = "unknown", yes = "yes", no = "no"), selected = input$sf %||% "unknown"),
                            textInput("threat_text", tooltip_span("Most serious plausible threat", "Defines locations (Guidelines 4.11) and D2 (Guidelines 8)"), value = input$threat_text %||% ""),
                            checkboxInput("no_threat", "No plausible threat (location subcriteria and D2 not met)", value = isTRUE(input$no_threat)),
                            numericInput("threat_locations", "Threat-defined locations:", value = num_or_na(input$threat_locations)),
                            tags$div(style = "margin-top:5px;", tags$label("b) Continuing decline, evidenced:", style = "font-weight:bold;"),
                                     p(tags$small(paste0("Indicated by the records: ", if (nzchar(shown(ind_b))) shown(ind_b) else "none"), style = "color:#337ab7;")),
                                     checkboxGroupInput("check_b_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Habitat"), tooltip_span("iv", "Locations/subpopulations"), tooltip_span("v", "Mature individuals")), choiceValues = c("i", "ii", "iii", "iv", "v"), selected = input$check_b_subs),
                                     checkboxInput("no_decline", "No continuing decline in any", value = isTRUE(input$no_decline))),
                            tags$div(style = "margin-top:5px;", tags$label("c) Extreme fluctuation, evidenced:", style = "font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Locations/subpopulations"), tooltip_span("iv", "Mature individuals")), choiceValues = c("i", "ii", "iii", "iv"), selected = input$check_c_subs),
                                     checkboxInput("no_fluct", "No extreme fluctuation in any", value = isTRUE(input$no_fluct)))
        )),
        column(4, wellPanel(h4("C, D. Population"),
                            count_audit_ui(data$pop),
                            p(tags$small(sprintf("From NDOP counts (minimum observed): %s mature; largest locality %s",
                                                 fmt(data$pop$total_mature, 0), fmt(data$pop$max_subpop, 0)), style = "color:#337ab7;")),
                            numericInput("pop_mature", "Mature individuals (estimate):", value = num_or_na(input$pop_mature)),
                            numericInput("pop_change", "Change in mature individuals (%):", value = num_or_na(input$pop_change)),
                            numericInput("pop_change_years", "... over years:", value = num_or_na(input$pop_change_years)),
                            numericInput("pop_largest", "Largest subpopulation (mature):", value = num_or_na(input$pop_largest)),
                            numericInput("pop_share", "Share in largest subpopulation (%):", value = num_or_na(input$pop_share))))
      ),
      fluidRow(
        column(4, wellPanel(h4("D2"),
                            if (isTRUE(rv$d2_candidate)) div(style = "color:#8a6d3b; font-size:90%;", "Range restriction present: describe the threat in panel B to apply D2 (Guidelines 8).") else div(style = "color:#999; font-size:90%;", "No range restriction detected."))),
        column(4, wellPanel(h4("E. Quantitative"), selectInput("check_e", "Extinction Prob:", choices = c("None", "CR", "EN", "VU"), selected = input$check_e %||% "None")))
      )
    )
  })

  output$live_category_ui <- renderUI({
    exp <- expert_final()
    div(style = paste0("background-color:", if (isTRUE(exp$category %in% c("CR", "EN", "VU"))) "#f2dede" else if (identical(exp$category, "NT")) "#fcf8e3" else "#dff0d8", "; padding:10px; border:1px solid #ccc; text-align:center;"),
        h2(exp$category, style = "margin:0;"),
        p(tags$small(exp$criteria)),
        if (nzchar(exp$criteria_iucn %||% "")) p(tags$small(paste("IUCN:", exp$criteria_iucn))),
        p(tags$small(paste0("Support: ", exp$support, " | range: ", exp$range, uncertain_note(exp$uncertain)))),
        needs_ui(exp$needs))
  })

  output$status_ui <- renderUI({
    a <- auto_assessment()
    res <- a$result
    div(style = "background-color:#eee; padding:15px; border:1px solid;",
        h3(res$Category, style = "margin-top:0;"),
        p(strong("Criteria: "), res$Criteria),
        if (nzchar(res$Criteria_IUCN %||% "")) p(strong("IUCN: "), res$Criteria_IUCN),
        p(strong("Support: "), res$Support, " | ", strong("range: "), res$Category_Range, uncertain_note(res$Uncertain)),
        needs_ui(a$proposal$needs))
  })

  output$metrics_table <- renderTable({
    a <- auto_assessment(); data <- raw_data(); pop <- data$pop; exp <- expert_final()
    m <- data$metrics
    data.frame(
      Metric = c("Taxon", "Expert Cat", "Expert Crit", "EOO", "AOO", "Clusters (5 km)",
                 "Mature (min. observed)", "Max site count", "Fluct ratio", "Extreme fluct."),
      Value = c(data$species, exp$category %||% "-", exp$criteria %||% "-",
                fmt(m$eoo_new), fmt(m$aoo_new), fmt(m$clusters_new, 0),
                fmt(pop$total_mature, 0), fmt(pop$max_subpop, 0),
                if (!is.null(pop$fluct_ratio) && !is.na(pop$fluct_ratio)) paste0(pop$fluct_ratio, "x") else "-",
                if (isTRUE(pop$extreme_fluctuation)) "yes" else "no")
    )
  })
}
shinyApp(ui, server)

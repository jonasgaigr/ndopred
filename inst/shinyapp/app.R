library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(shinythemes)

# The app holds no thresholds or category logic of its own. The automated
# proposal and the expert's view both come from ndopred::assess_taxon(); the
# expert's view adds the entries made here as an evidence table (author,
# date, justification, sources), together with any evidence file loaded.
# Untouched entries are UNKNOWN, so the two agree until the expert changes
# something. The final category is the assessor's, checked against the
# evidence and final only once a group expert approves it (AOPK, D9).

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
      textInput("gen_length_basis", "Basis for the generation length (if entered):"),
      uiOutput("window_note"),

      actionButton("run_calc", "Run Assessment", class = "btn-primary", style = "width: 100%"),
      hr(),
      h4("Assessment Settings"),
      checkboxInput("use_pop", "Use NDOP counts as indications (A2b, B b(v)/c(iv), C, D)", value = FALSE),
      p(tags$small("Auto-selected for vertebrates and plants when enough counted records carry a mature life stage.")),
      hr(),
      h4("Expert view"),
      uiOutput("live_category_ui"),
      hr(),
      h4("Evidence file"),
      textInput("assessor", "Assessor (name, recorded with every entry):"),
      fileInput("load_evidence", "Load evidence (CSV or XLSX), after running:", accept = c(".csv", ".xlsx")),
      downloadButton("save_evidence", "Download evidence (CSV)"),
      textInput("evidence_dir", "Evidence folder:",
                value = file.path(dirname(ndopred::default_snapshot_dir()), "ndop_evidence")),
      actionButton("store_evidence", "Save as a new version in the folder", style = "width: 100%"),
      uiOutput("evidence_status"),
      hr(),
      downloadButton("download_card", "Species card (HTML)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spatial & Metrics", fluidRow(
          column(8, checkboxInput("show_excluded", "Show excluded records (red crosses)", value = TRUE),
                 plotOutput("map_plot", height = "500px")),
          column(4, h4("Automated proposal"), uiOutput("status_ui"), tableOutput("metrics_table")))),
        tabPanel("Expert Review", uiOutput("verification_panel")),
        tabPanel("Records",
                 p(tags$small(paste(
                   "Exclude NDOP records the assessment should not use, or use records the automatic",
                   "flags excluded. Each change is an evidence entry with your name, the date and a",
                   "reason; the record keeps its row in the data. Negative findings and records without",
                   "coordinates cannot be used."))),
                 fluidRow(
                   column(4, textInput("rec_ids", "Record IDs (ID_NALEZ), separated by commas:")),
                   column(3, selectInput("rec_reason", "Reason for excluding:",
                                         c("misidentification", "introduction", "vagrant", "duplicate",
                                           "imprecise", "other"))),
                   column(5, textInput("rec_just", "Justification:"))),
                 actionButton("rec_exclude", "Exclude"),
                 actionButton("rec_include", "Use"),
                 actionButton("rec_clear", "Clear the record entries made here"),
                 hr(),
                 selectInput("rec_show", "Show:", c("excluded", "used", "all"), selected = "excluded"),
                 tableOutput("records_table")),
        tabPanel("Final category",
                 p(tags$small(paste(
                   "The proposal is ndopred's; the final category is the assessor's. A departure from",
                   "the proposal needs a justification. The category is checked against the evidence,",
                   "and it is final once a group expert (a conservationist or taxonomist trained in",
                   "Red List assessment) approves it on or after its date (AOPK decision D9)."))),
                 fluidRow(
                   column(6, wellPanel(
                     h4("Final category"),
                     selectInput("final_cat", "Category:",
                                 c("(none)" = "", "EX", "EW", "RE", "CR", "EN", "VU", "NT", "LC", "DD", "NA", "NE")),
                     textInput("final_criteria", "Criteria (IUCN notation, e.g. EN B1ab(iii)+2ab(iii)):"),
                     textAreaInput("final_just", "Justification:", rows = 3))),
                   column(6, wellPanel(
                     h4("Approval"),
                     textInput("appr_name", "Approver:"),
                     selectInput("appr_role", "Role:", c("conservationist", "taxonomist")),
                     textInput("appr_group", "Expert group:"),
                     dateInput("appr_date", "Date:", value = Sys.Date()),
                     actionButton("add_approval", "Record approval")))),
                 uiOutput("final_status")),
        tabPanel("Evidence table",
                 h4("Expert evidence in force"), tableOutput("evidence_entries"),
                 h4("Criteria, subcriteria and what decided them"), tableOutput("evidence_table")),
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
  today <- function() format(Sys.Date(), "%Y-%m-%d")
  # Stand-ins that keep the live preview running before the assessor has
  # named themselves or justified an entry; saving refuses them.
  NO_AUTHOR <- "(assessor not named)"
  NO_JUSTIFICATION <- "(no justification given)"
  NO_SOURCE <- "(no source given)"
  EV_COLS <- c("taxon", "field", "subfield", "value", "min", "best", "max", "qualifier", "basis",
               "record_id", "author", "date", "justification", "sources")

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

  rv <- reactiveValues(d2_candidate = FALSE, loaded = NULL, records = NULL, approvals = NULL,
                       status = NULL)

  # AOPK, D7: a range holding both non-threatened and threatened categories.
  uncertain_note <- function(u) {
    if (isTRUE(u)) " | UNCERTAIN: the range crosses the threatened threshold (Guidelines 10.1)" else ""
  }

  # A new run starts without evidence from the previous taxon.
  observeEvent(input$run_calc, {
    rv$loaded <- NULL
    rv$records <- NULL
    rv$approvals <- NULL
    rv$status <- NULL
  }, priority = 100)

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

      list(metrics = metrics, config = config, occ = occ_raw, occ_all = occ_all,
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
    ndopred::assess_taxon(occ = data$occ, species = data$species, config = data$config,
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

  # ------------------------------------------------------------ evidence

  # One evidence row in ndopred's schema (see ndopred::evidence_fields()).
  ev_row <- function(sp, field, value = "", subfield = "", min = NA, best = NA, max = NA,
                     basis = "", record_id = NA, author = NO_AUTHOR, date = today(),
                     just = "", src = "") {
    just <- trimws(just %||% "")
    data.frame(taxon = sp, field = field, subfield = subfield, value = as.character(value),
               min = min, best = best, max = max, qualifier = "", basis = basis,
               record_id = record_id, author = author, date = date,
               justification = if (nzchar(just)) just else NO_JUSTIFICATION,
               sources = trimws(src %||% ""), stringsAsFactors = FALSE)
  }

  # The panels' entries as evidence rows. Nothing is seeded from the
  # records: an untouched entry is UNKNOWN and produces no row.
  ui_rows <- reactive({
    data <- raw_data(); req(data)
    sp <- data$species
    who <- trimws(input$assessor %||% "")
    if (!nzchar(who)) who <- NO_AUTHOR
    rows <- list()
    add <- function(...) rows[[length(rows) + 1]] <<- ev_row(sp, ..., author = who)

    ja <- input$just_a; sa <- input$src_a
    red <- num_or_na(input$red_best)
    if (!is.na(red)) {
      types <- input$check_a_type %||% "A2"
      add("reduction", subfield = paste(types, collapse = "+"), min = num_or_na(input$red_min),
          best = red, max = num_or_na(input$red_max),
          basis = paste(input$a_basis %||% character(0), collapse = ","), just = ja, src = sa)
      if ("A1" %in% types) {
        for (k in c("reversible", "understood", "ceased")) {
          add("a1_causes", value = k %in% (input$a1_causes %||% character(0)), subfield = k, just = ja, src = sa)
        }
      }
    }

    jb <- input$just_b; sb <- input$src_b
    sfr <- tri(input$sf)
    if (!is.na(sfr)) add("severely_fragmented", value = sfr, just = jb, src = sb)
    threat_txt <- trimws(input$threat_text %||% "")
    if (isTRUE(input$no_threat)) {
      add("no_plausible_threat", value = TRUE, just = jb, src = sb)
    } else if (nzchar(threat_txt)) {
      add("threat", value = threat_txt, just = jb, src = sb)
    }
    loc <- num_or_na(input$threat_locations)
    if (!is.na(loc)) {
      add("threat_locations", min = num_or_na(input$loc_min), best = loc, max = num_or_na(input$loc_max),
          basis = input$loc_option %||% "all", just = jb, src = sb)
    }
    flags <- function(confirmed, none, keys) {
      out <- stats::setNames(rep(NA, length(keys)), keys)
      if (isTRUE(none)) out[] <- FALSE
      out[intersect(confirmed %||% character(0), keys)] <- TRUE
      out
    }
    dec <- flags(input$check_b_subs, input$no_decline, c("i", "ii", "iii", "iv", "v"))
    for (k in names(dec)[!is.na(dec)]) add("decline", value = dec[[k]], subfield = k, just = jb, src = sb)
    fl <- flags(input$check_c_subs, input$no_fluct, c("i", "ii", "iii", "iv"))
    for (k in names(fl)[!is.na(fl)]) add("fluctuation", value = fl[[k]], subfield = k, just = jb, src = sb)

    jc <- input$just_c; sc <- input$src_c
    mat <- num_or_na(input$pop_mature)
    if (!is.na(mat)) add("mature_individuals", min = num_or_na(input$mature_min), best = mat,
                         max = num_or_na(input$mature_max), just = jc, src = sc)
    if (!is.na(num_or_na(input$pop_change))) add("mature_decline", best = num_or_na(input$pop_change), just = jc, src = sc)
    if (!is.na(num_or_na(input$pop_change_years))) add("mature_decline_years", best = num_or_na(input$pop_change_years), just = jc, src = sc)
    if (!is.na(num_or_na(input$pop_largest))) add("largest_subpopulation", best = num_or_na(input$pop_largest), just = jc, src = sc)
    share <- num_or_na(input$pop_share)
    if (!is.na(share)) add("prop_largest_subpopulation", best = share / 100, just = jc, src = sc)

    if (!is.null(input$check_e) && input$check_e != "None") {
      src_e <- trimws(input$src_e %||% "")
      add("criterion_e", value = input$check_e, just = input$just_e,
          src = if (nzchar(src_e)) src_e else NO_SOURCE)
    }
    gl <- num_or_na(input$gen_length)
    if (!is.na(gl) && gl > 0) add("generation_length", best = gl, just = input$gen_length_basis)
    if (nzchar(input$final_cat %||% "")) {
      add("final_category", value = input$final_cat, basis = trimws(input$final_criteria %||% ""),
          just = input$final_just)
    }
    if (length(rows)) do.call(rbind, rows) else NULL
  })

  # Whether a row repeats the loaded entry in force for the same field, so
  # that saving does not re-attribute someone else's evidence.
  norm_value <- function(v) {
    v <- tolower(trimws(as.character(v)))
    v[v %in% c("true", "t", "yes", "y", "ano", "1")] <- "true"
    v[v %in% c("false", "f", "no", "n", "ne", "0")] <- "false"
    v
  }
  same_num <- function(a, b) (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && abs(a - b) < 1e-9)
  repeats_loaded <- function(row, old) {
    if (is.null(old) || !nrow(old)) return(FALSE)
    same_id <- (is.na(old$record_id) & is.na(row$record_id)) |
      (!is.na(old$record_id) & !is.na(row$record_id) & old$record_id == row$record_id)
    hit <- old[old$field == row$field & old$subfield == row$subfield & same_id, , drop = FALSE]
    if (!nrow(hit)) return(FALSE)
    h <- hit[order(as.Date(hit$date)), , drop = FALSE][nrow(hit), ]
    norm_value(h$value) == norm_value(row$value) && same_num(h$min, row$min) &&
      same_num(h$best, row$best) && same_num(h$max, row$max) && identical(h$basis, row$basis)
  }

  # Loaded evidence, plus the entries made here that change it.
  evidence_table <- reactive({
    new <- rbind(ui_rows(), rv$records, rv$approvals)
    old <- rv$loaded
    if (!is.null(new) && nrow(new) && !is.null(old)) {
      keep <- !vapply(seq_len(nrow(new)), function(i) repeats_loaded(new[i, ], old), logical(1))
      new <- new[keep, , drop = FALSE]
    }
    all <- rbind(if (is.null(old)) NULL else as.data.frame(old)[, EV_COLS], new)
    if (is.null(all) || !nrow(all)) return(NULL)
    tryCatch(ndopred::validate_evidence(all), error = function(e) list(error = conditionMessage(e)))
  })

  expert_final <- reactive({
    data <- raw_data(); req(data)
    ev <- evidence_table()
    a <- ndopred::assess_taxon(occ = data$occ, species = data$species, config = data$config,
                               evaluate_pop = isTRUE(input$use_pop),
                               evidence = if (is.data.frame(ev)) ev else list(),
                               metrics = data$metrics)
    r <- a$result
    list(category = r$Category, criteria = r$Criteria, criteria_iucn = r$Criteria_IUCN,
         support = r$Support, needs = a$proposal$needs, range = r$Category_Range,
         uncertain = isTRUE(r$Uncertain), assessment = a, final = a$final,
         evidence_error = if (is.list(ev) && !is.data.frame(ev)) ev$error else NULL)
  })

  # What saving needs that the live preview does without.
  save_problems <- function(ev) {
    if (!is.data.frame(ev)) return(if (is.null(ev)) "there is no evidence to save" else ev$error)
    probs <- character(0)
    if (any(ev$author == NO_AUTHOR)) probs <- c(probs, "enter the assessor's name")
    nj <- unique(ev$field[ev$justification == NO_JUSTIFICATION])
    if (length(nj)) probs <- c(probs, paste("give a justification for:", paste(nj, collapse = ", ")))
    if (any(ev$sources == NO_SOURCE)) probs <- c(probs, "criterion E needs its analysis as a source")
    probs
  }

  output$save_evidence <- downloadHandler(
    filename = function() {
      paste0(gsub("[^A-Za-z0-9]+", "_", raw_data()$species), "_evidence_",
             format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv")
    },
    content = function(file) {
      ev <- evidence_table()
      probs <- save_problems(ev)
      if (length(probs)) stop("Not saved: ", paste(probs, collapse = "; "), call. = FALSE)
      utils::write.csv(as.data.frame(ev)[, EV_COLS], file, row.names = FALSE, na = "",
                       fileEncoding = "UTF-8")
    }
  )

  observeEvent(input$store_evidence, {
    data <- raw_data(); req(data)
    ev <- evidence_table()
    probs <- save_problems(ev)
    if (length(probs)) {
      rv$status <- paste("Not saved:", paste(probs, collapse = "; "))
      return()
    }
    path <- tryCatch(ndopred::write_evidence(ev, input$evidence_dir, data$species),
                     error = function(e) e)
    if (inherits(path, "error")) {
      rv$status <- paste("Not saved:", conditionMessage(path))
      return()
    }
    # The stored version becomes the base; the entries made here are in it.
    rv$loaded <- ndopred::read_evidence(path)
    rv$records <- NULL
    rv$approvals <- NULL
    rv$status <- paste("Saved:", path)
  })

  observeEvent(input$load_evidence, {
    data <- tryCatch(raw_data(), error = function(e) NULL)
    if (is.null(data)) {
      rv$status <- "Run the assessment before loading evidence."
      return()
    }
    ev <- tryCatch(ndopred::read_evidence(input$load_evidence$datapath), error = function(e) e)
    if (inherits(ev, "error")) {
      rv$status <- paste("Not loaded:", conditionMessage(ev))
      return()
    }
    ev <- ev[ev$taxon == data$species, , drop = FALSE]
    if (!nrow(ev)) {
      rv$status <- paste("The file has no evidence for", data$species)
      return()
    }
    rv$loaded <- ev
    rv$records <- NULL
    rv$approvals <- NULL
    rv$status <- sprintf("Loaded %d entries for %s.", nrow(ev), data$species)
    show_loaded(ndopred::evidence_for_engine(ev, data$species)$entries)
  })

  # Shows the loaded entries in force in the panels, so the panels and the
  # file agree; unchanged entries are not written again.
  show_loaded <- function(e) {
    get1 <- function(f, s = NULL) {
      x <- e[e$field == f & (if (is.null(s)) TRUE else e$subfield == s), , drop = FALSE]
      if (nrow(x)) x[nrow(x), ] else NULL
    }
    flag <- function(x) !is.null(x) && norm_value(x$value) == "true"
    if (!is.null(r <- get1("threat"))) updateTextInput(session, "threat_text", value = r$value)
    if (flag(get1("no_plausible_threat"))) updateCheckboxInput(session, "no_threat", value = TRUE)
    if (!is.null(r <- get1("severely_fragmented"))) updateSelectInput(session, "sf", selected = if (flag(r)) "yes" else "no")
    if (!is.null(r <- get1("threat_locations"))) {
      updateNumericInput(session, "threat_locations", value = r$best)
      updateNumericInput(session, "loc_min", value = r$min)
      updateNumericInput(session, "loc_max", value = r$max)
      updateSelectInput(session, "loc_option", selected = tolower(r$basis))
    }
    dec <- e[e$field == "decline", , drop = FALSE]
    if (nrow(dec)) {
      updateCheckboxGroupInput(session, "check_b_subs", selected = dec$subfield[norm_value(dec$value) == "true"])
      if (nrow(dec) == 5 && all(norm_value(dec$value) == "false")) updateCheckboxInput(session, "no_decline", value = TRUE)
    }
    fl <- e[e$field == "fluctuation", , drop = FALSE]
    if (nrow(fl)) {
      updateCheckboxGroupInput(session, "check_c_subs", selected = fl$subfield[norm_value(fl$value) == "true"])
      if (nrow(fl) == 4 && all(norm_value(fl$value) == "false")) updateCheckboxInput(session, "no_fluct", value = TRUE)
    }
    if (!is.null(r <- get1("reduction"))) {
      updateNumericInput(session, "red_best", value = r$best)
      updateNumericInput(session, "red_min", value = r$min)
      updateNumericInput(session, "red_max", value = r$max)
      updateCheckboxGroupInput(session, "check_a_type", selected = strsplit(r$subfield, "[+,; ]+")[[1]])
      updateCheckboxGroupInput(session, "a_basis", selected = strsplit(gsub("[^a-e]", "", tolower(r$basis)), "")[[1]])
    }
    a1 <- e[e$field == "a1_causes", , drop = FALSE]
    if (nrow(a1)) updateCheckboxGroupInput(session, "a1_causes", selected = a1$subfield[norm_value(a1$value) == "true"])
    if (!is.null(r <- get1("mature_individuals"))) {
      updateNumericInput(session, "pop_mature", value = r$best)
      updateNumericInput(session, "mature_min", value = r$min)
      updateNumericInput(session, "mature_max", value = r$max)
    }
    if (!is.null(r <- get1("mature_decline"))) updateNumericInput(session, "pop_change", value = r$best)
    if (!is.null(r <- get1("mature_decline_years"))) updateNumericInput(session, "pop_change_years", value = r$best)
    if (!is.null(r <- get1("largest_subpopulation"))) updateNumericInput(session, "pop_largest", value = r$best)
    if (!is.null(r <- get1("prop_largest_subpopulation"))) updateNumericInput(session, "pop_share", value = 100 * r$best)
    if (!is.null(r <- get1("criterion_e"))) updateSelectInput(session, "check_e", selected = if (r$value == "none") "None" else r$value)
    if (!is.null(r <- get1("generation_length"))) updateNumericInput(session, "gen_length", value = r$best)
    if (!is.null(r <- get1("final_category"))) {
      updateSelectInput(session, "final_cat", selected = r$value)
      updateTextInput(session, "final_criteria", value = r$basis)
      updateTextAreaInput(session, "final_just", value = r$justification)
    }
    invisible(NULL)
  }

  output$evidence_status <- renderUI({
    e <- tryCatch(expert_final(), error = function(e) NULL)
    msgs <- c(rv$status, if (!is.null(e$evidence_error)) paste("Evidence not used:", e$evidence_error))
    if (!length(msgs)) return(NULL)
    div(style = "font-size:90%; margin-top:6px; color:#8a6d3b;", lapply(msgs, tags$p))
  })

  # ------------------------------------------------- records and approval

  add_record_rows <- function(field, reason) {
    data <- raw_data(); req(data)
    who <- trimws(input$assessor %||% "")
    just <- trimws(input$rec_just %||% "")
    ids <- suppressWarnings(as.numeric(strsplit(input$rec_ids %||% "", "[,; ]+")[[1]]))
    ids <- ids[!is.na(ids)]
    if (!nzchar(who) || !nzchar(just) || !length(ids)) {
      showNotification("Give the assessor's name, record IDs and a justification.", type = "warning")
      return()
    }
    rows <- do.call(rbind, lapply(ids, function(id) {
      ev_row(data$species, field, subfield = reason, record_id = id, author = who, just = just)
    }))
    rv$records <- rbind(rv$records, rows)
  }
  observeEvent(input$rec_exclude, add_record_rows("record_exclude", input$rec_reason))
  observeEvent(input$rec_include, add_record_rows("record_include", ""))
  observeEvent(input$rec_clear, rv$records <- NULL)

  observeEvent(input$add_approval, {
    data <- raw_data(); req(data)
    nm <- trimws(input$appr_name %||% "")
    grp <- trimws(input$appr_group %||% "")
    if (!nzchar(nm) || !nzchar(grp)) {
      showNotification("Name the approver and the expert group.", type = "warning")
      return()
    }
    row <- ev_row(data$species, "approval", value = grp, subfield = input$appr_role, author = nm,
                  date = format(as.Date(input$appr_date %||% Sys.Date())), just = "approval")
    rv$approvals <- rbind(rv$approvals, row)
  })

  output$records_table <- renderTable({
    rec <- expert_final()$assessment$records
    if (is.null(rec)) return(NULL)
    show <- input$rec_show %||% "excluded"
    rec <- switch(show, excluded = rec[!(rec$include %in% TRUE), , drop = FALSE],
                  used = rec[rec$include %in% TRUE, , drop = FALSE], rec)
    if (nrow(rec) > 500) rec <- rec[seq_len(500), , drop = FALSE]
    rec
  })

  output$final_status <- renderUI({
    e <- expert_final()
    fin <- e$final
    a <- auto_assessment()
    items <- function(v) if (length(v)) tags$ul(lapply(v, tags$li)) else tags$p(tags$em("None."))
    tagList(
      h4("Check"),
      p(strong("Proposal: "), a$result$Category, " ", a$result$Criteria_IUCN),
      if (is.null(fin)) p(tags$em("No final category entered.")) else tagList(
        p(strong("Evidence and data alone support: "), fin$evidence_only$category, " ",
          fin$evidence_only$criteria),
        p(strong("Final: "), fin$claimed, " ", fin$criteria, " - ", strong(fin$status)),
        h5("Inconsistencies with the evidence"), items(fin$issues),
        h5("Notes"), items(fin$notes),
        h5("Approvals"),
        if (nrow(fin$approvals)) tableOutput("approvals_table") else p(tags$em("None yet."))
      )
    )
  })
  output$approvals_table <- renderTable({
    ap <- expert_final()$final$approvals
    if (is.null(ap) || !nrow(ap)) return(NULL)
    data.frame(approver = ap$author, role = ap$subfield, group = ap$value, date = ap$date)
  })

  output$download_card <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", raw_data()$species), "_card.html"),
    content = function(file) ndopred::species_card(expert_final()$assessment, file)
  )

  # --------------------------------------------------------------- outputs

  output$map_plot <- renderPlot({
    d <- raw_data(); req(d)
    recs <- expert_final()$assessment$evidence$records
    occ <- if (!is.null(d$occ) && !is.null(recs) && nrow(recs)) ndopred::apply_record_evidence(d$occ, recs) else d$occ_all
    tryCatch(
      with(d$config$windows,
           ndopred::plot_iucn(d$species, occ_data = occ,
                              recent_start = recent_start, recent_end = recent_end,
                              comp_start = comp_start, comp_end = comp_end,
                              show_excluded = isTRUE(input$show_excluded))),
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

  output$evidence_entries <- renderTable({
    ex <- expert_final()$assessment$evidence
    if (is.null(ex)) return(NULL)
    x <- ex$entries
    x$record_id <- ifelse(is.na(x$record_id), "", format(x$record_id, scientific = FALSE))
    x
  })

  needs_ui <- function(needs) {
    if (!length(needs)) return(NULL)
    div(style = "font-size:90%; color:#8a6d3b; text-align:left;",
        tags$b("Needs evidence:"), tags$ul(lapply(needs, tags$li)))
  }

  # The panels. Values typed in are read with isolate(): the panel is drawn
  # anew only for a new run, so typing never redraws it.
  output$verification_panel <- renderUI({
    a <- auto_assessment(); req(a)
    data <- raw_data()
    m <- data$metrics
    ind <- if (is.null(a$assessment)) list() else a$assessment$inputs$indication
    ind_b <- ind$decline %||% logical(0)
    shown <- function(x) paste(names(x)[!is.na(x) & x], collapse = ", ")
    i <- function(id) isolate(input[[id]])
    basis_ui <- function(suffix) tagList(
      textAreaInput(paste0("just_", suffix), "Basis / justification:", value = i(paste0("just_", suffix)) %||% "", rows = 2),
      textInput(paste0("src_", suffix), "Sources:", value = i(paste0("src_", suffix)) %||% "")
    )

    tagList(
      div(style = "margin-bottom:15px; background:#f9f9f9; padding:10px; border-left: 5px solid #337ab7;",
          tags$small(strong("Automated proposal:")),
          h4(paste0(a$result$Category, ": ", a$result$Criteria), style = "margin-top:0; color:#337ab7;"),
          p(tags$small(paste0("Support: ", a$result$Support, " | range: ", a$result$Category_Range, uncertain_note(a$result$Uncertain))))),
      p(tags$small(paste("Every entry below starts as unknown. Entries are evidence: they replace the indication",
                         "from the records for that condition, and are recorded with the assessor's name, the date",
                         "and the panel's justification. Numbers take a best estimate and, optionally, a minimum and",
                         "maximum (Guidelines 3.2). The final category is set in its own tab."),
                   style = "color:#777;")),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"),
                            p(tags$small(sprintf("From the records: %s %% AOO, %s %% EOO change; reduction indication %s %% (%s)",
                                                 fmt(m$aoo_change), fmt(m$eoo_change), fmt(ind$reduction), ind$reduction_source %||% "-"),
                                         style = "color:#337ab7;")),
                            fluidRow(column(4, numericInput("red_min", "Min %:", value = num_or_na(i("red_min")))),
                                     column(4, numericInput("red_best", "Reduction %:", value = num_or_na(i("red_best")))),
                                     column(4, numericInput("red_max", "Max %:", value = num_or_na(i("red_max"))))),
                            checkboxGroupInput("check_a_type", "Type:", inline = TRUE, choiceNames = list(tooltip_span("A1", "Reversible, understood and ceased (90/70/50%)"), tooltip_span("A2", "Past; may not have ceased (80/50/30%)"), tooltip_span("A3", "Projected (80/50/30%)"), tooltip_span("A4", "Past + future (80/50/30%)")), choiceValues = c("A1", "A2", "A3", "A4"), selected = i("check_a_type") %||% "A2"),
                            checkboxGroupInput("a1_causes", "A1 only - the causes are:", inline = TRUE, choices = c("reversible", "understood", "ceased"), selected = i("a1_causes")),
                            checkboxGroupInput("a_basis", "Basis:", inline = TRUE, choiceNames = list(tooltip_span("a", "Direct observation"), tooltip_span("b", "Index of abundance"), tooltip_span("c", "AOO/EOO/habitat quality"), tooltip_span("d", "Exploitation"), tooltip_span("e", "Introduced taxa etc.")), choiceValues = c("a", "b", "c", "d", "e"), selected = i("a_basis")),
                            basis_ui("a"))),
        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(sprintf("EOO %s km2 | AOO %s km2 | %s clusters (5 km; not locations)",
                                                 fmt(m$eoo_new), fmt(m$aoo_new), fmt(m$clusters_new, 0)))),
                            selectInput("sf", tooltip_span("a) Severely fragmented", "Guidelines 4.8"), choices = c(unknown = "unknown", yes = "yes", no = "no"), selected = i("sf") %||% "unknown"),
                            textInput("threat_text", tooltip_span("Most serious plausible threat", "Defines locations (Guidelines 4.11) and D2 (Guidelines 8)"), value = i("threat_text") %||% ""),
                            checkboxInput("no_threat", "No plausible threat (location subcriteria and D2 not met)", value = isTRUE(i("no_threat"))),
                            fluidRow(column(4, numericInput("loc_min", "Min:", value = num_or_na(i("loc_min")))),
                                     column(4, numericInput("threat_locations", "Locations:", value = num_or_na(i("threat_locations")))),
                                     column(4, numericInput("loc_max", "Max:", value = num_or_na(i("loc_max"))))),
                            selectInput("loc_option", tooltip_span("Basis for the locations", "Guidelines 4.11: options for parts of the range no threat affects"),
                                        choices = c("The threat covers the whole range" = "all",
                                                    "(a) Unaffected parts: locations not used" = "a",
                                                    "(b) Unaffected parts: one per subpopulation" = "b",
                                                    "(c) Unaffected parts: smallest affected location size" = "c",
                                                    "(d) Unaffected parts: most likely future threat" = "d"),
                                        selected = i("loc_option") %||% "all"),
                            tags$div(style = "margin-top:5px;", tags$label("b) Continuing decline, evidenced:", style = "font-weight:bold;"),
                                     p(tags$small(paste0("Indicated by the records: ", if (nzchar(shown(ind_b))) shown(ind_b) else "none"), style = "color:#337ab7;")),
                                     checkboxGroupInput("check_b_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Habitat"), tooltip_span("iv", "Locations/subpopulations"), tooltip_span("v", "Mature individuals")), choiceValues = c("i", "ii", "iii", "iv", "v"), selected = i("check_b_subs")),
                                     checkboxInput("no_decline", "No continuing decline in any", value = isTRUE(i("no_decline")))),
                            tags$div(style = "margin-top:5px;", tags$label("c) Extreme fluctuation, evidenced:", style = "font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Locations/subpopulations"), tooltip_span("iv", "Mature individuals")), choiceValues = c("i", "ii", "iii", "iv"), selected = i("check_c_subs")),
                                     checkboxInput("no_fluct", "No extreme fluctuation in any", value = isTRUE(i("no_fluct")))),
                            basis_ui("b"))),
        column(4, wellPanel(h4("C, D. Population"),
                            count_audit_ui(data$pop),
                            p(tags$small(sprintf("From NDOP counts (minimum observed): %s mature; largest locality %s",
                                                 fmt(data$pop$total_mature, 0), fmt(data$pop$max_subpop, 0)), style = "color:#337ab7;")),
                            fluidRow(column(4, numericInput("mature_min", "Min:", value = num_or_na(i("mature_min")))),
                                     column(4, numericInput("pop_mature", "Mature:", value = num_or_na(i("pop_mature")))),
                                     column(4, numericInput("mature_max", "Max:", value = num_or_na(i("mature_max"))))),
                            numericInput("pop_change", "Change in mature individuals (%):", value = num_or_na(i("pop_change"))),
                            numericInput("pop_change_years", "... over years:", value = num_or_na(i("pop_change_years"))),
                            numericInput("pop_largest", "Largest subpopulation (mature):", value = num_or_na(i("pop_largest"))),
                            numericInput("pop_share", "Share in largest subpopulation (%):", value = num_or_na(i("pop_share"))),
                            basis_ui("c")))
      ),
      fluidRow(
        column(4, wellPanel(h4("D2"),
                            if (isTRUE(rv$d2_candidate)) div(style = "color:#8a6d3b; font-size:90%;", "Range restriction present: describe the threat in panel B to apply D2 (Guidelines 8).") else div(style = "color:#999; font-size:90%;", "No range restriction detected."))),
        column(4, wellPanel(h4("E. Quantitative"),
                            selectInput("check_e", "Extinction Prob:", choices = c("None", "CR", "EN", "VU"), selected = i("check_e") %||% "None"),
                            basis_ui("e")))
      )
    )
  })

  output$live_category_ui <- renderUI({
    exp <- expert_final()
    fin <- exp$final
    div(style = paste0("background-color:", if (isTRUE(exp$category %in% c("CR", "EN", "VU"))) "#f2dede" else if (identical(exp$category, "NT")) "#fcf8e3" else "#dff0d8", "; padding:10px; border:1px solid #ccc; text-align:center;"),
        h2(exp$category, style = "margin:0;"),
        p(tags$small(exp$criteria)),
        if (nzchar(exp$criteria_iucn %||% "")) p(tags$small(paste("IUCN:", exp$criteria_iucn))),
        p(tags$small(paste0("Support: ", exp$support, " | range: ", exp$range, uncertain_note(exp$uncertain)))),
        if (!is.null(fin) && !is.na(fin$claimed)) p(tags$small(strong(paste0("Final: ", fin$claimed, " (", fin$status, ")")))),
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

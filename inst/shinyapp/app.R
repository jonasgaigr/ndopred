library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(shinythemes)
# library(ndopred)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(tags$script(HTML("$(function () { $('body').tooltip({selector: '[data-toggle=\"tooltip\"]'}); });"))),
  titlePanel(paste0("NDOP Red List Assessor v", as.character(utils::packageVersion("ndopred")))),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("species_name", "Species Name:", value = "Onthophagus medius"),
      numericInput("window", "Current-distribution window (years):", value = 10, min = 1),
      p(tags$small("Criterion B (EOO, AOO, locations) uses this window: IUCN Guidelines 4.9 requires the *current* distribution.")),
      numericInput("gen_length", "Generation Length (Years):", value = 1, min = 0.1),
      uiOutput("window_note"),

      actionButton("run_calc", "Run Assessment", class = "btn-primary", style = "width: 100%"),
      hr(),
      h4("Assessment Settings"),
      checkboxInput("use_pop", "Use abundance data (A2b, B b(v)/c(iv), C, D1)", value = FALSE),
      p(tags$small("Auto-selected for vertebrates and plants when enough counted records carry a mature life stage. Governs every use of NDOP counts.")),
      hr(),
      h4("Expert Final Verdict"),
      uiOutput("live_category_ui"),
      textAreaInput("justification", "Expert Notes:", rows = 3),
      actionButton("save_assessment", "Finalize & Save", class = "btn-success", style = "width: 100%")
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
  get_pop_groups <- function() c("Ryby a mihule", "Obojživelníci", "Plazi", "Ptáci", "Letouni", "Savci", "Cévnaté rostliny")
  tooltip_span <- function(label, text) tags$span(label, `data-toggle` = "tooltip", `data-placement` = "top", title = text, style = "border-bottom: 1px dotted #777; cursor: help; margin-right: 5px;")
  get_val <- function(obj, field = "val") { if (is.list(obj) && field %in% names(obj)) return(obj[[field]]); if (is.list(obj) && "area_km2" %in% names(obj)) return(obj[["area_km2"]]); if (is.list(obj) && "n_locations" %in% names(obj)) return(obj[["n_locations"]]); if (is.list(obj) && "percent_change" %in% names(obj)) return(obj[["percent_change"]]); return(obj) }
  num_or_na <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0) NA_real_ else v[1] }
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # IUCN Guidelines 4.4 / 5.2: criterion A is measured over 10 years or three
  # generations, whichever is longer, capped at 100 years.
  reduction_window <- reactive({
    gl <- if (is.null(input$gen_length) || is.na(input$gen_length)) 1 else input$gen_length
    min(100, max(10, 3 * gl))
  })

  output$window_note <- renderUI({
    tags$small(style = "color:#337ab7;", paste0(
      "Criterion A reduction window: ", reduction_window(),
      " years (max of 10 years and 3 generations, capped at 100)."
    ))
  })

  rv <- reactiveValues(a_type = character(0), a_basis = "b", manual_trend = NA, loc = FALSE,
                       frag = FALSE, b_subs = character(0), c_subs = character(0),
                       pop_c = NA, c1 = FALSE, c2_ai = FALSE, c2_aii = FALSE, c2_b = FALSE,
                       pop_d1 = NA, d2 = FALSE, d2_candidate = FALSE, e_cat = "None",
                       manual_cat = "None")

  raw_data <- eventReactive(input$run_calc, {
    req(input$species_name)
    withProgress(message = 'Accessing NDOP...', value = 0, {
      occ_raw <- tryCatch(ndopred::get_assessment_data(input$species_name), error = function(e) NULL)

      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      b_window <- max(1, input$window)
      a_window <- reduction_window()

      # Handle 0 records gracefully for DD
      if (is.null(occ_raw) || nrow(occ_raw) == 0) {
        return(list(
          eoo = list(area_km2 = NA), aoo = list(area_km2 = NA), locs = NA,
          trend = list(percent_change = NA, eoo_percent_change = NA),
          pop = list(decline_rate = NA, fluct_ratio = NA, extreme_fluctuation = FALSE,
                     total_mature = NA, max_subpop = NA, decline_period_years = a_window),
          occ_all = data.frame(ROK = numeric(0)),
          taxon_group = "Unknown", species = input$species_name,
          b_window = b_window, a_window = a_window, year_now = current_year,
          n_records = 0, year_last = NA
        ))
      }

      occ_all <- occ_raw
      if (!"ROK" %in% names(occ_all)) occ_all$ROK <- NA_real_
      occ_all$ROK <- suppressWarnings(as.numeric(as.character(occ_all$ROK)))
      if ("DATUM_OD" %in% names(occ_all)) {
        from_date <- ndopred::extract_year(occ_all$DATUM_OD)
        gaps <- is.na(occ_all$ROK)
        occ_all$ROK[gaps] <- from_date[gaps]
      }
      incProgress(0.3, message = "Calculating metrics...")

      # Criterion B describes the CURRENT distribution (Guidelines 4.9:
      # "they should not include areas where the species no longer exists"),
      # so EOO/AOO/locations use the short recent window. Criterion A is
      # measured over the longer reduction window. The previous version drove
      # both from the 3-generation window (up to 100 years), which inflated
      # AOO and EOO with long-abandoned sites, and left the "Recent Window"
      # input doing nothing at all.
      b_cutoff <- current_year - b_window + 1

      eoo_res <- ndopred::calculate_eoo(occ_all, year_start = b_cutoff)
      aoo_res <- suppressWarnings(ndopred::calculate_aoo(occ_all, year_start = b_cutoff))
      locs_res <- suppressWarnings(ndopred::calculate_locations(occ_all, year_start = b_cutoff))
      trend_res <- ndopred::calculate_trend(occ_all, window_years = a_window,
                                            year_now = current_year)
      pop_res <- ndopred::calculate_pop_metrics(occ_all, window_years = a_window,
                                                year_now = current_year)

      y_last <- suppressWarnings(max(occ_all$ROK, na.rm = TRUE))
      if (!is.finite(y_last)) y_last <- NA

      list(eoo = eoo_res, aoo = aoo_res, locs = locs_res, trend = trend_res, pop = pop_res,
           occ_all = occ_all,
           taxon_group = if ("KAT_TAX" %in% names(occ_all)) unique(occ_all$KAT_TAX)[1] else "Unknown",
           species = input$species_name,
           b_window = b_window, a_window = a_window, year_now = current_year,
           n_records = nrow(occ_all), year_last = y_last)
    })
  })

  # Single source of truth for the automated baseline, so the "Automated
  # Results" panel and the seed values for the expert panel cannot drift apart.
  auto_assessment <- reactive({
    data <- raw_data()
    req(data)
    ndopred::summarize_assessment(
      species = data$species, eoo = data$eoo, aoo = data$aoo, trend = data$trend,
      locations = num_or_na(get_val(data$locs, "n_locations")),
      pop_metrics = data$pop, evaluate_pop = isTRUE(input$use_pop),
      year_last = data$year_last, n_records = data$n_records,
      generation_length = input$gen_length
    )
  })

  # Criteria C and D1 need a population *estimate*. NDOP gives a minimum
  # observed count, and only from records whose life-stage term is in
  # get_mature_terms() - on live data often a small minority (e.g. 3 of 41
  # counted records for Pulsatilla vernalis, which turned into a spurious
  # "CR D1" from 4 individuals). So C/D1 are auto-enabled only when the count
  # basis is substantial; otherwise the assessor must switch them on
  # deliberately, having seen the audit in the Expert Review panel.
  # Shows how much of the countable data actually reached total_mature, and
  # which life-stage terms were skipped, so a 3-of-41 match is visible rather
  # than silently becoming a population size.
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
      ),
      if (pct < 50) tags$div(
        style = "color:#a94442; margin-top:3px;",
        "Most counted records were skipped, so the total is a poor guide to population size. ",
        "Confirm or replace it before applying C or D1."
      )
    )
  }

  count_basis_ok <- function(pop) {
    used <- pop$n_records_used %||% 0
    counted <- pop$n_records_counted %||% 0
    isTRUE(used >= 10) && (counted == 0 || used / counted >= 0.5)
  }

  # Auto-decide whether the abundance data may be used at all, once per run.
  observeEvent(raw_data(), {
    data <- raw_data(); req(data)
    updateCheckboxInput(
      session, "use_pop",
      value = (data$taxon_group %in% get_pop_groups()) && count_basis_ok(data$pop)
    )
  }, priority = 10)

  # Seed the expert panel. This depends on input$use_pop as well as on the
  # data, so toggling the abundance switch re-derives every seed that rests on
  # counts - b(v), c(iv), the criterion A trend and its basis letter - instead
  # of leaving stale values from whichever state the checkbox was in at load.
  observeEvent(list(raw_data(), input$use_pop), {
    data <- raw_data(); req(data)
    is_pop <- isTRUE(input$use_pop)

    sum_obj <- ndopred::summarize_assessment(
      species = data$species, eoo = data$eoo, aoo = data$aoo, trend = data$trend,
      locations = num_or_na(get_val(data$locs, "n_locations")),
      pop_metrics = data$pop, evaluate_pop = is_pop,
      year_last = data$year_last, n_records = data$n_records,
      generation_length = input$gen_length
    )

    dets <- sum_obj$details
    rv$a_type <- if (length(dets$a_type) > 0) dets$a_type else "A2"

    t_spatial <- suppressWarnings(min(c(num_or_na(get_val(data$trend, "percent_change")),
                                        num_or_na(data$trend$eoo_percent_change)), na.rm = TRUE))
    if (!is.finite(t_spatial)) t_spatial <- NA_real_
    # The abundance trend is only admissible when the counts are admissible.
    t_pop <- if (is_pop) num_or_na(data$pop$decline_rate) else NA_real_

    # Pick the most severe decline (lowest negative number)
    t_init <- suppressWarnings(min(c(t_spatial, t_pop), na.rm = TRUE))
    rv$manual_trend <- if (is.finite(t_init)) t_init else NA

    # The basis letter must name the evidence the seeded figure came from:
    # (b) an index of abundance, (c) a decline in AOO/EOO/habitat quality.
    # Defaulting to "c" mislabelled count-derived declines - e.g. Onthophagus
    # medius, whose only trend came from counts, was reported as "A2c".
    rv$a_basis <- if (length(dets$a_basis) > 0) {
      dets$a_basis
    } else if (!is.na(t_init) && !is.na(t_pop) && identical(t_init, t_pop)) {
      "b"
    } else {
      "c"
    }
    rv$loc <- isTRUE(dets$loc_flag)
    rv$frag <- FALSE

    rv$b_subs <- dets$b_indices; rv$c_subs <- dets$c_indices
    rv$pop_c <- data$pop$total_mature
    rv$c1 <- isTRUE(dets$c1); rv$c2_ai <- isTRUE(dets$c2_ai)
    rv$c2_aii <- isTRUE(dets$c2_aii); rv$c2_b <- isTRUE(dets$c2_b)
    rv$pop_d1 <- data$pop$total_mature
    rv$d2_candidate <- isTRUE(dets$d2_candidate)
    # D2 is never pre-ticked: Guidelines section 8 requires the assessor to
    # identify a plausible threat capable of driving the taxon to CR or EX in
    # a very short time. Pre-ticking it pre-committed the expert to the very
    # over-listing the guidelines warn about.
    rv$d2 <- FALSE
    rv$e_cat <- "None"; rv$manual_cat <- "None"
  })

  # ignoreNULL = FALSE lets the assessor clear a checkbox group back to empty,
  # but it also fires once at start-up when the Expert Review panel has not
  # been rendered yet and every input is still NULL. Without ignoreInit that
  # start-up firing wipes the values just seeded from the automated run - and
  # a NULL manual_trend then makes expert_final() fail on
  # `if (!is.na(rv$manual_trend))` with "argument is of length zero".
  observeEvent(input$check_a_type, { rv$a_type <- input$check_a_type },
               ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$manual_a_trend, {
    rv$manual_trend <- num_or_na(input$manual_a_trend)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$a_basis, { rv$a_basis <- input$a_basis },
               ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$check_loc, { rv$loc <- input$check_loc })
  observeEvent(input$check_frag, { rv$frag <- input$check_frag })
  observeEvent(input$check_b_subs, { rv$b_subs <- input$check_b_subs }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$check_c_subs, { rv$c_subs <- input$check_c_subs }, ignoreNULL = FALSE, ignoreInit = TRUE)
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
    obj <- auto_assessment()
    list(res = obj$result, dets = obj$details, data = raw_data())
  })

  expert_final <- reactive({
    data <- raw_data(); req(data)
    if (!is.null(rv$manual_cat) && rv$manual_cat != "None") {
      return(list(category = rv$manual_cat, criteria = "Expert Override"))
    }

    y_last <- data$year_last
    n_rec <- data$n_records

    # RE screen. See the provenance note in summarize_assessment(): no
    # years-since-last-record rule appears in the 2012 Categories and
    # Criteria or in Guidelines v16, so this is a prompt for survey
    # follow-up, not a determination.
    if (!is.na(y_last) && (data$year_now - y_last) > 50) {
      return(list(category = "RE",
                  criteria = paste0("Screen: no record since ", y_last, " - confirm with surveys")))
    }

    eoo_v <- num_or_na(get_val(data$eoo, "area_km2"))
    aoo_v <- num_or_na(get_val(data$aoo, "area_km2"))
    eoo_v <- ndopred::reconcile_eoo_aoo(eoo_v, aoo_v)

    if (n_rec < 3 || is.na(aoo_v)) return(list(category = "DD", criteria = "Inadequate information"))

    manual_trend <- num_or_na(rv$manual_trend)
    tr_val <- if (!is.na(manual_trend)) manual_trend else 0
    is_extant <- (!is.na(aoo_v) && aoo_v > 0)
    nt_notes <- character(0)

    cat_A <- "LC"; code_A <- ""
    if (is_extant && !is.na(tr_val) && tr_val < 0 && length(rv$a_type) > 0) {
      red_val <- abs(tr_val)
      cat_A <- if ("A1" %in% rv$a_type) {
        dplyr::case_when(red_val >= 90 ~ "CR", red_val >= 70 ~ "EN", red_val >= 50 ~ "VU", red_val >= 20 ~ "NT", TRUE ~ "LC")
      } else {
        dplyr::case_when(red_val >= 80 ~ "CR", red_val >= 50 ~ "EN", red_val >= 30 ~ "VU", red_val >= 20 ~ "NT", TRUE ~ "LC")
      }
      if (cat_A != "LC" && cat_A != "NT") {
        code_A <- paste0(rv$a_type[1], paste(sort(unique(rv$a_basis)), collapse = ""))
      } else if (cat_A == "NT") {
        nt_notes <- c(nt_notes, paste0("nearly meets ", rv$a_type[1], " (", round(red_val), "% reduction)"))
      }
    }

    # The abundance switch governs every use of the counts, so b(v) and
    # c(iv) go with C and D1. They come back the moment it is ticked, because
    # the seeding observer re-runs on that input.
    b_valid <- if (!isTRUE(input$use_pop)) setdiff(rv$b_subs, "v") else rv$b_subs
    c_valid <- if (!isTRUE(input$use_pop)) setdiff(rv$c_subs, "iv") else rv$c_subs
    has_b <- (length(b_valid) > 0); has_c <- (length(c_valid) > 0)

    locs_numeric <- num_or_na(get_val(data$locs, "n_locations"))

    eval_b <- function(area, type) {
      if (!is.na(area) && area > 0) {
        t_cr <- if (type == "B1") 100 else 10
        t_en <- if (type == "B1") 5000 else 500
        t_vu <- if (type == "B1") 20000 else 2000

        curr <- if (area < t_cr) "CR" else if (area < t_en) "EN" else if (area < t_vu) "VU" else "LC"

        if (curr != "LC") {
          thresh_loc <- if (curr == "CR") 1 else if (curr == "EN") 5 else 10
          # Condition (a) is "severely fragmented OR number of locations <= x".
          met_a <- isTRUE(rv$frag) ||
            (isTRUE(rv$loc) && !is.na(locs_numeric) && locs_numeric <= thresh_loc)

          cond_sum <- sum(met_a, has_b, has_c)

          if (cond_sum >= 2) {
            s <- paste0(if (met_a) "a" else "",
                        if (has_b) paste0("b(", paste(sort(b_valid), collapse = ","), ")") else "",
                        if (has_c) paste0("c(", paste(sort(c_valid), collapse = ","), ")") else "")
            return(list(cat = curr, code = paste0(type, s), nt = NULL))
          } else if (cond_sum == 1) {
            return(list(cat = "NT", code = "",
                        nt = paste0("nearly meets ", type, " (only 1 of 3 conditions)")))
          }
          # cond_sum == 0 is NOT Near Threatened - Guidelines 10.1 lists a
          # restricted, stable, unthreatened taxon among the cases that
          # should not be listed.
        }
      }
      return(list(cat = "LC", code = "", nt = NULL))
    }
    res_b1 <- eval_b(eoo_v, "B1"); res_b2 <- eval_b(aoo_v, "B2")
    nt_notes <- c(nt_notes, res_b1$nt, res_b2$nt)

    cat_C <- "LC"; code_C <- ""
    if (isTRUE(input$use_pop) && !is.na(rv$pop_c)) {
      # C2a(i) subpopulation caps and C2a(ii) proportions are
      # category-specific (v3.1): 50/250/1000 and 90%/95%/100%.
      eval_c <- function(t_pop) {
        if (rv$pop_c < t_pop) {
          if (isTRUE(rv$c1)) return(list(met = TRUE, type = "1"))
          if (isTRUE(rv$c2_ai) || isTRUE(rv$c2_aii) || isTRUE(rv$c2_b)) {
            flags <- c()
            if (isTRUE(rv$c2_ai)) flags <- c(flags, "a(i)")
            if (isTRUE(rv$c2_aii)) flags <- c(flags, "a(ii)")
            if (isTRUE(rv$c2_b)) flags <- c(flags, "b")
            return(list(met = TRUE, type = paste0("2", paste(flags, collapse = ""))))
          }
          return(list(met = FALSE, near = (rv$pop_c < 2000)))
        }
        return(list(met = FALSE, near = FALSE))
      }
      r_cr <- eval_c(250); r_en <- eval_c(2500); r_vu <- eval_c(10000)
      if (r_cr$met) { cat_C <- "CR"; code_C <- paste0("C", r_cr$type) }
      else if (r_en$met) { cat_C <- "EN"; code_C <- paste0("C", r_en$type) }
      else if (r_vu$met) { cat_C <- "VU"; code_C <- paste0("C", r_vu$type) }
      else if (isTRUE(r_vu$near)) {
        cat_C <- "NT"
        nt_notes <- c(nt_notes, "small population (<2,000 mature individuals)")
      }
    }

    cat_D1 <- "LC"; code_D1 <- ""
    if (isTRUE(input$use_pop) && !is.na(rv$pop_d1)) {
      cat_D1 <- dplyr::case_when(rv$pop_d1 < 50 ~ "CR", rv$pop_d1 < 250 ~ "EN", rv$pop_d1 < 1000 ~ "VU", TRUE ~ "LC")
      if (cat_D1 != "LC") code_D1 <- "D1"
    }

    cat_E <- rv$e_cat; code_E <- if (cat_E != "None") "E" else ""; if (cat_E == "None") cat_E <- "LC"

    rank_c <- c("LC", "NT", "VU", "EN", "CR"); get_r <- function(x) match(x, rank_c)
    ranks <- c(get_r(cat_A), get_r(res_b1$cat), get_r(res_b2$cat), get_r(cat_C), get_r(cat_D1), get_r(cat_E))
    max_rank <- max(ranks, na.rm = TRUE)

    # D2 only raises the category once the assessor has confirmed a plausible
    # threat (the check_d2 box), never from the AOO/location figures alone.
    d2_applies <- isTRUE(rv$d2) && is_extant
    if (d2_applies) max_rank <- max(max_rank, get_r("VU"))

    final_cat <- rank_c[max_rank]; final_codes <- c()
    add_code <- function(c_code, c_cat) { if (c_code != "" && get_r(c_cat) >= max_rank) return(c_code) else return(NULL) }
    final_codes <- c(final_codes, add_code(code_A, cat_A), add_code(res_b1$code, res_b1$cat), add_code(res_b2$code, res_b2$cat),
                     add_code(code_C, cat_C), add_code(code_D1, cat_D1), add_code(code_E, cat_E))
    if (d2_applies && final_cat == "VU") final_codes <- c(final_codes, "D2")

    if (!is_extant && final_cat == "LC") final_cat <- "DD"

    if (final_cat == "NT") final_codes <- unique(nt_notes)
    if (final_cat %in% c("LC", "NT") && isTRUE(rv$d2_candidate) && !d2_applies) {
      final_codes <- c(final_codes, "D2 candidate - confirm plausible threat")
    }

    return(list(category = final_cat, criteria = paste(unique(final_codes), collapse = "; ")))
  })

  output$map_plot <- renderPlot({
    d <- raw_data(); req(d)
    tryCatch(
      ndopred::plot_iucn(d$species, occ_data = d$occ_all, window = d$b_window,
                         year_now = d$year_now),
      error = function(e) { plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = ""); text(1, 1, paste("Map error:", conditionMessage(e))) }
    )
  })

  output$trend_text <- renderPrint({
    d <- raw_data(); req(d)
    cat("Species:", d$species, "\n")
    cat("Records:", d$n_records, " Last year:", d$year_last, "\n\n")
    cat("Criterion B window (current distribution):", d$year_now - d$b_window + 1, "-", d$year_now, "\n")
    cat("Criterion A reduction window:", d$a_window, "years\n\n")
    cat("--- Spatial trend ---\n")
    str(d$trend)
    cat("\n--- Population metrics (minimum observed counts) ---\n")
    str(d$pop)
  })

  output$verification_panel <- renderUI({
    disp <- assessment_display(); req(disp)
    res <- disp$res; data_raw <- disp$data

    t_spatial <- num_or_na(get_val(data_raw$trend, "percent_change"))
    t_eoo <- num_or_na(data_raw$trend$eoo_percent_change)
    t_pop <- num_or_na(data_raw$pop$decline_rate)

    txt_s <- if (!is.na(t_spatial)) paste0(round(t_spatial, 1), "% (AOO)") else "NA (AOO)"
    txt_e <- if (!is.na(t_eoo)) paste0(round(t_eoo, 1), "% (EOO)") else "NA (EOO)"
    txt_p <- if (!is.na(t_pop)) paste0(round(t_pop, 1), "% (Pop)") else "NA (Pop)"

    style_disabled <- "color: #999; text-decoration: line-through; cursor: not-allowed;"
    lbl_b_v <- if (isTRUE(input$use_pop)) tooltip_span("v", "Continuing decline in mature individuals") else tags$span("v (needs abundance data)", style = style_disabled)
    lbl_c_iv <- if (isTRUE(input$use_pop)) tooltip_span("iv", "Extreme fluctuation in mature individuals") else tags$span("iv (needs abundance data)", style = style_disabled)

    tagList(
      div(style = "margin-bottom:15px; background:#f9f9f9; padding:10px; border-left: 5px solid #337ab7;",
          tags$small(strong("Automated Baseline:")), h4(paste0(res$Category, ": ", res$Criteria), style = "margin-top:0; color:#337ab7;")),
      div(style = "margin-bottom:15px; border:1px solid #d6d6d6; padding:10px; background: #fff;",
          h5(strong("Force Category Override"), style = "margin-top:0;"),
          selectInput("manual_cat_override", label = NULL, choices = c("None", "DD", "RE", "EW", "EX"), selected = rv$manual_cat, width = "100%"),
          p(tags$small("Select to strictly enforce DD/RE/EW/EX regardless of calculated metrics."), style = "color:#777;")),
      fluidRow(
        column(4, wellPanel(h4("A. Reduction"),
                            p(tags$small(strong(txt_s), " | ", strong(txt_e), " | ", strong(txt_p), style = "color:#337ab7;")),
                            checkboxGroupInput("check_a_type", "Type:", inline = TRUE, choiceNames = list(tooltip_span("A1", "Reversible, understood and ceased (90/70/50%)"), tooltip_span("A2", "Not ceased/understood/reversible (80/50/30%)"), tooltip_span("A3", "Projected (80/50/30%)"), tooltip_span("A4", "Past+Future (80/50/30%)")), choiceValues = c("A1", "A2", "A3", "A4"), selected = rv$a_type),
                            numericInput("manual_a_trend", "Reduction % (negative = decline):", value = rv$manual_trend),
                            checkboxGroupInput("a_basis", "Basis:", inline = TRUE, choiceNames = list(tooltip_span("a", "Direct observation"), tooltip_span("b", "Index of abundance"), tooltip_span("c", "AOO/EOO/habitat quality"), tooltip_span("d", "Exploitation"), tooltip_span("e", "Introduced taxa etc.")), choiceValues = c("a", "b", "c", "d", "e"), selected = rv$a_basis))),
        column(4, wellPanel(h4("B. Geographic Range"),
                            p(tags$small(paste0("EOO: ", base::format(res$EOO_km2, nsmall = 1), " | AOO: ", base::format(res$AOO_km2, nsmall = 1), " | Locs: ", res$Locations))),
                            checkboxInput("check_frag", tooltip_span("a) Severely fragmented", "Guidelines 4.8 - expert judgement"), value = rv$frag),
                            checkboxInput("check_loc", tooltip_span("a) Locations <= 1/5/10", "Threat-based locations (Guidelines 4.11); the automated count is a distance-cluster surrogate"), value = rv$loc),
                            tags$div(style = "margin-top:5px;", tags$label("b) Continuing decline:", style = "font-weight:bold;"),
                                     checkboxGroupInput("check_b_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Habitat - not derivable from NDOP"), tooltip_span("iv", "Locations/subpopulations"), lbl_b_v), choiceValues = c("i", "ii", "iii", "iv", "v"), selected = rv$b_subs)),
                            tags$div(style = "margin-top:5px;", tags$label("c) Extreme fluctuation:", style = "font-weight:bold;"),
                                     checkboxGroupInput("check_c_subs", label = NULL, inline = TRUE, choiceNames = list(tooltip_span("i", "EOO"), tooltip_span("ii", "AOO"), tooltip_span("iii", "Locations/subpopulations"), lbl_c_iv), choiceValues = c("i", "ii", "iii", "iv"), selected = rv$c_subs))
        )),
        column(4, wellPanel(h4("C. Small Pop"),
                            count_audit_ui(data_raw$pop),
                            if (isTRUE(input$use_pop)) tagList(
                              numericInput("pop_size_c", "Mature individuals:", value = rv$pop_c),
                              p(tags$small("NDOP counts are minimum observed totals, not population estimates - override before use.", style = "color:#a94442;")),
                              checkboxInput("check_c1", tooltip_span("C1. Continuing decline", "25% in 3yr/1gen (CR), 20% in 5yr/2gen (EN), 10% in 10yr/3gen (VU)"), value = rv$c1),
                              tags$label("C2. Decline +:", style = "font-weight:bold; display:block;"),
                              div(style = "margin-left:10px;",
                                  checkboxInput("check_c2_ai", tooltip_span("a(i) Small subpops", "No subpopulation > 50 (CR) / 250 (EN) / 1,000 (VU)"), value = rv$c2_ai),
                                  checkboxInput("check_c2_aii", tooltip_span("a(ii) % in one subpop", "90-100% (CR), 95-100% (EN), 100% (VU)"), value = rv$c2_aii),
                                  checkboxInput("check_c2_b", tooltip_span("b. Fluctuations", "Extreme fluctuations in mature individuals"), value = rv$c2_b)
                              )
                            ) else div(style = "color:#999; font-style:italic;", "Disabled (Enable in Settings)"))
        )
      ),
      fluidRow(
        column(4, wellPanel(h4("D. Very Small"),
                            if (isTRUE(input$use_pop)) numericInput("pop_size_d1", "D1 Mature:", value = rv$pop_d1) else div(style = "color:#999;", "D1 Disabled"),
                            checkboxInput("check_d2", tooltip_span("D2. Restricted AND plausible threat", "Guidelines 8: AOO < 20 km2 or <= 5 locations is NOT sufficient - a plausible threat capable of causing CR/EX in a very short time is required"), value = rv$d2),
                            if (isTRUE(rv$d2_candidate)) div(style = "color:#8a6d3b; font-size:90%;", "Range restriction present - tick only if you can name a plausible threat.") else div(style = "color:#999; font-size:90%;", "No range restriction detected."))),
        column(4, wellPanel(h4("E. Quantitative"), selectInput("check_e", "Extinction Prob:", choices = c("None", "CR", "EN", "VU"), selected = rv$e_cat)))
      )
    )
  })

  output$live_category_ui <- renderUI({ exp <- expert_final(); div(style = paste0("background-color:", if (exp$category %in% c("CR", "EN", "VU")) "#f2dede" else if (exp$category == "NT") "#fcf8e3" else "#dff0d8", "; padding:10px; border:1px solid #ccc; text-align:center;"), h2(exp$category, style = "margin:0;"), p(tags$small(exp$criteria))) })
  output$status_ui <- renderUI({ res <- assessment_display()$res; div(style = "background-color:#eee; padding:15px; border:1px solid;", h3(res$Category, style = "margin-top:0;"), p(strong("Automated: "), res$Criteria)) })
  output$metrics_table <- renderTable({
    data <- assessment_display(); res <- data$res; pop <- data$data$pop; exp <- expert_final()
    data.frame(
      Metric = c("Taxon", "Expert Cat", "Expert Crit", "EOO", "AOO", "Locs",
                 "Mature (min. observed)", "Max site count", "Fluct ratio", "Extreme fluct."),
      Value = c(res$Species, exp$category, exp$criteria,
                base::format(res$EOO_km2, nsmall = 1), base::format(res$AOO_km2, nsmall = 1),
                as.character(res$Locations),
                if (!is.na(pop$total_mature)) as.character(pop$total_mature) else "Unk",
                if (!is.na(pop$max_subpop)) as.character(pop$max_subpop) else "Unk",
                if (!is.na(pop$fluct_ratio)) paste0(pop$fluct_ratio, "x") else "-",
                if (isTRUE(pop$extreme_fluctuation)) "yes" else "no")
    )
  })
}
shinyApp(ui, server)

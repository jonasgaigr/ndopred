#' Species Card: One Assessment on One Page
#'
#' A self-contained HTML page documenting an assessment (prompt section 8;
#' review section 8): the proposal and its plausible range, the final
#' category with its approvals and checks, the parameters with their
#' bounds, every criterion and subcriterion with the rule applied and
#' whether evidence or an indication decided it, the expert evidence with
#' its authors and sources, the NDOP record IDs used and excluded, what is
#' still open, and the versions and conventions of the run. Observer names
#' and coordinates from NDOP are not included. For a PDF, print the page
#' from a browser.
#'
#' @param x The result of [assess_taxon()].
#' @param file Optional path of the `.html` file to write.
#' @return The page as an `htmltools` tag list, invisibly; written to `file`
#'   when given.
#' @seealso [assess_taxon()], [final_assessment()]
#' @examples
#' inp <- assessment_inputs(eoo_km2 = 1700, aoo_km2 = 84, n_records = 40,
#'                          n_current = 12, year_last = 2025)
#' cfg <- ndopred_config(2026)
#' a <- assess_criteria(inp, cfg)
#' x <- list(result = data.frame(Species = "Example sp", EOO_km2 = 1700, AOO_km2 = 84),
#'           assessment = a, proposal = categorise(a), config = cfg)
#' card <- species_card(x, file = tempfile(fileext = ".html"))
#' @export
species_card <- function(x, file = NULL) {
  h <- htmltools::tags
  r <- x$result
  p <- x$proposal
  cfg <- x$config
  m <- x$metrics
  fin <- x$final
  val <- function(v, d = 2) {
    if (is.null(v) || !length(v) || is.na(v[1])) return("-")
    if (is.numeric(v)) format(round(v[1], d), big.mark = " ", scientific = FALSE) else as.character(v[1])
  }
  table_of <- function(df) {
    if (is.null(df) || !nrow(df)) return(h$p(h$em("None.")))
    h$table(
      h$thead(h$tr(lapply(names(df), h$th))),
      h$tbody(lapply(seq_len(nrow(df)), function(i) {
        h$tr(lapply(df[i, , drop = TRUE], function(v) h$td(if (is.na(v)) "" else as.character(v))))
      }))
    )
  }
  items <- function(v) if (length(v)) h$ul(lapply(v, h$li)) else h$p(h$em("None."))
  section <- function(title, ...) h$section(h$h2(title), ...)

  # The proposal.
  proposal <- section(
    "Proposal (ndopred)",
    h$p(h$span(class = "cat", p$category), " ", if (nzchar(p$criteria_iucn)) p$criteria_iucn),
    table_of(data.frame(
      item = c("Criteria (established format)", "Support", "Plausible range", "Uncertain (D7)", "Flags"),
      value = c(p$criteria, p$support, p$range, if (isTRUE(p$uncertain)) "yes" else "no",
                paste(p$flags, collapse = "; ")),
      stringsAsFactors = FALSE
    ))
  )

  # The final category.
  final <- if (is.null(fin)) {
    section("Final category", h$p(h$em("No final category recorded.")))
  } else {
    ap <- fin$approvals
    section(
      "Final category",
      h$p(h$span(class = "cat", val(fin$claimed)), " ", val(fin$criteria), " - ", h$strong(fin$status)),
      h$p("Evidence and measured data alone support: ",
          fin$evidence_only$category, " ", fin$evidence_only$criteria),
      h$h3("Approval (AOPK D9: group experts trained in Red List assessment)"),
      table_of(if (nrow(ap)) data.frame(approver = ap$author, role = ap$subfield, group = ap$value,
                                        date = ap$date, stringsAsFactors = FALSE) else NULL),
      h$h3("Checks against the evidence"), items(fin$issues),
      h$h3("Notes"), items(fin$notes)
    )
  }

  # Parameters with their bounds.
  w <- cfg$windows
  par <- data.frame(
    parameter = c("EOO (km2)", "AOO, 2 x 2 km (km2)", "5 km clusters (indication of locations)",
                  "AOO change between windows (%)", "EOO change between windows (%)",
                  "Current window", "Comparison window", "Generation length (years)",
                  "Records: total / current window / excluded"),
    best = c(val(r$EOO_km2), val(r$AOO_km2), val(r$Locations), val(m$aoo_change, 1),
             val(m$eoo_change, 1), paste(w$recent_start, w$recent_end, sep = "-"),
             paste(w$comp_start, w$comp_end, sep = "-"), val(cfg$generation_length),
             paste(val(m$n_records), val(m$n_current), val(m$n_excluded), sep = " / ")),
    range = c(paste(val(r$EOO_km2_min), val(r$EOO_km2_max), sep = " - "),
              paste(val(r$AOO_km2_min), val(r$AOO_km2_max), sep = " - "), "", "", "", "", "",
              val(cfg$generation_length_source), ""),
    stringsAsFactors = FALSE
  )
  params <- section("Parameters", table_of(par),
                    if (length(m$exclusions)) h$p("Excluded records by reason: ",
                                                  paste(names(m$exclusions), m$exclusions,
                                                        sep = ": ", collapse = "; ")))

  # Every criterion x subcriterion x level, and what decided it.
  crit <- if (is.null(x$assessment)) NULL else {
    t <- x$assessment$table
    t[, c("criterion", "subcriterion", "level", "value", "threshold", "evidence",
          "indication", "used", "support", "ref")]
  }
  criteria <- section("Criteria and the rules applied",
                      h$p("support: evidence = the assessor's evidence decided; indication = a proxy ",
                          "from the records decided; data = a measured value; none = unknown."),
                      table_of(crit))

  # Expert evidence in force.
  ev <- if (is.null(x$evidence)) NULL else {
    e <- x$evidence$entries
    num <- ifelse(is.na(e$best) & is.na(e$min), "",
                  sprintf("%s (%s-%s)", e$best, e$min, e$max))
    data.frame(field = e$field, subfield = e$subfield,
               value = trimws(paste(e$value, num)), qualifier = e$qualifier, basis = e$basis,
               record = ifelse(is.na(e$record_id), "", e$record_id), author = e$author,
               date = e$date, justification = e$justification, sources = e$sources,
               stringsAsFactors = FALSE)
  }
  evidence <- section("Expert evidence", table_of(ev))

  # Records: IDs used and excluded.
  rec <- x$records
  records <- section(
    "NDOP records",
    if (is.null(rec) || !"ID_NALEZ" %in% names(rec)) h$p(h$em("Record list not available.")) else {
      used <- rec$include %in% TRUE
      htmltools::tagList(
        h$p(sprintf("%d records, %d used, %d excluded.", nrow(rec), sum(used), sum(!used))),
        h$h3("Used (ID_NALEZ)"), h$p(class = "ids", paste(sort(rec$ID_NALEZ[used]), collapse = ", ")),
        h$h3("Excluded"),
        table_of(data.frame(ID_NALEZ = rec$ID_NALEZ[!used], reason = rec$exclusion_reason[!used],
                            stringsAsFactors = FALSE))
      )
    }
  )

  open <- section("Open questions", items(c(p$needs, if (!is.null(fin)) fin$issues)))

  conv <- cfg$conventions
  run <- section(
    "Run",
    table_of(data.frame(
      item = c("Reference year", "Snapshot", "Proposal rule", names(conv), "ndopred", "Criteria",
               "Guidelines", "Regional guidelines", "Card written"),
      value = c(cfg$reference_year, val(r$Snapshot), cfg$attitude$proposal, unlist(conv),
                cfg$versions$ndopred, cfg$versions$criteria, cfg$versions$guidelines,
                cfg$versions$regional, format(Sys.time(), "%Y-%m-%d %H:%M")),
      stringsAsFactors = FALSE
    ))
  )

  css <- paste(
    "body{font-family:system-ui,sans-serif;max-width:1100px;margin:2em auto;padding:0 1em;color:#222}",
    "h1{margin-bottom:0}h2{border-bottom:1px solid #ccc;padding-bottom:.2em;margin-top:1.6em}",
    "table{border-collapse:collapse;width:100%;font-size:90%}",
    "th,td{border:1px solid #ddd;padding:3px 6px;text-align:left;vertical-align:top}",
    "th{background:#f3f3f3}.cat{font-size:160%;font-weight:bold}",
    ".ids{font-family:monospace;font-size:85%;word-break:break-all}",
    "@media print{body{margin:0}h2{page-break-after:avoid}}"
  )
  page <- htmltools::tagList(
    h$head(h$meta(charset = "utf-8"), h$title(paste("Species card:", r$Species)), h$style(css)),
    h$h1(h$em(r$Species)),
    h$p(sprintf("Czech national Red List assessment, reference year %s.", cfg$reference_year)),
    proposal, final, params, criteria, evidence, records, open, run
  )
  if (!is.null(file)) htmltools::save_html(page, file)
  invisible(page)
}

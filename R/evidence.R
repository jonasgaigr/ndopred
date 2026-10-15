# The evidence layer: what an expert adds to the NDOP records, as attributed
# rows of one table (prompt section 6; gap analysis, Phase 3). Evidence never
# edits the records: record-level entries become flags, and every entry is
# shown beside the computed value it informs.

# One row per field an expert may give. `type`: logical, number (min / best
# / max), text, category (one of `values`) or record (keyed by ID_NALEZ).
.EVIDENCE_FIELDS <- data.frame(
  field = c("origin", "occurrence_status", "taxonomic_note",
            "record_exclude", "record_include",
            "generation_length", "current_window_start",
            "threat", "no_plausible_threat", "threat_locations",
            "severely_fragmented", "decline", "fluctuation",
            "reduction", "a1_causes",
            "mature_individuals", "mature_decline", "mature_decline_years",
            "largest_subpopulation", "prop_largest_subpopulation", "subpopulations",
            "d2_scenario", "criterion_e", "survey",
            "final_category", "approval"),
  type = c("category", "category", "text",
           "record", "record",
           "number", "number",
           "text", "logical", "number",
           "logical", "logical", "logical",
           "number", "logical",
           "number", "number", "number",
           "number", "number", "number",
           "text", "category", "text",
           "category", "text"),
  subfields = c("", "", "",
                "misidentification|introduction|vagrant|duplicate|imprecise|other", "",
                "", "",
                "", "", "",
                "", "i|ii|iii|iv|v", "i|ii|iii|iv",
                "A1|A2|A3|A4", "reversible|understood|ceased",
                "", "", "",
                "", "", "",
                "", "", "",
                "", "conservationist|taxonomist"),
  values = c("native|introduced|reintroduced|uncertain",
             "resident|breeding|visitor|vagrant|uncertain", "",
             "", "",
             "", "",
             "", "", "",
             "", "", "",
             "", "",
             "", "", "",
             "", "", "",
             "", "CR|EN|VU|none", "",
             "EX|EW|RE|CR|EN|VU|NT|LC|DD|NA|NE", ""),
  ref = c("Regional Guidelines v4.0 (Phase 4)", "Regional Guidelines v4.0 (Phase 4)", "",
          "", "",
          "Criteria v3.1 III.4; Guidelines 4.4", "Guidelines 4.9",
          "Guidelines 4.11, 8", "Guidelines 4.11", "Guidelines 4.11",
          "Guidelines 4.8", "Guidelines 4.6", "Guidelines 4.7",
          "Guidelines 5", "Criteria v3.1 A1; Guidelines 5",
          "Guidelines 4.3", "Guidelines 4.5, 4.6", "Guidelines 4.5",
          "Guidelines 4.2", "Guidelines 4.2", "Guidelines 4.2",
          "Guidelines 8.3", "Guidelines 9", "Guidelines 11.3; Regional Guidelines III.14",
          "Guidelines 2.3, 10", "AOPK decision D9"),
  description = c(
    "Native, introduced, reintroduced or uncertain (eligibility; used in Phase 4).",
    "Resident, breeding, visitor, vagrant or uncertain (eligibility; used in Phase 4).",
    "Taxonomic concept, synonyms, identification issues.",
    "Exclude one NDOP record (record_id = ID_NALEZ); subfield gives the reason.",
    "Use one NDOP record that the automatic flags excluded (record_id = ID_NALEZ).",
    "Generation length in years (min/best/max).",
    "First year of the current window for this taxon (best).",
    "The most serious plausible threat, described in words.",
    "TRUE when the assessor finds no plausible threat (location subcriteria then not met).",
    paste("Threat-defined locations (min/best/max). basis: 'all' when the most serious plausible threat",
          "covers the whole range; otherwise the Guidelines 4.11 option for the unaffected parts:",
          "a (locations not used, subcriteria not met), b, c or d."),
    "Severe fragmentation, TRUE or FALSE.",
    "Continuing decline, TRUE or FALSE, per B(b) subcriterion (i)-(v); (v) also serves C2.",
    "Extreme fluctuations, TRUE or FALSE, per B(c) subcriterion (i)-(iv).",
    "Population reduction in per cent (min/best/max); subfield = type (A1-A4, or e.g. A2+A4); basis = letters a-e.",
    "For A1: whether the causes are reversible, understood and ceased (all three TRUE for A1).",
    "Number of mature individuals (min/best/max).",
    "Change in mature individuals in per cent, negative for a decline (min/best/max).",
    "Years over which mature_decline was measured (best).",
    "Mature individuals in the largest subpopulation (min/best/max).",
    "Share of mature individuals in the largest subpopulation, 0-1 (min/best/max).",
    "Number of subpopulations (min/best/max), for the record.",
    "The D2 scenario: how a plausible threat could drive the taxon to CR or EX in a very short time.",
    "Result of a documented quantitative analysis: CR, EN, VU or none (sources required).",
    "Surveys for RE or possibly extinct taxa: what was searched, where and when.",
    "The assessor's final category; basis = the criteria string (e.g. EN B1ab(iii)+2ab(iii)).",
    "Approval of the final category by a group expert: author = approver, subfield = role, value = expert group."
  ),
  stringsAsFactors = FALSE
)

.EVIDENCE_COLUMNS <- c("taxon", "field", "subfield", "value", "min", "best", "max",
                       "qualifier", "basis", "record_id", "author", "date",
                       "justification", "sources")

# Guidelines 3.1: the terms for how a value was obtained.
.QUALIFIERS <- c("observed", "estimated", "projected", "inferred", "suspected")

#' Expert Evidence: Fields, Templates, Import and Validation
#'
#' Expert evidence is one table with a row per piece of evidence (prompt
#' section 6). Every row names the taxon, the `field` (see
#' `evidence_fields()`), and who gave it and why: `author`, `date`
#' (YYYY-MM-DD) and `justification` are required, `sources` recommended.
#' Numbers are `min`/`best`/`max` (Guidelines §3.2); `qualifier` says how
#' they were obtained (observed, estimated, projected, inferred, suspected;
#' §3.1). Record-level entries carry the NDOP `record_id` (`ID_NALEZ`).
#'
#' Evidence never edits NDOP data. Record entries become flags, numbers and
#' conditions go to the rule engine as evidence beside the computed values,
#' and the final category is checked against them ([final_assessment()]).
#' When a field is given more than once for a taxon, the latest entry (by
#' `date`, then by row) is used, so an evidence file can grow over time.
#'
#' @name evidence
#' @return `evidence_fields()`: the field table. `evidence_template()`: an
#'   empty evidence table with one example row per field, written to `file`
#'   when given (`.csv`, or `.xlsx` with a second sheet describing the
#'   fields; needs \pkg{writexl}). `read_evidence()` and
#'   `validate_evidence()`: a validated table of class `ndopred_evidence`.
#' @seealso [write_evidence()], [assess_taxon()], [final_assessment()]
#' @examples
#' ev <- data.frame(
#'   taxon = "Bombina bombina", field = c("threat", "threat_locations", "decline"),
#'   subfield = c("", "", "iii"), value = c("drainage of breeding ponds", "", "TRUE"),
#'   min = c(NA, 6, NA), best = c(NA, 8, NA), max = c(NA, 12, NA),
#'   basis = c("", "b", ""), author = "J. Novak", date = "2026-10-15",
#'   justification = "Field survey 2024-2026"
#' )
#' ev <- validate_evidence(ev)
#' evidence_for_engine(ev)$engine$threat_locations
NULL

#' @rdname evidence
#' @export
evidence_fields <- function() .EVIDENCE_FIELDS

#' @rdname evidence
#' @param taxon Character. Taxon name(s) for the template rows.
#' @param file Optional path; `.csv` or `.xlsx`.
#' @export
evidence_template <- function(taxon = "", file = NULL) {
  f <- .EVIDENCE_FIELDS
  tmpl <- do.call(rbind, lapply(taxon, function(tx) data.frame(
    taxon = tx, field = f$field,
    subfield = sub("\\|.*$", "", f$subfields),
    value = "", min = NA_real_, best = NA_real_, max = NA_real_,
    qualifier = "", basis = "", record_id = NA_real_,
    author = "", date = "", justification = "", sources = "",
    stringsAsFactors = FALSE
  )))
  if (!is.null(file)) {
    if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Writing .xlsx needs the writexl package; use a .csv file instead.", call. = FALSE)
      }
      writexl::write_xlsx(list(evidence = tmpl, fields = f), file)
    } else {
      utils::write.csv(tmpl, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  }
  invisible(tmpl)
}

#' @rdname evidence
#' @param path Path to an evidence file (`.csv` in UTF-8, or `.xlsx` with the
#'   evidence on a sheet named "evidence" or on the first sheet; needs
#'   \pkg{readxl}).
#' @export
read_evidence <- function(path) {
  if (!file.exists(path)) stop("Evidence file not found: ", path, call. = FALSE)
  ev <- if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading .xlsx needs the readxl package; save the sheet as .csv instead.", call. = FALSE)
    }
    sheets <- readxl::excel_sheets(path)
    as.data.frame(readxl::read_excel(path, sheet = if ("evidence" %in% sheets) "evidence" else 1,
                                     col_types = "text"))
  } else {
    utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                    colClasses = "character", na.strings = "")   # "NA" is a category (not applicable)
  }
  out <- validate_evidence(ev)
  attr(out, "path") <- path
  out
}

#' @rdname evidence
#' @param ev A data frame of evidence rows.
#' @export
validate_evidence <- function(ev) {
  ev <- as.data.frame(ev, stringsAsFactors = FALSE)
  if (!all(c("taxon", "field") %in% names(ev))) {
    stop("Evidence needs at least the columns `taxon` and `field`.", call. = FALSE)
  }
  for (k in setdiff(.EVIDENCE_COLUMNS, names(ev))) ev[[k]] <- NA
  ev <- ev[, c(.EVIDENCE_COLUMNS, setdiff(names(ev), .EVIDENCE_COLUMNS)), drop = FALSE]
  txt <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  for (k in c("taxon", "field", "subfield", "value", "qualifier", "basis", "author",
              "date", "justification", "sources")) ev[[k]] <- txt(ev[[k]])
  for (k in c("min", "best", "max", "record_id")) {
    ev[[k]] <- suppressWarnings(as.numeric(gsub(",", ".", txt(ev[[k]]), fixed = TRUE)))
  }
  # Template rows left empty are not evidence.
  blank <- ev$value == "" & is.na(ev$min) & is.na(ev$best) & is.na(ev$max) &
    is.na(ev$record_id) & ev$author == "" & ev$justification == ""
  ev <- ev[!blank, , drop = FALSE]
  rownames(ev) <- NULL

  problems <- character(0)
  bad <- function(i, msg) problems <<- c(problems, sprintf("row %d (%s): %s", i, ev$field[i], msg))
  f <- .EVIDENCE_FIELDS
  for (i in seq_len(nrow(ev))) {
    def <- f[f$field == ev$field[i], ]
    if (!nrow(def)) { bad(i, "unknown field; see evidence_fields()"); next }
    if (ev$taxon[i] == "") bad(i, "taxon is empty")
    if (ev$author[i] == "") bad(i, "author is required")
    if (is.na(suppressWarnings(as.Date(ev$date[i], format = "%Y-%m-%d")))) bad(i, "date must be YYYY-MM-DD")
    if (ev$justification[i] == "" && def$field != "approval") bad(i, "justification is required")
    if (ev$qualifier[i] != "" && !tolower(ev$qualifier[i]) %in% .QUALIFIERS) {
      bad(i, paste("qualifier must be one of", paste(.QUALIFIERS, collapse = ", ")))
    }
    subs <- if (nzchar(def$subfields)) strsplit(def$subfields, "|", fixed = TRUE)[[1]] else character(0)
    if (length(subs)) {
      given <- strsplit(ev$subfield[i], "[+,; ]+")[[1]]
      if (!length(given) || !all(given %in% subs)) {
        bad(i, paste("subfield must be one of", paste(subs, collapse = ", ")))
      }
    }
    if (def$type == "logical" && is.na(.as_flag(ev$value[i]))) bad(i, "value must be TRUE or FALSE")
    if (def$type == "category") {
      vals <- strsplit(def$values, "|", fixed = TRUE)[[1]]
      if (!ev$value[i] %in% vals) bad(i, paste("value must be one of", paste(vals, collapse = ", ")))
    }
    if (def$type == "text" && def$field != "approval" && ev$value[i] == "") bad(i, "value (the text) is empty")
    if (def$type == "record" && is.na(ev$record_id[i])) bad(i, "record_id (NDOP ID_NALEZ) is required")
    if (def$type == "number") {
      r <- c(ev$min[i], ev$best[i], ev$max[i])
      if (all(is.na(r))) {
        bad(i, "give best, or min and max")
      } else {
        rr <- .range3(c(min = r[1], best = r[2], max = r[3]))
        if (rr[["min"]] > rr[["best"]] || rr[["best"]] > rr[["max"]]) bad(i, "min <= best <= max is required")
        if (def$field == "prop_largest_subpopulation" && (rr[["min"]] < 0 || rr[["max"]] > 1)) bad(i, "a share between 0 and 1")
        if (def$field == "reduction" && (rr[["min"]] < 0 || rr[["max"]] > 100)) bad(i, "a reduction between 0 and 100 per cent")
        if (def$field %in% c("generation_length", "mature_decline_years") && rr[["min"]] <= 0) bad(i, "must be positive")
      }
    }
    if (def$field == "threat_locations" && !tolower(ev$basis[i]) %in% c("all", "a", "b", "c", "d")) {
      bad(i, "basis must be 'all' (the threat covers the whole range) or the Guidelines 4.11 option a, b, c or d")
    }
    if (def$field == "reduction" && ev$basis[i] != "" &&
        !all(strsplit(gsub("[^a-z]", "", tolower(ev$basis[i])), "")[[1]] %in% letters[1:5])) {
      bad(i, "basis must be letters a-e (Criteria v3.1 A)")
    }
    # Guidelines 8: the threat "must be specified"; a bare yes is not one.
    if (def$field %in% c("threat", "d2_scenario") && !is.na(.as_flag(ev$value[i]))) {
      bad(i, "describe the threat in words (Guidelines 8); use no_plausible_threat for 'none'")
    }
    if (def$field == "criterion_e" && ev$sources[i] == "") bad(i, "criterion E needs its analysis in sources")
    if (def$field == "approval" && ev$value[i] == "") bad(i, "value must name the expert group")
  }
  if (length(problems)) {
    stop("Evidence is not valid:\n  ", paste(problems, collapse = "\n  "), call. = FALSE)
  }
  structure(ev, class = c("ndopred_evidence", "data.frame"))
}

#' Store an Evidence File as a New Version
#'
#' Writes the evidence as a new UTF-8 CSV named
#' `<taxon>_evidence_<YYYYmmddTHHMMSS>.csv`. Earlier versions are never
#' overwritten, so the history of an assessment stays on disk.
#'
#' @param ev Evidence, validated or not.
#' @param dir Folder to write to.
#' @param taxon Name used in the file name; defaults to the evidence's taxon.
#' @return The path of the new file, invisibly.
#' @seealso [latest_evidence()], [read_evidence()]
#' @export
write_evidence <- function(ev, dir, taxon = NULL) {
  ev <- validate_evidence(ev)
  if (is.null(taxon)) taxon <- if (nrow(ev)) ev$taxon[1] else "taxon"
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  path <- file.path(dir, paste0(gsub("[^A-Za-z0-9]+", "_", taxon), "_evidence_", stamp, ".csv"))
  k <- 1
  while (file.exists(path)) {
    k <- k + 1
    path <- sub("(_[0-9]+)?\\.csv$", paste0("_", k, ".csv"), path)
  }
  utils::write.csv(as.data.frame(ev)[, .EVIDENCE_COLUMNS], path, row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  invisible(path)
}

#' The Latest Evidence File for a Taxon
#'
#' @param dir Folder of evidence files written by [write_evidence()].
#' @param taxon Taxon name.
#' @return The path of the newest version, or `NULL` if there is none.
#' @export
latest_evidence <- function(dir, taxon) {
  pat <- paste0("^", gsub("[^A-Za-z0-9]+", "_", taxon), "_evidence_.*\\.csv$")
  files <- sort(list.files(dir, pattern = pat, full.names = TRUE))
  if (length(files)) files[length(files)] else NULL
}

# TRUE / FALSE from the spellings experts use; NA otherwise.
.as_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("true", "t", "yes", "y", "ano", "1")) return(TRUE)
  if (x %in% c("false", "f", "no", "n", "ne", "0")) return(FALSE)
  NA
}

# The entries in force: the latest per taxon, field, subfield and record.
.active_evidence <- function(ev, taxon = NULL) {
  if (!is.null(taxon)) ev <- ev[ev$taxon == taxon, , drop = FALSE]
  if (!nrow(ev)) return(ev)
  ord <- order(as.Date(ev$date), seq_len(nrow(ev)))
  ev <- ev[ord, , drop = FALSE]
  key <- paste(ev$taxon, ev$field, ev$subfield, ev$record_id, sep = "\r")
  multi <- ev$field %in% c("approval", "survey")   # every approval and survey counts
  keep <- multi | !duplicated(key, fromLast = TRUE)
  ev[keep, , drop = FALSE]
}

#' Turn Evidence into Engine Inputs
#'
#' Splits validated evidence into what each part of the pipeline uses: the
#' rule engine's `evidence` list ([assessment_inputs()]), record flags,
#' configuration overrides, the final category with its approvals, and the
#' eligibility entries for the regional step.
#'
#' @param ev Validated evidence ([validate_evidence()]).
#' @param taxon Optional taxon name; evidence for other taxa is ignored.
#' @return A list: `engine`, `records` (data frame: `record_id`, `action`,
#'   `reason`, `author`, `date`), `config` (`generation_length`,
#'   `generation_length_source`, `recent_start`), `final`, `approvals`,
#'   `surveys`, `eligibility`, `checks` (notes on entries not used as given)
#'   and `entries` (the rows in force).
#' @export
evidence_for_engine <- function(ev, taxon = NULL) {
  ev <- .active_evidence(validate_evidence(ev), taxon)
  # The entry in force for a field (and subfield), or NULL.
  one <- function(field, sub = NULL) {
    keep <- ev$field == field
    if (!is.null(sub)) keep <- keep & ev$subfield == sub
    x <- ev[keep, , drop = FALSE]
    if (nrow(x)) x[nrow(x), , drop = FALSE] else NULL
  }
  rng <- function(row) if (is.null(row)) NA_real_ else .range3(c(min = row$min, best = row$best, max = row$max))
  flag <- function(row) if (is.null(row)) NA else .as_flag(row$value)
  checks <- character(0)
  engine <- list()

  no_threat <- one("no_plausible_threat")
  threat <- one("threat")
  d2 <- one("d2_scenario")
  if (!is.null(no_threat) && isTRUE(flag(no_threat))) {
    engine$threat <- FALSE
  } else if (!is.null(threat)) {
    engine$threat <- threat$value
  } else if (!is.null(d2)) {
    engine$threat <- d2$value
  }
  loc <- one("threat_locations")
  if (!is.null(loc)) {
    if (tolower(loc$basis) == "a") {
      # Guidelines 4.11 (a): "number of locations is not used (i.e., the
      # subcriteria that refer to the number of locations consequently are
      # not met)".
      engine$threat_locations <- Inf
      checks <- c(checks, "locations not used (Guidelines 4.11 option a): the location subcriteria are not met")
    } else {
      engine$threat_locations <- rng(loc)
    }
  }
  if (!is.null(one("severely_fragmented"))) engine$severely_fragmented <- flag(one("severely_fragmented"))
  flags_of <- function(field, keys) {
    x <- ev[ev$field == field, , drop = FALSE]
    if (!nrow(x)) return(NULL)
    out <- vapply(x$value, .as_flag, logical(1), USE.NAMES = FALSE)
    names(out) <- x$subfield
    out[intersect(keys, names(out))]
  }
  dec <- flags_of("decline", c("i", "ii", "iii", "iv", "v"))
  if (length(dec)) engine$decline <- dec
  fl <- flags_of("fluctuation", c("i", "ii", "iii", "iv"))
  if (length(fl)) engine$fluctuation <- fl

  red <- ev[ev$field == "reduction", , drop = FALSE]
  if (nrow(red)) {
    red <- red[nrow(red), , drop = FALSE]
    types <- strsplit(red$subfield, "[+,; ]+")[[1]]
    if ("A1" %in% types) {
      causes <- vapply(c("reversible", "understood", "ceased"),
                       function(k) isTRUE(flag(one("a1_causes", k))), logical(1))
      if (!all(causes)) {
        types <- unique(c(setdiff(types, "A1"), "A2"))
        checks <- c(checks, paste0("A1 needs the causes to be reversible, understood and ceased ",
                                   "(Criteria v3.1 A1); evaluated as A2"))
      }
    }
    engine$reduction <- rng(red)
    engine$reduction_types <- types
    if (nzchar(red$basis)) engine$reduction_basis <- gsub("[^a-e]", "", tolower(red$basis))
  }
  if (!is.null(one("mature_individuals"))) engine$mature <- rng(one("mature_individuals"))
  if (!is.null(one("mature_decline"))) engine$mature_decline <- rng(one("mature_decline"))
  if (!is.null(one("mature_decline_years"))) engine$mature_decline_years <- rng(one("mature_decline_years"))[["best"]]
  if (!is.null(one("largest_subpopulation"))) engine$largest_subpop <- rng(one("largest_subpopulation"))
  if (!is.null(one("prop_largest_subpopulation"))) engine$prop_largest <- rng(one("prop_largest_subpopulation"))
  e <- one("criterion_e")
  if (!is.null(e) && e$value %in% c("CR", "EN", "VU")) engine$e_category <- e$value

  rec <- ev[ev$field %in% c("record_exclude", "record_include"), , drop = FALSE]
  records <- data.frame(
    record_id = rec$record_id,
    action = ifelse(rec$field == "record_exclude", "exclude", "include"),
    reason = ifelse(rec$field == "record_exclude", rec$subfield, "re-included"),
    author = rec$author, date = rec$date, justification = rec$justification,
    stringsAsFactors = FALSE
  )

  config <- list()
  gl <- one("generation_length")
  if (!is.null(gl)) {
    config$generation_length <- rng(gl)[["best"]]
    config$generation_length_source <- sprintf("expert evidence: %s, %s", gl$author, gl$date)
  }
  cw <- one("current_window_start")
  if (!is.null(cw)) config$recent_start <- rng(cw)[["best"]]

  fin <- one("final_category")
  final <- if (is.null(fin)) NULL else list(
    category = fin$value, criteria = fin$basis, justification = fin$justification,
    author = fin$author, date = fin$date
  )
  list(
    engine = engine, records = records, config = config, final = final,
    approvals = ev[ev$field == "approval", c("author", "subfield", "value", "date", "justification"), drop = FALSE],
    surveys = ev[ev$field == "survey", c("value", "author", "date", "sources"), drop = FALSE],
    eligibility = list(origin = if (is.null(one("origin"))) NA_character_ else one("origin")$value,
                       occurrence_status = if (is.null(one("occurrence_status"))) NA_character_ else
                         one("occurrence_status")$value),
    checks = checks,
    entries = ev
  )
}

#' Apply Record-Level Evidence to Flagged Records
#'
#' An expert can exclude an NDOP record (misidentification, introduction,
#' vagrant, duplicate, imprecise, other) or use one the automatic flags
#' excluded. The record keeps its row; `include` and `exclusion_reason`
#' change and `flag_expert` says who decided. Negative findings and records
#' without coordinates cannot be re-included: they carry no position of a
#' presence.
#'
#' @param occ Records from [get_assessment_data()].
#' @param records The `records` element of [evidence_for_engine()].
#' @return `occ` with `include`, `exclusion_reason` and `flag_expert`
#'   updated. Attribute `expert_record_notes` lists entries that matched no
#'   record or could not be applied.
#' @export
apply_record_evidence <- function(occ, records) {
  if (is.null(occ) || !nrow(occ)) return(occ)
  if (!"flag_expert" %in% names(occ)) occ$flag_expert <- ""
  notes <- character(0)
  if (is.null(records) || !nrow(records) || !"ID_NALEZ" %in% names(occ)) return(occ)
  ids <- suppressWarnings(as.numeric(as.character(occ$ID_NALEZ)))
  for (k in seq_len(nrow(records))) {
    r <- records[k, ]
    hit <- which(ids == r$record_id)
    if (!length(hit)) {
      notes <- c(notes, sprintf("record %s: not among this taxon's records", r$record_id))
      next
    }
    who <- sprintf("%s, %s", r$author, r$date)
    if (r$action == "exclude") {
      occ$include[hit] <- FALSE
      occ$exclusion_reason[hit] <- sprintf("expert: %s (%s)", r$reason, who)
      occ$flag_expert[hit] <- paste("excluded:", r$reason)
    } else {
      blocked <- (if ("flag_negative" %in% names(occ)) occ$flag_negative[hit] %in% TRUE else FALSE) |
        (if ("flag_no_coords" %in% names(occ)) occ$flag_no_coords[hit] %in% TRUE else FALSE)
      if (any(blocked)) {
        notes <- c(notes, sprintf("record %s: a negative finding or without coordinates; not re-included", r$record_id))
        next
      }
      occ$include[hit] <- TRUE
      occ$exclusion_reason[hit] <- NA_character_
      occ$flag_expert[hit] <- sprintf("re-included (%s)", who)
    }
  }
  attr(occ, "expert_record_notes") <- notes
  occ
}

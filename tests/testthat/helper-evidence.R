# Evidence rows for the tests (Phase 3).

ev_row <- function(field, value = "", subfield = "", min = NA, best = NA, max = NA,
                   basis = "", record_id = NA, date = "2026-10-15", author = "A. Expert",
                   justification = "test entry", sources = "", taxon = "Test sp") {
  data.frame(taxon = taxon, field = field, subfield = subfield, value = value,
             min = min, best = best, max = max, qualifier = "", basis = basis,
             record_id = record_id, author = author, date = date,
             justification = justification, sources = sources, stringsAsFactors = FALSE)
}
evidence_of <- function(...) validate_evidence(do.call(rbind, list(...)))

approval <- function(date = "2026-10-15") {
  ev_row("approval", value = "Amphibian expert group", subfield = "taxonomist",
         author = "B. Approver", date = date, justification = "")
}

approval_for <- function(taxon) {
  a <- approval()
  a$taxon <- taxon
  a
}

# Phase 4: the regional step (IUCN 2012, Guidelines for Application of IUCN
# Red List Criteria at Regional and National Levels v4.0, "RG").

cfg <- ndopred_config(2026)
deg <- intToUtf8(176)   # the degree sign of RG V.3

# ------------------------------------------------------- step one (RG IV.2)

test_that("Introduced taxa, vagrants and occasional breeders are Not Applicable", {
  expect_false(regional_eligibility(list(origin = "introduced"), cfg)$eligible)
  expect_false(regional_eligibility(list(occurrence_status = "vagrant"), cfg)$eligible)
  expect_false(regional_eligibility(list(occurrence_status = "occasional_breeder"), cfg)$eligible)
  expect_match(regional_eligibility(list(origin = "introduced"), cfg)$reason, "introduced")
})

test_that("A taxon on the list of introduced taxa is NA unless the evidence gives its origin", {
  # NDOP records carry no origin. The 2026-10-17 bulk run proposed Testudo
  # hermanni as CR and Hemidactylus turcicus as EN.
  expect_true(all(c("Testudo hermanni", "Hemidactylus turcicus") %in% introduced_taxa()))
  expect_equal(cfg$introduced, introduced_taxa())
  listed <- regional_eligibility(list(), cfg, taxon = "Testudo hermanni")
  expect_false(listed$eligible)
  expect_match(listed$reason, "^introduced \\(on the list of introduced taxa")
  # An assessor's origin entry wins over the list.
  expect_true(regional_eligibility(list(origin = "native"), cfg, taxon = "Testudo hermanni")$eligible)
  expect_true(regional_eligibility(list(origin = "uncertain"), cfg, taxon = "Testudo hermanni")$eligible)
  # Not on the list, no taxon given, or no list: assessed.
  expect_true(regional_eligibility(list(), cfg, taxon = "Emys orbicularis")$eligible)
  expect_true(regional_eligibility(list(), cfg)$eligible)
  expect_true(regional_eligibility(list(), ndopred_config(2026, introduced = character(0)),
                                   taxon = "Testudo hermanni")$eligible)
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(taxon = c(" A b ", "C d", "")), path, row.names = FALSE)
  withr::local_envvar(NDOPRED_INTRODUCED_TAXA = path)
  expect_equal(introduced_taxa(), c("A b", "C d"))
})

test_that("A colonizer is assessed after reproducing for 10 consecutive years", {
  young <- regional_eligibility(list(reproduction_since = 2018), cfg)   # 9 years to 2026
  expect_false(young$eligible)
  expect_match(young$reason, "recent colonizer: reproducing in the region for 9 years")
  expect_true(regional_eligibility(list(reproduction_since = 2017), cfg)$eligible)
  expect_true(regional_eligibility(list(reproduction_since = 2021),
                                   ndopred_config(2026, colonizer_years = 5))$eligible)
})

test_that("Visitors are assessed as visiting populations; unknowns lead to assessment", {
  v <- regional_eligibility(list(occurrence_status = "visitor"), cfg)
  expect_true(v$eligible)
  expect_equal(v$population, "visiting")
  none <- regional_eligibility(list(), cfg)
  expect_true(none$eligible)
  expect_false(none$documented)
  expect_match(none$reason, "eligibility not documented")
  expect_true(regional_eligibility(list(origin = "uncertain"), cfg)$eligible)
  expect_match(regional_eligibility(list(origin = "reintroduced"), cfg)$reason, "reintroduced")
})

test_that("The optional filter is off by default and applies only to a known share", {
  low <- list(global_share = 0.5)
  expect_true(regional_eligibility(low, cfg)$eligible)
  filtered <- ndopred_config(2026, regional_filter_share = 1)
  expect_false(regional_eligibility(low, filtered)$eligible)
  expect_true(regional_eligibility(list(global_share = 3), filtered)$eligible)
  expect_true(regional_eligibility(list(), filtered)$eligible)   # share unknown: assessed
})

# ----------------------------------------------- step three (RG IV.4, Fig. 3)

test_that("Breeding populations follow questions 3a-3c", {
  adj <- function(...) regional_adjustment("EN", c(...))
  # Immigration that will not decrease: downlist one step, with a degree sign.
  d <- adj("3a" = "yes", "3b" = "no")
  expect_equal(c(d$category, d$display, d$direction), c("VU", paste0("VU", deg), "down"))
  expect_equal(adj("3a" = "likely", "3b" = "unlikely")$category, "VU")
  # No immigration, or not known: no change.
  expect_false(adj("3a" = "no")$adjusted)
  expect_false(adj("3a" = "unknown")$adjusted)
  expect_false(regional_adjustment("EN")$adjusted)
  # Immigration decreasing (or not known) and a sink: uplist.
  u <- adj("3a" = "yes", "3b" = "yes", "3c" = "yes")
  expect_equal(c(u$category, u$direction), c("CR", "up"))
  expect_equal(adj("3a" = "yes", "3b" = "unknown", "3c" = "likely")$category, "CR")
  # Decreasing immigration and not a sink, or not known: no change.
  expect_false(adj("3a" = "yes", "3b" = "yes", "3c" = "no")$adjusted)
  expect_false(adj("3a" = "yes", "3b" = "yes", "3c" = "unknown")$adjusted)
})

test_that("Two steps when documented; the ends of the scale stay put", {
  expect_equal(regional_adjustment("EN", c("3a" = "yes", "3b" = "no"), steps = 2)$category, "NT")
  expect_equal(regional_adjustment("VU", c("3a" = "yes", "3b" = "no"), steps = 3)$category, "LC")
  expect_false(regional_adjustment("LC", c("3a" = "yes", "3b" = "no"))$adjusted)
  expect_false(regional_adjustment("CR", c("3a" = "yes", "3b" = "yes", "3c" = "yes"))$adjusted)
})

test_that("EX, EW, RE, DD, NE and NA are never adjusted", {
  for (k in c("EX", "EW", "RE", "DD", "NE", "NA")) {
    a <- regional_adjustment(k, c("3a" = "yes", "3b" = "no"))
    expect_false(a$adjusted)
    expect_equal(a$category, k)
  }
})

test_that("Visiting populations follow questions 3d-3f", {
  adj <- function(..., global = NA) regional_adjustment("VU", c(...), population = "visiting",
                                                        global_criteria = global)
  expect_equal(adj("3d" = "no", "3e" = "no", "3f" = "yes")$category, "NT")
  expect_false(adj("3d" = "yes", "3e" = "no", "3f" = "yes")$adjusted)
  expect_false(adj("3d" = "unknown", "3e" = "no", "3f" = "yes")$adjusted)
  expect_false(adj("3d" = "no", "3e" = "yes", "3f" = "yes")$adjusted)
  expect_false(adj("3d" = "no", "3e" = "no", "3f" = "unknown")$adjusted)
  # A taxon listed globally under D cannot be expected to rescue (box 3f).
  d <- adj("3d" = "no", "3e" = "no", "3f" = "yes", global = "VU D1")
  expect_false(d$adjusted)
  expect_match(utils::tail(d$trail, 1), "listed globally under D")
})

# -------------------------------------------------------------- the pipeline

test_that("Regional evidence reaches the result: degree sign, trail, global fields", {
  local_mock_ndop()
  sp <- "Restricted stable sp"
  occ <- get_assessment_data(sp)
  yn <- as.numeric(format(Sys.Date(), "%Y"))
  ev <- evidence_of(
    ev_row("threat", "building on the only site", taxon = sp),
    ev_row("regional_step3", "yes", subfield = "3a", taxon = sp),
    ev_row("regional_step3", "no", subfield = "3b", taxon = sp),
    ev_row("global_category", "LC", taxon = sp, sources = "IUCN Red List 2025-2"),
    ev_row("global_population_share", best = 2, taxon = sp)
  )
  res <- assess_taxon(occ, sp, ndopred_config(yn), evidence = ev)
  expect_equal(res$result$Category, "VU")                         # step two: VU D2
  expect_equal(res$result$Regional_Category, paste0("NT", deg))   # step three: downlisted
  expect_true(is.na(res$result$Regional_Criteria))                # no criteria for NT
  expect_match(res$result$Regional_Step, "3a yes; 3b no; downlisted 1 step from VU")
  expect_equal(res$result$Global_Category, "LC")
  expect_equal(res$result$Global_Population_Share, "2")
  # Without regional evidence: assessed, unchanged, share unknown ("?", RG V.4).
  plain <- assess_taxon(occ, sp, ndopred_config(yn))
  expect_equal(plain$result$Regional_Category, plain$result$Category)
  expect_equal(plain$result$Global_Population_Share, "?")
  expect_match(plain$result$Regional_Step, "eligibility not documented")
})

test_that("Step one makes an introduced taxon NA before any criterion is applied", {
  local_mock_ndop()
  sp <- "Declining sp"
  occ <- get_assessment_data(sp)
  ev <- evidence_of(ev_row("origin", "introduced", taxon = sp))
  res <- assess_taxon(occ, sp, ndopred_config(as.numeric(format(Sys.Date(), "%Y"))), evidence = ev)
  expect_equal(res$result$Category, "NA")
  expect_equal(res$result$Regional_Category, "NA")
  expect_match(res$result$Criteria, "^Not applicable: introduced")
})

test_that("A listed introduced taxon is NA in the pipeline without any evidence", {
  local_mock_ndop()
  sp <- "Declining sp"
  occ <- get_assessment_data(sp)
  cfg_sp <- ndopred_config(as.numeric(format(Sys.Date(), "%Y")), introduced = sp)
  res <- assess_taxon(occ, sp, cfg_sp)
  expect_equal(res$result$Category, "NA")
  expect_equal(res$result$Regional_Category, "NA")
  expect_match(res$result$Criteria, "^Not applicable: introduced \\(on the list")
  # The list survives a taxon's own config (here its generation length).
  gl <- assess_taxon(occ, sp, cfg_sp, evidence = evidence_of(ev_row("generation_length", best = 2, taxon = sp)))
  expect_equal(gl$config$introduced, sp)
  expect_equal(gl$result$Category, "NA")
  # An assessor's origin entry overrides the list, here and in the final check.
  ev <- evidence_of(ev_row("origin", "native", taxon = sp))
  native <- assess_taxon(occ, sp, cfg_sp, evidence = ev)
  expect_false(identical(native$result$Category, "NA"))
  expect_true(native$regional$eligibility$eligible)
  expect_false(grepl("^NA", native$final$evidence_only$regional))
})

# An assessment with EOO 4,000 km2 and AOO 400 km2 and the given evidence,
# and the final-category check on it.
final_regional <- function(ev) {
  ex <- evidence_for_engine(ev)
  inp <- assessment_inputs(eoo_km2 = 4000, aoo_km2 = 400, n_records = 50, n_current = 20,
                           year_last = 2026, evidence = ex$engine)
  a <- assess_criteria(inp, cfg)
  final_assessment(a, categorise(a), ex)
}

test_that("A final category downlisted by documented rescue is consistent and marked", {
  base <- list(ev_row("threat", "drainage"), ev_row("threat_locations", best = 3, basis = "all"),
               ev_row("decline", "TRUE", subfield = "iii"),
               ev_row("regional_step3", "yes", subfield = "3a"),
               ev_row("regional_step3", "no", subfield = "3b"))
  fin <- final_regional(do.call(evidence_of, c(base, list(
    ev_row("final_category", "VU", basis = "B1ab(iii)+2ab(iii)", justification = "rescue from Slovakia"),
    approval()))))
  expect_equal(fin$status, "approved")
  expect_equal(fin$display, paste0("VU", deg))
  expect_equal(fin$evidence_only$regional, paste0("VU", deg))
  # Without the regional answers the same VU would be below the evidence.
  plain <- final_regional(do.call(evidence_of, c(base[1:3], list(
    ev_row("final_category", "VU", basis = "B1ab(iii)+2ab(iii)", justification = "x"), approval()))))
  expect_true(any(grepl("a lower category needs a documented reason", plain$issues)))
  # Step one decides NA: another category is inconsistent.
  na <- final_regional(evidence_of(ev_row("occurrence_status", "vagrant"),
                                   ev_row("final_category", "LC", justification = "x"), approval()))
  expect_true(any(grepl("step one: vagrant", na$issues)))
})

test_that("More than two steps is noted as extremely rare", {
  ex <- evidence_for_engine(evidence_of(ev_row("regional_steps", best = 3)))
  expect_match(ex$checks, "extremely rare")
  expect_error(validate_evidence(ev_row("regional_steps", best = 1.5)), "whole number")
})

test_that("GAP-12: regional eligibility and adjustment exist in the pipeline", {
  # RG IV.2-IV.4 and V.3-V.4: eligibility before (NA for introduced and
  # vagrant taxa), regional adjustment after, and the published fields.
  local_mock_ndop()
  res <- assess_species("Stable widespread sp", recent_start = 2010, recent_end = 2026)
  expect_true(all(c("Regional_Category", "Regional_Step", "Global_Category",
                    "Global_Population_Share") %in% names(res)))
})

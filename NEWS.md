# ndopred 0.3.0

Phase 1 of the IUCN alignment planned in `methodology/gap_analysis.md`: one
rule engine for every entry point, three-valued logic, and proposals that say
what they rest on. 29 of the 42 Phase 0 gap tests now pass and have moved to
the regular suite (`test-iucn-alignment.R`); the other 13 belong to later
phases and stay opt-in (`test-gap-phase0.R`).

## One rule engine

* New `ndopred_config()`, `assessment_inputs()`, `assess_criteria()` and
  `categorise()`. Every threshold now lives in one file (`R/engine.R`), each
  with its source in Criteria v3.1 or Guidelines v16. The single-taxon app no
  longer re-implements criteria A to D (its C2 check had promoted EN C2a(i) to
  CR on identical inputs).
* New `taxon_metrics()` and `assess_taxon()`: the pipeline used by
  `assess_species()`, `batch_assess()` and both apps. The bulk app and
  `assess_species()` now compute the same B2 AOO for the same window (the bulk
  app used to pin it to the origin minimised over both windows).
* `summarize_assessment()`, `assess_criterion_a()` and `assess_criterion_d2()`
  are kept as wrappers over the engine.

## Proposals, support and evidence gaps

* Every result has a proposed category and its criteria in the established
  format, plus `Criteria_IUCN` (Annex 2 notation), `Support` (evidence /
  indication / candidate / none), `Needs_Evidence`, `Category_Range` and
  `Flags`. The bulk export keeps every existing column and adds
  `Kritéria (IUCN zápis)`, `Opora návrhu`, `Nutno doložit`,
  `Rozsah kategorií`, `Příznaky`, `AOO nový, zvolená mřížka (km2)`,
  `Referenční rok` and `issue_*` reason codes (gap analysis decisions D10,
  D11).
* Cluster counts, raw window-to-window changes and NDOP counts are
  *indications*: they still support proposals, and each one that does is
  listed as needing evidence.
* Unknown is no longer read as "no". `severely_fragmented`,
  `habitat_decline` and `d2_threat` default to `NA`. A restricted taxon with
  nothing evidenced is now proposed as NT with a range (Guidelines 10.1)
  rather than LC.

## Behaviour changes

* **The reference year is explicit.** `summarize_assessment()` and
  `batch_assess()` require `reference_year`; `calculate_trend()`,
  `calculate_pop_metrics()` and `plot_iucn()` require `year_now` (or explicit
  window bounds). Nothing reads the system clock any more.
* **D2 needs a described threat.** `d2_threat = "drainage of the only fen"`;
  `d2_threat = TRUE` warns and counts as unknown; `d2_threat = FALSE` records
  that there is no plausible threat, so D2 and the location subcriteria are
  not met (Guidelines 4.11, 8). `assess_criterion_d2()` returns `NA` when the
  threat is unknown.
* **RE, DD and NE.** "Last record more than 50 years ago" is now an RE
  *candidate* needing documented surveys; the year threshold is a config
  convention. DD is proposed only as a candidate, for a taxon with records but
  none current and nothing else to go on; few records alone no longer give DD.
  A taxon with no usable records is NE (Guidelines 10.4).
  `assess_criterion_a(NA)` returns `NA`, not `"DD"`.
* **Errors are not categories.** A failed download or calculation leaves the
  category empty and says why in `issue_*`; `batch_assess()` keeps a row for
  every taxon.
* **Notation.** `Criteria_IUCN` follows Annex 2: `B1ab(ii)+2ab(ii)`, `CR D` /
  `EN D` (not `D1`), `C2a(i,ii)`, `VU D1+2`.
* **Bulk app.** It has a reference-year input. B2 always uses the 2 x 2 km
  grid; another grid size is for maps and a display column only.
* **Single-taxon app.** Expert entries start as unknown, are tri-state where it
  matters, and are passed to the engine as evidence. The expert view equals the
  automated proposal until something is entered. The new Evidence table tab
  shows every criterion and level.

## Fixes found while rebuilding the engine

* Criterion B was evaluated only at the level of the area threshold met. A
  taxon with EOO under 100 km2 but four locations failed CR and came out NT;
  it is now EN, as the EN area threshold is met too.
* C1 periods are capped at 100 years ("up to a maximum of 100 years in the
  future", Criteria v3.1 C1).
* `R CMD check`: non-ASCII strings in R code are escaped and `.data` is
  imported from dplyr.

# ndopred 0.2.0

Audit of the code against the IUCN documents in `methodology/`
(Categories and Criteria v3.1, 2012; Guidelines v16, March 2024), plus a
robustness pass over live NDOP downloads for six taxa spanning beetles,
plants, an amphibian and a reptile (25 to 27,322 records).

## Found by running against live NDOP data

* **`evaluate_pop` now governs every use of the abundance data**, uniformly:
  criteria C and D1, criterion A basis (b), B(b)(v) and B(c)(iv). The switch
  is a judgement about whether the counts are fit to be used at all, and a
  *trend* from thin or unrepresentative counts is no more trustworthy than a
  *total* from them — less so, being confounded by recording effort
  (Guidelines 4.10.8). Previously the abundance trend leaked past the gate, so
  criterion A could be driven by data the same call had just refused for C and
  D1. With the gate off, criterion A rests on the spatial trend alone, i.e.
  basis (c). *Cypripedium calceolus* (336 of 1,223 counted records usable)
  moves from `EN B2b(i,v)c(iv)` to `NT` on its spatial evidence; *Bombina
  bombina* (3,441 of 6,237) keeps `VU A2b`.
* **`get_mature_terms()` missed the fruiting plant stages.** Only the combined
  `"kvetoucí a plodné …"` forms were listed, so records counted as fruiting
  alone were discarded. The bare `plodné …` / `plodní jedinci` forms are now
  included.
* **The life-stage vocabulary reaches only a minority of counted records**, and
  did so silently. On live data: 93% of counted records used for *Onthophagus
  medius*, 55% for *Bombina bombina*, 49% for *Lucanus cervus*, 27% for
  *Cypripedium calceolus*, 9% for *Pulsatilla vernalis* — the unqualified term
  `"jedinci"` is the commonest value in NDOP and cannot assert maturity.
  `calculate_pop_metrics()` now reports `n_records_used`, `n_records_counted`
  and `ignored_terms`, accepts a `mature_terms` override, and the app shows the
  shortfall beside the criterion C inputs.
* **That shortfall produced a spurious CR.** *Pulsatilla vernalis* was listed
  `CR D1` from `total_mature = 4`, derived from 3 of 41 counted records. The
  app now auto-enables C/D1 only when the count basis is substantial; the taxon
  now comes out `EN B1ab(i,ii); B2ab(i,ii)` on its spatial evidence.
* **The criterion A basis letter was mislabelled.** A count-derived reduction
  was reported as `A2c` (decline in AOO/EOO) instead of `A2b` (index of
  abundance) — seen on *Onthophagus medius*.
* **The expert panel and the automated baseline could disagree**, because the
  panel's seed values were computed with a different `evaluate_pop` and then
  stripped of b(v)/c(iv) a second time. All six live taxa now agree exactly.
* **`!input$use_pop` crashed on a NULL input** ("invalid argument type"); every
  read is now NULL-safe.
* **The map subtitle overflowed the plot width** at the 8x6in size both apps
  save at, clipping the last window; it now wraps onto two lines.
* Confirmed against live exports: NDOP has **no `ROK` column** (only integer
  `DATUM_OD`) and **no `CELED` column**, so the family fallback in the bulk app
  never fires and family must come from the input spreadsheet.

## Crashes fixed

* The single-species app called `calculate_trend(occ, window_years = ...)`,
  which was not an argument of that function. Every assessment failed with
  "unused argument"; `calculate_trend()` now takes `window_years`.
* Both apps called `plot_iucn(..., window = ...)`, also not an argument, so
  **no map was ever produced**. `plot_iucn()` now takes `window`/`year_now` and
  derives its windows from them.
* `assess_species()` passed five positional arguments to
  `summarize_assessment()`, leaving `pop_metrics` missing, so `batch_assess()`
  returned nothing. Both now work and return a consistent one-row frame.
* `summarize_assessment()` failed with "missing value where TRUE/FALSE needed"
  when the number of locations was `NA`.
* `calculate_pop_metrics()` errored instead of returning `NA`s when NDOP data
  had no `POCITANO`/`POCET` columns, taking the single-species app with it.
* Shiny observers with `ignoreNULL = FALSE` fired at start-up and wiped the
  values just seeded from the automated run; `ignoreInit = TRUE` added.

## Methodology corrections

* **D2 no longer auto-lists (Guidelines 8).** Meeting `AOO < 20 km2` or
  `<= 5 locations` now raises a **candidate flag**; VU D2 requires the new
  `d2_threat` argument. Section 8 states that meeting the figures "is not
  sufficient" and that literal application "results in massive over-listing".
* **Restricted-but-unthreatened taxa are no longer Near Threatened.** The
  cascade assigned NT when a taxon met the CR/EN area threshold and *none* of
  the three B conditions — the exact case Guidelines 10.1 lists as one that
  should not be listed.
* **Criterion B subindices reflect what was measured.** (i) now comes from an
  EOO trend rather than an AOO trend, and (iii) habitat decline is never
  inferred — so the string surgery that stripped "iii" out of the bulk app's
  criteria codes afterwards is gone. New `habitat_decline`,
  `severely_fragmented` and `locations_trend` arguments.
* **C2a(ii) uses category-specific proportions** (90 %/95 %/100 % for CR/EN/VU)
  instead of a single 0.95 for all three.
* **C2 requires a continuing decline in mature individuals**, per v3.1; a
  purely spatial trend no longer satisfies it.
* **C1 re-projects the observed rate onto each category's reference period**
  (3 years/1 gen, 5 years/2 gen, 10 years/3 gen) instead of testing one
  10-year rate against all three thresholds. New `generation_length` argument.
* **Small-population NT is judged against the D1 threshold**, not the full
  criterion C ceiling of 10,000 (Guidelines 10.1: ~1,500 is NT, "more than
  2,000" is not).
* **Extreme fluctuations require a recurring pattern** (Guidelines 4.6, 4.7):
  `calculate_pop_metrics()` reports direction reversals alongside the max/min
  ratio, so a monotonic crash is a reduction, not a fluctuation.
* **Decline rates use the exponential model of Guidelines 4.5.1**, projected
  over the assessment window and floored at −100 %, replacing an unconstrained
  linear fit read off at its endpoints.
* **Trend windows are equal-length and adjacent.** The bulk app compared the
  last N years against *all* earlier records combined, and `calculate_trend()`
  defaulted to overlapping windows (2010–2026 vs 2000–2016). Both are fixed;
  overlapping windows now warn.
* **Both trend periods share one AOO grid origin.** Minimising the origin
  separately per period (Guidelines 4.10.2) compared two different grids.
* **Criterion B uses the current-distribution window.** The single-species app
  drove EOO/AOO/locations from the 3-generation reduction window (up to 100
  years), contrary to Guidelines 4.9; its "Recent Window" input had no effect.
* **NT results say "nearly meets"** instead of carrying a criteria code that
  read as though the criterion had been met.
* **The RE screen is documented and parameterised.** The 50-year rule was
  attributed to Guidelines 11.2, which is about the 'Possibly Extinct' tag for
  CR taxa; no such rule exists in either bundled document. `re_threshold_years`
  is now an argument, and the output says it is a screen to confirm by survey.
* **New `decline_tolerance`** (default 5 %) keeps a sampling-noise wobble from
  being asserted as a continuing decline; set to 0 for the literal reading.
* **Non-reference AOO grid sizes warn** (Guidelines 4.10.3).

## Data handling

* `get_assessment_data()` no longer silently drops records with a missing
  `KATASTR` or `VEROH`; `NA` means "not recorded", consistent with the existing
  handling of `NEGATIV`.
* New `extract_year()` parses `YYYYMMDD`, ISO, `Date` and Czech `DD.MM.YYYY`
  values. The previous `substr(x, 1, 4)` returned `NA` for the last of these.
* `calculate_locations()` de-duplicates coordinates and falls back to
  grid-cell centroids above `max_points`; the exact pairwise clustering is
  quadratic and became intractable for well-recorded taxa.
* `calculate_eoo()`/`calculate_aoo()` accept `year_end`; `calculate_aoo()`
  accepts and reports a pinned `origin` and returns `n_cells`.
* The bulk app validates the uploaded CSV, reports the columns it found when
  `TAXON` is missing, states both window ranges in the output, and adds record
  counts, last-record year and a `D2 kandidát` column.

## Packaging and documentation

* **Both apps are now plain exported functions.** `run_bulk_app()` joins
  `run_app()`, so `library(ndopred); run_bulk_app()` is all that is needed.
  Both accept `...` passed to `shiny::runApp()` (`port`, `launch.browser`, …).
* `R/app_bulk.R` previously ran `library()` calls at package load and leaked
  `ui` and `server` objects into the namespace; its UI and server are now
  `bulk_ui()` and `bulk_server()`, with `shiny` imported properly, and the
  100 MB upload cap set and restored inside `run_bulk_app()` rather than
  globally at load.
* Added a package-level help page (`?ndopred`) covering the pipeline, the two
  apps, credentials, and what is deliberately left to the assessor.
* Every exported function now carries a runnable `@examples` block plus
  `@seealso` and `@references` pointing at the guideline sections it
  implements. All 18 example blocks are verified to execute.
* README rewritten: install, credential setup (including the `HOME`/`R_USER`
  trap that silently loses a stored login), a quick start, a full function
  reference, per-app documentation, and the methodology notes that most often
  surprise people.

## Testing

* End-to-end tests drive **both Shiny apps** against a synthetic NDOP fixture,
  alongside unit tests for every corrected rule. 171 assertions.

# ndopred 0.1.1

* **Bug Fix:** Fixed crash when assessing species with 0 occurrences.
* **Harmonization:** Aligned Automated and Initial Expert assessments (Criterion A trends and Criterion B thresholds).

# ndopred 0.9.0

Phase 5 of the IUCN alignment (`methodology/gap_analysis.md`): the trend
module with data tiers T0-T3 (review, Table 5), the alpha-hull for EOO
change, criterion C corrections, and the minimum data quality of Guidelines
Table 3.1. All 42 gap tests of the Phase 0 audit now pass; the opt-in file
is gone.

## Trends and data tiers

* New `trend_estimate()` and `data_tier()`:
  * **T0**, records only: the raw change in AOO and in alpha-hull EOO, with
    the range from moving the comparison window two years either way
    (start-year sensitivity). A diagnostic.
  * **T1**, with an effort layer of the group: relative occupancy on the
    cells recorded for the group in both windows, with a bootstrap interval.
    An interim proxy.
  * **T2**, repeat visits: a multi-season occupancy-detection model
    (spOccupancy, in Suggests), run when `occupancy_model = TRUE`.
  * **T3**, counts from standardized monitoring projects that AOPK lists: an
    abundance index (basis b).
* The tier's estimate is the reduction indication, its interval the
  plausible range. A continuing decline in EOO or AOO is indicated only when
  the whole interval says so, and is unknown otherwise. The needs list says
  what each tier lacks.
* New `trend_settings()`, the trend conventions, carried in
  `ndopred_config(trend = )`.
* New `ndop_effort()`, `effort_layer()` and `read_effort()`: the recording
  effort of a whole NDOP group as cells and anonymous visits (no names).
  Downloaded year by year, month by month where a year is too large;
  resumable and cached.
* `assess_taxon(effort = )`, `assess_species(effort_dir = )`,
  `batch_assess(effort_dir = )`; the bulk app reads (and optionally
  downloads) effort layers; both apps can fit occupancy models and take a
  list of monitoring projects.
* Rows gain `Data_Tier`, `Trend_Estimate`, `Trend_Source` and
  `Trend_Sensitivity`. The bulk export gains `Úroveň dat (T0-T3)`,
  `Odhad trendu (%)`, `Zdroj trendu` and
  `Citlivost trendu na začátek období`.
* The engine accepts indications as min/best/max ranges too.

## EOO change

* New `calculate_eoo_alpha()`. Change in EOO uses an alpha-hull (alpha 2) on
  occupied 2 x 2 km cells (Guidelines §4.9); the convex hull stays the
  criterion B1 measure. `calculate_trend()` reports both.

## Criterion C and data quality

* `calculate_pop_metrics()`:
  * `total_mature` is the sum of each locality's most recent count, with
    `total_mature_min` (latest year) and `total_mature_max` (per-locality
    maxima), no longer a sum of maxima from different years (GAP-09c);
  * extreme fluctuations are diagnosed only from a consistent-effort series
    (`consistent_effort = TRUE`; T3) and are otherwise unknown (GAP-09d);
  * `decline_estimated` says whether the decline rate is an estimate.
* C1 needs an estimated decline (Guidelines Table 3.1): it is met only from
  evidence or a T3 series, never from opportunistic counts.
* Expert evidence below the minimum quality of its criterion (Table 3.1) is
  documented but not used, with a note.
* The single-taxon app no longer caps past reductions (A2) at 100 years
  (GAP-05c).

# ndopred 0.8.0

Phase 4 of the IUCN alignment (`methodology/gap_analysis.md`): the regional
step of the Guidelines for Application of IUCN Red List Criteria at Regional
and National Levels v4.0 (RG). GAP-12 is closed. GAP-05b, already closed by
the generation-length table of 0.6.0, moves to the regular suite.

## Step one: eligibility

* New `regional_eligibility()` (RG IV.2, Annex 3). Not Applicable (NA):
  * introduced taxa, vagrants, and occasional breeders that regularly become
    regionally extinct;
  * colonizers reproducing for fewer than `colonizer_years` (10);
  * taxa below an optional filter on the share of the global population
    (`regional_filter_share`, off by default).

  Unknown answers lead to assessment, as in the flowchart. Visitors are
  assessed as visiting populations.
* An ineligible taxon's proposal is NA, with the reason, before any
  criterion is applied.

## Step three: adjustment

* New `regional_adjustment()` (RG IV.4, Figure 3, Table 1):
  * breeding populations follow questions 3a-3c, visiting populations 3d-3f;
  * the change is normally one step, as many as the evidence states, noted
    when more than two;
  * EX, EW, RE, DD, NE and NA are never adjusted;
  * an adjusted category carries the degree sign (RG V.3);
  * a visiting taxon listed globally under D is not downlisted.

  Unanswered questions leave the category unchanged.
* New `regional_assessment()` combines both steps around the step-two
  category. It also carries the global category and the share of the global
  population, `?` when unknown (RG V.4).

## Pipeline, checks and outputs

* Evidence fields:
  * `occurrence_status` gains `occasional_breeder`;
  * new `reproduction_since`, `global_population_share` and
    `global_category`;
  * new `regional_step3` (subfields 3a-3f) and `regional_steps`.
* Result rows gain `Regional_Category`, `Regional_Criteria`,
  `Regional_Step`, `Global_Category` and `Global_Population_Share`.
* The bulk export gains `Kategorie (regionální)`, `Kritéria (regionální)`,
  `Regionální krok`, `Globální kategorie` and
  `Podíl globální populace (%)`.
* `final_assessment()` applies the regional step to what the evidence
  supports. A final category set by a documented rescue effect is
  consistent and displayed with its degree sign (`display`). NA is checked
  against step one, and RE should not pre-date 1500 AD. Visitors no longer
  count as NA.
* Species card: a regional-step section.
* Single-taxon app: a regional-step panel (eligibility, questions 3a-3f,
  steps, global category and share) and the regional category in the live
  view.

# ndopred 0.7.0

Phase 3 of the IUCN alignment (`methodology/gap_analysis.md`): the evidence
layer, the final category with its approval, the species card and the
expert app.

## Evidence

* New evidence schema: one table, a row per piece of evidence, with author,
  date, justification, sources, `min`/`best`/`max` and a qualifier.
  `evidence_fields()`, `evidence_template()` (CSV or XLSX), `read_evidence()`
  and `validate_evidence()` (every problem reported with its row),
  `write_evidence()` (a new dated version each time, never overwriting) and
  `latest_evidence()`.
* `evidence_for_engine()` turns evidence into the engine's inputs, record
  flags and config overrides; `apply_record_evidence()` excludes or re-includes
  records by `ID_NALEZ` without deleting anything.
* Rules: locations state their basis, either the whole range or a Guidelines
  §4.11 option, where (a) means the location subcriteria are not met; A1
  needs reversible, understood and ceased causes, otherwise A2; the threat
  must be described in words (§8).
* The engine accepts evidence numbers as ranges, `c(min = , best = , max = )`.
  The proposal uses the best estimate; the plausible range uses the ends.
* `assess_taxon()`, `assess_species()` and `batch_assess()` take an evidence
  table (or file) for any number of taxa. A generation length or current
  window given as evidence adjusts the taxon's config.

## Final category and approval (D9)

* New `final_assessment()`. It checks the assessor's final category against
  what the evidence and measured data support without the record-based
  indications: a threatened category above that (e.g. EN under B with one of
  a/b/c evidenced), a named criterion not met, a category below a threatened
  one the evidence meets, DD, RE, EX/EW and NA checks. The category is final
  only with an approval dated on or after it. Result rows gain
  `Final_Category`, `Final_Claimed`, `Final_Status` and `Final_Issues`.

## Species card

* New `species_card()`: one assessment on one HTML page, covering:
  * the proposal and its range;
  * the final category with approvals and checks;
  * parameters with bounds;
  * every criterion with the rule applied and what decided it;
  * the evidence with its authors;
  * the record IDs used and excluded;
  * open questions and versions.

  NDOP observer names and coordinates are not included.

## Apps

* Single-taxon app:
  * every entry is recorded with the assessor's name, the date and a
    justification and sources per panel;
  * numbers take min/best/max, locations their §4.11 basis, and A1 its
    causes;
  * new **Records** tab: exclude or re-include records by ID, with excluded
    records drawn as red crosses (`plot_iucn(show_excluded = TRUE)`);
  * new **Final category** tab: live checks and approvals;
  * evidence files are loaded (CSV or XLSX) and saved as CSV, or as a new
    version in an evidence folder;
  * species card download.

  The unchecked "Force Category Override" and the "Finalize & Save" button,
  which saved nothing, are gone. The panels no longer redraw while typing.
* Bulk app: optional evidence file; new columns `Kategorie (finální)`,
  `Stav finální kategorie` and `Nesoulad finální kategorie s doklady`.

# ndopred 0.6.0

AOPK decisions of 2026-10-15 on D8 (revised), D9 and generation lengths.

## VEROH 3 is always used (D8, revised)

* Records with `VEROH = 3` (less credible) are never excluded. They are
  flagged (`flag_less_credible`) only when more than 10 % of the taxon's
  records carry 3; at or under 10 % there is no flag. The attribute
  `less_credible_retained` is replaced by `less_credible_flagged`. Taxa with
  a few such records gain them back in AOO, EOO and the counts.

## Generation lengths per taxonomic group

* New `generation_lengths()` and `generation_length_for()`: a table
  (`inst/extdata/generation_lengths.csv`) with one row per NDOP taxonomic
  group, keyed by the group's NDOP code, Latin name and Czech `KAT_TAX`. Every
  group is 1 year for now (`status = "placeholder"`), to be refined. Point
  `NDOPRED_GENERATION_LENGTHS` at another CSV to use other values.
* `assess_species()`, `batch_assess()` and both apps take each taxon's
  generation length from its records' group unless one is given. Result rows
  report `Generation_Length` and `Generation_Length_Source`; the bulk export
  adds `Generační doba (roky)` and `Zdroj generační doby`.
* `ndopred_config(trend_shift = NULL)` now derives the shift from the
  criterion A period, 10 years or three generations, whichever is longer. With
  every group at 1 year the windows stay 2010-2026 against 2000-2016.
  `batch_assess()` leaves `comp_start`/`comp_end` to that rule by default. In
  the bulk app the shift field is a minimum.
* The single-taxon app's generation-length field is empty by default, meaning
  "use the group table".

## Approval (D9)

* Recorded for Phase 3: final categories are approved by group experts,
  conservationists and taxonomists trained in Red List assessment.

# ndopred 0.5.0

AOPK decisions D4-D7 of 2026-10-15 (`methodology/gap_analysis.md`, section 9).
D5 and D7 change the output for most taxa.

## Windows (D5)

* The default current window is the 17 years up to the reference year
  (2010-2026 for 2026), and the trend compares it with 2000-2016.
  `ndopred_config()` replaces `trend_window` with `trend_shift` (default 10):
  the comparison window is the current window moved that many years back.
  `window` now defaults to 17. `batch_assess()` and both apps use the same
  defaults; the bulk app has a field for the shift.
* Overlapping windows no longer warn in `calculate_trend()` and
  `batch_assess()`. They are the agreed design; the vignette describes how the
  shared years damp the measured change.
* The trend now always compares the comparison window with the whole current
  window (`trend_start` = `recent_start`). Previously, explicit comparison
  bounds made the recent side start the year after the comparison window.
* The single-taxon app moves the comparison window back by the criterion A
  period: 10 years, or three generations when longer.
* `plot_iucn()` draws overlapping windows. The blue hull and grid use the
  whole comparison window, the grey points only its years outside the current
  window. Both apps now pass the assessment's windows to the map.

## The proposal is the best estimate (D7)

* Uncertainty no longer moves the proposal. A best estimate of LC whose
  plausible range reaches VU or above stays **LC**; it is no longer proposed as
  NT. Such taxa, and any other proposal whose range holds both a
  non-threatened and a threatened category, are flagged **uncertain**:
  * `categorise()` returns `uncertain`;
  * result rows gain `Uncertain`, and the bulk export gains `Nejistý návrh`
    (`ano`/`ne`);
  * `Flags` / `Příznaky` say which choice Guidelines §10.1 allows, e.g.
    "Uncertain: plausible range LC-EN - NT may be chosen (Guidelines 10.1)".

  NT is still proposed where a criterion is nearly met.
* `ndopred_config()` records the attitude: best estimate, dispute tolerance 0
  (the whole plausible range reported).
* The DD flag (range LC to CR) now cites Guidelines §10.1 and §10.4.

## Recorded, no code change

* D4: NDOP data come only through rndop. Phase 5 rebuilds visits from date,
  site and observer, because rndop exports no visit identifier.
* D6: threats, threat-defined locations and the D2 threat are expert input
  (Phase 3); no threat layer.

# ndopred 0.4.0

Phase 2 of the IUCN alignment (`methodology/gap_analysis.md`): snapshots,
record flags instead of deletion, and AOO/EOO with uncertainty. It
implements the rules AOPK decided on 2026-10-15 (D2, D3, D8, D12, D13).
37 of the 42 Phase 0 gap tests now pass; the remaining five belong to
Phases 4 and 5.

## Snapshots

* New `ndop_snapshot()`, `read_ndop_snapshot()` and `default_snapshot_dir()`.
  A download is kept unchanged, with its points, lines and polygons
  (`rndop::ndop_download(locations = 2)`), the query, download time and
  versions. The default folder is `Documents/ndop_snapshots`.
* `get_assessment_data()` gains `snapshot` (rerun on a stored download) and
  `snapshot_dir` (store this one). The same options are on `assess_species()`
  and `batch_assess()`, and the bulk app stores snapshots by default. The file
  name goes into the new `Snapshot` / `Snapshot NDOP` column.

## Records are flagged, not deleted

* New `flag_records()`. `get_assessment_data()` now returns **every** record,
  with `flag_*` columns, `exclusion_reason`, `include`, `year_min`, `year_max`,
  `precision_m` and `geom_type`; metrics use `include == TRUE`. Attributes
  `exclusions` (counts per reason) and `record_geometries` are added.
* `VEROH` follows the AOPK code list: 6 and 9 are excluded, 0 and 1 used. 3 is
  excluded unless more than 10 % of the taxon's records carry it; then those
  records are kept and flagged (`less_credible_share`, a convention). A
  proposed `VALIDACE` mark on an unguaranteed record is flagged only.
* Taxon names match exactly. The NDOP query for *Bombina bombina* also returns
  *B. bombina × variegata*; those 229 records are now excluded.
* Negative findings are kept as non-detections (18 % of the amphibian records).
* Literature sources and suspected duplicates are flagged.
* Date ranges: a record belongs to a time window only if its whole
  `DATUM_OD`–`DATUM_DO` range does.

## AOO and EOO with uncertainty

* `calculate_aoo()` returns `area_km2_min` and `area_km2_max` from positional
  precision (`CXPRESNOST`, 0 = exact) and NDOP record polygons and lines.
  `calculate_eoo()` returns EOO bounds the same way. The best estimates are
  unchanged.
* The engine uses the bounds for the plausible range: the low end uses the
  largest areas, the high end the smallest. Results and the bulk export gain
  `EOO_km2_min/max`, `AOO_km2_min/max` (`AOO/EOO nový min/max (km2)`),
  `Records_Excluded` (`Záznamů vyřazeno`) and `Důvody vyřazení`.

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

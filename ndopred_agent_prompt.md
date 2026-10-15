# Prompt: make ndopred a fully IUCN-aligned national Red List assessment tool

*Use with a coding agent (e.g. Claude Code) working in a checkout of `jonasgaigr/ndopred`, branch `bulk-assesment`. Attach or copy the review `Algoritmicke_hodnoceni_IUCN_NDOP_reserse_metodika.md` into the repo as `methodology/review_2026-10.md` before starting.*

---

## 1. Your role and the goal

You are working on **ndopred**, an R package (author Jonáš Gaigr, AOPK ČR) that produces **Czech national IUCN Red List assessments** from occurrence data in the Nálezová databáze ochrany přírody (NDOP), downloaded through `rndop`.

The core idea must not change: take the IUCN Red List Categories and Criteria and apply them to NDOP data to produce national assessments, in two modes:

1. **Bulk mode** – evaluate many taxa at once, as far as each criterion can be evaluated automatically, and say clearly how far that is for every taxon.
2. **Single-taxon expert mode** – go through one taxon criterion by criterion; an expert adds what the data cannot show and approves the result.

Your job is to make the package as **usable** as possible for both modes while being **fully aligned with the IUCN criteria**. Where usability and IUCN alignment pull in different directions, IUCN alignment wins, and you say so in the output rather than hiding it.

## 2. Sources of truth, in order of precedence

1. *IUCN Red List Categories and Criteria*, v3.1, 2nd ed. – `methodology/RL-2001-001-2nd.pdf`
2. *Guidelines for Using the IUCN Red List Categories and Criteria*, **v16 (March 2024)** – `methodology/RedListGuidelines.pdf`
3. *Guidelines for Application of IUCN Red List Criteria at Regional and National Levels*, v4.0 (2012). **Not yet in the repo.** If you cannot download it, stop before Phase 4 (regional step) and ask for the PDF to be added to `methodology/`.
4. The review in `methodology/review_2026-10.md` (architecture, data limitations of NDOP, tool comparison).
5. The existing code. It is a starting point, not a specification.

Read the PDFs, not your memory of them. Every threshold, time window and decision rule you implement must carry a comment citing the section of (1)–(3) it comes from (e.g. `# Guidelines v16 §4.11`). Where the Guidelines leave something to judgement, the code must not decide it silently: it becomes either an expert input or a documented, user-configurable convention.

## 3. Non-negotiable principles

- **Calculated ≠ evidenced ≠ categorised ≠ approved.** Keep four separate things: (i) a calculated parameter (e.g. AOO = 84 km²), (ii) whether a sub-criterion condition is satisfied, with its evidence, (iii) a provisional category, (iv) an expert-approved national category. The software may produce (i)–(iii); only an expert produces (iv).
- **Three-valued logic everywhere.** Every condition is `TRUE`, `FALSE` or `UNKNOWN`. `UNKNOWN` is never coerced to `FALSE`, and missing data never yields `LC`. A taxon whose B2 threshold is met at EN but whose (a), (b), (c) are unknown is "potentially EN – needs evidence for B(a)/(b)/(c)", not LC.
- **Parameters carry uncertainty.** Store numeric parameters as `min / best / max` (Guidelines §3.2). Propagate them to a *plausible range of categories*, with the dispute/risk-tolerance logic of §3.2.3 configurable, and report the range alongside the best-estimate category.
- **NDOP data are never edited.** The occurrence snapshot is immutable. Expert input is stored in a separate, versioned evidence layer keyed by taxon and, where relevant, by NDOP record ID. Excluding a record means adding a flag with a reason and an author, never deleting it.
- **One rule engine.** Bulk mode, single-taxon mode, the Shiny apps and `batch_assess()` all call the same engine. No UI file may reimplement thresholds or category logic.
- **Reproducibility.** Every run is defined by a configuration (taxa, NDOP snapshot ID or download timestamp, assessment reference year, time windows, filters, conventions, package and Guidelines version). No `Sys.Date()` inside calculations; the reference year is an explicit argument.
- **Highest category wins, all criteria are reported.** The final category is the highest supported by any criterion; all criteria met at that category are listed in standard IUCN notation (see §7).

## 4. What I found in the current branch (start here)

These are systemic problems, not one-off bugs. Confirm each in the code before changing it, and write a failing test for each before fixing it.

| # | Where | Problem | IUCN basis |
|---|---|---|---|
| 1 | `summarize_assessment()` vs `expert_final()` in `inst/shinyapp/app.R`; window logic in `R/app_bulk.R` | Three separate implementations of the category logic that already diverge (e.g. C thresholds, NT handling, D2). | – |
| 2 | `calculate_locations()` → B(a), D2 | Distance clusters (5 km) are used directly as IUCN *locations*, so B(a) is met automatically. Locations are threat-defined; with no plausible threat the location sub-criteria are not met. | Guidelines §4.11 |
| 3 | `summarize_assessment()` B(b) | Any negative change in AOO between two windows sets b(i), b(ii) **and** b(iii) at once. `app_bulk.R` then strips "iii" with a regex. Habitat-quality decline (iii) cannot be inferred from AOO at all. | §4.6, §4.10.8 |
| 4 | D2 | VU D2 is assigned whenever AOO < 20 km² or clusters ≤ 5, with no plausible threat. D2 requires a specified plausible threat that could drive the taxon to CR/EX in a very short time; if that is lacking, consider NT. | §8, §8.3 |
| 5 | `calculate_trend()`, A2c | A2c comes from a naive comparison of observed AOO in two windows, uncorrected for recording effort. Default windows in `batch_assess()` overlap (2000–2016 vs 2010–2026). The window is not tied to generation length in bulk mode. | §4.4, §4.5, §5, §4.10.8; review §7 |
| 6 | RE pre-check | "Last record > 50 years ago ⇒ RE" is not an IUCN rule. RE (and EX) require no reasonable doubt that the last individual has died, based on adequate surveys. | §11, §11.3; Regional Guidelines |
| 7 | DD pre-check and catch-all | `n_records < 3` or absence from the recent window ⇒ DD. DD requires that all five criteria were considered and documented; few records can mean CR (cf. §8.1, type-locality taxa). | §10.3, §10.4, §8.1 |
| 8 | everywhere | Missing values collapse to `LC` (e.g. `get_a_cat(NA)` returns `"LC"`). | §3.2 |
| 9 | Criterion C | C1 has no time frame (25% in 3 y/1 gen, 20% in 5 y/2 gen, 10% in 10 y/3 gen, max 100 y); C2a(ii) uses 95% for all categories instead of 90/95/100%; NDOP `LOKALITA` is treated as a subpopulation; total mature individuals sums per-locality maxima from different years. | Criteria v3.1; §4.2, §4.3, §7 |
| 10 | AOO | Records are points; polygon records and positional precision are ignored. The bulk app lets the user change the cell size, but thresholds apply only to AOO at 2×2 km. Observed AOO is reported as if it were the AOO, though for under-recorded taxa it is a lower bound. | §4.10.3, §4.10.5, §4.10.8 |
| 11 | `get_assessment_data()` | Records are deleted rather than flagged (missing KATASTR, VEROH 3/9, negative findings). Negative findings are valuable non-detections for trend models. | review §2, §4 |
| 12 | regional step | Regional Guidelines not implemented: taxon eligibility (native, established, not vagrant → NA), regional adjustment (rescue effect / sink). | Regional Guidelines v4.0 |
| 13 | notation | Criteria strings are not in IUCN notation (`B1ab(i,ii,iii); B2ab(...)`, `"B2 (close)"`). | Criteria v3.1 |
| 14 | signatures | `app.R` calls `calculate_trend(window_years = …)` and `plot_iucn(window = …)`, which no longer exist; README examples are stale; README/vignette cite Guidelines v15.1 though v16 is in the repo. | – |
| 15 | bulk output | Known symptom: partially blank rows (taxa assessed but with no AOO/EOO). `safe_metric()` and `tryCatch()` swallow errors and return `NA` or `0`. Every empty value must instead carry a reason code. | – |

Keep what is already right: AOO by `floor()` binning with the minimum over grid origins (§4.10.2 says the minimum estimate should be used), `reconcile_eoo_aoo()` (§4.9), and the `filter_counts` diagnostics in `get_assessment_data()`.

## 5. Target architecture

Follow the layered design from the review (§4), as R functions with clear, tested contracts:

1. **Snapshot** – `ndop_snapshot()`: download via `rndop`, store raw records unchanged with download timestamp, taxon checklist version and query parameters (Parquet or RDS under a run directory).
2. **Validation flags** – `flag_records()`: taxonomy, coordinates (CoordinateCleaner-style tests), dates and date precision, VEROH, negative findings, duplicates of literature/collection records, nativeness, positional precision and geometry type. Output: one row per record with boolean flags and `exclusion_reason`; nothing deleted. Filters are applied downstream according to the run configuration.
3. **Events layer** – where NDOP allows it, separate records, visits/survey events and complete lists. First verify which NDOP fields actually exist (the review's field names are hypothetical); produce a data dictionary from a real export.
4. **Spatial metrics** – EOO, AOO, cluster diagnostics, each with `min / best / max` scenarios (e.g. with and without uncertain records; precision-aware cell assignment), plus per-metric `issue_*` columns explaining any `NA`.
5. **Trend module** – classify each taxon's data into tiers T0–T3 (review Table 5). Only T2/T3 data may feed a reduction estimate; T0/T1 produce diagnostics labelled as such. For T2, implement or interface an occupancy/detection model; an effort-corrected AOO-change indicator is acceptable as an explicitly labelled interim proxy.
6. **Evidence layer** – the expert inputs described in §6, validated against a schema.
7. **Rule engine** – `assess_criteria()`: evaluates A, B, C, D, E separately and at each threshold level, returning a tidy evidence table (one row per criterion × sub-criterion × level).
8. **Regional step** – eligibility before assessment; regional adjustment after it, both recorded.
9. **Outputs** – see §7.

## 6. Expert input: what it may specify (and what it may not)

Expert input **cannot replace NDOP data**. It can add what the data cannot show, as structured, attributed evidence (author, date, justification, sources, and for numbers `min/best/max`). Provide CSV/XLSX templates so specialists can fill evidence offline, and import/validate them.

Allowed inputs:

- Taxon eligibility: native/introduced, established/vagrant, taxonomic concept and synonym mapping.
- Record-level flags by NDOP record ID: misidentification, introduction, vagrant, duplicate, imprecise – each with reason.
- Generation length (drives A, C1, E windows; §4.4) and the definition of "current" occurrence for the taxon group.
- Plausible threats and the **threat-defined number of locations**, using one of the options in §4.11 when parts of the range are unaffected, with the basis documented.
- Severe fragmentation: `TRUE/FALSE/UNKNOWN` with basis (§4.8).
- Continuing decline per B(b)(i)–(v) and C, each with basis: observed / estimated / inferred / projected (§3.1, §4.6).
- Extreme fluctuations per B(c)(i)–(iv) (§4.7).
- Reduction estimates from external monitoring: type A1–A4, basis (a)–(e), and for A1 whether causes are reversible, understood and ceased (§5).
- Mature individuals, subpopulations, proportion in the largest subpopulation (§4.1–4.3).
- D2 threat scenario (§8.3); criterion E results with documentation (§9.6).
- Survey evidence for RE/possibly extinct candidates (§11.3).
- Regional adjustment: immigration potential, rescue effect, sink status.
- A final category that departs from the engine's result **only** with a mandatory justification; the engine re-checks it against the documented evidence and flags inconsistencies (e.g. EN claimed under B with only one of a/b/c evidenced).

Expert inputs never overwrite computed values: both are stored and shown side by side, and the species card shows which one decided each condition.

## 7. Criterion rules to implement (verify every number against the PDFs)

- **A**: thresholds A1 ≥90/70/50%, A2–A4 ≥80/50/30%; window = 10 years or 3 generations, whichever is longer, with the 100-year cap only where the criteria impose it (A3, A4). Basis (c) from AOO/EOO/habitat decline requires a documented link to population reduction (§5.8). Never infer A2(b) from occupancy alone. Report sensitivity to start year (review §7.3).
- **B**: B1 EOO <100 / 5 000 / 20 000 km²; B2 AOO <10 / 500 / 2 000 km² at 2×2 km; **plus at least two of** (a) severe fragmentation or locations =1 / ≤5 / ≤10, (b) continuing decline in any of (i)–(v), (c) extreme fluctuations in any of (i)–(iv). EOO by minimum convex polygon over records in the assessed region, set equal to AOO if smaller (§4.9). Report observed AOO as a lower bound and, where data allow, an estimated AOO (e.g. habitat-masked × prevalence, review §6.2; §4.10.7–4.10.8).
- **C**: <250 / 2 500 / 10 000 mature individuals, with C1 (rates and time frames above) or C2 (continuing decline plus a(i) ≤50 / 250 / 1 000 in each subpopulation, a(ii) 90 / 95 / 100% in one subpopulation, or b extreme fluctuations).
- **D**: D1 <50 / 250 / 1 000 mature individuals; D2 (VU only) restricted AOO (typically <20 km²) or locations (typically ≤5) **and** a specified plausible threat.
- **E**: only from a documented quantitative analysis supplied as evidence.
- **NT**: produce NT only as a *candidate* with the reason (close to thresholds, e.g. area threshold met with one of a/b/c) and require expert confirmation (§10.1). Any numeric "near" margin (e.g. 20% for A) is a configurable convention, labelled as such.
- **DD / NE**: DD only through expert decision after all criteria are documented (§10.3–10.4); the engine may suggest "DD candidate" when the plausible range spans threatened to LC. Taxa not run through all criteria are NE, not DD.
- **RE / CR(PE)**: the engine may raise "RE candidate" (e.g. no records for a configurable period *and* survey effort evidence); assignment requires expert confirmation and survey evidence (§11).
- **Notation**: produce standard criteria strings, e.g. `EN B1ab(ii,iii)+2ab(ii,iii)`, `VU A2c; B2ab(iii)`, `VU D2`. Write tests against worked examples from the Guidelines.

## 8. The two modes

**Bulk mode** (`batch_assess()`, `R/app_bulk.R`)
- Runs from a configuration file; parallel and resumable (cache snapshots per taxon; one failure never aborts the run).
- Output per taxon: computed parameters with `min/best/max`; the per-criterion evidence table; best-estimate provisional category and plausible range; **what is missing** to close the assessment (e.g. "needs: threat-defined locations, evidence of continuing decline"); data tier T0–T3; `issue_*`/reason codes for every empty field; triage priority for the expert queue (e.g. taxa meeting CR/EN area thresholds first). Rapid-LC-style triage may label clearly widespread taxa as "LC candidate" but never as LC (review §3).
- Keep the Czech column names the AOPK workflow already uses (`Kategorie (automatická)`, `AOO nový (km2)`, `Karta druhu` …), add new columns rather than renaming, and write a data dictionary for the export.

**Single-taxon expert mode** (`inst/shinyapp/app.R`)
- Panels per criterion showing computed values, their uncertainty, the rule applied and its Guidelines section, and evidence forms from §6.
- A live preview of how each piece of evidence changes the condition states and the category.
- Map with record flags (toggle excluded/uncertain records to see their effect, like GeoCAT), AOO grid at the chosen origin, EOO hull, and clusters labelled as clusters, not locations.
- Save/load the evidence file; export a species card (HTML/PDF) with record IDs used, parameters, rules applied, evidence, author, versions, and open questions (review §8).

## 9. Borrow from existing tools (adapt, don't depend blindly)

Check the current state of each tool before relying on it (the review notes ConR is distributed from GitHub, not CRAN; redlistr 2.1.0 uses sf/terra).

- **ConR** – batch multispecies workflow; locations computed against a threat layer rather than fixed clusters; diagnostic columns explaining missing metrics. Use as a **benchmark**: add a script that runs ConR on the pilot taxa and compares EOO/AOO/locations/category with ndopred.
- **redlistr** – AOO grid-uncertainty and EOO functions for cross-checking spatial metrics in tests.
- **red** – bootstrap uncertainty of EOO/AOO from positional error.
- **sRedList** – stepwise, documented workflow; parameter fields with min/best/max; outputs structured for the IUCN Species Information Service. Mirror its field structure in the export so assessments can move into SIS-compatible formats later.
- **GeoCAT** – interactive inclusion/exclusion of points with immediate recalculation.
- **RAMAS Red List / Akçakaya et al. 2000** – fuzzy parameters, plausible category ranges, dispute and risk tolerance.
- **IUCNN and other ML predictors** – optional, later, as a prioritisation "red flag" only; never a category source (review §3, §10).

## 10. Testing and validation

- Unit tests for every threshold boundary (`<` vs `≤`), every `UNKNOWN` path and every criteria-string format.
- Golden tests from worked examples in the Guidelines (e.g. §8.2–8.3) and the review's hypothetical taxa X and Y (Table 6): identical EOO/AOO, different evidence, different outcomes.
- Regression tests for each item in §4 of this prompt.
- Property tests: adding records never decreases observed AOO; excluding a record never increases it; changing `UNKNOWN` evidence to `TRUE` never lowers the category.
- A pilot validation script for two insect groups (one well recorded, one poorly recorded), reporting false threatened and false non-threatened separately against blinded expert assessments, plus stability under alternative filters (review §9).

## 11. How to work

1. **Phase 0 – audit (no code changes).** Read the code, the three IUCN documents and the review. Inspect a real NDOP export via `rndop` and write the data dictionary. Deliver `methodology/gap_analysis.md`: confirmed problems (with tests that demonstrate them), what is already correct, and a phased plan. **Stop and wait for approval.**
2. **Phase 1** – single rule engine with three-valued logic, evidence table, IUCN notation; both apps and `batch_assess()` switched to it. Fixes for §4 items 1, 3, 4, 6, 7, 8, 13, 14, 15.
3. **Phase 2** – snapshot, flags and spatial module with uncertainty; precision-aware AOO; locations as expert input with clusters as diagnostics only (items 2, 10, 11).
4. **Phase 3** – evidence layer, templates, expert-mode UI, species card.
5. **Phase 4** – regional step (needs the Regional Guidelines PDF) (item 12).
6. **Phase 5** – trend module with data tiers; criterion C corrections (items 5, 9).
7. **Phase 6** – ConR benchmark, pilot validation scripts, documentation.

Work in small, reviewable commits per phase, keep `R CMD check` clean, update the README, vignette and NEWS as behaviour changes, and bump the version.

## 12. Decisions you must not make alone — ask

- Which NDOP fields exist for events, effort, precision and geometry, and whether events can be linked to complete species lists.
- The definition of "current" occurrence per taxon group, and the default comparison windows.
- The source and maintenance of a threat layer (if any) for locations.
- Default dispute and risk tolerance for the national list.
- Whether VEROH 3/9 and literature/collection records are excluded by default or only flagged.
- Who approves eligibility, regional adjustment and final categories, and how approvals are recorded.
- Any change to existing export columns used downstream at AOPK.

## 13. Definition of done

- Bulk and single-taxon modes give identical results for identical inputs.
- No category is produced from missing data; every empty output field has a reason code.
- Every implemented rule cites its IUCN section, and every number in the engine has a test.
- Expert evidence is stored separately from NDOP data, attributed, versioned and round-trips through the templates.
- A species card and a bulk export can be regenerated exactly from the run configuration.

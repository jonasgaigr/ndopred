# ndopred: Phase 0 gap analysis against the IUCN Red List standard

| | |
|---|---|
| Date | 2026-10-15 |
| Branch | `bulk-assesment`: HEAD `6af2b2b` plus an uncommitted working tree (39 modified and 15 new files, about 3,800 changed lines) |
| Scope | Audit only. No package code was changed. Added: this file, [`ndop_data_dictionary.md`](ndop_data_dictionary.md), [`review_2026-10.md`](review_2026-10.md) (a copy of the review), and the opt-in failing tests in [`tests/testthat/test-gap-phase0.R`](../tests/testthat/test-gap-phase0.R) |
| Status | **Approved 2026-10-15** with decisions D1, D10 and D11 (§9). Baseline committed as `ec341f9`. Phase 1 (0.3.0) and Phase 2 (0.4.0) done; decisions D4–D7 implemented (0.5.0), D8 revised, generation-length table and D9 recorded (0.6.0); Phase 3 done (0.7.0); Phase 4 done (0.8.0); Phase 5 done (0.9.0); Phase 6 done (0.10.0); decisions D14–D17 implemented (0.11.0) |

## 1. Summary

- **Every item in the prompt's §4 table is confirmed.** Five (3, 4, 5, 9, 14) are partly fixed in the uncommitted working tree; the rest stand as described. The audit found **18 further problems** (§5), most of them visible only in real NDOP exports.
- **42 failing tests demonstrate the gaps** ([`test-gap-phase0.R`](../tests/testthat/test-gap-phase0.R)). Each asserts the IUCN-aligned behaviour, and all 42 fail on the current code for the intended reason (none errors). They are opt-in, so the existing suite stays green: 177 tests pass, 0 fail.
- **The root problem is evidential, not arithmetic.** Thresholds are mostly right. What goes wrong is that artefacts of opportunistic recording are promoted to evidence (5 km clusters become *locations*; a drop in records becomes *continuing decline* and an *A2c reduction*), and that missing information is silently treated as FALSE, which lands on LC, DD or RE.
- **The clearest single demonstration:** a synthetic taxon whose range is identical in both windows, recorded five times less often in the recent one, comes out of `assess_species()` as **`EN A2c; B1ab(i,ii); B2ab(i,ii)`**.
- **The two modes already disagree.** On identical inputs the expert panel shows **CR** where the engine says **EN** (GAP-01a). For the same window the bulk app reports **AOO 16 km²** where `assess_species()` reports **12 km²** (GAP-16).
- **Real exports carry more than ndopred reads.** Positional precision, record polygons, validation verdicts, date ranges and negative findings are all present. ndopred deletes or ignores them (see §7 and the data dictionary).
- **The Regional Guidelines v4.0 are in the repo** (`methodology/RL-2012-002.pdf`), so Phase 4 is not blocked.
- **Three of the thirteen decisions are made** (§9). The baseline is committed. Bulk mode keeps proposing a category with the criteria behind it, in today's format plus a new Annex 2 column. What still needs evidence is reported alongside the proposal, not instead of it. The other ten decisions block later phases only.

## 2. What was audited, and how

**Code.** Every file in `R/`, `inst/shinyapp/app.R`, the tests, README, vignette and NEWS. The prompt's §4 table describes HEAD (`6af2b2b`). The working tree contains a large uncommitted revision that already addresses part of it, so each item below records both states. Line references are to the working tree.

**Sources.** All three were read from the PDFs, not from memory:

| Document | File | Sections used |
|---|---|---|
| IUCN Red List Categories and Criteria v3.1, 2nd ed. | `RL-2001-001-2nd.pdf` | III Definitions, IV Categories, V Criteria, Annex 1 (uncertainty), Annex 2 (notation) |
| Guidelines for Using the IUCN Red List Categories and Criteria **v16 (March 2024)** | `RedListGuidelines.pdf` | 2.2–2.5, 3.1–3.2, 4.1–4.11, 5–5.8, 6, 7, 8–8.3, 9.6, 10.1–10.4, 11.1–11.3 |
| Guidelines for Application of IUCN Red List Criteria at Regional and National Levels **v4.0** | `RL-2012-002.pdf` | II Preamble, III Definitions, IV.1–IV.4 (with Fig. 3, Table 1), V |
| Review: *Automatizované hodnocení ohroženosti druhů podle IUCN* (2026-10-13) | `review_2026-10.md` | All sections |

**Tests.** The existing suite was run as a baseline (177 pass). The gap tests were then written and run with `NDOPRED_GAP_TESTS=true` (42 fail, 0 error, 0 pass). §10 explains how to run them.

**Real data.** `login.nature.cz` did not answer for the first 15 minutes of the audit, so the data dictionary is built mainly from three cached exports on this machine: Scarabaeidae (46,144 records), Annex II amphibians with geometries (60,331 records, 32,342 localisations), and an AOPK internal export for *Phengaris*. When the login came back, a live export of *Onthophagus medius* (25 records, with geometries) confirmed the same 45-column schema.

## 3. Rules extracted from the sources

This is the rule set the Phase 1 engine will implement. Every rule will carry this citation in code.

| Rule | Source | Current code |
|---|---|---|
| A thresholds: A1 ≥90/70/50 %; A2–A4 ≥80/50/30 % | Criteria v3.1 V; Guidelines §5 | Correct (`summarize_assessment.R:327-343`), but re-typed in `app.R:316-320` |
| A window: longer of 10 years or 3 generations. **No cap for past reductions (A1, A2)**; 100-year cap only for future projections (A3, A4) | Criteria v3.1 V A1–A4; Guidelines §5.2 | `app.R:53` caps every A type at 100 years; bulk and batch ignore generation length |
| A basis (a)–(e) must be specified; (a) not allowed for A3; minimum quality "suspected" | Criteria A; Guidelines §5.1, Tables 3.1 and 5.1 | Basis inferred from which metric declined; no quality qualifier |
| A from habitat/AOO/EOO decline needs a documented habitat-to-population relationship | Guidelines §5.8 | Not documented; raw AOO change is used directly as the reduction |
| B: area threshold (EOO <100/5,000/20,000; AOO <10/500/2,000 km²) **and at least two of** (a), (b), (c) | Criteria v3.1 V B; Guidelines §6 | Correct structure (`summarize_assessment.R:276-318`) |
| B(a): severely fragmented **or** locations ≤1/5/10; document the two separately | Criteria B; Guidelines §6, §4.8, §4.11 | Cluster count stands in for locations |
| Locations are threat-defined; with no plausible threat the location subcriteria are **not met**; options (a)–(d) when part of the range is unaffected | Guidelines §4.11 | Not implemented |
| Continuing decline: any rate for B and C2; projected decline only with "high degree of certainty"; minimum quality "inferred" for B(b) and C2 | Guidelines §4.6, Table 3.1 | Raw two-window change, 5 % noise tolerance, no qualifier |
| Extreme fluctuation: ≥10-fold, recurring increases and decreases, must reflect the total population | Criteria III.7; Guidelines §4.7 | Ratio + reversals on yearly totals of opportunistic counts |
| EOO: minimum convex polygon; no exclusions for B1; EOO ≥ AOO; **α-hull with a fixed discontinuity distance for temporal comparisons** | Guidelines §4.9 | MCP and reconciliation correct; MCP also used for temporal comparison |
| AOO: 2×2 km grid; minimum over grid origins; observed AOO is a **plausible lower bound**; the upper bound comes from potential habitat; DD if the bounds span LC–CR | Guidelines §4.10.2, §4.10.3, §4.10.8 | Grid and minimum correct; no bounds; bulk pins the origin |
| C: <250/2,500/10,000 mature individuals; C1 25 %/20 %/10 % within 3 y or 1 gen / 5 y or 2 gen / 10 y or 3 gen, **max 100 years**; C1 minimum quality "estimated" | Criteria V C; Guidelines §4.6, §7, Table 3.1 | Periods correct, no 100-year cap, no qualifier |
| C2a(i) no subpopulation >50/250/1,000; C2a(ii) ≥90 %/95 %/100 % in one subpopulation | Criteria V C2 | Thresholds correct; subpopulation = NDOP locality |
| D: CR **D** <50, EN **D** <250, VU D1 <1,000; VU D2 needs a specified plausible threat able to drive the taxon to CR/EX within 1–2 generations (or 3–5 years); if threats could only make it VU/EN, consider NT | Criteria V D; Guidelines §8, §8.3 | Gated on `d2_threat`, but notation is `D1` under CR/EN |
| E only from a documented quantitative analysis | Criteria V E; Guidelines §9.6 | App dropdown, no documentation |
| Highest category wins; list every criterion met at that category | Criteria Annex 2; Guidelines §2.3, §2.5 | Correct in principle |
| Notation: `+` between numbered subcriteria of one criterion (`B1ab(iii)+2ab(iii)`, `C1+2a(ii)`, `D1+2`); `;` between criteria; no punctuation between letters; roman numerals in parentheses, comma-separated | Criteria Annex 2 | `B1ab(ii); B2ab(ii)`, `C2a(i),a(ii)`, `D1; D2` |
| Uncertainty: best estimate plus plausible range; dispute tolerance (recommended 0) and risk tolerance ("precautionary but realistic", 0.40–0.49 suggested for global listings; national authorities set their own) | Criteria Annex 1; Guidelines §3.2.2–3.2.5 | Point estimates only |
| NT: close to VU, or the plausible range contains LC and VU and the best estimate is not VU; justify with the criteria nearly met; conservation dependence no longer qualifies | Guidelines §10.1 | Assigned as a final category |
| DD only if CR and LC are both plausible **and** all five criteria are documented; otherwise NE | Guidelines §10.3, §10.4 | Pre-checks assign DD |
| EX/CR(PE): P(E) ≥0.9 / 0.5–0.9 from the Threats and Records-and-Surveys models | Guidelines §11.3 | Not implemented |
| Regional: assess only wild populations within their natural range (colonists after ≥10 consecutive years of reproduction); vagrants and introduced taxa are NA; step 3 up/down-listing from Fig. 3 and Table 1; no adjustment of EX, EW, RE, DD, NE, NA; mark adjusted categories with ° | Regional Guidelines IV.2–IV.4 | Not implemented |
| RE: "no reasonable doubt" that the last potentially reproducing individual has gone; any time limit is the regional authority's documented choice | Regional Guidelines III.14, IV.3 | `last record > 50 years ⇒ RE` |

## 4. The fifteen items from the prompt

| # | Problem | At HEAD | In working tree | Gap tests |
|---|---|---|---|---|
| 1 | Three diverging category implementations | confirmed | **confirmed** | 01a, 01b (also 16) |
| 2 | 5 km clusters used as locations | confirmed | **confirmed** (documented as a surrogate, still feeds B(a), D2, B(b)(iv) and NT) | 02, 04d |
| 3 | B(b) asserted from a raw AOO change | confirmed exactly | **partly fixed**: (iii) no longer inferred, regex removed; raw AOO/MCP-EOO change still asserted as (i)/(ii) | 03 |
| 4 | D2 without a plausible threat | confirmed | **partly fixed**: requires `d2_threat = TRUE`; unknown threat still gives LC, the threat is a bare logical, clusters count as locations | 04a–d |
| 5 | Naive A2c; overlapping windows; window not tied to generation length | confirmed | windows no longer overlap; **rest confirmed**; also a 100-year cap on past reductions | 05a–c |
| 6 | "Last record >50 years ⇒ RE" | confirmed | **confirmed** (provenance documented, still returns RE) | 06 |
| 7 | DD pre-check and catch-all | confirmed | **confirmed** | 07a–c |
| 8 | Missing values collapse to LC | confirmed | **confirmed** | 08a–b, 04a–b |
| 9 | Criterion C defects | confirmed | **partly fixed**: C1 periods and C2a(ii) 90/95/100 correct; no 100-year cap, locality = subpopulation, sum of maxima across years | 09a–d |
| 10 | AOO from points only; precision ignored; cell-size option; observed AOO not a lower bound | confirmed | **confirmed** (non-2 km grid now warns but is still compared with the thresholds) | 10a–d |
| 11 | Records deleted, not flagged | confirmed | **confirmed**, plus more fields ignored (§5) | 11a–e |
| 12 | No regional step | confirmed | **confirmed** (now unblocked) | 12 |
| 13 | Non-standard notation | confirmed (`"B2 (close)"`) | **confirmed** | 13a–e |
| 14 | Stale signatures and docs | confirmed | **mostly fixed**: signatures, tests and v16 citations correct; README install path still `yourusername` | 14 |
| 15 | Blank bulk rows; swallowed errors | confirmed | **confirmed** | 15a–c |

### Details

**1. Three implementations.** `inst/shinyapp/app.R::expert_final()` (lines 284–431) re-implements A, B, C, D, the RE screen (hard-coded 50 years, line 297) and NT. `R/app_bulk.R` computes its own windows (116–121), grid origin (228–235) and percentage changes, then calls the engine. `batch_assess()` returns a different 7-column schema from the bulk app's 21 Czech columns. **Demonstrated divergence (GAP-01a):** with population inputs of 200 mature individuals, 100 in the largest locality and −30 % over 10 years, the engine gives **EN** (C2a(i): 100 ≤ 250 at EN) and the expert panel, seeded from the engine and untouched, shows **CR**. Its `eval_c()` (line 376) applies the seeded a(i) box at every level, ignoring the 50/250/1,000 caps.

**2. Clusters as locations.** `calculate_locations()` returns the number of 5 km connected components (`calculate_locations.R:100`). Its documentation now calls this a surrogate, but every caller passes it as `locations`. B(a) is then met automatically (`summarize_assessment.R:291-292`), it drives the D2 candidate flag (489–490), its change between windows asserts B(b)(iv) (264), and it produces NT through "area plus one condition". GAP-02: three clusters, EOO 4,000 km², AOO 400 km², −10 % AOO gives **`EN B1ab(ii); B2ab(ii)`**. Guidelines §4.11 makes locations threat-defined, and without a plausible threat they "will not be met".

**3. B(b) from AOO.** At HEAD a negative AOO change set (i), (ii) and (iii), and `app_bulk.R` stripped `iii` with regex. The working tree drives (i) from MCP-EOO change and (ii) from AOO change, and never infers (iii). What remains is the core issue: a difference between two windows of opportunistic records is asserted as an observed continuing decline. Guidelines §4.10.8 says AOO depends on sampling effort; §4.9 says MCP is "less suitable" for temporal comparisons (use an α-hull); §4.6 requires the decline to be liable to continue; Table 3.1 sets "inferred" as the minimum quality. GAP-03: the effort-drop taxon gets **b(i,ii)**.

**4. D2.** Fixed in direction: VU D2 now needs `d2_threat = TRUE`. Remaining problems:
- an unknown threat is FALSE, so `assess_criterion_d2(10, 3)` returns **LC** and a restricted taxon with no threat information is **LC** (GAP-04a, 04b);
- `d2_threat = TRUE` with no stated threat gives **VU D2**, although §8 says the events "must be specified" (GAP-04c);
- the candidate test uses cluster counts, so AOO 800 km² with four clusters and `d2_threat = TRUE` gives **VU D2** (GAP-04d);
- the closing advice of §8, to consider NT when threats could only make the taxon VU or EN, is not represented.

**5. Criterion A data and windows.**
- *Naive comparison.* The effort-drop taxon gets **A2c at EN** (GAP-05a). Review §7.1 and Table 5 classify such data as T0/T1: diagnostics, not reductions.
- *Overlapping defaults.* Fixed in the working tree (now `recent_end-9 … recent_end` against the 10 years before).
- *Generation length.* The bulk app has no input for it. `batch_assess()` accepts `generation_length`, but it only scales C1 and never the A window: the windows are identical for GL 1 and GL 10 (GAP-05b).
- *100-year cap.* `app.R:53` caps the A window at 100 years for all A types, but Guidelines §5.2 says "a maximum time cap is not applied to assessments based on past reductions". With GL 40 the window is 100, not 120 (GAP-05c).
- *Sensitivity to start year* (review §7.3) is not reported.

**6. RE.** `summarize_assessment.R:200` still returns category **RE** when the last record is more than 50 years old. The comment documents that this is not an IUCN rule, but the output is still a category. `batch_assess("Ghost sp")` returns **RE**. Regional Guidelines III.14 and IV.3 tie RE to search effort, and Guidelines §11.3 provides the probability models. The engine may raise an *RE candidate*; it must not assign RE (GAP-06).

**7. DD.**
- `n_records < 3` or a missing AOO gives **DD** (`summarize_assessment.R:215`); a taxon absent from the recent window falls into the **DD** catch-all (500–504).
- `assess_species()` returns **DD** for a taxon with *no records at all* (`assess_criteria.R:135`), and the bulk app does the same.
- `assess_criterion_a(NA)` returns "DD", although a criterion cannot be Data Deficient.

Guidelines §10.4: DD requires all five criteria to be documented, otherwise the category is NE. §8.1: very few records can mean CR. (GAP-07a–c.)

**8. Missing → LC.**
- `get_a_cat(NA)` returns "LC" (`summarize_assessment.R:328`).
- `severely_fragmented`, `habitat_decline` and `d2_threat` default to FALSE (127–129).
- `locations = NA` makes (a) FALSE.

So a widespread taxon with *no* trend information is **LC** (GAP-08a), and the review's taxon Y (EOO 1,700 km², AOO 84 km², nothing evidenced) is **LC "None"** (GAP-08b). The review requires three-valued logic, with UNKNOWN never treated as FALSE.

**9. Criterion C.** Fixed in the working tree: C1 compares a decline re-projected onto each category's own period, and C2a(ii) uses 90/95/100 %. Remaining problems:
- there is no 100-year cap on the C1 period: with GL 40, N = 5,000 and −1 % per decade the result is **VU C1**, although on the capped 100-year period the decline is 9.6 %, below 10 % (GAP-09a);
- `max_subpop` is the largest *locality* count, so 240 adults spread over localities of ≤40 gives **CR C2a(i)** (GAP-09b, Guidelines §4.2);
- `total_mature` sums each locality's maximum across different years: sites with 100 in 2016 and 10 + 10 in 2025 give **110** instead of 20 (GAP-09c, `calculate_pop_metrics.R:186`);
- the extreme-fluctuation test fires when *survey coverage* alternates, with three sites in odd years and one in even years (GAP-09d, Guidelines §4.7);
- C1's minimum quality ("estimated") is not enforced.

**10. AOO.**
- `get_assessment_data()` builds points from `X`/`Y` only (`get_assessment_data.R:105`) and never asks rndop for record geometries (GAP-10b).
- `CXPRESNOST` is not read, so a record with 3 km precision gives the same AOO as one with 1 m (GAP-10a).
- The bulk app's 5 km grid now logs a warning, but its AOO is still compared with the B2 thresholds and a category is written (GAP-10c).
- Observed AOO is reported as *the* AOO, with no lower or upper bound (GAP-10d, §4.10.8).

**11. Records deleted.** `get_assessment_data.R:82-85` deletes records without coordinates (including hidden localities), negative findings, records with a blank cadastral area, and `VEROH` 3/9. Of seven fixture records, five survive and nothing is flagged (GAP-11a). Negative findings are lost (GAP-11b). The audit also found that several relevant fields are never read: `VALIDACE = "chybný záznam"` (GAP-11c), hybrid records pulled in by the name query (GAP-11d), and date ranges (GAP-11e). See §5 and the data dictionary.

**12. Regional step.** There is no eligibility check (native, wild, established, vagrant → NA), no regional adjustment (rescue effect, sink), no NA category, no ° mark, and no fields for the global category or the share of the global population (Regional Guidelines IV.2–V.4) (GAP-12).

**13. Notation.** Output is `B1ab(ii); B2ab(ii)` instead of `B1ab(ii)+2ab(ii)` (GAP-13a), and the review's taxon X gives `EN B1ab(iii); B2ab(iii)` (GAP-13b). CR and EN under D are written `D1` instead of `D` (GAP-13c; Annex 2 "CR D", Guidelines §8.2 "Critically Endangered: D"). The engine also writes `C2a(i),a(ii)` instead of `C2a(i,ii)` (GAP-13d) and `D1; D2` instead of `D1+2` (GAP-13e).

**14. Stale signatures and docs.** At HEAD, `app.R` called `calculate_trend(window_years = …)` and `plot_iucn(window = …)`, which did not exist, and README/vignette cited "v15.1". All of this is fixed in the working tree and covered by `test-app-single.R`. The README install line still reads `remotes::install_github("yourusername/ndopred")` (GAP-14).

**15. Blank fields and swallowed errors.** In the bulk app:
- a download error is caught (`app_bulk.R:168`) and the row reads **DD "No records found"** (358–361);
- an engine error becomes **DD "Inadequate information"** (316–318);
- metric errors become a bare `NA` through `safe_num()` (237–241).

There are no reason columns (GAP-15a, 15b). `batch_assess()` drops a taxon that errored from the table entirely (`batch_assess.R:57-62`) (GAP-15c).

## 5. Further problems found during the audit

| # | Problem | Evidence | Source | Test |
|---|---|---|---|---|
| 16 | **Bulk and single-taxon AOO differ.** The bulk app computes the recent window's B2 AOO on the grid origin minimised over *both* windows; `assess_species()` and `app.R` minimise over the recent window alone | Same taxon and window: bulk 16 km², single 12 km² | §4.10.2 ("the minimum estimate should be used"); prompt §13 | 16 |
| 17 | `Sys.Date()` inside calculations; no reference-year argument (RE screen, default windows, both apps) | `summarize_assessment.R:189`, `batch_assess.R:32`, `app_bulk.R:116`, `app.R:74` | prompt §3 (reproducibility) | 17 |
| 18 | No data-quality qualifiers (observed/estimated/projected/inferred/suspected), so Table 3.1 minimums cannot be enforced (e.g. C1 needs "estimated") | design | Guidelines §3.1, Table 3.1 | — (Phase 1 design) |
| 19 | **Taxon query is a substring match.** `rndop::set_search_payload()` greps the name list, so `"Bombina bombina"` also returns `"Bombina bombina × variegata"` (229 records in the amphibian export); aggregates such as `"Triturus cristatus s. l."` also occur | live `ndop_list()`; cached export | Guidelines §2.1.1; Regional II.6 | 11d |
| 20 | `VALIDACE` is never read: "chybný záznam" (erroneous) and "méně věrohodný záznam" (less credible) records are used as valid | Scarabaeidae: 1 + 6 records, 47 "k opravě" | data quality | 11c |
| 21 | **`VEROH` meaning unverified.** `VEROH ≠ 0` exactly when `GARANCE = "Garantováno"` (13,486/13,486 and 59,870/59,870). ndopred deletes `VEROH = 3` (255 guarantor-reviewed amphibian records) as "dubious"; another AOPK project treats `VEROH = 1` as doubtful | cached exports | — | — (decision D2) |
| 22 | `CXPRESNOST` ignored: 6.2 % of Scarabaeidae records are worse than 2 km; value 0 occurs in 19–32 % of records with unknown meaning | cached exports | §3.2, §4.10.8 | 10a |
| 23 | Record geometries ignored: 29 % of amphibian localisations are polygons, 668 of them larger than a 4 km² cell (max 137 km²) | geometry layer | §4.10.2; review §6.2 | 10b |
| 24 | `DATUM_DO` ignored: 21.5 % of records are date ranges, 33 span more than ten years; all are filed under the start year | cached export | review §2, Table 2 | 11e |
| 25 | Negative findings deleted, although they make up 18 % (amphibians) and 36 % (*P. teleius*) of monitored groups' records | cached exports | review §7.2; Guidelines §11.3 | 11b |
| 26 | Extreme fluctuations diagnosed from effort-confounded yearly totals | fixture | §4.7 | 09d |
| 27 | MCP-EOO used for temporal comparison (B(b)(i), A2c, bulk "Změna EOO (%)") | `calculate_trend.R` | §4.9 (α-hull, fixed discontinuity distance) | (03) |
| 28 | Past-reduction window capped at 100 years | `app.R:53` | §5.2 | 05c |
| 29 | B(b)(iv) asserted from a change in the cluster count | `summarize_assessment.R:264` | §4.11 | (02) |
| 30 | NT is a final category, not a candidate. The 20 % A margin and the "<2,000 mature" C margin are conventions not labelled in the output; NT can rest on cluster-based (a) | `summarize_assessment.R:302-307, 341, 414-419` | §10.1; prompt §7 | (02) |
| 31 | A taxon with earlier records but none in the recent window becomes DD. Under the Guidelines it is a candidate for A2 (disappearance), CR(PE) or RE review | `summarize_assessment.R:500-504` | §11.2, §11.3 | 07b |
| 32 | E is a bare dropdown in the app; no documentation is stored | `app.R:512` | §9.6 | — (Phase 3) |
| 33 | **A failed NDOP login destroys the credentials and is reported as DD.** When the login response lacks the session cookie (wrong password, but also an error or maintenance page), `rndop::isop_login()` calls `renv_cleanup()`, which deletes every `NDOP_*` line from `$HOME/.Renviron`. That is `~\.Renviron` from Git Bash but `~\Documents\.Renviron` from RStudio or Rgui. It also reads the cookie by position (`cookies$value[2]`), not by name. ndopred catches the error and writes "DD – No records found" | rndop 0.2.2.9000 (`f78ebae`); login checked 2026-10-15 | prompt §3 (reason codes) | (15a) |

**One opportunity rather than a problem:** `STRUKT_POZN` carries structured site-threat fields for some monitoring programmes ("Negativní vlivy na lokalitě", "Perspektiva lokality"). Once an expert confirms them, these could support threat-defined locations and B(b)(iii).

## 6. What is already right, and should be kept

- **AOO** by `floor()` binning, minimised over a 4×4 lattice of grid origins (§4.10.2), on a 2×2 km default with a warning at other scales (§4.10.3). Phase 2 extends this to a range; it does not replace it.
- **`reconcile_eoo_aoo()`**: EOO is raised to AOO, and stands in for AOO when a hull cannot be built (§4.9).
- **No outlier trimming of EOO for B1** (§4.9 discourages exclusions).
- A **shared grid origin for comparing two periods**, which is right for a trend metric, though it must not be used for the B2 value (item 16).
- **`filter_counts`** diagnostics in `get_assessment_data()`, which become the summary of `flag_records()`.
- **`extract_year()`** handles all NDOP date encodings seen.
- **Direction of the working-tree fixes:**
  - D2 gated on a threat;
  - (iii) never inferred;
  - the `evaluate_pop` gate on all abundance uses;
  - the mature-term vocabulary audit (`n_records_used` / `ignored_terms`);
  - reversals required for fluctuations;
  - C1 category-specific periods;
  - C2a(ii) 90/95/100 %;
  - restricted, stable and unthreatened is not NT (§10.1).
- **The end-to-end tests** that drive both Shiny apps against a mock NDOP (`helper-mock-ndop.R`). The gap tests reuse them.

## 7. Real NDOP data in brief

Full detail is in [`ndop_data_dictionary.md`](ndop_data_dictionary.md).

- An rndop export has 45 columns. **There is no `ROK` and no `CELED`**, so those code paths never run on real data.
- **No event (visit) ID** is exported by rndop. `IDX_ND_AKCE` exists only in another AOPK export route, and the complete-list flag `KOMPLETNI_SEZNAM` was 0 in every record seen. AOPK decided on rndop only (D4), so T2 occupancy models need visits rebuilt from date, site and observer.
- `ID_NALEZ` is a stable unique record key, which suits the evidence layer.
- Fields ndopred could use and does not: `CXPRESNOST`, the geometry layer (`locations = 2`), `DATUM_DO`, `VALIDACE`/`GARANCE`, `NEGATIV` as data rather than a deletion filter, `PROJEKT`/`DAT_SADA` (to identify monitoring programmes), `REDLIST` (the previous category, for §2.2.1 transfers) and `DATI_INSERT` (snapshot reconstruction).

## 8. Phased plan

Each phase is a small series of reviewable commits. `R CMD check` stays clean, README, vignette and NEWS are updated with every behaviour change, and the version is bumped. A gap test moves from `test-gap-phase0.R` into the regular suite in the commit that fixes it.

### Step 0: baseline (done)

The uncommitted working tree was committed as-is as `ec341f9` ("Baseline: v0.2.0 working-tree revision"), followed by the Phase 0 deliverables.

### Phase 1: one rule engine (items 1, 3, 4, 6, 7, 8, 13, 14, 15, and 16, 17)

**The output model (decided 2026-10-15, D10 and D11).** Bulk mode always proposes a category, together with the criteria behind it. What still needs evidence comes **alongside** the proposal and never replaces it. Proposals may rest on indications from the records (cluster counts, raw window-to-window change, locality counts), but every proposal states which of its conditions are evidenced and which are only indicated. That labelling is how the rule "calculated ≠ evidenced ≠ categorised ≠ approved" is kept.

**Design**

- **`ndopred_config()`** holds everything a run depends on:
  - reference year and windows;
  - conventions, each labelled as such: decline tolerance, NT margins, RE-candidate years, D2 indicative values;
  - dispute and risk tolerance;
  - Guidelines and package versions.

  The config is stored with every output. Nothing in the engine reads `Sys.Date()`.
- **Parameters** are `best`/`min`/`max` with a data-quality qualifier (Table 3.1), a source (`computed`/`expert`/`default`) and a reason code whenever `best` is `NA`.
- **Conditions have two layers.**
  - *Evidence*: `TRUE`/`FALSE`/`NA` from expert input, and later from T2/T3 estimates. `NA` means UNKNOWN. Base R already implements Kleene logic (`NA & FALSE` is `FALSE`, `NA | TRUE` is `TRUE`), and a lint test forbids `isTRUE()` and `%in% TRUE` coercions inside the engine.
  - *Indication*: `TRUE`/`FALSE`/`NA` from record-based proxies. A cluster count indicates locations, raw window-to-window AOO/EOO change indicates B(b) and A2c, and locality counts indicate subpopulations.

  The proposal uses the evidence where it exists and the indication otherwise, and each condition records which it used.
- **`assess_criteria(params, evidence, config)`** returns a tidy evidence table with one row per criterion × subcriterion × level: state, support (`evidence`/`indication`/`none`), values used, threshold, rule, citation, source and evidence ID.
- **`categorise()`** returns:
  - the **proposed category**: the best estimate from evidence plus indications;
  - the **criteria behind it**, in today's format and in Annex 2 notation;
  - the **support**: fully evidenced, or resting on indications;
  - **needs further evidence**: every condition behind the proposal that rests on an indication, plus every unknown condition that could raise the category;
  - the **plausible range**. The low end uses evidence alone. The high end sets every unevidenced B subcondition TRUE where the taxon's own area meets a B threshold, and the D2 threat TRUE where its AOO or locations meet the D2 restriction; parameter min/max feed in from Phase 2.
- **Rules applied to the range**
  - §10.1: a best estimate of LC whose range reaches VU or above is proposed as **NT**, with the criteria nearly met. *Superseded by D7 in 0.5.0: the range flags the proposal as uncertain and no longer moves it.*
  - §10.3: a range spanning LC to CR adds a **DD flag** beside the proposal; DD itself is never proposed. *(The rule is in §10.1 and §10.4; the citation was corrected in 0.5.0.)*
  - **RE** is proposed only as a labelled candidate, with survey evidence listed as needed (Regional Guidelines III.14; Guidelines §11.3); the year threshold is a config convention.
  - A taxon with no usable records is **NE**.

  The expert's approved category (Phase 3) is stored separately from the proposal.
- **Bulk export (D10).**
  - `Kategorie (automatická)` holds the proposed category.
  - `Kriteria (automatická)` holds the criteria behind it in **today's format**.
  - New columns: `Kritéria (IUCN zápis)` (Annex 2 notation), `Opora návrhu` (evidenced or indicated), `Nutno doložit` (needs further evidence), `Rozsah kategorií` (plausible range) and `issue_*` reason codes.

  In the R API, `summarize_assessment()` likewise keeps `Category` and `Criteria` in today's format and adds `Criteria_IUCN`, `Support`, `Needs_Evidence` and `Category_Range`.
- **Inputs.** `severely_fragmented`, `habitat_decline` and the D2 threat default to UNKNOWN. A D2 threat must be described in words, not just `TRUE`; Phase 3 makes it structured. Cluster counts are passed as an *indication* of locations, never as threat-defined locations.
- **Pipeline and apps**
  - A single `assess_taxon()` pipeline is used by `batch_assess()`, the bulk app and the single-taxon app.
  - `expert_final()` is replaced by an engine call with the expert's inputs as evidence.
  - B2 AOO always uses the per-window minimum origin and the 2×2 km grid. Another cell size is for display only.
  - Errors are captured per taxon with a reason code; no taxon is dropped, and none becomes DD.
- **Compatibility.** `summarize_assessment()`, `assess_criterion_a()` and `assess_criterion_d2()` stay as deprecated wrappers.
- **Release.** README install path fixed; NEWS; version 0.3.0.

**Gap tests turned green:** 01a, 01b, 02, 03, 04a–d, 05a, 06, 07a–c, 08a–b, 10c, 13a–e, 14, 15a–c, 16, 17. New tests cover every threshold boundary (`<` versus `≤`), every UNKNOWN path, the Annex 2 and Guidelines §8.2/§8.3 worked examples, and the unchanged format of `Kriteria (automatická)`.

**Status (2026-10-15): done, version 0.3.0.**
- All planned gap tests pass, plus two planned for Phase 5 that the engine rewrite closed on the way: 09a (C1 capped at 100 years) and 09b (C2a from localities now listed as needing subpopulation evidence). The 29 closed tests (GAP-14 included, closed by the README rewrite) live in `test-iucn-alignment.R`.
- Still open, as planned: 05b, 05c, 09c, 09d (Phase 5); 10a, 10b, 10d, 11a–e (Phase 2); 12 (Phase 4).
- Found and fixed during the rewrite: criterion B was checked only at the level of the area threshold met, so a taxon with EOO under 100 km² and four locations came out NT instead of EN.
- One design point settled during implementation: the low end of the plausible range is the working scenario (evidence, else indication), not evidence alone. Otherwise every NDOP-only range would start at LC and the DD flag would fire for almost every taxon. Whether a condition is evidenced is reported by `Support` and `Needs_Evidence` instead.

**Expected visible effect.**
- Most proposals with records stay as they are.
- Every row gains its support, what needs evidence and a plausible range.
- Restricted taxa whose B subconditions are all unknown move from LC to NT under §10.1. *(Reverted by D7 in 0.5.0: LC, flagged uncertain.)*
- The RE screen becomes a labelled candidate, and the DD pre-checks become a flag.
- An error gives a reason code instead of DD; a taxon with no records is NE.

### Phase 2: snapshot, flags, spatial metrics with uncertainty (items 2, 10, 11)

- **`ndop_snapshot()`**
  - calls rndop with `locations = 2`;
  - stores the raw table and geometries unchanged, with query, timestamp, rndop version and maximum `DATI_INSERT`;
  - writes to a run directory outside the repository (D12);
  - checks names exactly on `DRUH`.
- **`flag_records()`** adds one boolean per test plus `exclusion_reason`, and deletes nothing. Flags cover:
  - hidden or missing coordinates, outside the region;
  - negative findings;
  - `VALIDACE`/`VEROH`/`GARANCE` states (D2);
  - taxon mismatch (hybrid, s. l., other subspecies);
  - date range (`year_min`/`year_max`);
  - precision class and geometry type;
  - suspected duplicates of literature and collection records;
  - CoordinateCleaner-style coordinate tests.

  Which flags exclude a record is set in the config.
- **Spatial metrics with `min`/`best`/`max`** and `issue_*` columns:
  - AOO lower bound from precise point records at the minimum origin;
  - upper scenarios from precision buffers and polygons intersecting cells;
  - EOO with and without uncertain records.

  Cross-checked against redlistr 2.1.0 in tests.
- **Cluster diagnostics** are renamed `cluster_count` and documented as such. A location workflow uses the four §4.11 options (a)–(d) with a documented basis, with threats from the experts (D6: no threat layer).

**Gap tests turned green:** 10a, 10b, 10d, 11a–e.

**Status (2026-10-15): done, version 0.4.0.**
- All eight gap tests pass and have moved to `test-iucn-alignment.R`. GAP-10a was rewritten: as first written it compared two records 10 km apart, whose AOO precision cannot change; it now checks that a 3 km precision gives the minimum and maximum it should.
- `ndop_snapshot()`, `read_ndop_snapshot()` and `default_snapshot_dir()` are in place. `get_assessment_data()` keeps every record with flags.
- `flag_records()` applies decisions D2, D3, D8 and D13.
- AOO and EOO carry `min`/`best`/`max`, and the engine uses them for the plausible range (the low end uses the largest areas, the high end the smallest).
- Time windows use a record's whole date range.
- Not done, and moved to Phase 6: the redlistr cross-check (redlistr is not installed here) and the location workflow with §4.11 options (a)–(d). Phase 1's `threat_locations` evidence already covers the count; the options belong with the Phase 3 evidence layer, where experts enter threats (D6).
- Checked on real data:
  - *Onthophagus medius* (live export, 6 polygon records): AOO 28 km² best, 28–40 km² with positional uncertainty.
  - *Bombina bombina* (27,396 cached records): 229 hybrid records excluded by the exact-name rule; 3,649 negative findings kept as non-detections; 117 `VEROH` 3 records excluded (0.4 % share).

### Decisions D4–D7 (done, version 0.5.0)

- **D5 windows.** `ndopred_config()` defaults to a 17-year current window and a comparison window 10 years earlier: 2010–2026 against 2000–2016 for reference year 2026. `trend_shift` replaces `trend_window`, and the trend always compares the comparison window with the whole current window. `batch_assess()`, both apps and `plot_iucn()` follow; overlap no longer warns. The windows share 2010–2016, which damps measured change, and the B window can hold sites lost since 2010 (§4.9). Both are documented in the vignette and README, not hidden.
- **D7 proposal.** Uncertainty no longer moves the proposal. `categorise()` returns `uncertain`, rows gain `Uncertain` / `Nejistý návrh`, and the flag text says which §10.1 choice applies. GAP-04b and GAP-08b were restated: they asserted "not LC", which stood for "not a silent LC"; they now check the uncertainty flag, the range and the evidence needed.
- **D4, D6** need no code now; their consequences are in the Phase 3 and Phase 5 plans.
- Checked on real data (reference year 2026): *O. medius* stays NT (B1 and B2 nearly met), now flagged uncertain (LC–EN). *P. cuprea* and *B. bombina* stay LC. The longer window raises *B. bombina* AOO from 5,704 to 8,020 km² and turns its AOO change from −3.4 % to +10.9 %.

### Decisions D8 (revised), D9 and generation lengths (done, version 0.6.0)

- **D8.** `VEROH` 3 is always used, flagged only above 10 % of the taxon's records. Taxa with a few such records gain them back in AOO, EOO and the counts.
- **Generation lengths.** `inst/extdata/generation_lengths.csv` has one row per NDOP taxonomic group (58, from `rndop::ndop_list("group")`), keyed by NDOP code, Latin name and the Czech `KAT_TAX` the records carry; every value is a 1-year placeholder. `assess_species()`, `batch_assess()` and both apps take the taxon's value from its records' group; result rows report the value and its source. The comparison window moves back by three generations where that exceeds 10 years, so refining a group changes its windows without code changes.
- **D9** is recorded; the approval record is built in Phase 3.

### Phase 3: evidence layer, templates, expert UI, species card

- **Evidence schema and storage**
  - an evidence schema with CSV/XLSX templates and validation on import;
  - versioned evidence files keyed by taxon and `ID_NALEZ`;
  - every entry carries author, date, justification, sources, `min`/`best`/`max` and a qualifier.
- **Expert UI** (`app.R`)
  - one panel per criterion, showing computed values, uncertainty, the rule applied and its section;
  - evidence forms with a live preview of how each piece of evidence changes the condition states;
  - GeoCAT-style record toggles on the map;
  - clusters labelled as clusters.
- **Final category.** The expert's final category must carry a justification, and the engine re-checks it for consistency.
- **Species card** (HTML/PDF): record IDs used, parameters, rules applied, evidence, author, versions and open questions.

**Gap test turned green:** 04c in its structured form.

**Status (2026-10-15): done, version 0.7.0.**
- Evidence schema, CSV/XLSX templates, validation reporting every problem with its row, and versioned files: `evidence_fields()`, `evidence_template()`, `read_evidence()`, `validate_evidence()`, `write_evidence()`, `latest_evidence()`. Record entries are keyed by `ID_NALEZ` and become flags (`apply_record_evidence()`).
- Evidence numbers are `min`/`best`/`max` ranges in the engine (§3.2): the proposal uses the best estimate, the plausible range the ends.
- The §4.11 location options are part of the schema (`basis`: `all`, or a-d for unaffected parts; (a) means the location subcriteria are not met). This closes the location workflow deferred from Phase 2.
- A1 needs reversible, understood and ceased causes, otherwise A2. A threat must be described in words: GAP-04c now also checks the schema.
- `final_assessment()` checks the final category against what the evidence and measured data support without the indications, and applies D9: final only with an approval dated on or after it.
- `species_card()` writes one HTML page per assessment; for a PDF, print it from a browser (no LaTeX or headless browser is assumed).
- Single-taxon app rebuilt around the evidence table (assessor, justification and sources per panel; Records and Final category tabs; load and save; card). The bulk app takes one evidence file for any number of taxa.
- Not done here:
  - the qualifier is stored but not yet used by the engine (C1 "estimated" quality: Phase 5);
  - eligibility and survey entries are stored for the regional step (Phase 4);
  - the panels show the computed values and the rule per criterion in the Evidence table tab rather than inside each panel.

### Phase 4: regional step (item 12)

- **Eligibility** before assessment: wild, natural range, ≥10 years of established reproduction for colonists, vagrants NA, documented filters.
- **Step-3 adjustment** after assessment: questions 3a–3c for breeding populations and 3d–3f for visitors, with the ° mark. EX, EW, RE, DD, NE and NA are never adjusted.
- **RE** only through survey evidence (Phase 3 evidence, §11.3 models).
- **Output fields** for the global category and the share of the global population.

**Gap test turned green:** 12.

**Status (2026-10-15): done, version 0.8.0.**
- Step one, `regional_eligibility()` (RG IV.2, Annex 3): introduced taxa, vagrants, occasional breeders, colonizers reproducing for fewer than 10 years (`colonizer_years`) and taxa below an optional filter (`regional_filter_share`, off until AOPK sets one) are NA before any criterion is applied. Unknown answers lead to assessment, as in the flowchart. Visitors are assessed as visiting populations (the Phase 3 check had wrongly allowed NA for visitors).
- Step three, `regional_adjustment()` (RG IV.4, Figure 3, Table 1): questions 3a-3c for breeding and 3d-3f for visiting populations, normally one step, never for EX, EW, RE, DD, NE or NA, with the degree sign (RG V.3); a taxon listed globally under D is not expected to rescue a visiting population. Unanswered questions leave the category unchanged, as RG IV.4 requires.
- Rows carry the regional category and criteria, the step trail, the global category and the share of the global population (`?` when unknown, RG V.4); the bulk export and the species card show them; the single-taxon app has a regional-step panel.
- `final_assessment()` applies the same regional step to what the evidence supports, so a final category set by a documented rescue effect is consistent; RE must have surveys and should not pre-date 1500 AD (RG III.14).
- GAP-12 restated and moved to the regular suite (it had only looked for an argument name in the deprecated wrapper). GAP-05b already passed after the generation-length table (0.6.0) and moved too. Open: 05c, 09c, 09d (Phase 5).
- Not done: breeding and visiting populations of one taxon cannot be assessed separately; NDOP records carry no breeding/visiting distinction and evidence is keyed by taxon name.

### Phase 5: trend module and criterion C (items 5, 9)

- **Data tiers T0–T3 per taxon** (review Table 5), from:
  - the share of negative findings;
  - monitoring `PROJEKT`/`DAT_SADA`;
  - repeat visits per site;
  - visits rebuilt from date, site and observer (D4: rndop exports no event ID).
- **T0/T1 data** produce labelled diagnostics only. The interim proxy is an effort-corrected AOO-change indicator (relative to group-level recording effort, or Frescalo via sparta 1.0.1).
- **T2** uses occupancy/detection models (spOccupancy 0.8.2 or sparta); **T3** uses abundance indices.
- **Reduction windows**
  - window = max(10 y, 3 × GL); cap only for A3/A4 and future C1;
  - A4 moving window;
  - α-hull EOO with a fixed discontinuity distance (alphahull 2.5);
  - start-year sensitivity reported.
- **Criterion C corrections**
  - subpopulations come from expert input;
  - `total_mature` is taken from a defined census period with min/best/max;
  - C1 is capped at 100 years and requires "estimated" quality;
  - fluctuation diagnosis requires a consistent-effort series or expert evidence.
- **Generation length per taxon or group** in the bulk input, once AOPK names a source (D5); the comparison window then moves back by three generations where that exceeds 10 years, as the single-taxon app already does.

**Gap tests turned green:** 05b, 05c, 09a–d.

**Status (2026-10-15): done, version 0.9.0.**
- `trend_estimate()` classifies each taxon's data into T0-T3 (review Table 5) and gives the estimate the tier allows, with an interval; the estimate is the reduction indication and its interval the plausible range. T0: raw AOO and alpha-hull EOO change with the start-year range (comparison window moved two years either way). T1: relative occupancy on cells recorded for the group in both windows, bootstrap interval. T2: spOccupancy `tPGOcc` with list length as the detection covariate, run when `occupancy_model = TRUE`. T3: a log-linear index from monitoring projects AOPK lists.
- A first T0 design (equal-size subsamples of each window) gave intervals that excluded zero for a stable taxon: subsampling does not capture the noise of the window samples themselves. It was replaced by the start-year range, and the alpha-hull is built on occupied cells, because on raw records it followed recording density (*Bombina bombina*: -41 % on records, +0 % on cells).
- Effort layers (`ndop_effort()`): every record of an NDOP group as cells and anonymous visits; downloaded year by year (month by month where a year is too large), resumable. NDOP counts per window: Amphibia ~174,000-296,000, Odonata ~129,000-153,000, Coleoptera 363,000 (2000-2016); the counts for Lepidoptera, and for Coleoptera in 2010-2026, timed out, so chunking is required.
- Alpha-hull for EOO change (Guidelines 4.9), `calculate_eoo_alpha()`; MCP stays for B1.
- Criterion C: census total from each locality's most recent count with a min/max range (GAP-09c); fluctuations only from consistent-effort (T3) series (GAP-09d); C1 only from an estimated decline (Table 3.1); expert evidence below the Table 3.1 minimum documented but not used. Past reductions not capped at 100 years in the single-taxon app (GAP-05c).
- All 42 gap tests now pass in the regular suite; the opt-in file is removed.
- Real data: *Bombina bombina* T1 +2 % (+1 to +3) on 2,784 cells, T2 +9 % (+3 to +15), Rhat 1.09, 100 s; *Protaetia cuprea* T0 +236 % (+180 to +313), which mostly reflects growing recording effort.
- Not done: negative findings are not yet used as non-detections in T1/T2; an occupancy model with year effects (rather than a linear trend); the A4 moving window from data; C1 from T2.

### Phase 6: benchmark, pilot validation, documentation

- **ConR 2.1** (GitHub `gdauby/ConR`, active, not on CRAN): a benchmark script comparing EOO, AOO, locations and category with ndopred on the pilot taxa.
- **redlistr and red** cross-checks.
- **Pilot validation script** for one well-recorded and one poorly recorded insect group. It reports false threatened and false non-threatened separately against blinded expert assessments, and tests stability under alternative filters.
- **Documentation:** the vignette is rewritten around the evidence model, and the export gets its own data dictionary.

**Status (2026-10-16): done, version 0.10.0.**
- `compare_tools()` and `inst/benchmark/benchmark_tools.R`: ndopred against ConR 2.1, redlistr 2.1.0 and red 1.6.3 on *Onthophagus medius*, *Protaetia cuprea* and *Bombina bombina*. EOO agrees with redlistr exactly, with red within 0.01 % and with ConR within 0.3 % (projection). ndopred's AOO is the lowest; ConR's is 5–14 % higher, because its cells are square in a cylindrical projection, not on the ground. ConR's criterion B assumes a continuing decline and counts locations on a 10 km grid, so it is compared with ndopred's plausible maximum, not the best estimate. Tests cross-check EOO and AOO against redlistr and ConR, and the category against ConR where both rules agree.
- `validate_against_experts()`, `filter_stability()`, `filter_variants()`, `region_variants()` and `inst/validation/pilot_validation.R`: false threatened and false non-threatened counted separately against blinded expert categories, overall and per pilot group, and stability under alternative filters (review §9). The expert comparison was later dropped by AOPK (D17, below). On the three benchmark taxa only dropping unvalidated records changes a proposal: *O. medius* NT → NE (all 25 records `VEROH` 0), *P. cuprea* LC → NT (1,281 of 1,402 records `VEROH` 0).
- `export_dictionary()`: every column of both exports with its layer, IUCN section and a candidate SIS field; a test keeps it in step with the exports. Triage for the expert queue (`Triage_Priority`, `Triage`; prompt §8, review §3).
- The vignette is rewritten around the four states of a value: calculated, evidenced, proposed, approved.
- Fixes found by the checks:
  - **AOO origin search.** redlistr's random jitter beat the 4 × 4 search by one cell. `calculate_aoo()` now searches exactly for up to 150 distinct positions.
  - **Literature flag.** The stability test showed that the literature flag matched 69 % of *B. bombina* records, because NDOP cites every source as "AUTHOR (year) title". It now looks for markers of secondary sources.
  - **Unit filter.** NDOP uses only the first 1,000 cadastral units of a filter, without a warning. `ndop_effort()` now sends longer unit samples in parts, and the Diptera, Hymenoptera and Trichoptera layers were downloaded again.
- Effort layers for T1, larger samples (2026-10-16, 75 min of NDOP queries in all): Lepidoptera 512 cells (was 42), Coleoptera 513 (66), Hymenoptera 1,747, Trichoptera 998 (88), Diptera 495 (92), Aves 195 (35), Tracheophyta 144 (21). Hymenoptera, Trichoptera and Diptera were downloaded again after the 1,000-unit fix (before it: 716, 109 and 312 cells). A cadastral-unit sample spreads effort thinly over the country, so T1 is reached only for taxa whose occupied cells overlap enough sampled cells; most taxa of the large groups stay T0. With the new Coleoptera layer *P. cuprea* reaches T1, +140 % (+64 to +344) against +236 % raw at T0; *O. medius* stays T0 (none of its cells in the sample); *B. bombina* is T1 +0 % (−10 to +11) on the 1.4 % amphibian sample.
- Decided by AOPK on 2026-10-16 (§9), version 0.11.0:
  - **D14:** the T3 projects. T3 now uses only sites counted in two or more years, since many listed projects are inventories and mappings.
  - **D15:** the refined generation lengths are the default.
  - **D16:** no regional share filter.
  - **D17:** no validation against blinded expert assessments. `validate_against_experts()` became `compare_categories()`, which takes no list as the truth, and the pilot script became `check_proposals.R`.
- Still open: checking the SIS field mapping against the current SIS schema.

## 9. Decisions needed from AOPK

**Decided:** D1–D13 (all 2026-10-15). **Open:** the generation-length values themselves (a placeholder of 1 year per group until refined).

One question the prompt already settles is not listed: taxa with no usable records become NE with a reason code, not DD (Guidelines §10.4).

| # | Question | Recommendation |
|---|---|---|
| D1 | Commit the current uncommitted working tree (your v0.2.0 revision) as the baseline before Phase 1? | **Decided 2026-10-15: yes.** Committed as `ec341f9` |
| D2 | What do `VEROH` 0/1/3 mean, and how do `VEROH`, `GARANCE` and `VALIDACE` combine? Which states exclude a record by default? | **Decided 2026-10-15.** 0 not yet validated, 1 credible, 3 less credible, 6 erroneous (to delete), 9 erroneous (to correct). After the guarantee only 1 or 3 remain; while it is open `VEROH` is 0 even with a proposed mark. Implemented: 6 and 9 excluded; 0 and 1 used; a proposed `VALIDACE` mark flagged only; 3 per D8 |
| D3 | Does `CXPRESNOST = 0` mean "exact" or "unknown"? | **Decided 2026-10-15: exact.** |
| D4 | Can records be linked to visits (`IDX_ND_AKCE`) and complete lists? Which export route may ndopred depend on: rndop only, or the AOPK export? | **Decided 2026-10-15: rndop only**; the records come from a range of sources. No `IDX_ND_AKCE`, so Phase 5 rebuilds visits from date, site (`ID_LOKAL`) and observer (`AUTOR`), and uses `ZDROJ`/`DAT_SADA`/`PROJEKT` to tell monitoring from opportunistic records |
| D5 | The definition of "current" occurrence per taxon group, the default comparison windows, and the source of generation lengths | **Decided 2026-10-15:** current window 1.1.2010–31.12.2026, trend reference window 1.1.2000–31.12.2016, nationally. Implemented as `ndopred_config(window = 17)` with the comparison window moved back by the criterion A period (10 years, or three generations where longer). Generation lengths: a table per NDOP taxonomic group (`generation_lengths()`), 1 year for every group for now, to be refined by a dedicated task; a value given for a taxon wins |
| D6 | Is there, or will there be, a maintained threat layer for locations? | **Decided 2026-10-15: no layer.** Threats, if any, are filled in by the experts: threat-defined locations and the D2 threat are expert input only (Phase 3) |
| D7 | Default dispute and risk tolerance for the national list | **Decided 2026-10-15:** "Propose the category and the criteria behind it, but flag as uncertain when uncertainty criteria are clearly met." Implemented: the proposal is the best estimate, the whole plausible range is reported (dispute tolerance 0), and a range holding both a non-threatened and a threatened category is flagged uncertain (the §10.1/§10.4 case) instead of moving the proposal to NT |
| D8 | Are `VEROH` 3/9 and literature or collection records excluded by default, or only flagged? | **Decided 2026-10-15 (revised the same day):** `VEROH` 3 is never excluded; it is flagged only when more than 10 % of the taxon's records carry it (no flag at or under 10 %; `flag_records(less_credible_share = 0.10)`). Literature/collection records are flagged only |
| D9 | Who approves eligibility, regional adjustment and final categories, and how is approval recorded? | **Decided 2026-10-15:** group experts, conservationists and taxonomists trained in Red List assessment, approve. How approval is recorded is not specified; Phase 3 implements the recommendation: an approval record in the evidence file (approvers, expert group, role, date, version, justification), and the engine refuses a final category without one |
| D10 | May `Kategorie (automatická)` and `Kriteria (automatická)` change meaning (no final LC/NT/DD/RE from the engine; Annex 2 notation)? | **Decided 2026-10-15: no.** Both columns keep a proposed category and the criteria behind it, and `Kriteria (automatická)` keeps today's format. Annex 2 notation goes in a new `Kritéria (IUCN zápis)` column. Further new columns: `Opora návrhu`, `Nutno doložit`, `Rozsah kategorií`, `issue_*` |
| D11 | Should raw window-to-window change and cluster counts stop driving bulk categories until Phase 5 provides effort-corrected trends? | **Decided 2026-10-15: no.** Bulk mode must propose a category. Indications from the records may support the proposal; they are labelled as indications, and "needs further evidence" comes alongside the proposal, not instead of it |
| D12 | Where are snapshots stored? They contain exact and hidden coordinates and personal names | **Decided 2026-10-15:** RDS files in a new folder in Documents: `default_snapshot_dir()` = `Documents/ndop_snapshots` (override with `NDOPRED_SNAPSHOT_DIR`) |
| D13 | How are taxa identified: exact `DRUH`, `ID_TAXON` from the input sheet, or an accepted-name checklist? | **Decided 2026-10-15: exact names.** Records whose `DRUH` differs from the queried name are flagged and excluded |
| D14 | Which NDOP projects count as monitoring for data tier T3? | **Decided 2026-10-16:** 18 projects. They cover species and site monitoring (Monitoring druhů ČR; OP MapInv and OP Mon3 monitoring and mapping; Natura 2000 implementation and monitoring; monitoring of habitats on permanent plots, of bird areas and Annex I birds), inventories and state surveys of protected areas, habitat mapping and its updates and rectification, the national phytosociological database, bird counts (LSD, IWC) and the mapping of invasive species. Implemented as `monitoring_projects()` (`inst/extdata/monitoring_projects.csv`), the default of `trend_settings()` and of both apps. Names are matched exactly against `PROJEKT`. Many of these projects visit a site once, so T3 now uses only sites counted in two or more years |
| D15 | Generation lengths: keep the 1-year placeholder or adopt the refined tables? | **Decided 2026-10-16: the refined tables** (`generation_lengths_refined()`; `methodology/generation_lengths/`) are the default; the placeholder can still be chosen |
| D16 | Should taxa below a share of the global population be Not Applicable (Regional Guidelines IV.2, optional filter)? | **Decided 2026-10-16: no.** `regional_filter_share` stays `NA` |
| D17 | Should the proposals be validated against blinded expert assessments (review §9)? | **Decided 2026-10-16: no.** Experts often depart from the IUCN criteria, so their categories are no gold standard. The reference is the criteria (tests at the thresholds, the unknown paths, the notation and the Guidelines' worked examples). `compare_categories()` sets the proposals beside any list without taking either side as the truth, and `final_assessment()` checks expert categories against the evidence |

## 10. Running the gap tests

```r
devtools::test()   # every gap test now runs in the regular suite (test-iucn-alignment.R)
```

Each test name starts with its item number (GAP-01 … GAP-17). The fixtures reuse `helper-mock-ndop.R` and add two synthetic taxa: *Effort drop sp*, whose range is unchanged while records fall from 200 to 40, and *Origin sp*, whose coordinates were chosen so that the two grid-origin rules disagree.

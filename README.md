# ndopred: NDOP Red List Assessor <img src="ndopred_hex.png" align="right" width="200" />

**ndopred** turns occurrence records from **NDOP** (*Nálezová databáze ochrany přírody*, the Czech Nature Conservation Agency's species occurrence database) into proposed IUCN Red List categories for the Czech national list, with the evidence behind each proposal and what still needs evidence.

It follows the **IUCN Red List Categories and Criteria v3.1** (IUCN 2012), the **Guidelines for Using the IUCN Red List Categories and Criteria v16** (March 2024) and the **Guidelines for Application of IUCN Red List Criteria at Regional and National Levels v4.0** (2012). All three are in [`methodology/`](methodology/). [`methodology/gap_analysis.md`](methodology/gap_analysis.md) records how the package is being aligned with them, phase by phase.

> Rendering the vignette needs [pandoc](https://pandoc.org/installing.html) (RStudio and Quarto bundle it). Without pandoc, `R CMD INSTALL .` still works; only `R CMD build` and `vignette("methodology")` need it.

---

## Contents

- [How a category is proposed](#how-a-category-is-proposed)
- [The two apps](#the-two-apps)
- [Installation](#installation)
- [Credentials](#credentials)
- [Quick start](#quick-start)
- [Function reference](#function-reference)
- [The bulk export](#the-bulk-export)
- [The single-species app](#the-single-species-app)
- [Methodology notes](#methodology-notes)
- [Testing](#testing)

---

## How a category is proposed

Every entry point (both apps, `assess_species()`, `batch_assess()`) runs the same rule engine and reports four things for each taxon:

1. **A proposed category** with the criteria behind it, in ndopred's established format (`B1ab(ii); B2ab(ii)`) and in IUCN notation (`B1ab(ii)+2ab(ii)`).
2. **Its support.** *Evidenced* means every condition behind the proposal comes from the assessor or from a measured value. *Indication* means at least one condition rests on a proxy from the records, such as 5 km clusters standing in for threat-defined locations, or the raw change in AOO between two windows standing in for a decline.
3. **What still needs evidence**, condition by condition, e.g. `B(a): severe fragmentation or threat-defined locations (5 km clusters used as an indication)`.
4. **The plausible range** of categories if the unknown conditions turned out true, e.g. `LC-EN`.

What the records cannot show is never assumed absent. An unknown condition is **unknown**: it does not count against the taxon in the proposal, but it widens the range and appears in "needs evidence". A taxon whose range could reach VU or above is proposed as **NT** rather than LC (Guidelines §10.1).

The proposal is not a final category. Approval belongs to the assessor.

---

## The two apps

```r
library(ndopred)

run_bulk_app()   # screen a whole spreadsheet of taxa
run_app()        # assess one taxon at a time, as an expert
```

| | `run_bulk_app()` | `run_app()` |
|---|---|---|
| **Input** | CSV with a `TAXON` column | one scientific name |
| **Pipeline** | `assess_taxon()` | `assess_taxon()`, plus the assessor's evidence |
| **NDOP counts (C, D)** | not used | used as indications when the count basis supports it |
| **Output** | one row per taxon + a map per taxon (CSV + ZIP) | the proposal, its evidence table, and the assessor's view |

Both apps compute identical numbers for identical inputs.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("jonasgaigr/ndopred")
```

`rndop` comes from GitHub and is declared in `Remotes:`. If you install by hand:

```r
remotes::install_github("kalab-oto/rndop")
install.packages(c("dplyr", "ggplot2", "giscoR", "magrittr", "sf", "shiny",
                   "shinythemes", "terra", "units", "zip"))
install.packages("igraph")   # required by calculate_locations()
```

---

## Credentials

NDOP requires a login. Everything that downloads data needs `NDOP_USER` and `NDOP_PWD` in the environment, usually stored in `.Renviron`:

```
NDOP_USER='your.username'
NDOP_PWD='your password'
```

⚠️ **Three gotchas:**

1. **Which `.Renviron` is read depends on how R starts.** R started from RStudio, Rgui or PowerShell on Windows reads `~/Documents/.Renviron`. `Rscript` from Git Bash reads `C:/Users/<you>/.Renviron`. Keep both in step, or check with `path.expand("~/.Renviron")`.
2. **A failed login deletes the credentials.** If a login gets no session cookie back, `rndop::isop_login()` removes every `NDOP_*` line from `$HOME/.Renviron`. That happens on a wrong password, and also when the server returns an error or maintenance page. Keep a copy.
3. `isop_login()` keeps its session cookie in `.GlobalEnv` of the R session that ran it; other processes log in again from the environment variables.

The password is stored in plain text, so restrict the file's permissions. A failed download is reported in the output (`issue_download`) and never turned into a category.

---

## Quick start

### One taxon

```r
library(ndopred)

assess_species("Lucanus cervus",
               recent_start = 2016, recent_end = 2025,
               comp_start   = 2006, comp_end   = 2015)
```

The reference year defaults to `recent_end`. The assessor's evidence can be passed straight through:

```r
assess_species("Bombina bombina",
               recent_start = 2016, recent_end = 2025,
               comp_start   = 2006, comp_end   = 2015,
               d2_threat = "drainage of breeding ponds",   # describe the threat
               threat_locations = 8,
               habitat_decline = TRUE)                       # B(b)(iii)
```

### Many taxa

```r
batch_assess(c("Lucanus cervus", "Bombina bombina", "Pulsatilla vernalis"),
             reference_year = 2025)
```

A taxon that fails keeps its row: the category is `NA` and an `issue_*` column says why.

### Step by step

```r
cfg <- ndopred_config(reference_year = 2025, window = 10)
occ <- get_assessment_data("Bombina bombina")
attr(occ, "filter_counts")          # how many records survived each QC step

m <- taxon_metrics(occ, cfg)        # EOO, AOO, clusters, trend, counts, issues
res <- assess_taxon(species = "Bombina bombina", config = cfg, metrics = m,
                    evaluate_pop = TRUE,
                    evidence = list(threat = "drainage of breeding ponds",
                                    threat_locations = 8))
res$proposal                        # category, criteria, support, needs, range
res$assessment$table                # one row per criterion x subcriterion x level
```

The engine can also be driven from your own numbers:

```r
inp <- assessment_inputs(
  eoo_km2 = 1700, aoo_km2 = 84, n_records = 40, n_current = 12, year_last = 2025,
  evidence = list(threat = "drainage of the two remaining fens",
                  threat_locations = 2, decline = c(iii = TRUE))
)
categorise(assess_criteria(inp, ndopred_config(2025)))
#> Proposed: EN B1ab(iii)+2ab(iii)
```

### A map

```r
p <- plot_iucn("Bombina bombina", occ_data = occ, window = 10, year_now = 2025)
ggplot2::ggsave("bombina.png", p, width = 8, height = 6, dpi = 150, bg = "white")
```

---

## Function reference

**Configuration and engine**

| Function | Purpose |
|---|---|
| `ndopred_config()` | Everything a run depends on: reference year, windows, generation length, labelled conventions, versions |
| `assessment_inputs()` | Data, indications and evidence for one taxon |
| `assess_criteria()` | The single rule engine: criteria A-E in two scenarios, plus a tidy evidence table |
| `categorise()` | The proposal: category, criteria (both formats), support, needs, range, flags |

**Pipeline**

| Function | Purpose |
|---|---|
| `get_assessment_data()` | Download and quality-filter NDOP records; records how many survived each step and any download error |
| `taxon_metrics()` | Metrics for one taxon, with a reason for every value it cannot compute |
| `assess_taxon()` | Records in, proposal out: what both apps and `batch_assess()` use |
| `assess_species()`, `batch_assess()` | The pipeline for one or many taxa, headless |

**Metrics**

| Function | Purpose |
|---|---|
| `calculate_eoo()` | Extent of occurrence, minimum convex polygon (Guidelines §4.9) |
| `calculate_aoo()` | AOO on the 2 x 2 km reference grid, minimised over grid origins (§4.10.2, §4.10.3) |
| `calculate_locations()` | 5 km cluster count: an *indication* for threat-defined locations (§4.11), never a location count |
| `reconcile_eoo_aoo()` | Enforce EOO >= AOO (§4.9) |
| `calculate_trend()` | AOO and EOO change between two windows, on a shared grid origin |
| `calculate_pop_metrics()` | Minimum observed counts, decline rate, fluctuation diagnosis (§4.3, §4.5.1, §4.7) |
| `get_mature_terms()` | The `POCITANO` values that assert reproductive capability (§4.3) |

**Deprecated, kept for existing scripts:** `summarize_assessment()`, `assess_criterion_a()`, `assess_criterion_d2()`.

**Presentation:** `plot_iucn()`, `run_app()`, `run_bulk_app()`.

---

## The bulk export

Upload a CSV with `TAXON` (required), and optionally `CELED`, `SKUPINA` and `ID_TAXON` (for a link to the species card on portal.nature.cz). Set the reference year and the window length; the comparison window is the equal-length window before it.

The established columns are unchanged. `Kategorie (automatická)` holds the proposed category and `Kriteria (automatická)` the criteria behind it, in the established format. The new columns are:

| Column | Content |
|---|---|
| `Kritéria (IUCN zápis)` | The same criteria in IUCN Annex 2 notation (`B1ab(ii)+2ab(ii)`, `D` under CR/EN, `D1+2`) |
| `Opora návrhu` | `doloženo` (evidenced), `indikace z nálezových dat` (rests on indications), `kandidát` (RE/DD candidate), `nehodnoceno` (NE) |
| `Nutno doložit` | What still needs evidence, condition by condition |
| `Rozsah kategorií` | Plausible range, e.g. `LC-EN` |
| `Příznaky` | Candidate flags: `D2 candidate`, `DD candidate (plausible range LC-CR)`, `RE candidate` |
| `AOO nový, zvolená mřížka (km2)` | AOO on the display grid chosen in the app (the B2 value is always on 2 x 2 km) |
| `Referenční rok` | The reference year of the run |
| `issue_download` … `issue_population` | The reason for every empty value |

---

## The single-species app

Enter a name and the reference year, press **Run Assessment**. The *automated proposal* comes from the records alone. In **Expert Review**, every entry starts as *unknown*; whatever the assessor enters becomes evidence and replaces the indication for that condition:

- severe fragmentation (yes / no / unknown);
- the most serious plausible threat (a description), or "no plausible threat";
- threat-defined locations;
- continuing decline and extreme fluctuations, confirmed or ruled out;
- population figures;
- an evidenced reduction with its A subcriterion and basis;
- criterion E.

The **Expert view** updates live and equals the automated proposal until something is entered. The **Evidence table** tab shows every criterion, subcriterion and level with its value, threshold, support and source.

---

## Methodology notes

- **Unknown is not "no".** Severe fragmentation, habitat decline and the threat default to unknown. A restricted taxon with nothing evidenced is NT with a range reaching CR; the same taxon with the conditions evidenced absent is LC (Guidelines §10.1).
- **D2 needs a described threat.** A restricted range is flagged as a D2 candidate; VU D2 follows once the assessor names a plausible threat able to drive the taxon to CR or EX in a very short time (§8). `d2_threat = TRUE` alone is not a threat and is treated as unknown.
- **Clusters are not locations.** The 5 km cluster count is an indication for B(a) and D2, always listed as needing threat-defined locations (§4.11). With no plausible threat, the location subcriteria are not met.
- **Raw changes between windows are indications.** They can support B(b) and A2c proposals, but are listed as needing an effort-corrected or monitored estimate, because the number of records changes with recording effort (§4.10.8).
- **RE and DD are candidates, NE is "not evaluated".** A taxon not recorded for longer than `re_candidate_years` (a convention, default 50) is an RE candidate needing documented surveys (Regional Guidelines III.14). DD is proposed only for a taxon with records but none current and nothing else to go on, and is flagged when the range spans LC to CR (§10.3). A taxon with no usable records is NE (§10.4).
- **Notation follows Annex 2** in `Criteria_IUCN` / `Kritéria (IUCN zápis)`: `CR D` and `EN D` (not `D1`), `B1…+2…`, `C2a(i,ii)`, `VU D1+2`.
- **No system clock.** Every run states its reference year.
- **Counts are minima.** `total_mature` and `max_subpop` are minimum observed values from opportunistic records, and NDOP *localities* stand in for subpopulations, so both are indications (§4.2, §4.3).

---

## Testing

```r
devtools::test()
```

The suite covers:
- the engine at every threshold boundary;
- the IUCN Annex 2 and Guidelines §8.2/§8.3 worked examples;
- every closed gap from the Phase 0 audit (`test-iucn-alignment.R`);
- both Shiny apps end-to-end against a synthetic NDOP fixture (`tests/testthat/helper-mock-ndop.R`).

Gaps still open are opt-in tests:

```r
Sys.setenv(NDOPRED_GAP_TESTS = "true"); devtools::test(filter = "gap-phase0")
```

---

## Logo

<img src="ndopred_hex.png" align="right" width="120" />

The `ndopred` logo showcases a silhouette of a [European stag beetle](https://portal.nature.cz/w/druh-6707#/) (*Lucanus cervus*) and [spring pasqueflower](https://portal.nature.cz/w/druh-39051#/) (*Pulsatilla vernalis*).

## Status

Version 0.3.0 (experimental). Proposals are a screening aid; the category is the assessor's decision.

## References

IUCN (2012). *IUCN Red List Categories and Criteria: Version 3.1*, 2nd edition. Gland, Switzerland and Cambridge, UK: IUCN.

IUCN (2012). *Guidelines for Application of IUCN Red List Criteria at Regional and National Levels: Version 4.0*. Gland, Switzerland and Cambridge, UK: IUCN.

IUCN Standards and Petitions Committee (2024). *Guidelines for Using the IUCN Red List Categories and Criteria*, Version 16. <https://www.iucnredlist.org/documents/RedListGuidelines.pdf>

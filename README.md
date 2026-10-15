# ndopred: NDOP Red List Assessor <img src="ndopred_hex.png" align="right" width="200" />

**ndopred** turns occurrence records from **NDOP** (*Nálezová databáze ochrany přírody*, the Czech Nature Conservation Agency's species occurrence database) into IUCN Red List metrics and categories, for national and regional Red List work.

Thresholds follow **IUCN Red List Categories and Criteria v3.1** (IUCN 2012); their interpretation follows the **Guidelines for Using the IUCN Red List Categories and Criteria v16** (March 2024). Both documents are in [`methodology/`](methodology/), and [`vignettes/methodology.Rmd`](vignettes/methodology.Rmd) maps every rule in the code to its guideline section.

> Rendering that vignette needs [pandoc](https://pandoc.org/installing.html) (RStudio bundles it; a bare R install does not). Without pandoc, `R CMD INSTALL .` still works and the vignette source stays readable in the repo — only `R CMD build` and `vignette("methodology")` need it.

---

## Contents

- [The two apps](#the-two-apps)
- [Installation](#installation)
- [Credentials](#credentials)
- [Quick start](#quick-start)
- [Function reference](#function-reference)
- [The bulk app in detail](#the-bulk-app-in-detail)
- [The single-species app in detail](#the-single-species-app-in-detail)
- [What is *not* automated](#what-is-not-automated)
- [Methodology notes](#methodology-notes)
- [Testing](#testing)

---

## The two apps

The package ships two complementary dashboards, both callable as plain functions:

```r
library(ndopred)

run_bulk_app()   # screen a whole spreadsheet of taxa
run_app()        # assess one taxon at a time, as an expert
```

| | `run_bulk_app()` | `run_app()` |
|---|---|---|
| **Input** | CSV with a `TAXON` column | one scientific name |
| **Scope** | criteria automatable from occurrence records (EOO / AOO / locations) | all criteria, A–E |
| **Criteria C & D1** | never (no population estimate attempted) | when the count basis supports it, subject to expert confirmation |
| **Output** | one row per taxon + a map per taxon (CSV + ZIP) | a category and criteria string per taxon, with expert overrides |
| **Purpose** | narrow a long list to what needs attention | settle a defensible category |

They are designed to be used in that order.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("yourusername/ndopred")
```

Dependencies are resolved automatically. `rndop` comes from GitHub and is declared in `Remotes:`; if you install by hand:

```r
remotes::install_github("kalab-oto/rndop")
install.packages(c("dplyr", "ggplot2", "giscoR", "magrittr", "sf", "shiny",
                   "shinythemes", "terra", "units", "zip"))
install.packages("igraph")   # required by calculate_locations()
```

To check everything resolves:

```r
for (p in c("dplyr","ggplot2","giscoR","magrittr","rndop","sf","shiny",
            "shinythemes","terra","units","zip","igraph")) {
  cat(sprintf("%-12s %s\n", p, requireNamespace(p, quietly = TRUE)))
}
```

---

## Credentials

NDOP requires a login. Everything that downloads data needs `NDOP_USER` and `NDOP_PWD` in the environment.

The usual way is to log in once and let `rndop` store them:

```r
rndop::isop_login()   # prompts, then writes to ~/.Renviron
```

⚠️ **Two gotchas:**

1. `isop_login()` keeps the session cookie in `.GlobalEnv` of *the R session that ran it*. Other processes — including a Shiny app started from a different session — cannot use it. Only the `~/.Renviron` entries carry over, and only to R sessions started **afterwards**.
2. It writes to `file.path(Sys.getenv("HOME"), ".Renviron")`. RStudio on Windows often sets `HOME` to your *Documents* folder while `Rscript` resolves it to your user profile, so the file may not be where a later session looks. Check with `path.expand("~/.Renviron")`.

If in doubt, write the file yourself:

```
NDOP_USER='your.username'
NDOP_PWD='your password'
```

This stores the password in plain text — restrict the file's permissions accordingly. Without credentials every taxon comes back Data Deficient with "No records found".

---

## Quick start

### Programmatic, one taxon

```r
library(ndopred)

assess_species("Lucanus cervus",
               recent_start = 2016, recent_end = 2025,
               comp_start   = 2006, comp_end   = 2015)
#>          Species Category Criteria EOO_km2 AOO_km2 Locations Trend_Perc
#> 1 Lucanus cervus       LC     None   60894    5132        92       99.2
```

### Programmatic, many taxa

```r
batch_assess(c("Lucanus cervus", "Bombina bombina", "Pulsatilla vernalis"))
```

### Step by step, with full control

```r
occ <- get_assessment_data("Bombina bombina")
attr(occ, "filter_counts")     # how many records survived each QC step

this_year <- as.numeric(format(Sys.Date(), "%Y"))

# Criterion B describes the CURRENT distribution (Guidelines 4.9)
eoo  <- calculate_eoo(occ,       year_start = this_year - 9)
aoo  <- calculate_aoo(occ,       year_start = this_year - 9)
locs <- calculate_locations(occ, year_start = this_year - 9)

# Criterion A compares the last 10 years with the 10 before them
trend <- calculate_trend(occ, window_years = 10)
pop   <- calculate_pop_metrics(occ, window_years = 10)

# Always check what the abundance figure rests on
pop$n_records_used; pop$n_records_counted; pop$ignored_terms

res <- summarize_assessment(
  species      = "Bombina bombina",
  eoo          = eoo,
  aoo          = aoo,
  trend        = trend,          # carries both AOO and EOO change
  locations    = locs,           # a plain numeric count
  pop_metrics  = pop,
  evaluate_pop = TRUE,           # are the counts fit to be used at all?
  year_last    = max(occ$ROK, na.rm = TRUE),
  n_records    = nrow(occ),
  generation_length = 3,
  # assessor judgements - see "What is not automated"
  severely_fragmented = FALSE,
  habitat_decline     = FALSE,
  d2_threat           = FALSE
)

res$result                    # category, criteria and metrics
res$details$d2_candidate      # restricted range flagged for review
res$details$nt_reasons        # why NT, if NT
```

### A map

```r
p <- plot_iucn("Bombina bombina", occ_data = occ, window = 10)
ggplot2::ggsave("bombina.png", p, width = 8, height = 6, dpi = 150, bg = "white")
```

---

## Function reference

**Fetching**

| Function | Purpose |
|---|---|
| `get_assessment_data()` | Download and quality-filter NDOP records; carries a `filter_counts` attribute recording survivors at each step |
| `clean_dates()`, `extract_year()` | Parse NDOP's several date encodings (`YYYYMMDD`, ISO, Czech `DD.MM.YYYY`, `Date`) |

**Metrics**

| Function | Purpose |
|---|---|
| `calculate_eoo()` | Extent of occurrence, minimum convex polygon (Guidelines 4.9) |
| `calculate_aoo()` | Area of occupancy on the 2×2 km reference grid, minimised over grid origins (4.10.2, 4.10.3) |
| `calculate_locations()` | Distance-cluster **surrogate** for threat-based locations (4.11) |
| `reconcile_eoo_aoo()` | Enforce EOO ≥ AOO (4.9) |
| `calculate_trend()` | AOO and EOO change, recent window vs. the equal-length window before it |
| `calculate_pop_metrics()` | Minimum observed counts, exponential decline rate, extreme-fluctuation diagnosis (4.3, 4.5.1, 4.7) |
| `get_mature_terms()` | The `POCITANO` values that assert reproductive capability (4.3) |

**Assessment**

| Function | Purpose |
|---|---|
| `summarize_assessment()` | The criteria A–D cascade; the core of the package |
| `assess_criterion_a()`, `assess_criterion_d2()` | Single criteria, standalone |
| `assess_species()`, `batch_assess()` | The whole chain, headless |

**Presentation**

| Function | Purpose |
|---|---|
| `plot_iucn()` | Distribution map with EOO hulls and AOO grids for both windows |
| `run_app()`, `run_bulk_app()` | The two dashboards |

Every function has a worked example: `?summarize_assessment`, `example("calculate_aoo")`, and so on.

---

## The bulk app in detail

Upload a CSV containing:

| Column | Required | Use |
|---|---|---|
| `TAXON` | **yes** | scientific name to query |
| `CELED` | no | family, copied to the output |
| `SKUPINA` | no | taxonomic group, copied to the output |
| `ID_TAXON` | no | builds a link to the species card on portal.nature.cz |

Separator and encoding are auto-detected by trying `read.csv2`/`read.csv` with CP1250 and UTF-8 and keeping whichever yields a `TAXON` column; if none does, the log names the columns it actually found.

Two settings:

- **Window length** — the recent window is the last *N* years and the comparison window is the *N* years immediately before it. Equal length and non-overlapping, so the change is like-for-like.
- **AOO grid size** — leave at 2 km. Guidelines 4.10.3 makes 2×2 km the reference scale for B2 and D2; anything else is logged as a warning because the result is no longer comparable with the thresholds.

Output columns include the automated category and criteria, both window ranges, AOO/EOO/locations for each window, percentage changes, record counts, last record year, and a **`D2 kandidát`** flag marking restricted taxa that need a threat assessment before VU D2 could apply.

---

## The single-species app in detail

Enter a name, press **Run Assessment**. You get an *automated baseline* plus a panel per criterion where every input behind it can be confirmed or changed; the **Expert Final Verdict** updates live.

**Two windows, two purposes.** *Current-distribution window* drives EOO, AOO and locations, because criterion B describes the current distribution (Guidelines 4.9). *Generation length* drives the criterion A reduction window — 10 years or 3 generations, whichever is longer, capped at 100 (4.4, 5.2). The app shows the resulting reduction window beneath the input.

**Abundance is shown for what it is.** The criterion C panel reports how many counted records actually fed `total_mature` and which life stages were skipped. Criteria C and D1 are auto-enabled only for vertebrates and plants *and* only when that basis is substantial — NDOP counts are minimum observed values, not population estimates.

**D2 is never pre-ticked.** A restricted range is flagged, but VU D2 requires you to affirm a plausible threat.

---

## What is *not* automated

Occurrence records cannot evidence every subcriterion. Rather than infer them, `ndopred` exposes them as arguments and flags candidates:

| Judgement | Argument | Why |
|---|---|---|
| Severe fragmentation, B(a) | `severely_fragmented` | Guidelines 4.8 |
| Habitat decline, B(b)(iii) | `habitat_decline` | NDOP holds no habitat data |
| Plausible threat for D2 | `d2_threat` | Guidelines 8: meeting the AOO/location figures "is not sufficient"; literal application "results in massive over-listing" |
| Criterion E | — | Requires a quantitative extinction-risk analysis |

Two further limits apply to national work:

- **No regional adjustment.** RE, and the downgrade/upgrade step for immigration from neighbouring populations, are defined in the *Guidelines for Application of IUCN Red List Criteria at Regional and National Levels v4.0*, which is not implemented here. `ndopred` produces a global-style category from Czech data; the regional step remains manual. The `RE` output is a **screen** for survey follow-up, not a determination.
- **Counts are minima.** `total_mature` and `max_subpop` are minimum observed values, not population estimates (Guidelines 4.3), and *locality* stands in for *subpopulation* (4.2).

---

## Methodology notes

A few behaviours that surprise people, and the reason for each:

- **A restricted, stable, unthreatened taxon is `LC`, not `NT`.** Guidelines 10.1 lists that case explicitly among those that should *not* be listed.
- **NT results say "nearly meets …"** rather than carrying a criteria code, because NT is defined by proximity to Vulnerable, not by criteria of its own.
- **A steep monotonic crash is a reduction, not an extreme fluctuation.** Section 4.7 requires a recurring pattern of increases and decreases; `calculate_pop_metrics()` reports direction reversals alongside the max/min ratio.
- **Small measured changes are not asserted as continuing decline.** A trend resampled from opportunistic records carries noise, and 4.9 warns that convex-hull EOO is unsuitable for comparing temporal estimates. Changes below `decline_tolerance` (default 5%) are ignored; set it to `0` for the literal "decline at any rate" reading.
- **`evaluate_pop` governs every use of the abundance data**, not just criteria C and D1. It also gates criterion A basis (b), B(b)(v) and B(c)(iv), because a *trend* derived from opportunistic counts is no more trustworthy than a *total* derived from them — less so, if anything, since it is confounded by recording effort (Guidelines 4.10.8). With it off, criterion A rests on the spatial trend alone, i.e. basis (c).
- **`"jedinci"` is not counted as mature individuals.** It is the commonest `POCITANO` value in NDOP but names no life stage, so it cannot assert maturity (4.3). The shortfall is reported rather than hidden; pass `mature_terms` to override it for a taxon where unqualified records are known to be adults.

---

## Testing

```r
devtools::test()
```

The suite covers the criteria cascade rule by rule, the metric functions, and drives **both Shiny apps end-to-end** against a synthetic NDOP fixture (`tests/testthat/helper-mock-ndop.R`) covering declining, stable, restricted, data-poor, historical-only and counted-population taxa.

---

## Logo

<img src="ndopred_hex.png" align="right" width="120" />

The `ndopred` logo showcases a silhouette of a [European stag beetle](https://portal.nature.cz/w/druh-6707#/) (*Lucanus cervus*) and [spring pasqueflower](https://portal.nature.cz/w/druh-39051#/) (*Pulsatilla vernalis*).

## Status

Version 0.2 (experimental). Automated categories are a **screening aid**: always validate them with expert opinion.

## References

IUCN (2012). *IUCN Red List Categories and Criteria: Version 3.1*, 2nd edition. Gland, Switzerland and Cambridge, UK: IUCN.

IUCN Standards and Petitions Committee (2024). *Guidelines for Using the IUCN Red List Categories and Criteria*, Version 16. <https://www.iucnredlist.org/documents/RedListGuidelines.pdf>

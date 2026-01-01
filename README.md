# ndopred: NDOP Red List Assessor

**ndopred** is an R package designed to streamline and automate IUCN Red List assessments for species data stored in the NDOP database (Nature Conservation Agency of the Czech Republic). 

It bridges the gap between raw occurrence data and formal IUCN categorization by calculating spatial metrics (EOO, AOO), analyzing population trends, and applying strict IUCN guidelines (Criteria A–E), including logic for Near Threatened (NT), Data Deficient (DD), and Regionally Extinct (RE) statuses.

## 🚀 Features

* **Automated Metrics:** Calculates EOO (Convex Hull), AOO (2x2km Grid), and number of Locations.
* **Population Analysis:** Evaluates population size, continuous decline, and extreme fluctuations (Criteria C/D).
* **Strict IUCN Logic:** Implements the full cascade of IUCN rules, including:
    * Criterion A (Population Reduction)
    * Criterion B (Geographic Range)
    * Criterion C (Small Population Size & Decline)
    * Criterion D (Very Small/Restricted Population)
    * **Near Threatened (NT):** Correctly identifies species close to thresholds.
    * **DD / RE detection:** Automatically flags Data Deficient or Regionally Extinct taxa based on record count and time-lag rules.
* **Interactive Dashboard:** A Shiny app for experts to review maps, visualize trends, and override automated categories with justification.

## 📦 Installation

You can install the development version of ndopred from GitHub (assuming repository exists):

```r
# install.packages("devtools")
devtools::install_github("yourusername/ndopred")
```

## ⚡ Quick Start
1. Run the Interactive App
The easiest way to use the package is via the built-in Shiny dashboard:

```r
library(ndopred)
```

Launch the assessment interface
```r
shiny::runApp(system.file("shinyapp", package = "ndopred"))
```

2. Programmatic Usage
You can also run assessments directly in R scripts:

```r
library(ndopred)
```
## 1. Fetch Data (Mock/API)
```r
species_name <- "Onthophagus medius"
occ_data <- get_assessment_data(species_name)
```

## 2. Calculate Spatial Metrics
```r
cutoff_year <- 2013 # Last 10 years
eoo <- calculate_eoo(occ_data, year_start = cutoff_year)
aoo <- calculate_aoo(occ_data, year_start = cutoff_year)
locs <- calculate_locations(occ_data, year_start = cutoff_year)
```
## 3. Calculate Trends & Pop Metrics
```r
trend <- calculate_trend(occ_data, window_years = 10)
pop <- calculate_pop_metrics(occ_data, window_years = 10)
```
## 4. Run the IUCN Logic Engine
(Set ```r evaluate_pop = TRUE``` for Vertebrates/Plants)
```r
result <- summarize_assessment(
  species = species_name,
  eoo = eoo, 
  aoo = aoo, 
  trend = trend, 
  locations = locs$n_locations, 
  pop_metrics = pop,
  evaluate_pop = TRUE,
  year_last = max(occ_data$ROK),
  n_records = nrow(occ_data)
)

print(result$result)
```
## 5. Run the R Shiny app
```r
ndopred::run_app()
```

## 🛠 Project Structure
R/: Core functions (calculate_eoo, summarize_assessment, etc.).

inst/shinyapp/: Source code for the Shiny dashboard.

tests/: Unit tests for IUCN logic integrity.

## ⚠️ Status
Version 0.1.1 (Experimental). Logic is compliant with IUCN Guidelines v15.1. Always validate automated results with expert opinion.

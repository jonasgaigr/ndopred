# Synthetic NDOP-shaped occurrence data for end-to-end tests.
#
# Coordinates are S-JTSK / Krovak East North (EPSG:5514), the CRS
# get_assessment_data() assumes for NDOP's X/Y fields. Czechia spans roughly
# X in [-900000, -430000] and Y in [-1230000, -930000] there.

ndop_row <- function(x, y, year, pocitano = "adulti", pocet = 1,
                     lokal = "Loc", kat_tax = "Brouci", celed = "Scarabaeidae") {
  data.frame(
    X = x, Y = y,
    DATUM_OD = sprintf("%04d0615", year),
    ROK = year,
    NEGATIV = 0, KATASTR = "KU", VEROH = 1,
    POCITANO = pocitano, POCET = pocet,
    NAZ_LOKAL = lokal, KAT_TAX = kat_tax, CELED = celed,
    stringsAsFactors = FALSE
  )
}

# Points scattered inside a square of `span` metres around (cx, cy).
scatter <- function(n, cx, cy, span, years, seed = 1, ...) {
  set.seed(seed)
  ndop_row(
    x = runif(n, cx - span / 2, cx + span / 2),
    y = runif(n, cy - span / 2, cy + span / 2),
    # years[sample.int(...)], not sample(years, ...): with a single year the
    # latter would sample from 1:year.
    year = years[sample.int(length(years), n, replace = TRUE)],
    lokal = paste0("L", seq_len(n) %% 7),
    ...
  )
}

# A fixture set covering the cases the assessment cascade has to separate.
mock_species <- function(name, year_now = as.numeric(format(Sys.Date(), "%Y"))) {
  recent <- seq(year_now - 9, year_now)
  comp <- seq(year_now - 19, year_now - 10)

  switch(
    name,
    # Wide range in the comparison window, collapsed to one corner recently.
    "Declining sp" = rbind(
      scatter(120, -700000, -1080000, 260000, comp, seed = 11),
      scatter(12, -700000, -1080000, 12000, recent, seed = 12)
    ),
    # Same wide range in both windows.
    "Stable widespread sp" = rbind(
      scatter(120, -700000, -1080000, 260000, comp, seed = 21),
      scatter(120, -700000, -1080000, 260000, recent, seed = 22)
    ),
    # Small range, stable, no threat information: the D2 over-listing case.
    "Restricted stable sp" = rbind(
      scatter(40, -700000, -1080000, 6000, comp, seed = 31),
      scatter(40, -700000, -1080000, 6000, recent, seed = 32)
    ),
    # Nothing for 60 years.
    "Ghost sp" = scatter(20, -700000, -1080000, 60000,
                         seq(year_now - 70, year_now - 60), seed = 41),
    # Two records only.
    "Barely known sp" = scatter(2, -700000, -1080000, 4000, recent, seed = 51),
    # Counted population, declining monotonically by an order of magnitude.
    "Counted declining sp" = do.call(rbind, lapply(seq_along(recent), function(i) {
      scatter(6, -700000, -1080000, 40000, recent[i], seed = 60 + i,
              pocitano = "adulti", pocet = max(1, round(400 * 0.75^(i - 1))),
              kat_tax = "Savci")
    })),
    NULL
  )
}

local_mock_ndop <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    ndop_download = function(species, ...) mock_species(species),
    .package = "rndop", .env = env
  )
}

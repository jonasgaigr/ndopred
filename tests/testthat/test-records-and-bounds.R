# Phase 2: snapshots, record flags (nothing deleted) and AOO/EOO bounds from
# positional uncertainty. Rules decided with AOPK on 2026-10-15 (gap
# analysis D2, D3, D8, D12, D13).

raw_records <- function(...) {
  base <- data.frame(
    ID_NALEZ = 1:8,
    ID_LOKAL = 101:108,
    DRUH = c(rep("Bombina bombina", 7), "Bombina bombina \u00d7 variegata"),
    X = c(-700000, -701000, -702000, -703000, -704000, -705000, -706000, -707000),
    Y = -1080000,
    DATUM_OD = 20200601, DATUM_DO = 20200601,
    NEGATIV = 0, KATASTR = "KU",
    VEROH = c(0, 1, 3, 6, 9, 1, 1, 1),
    VALIDACE = c("chybn\u00fd z\u00e1znam", "", "", "", "", "", "", ""),
    CXPRESNOST = 0,
    ZDROJ = "Vlastn\u00ed n\u00e1lezy",
    stringsAsFactors = FALSE
  )
  utils::modifyList(base, list(...))
}

# ---------------------------------------------------------------- flags

test_that("VEROH codes follow the AOPK code list (D2)", {
  f <- flag_records(raw_records(), species = "Bombina bombina")
  by_id <- function(i) f[f$ID_NALEZ == i, ]
  # 0: not yet validated - used; a proposed validation mark is only flagged.
  expect_true(by_id(1)$include)
  expect_true(by_id(1)$flag_validation_proposed)
  # 1: credible - used.
  expect_true(by_id(2)$include)
  # 6 and 9: erroneous - excluded.
  expect_false(by_id(4)$include)
  expect_match(by_id(4)$exclusion_reason, "VEROH 6")
  expect_false(by_id(5)$include)
  expect_match(by_id(5)$exclusion_reason, "VEROH 9")
})

test_that("VEROH 3 is always used, and flagged only above 10 % of the taxon's records (D8)", {
  # One VEROH 3 in seven own records = 14 %: used, flagged.
  f <- flag_records(raw_records(), species = "Bombina bombina")
  expect_true(f$include[f$ID_NALEZ == 3])
  expect_true(f$flag_less_credible[f$ID_NALEZ == 3])
  expect_true(attr(f, "less_credible_flagged"))
  # At a 20 % threshold the same record is used and not flagged.
  g <- flag_records(raw_records(), species = "Bombina bombina", less_credible_share = 0.20)
  expect_true(g$include[g$ID_NALEZ == 3])
  expect_false(g$flag_less_credible[g$ID_NALEZ == 3])
  # Exactly 10 %: no flag ("no flag if under or at 10 %").
  ten <- data.frame(ID_NALEZ = 1:10, DRUH = "A b", X = -700000 - 1000 * (1:10), Y = -1080000,
                    DATUM_OD = 20200601, DATUM_DO = 20200601, NEGATIV = 0, KATASTR = "KU",
                    VEROH = c(3, rep(1, 9)))
  h <- flag_records(ten, species = "A b")
  expect_equal(attr(h, "less_credible_share"), 0.1)
  expect_false(any(h$flag_less_credible))
  expect_true(all(h$include))
})

test_that("Records of another taxon are excluded: names match exactly (D13)", {
  f <- flag_records(raw_records(), species = "Bombina bombina")
  hybrid <- f[f$ID_NALEZ == 8, ]
  expect_false(hybrid$include)
  expect_match(hybrid$exclusion_reason, "different taxon")
  # Without a queried name nothing is checked.
  expect_false(any(flag_records(raw_records())$flag_taxon_mismatch))
})

test_that("Negative findings, missing and implausible positions are excluded with a reason", {
  r <- raw_records(NEGATIV = c(1, rep(0, 7)), X = c(-700000, NA, -702000, -703000, 0,
                                                   -705000, -706000, -707000))
  f <- flag_records(r, species = "Bombina bombina")
  expect_match(f$exclusion_reason[1], "negative finding")
  expect_match(f$exclusion_reason[2], "no coordinates")
  expect_true(f$flag_coords_suspect[5])
  expect_equal(nrow(f), nrow(r))            # nothing deleted
})

test_that("Date ranges carry year_min and year_max; windows need the whole range", {
  r <- raw_records(DATUM_OD = c(19900101, rep(20200601, 7)),
                   DATUM_DO = c(20151231, rep(20200601, 7)))
  f <- flag_records(r, species = "Bombina bombina")
  expect_equal(c(f$year_min[1], f$year_max[1]), c(1990, 2015))
  expect_true(f$flag_date_range[1])
  # 1990-2015 is in neither 2006-2015 nor 2016-2025.
  d <- data.frame(year = f$year_min, year_min = f$year_min, year_max = f$year_max)
  expect_equal(nrow(filter_years(d[1, ], 2006, 2015)), 0)
  expect_equal(nrow(filter_years(d[1, ], 1990, 2015)), 1)
})

test_that("Suspected duplicates and literature sources are flagged, never excluded", {
  r <- raw_records(X = c(-700000, -700000, -702000, -703000, -704000, -705000, -706000, -707000),
                   VEROH = 1,
                   ZDROJ = c(rep("Vlastn\u00ed n\u00e1lezy", 6),
                             "NOV\u00c1K J. (2008) P\u0159\u00edsp\u011bvek k fauna. Klapalekiana 44: 1-10.",
                             "Vlastn\u00ed n\u00e1lezy"))
  f <- flag_records(r, species = "Bombina bombina")
  expect_true(f$flag_duplicate[2])
  expect_true(f$include[2])
  expect_true(f$flag_literature[7])
  expect_true(f$include[7])
})

# ------------------------------------------------------------ snapshots

test_that("A snapshot keeps the download unchanged and can be rerun (D12)", {
  poly <- sf::st_sf(idx_nd_lok = 102,
                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                      c(-702500, -1081000), c(-698500, -1081000), c(-698500, -1079000),
                      c(-702500, -1079000), c(-702500, -1081000)))), crs = 5514))
  r <- raw_records()
  testthat::local_mocked_bindings(
    ndop_download = function(species, ...) list(r, list(poly)),
    .package = "rndop"
  )
  dir <- withr::local_tempdir()
  snap <- ndop_snapshot("Bombina bombina", dir = dir)
  expect_true(file.exists(snap$meta$path))
  expect_identical(read_ndop_snapshot(snap$meta$path)$table, r)
  expect_equal(snap$meta$query, "Bombina bombina")

  live <- get_assessment_data("Bombina bombina", snapshot_dir = dir)
  rerun <- get_assessment_data("Bombina bombina", snapshot = snap$meta$path)
  # Same records and flags (the attributes differ: download time, file).
  expect_equal(sf::st_drop_geometry(live), sf::st_drop_geometry(rerun), ignore_attr = TRUE)
  expect_equal(sf::st_coordinates(live), sf::st_coordinates(rerun))
  # Record geometries are joined by ID_LOKAL (rndop's idx_nd_lok).
  expect_equal(live$geom_type[live$ID_LOKAL == 102], "POLYGON")
  expect_s3_class(attr(live, "record_geometries"), "sf")
  expect_false(is.null(attr(live, "snapshot")$path))
})

test_that("The default snapshot folder is Documents/ndop_snapshots, overridable", {
  withr::local_envvar(NDOPRED_SNAPSHOT_DIR = "")
  expect_match(default_snapshot_dir(), "Documents[/\\\\]ndop_snapshots$")
  withr::local_envvar(NDOPRED_SNAPSHOT_DIR = "D:/elsewhere")
  expect_equal(default_snapshot_dir(), "D:/elsewhere")
})

# --------------------------------------------------------- AOO and EOO bounds

pts3035 <- function(x, y, prec = 0, ...) {
  sf::st_as_sf(data.frame(x = x, y = y, precision_m = prec, ...), coords = c("x", "y"), crs = 3035)
}

test_that("Exact positions give min = best = max", {
  p <- pts3035(c(4600500, 4610500, 4620500), 2900500)
  a <- calculate_aoo(p)
  expect_equal(c(a$area_km2_min, a$area_km2, a$area_km2_max), rep(12, 3))
  expect_equal(a$n_uncertain, 0)
})

test_that("Overlapping precision discs lower the minimum; spreading discs raise the maximum", {
  near <- calculate_aoo(pts3035(c(4600500, 4603000), 2900500, prec = 3000))
  expect_equal(near$area_km2, 8)
  expect_equal(near$area_km2_min, 4)
  same <- calculate_aoo(pts3035(c(4600500, 4600500, 4600500), 2900500, prec = 3000))
  expect_equal(same$area_km2, 4)
  expect_equal(same$area_km2_max, 12)
})

test_that("A record polygon spanning several cells is placed by the polygon", {
  p <- pts3035(4601000, 2901000, prec = 0, ID_LOKAL = 7)
  big <- sf::st_sf(ID_LOKAL = 7, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(4599000, 2899000), c(4605000, 2899000), c(4605000, 2903000), c(4599000, 2903000),
    c(4599000, 2899000)))), crs = 3035))
  a <- calculate_aoo(p, n_shifts = 1, geometries = big)
  expect_equal(a$area_km2, 4)
  expect_equal(a$n_uncertain, 1)
  expect_equal(a$area_km2_max, 4)   # one record occupies one cell...
  q <- rbind(p, p)
  b <- calculate_aoo(q, n_shifts = 1, geometries = big)
  expect_equal(b$area_km2_max, 8)   # ...two records in the polygon, up to two
})

test_that("Adding records never lowers observed AOO; excluding never raises it", {
  set.seed(42)
  for (k in 1:25) {
    n <- sample(3:30, 1)
    p <- pts3035(runif(n, 4600000, 4640000), runif(n, 2900000, 2930000),
                 prec = sample(c(0, 10, 500, 3000), n, replace = TRUE))
    base <- calculate_aoo(p)
    more <- calculate_aoo(rbind(p, pts3035(runif(3, 4600000, 4640000), runif(3, 2900000, 2930000))))
    fewer <- calculate_aoo(p[-1, ])
    expect_gte(more$area_km2, base$area_km2)
    expect_lte(fewer$area_km2, base$area_km2)
    expect_lte(base$area_km2_min, base$area_km2)
    expect_gte(base$area_km2_max, base$area_km2)
  }
})

test_that("EOO bounds bracket the best estimate", {
  p <- pts3035(c(4600000, 4650000, 4620000, 4640000), c(2900000, 2900000, 2950000, 2930000),
               prec = c(0, 0, 0, 5000))
  e <- calculate_eoo(p)
  expect_gte(e$area_km2_max, e$area_km2)
  expect_lte(e$area_km2_min, e$area_km2)     # the 5 km record is left out
})

test_that("EOO bounds accept a geometry layer mixing points, lines and polygons", {
  # Real NDOP layers are mixed; st_coordinates() refuses those as a whole.
  p <- pts3035(c(4600000, 4650000, 4620000), c(2900000, 2900000, 2950000), ID_LOKAL = 1:3)
  mixed <- sf::st_sf(ID_LOKAL = 1:3, geometry = sf::st_sfc(
    sf::st_point(c(4600000, 2900000)),
    sf::st_linestring(rbind(c(4650000, 2900000), c(4660000, 2905000))),
    sf::st_polygon(list(rbind(c(4618000, 2948000), c(4622000, 2948000), c(4622000, 2955000),
                              c(4618000, 2955000), c(4618000, 2948000)))),
    crs = 3035))
  e <- calculate_eoo(p, geometries = mixed)
  expect_gt(e$area_km2_max, e$area_km2)
  a <- calculate_aoo(p, geometries = mixed)
  expect_gte(a$area_km2_max, a$area_km2)
})

test_that("AOO uncertainty widens the plausible range of categories", {
  # Best AOO 520 km2 (no B2 gate); with positional error as low as 480 km2,
  # the EN B2 area threshold is plausible.
  inp <- assessment_inputs(eoo_km2 = 30000, aoo_km2 = 520, aoo_km2_min = 480, aoo_km2_max = 560,
                           n_records = 200, n_current = 150, year_last = 2026,
                           evidence = list(severely_fragmented = TRUE, decline = c(iii = TRUE)))
  p <- categorise(assess_criteria(inp, ndopred_config(2026)))
  expect_equal(p$category, "VU")
  expect_equal(p$range, "VU-EN")
})

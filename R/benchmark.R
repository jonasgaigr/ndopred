#' Benchmark the Spatial Metrics Against ConR, redlistr and red
#'
#' Runs the records of one taxon's current window through ndopred and
#' through established tools, and tabulates EOO, AOO, locations and the
#' criterion B category side by side. The tools are suggested packages: a
#' tool that is not installed, or that fails, gets a row saying so.
#'
#' @section What is compared:
#' Every tool gets the same points: the included records of the current
#' window (as in [taxon_metrics()]), one point per record.
#' * **EOO**: a minimum convex polygon in every tool (Guidelines 4.9).
#'   ndopred and redlistr work in EPSG:3035, ConR in a cylindrical
#'   equal-area projection, red in its own; small differences come from the
#'   projection.
#' * **AOO**: 2 x 2 km cells (Guidelines 4.10). ndopred minimises over grid
#'   origins (4.10.2). redlistr and ConR minimise over `n_jitter` random
#'   origins; redlistr's bottom-1 % rule is switched off, since it is a rule
#'   for ecosystems (Red List of Ecosystems), not species. red uses one fixed
#'   grid. ndopred's value is for points; its record-precision bounds are
#'   in [taxon_metrics()].
#' * **Locations**: ConR counts the occupied cells of a
#'   `cell_size_locations` km fixed grid, its default without a threat
#'   layer. ndopred does not derive locations from records (Guidelines
#'   4.11); its row shows the distance clusters, a diagnostic only.
#' * **Category B**: ConR assumes continuing decline when none is supplied
#'   and uses subcriteria (a) and (b) only. ndopred appears twice: the best
#'   estimate (unknown subcriteria not met) and the plausible maximum
#'   (unknown subcriteria met, Guidelines 3.2). The plausible maximum is the
#'   value comparable with ConR.
#'
#' @param occ An `sf` object from [get_assessment_data()].
#' @param config Object from [ndopred_config()].
#' @param taxon Character. Taxon name, for the output.
#' @param tools Which tools to run besides ndopred.
#' @param n_jitter Number of random grid origins for the AOO of redlistr and
#'   ConR.
#' @param cell_size_locations Cell size in km of ConR's location grid.
#' @param result Optional result of [assess_taxon()] for `occ`, to reuse.
#' @return A data frame, one row per tool: `Taxon`, `Tool`, `Version`,
#'   `Points`, `EOO_km2`, `AOO_km2`, `Locations`, `Category_B`, `Criteria_B`,
#'   `EOO_diff_pct` and `AOO_diff_pct` (relative to ndopred's best
#'   estimate), and `Method`.
#' @seealso `inst/benchmark/benchmark_tools.R` runs it on the pilot taxa.
#' @export
compare_tools <- function(occ, config, taxon = NA_character_,
                          tools = c("ConR", "redlistr", "red"),
                          n_jitter = 20, cell_size_locations = 10, result = NULL) {
  stopifnot(inherits(config, "ndopred_config"))
  bad <- setdiff(tools, c("ConR", "redlistr", "red"))
  if (length(bad)) stop("unknown tools: ", paste(bad, collapse = ", "), call. = FALSE)
  if (is.null(result)) result <- assess_taxon(occ, taxon, config)
  m <- result$metrics
  w <- config$windows
  pts <- occ
  if ("include" %in% names(pts)) pts <- suppressWarnings(pts[pts$include %in% TRUE, , drop = FALSE])
  pts <- .window_records(add_year_column(pts), w$recent_start, w$recent_end)
  xy <- if (nrow(pts)) sf::st_coordinates(sf::st_transform(sf::st_geometry(pts), 3035))[, 1:2, drop = FALSE] else
    matrix(numeric(0), ncol = 2)
  n <- nrow(xy)

  row <- function(tool, eoo = NA_real_, aoo = NA_real_, locations = NA_real_,
                  category = NA_character_, criteria = NA_character_, method = NA_character_) {
    data.frame(Taxon = taxon, Tool = tool, Version = .tool_version(sub(" .*", "", tool)),
               Points = n, EOO_km2 = round(as.numeric(eoo), 1), AOO_km2 = as.numeric(aoo),
               Locations = as.numeric(locations), Category_B = category,
               Criteria_B = criteria, Method = method, stringsAsFactors = FALSE)
  }
  b_at <- function(s) {
    W <- result$assessment$scenarios[[s]]
    if (is.null(W)) return(c(NA_character_, NA_character_))
    level <- c(W$B$B1$level, W$B$B2$level)
    if (all(is.na(level))) return(c("LC or NT", NA_character_))
    cat <- .max_cat(level)
    c(cat, .parts_at(W, cat)$B$iucn)
  }
  best <- b_at("working")
  high <- b_at("high")
  out <- list(
    row("ndopred (best estimate)", m$eoo_new, m$aoo_new, m$clusters_new, best[1], best[2],
        "MCP in EPSG:3035; AOO minimised over grid origins; locations: 5 km clusters (diagnostic only)"),
    row("ndopred (plausible maximum)", m$eoo_new, m$aoo_new, m$clusters_new, high[1], high[2],
        "as above, with unknown subcriteria met (Guidelines 3.2)")
  )
  if (n) {
    ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(as.data.frame(xy), coords = 1:2, crs = 3035), 4326))
    for (tool in tools) {
      res <- tryCatch(
        switch(tool,
               ConR = .run_conr(ll, n_jitter, cell_size_locations),
               redlistr = .run_redlistr(xy, n_jitter),
               red = .run_red(ll)),
        error = function(e) list(method = paste("failed:", conditionMessage(e))))
      out[[length(out) + 1]] <- do.call(row, c(list(tool), res))
    }
  }
  tab <- do.call(rbind, out)
  ref <- tab[1, ]
  diff <- function(x, r) if (is.na(r) || r <= 0) rep(NA_real_, length(x)) else round(100 * (x - r) / r, 1)
  tab$EOO_diff_pct <- diff(tab$EOO_km2, ref$EOO_km2)
  tab$AOO_diff_pct <- diff(tab$AOO_km2, ref$AOO_km2)
  tab[, c("Taxon", "Tool", "Version", "Points", "EOO_km2", "AOO_km2", "Locations",
          "Category_B", "Criteria_B", "EOO_diff_pct", "AOO_diff_pct", "Method")]
}

.tool_version <- function(pkg) {
  if (pkg == "ndopred") return(as.character(utils::packageVersion("ndopred")))
  if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(pkg))
}

# The tools print progress bars and messages; keep them out of the output.
.quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressWarnings(suppressMessages(expr)))
  out
}

.need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop(pkg, " is not installed", call. = FALSE)
}

.run_conr <- function(ll, n_jitter, cell_size_locations) {
  .need("ConR")
  XY <- data.frame(ddlat = ll[, 2], ddlon = ll[, 1], tax = "taxon")
  eoo <- .quiet(ConR::EOO.computing(XY, show_progress = FALSE))$eoo
  aoo <- .quiet(ConR::AOO.computing(XY, cell_size_AOO = 2, nbe.rep.rast.AOO = n_jitter,
                                    show_progress = FALSE))$aoo
  loc <- .quiet(ConR::locations.comp(XY, cell_size_locations = cell_size_locations,
                                     show_progress = FALSE))$locations$locations
  # For the category an EOO below the AOO (or no hull, under three points)
  # is set to the AOO, as in ndopred (Guidelines 4.9); the row keeps ConR's
  # own value.
  eoo_b <- if (is.na(eoo)) aoo else max(eoo, aoo)
  b <- .quiet(ConR::cat_criterion_b(EOO = eoo_b, AOO = aoo, locations = loc))
  list(eoo = eoo, aoo = aoo, locations = loc, category = b$ranks_B, criteria = b$cats_code,
       method = sprintf(paste("convex hull (cea); AOO min over %d random origins;",
                              "locations: %g km fixed grid; continuing decline assumed"),
                        n_jitter, cell_size_locations))
}

.run_redlistr <- function(xy, n_jitter) {
  .need("redlistr")
  # Its grid fails on one point and returns no cells for two.
  if (nrow(unique(xy)) < 3) return(list(method = "not run: redlistr needs three distinct points"))
  pts <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = 3035)
  eoo <- .quiet(redlistr::getEOO(pts))@EOO
  a <- .quiet(redlistr::getAOO(pts, cell_size = 2000, bottom_1pct_rule = FALSE,
                               n_jitter = n_jitter))
  list(eoo = eoo, aoo = 4 * a@AOO,
       method = sprintf(paste("MCP in EPSG:3035; AOO min over %d random origins",
                              "(only units of 150 cells or fewer are jittered);",
                              "bottom-1 %% rule off"), n_jitter))
}

.run_red <- function(ll) {
  .need("red")
  d <- as.matrix(ll[, 1:2, drop = FALSE])
  eoo <- if (nrow(unique(d)) >= 3) .quiet(red::eoo(d)) else NA_real_
  list(eoo = eoo, aoo = .quiet(red::aoo(d)),
       method = "MCP; AOO on one fixed 2 km grid")
}

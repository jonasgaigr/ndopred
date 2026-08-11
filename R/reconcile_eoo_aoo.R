#' Reconcile EOO with AOO per IUCN Guidelines Section 4.9
#'
#' "If EOO is less than AOO, EOO should be changed to make it equal to AOO to
#' ensure consistency with the definition of AOO as an area within EOO."
#' This also resolves cases where EOO cannot be computed directly (e.g. fewer
#' than 3 occurrence points for a convex hull, where \code{calculate_eoo()}
#' returns \code{NA}) by falling back to AOO, consistent with the same rule.
#'
#' @param eoo_km2 Numeric. Estimated EOO in km2 (may be NA).
#' @param aoo_km2 Numeric. Estimated AOO in km2 (may be NA).
#' @return Numeric. The reconciled EOO in km2.
#' @export
reconcile_eoo_aoo <- function(eoo_km2, aoo_km2) {
  if (is.na(aoo_km2) || aoo_km2 == 0) return(eoo_km2)
  if (is.na(eoo_km2) || eoo_km2 < aoo_km2) return(aoo_km2)
  return(eoo_km2)
}

#' Get List of Mature Life Stages
#'
#' Defines the `POCITANO` values from NDOP that assert reproductive capability,
#' i.e. IUCN "mature individuals" (Guidelines v16 section 4.3: individuals
#' "known, estimated or inferred to be capable of reproduction").
#'
#' @section Terms deliberately excluded:
#' * `"jedinci"` ("individuals") and `"lodyhy"`/`"prýty"`/`"trsy"` (stems,
#'   shoots, tufts) name no life stage, so they cannot assert maturity. These
#'   are the *commonest* counted values in NDOP, so excluding them can leave
#'   `total_mature` resting on a small minority of records - which is why
#'   [calculate_pop_metrics()] reports `ignored_terms` and the record counts
#'   behind the total, and accepts a `mature_terms` override.
#' * `"sterilní ..."` and `"semenáčky"` (sterile stages, seedlings) are by
#'   definition not reproductive.
#' * `"mrtví/mrtvé ..."`, `"fragmenty"`, `"požerky"`, `"výletové otvory"`
#'   (dead individuals, fragments, feeding traces, emergence holes) are not
#'   living individuals at all.
#' * Mixed classes such as `"plodní a sterilní jedinci"` pool reproductive and
#'   non-reproductive individuals into one count, so they overstate maturity.
#'
#' @return A character vector of `POCITANO` values.
#' @seealso [calculate_pop_metrics()], which reports the counted terms it had
#'   to skip and accepts a `mature_terms` override.
#' @references IUCN Standards and Petitions Committee (2024), section 4.3.
#' @examples
#' head(get_mature_terms())
#'
#' # The commonest NDOP value is absent by design: it names no life stage.
#' "jedinci" %in% get_mature_terms()
#'
#' # Override it for a taxon whose unqualified records are known to be adults.
#' occ <- data.frame(ROK = 2025, POCET = 12, POCITANO = "jedinci",
#'                   NAZ_LOKAL = "Site A")
#' calculate_pop_metrics(occ, year_now = 2025)$total_mature            # NA
#' calculate_pop_metrics(
#'   occ, year_now = 2025,
#'   mature_terms = c(get_mature_terms(), "jedinci")
#' )$total_mature                                                      # 12
#' @export
get_mature_terms <- function() {
  c(
    # --- ANIMALS ---
    "adulti",
    "adulti s ml\u00e1\u010faty",
    "adulti s vejci",
    "adulti v prost\u00e9m \u0161at\u011b",
    "adulti ve svatebn\u00edm \u0161at\u011b",
    "amplexus",          # Count as 2
    "d\u011blnice",
    "dosp\u011blci",
    "far\u00e1tn\u00ed imaga",
    "imaga",
    "kopulace",          # Count as 2
    "samci",             # Male
    "samice",            # Female
    "vychovan\u00ed samci",   # Male

    # --- PLANTS ---
    "kvetouc\u00ed a plodn\u00e9 lodyhy",
    "kvetouc\u00ed a plodn\u00e9 trsy",
    "kvetouc\u00ed a plodn\u00ed jedinci",
    "kvetouc\u00ed jedinci",
    "kvetouc\u00ed lodyhy",
    "kvetouc\u00ed pr\u00fdty",
    "kvetouc\u00ed st\u00e9bla",
    "kvetouc\u00ed trsy",
    "zral\u00e9 tobolky",

    # Fruiting stages. A plant in fruit has demonstrably reproduced, so these
    # are mature under section 4.3 just as the flowering stages above are.
    # The list previously held only the combined "kvetoucí a plodné ..."
    # forms, so records counted as fruiting alone were silently discarded
    # (e.g. Cypripedium calceolus).
    "plodn\u00e9 lodyhy",
    "plodn\u00e9 pr\u00fdty",
    "plodn\u00e9 st\u00e9bla",
    "plodn\u00e9 trsy",
    "plodn\u00ed jedinci"
  )
}

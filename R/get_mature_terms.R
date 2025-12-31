#' Get List of Mature Life Stages
#'
#' Defines the specific terms from NDOP that represent reproductive capability
#' for both animals and plants.
#'
#' @return A character vector of terms.
#' @export
get_mature_terms <- function() {
  c(
    # --- ANIMALS ---
    "adulti",
    "adulti s mláďaty",
    "adulti s vejci",
    "adulti v prostém šatě",
    "adulti ve svatebním šatě",
    "amplexus",          # Count as 2
    "dělnice",
    "dospělci",
    "farátní imaga",
    "imaga",
    "kopulace",          # Count as 2
    "samci",             # Male
    "samice",            # Female
    "vychovaní samci",   # Male

    # --- PLANTS ---
    "kvetoucí a plodné lodyhy",
    "kvetoucí a plodné trsy",
    "kvetoucí a plodní jedinci",
    "kvetoucí jedinci",
    "kvetoucí lodyhy",
    "kvetoucí prýty",
    "kvetoucí stébla",
    "kvetoucí trsy",
    "zralé tobolky"
  )
}

#' Convert behaviour abbreviations to full names
#'
#' Translates standardised behaviour abbreviations into their corresponding
#' descriptive behaviour names.
#'
#' @param abbrv A character vector of behaviour abbreviations.
#'
#' @return
#' A character vector of the same length as `abbrv`, containing the
#' corresponding behaviour names. Unknown abbreviations return `NA`.
#'
#' @examples
#' behaviour_abbrv_to_name(c("FO", "R", "TF", "Ni"))
#'
#' @export
behaviour_abbrv_to_name <- function(abbrv) {
  recode(
    x    = abbrv,
    from = c(
      "FO", "M", "R", "TF", "TA", "TS", "SO", "BR",
      "LB", "FL", "SP", "OT", "BS", "BF", "BB", "H",
      "Ni", "NI"
    ),
    to   = c(
      "foraging",
      "milling",
      "resting",
      "travelling fast",
      "travelling average",
      "travelling slow",
      "socializing",
      "bow riding",
      "lobe tailing",
      "fluking",
      "spy hopping",
      "other",
      "breaching side",
      "breaching front",
      "breaching back",
      "hiding",
      "not identified",
      "not identified"
    ),
    .no_match = NA_character_
  )
}

#' Convert age-class abbreviations to full names
#'
#' Translates standardised age-class abbreviations into their corresponding
#' descriptive category names.
#'
#' @param abbrv A character vector of age-class abbreviations.
#'
#' @return
#' A character vector of the same length as `abbrv`, containing the
#' corresponding age-class names. Unknown abbreviations return `NA`.
#'
#' @examples
#' age_abbrv_to_name(c("Ad", "J", "Ni"))
#'
#' @export
age_abbrv_to_name <- function(abbrv) {
  recode(
    x    = abbrv,
    from = c("Max", "Ad", "J", "C", "NB", "Ni", "NI"),
    to   = c(
      "total",
      "adult",
      "juvenile",
      "calf",
      "newborn",
      "not identified",
      "not identified"
    ),
    .no_match = NA_character_
  )
}

#' Convert reaction abbreviations to full names
#'
#' Translates abbreviations describing cetacean reactions to a vessel into
#' descriptive reaction categories.
#'
#' Non-informative reaction codes (`"Ni"`, `"NI"`) are recoded to `"other"`.
#'
#' @param abbrv A character vector of reaction abbreviations.
#'
#' @return
#' A character vector of the same length as `abbrv`, containing the
#' corresponding reaction descriptions. Unknown abbreviations return `NA`.
#'
#' @examples
#' reaction_abbrv_to_name(c("A", "I", "E", "NI"))
#'
#' @export
reaction_abbrv_to_name <- function(abbrv) {
  recode(
    x    = abbrv,
    from = c("A", "I", "E", "Ni", "NI"),
    to   = c(
      "approach",
      "indifference",
      "evasive",
      "other",
      "other"
    ),
    .no_match = NA_character_
  )
}

#' Convert species abbreviations to full species names
#'
#' Translates uppercase species abbreviations (e.g. `"BP"`, `"GME"`) into their
#' corresponding full scientific species names.
#'
#' The mapping is derived from the internal dataset
#' [azores.cetaceans::species], using the `abb_sp` and `species` columns.
#'
#' @param abb_sp A character vector of species abbreviations, typically in
#'   uppercase.
#'
#' @return
#' A character vector of full scientific species names. Abbreviations not found
#'   in the reference table return `NA`.
#'
#' @examples
#' species_abbrv_to_name(c("BP", "GME", "OO"))
#'
#' species_abbrv_to_name(c("XX", "BP"))
#'
#' @export
species_abbrv_to_name <- function(abb_sp) {
  lookup <-
    azores.cetaceans::species |>
    dplyr::transmute(abb_sp  = base::toupper(.data$abb_sp),
                     species = .data$species)

  recode(
    x = base::toupper(abb_sp),
    from = lookup$abb_sp,
    to = lookup$species,
    .no_match = NA_character_
  )
}

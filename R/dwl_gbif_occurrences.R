#' Download occurrence records from GBIF using dismo
#'
#' [dwl_gbif_occurrences()] retrieves occurrence records for one or more species
#' from the GBIF database using the \code{dismo::gbif()} interface. Species names
#' are split into genus and species components internally before querying GBIF.
#'
#' @param species A character vector of species names in binomial format,
#'   e.g. `"Orcinus orca"` or `c("Orcinus orca", "Physeter macrocephalus")`.
#' @param extent A numeric vector defining the bounding box of the region to be
#'   queried, with four values in the following order:
#'   most western longitude, eastern longitude, southern latitude, and northern
#'   latitude. Defaults to the Azores region
#'   (`c(-34, -21, 34, 42)`). Units are in decimal degrees.
#'
#' @returns A tibble of GBIF occurrence records. Each row represents a single
#'   occurrence, and the returned columns include:
#'   \itemize{
#'     \item \code{species}: Species name
#'     \item \code{eventDate}: Date of the occurrence record
#'     \item \code{lon}: Longitude of the record
#'     \item \code{lat}: Latitude of the record
#'   }
#'
#' @details
#' The function splits each species name into genus and species components,
#' queries GBIF for each species using spatial constraints, removes zero
#' coordinates, and combines all results into a single tibble.
#'
#' @examples
#' if (FALSE) {
#'   dwl_gbif_occurrences("Orcinus orca")
#' }
#'
#' @export
dwl_gbif_occurrences <- function(species, extent = c(-34, -21, 34, 42)) {
  m <- matrix(unlist(strsplit(species, split = " ")), byrow = TRUE, ncol = 2)
  colnames(m) <- c("genus", "species")
  species_tbl <- tibble::as_tibble(m)

  purrr::pmap(
    species_tbl,
    dismo::gbif,
    geo = TRUE,
    removeZeros = TRUE,
    ext = extent,
    .progress = TRUE
  ) |>
    purrr::list_rbind() |>
    tibble::as_tibble() |>
    dplyr::select(c("species", "eventDate", "lon", "lat"))
}

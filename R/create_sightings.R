#' Read raw sightings from MSP Excel file
#'
#' Reads the raw MSP Excel file and adds a unique `sighting_id` column derived
#' from the `Sighting` field.
#'
#' @param path Path to the MSP Excel file.
#' @param col_names Logical indicating whether the file contains column names.
#'   Defaults to `TRUE`.
#' @param skip Number of rows to skip before reading data. Defaults to `1L`.
#' @param sheet Sheet index or name to read from the Excel file. Defaults to `4`.
#'
#' @return
#' A tibble containing the raw sightings data with an added `sighting_id`
#'   column.
#'
#' @export
read_raw_sightings <- function(
    path,
    col_names = TRUE,
    skip      = 1L,
    sheet    = 4
) {

  msp_data_raw <-
    readxl::read_xlsx(
      path,
      col_names = col_names,
      skip      = skip,
      sheet    = sheet
    )

  msp_data_raw |>
    dplyr::mutate(
      sighting_id = base::as.integer(.data$Sighting)
    )
}


#' Create sightings table from MSP Excel file
#'
#' Reads the MSP Excel file and constructs a cleaned `sightings` table with
#' harmonised coordinates, datetimes, and species information.
#'
#' @param path Path to the MSP Excel file.
#' @param col_names Logical indicating whether the file contains column names.
#'   Defaults to `TRUE`.
#' @param skip Number of rows to skip before reading data. Defaults to `1L`.
#' @param sheet Sheet index or name to read from the Excel file. Defaults to `4`.
#'
#' @return
#' A tibble containing one row per sighting, with derived datetime fields and
#' standardised species names.
#'
#' @export
create_sightings <- function(path,
                             col_names = TRUE,
                             skip      = 1L,
                             sheet    = 4) {
  sightings01 <- read_raw_sightings(
    path = path,
    col_names = col_names,
    skip = skip,
    sheet = sheet
  )

  sightings02 <-
    sightings01 |>
    dplyr::transmute(
      .data$sighting_id,
      latitude = .data$lat,
      longitude = .data$lon,
      date = base::as.Date(.data$Date),
      initial_time = combine_date_time(date = .data$Date, time = .data$`Time i`),
      final_time    = combine_date_time(date = .data$Date, time = .data$`Time f`),
      other_species = species_abbrv_to_name(.data$`OT. Species`)
    )

  sightings03 <-
    sightings02 |>
    dplyr::mutate(final_time = dplyr::if_else(
      base::is.na(.data$final_time),
      .data$initial_time + 60,
      # 1 min = 60 seconds
      .data$final_time
    ))

  sightings03
}

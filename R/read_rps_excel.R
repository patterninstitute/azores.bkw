#' Read and format RPS sighting data from an Excel file
#'
#' [read_rps_excel()] reads an Excel file containing RPS sighting data and
#' formats date and time information into a single POSIXct datetime column.
#' Unnecessary columns are removed to return a cleaned and standardized tibble
#' ready for further analysis.
#'
#' @param file A character string giving the path to the Excel (\code{.xlsx})
#'   file containing RPS sighting data.
#'
#' @returns A tibble of RPS sighting records with a standardized datetime field.
#'   Each row represents a single sighting, and the returned columns include
#'   those present in the original file except for removed date/time components.
#'   In particular, the output includes:
#'   \itemize{
#'     \item \code{lon}: Longitude of the sighting
#'     \item \code{lat}: Latitude of the sighting
#'     \item \code{datetime}: POSIXct datetime combining date and time
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Reads an Excel file using \code{readxl::read_xlsx()}
#'   \item Converts date and time columns into proper R date and time formats
#'   \item Combines date and time into a single \code{datetime} column
#'   \item Removes redundant or unused date/time and metadata columns
#' }
#'
#' The time component is extracted from the original \code{Time} column by
#' removing any leading text before conversion.
#'
#' @examples
#' if (FALSE) {
#'   rps_data <- read_rps_excel("data/RPS_sightings.xlsx")
#' }
#'
#' @export
read_rps_excel <- function(file) {
  readxl::read_xlsx(file) |>
    dplyr::mutate(date = as.Date(as.character(Date)), .after = "lon") |>
    dplyr::mutate(
      time = lubridate::hms(
        stringr::str_remove(as.character(Time), "^.+\\s")
      ),
      .after = "date"
    ) |>
    dplyr::mutate(datetime = date + time) |>
    dplyr::select(-c("date", "Date", "time", "Time", "Day", "Month", "Year", "name"))
}


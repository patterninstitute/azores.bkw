#' Combine date and time into a UTC datetime
#'
#' Constructs a POSIXct datetime by combining the calendar components of a
#' `Date` object with the clock components of a time-like object.
#'
#' This helper is intended for datasets where dates and times are stored in
#' separate columns.
#'
#' @param date A vector coercible to `Date`.
#' @param time A vector coercible to POSIXct/POSIXlt, from which hour, minute,
#'   and second components will be extracted.
#' @param tz Time zone for the resulting datetime. Defaults to `"UTC"`.
#'
#' @return A POSIXct vector.
#'
#' @keywords internal
combine_date_time <- function(date, time, tz = "UTC") {
  lubridate::make_datetime(
    year  = lubridate::year(date),
    month = lubridate::month(date),
    day   = lubridate::day(date),
    hour  = lubridate::hour(time),
    min   = lubridate::minute(time),
    sec   = lubridate::second(time),
    tz    = tz
  )
}

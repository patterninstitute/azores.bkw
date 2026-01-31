#' Generate study date sequence
#'
#' @description
#'
#' [study_dates()] generates a daily sequence of dates between the
#' specified start and end dates. This sequence is used to index
#' environmental raster time series.
#'
#' @param from A character or `Date` object indicating the start date.
#'   Defaults to `"2012-01-01"`.
#' @param to A character or `Date` object indicating the end date.
#'   Defaults to `"2018-12-31"`.
#'
#' @returns A vector of class `Date` with daily resolution.
#'
#' @export
study_dates <- function(from = "2012-01-01", to = "2018-12-31") {
  seq.Date(from = as.Date(from),
           to = as.Date(to),
           by = "day")
}

#' Index SST raster layers by date
#'
#' @description
#'
#' [sst_dates()] creates a lookup table linking each study date to
#' the corresponding layer index in the SST NetCDF file.
#'
#' @returns A tibble with columns:
#' \describe{
#'   \item{date}{Daily study dates.}
#'   \item{day}{Row index of the date sequence.}
#'   \item{file}{Name of the NetCDF file containing SST data.}
#'   \item{i}{Layer index within the NetCDF file corresponding to each date.}
#' }
#'
#' @export
sst_dates <- function() {
  tibble::tibble(date = study_dates()) |>
    tibble::rowid_to_column(var = "day") |>
    dplyr::mutate(
      file = "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1741967803421.nc") |>
    dplyr::group_by(.data$file) |>
    dplyr::mutate(i = seq_len(dplyr::n()), .after = "date") |>
    dplyr::ungroup()
}

#' Read daily SST raster
#'
#' @description
#'
#' [read_sst()] reads the Sea Surface Temperature (SST) raster
#' corresponding to a given date from a NetCDF file stored on disk.
#' The raster is optionally smoothed using a focal mean and projected
#' to the Azores analysis grid.
#'
#' SST values are converted from Kelvin to degrees Celsius.
#'
#' @param date A `Date` or character coercible to `Date`.
#' @param dir Directory containing the SST NetCDF file.
#' @param resolution Resolution (in meters) of the output raster grid.
#'   Defaults to 500.
#' @param w Window size used for focal smoothing. Defaults to 5.
#'
#' @returns A `SpatRaster` object projected to the Azores grid.
#'
#' @export
read_sst <- function(date, dir, resolution = 500, w = 5) {

  sst_dates <- sst_dates()
  date_ <- as.Date(date)

  tbl <-
    tibble::tibble(date = date_) |>
    dplyr::left_join(sst_dates, by = "date")

  cli::cli_inform("{.file {tbl$file}} -- {date}")

  sp <- terra::rast(x = file.path(dir, tbl$file), lyrs = tbl$i) |>
    terra::focal(w = w, fun = mean, na.policy = "only", na.rm = TRUE)
  # Convert from Kelvin to Celsius.
  sp <- sp - 273.15
  terra::project(sp, y = azores.bathymetry::azores_grid(resolution = resolution, type = "rast"), method = "near")
}

#' Extract SST values at sf locations
#'
#' @description
#'
#' [extract_sst()] extracts Sea Surface Temperature (SST) values
#' for each geometry in an `sf` object based on the corresponding
#' `date` column.
#'
#' Extraction is performed by grouping records by date and matching
#' each group to the appropriate raster layer.
#'
#' @param sf An `sf` object containing a `date` column.
#' @param dir Directory containing the SST NetCDF file.
#'
#' @returns An `sf` object with the same columns as the input,
#'   plus an additional column named `"sst"` placed before
#'   the geometry column.
#'
#' @export
extract_sst <- function(sf, dir) {

  sf_by_group <- dplyr::group_split(sf, .data$date)

  sst_by_group <-
    sf |>
    dplyr::group_by(.data$date) |>
    dplyr::group_map(.f = ~ terra::extract(x = read_sst(date = .y$date, dir = dir), y = .x)) |>
    purrr::map(~dplyr::select(dplyr::rename(.x, sst = 2L), -dplyr::all_of("ID")))

  purrr::map2(sf_by_group, sst_by_group, dplyr::bind_cols) |>
    purrr::map(~ dplyr::relocate(.x, "sst", .before = "geometry")) |>
    purrr::list_rbind() |>
    sf::st_as_sf()
}

#
# For hmlmeso variable
#

hmlmeso_dates <- function() {
  tibble::tibble(date = study_dates()) |>
    tibble::rowid_to_column(var = "day") |>
    dplyr::mutate(
      file = "mlmeso.2012.2018.nc") |>
    dplyr::group_by(.data$file) |>
    dplyr::mutate(i = seq_len(dplyr::n()), .after = "date") |>
    dplyr::ungroup()
}
read_hmlmeso <- function(date, dir, resolution = 500, w = 5) {

  hmlmeso_dates <- hmlmeso_dates()
  date_ <- as.Date(date)

  tbl <-
    tibble::tibble(date = date_) |>
    dplyr::left_join(hmlmeso_dates, by = "date")

  cli::cli_inform("{.file {tbl$file}} -- {date}")

  sp <- terra::rast(x = file.path(dir, tbl$file), lyrs = tbl$i) |>
    terra::focal(w = w, fun = mean, na.policy = "only", na.rm = TRUE)
  terra::project(sp, y = azores.bathymetry::azores_grid(resolution = resolution, type = "rast"), method = "near")
}

extract_hmlmeso <- function(sf, dir) {

  sf_by_group <- dplyr::group_split(sf, .data$date)

  hmlmeso_by_group <-
    sf |>
    dplyr::group_by(.data$date) |>
    dplyr::group_map(.f = ~ terra::extract(x = read_hmlmeso(date = .y$date, dir = dir), y = .x)) |>
    purrr::map(~dplyr::select(dplyr::rename(.x, hmlmeso = 2L), -dplyr::all_of("ID")))

  purrr::map2(sf_by_group, hmlmeso_by_group, dplyr::bind_cols) |>
    purrr::map(~ dplyr::relocate(.x, "hmlmeso", .before = "geometry")) |>
    purrr::list_rbind() |>
    sf::st_as_sf()
}

#
# For lmeso variable
#

lmeso_dates <- function() {
  tibble::tibble(date = study_dates()) |>
    tibble::rowid_to_column(var = "day") |>
    dplyr::mutate(
      file = "lmeso.2012.2018.nc") |>
    dplyr::group_by(.data$file) |>
    dplyr::mutate(i = seq_len(dplyr::n()), .after = "date") |>
    dplyr::ungroup()
}
read_lmeso <- function(date, dir, resolution = 500, w = 5) {

  lmeso_dates <- lmeso_dates()
  date_ <- as.Date(date)

  tbl <-
    tibble::tibble(date = date_) |>
    dplyr::left_join(lmeso_dates, by = "date")

  cli::cli_inform("{.file {tbl$file}} -- {date}")

  sp <- terra::rast(x = file.path(dir, tbl$file), lyrs = tbl$i) |>
    terra::focal(w = w, fun = mean, na.policy = "only", na.rm = TRUE)

  terra::project(sp, y = azores.bathymetry::azores_grid(resolution = resolution, type = "rast"), method = "near")
}

extract_lmeso <- function(sf, dir) {

  sf_by_group <- dplyr::group_split(sf, .data$date)

  lmeso_by_group <-
    sf |>
    dplyr::group_by(.data$date) |>
    dplyr::group_map(.f = ~ terra::extract(x = read_lmeso(date = .y$date, dir = dir), y = .x)) |>
    purrr::map(~dplyr::select(dplyr::rename(.x, lmeso = 2L), -dplyr::all_of("ID")))

  purrr::map2(sf_by_group, lmeso_by_group, dplyr::bind_cols) |>
    purrr::map(~ dplyr::relocate(.x, "lmeso", .before = "geometry")) |>
    purrr::list_rbind() |>
    sf::st_as_sf()
}

#
# For mlmeso variable
#

mlmeso_dates <- function() {
  tibble::tibble(date = study_dates()) |>
    tibble::rowid_to_column(var = "day") |>
    dplyr::mutate(
      file = "mlmeso.2012.2018.nc") |>
    dplyr::group_by(.data$file) |>
    dplyr::mutate(i = seq_len(dplyr::n()), .after = "date") |>
    dplyr::ungroup()
}
read_mlmeso <- function(date, dir, resolution = 500, w = 5) {

  mlmeso_dates <- mlmeso_dates()
  date_ <- as.Date(date)

  tbl <-
    tibble::tibble(date = date_) |>
    dplyr::left_join(mlmeso_dates, by = "date")

  cli::cli_inform("{.file {tbl$file}} -- {date}")

  sp <- terra::rast(x = file.path(dir, tbl$file), lyrs = tbl$i) |>
    terra::focal(w = w, fun = mean, na.policy = "only", na.rm = TRUE)

  terra::project(sp, y = azores.bathymetry::azores_grid(resolution = resolution, type = "rast"), method = "near")
}

extract_mlmeso <- function(sf, dir) {

  sf_by_group <- dplyr::group_split(sf, .data$date)

  mlmeso_by_group <-
    sf |>
    dplyr::group_by(.data$date) |>
    dplyr::group_map(.f = ~ terra::extract(x = read_mlmeso(date = .y$date, dir = dir), y = .x)) |>
    purrr::map(~dplyr::select(dplyr::rename(.x, mlmeso = 2L), -dplyr::all_of("ID")))

  purrr::map2(sf_by_group, mlmeso_by_group, dplyr::bind_cols) |>
    purrr::map(~ dplyr::relocate(.x, "mlmeso", .before = "geometry")) |>
    purrr::list_rbind() |>
    sf::st_as_sf()
}

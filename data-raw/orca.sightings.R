library(here)
library(robis)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(hms)
library(usethis)

library(azores.rorquals)
library(azores.cetaceans)
library(azores.bkw)

blackfish <- readr::read_delim(here::here("data-raw", "blackfish.csv"), col_types = "ccl", delim = ";")

# Species of interest, i.e. included in this study (`in_study == TRUE`).
species <- dplyr::filter(blackfish, in_study) |> dplyr::pull(species)

#  Global Biodiversity Information Facility (GBIF) records for the species of
#  interest, i.e. the ones indicated in `species`.
oo.gbif <- azores.bkw::dwl_gbif_occurrences(species) |>
  dplyr::rename(datetime = "eventDate") |>
  dplyr::mutate(datetime = str_replace(datetime, "(T\\d{2}:\\d{2}$)", "\\1:00")) |>
  # When dates/times are not well formatted we get warnings from as_datetime
  # that converts those values to NA -- here we suppress those warnings.
  dplyr::mutate(datetime = suppressWarnings(lubridate::as_datetime(datetime)))

#  Ocean Biodiversity Information System (OBIS) records for the species of
#  interest, i.e. the ones indicated in `species`.
extent <- "POLYGON ((-34 34, -21 34, -21 42, -34 42, -34 34))"

oo.obis <-  robis::occurrence(scientificname = species, geometry = extent) |>
  dplyr::select(c("species", "eventDate", "decimalLongitude", "decimalLatitude")) |>
  dplyr::mutate(datetime = stringr::str_remove(eventDate, pattern = "\\/.+$")) |>
  dplyr::mutate(datetime = str_replace(datetime, "(T\\d{2}:\\d{2}$)", "\\1:00")) |>
  dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
  dplyr::select(-c("eventDate")) |>
  dplyr::rename(lon = "decimalLongitude", lat = "decimalLatitude") |>
  dplyr::relocate(datetime, .after = 1L)

#  read RPS data collected from 2012 to 2018

rps_path <- here::here("data-raw/rps_sightings")
rps_files <- list.files(rps_path, full.names = TRUE)

oo.rps <- purrr::map(rps_files, .f = azores.bkw::read_rps_excel) |>
  purrr::set_names(species) |>
  purrr::list_rbind(names_to = "species") |>
  dplyr::relocate(datetime, .after = 1) |>
  dplyr::relocate(lat, .after = 4)


# Define the start and end date-times for the one-year interval
interval_start <- lubridate::ymd_hms("2012-01-01 00:00:00")
interval_end <- lubridate::ymd_hms("2018-12-31 23:59:59")
study_interval <- lubridate::interval(interval_start, interval_end)

oo.sightings <- dplyr::bind_rows(list(gbif = oo.gbif, obis = oo.obis, rps = oo.rps), .id = "source") |>
  dplyr::filter(datetime %within% study_interval) |>
  dplyr::filter(!(lubridate::month(datetime) == 1 & lubridate::day(datetime) == 1 & as.character(hms::as_hms(datetime)) == "00:00:00")) %>%
  dplyr::bind_cols(azores.rorquals::longlat_to_utm(.[c("lon", "lat")])) |>
  dplyr::mutate(easting = round(easting), northing = round(northing)) |>
  dplyr::group_by(species, datetime, easting, northing) |>
  dplyr::summarise(source = paste(unique(source), collapse = "+"), .groups = "drop") |>
  dplyr::relocate(source, .before = 1L) |>
  dplyr::mutate(
    date = lubridate::date(datetime),
    time = hms::as_hms(datetime),
    .after = "datetime") |>
  dplyr::mutate(time = dplyr::if_else(as.character(time) == "00:00:00", NA, time)) |>
  azores.rorquals::define_presences(eps_meters = 2000, eps_hour = 1.5) |>
  dplyr::arrange(datetime)|>
  dplyr::distinct(date, .keep_all = TRUE)

usethis::use_data(oo.sightings, overwrite = TRUE)
readr::write_csv(oo.sightings, here::here("data-raw", "orca.sightings.csv.gz"))


library(tidyverse)
library(here)
library(lubridate)
library(readr)

orca.sightings <- readr::read_delim(
  here::here("data", "orca.sightings.csv.gz"),
  delim = ","
)

orca.season <- orca.sightings |>
  dplyr::mutate(
    season = dplyr::case_when(
      lubridate::month(date) %in% c(1, 2, 3)   ~ "winter",
      lubridate::month(date) %in% c(4, 5, 6)   ~ "spring",
      lubridate::month(date) %in% c(7, 8, 9)   ~ "summer",
      lubridate::month(date) %in% c(10, 11, 12) ~ "autumn"
    )
  )

# Save for a rda file
usethis::use_data(orca.season, overwrite = TRUE)

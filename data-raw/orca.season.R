library(dplyr)
library(usethis)
library(azores.bkw)

orca.season <-
  azores.bkw::oo.sightings |>
  dplyr::mutate(
    season = dplyr::case_when(
      lubridate::month(date) %in% 1:3 ~ "winter",
      lubridate::month(date) %in% 4:6 ~ "spring",
      lubridate::month(date) %in% 7:9 ~ "summer",
      lubridate::month(date) %in% 10:12 ~ "autumn"
    )
  )

# Save for a rda file
usethis::use_data(orca.season, overwrite = TRUE)

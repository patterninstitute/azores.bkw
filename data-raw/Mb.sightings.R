library(tidyverse)

path <- here::here("data-raw/Mb_Data.xlsx")
raw_sightings <- read_raw_sightings(path = path)

#
# `sightings`
#

mb.sightings <- create_sightings(path = path)

#
# `ethology`
#

mb.ethology <-
  raw_sightings |>
  dplyr::transmute(sighting_id, dplyr::across(FO:NI...30)) |>
  tidyr::pivot_longer(
    cols = FO:NI...30,
    names_to = "behavior",
    values_drop_na = TRUE
  ) |>
  dplyr::select(-"value") |>
  dplyr::mutate(behavior = behaviour_abbrv_to_name(abbrv = .data$behavior))

#
# `pod_age_composition`
#

age_levels <- c("newborn", "calf", "juvenile", "adult", "other")
mb.pod_age_composition <-
  raw_sightings |>
  dplyr::transmute(sighting_id, dplyr::across(Max:NI...36)) |>
  tidyr::pivot_longer(
    cols = Max:NI...36,
    names_to = "age_group",
    values_to = "n",
    values_drop_na = TRUE
  ) |>
  dplyr::mutate(age_group = age_abbrv_to_name(abbrv = .data$age_group)) |>
  dplyr::filter(age_group != "total") |>
  tidyr::complete(
    sighting_id,
    age_group = factor(age_group, levels = age_levels),
    fill = list(n = 0)
  )

#
# `reaction_to_boat`
#

mb.reaction_to_boat <-
  raw_sightings |>
  dplyr::transmute(sighting_id, dplyr::across(A:Ni)) |>
  tidyr::pivot_longer(
    cols = A:Ni,
    names_to = "reaction",
    values_to = "n",
    values_drop_na = TRUE
  ) |>
  dplyr::select(-n) |>
  dplyr::mutate(reaction = reaction_abbrv_to_name(abbrv = .data$reaction))

usethis::use_data(mb.ethology, overwrite = TRUE)
usethis::use_data(mb.pod_age_composition, overwrite = TRUE)
usethis::use_data(mb.reaction_to_boat, overwrite = TRUE)
usethis::use_data(mb.sightings, overwrite = TRUE)

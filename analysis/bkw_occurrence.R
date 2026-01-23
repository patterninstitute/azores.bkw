library(sf)
library(tidyverse)
library(starsExtra)
library(terra)
library(tidyterra)
library(azores.rorquals)
library(azores.bathymetry)
library(azores.cetaceans)

env_lay_pah <- "/Users/ruisantos/Desktop/Github/azores.bkw/data-raw/env_layers"
presences <- azores.rorquals::presences_sf
presences_rps <- presences |>
  dplyr::filter(source == "rps")

depth <- azores.bathymetry::bathymetry("depth", resolution = 500)
tidyterra::autoplot(depth)
slope <- azores.bathymetry::bathymetry("depth_slope", resolution = 500)
tidyterra::autoplot(slope)


#
#sst
#

sst_sf <- extract_sst(presences_rps, dir = env_lay_pah) |>
  dplyr::select("sst", "date", "geometry")

#
#hmlmeso
#

hmlmeso_sf <- extract_hmlmeso(presences_rps, dir = env_lay_pah) |>
  dplyr::select("hmlmeso", "date", "geometry")


#
#lmeso
#

lmeso_sf <- extract_lmeso(presences_rps, dir = env_lay_pah)|>
  dplyr::select("lmeso", "date", "geometry")


#
#mlmeso
#

mlmeso_sf <- extract_mlmeso(presences_rps, dir = env_lay_pah)|>
  dplyr::select("mlmeso", "date", "geometry")

#
# depth
#

presences_rps$id <- 1:nrow(presences_rps)

depth_extract <- terra::extract(depth, presences_rps)

# Join the extracted values back to the sf object
depth_sf <- presences_rps |>
  dplyr::left_join(depth_extract, by = c("id" = "ID")) |>
  dplyr::select(depth, date, geometry)

#
#slope
#

slope_extract <- terra::extract(slope, presences_rps)

# Join the extracted values back to the sf object
slope_sf <- presences_rps |>
  dplyr::left_join(slope_extract, by = c("id" = "ID")) |>
  dplyr::select(Slope, date, geometry) |>
  dplyr::rename(slope = Slope)


#
#Bind the several predictors
#

presences_rps <- presences_rps |>
  dplyr::select(-id)

environmental_predictors <-
  bind_cols(
    presences_rps,
    sst = sst_sf[["sst"]],
    hmlmeso = hmlmeso_sf[["hmlmeso"]],
    lmeso = lmeso_sf[["lmeso"]],
    mlmeso = mlmeso_sf[["mlmeso"]],
    depth = depth_sf[["depth"]],
    slope = slope_sf[["slope"]],
  )

target_species <- c("Mesoplodon bidens", "Mesoplodon densirostris", "Mesoplodon europaeus",
                    "Mesoplodon mirus", "Hyperoodon ampullatus", "Ziphius cavirostris")


bkw_occurrence <-
  environmental_predictors |>
  dplyr::mutate(class = dplyr::if_else(species %in% target_species, "presence", "absence")) |>
  dplyr::relocate("class", .after = "species") |>
  dplyr::group_by(mixed_sp_grp) |>
  dplyr::filter(!(class == "absence" & any(class == "presence"))) |>
  dplyr::ungroup() |>
  # Keep only observations at sea.
  sf::st_join(CAOP.RAA.2024::districts(), join = sf::st_within) |>
  dplyr::select(-c("id", "district", "perimeter", "n_municipalities", "n_parishes", "area"))

readr::write_csv(bkw_occurrence, "bkw_occurrence_rps.csv.gz")
# Save to a folder as a shapefile
sf::st_write(bkw_occurrence, "bkw_occurrence_rps.shp", append = TRUE)

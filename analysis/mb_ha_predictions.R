library(tidysdm)
library(terra)
library(tidyterra)
library(readr)
library(azores.fkw)

#
# Prediction
#

# Set file paths
env_layers_path <- "data-raw/env-layers-by-season"
env_layer_spring_path <- file.path(env_layers_path, "mb_ha.climate_2012_2018_spring.tif")
env_layer_summer_path <- file.path(env_layers_path, "mb_ha.climate_2012_2018_summer.tif")
env_layer_autumn_path <- file.path(env_layers_path, "mb_ha.climate_2012_2018_autumn.tif")

prediction_path <- "analysis/predictions"
spring_prediction_path <- file.path(prediction_path, "mb_ha_prediction_spring.tif")
summer_prediction_path <- file.path(prediction_path, "mb_ha_prediction_summer.tif")
autumn_prediction_path <- file.path(prediction_path, "mb_ha_prediction_autumn.tif")

# Define study area extent: xmin, xmax, ymin, ymax
extent <- terra::ext(-29, -27.5, 38, 38.8)

# Import ensemble of models
mb_ha_ensemble <- readr::read_rds("analysis/models/ensemble-model-mb-ha.rds")
mb_ha.climate_2012_2018_spring <- terra::rast(x = env_layer_spring_path)
mb_ha.climate_2012_2018_summer <- terra::rast(x = env_layer_summer_path)
mb_ha.climate_2012_2018_autumn <- terra::rast(x = env_layer_autumn_path)

# Crop areas
mb_ha.climate_2012_2018_spring_study_area <- terra::crop(mb_ha.climate_2012_2018_spring, extent)
mb_ha.climate_2012_2018_summer_study_area <- terra::crop(mb_ha.climate_2012_2018_summer, extent)
mb_ha.climate_2012_2018_autumn_study_area <- terra::crop(mb_ha.climate_2012_2018_autumn, extent)

# Calculate predictions
mb_ha_prediction_spring <- tidysdm::predict_raster(mb_ha_ensemble, mb_ha.climate_2012_2018_spring_study_area)
mb_ha_prediction_summer <- tidysdm::predict_raster(mb_ha_ensemble, mb_ha.climate_2012_2018_summer_study_area)
mb_ha_prediction_autumn <- tidysdm::predict_raster(mb_ha_ensemble, mb_ha.climate_2012_2018_autumn_study_area)

# Save predictions to disk
terra::writeRaster(x = mb_ha_prediction_spring, filename = spring_prediction_path, overwrite = TRUE)
terra::writeRaster(x = mb_ha_prediction_summer, filename = summer_prediction_path, overwrite = TRUE)
terra::writeRaster(x = mb_ha_prediction_autumn, filename = autumn_prediction_path, overwrite = TRUE)

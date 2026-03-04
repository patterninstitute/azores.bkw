library(tidyverse)
library(tidysdm)
library(sf)
library(terra)
library(tidyterra)
library(ggbeeswarm)
library(DALEX)
library(units)

library(azores.fkw)


#
# Prediction
#

### Upload the saved emsemble ###
bkw_ensemble <- readr::read_rds("data/bkw_ensemble_rps.rds")
bkw.climate_2012_2018_spring <- terra::rast(x = "data-raw/cms-azores-data/bkw.climate_2012_2018_spring.tif")
bkw.climate_2012_2018_summer <- terra::rast(x = "data-raw/cms-azores-data/bkw.climate_2012_2018_summer.tif")
bkw.climate_2012_2018_autumn <- terra::rast(x = "data-raw/cms-azores-data/bkw.climate_2012_2018_autumn.tif")

# define extent: xmin, xmax, ymin, ymax
extent <- ext(-29, -27.5, 38, 38.8)

# crop
bkw.climate_2012_2018_spring_study_area <- terra::crop(bkw.climate_2012_2018_spring, extent)
bkw.climate_2012_2018_summer_study_area <- terra::crop(bkw.climate_2012_2018_summer, extent)
bkw.climate_2012_2018_autumn_study_area <- terra::crop(bkw.climate_2012_2018_autumn, extent)

### Predict for the season of spring ###
bkw_prediction_spring <- tidysdm::predict_raster(bkw_ensemble, bkw.climate_2012_2018_spring_study_area)

### Save the predicted raster for the season of spring ###
terra::writeRaster(x = bkw_prediction_spring, filename = "analysis/bkw_prediction_spring.tif", overwrite = TRUE)

### Predict for the season of summer ###
bkw_prediction_summer <- tidysdm::predict_raster(bkw_ensemble, bkw.climate_2012_2018_summer_study_area)

### Save the predicted raster for the season of summer ###
terra::writeRaster(x = bkw_prediction_summer, filename = "analysis/bkw_prediction_summer.tif", overwrite = TRUE)

### Predict for the season of autumn ###
bkw_prediction_autumn <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_autumn_study_area)

### Save the predicted raster for the season of summer ###
terra::writeRaster(x = bkw_prediction_autumn, filename = "analysis/bkw_prediction_autumn.tif", overwrite = TRUE)

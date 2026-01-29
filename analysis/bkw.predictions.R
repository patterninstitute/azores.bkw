library(tidyverse)
library(tidysdm)
library(sf)
library(terra)
library(tidyterra)
library(ggbeeswarm)
library(DALEX)
library(units)
library(azores.fkw)
library(patchwork)

setwd("/Users/ruisantos/Desktop/Tidysdm/breeding/")

#
# Prediction
#

### Upload the saved emsemble ###
bkw_ensemble <- readr::read_rds("bkw_ensemble_rps.rds")
bkw.climate_2012_2018_spring <- terra::rast(x = "analysis/bkw.climate_2012_2018_spring.tif")
bkw.climate_2012_2018_summer <- terra::rast(x = "analysis/bkw.climate_2012_2018_summer.tif")
bkw.climate_2012_2018_autumn <- terra::rast(x = "analysis/bkw.climate_2012_2018_autumn.tif")

# define extent: xmin, xmax, ymin, ymax
e <- ext(-29, -27.5, 38, 38.8)

# crop
bkw.climate_2012_2018_spring <- terra::crop(bkw.climate_2012_2018_spring, e)
bkw.climate_2012_2018_summer <- terra::crop(bkw.climate_2012_2018_summer, e)
bkw.climate_2012_2018_autumn <- terra::crop(bkw.climate_2012_2018_autumn, e)


### Predict for the month of April ###
# bkw_prediction_april <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_april)
#
# ggplot() +
#   geom_spatraster(data = bkw_prediction_april, aes(fill = mean)) +
#   scale_fill_terrain_c()
#
# ### Predict for the month of July ###
# bkw_prediction_july <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_july)
#
# ggplot() +
#   geom_spatraster(data = bkw_prediction_july, aes(fill = mean)) +
#   scale_fill_terrain_c()

### Predict for the season of spring ###
bkw_prediction_spring <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_spring)

# Define a new resolution (smaller cell size for higher resolution)
new_resolution <- 0.001  # Change to your desired resolution

# Create a target raster with the new resolution
target_raster <- terra::rast(terra::ext(bkw_prediction_spring),
                             resolution = new_resolution,
                             crs = crs(bkw_prediction_spring))
# Resample the spat raster
bkw_prediction_spring2 <- resample(bkw_prediction_spring, target_raster, method = "bilinear")

bkw_prediction_spring_plot <- ggplot() +
  geom_spatraster(data = bkw_prediction_spring2, aes(fill = mean)) +
  scale_fill_terrain_c(limits = c(0,0.55)) +
  geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 1 degree
  scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2))
bkw_prediction_spring_plot

terra::writeRaster(x = bkw_prediction_spring2, filename = "analysis/bkw_prediction_spring.tif", overwrite = TRUE)

### Predict for the season of summer ###
bkw_prediction_summer <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_summer)

# Resample the spat raster
bkw_prediction_summer2 <- resample(bkw_prediction_summer, target_raster, method = "bilinear")

bkw_prediction_summer_plot <- ggplot() +
  geom_spatraster(data = bkw_prediction_summer2, aes(fill = mean)) +
  scale_fill_terrain_c(limits = c(0,0.55)) +
  geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 1 degree
  scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2)) + # Breaks every 0.2 degree
  labs(fill = "Suitability")
bkw_prediction_summer_plot

terra::writeRaster(x = bkw_prediction_summer2, filename = "analysis/bkw_prediction_summer.tif", overwrite = TRUE)

### Predict for the season of autumn ###
bkw_prediction_autumn <- predict_raster(bkw_ensemble, bkw.climate_2012_2018_autumn)

# Resample the spat raster
bkw_prediction_autumn2 <- resample(bkw_prediction_autumn, target_raster, method = "bilinear")

bkw_prediction_autumn_plot <- ggplot() +
  geom_spatraster(data = bkw_prediction_autumn2, aes(fill = mean)) +
  scale_fill_terrain_c(limits = c(0,0.55)) +
  geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 0.5 degree
  scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2)) # Breaks every 0.2 degree
bkw_prediction_autumn_plot

terra::writeRaster(x = bkw_prediction_autumn2, filename = "analysis/bkw_prediction_autumn.tif", overwrite = TRUE)


combined_plot <- bkw_prediction_spring_plot + bkw_prediction_summer_plot + bkw_prediction_autumn_plot +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A')
combined_plot
# Save the combined plot as a PDF with specific dimensions
ggsave(
  filename = "Study.bkw.allseasons.pdf",
  plot = combined_plot,
  width = 6,    # Width in inches
  height = 10,  # Height in inches
  units = "in", # Units for dimensions (can be "in", "cm", or "mm")
  device = "pdf"  # Specify the file format
)

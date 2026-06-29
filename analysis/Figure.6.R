library(ggplot2)
library(terra)
library(patchwork)
library(here)

path <- here::here("analysis")
figures_path <- file.path(path, "figures")
prediction_path <- "analysis/predictions"
spring_prediction_path <- file.path(prediction_path, "mb_ha_prediction_spring.tif")
summer_prediction_path <- file.path(prediction_path, "mb_ha_prediction_summer.tif")
autumn_prediction_path <- file.path(prediction_path, "mb_ha_prediction_autumn.tif")

mb_ha_prediction_spring <- terra::rast(x = spring_prediction_path)
mb_ha_prediction_summer <- terra::rast(x = summer_prediction_path)
mb_ha_prediction_autumn <- terra::rast(x = autumn_prediction_path)

# Define a new resolution (smaller cell size for higher resolution)
new_resolution <- 0.001  # Change to your desired resolution

# Create a target raster with the new resolution
target_raster <- terra::rast(
  terra::ext(mb_ha_prediction_spring),
  resolution = new_resolution,
  crs = terra::crs(mb_ha_prediction_spring)
)
# Resample the Spring spatraster
mb_ha_prediction_spring2 <- terra::resample(mb_ha_prediction_spring, target_raster, method = "bilinear")

# Resample the Summer spatraster
mb_ha_prediction_summer2 <- terra::resample(mb_ha_prediction_summer, target_raster, method = "bilinear")

# Resample the Autumn spatraster
mb_ha_prediction_autumn2 <- terra::resample(mb_ha_prediction_autumn, target_raster, method = "bilinear")

#
# Figure 6
#

mb_ha_prediction_spring_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = mb_ha_prediction_spring2, ggplot2::aes(fill = mean)) +
  tidyterra::scale_fill_terrain_c(limits = c(0, 0.55)) +
  ggplot2::geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 1 degree
  ggplot2::scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2))

mb_ha_prediction_summer_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = mb_ha_prediction_summer2, ggplot2::aes(fill = mean)) +
  tidyterra::scale_fill_terrain_c(limits = c(0,0.55)) +
  ggplot2::geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  ggplot2::scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 1 degree
  ggplot2::scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2)) + # Breaks every 0.2 degree
  ggplot2::labs(fill = "Suitability")

mb_ha_prediction_autumn_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = mb_ha_prediction_autumn2, ggplot2::aes(fill = mean)) +
  tidyterra::scale_fill_terrain_c(limits = c(0,0.55)) +
  ggplot2::geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::coord_sf(
    xlim = c(-29, -27.5),
    ylim = c(38, 38.8),
    expand = FALSE
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 0.5 degree
  ggplot2::scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2)) # Breaks every 0.2 degree

combined_plot <-
  mb_ha_prediction_spring_plot +
  mb_ha_prediction_summer_plot +
  mb_ha_prediction_autumn_plot +
  patchwork::plot_annotation(tag_levels = 'a',
                             tag_prefix = "(",
                             tag_suffix = ")") +
  plot_layout(ncol = 1)

combined_plot
# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "mb_ha.fig6.pdf",
       plot = combined_plot,
       width = 6,    # Width in inches
       height = 10,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

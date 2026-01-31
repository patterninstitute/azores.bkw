library(ggplot2)
library(terra)
library(patchwork)
library(here)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
figures_path <- file.path(path, "figures")

bkw_prediction_spring <- terra::rast(x = "analysis/bkw_prediction_spring.tif")
bkw_prediction_summer <- terra::rast(x = "analysis/bkw_prediction_summer.tif")
bkw_prediction_autumn <- terra::rast(x = "analysis/bkw_prediction_autumn.tif")

# Define a new resolution (smaller cell size for higher resolution)
new_resolution <- 0.001  # Change to your desired resolution

# Create a target raster with the new resolution
target_raster <- terra::rast(terra::ext(bkw_prediction_spring),
                             resolution = new_resolution,
                             crs = terra::crs(bkw_prediction_spring))
# Resample the Spring spatraster
bkw_prediction_spring2 <- terra::resample(bkw_prediction_spring, target_raster, method = "bilinear")

# Resample the Summer spatraster
bkw_prediction_summer2 <- terra::resample(bkw_prediction_summer, target_raster, method = "bilinear")

# Resample the Autumn spatraster
bkw_prediction_autumn2 <- terra::resample(bkw_prediction_autumn, target_raster, method = "bilinear")

#
# Figure 6
#

bkw_prediction_spring_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = bkw_prediction_spring2, ggplot2::aes(fill = mean)) +
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
  ggplot2::scale_x_continuous(breaks = seq(-28.5, -28, by = 0.5)) +  # Breaks every 1 degree
  ggplot2::scale_y_continuous(breaks = seq(38.2, 38.6, by = 0.2))

bkw_prediction_summer_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = bkw_prediction_summer2, ggplot2::aes(fill = mean)) +
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

bkw_prediction_autumn_plot <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = bkw_prediction_autumn2, ggplot2::aes(fill = mean)) +
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

combined_plot <- bkw_prediction_spring_plot + bkw_prediction_summer_plot + bkw_prediction_autumn_plot +
  patchwork::plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") +
  plot_layout(ncol = 1)
combined_plot
# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "bkw.fig6.pdf",
       plot = combined_plot,
       width = 6,    # Width in inches
       height = 10,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

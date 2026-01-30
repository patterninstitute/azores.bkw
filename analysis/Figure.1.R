library(tidyverse)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(sf)
library(dplyr)

library(azores.rorquals)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
zze <- sf::st_read(file.path(path, "zze-200-miles.kml"))
figures_path <- file.path(path, "figures")

load("data/boat_trips_by_min.rda")
load("data/azores_islands.rda")

bkw_sightings <- azores.rorquals::presences_sf |>
  dplyr::filter(source == "rps") |>
  filter(species %in% c("Mesoplodon bidens", "Mesoplodon densirostris", "Mesoplodon europaeus",
                        "Mesoplodon mirus", "Hyperoodon ampullatus", "Ziphius cavirostris"))

bkw_sightings <- sf::st_transform(bkw_sightings, 4326)
### Include mb_sightings and ha_sightings for reviewer comments
mb.sightings <- bkw_sightings |>
  dplyr::filter(species == "Mesoplodon bidens")

ha.sightings <- bkw_sightings |>
  dplyr::filter(species == "Hyperoodon ampullatus")

#
# Figure 1A
#

boat_tracks_main_plot <-
  ggplot2::ggplot(data = boat_trips_by_min) +
  ggplot2::geom_path(
    ggplot2::aes(x = longitude, y = latitude, group = trip),
    alpha = 0.5,
    colour = "black",
    linewidth = 0.1
  ) +
  ggplot2::geom_sf(
    data = bkw_sightings,
    color = "red",   # outline color
    size = 2         # point size
  ) +
  ggplot2::geom_sf(
    data = azores.fkw::azores_triangle(),
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_x_continuous(limits = c(-29, -27.5), breaks = c(-28.8, -28.4, -28.0, -27.6)) +
  ggplot2::scale_y_continuous(limits = c(38, 39), breaks = c(38.0, 38.4, 38.8)) +
  ggplot2::scale_fill_continuous(limits = c(0, 0.012),
                                 breaks = seq(0, 0.012, 0.004)) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white"),
    pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(1, "in"), width = unit(1, "in"),
    pad_x = unit(0.10, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )

boat_tracks_inset_plot <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = azores.fkw::azores_islands,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = zze,
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_x_continuous(breaks = c(-32, -24)) +
  ggplot2::scale_y_continuous(breaks = c(34, 38, 42)) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(color = "black"),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_blank(),
  )

bkw.fig1A <-
  boat_tracks_main_plot + patchwork::inset_element(
    boat_tracks_inset_plot,
    left = 0.635,
    bottom = 0.635,
    right = 1,
    top = 1.035,
    align_to = "panel"
  )
bkw.fig1A

#Save Figure 1A
ggsave(figures_path,
       filename = "bkw.study.area.pdf",
       plot = bkw.fig1A,
       width = 9,    # Width in inches
       height = 8,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

#
# Figure 1B
#

# 1. Define grid (4km x 4km) over the study area
grid_size_km  <- 4
grid_size_deg <- grid_size_km / 111.12  # Approx conversion

# Convert tibble to sf object
boat_trips_sf <- sf::st_as_sf(
  boat_trips_by_min,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Bounding box
bbox <- sf::st_bbox(boat_trips_sf)

# Create regular grid
grid <- sf::st_make_grid(
  sf::st_as_sfc(bbox),
  cellsize = c(grid_size_deg, grid_size_deg),
  square = TRUE
) |>
  sf::st_sf()

# Ensure CRS consistency
bkw_sightings_sf <- sf::st_transform(bkw_sightings, 4326)
boat_effort_sf   <- sf::st_transform(boat_trips_sf, 4326)

# 2. Count sightings per grid cell
sightings_per_grid <- grid |>
  dplyr::mutate(
    bkw_sightings = lengths(sf::st_intersects(grid, bkw_sightings_sf))
  )

# Count effort per grid cell
effort_per_grid <- grid |>
  dplyr::mutate(
    boat_trips = lengths(sf::st_intersects(grid, boat_effort_sf))
  )

# Merge counts
grid_data <- sf::st_join(
  sightings_per_grid,
  effort_per_grid,
  join = sf::st_equals
)

# Remove cells without effort
grid_data <- grid_data |>
  dplyr::filter(boat_trips > 0)

# Calculate encounter rate
grid_data <- grid_data |>
  dplyr::mutate(
    ER = ifelse(boat_trips > 0,
                (bkw_sightings / boat_trips) * 100,
                NA_real_)
  )

grid_data1 <- grid_data |>
  dplyr::mutate(
    Encounter_Rate = dplyr::if_else(ER == 0, NA_real_, ER)
  )

# 3. Plot Figure 1B
bkw.fig1B <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = grid_data1,
    ggplot2::aes(fill = Encounter_Rate),
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::geom_sf(
    data = azores.fkw::azores_triangle(),
    fill = "gray",
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-29, -27.5),
    breaks = c(-28.8, -28.4, -28.0, -27.6)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(38, 39),
    breaks = c(38.0, 38.4, 38.8)
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white", color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "plasma",
    na.value = "white"
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = 12,
                                         margin = ggplot2::margin(b = 12))
  )

bkw.fig1B

#Save Figure 1B
ggsave(figures_path,
       filename = "bkw.er.pdf",
       plot = bkw.fig1B,
       width = 9,    # Width in inches
       height = 8,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)


# Combine Figure 1A and 1B
bkw.fig1 <- bkw.fig1A / bkw.fig1B #+
plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

bkw.fig1

#Save Figure 1
ggsave(figures_path,
       filename = "bkw.fig1.pdf",
       plot = bkw.fig1,
       width = 6,    # Width in inches
       height = 12,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

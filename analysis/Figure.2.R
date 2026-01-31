library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(here)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
figures_path <- file.path(path, "figures")

load("data/bkw.pod_age_composition.rda")
load("data/ha.pod_age_composition.rda")
load("data/mb.pod_age_composition.rda")
load("data/bkw.sightings.rda")


### Include mb_sightings and ha_sightings for reviewer comments
mb.sightings <- bkw.sightings |>
  dplyr::filter(species == "Mb")

ha.sightings <- bkw.sightings |>
  dplyr::filter(species == "Ha")


#
# Figure 2
#

# Filter the data to exclude "new born" and "others" if no data exists for them
bkw.pod_age_composition <- bkw.pod_age_composition |>
  dplyr::filter(age_group != "newborns" & age_group != "other" | n > 1)

bkw.group <- ggplot2::ggplot(bkw.pod_age_composition, ggplot2::aes(x = forcats::fct_relevel(age_group, "adults", "juveniles", "calves"), y = n)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "", y = "Bkw number of individuals")
bkw.group

# Create a boxplot for the average duration of each sighting
bkw.time <- ggplot2::ggplot(bkw.sightings, ggplot2::aes(x = "", y = base::as.numeric(duration))) +
  ggplot2::geom_boxplot(fill = "gray", color = "black") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "",
       y = "Bkw duration (minutes)") + ggplot2::ylim(c(0, 30))
bkw.time

# Filter the data to exclude "new born" and "others" if no data exists for them
mb.pod_age_composition <- mb.pod_age_composition |>
  dplyr::filter(age_group != "newborn" & age_group != "other" | n > 1) |>
  dplyr::mutate(age_group = dplyr::recode(age_group,
                                   "calf" = "calves",
                                   "juvenile" = "juveniles",
                                   "adult" = "adults"))

mb.group <- ggplot2::ggplot(mb.pod_age_composition, ggplot2::aes(x = forcats::fct_relevel(age_group, "adults", "juveniles", "calves"), y = n)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "", y = "Mb number of individuals")
mb.group

# Create a boxplot for the average duration of each sighting
mb.time <- ggplot2::ggplot(mb.sightings, ggplot2::aes(x = "", y = as.numeric(duration))) +
  ggplot2:: geom_boxplot(fill = "gray", color = "black") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "",
       y = "Mb duration (minutes)") + ggplot2::ylim(c(0, 30))
mb.time

# Filter the data to exclude "new born" and "others" if no data exists for them
ha.pod_age_composition <- ha.pod_age_composition |>
  dplyr::filter(age_group != "newborn" & age_group != "other" | n > 1) |>
  dplyr::mutate(age_group = dplyr::recode(age_group,
                                   "calf" = "calves",
                                   "juvenile" = "juveniles",
                                   "adult" = "adults"))

ha.group <- ggplot2::ggplot(ha.pod_age_composition, ggplot2::aes(x = forcats::fct_relevel(age_group, "adults", "juveniles", "calves"), y = n)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "Group composition", y = "Ha number of individuals")
ha.group

# Create a boxplot for the average duration of each sighting
ha.time <- ggplot2::ggplot(ha.sightings, ggplot2::aes(x = "", y = as.numeric(duration))) +
  ggplot2::geom_boxplot(fill = "gray", color = "black") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 12),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")) +
  ggplot2::labs(x = "Sightings",
       y = "Ha duration (minutes)") + ggplot2::ylim(c(0, 30))
ha.time

bkw.fig2 <- patchwork::wrap_plots(
  bkw.time, bkw.group,
  mb.time, mb.group,
  ha.time, ha.group,
  ncol = 2) +
  patchwork::plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

bkw.fig2

# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "bkw.fig2.pdf",
       plot = bkw.fig2,
       width = 8,    # Width in inches
       height = 12,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)


### Now to include the table asked from the reviewer comment 1 second part for Mb and Ha ###
mb_group_sizes <- mb.pod_age_composition |>
  dplyr::group_by(sighting_id) |>
  dplyr::summarise(group_size = sum(n), .groups = "drop")

mb_data_summary <- mb_group_sizes |>
  dplyr::left_join(mb.sightings, by = "sighting_id")

mb_table_summary <- mb_data_summary |>
  dplyr::group_by(species, season) |>
  dplyr::summarise(
    n_sightings = dplyr::n(),
    group_size_mean = mean(group_size, na.rm = TRUE),
    group_size_range = paste0(min(group_size, na.rm = TRUE), "–", max(group_size, na.rm = TRUE)),
    duration_mean = mean(duration, na.rm = TRUE),
    duration_range = paste0(min(duration, na.rm = TRUE), "–", max(duration, na.rm = TRUE)),
    .groups = "drop"
  )

ha_group_sizes <- ha.pod_age_composition |>
  dplyr::group_by(sighting_id) |>
  dplyr::summarise(group_size = sum(n), .groups = "drop")

ha_data_summary <- ha_group_sizes |>
  dplyr::left_join(ha.sightings, by = "sighting_id")

ha_table_summary <- ha_data_summary |>
  dplyr::group_by(species, season) |>
  dplyr::summarise(
    n_sightings = dplyr::n(),
    group_size_mean = mean(group_size, na.rm = TRUE),
    group_size_range = paste0(min(group_size, na.rm = TRUE), "–", max(group_size, na.rm = TRUE)),
    duration_mean = mean(duration, na.rm = TRUE),
    duration_range = paste0(min(duration, na.rm = TRUE), "–", max(duration, na.rm = TRUE)),
    .groups = "drop"
  )
View(mb_table_summary)
View(ha_table_summary)

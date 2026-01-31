library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(here)

library(azores.rorquals)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
figures_path <- file.path(path, "figures")


bkw_sightings <- azores.rorquals::presences_sf |>
  dplyr::filter(source == "rps") |>
  dplyr::filter(species %in% c("Mesoplodon bidens", "Mesoplodon densirostris", "Mesoplodon europaeus",
                        "Mesoplodon mirus", "Hyperoodon ampullatus", "Ziphius cavirostris"))

#
# Figure 5
#

#Create a bar graph with number of sightings for each season
# Add a 'season' column based on the 'date' column
bkw.sightings <- bkw_sightings |>
  dplyr::mutate(season = dplyr::case_when(
      lubridate::month(date) %in% c(4, 5, 6) ~ "Spring",
      lubridate::month(date) %in% c(7, 8, 9) ~ "Summer",
      lubridate::month(date) %in% c(10, 11, 12) ~ "Autumn",
      TRUE ~ NA_character_  # Assign NA for other months (e.g., Winter)
    )
  ) |> dplyr::mutate(family = "Ziphiidae")

# Reorder the 'season' factor
bkw.sightings$season <- forcats::fct_relevel(bkw.sightings$season, "Spring", "Summer", "Autumn")

bkw.season <- ggplot2::ggplot(data = bkw.sightings, ggplot2::aes(x = season)) +
  ggplot2::geom_bar() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray"),
    strip.text = ggplot2::element_text(face = "italic", size = 12)) +
  ggplot2::labs(y = "Number of sightings") +
  ggplot2::facet_wrap(family ~ .)
bkw.season

#Create a bar graph with number of orca sightings for each season
oo.sightings <- oo.sightings |>
  dplyr::mutate(season = dplyr::case_when(
    lubridate::month(date) %in% c(4, 5, 6) ~ "Spring",
    lubridate::month(date) %in% c(7, 8, 9) ~ "Summer",
    lubridate::month(date) %in% c(1, 2, 3) ~ "Winter",
    TRUE ~ NA_character_  # Assign NA for other months (e.g., Winter)
  )
  )

orca.season <- ggplot2::ggplot(data = oo.sightings, ggplot2::aes(x = season)) +
  ggplot2::geom_bar() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray"),
    strip.text = ggplot2::element_text(face = "italic", size = 12)) +
  ggplot2::facet_wrap(species ~ .)
orca.season


bkw.fig5 <- bkw.season + orca.season +
  patchwork::plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")
bkw.fig5

# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "bkw.fig5.pdf",
       plot = bkw.fig5,
       width = 12,    # Width in inches
       height = 8,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

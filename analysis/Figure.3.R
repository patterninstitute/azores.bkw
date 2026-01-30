library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
figures_path <- file.path(path, "figures")

load("data/bkw.ethology.rda")
load("data/bkw.reaction_to_boat.rda")

#
# Figure 3
#

bkw.behaviors <- ggplot2::ggplot(bkw.ethology, ggplot2::aes(x = forcats::fct_infreq(grouped_behaviors))) +
  ggplot2::geom_bar() +
  ggplot2::scale_y_continuous(breaks = c(0, 40, 80, 120)) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),  # Hide only the X-axis title
    axis.title.y = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")
  ) +
  ggplot2::labs(y = "Number of sightings")
bkw.behaviors

bkw.boat <- ggplot2::ggplot(bkw.reaction_to_boat, ggplot2::aes(x = forcats::fct_infreq(reaction))) +
  ggplot2::geom_bar() +
  ggplot2::labs(x = ggplot2::element_blank(), y = ggplot2::element_blank()) +
  ggplot2::scale_y_continuous(breaks = c(0, 40, 80, 120)) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
    panel.grid.major = ggplot2::element_line(color = "gray")
  )
bkw.boat

bkw.fig3 <- bkw.behaviors + bkw.boat +
  patchwork::plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")
bkw.fig3

# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "bkw.fig3.pdf",
       plot = bkw.fig3,
       width = 12,    # Width in inches
       height = 8,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "pdf"  # Specify the file format
)

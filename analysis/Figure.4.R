library(ggplot2)
library(cowplot)
library(patchwork)

path <- here::here("/Users/ruisantos/Desktop/Github/azores.bkw/analysis")
figures_path <- file.path(path, "figures")

#
# Figure 4
#

image_top <- cowplot::ggdraw() + cowplot::draw_image("analysis/bkw2.jpeg") +
  theme(plot.tag = element_text(face = "plain"))
image_bottom <- cowplot::ggdraw() + cowplot::draw_image("analysis/bkw3.jpeg") +
  theme(plot.tag = element_text(face = "plain"))
bkw.fig4 <- image_top + plot_spacer() + image_bottom +
  patchwork::plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") +
  plot_layout(ncol = 1, heights = c(1, 0.05, 1)) # '/' stacks plots vertically
bkw.fig4
# Save the combined plot as a PDF with specific dimensions
ggplot2::ggsave(figures_path,
       filename = "bkw.Fig4.jpg",
       plot = bkw.fig4,
       width = 6,    # Width in inches
       height = 6.6,  # Height in inches
       units = "in", # Units for dimensions (can be "in", "cm", or "mm")
       device = "jpg"  # Specify the file format
)

# Shared look of the figures: one font size, recessive grid, fixed colours.
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

status_col <- c(certified_optimal = "#2a78d6", epsilon_optimal = "#1baf7a",
                uncertain = "#eb6834", certified_dominated = "#9a9893")
status_shape <- c(certified_optimal = 16, epsilon_optimal = 17, uncertain = 15, certified_dominated = 1)
status_lab <- c(certified_optimal = "certified optimal", epsilon_optimal = "accepted (epsilon-optimal)",
                uncertain = "undecided", certified_dominated = "discarded")
proc_col <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_paper <- function(base = 9) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
          axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink),
          plot.title = element_text(face = "bold", size = base + 1, colour = ink),
          plot.subtitle = element_text(colour = ink2), legend.position = "bottom",
          strip.text = element_text(face = "bold", colour = ink),
          plot.background = element_rect(fill = "white", colour = NA))
}

save_fig <- function(p, file, width = 6.5, height = 3) {
  ggsave(file, p, width = width, height = height, device = cairo_pdf)
}

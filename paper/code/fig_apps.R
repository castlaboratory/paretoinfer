# Figure 3: the two applications at the end of one identification --------------
if (dir.exists("paper/.lib/current")) .libPaths(c("paper/.lib/current", .libPaths()))
suppressPackageStartupMessages(library(paretoinfer))
source("paper/code/fig_theme.R")

box_panel <- function(state, truth, title, xlab, ylab, xscale = 1, yscale = 1, label_fun = identity,
                      xlim = NULL, ylim = NULL) {
  tb <- tidy(state); tab <- plausible_pareto_set(state)$table
  o <- state$design$objectives
  a <- tb[tb$objective == o[1], ]; b <- tb[tb$objective == o[2], ]
  df <- data.frame(alternative = a$alternative, status = a$status,
                   xmin = a$lower * xscale, xmax = a$upper * xscale, ymin = b$lower * yscale, ymax = b$upper * yscale,
                   x = truth[a$alternative, 1] * xscale, y = truth[a$alternative, 2] * yscale,
                   rep = tab$representative[match(a$alternative, tab$alternative)])
  df$status <- factor(df$status, names(status_col))
  df$lab <- ifelse(df$rep, paste0(label_fun(df$alternative), "*"), label_fun(df$alternative))
  g <- ggplot(df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, colour = status, fill = status),
              alpha = 0.12, linewidth = 0.35) +
    geom_point(aes(x, y, colour = status, shape = status), size = 1.8) +
    ggrepel::geom_text_repel(aes(x, y, label = lab), size = 2.2, colour = ink, min.segment.length = 0.1,
                             segment.colour = ink2, segment.size = 0.2, box.padding = 0.25, max.overlaps = Inf,
                             seed = 1) +
    scale_colour_manual(values = status_col, labels = status_lab, drop = FALSE, name = NULL) +
    scale_fill_manual(values = status_col, labels = status_lab, drop = FALSE, name = NULL) +
    scale_shape_manual(values = status_shape, labels = status_lab, drop = FALSE, name = NULL) +
    labs(title = title, x = xlab, y = ylab) + theme_paper()
  if (!is.null(xlim) || !is.null(ylim)) g <- g + coord_cartesian(xlim = xlim, ylim = ylim)
  g
}

source("paper/code/app_binomial_methods.R")
bin <- readRDS("paper/results/cache/binomial_state.rds")
bin_truth <- binomial_truth(30)
inv <- readRDS("paper/results/cache/inventory_state.rds")
inv_truth <- readRDS("paper/results/cache/inventory_truth.rds")

p1 <- box_panel(bin, bin_truth, "(a) intervals for a binomial proportion, n = 30",
                "average miscoverage (%, log scale)", "average width", xscale = 100) +
  scale_x_log10(breaks = c(2.5, 3, 4, 5, 7.5, 10, 12.5))
p2 <- box_panel(inv, inv_truth, "(b) (s, S) inventory policies",
                "average cost per period", "fraction of periods with backlog (%)", yscale = 100,
                xlim = c(107, 145))
p1 <- p1 + guides(colour = "none", fill = "none", shape = "none")
p2 <- p2 + guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
save_fig(p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
         "paper/manuscript/figures/fig3_applications.pdf", width = 7, height = 3.6)

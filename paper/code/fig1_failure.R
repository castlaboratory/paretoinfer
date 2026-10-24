# Figure 1: the three instances of Proposition 1 -------------------------------
source("paper/code/fig_theme.R")
eps <- 0.1

# All ordered pairs (j, k) with mu[j] <= mu[k] + eps; flag mutual pairs.
certs <- function(mu) {
  K <- nrow(mu); out <- NULL
  for (j in seq_len(K)) for (k in seq_len(K)) if (j != k && all(mu[j, ] <= mu[k, ] + eps)) {
    out <- rbind(out, data.frame(j = j, k = k, mutual = all(mu[k, ] <= mu[j, ] + eps)))
  }
  out[!(out$mutual & out$j > out$k), ]
}
arrows_df <- function(mu, pos) {
  cc <- certs(mu)
  data.frame(x = pos[cc$j, 1], y = pos[cc$j, 2], xend = pos[cc$k, 1], yend = pos[cc$k, 2], mutual = cc$mutual)
}
panel <- function(mu, pos, discarded, title, xlab, ylab, labels = NULL, lim = NULL) {
  a <- arrows_df(mu, pos)
  shrink <- function(a, f = 0.16) {   # stop arrows short of the points
    dx <- a$xend - a$x; dy <- a$yend - a$y
    transform(a, x = x + f * dx, y = y + f * dy, xend = xend - f * dx, yend = yend - f * dy)
  }
  a <- shrink(a)
  pts <- data.frame(x = pos[, 1], y = pos[, 2], lab = labels %||% as.character(seq_len(nrow(mu))),
                    disc = seq_len(nrow(mu)) %in% discarded)
  g <- ggplot() +
    geom_curve(data = a[!a$mutual, ], aes(x = x, y = y, xend = xend, yend = yend), curvature = -0.35,
               arrow = arrow(length = unit(1.6, "mm"), type = "closed"), colour = ink, linewidth = 0.45) +
    geom_segment(data = a[a$mutual, ], aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(1.6, "mm"), type = "closed", ends = "both"),
                 colour = ink2, linewidth = 0.45, linetype = "32") +
    geom_point(data = pts, aes(x, y, shape = disc, colour = disc), size = 2.6, stroke = 0.8) +
    geom_text(data = pts, aes(x, y, label = lab), nudge_y = 0.022, size = 2.8, colour = ink, vjust = 0) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
    scale_colour_manual(values = c(`FALSE` = status_col[["certified_optimal"]], `TRUE` = "#9a9893"), guide = "none") +
    labs(title = title, x = xlab, y = ylab) + theme_paper()
  if (!is.null(lim)) g <- g + coord_equal(xlim = lim$x, ylim = lim$y)
  g
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# (a) three objectives: draw the alternatives on a triangle, with their mean vectors.
mu_a <- rbind(c(0.400, 0.452, 0.504), c(0.504, 0.400, 0.452), c(0.452, 0.504, 0.400))
pos_a <- rbind(c(0.5, 0.85), c(0.05, 0.3), c(0.95, 0.3))
lab_a <- sprintf("%d\n(%.3f, %.3f, %.3f)", 1:3, mu_a[, 1], mu_a[, 2], mu_a[, 3])
pa <- panel(mu_a, pos_a, 1:3, "(a) three objectives: a cycle", NULL, NULL, labels = rep("", 3)) +
  annotate("text", x = pos_a[, 1], y = pos_a[, 2] + c(0.13, -0.15, -0.15), label = lab_a,
           size = 2.6, colour = ink) +
  scale_x_continuous(limits = c(-0.25, 1.25)) + scale_y_continuous(limits = c(0.0, 1.1)) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank())

# (b) two objectives, tie-break: x1 < x2 < x3, y1 > y2 > y3.
mu_b <- rbind(c(0.30, 0.50), c(0.34, 0.44), c(0.38, 0.38))
pb <- panel(mu_b, mu_b, 1:3, "(b) two objectives: ties", "objective 1", "objective 2",
            lim = list(x = c(0.26, 0.42), y = c(0.34, 0.54)))

# (c) two objectives, chain 3 -> 2 -> 1.
mu_c <- rbind(c(0.30, 0.70), c(0.38, 0.55), c(0.46, 0.40), c(0.10, 0.95))
pc <- panel(mu_c, mu_c, 1:2, "(c) two objectives: a chain", "objective 1", "objective 2",
            lim = list(x = c(0.05, 0.55), y = c(0.35, 1.0)))

save_fig(pa + pb + pc + plot_layout(widths = c(1.1, 1, 1)), "paper/manuscript/figures/fig1_failure.pdf",
         width = 7, height = 2.9)

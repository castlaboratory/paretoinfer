# Table 1 (Study 1) and Figure 2 (Study 2) --------------------------------------
source("paper/code/fig_theme.R")

cp <- function(x, n) {   # Clopper-Pearson 95% interval, in percent
  lo <- if (x == 0) 0 else qbeta(0.025, x, n - x + 1); up <- if (x == n) 1 else qbeta(0.975, x + 1, n - x)
  sprintf("%.1f (%.1f, %.1f)", 100 * x / n, 100 * lo, 100 * up)
}

# Study 1 -------------------------------------------------------------------
s1 <- read.csv("paper/results/study1_validity.csv")
if (file.exists("paper/results/study1b_validity.csv")) s1 <- rbind(s1, read.csv("paper/results/study1b_validity.csv"))
s1$error <- with(s1, false_discard | false_accept | false_optimal | cover_fail)
scen <- c("flat", "close", "boundary", "cycle", "ties", "chain"); scen <- scen[scen %in% s1$scenario]
procs <- c("paretoinfer 0.2.0", "epsilon discarding (0.1.0)", "repeated t intervals", "no union bound")
proc_tex <- c("proposed", "$\\varepsilon$-discarding", "repeated $t$", "no correction")
pct <- function(x) sprintf("%.1f", 100 * mean(x))
rows <- character()
for (sc in scen) {
  cells <- vapply(procs, function(p) {
    d <- s1[s1$scenario == sc & s1$procedure == p, ]
    ev <- sprintf("%.0f", median(d$n_evaluations))
    if (mean(d$identified) < 1) ev <- sprintf("%s (%.1f)", ev, 100 * mean(d$identified))
    sprintf("%s & %s & %s", pct(d$error), pct(d$frontier_loss), ev)
  }, character(1))
  rows <- c(rows, paste0(sc, " & ", paste(cells, collapse = " & "), " \\\\"))
}
hdr <- paste0(" & ", paste(sprintf("\\multicolumn{3}{c}{%s}", proc_tex), collapse = " & "), " \\\\")
cm <- paste(sprintf("\\cmidrule(lr){%d-%d}", 3 * (0:3) + 2, 3 * (0:3) + 4), collapse = "")
sub <- paste0("Configuration", strrep(" & Err. & Loss & Eval.", 4), " \\\\")
writeLines(c("\\setlength{\\tabcolsep}{3pt}", "\\begin{tabular}{l*{4}{rrr}}", "\\toprule", hdr, cm, sub,
             "\\midrule", rows, "\\bottomrule", "\\end{tabular}"),
           "paper/manuscript/tables/tab1_validity.tex")

# Study 2 -------------------------------------------------------------------
s2 <- read.csv("paper/results/study2_efficiency.csv")
main <- s2[s2$cell <= 9, ]
main$procedure <- factor(main$procedure, c("adaptive", "uniform", "epsilon discarding (0.1.0)"),
                         c("open-pair allocation", "uniform allocation", "epsilon-discarding with its allocation (first version)"))
main$eps_lab <- factor(sprintf("epsilon == %.3g", main$eps), sprintf("epsilon == %.3g", sort(unique(main$eps))))
ann <- aggregate(cbind(identified, frontier_loss) ~ K + eps_lab, main[main$procedure == levels(main$procedure)[3], ], mean)
ann$label <- sprintf("stop %.0f%%\nloss %.0f%%", 100 * ann$identified, 100 * ann$frontier_loss)
main$censored <- !main$identified
g <- ggplot(main, aes(x = factor(K), y = n_evaluations, colour = procedure)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.size = 0.6, linewidth = 0.35) +
  geom_text(data = ann, aes(x = factor(K), y = 7.5e4, label = label), inherit.aes = FALSE,
            nudge_x = 0.25, size = 2.2, colour = proc_col[3], lineheight = 0.85) +
  facet_wrap(~eps_lab, nrow = 1, labeller = label_parsed) +
  scale_y_log10(labels = scales::label_number(big.mark = ","), breaks = c(1e3, 3e3, 1e4, 3e4)) +
  scale_colour_manual(values = proc_col[1:3], name = NULL) +
  labs(x = "number of alternatives K", y = "evaluations until stopping (log scale)") +
  theme_paper()
save_fig(g, "paper/manuscript/figures/fig2_efficiency.pdf", width = 7, height = 3.2)

# Summary for the text and Table S2.
agg <- aggregate(cbind(n_evaluations, identified, frontier_loss) ~ cell + K + m + eps + rho + boundary + procedure,
                 s2, function(x) c(med = median(x), mean = mean(x)))
print(agg)
saveRDS(agg, "paper/results/study2_summary.rds")

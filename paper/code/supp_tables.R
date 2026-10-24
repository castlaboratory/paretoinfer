# Tables of the Supplementary Material -------------------------------------------
if (dir.exists("paper/.lib/current")) .libPaths(c("paper/.lib/current", .libPaths()))
suppressPackageStartupMessages(library(paretoinfer))
out <- function(lines, file) writeLines(lines, file.path("paper/manuscript/tables", file))
fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")

# Table S1: planning horizon of Theorem 2 for the Hoeffding sequence.
h <- read.csv("paper/results/planning_horizon.csv")
h <- h[order(h$eps, h$m, h$K), ]
rows <- sprintf("%.3f & %d & %d & %s & %s \\\\", h$eps, h$m, h$K, fmt_int(h$nstar), fmt_int(h$horizon))
out(c("\\begin{tabular}{rrrrr}", "\\toprule",
      "$\\varepsilon$ & $m$ & $K$ & $n^*(\\varepsilon/4)$ & horizon \\\\", "\\midrule", rows,
      "\\bottomrule", "\\end{tabular}"), "tabS1_horizon.tex")

# Table S2: all cells of Study 2.
s2 <- read.csv("paper/results/study2_efficiency.csv")
cells <- unique(s2[, c("cell", "K", "m", "eps", "rho", "boundary")])
cells <- cells[order(cells$cell), ]
proc <- c("adaptive", "uniform", "epsilon discarding (0.1.0)")
q <- function(x) sprintf("%s (%s, %s)", fmt_int(median(x)), fmt_int(quantile(x, 0.25)), fmt_int(quantile(x, 0.75)))
rows <- character()
for (i in seq_len(nrow(cells))) {
  cc <- cells[i, ]
  f <- function(p) { d <- s2[s2$cell == cc$cell & s2$procedure == p, ]; if (!nrow(d)) "" else q(d$n_evaluations) }
  e <- s2[s2$cell == cc$cell & s2$procedure == proc[3], ]
  st <- if (nrow(e)) sprintf("%.0f / %.0f", 100 * mean(e$identified), 100 * mean(e$frontier_loss)) else ""
  bd <- c(betting = "betting", empirical_bernstein = "emp.\\ Bernstein", hoeffding = "Hoeffding")[cc$boundary]
  rows <- c(rows, sprintf("%d & %d & %.3f & %+.1f & %s & %s & %s & %s \\\\", cc$K, cc$m, cc$eps, cc$rho, bd,
                          f(proc[1]), f(proc[2]), st))
}
rows <- sub("\\+0\\.0", "0.0", rows)
out(c("\\begin{tabular}{rrrrlrrr}", "\\toprule",
      "$K$ & $m$ & $\\varepsilon$ & $\\rho$ & sequence & open-pair & uniform & $\\varepsilon$-disc.\\ stop / loss (\\%) \\\\",
      "\\midrule", rows, "\\bottomrule", "\\end{tabular}"), "tabS2_study2.tex")

# Tables S3 and S4: the applications, true means and final statements.
status_tex <- c(certified_optimal = "certified optimal", epsilon_optimal = "$\\epsb$-optimal",
                uncertain = "undecided", certified_dominated = "discarded")
app_table <- function(state, truth, digits, fmt2, scale, file, header) {
  tab <- plausible_pareto_set(state)$table
  tab <- tab[match(rownames(truth), tab$alternative), ]
  P <- local({
    K <- nrow(truth)
    vapply(seq_len(K), function(k) !any(vapply(setdiff(seq_len(K), k), function(j)
      all(truth[j, ] <= truth[k, ]) && any(truth[j, ] < truth[k, ]), logical(1))), logical(1))
  })
  dom <- ifelse(is.na(tab$dominated_by), "", tab$dominated_by)
  rows <- sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", rownames(truth),
                  formatC(truth[, 1] * scale[1], format = "f", digits = digits[1]),
                  formatC(truth[, 2] * scale[2], format = fmt2, digits = digits[2]),
                  ifelse(P, "yes", "no"), fmt_int(state$n), status_tex[tab$status],
                  paste0(dom, ifelse(tab$representative, " (representative)", "")))
  rows <- sub("&  \\(representative\\)", "& representative", rows)
  out(c("\\begin{tabular}{lrrlrll}", "\\toprule", header, "\\midrule", rows, "\\bottomrule", "\\end{tabular}"), file)
}
source("paper/code/app_binomial_methods.R")
app_table(readRDS("paper/results/cache/binomial_state.rds"), binomial_truth(30), c(3, 4), "f", c(100, 1),
          "tabS3_binomial.tex",
          "Method & miscoverage (\\%) & width & Pareto & evaluations & statement & dominator or role \\\\")
app_table(readRDS("paper/results/cache/inventory_state.rds"), readRDS("paper/results/cache/inventory_truth.rds"),
          c(2, 2), "e", c(1, 1), "tabS4_inventory.tex",
          "Policy & cost & backlog & Pareto & evaluations & statement & dominator or role \\\\")

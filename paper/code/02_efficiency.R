# Study 2: evaluations to identification ----------------------------------------
#
# Random instances (common.R, make_instance), a fresh instance per replication.
# Main grid: K x epsilon with m = 2, independent Beta noise; procedures:
# adaptive and uniform allocation with exact-dominance discarding, and the
# 0.1.0 epsilon-discarding rule with its own adaptive allocation (the price of
# correctness). Secondary cells vary m, the correlation and the boundary.

source("paper/code/common.R")
R <- as.integer(Sys.getenv("PI_R", "30"))
budget <- 3e4

cells <- rbind(
  expand.grid(K = c(5L, 10L, 20L), m = 2L, eps = c(0.025, 0.05, 0.10), rho = 0, boundary = "betting",
              stringsAsFactors = FALSE),
  data.frame(K = 10L, m = 3L, eps = 0.05, rho = 0, boundary = "betting"),
  data.frame(K = 10L, m = 2L, eps = 0.05, rho = c(-0.6, 0.6), boundary = "betting"),
  data.frame(K = 10L, m = 2L, eps = 0.05, rho = 0, boundary = c("empirical_bernstein", "hoeffding"))
)
main <- seq_len(9)
procedures <- list(
  adaptive = list(rule = "v020", sampler = "adaptive"),
  uniform = list(rule = "v020", sampler = "uniform"),
  `epsilon discarding (0.1.0)` = list(rule = "v010", sampler = "adaptive")
)

file <- "paper/results/study2_efficiency.csv"
res <- if (file.exists(file)) read.csv(file) else NULL
for (ci in seq_len(nrow(cells))) {
  cl <- cells[ci, ]
  procs <- if (ci %in% main) names(procedures) else c("adaptive", "uniform")
  for (pn in procs) {
    if (!is.null(res) && any(res$cell == ci & res$procedure == pn)) next
    pr <- procedures[[pn]]
    t0 <- proc.time()[["elapsed"]]
    out <- mc(R, function() {
      mu <- make_instance(cl$K, cl$m)
      sim <- make_beta_sim(mu, kappa = 20, rho = cl$rho)
      cbind(run_one(mu, sim, eps = cl$eps, rule = pr$rule, sampler = pr$sampler, boundary = cl$boundary,
                    batch = 5L, initial = 5L, max_evaluations = budget, check_every = 5L),
            n_pareto = length(pareto_index(mu)))
    }, seed = 1000 + ci)
    out <- cbind(cell = ci, cl[rep(1, nrow(out)), ], procedure = pn, out, row.names = NULL)
    res <- rbind(res, out)
    write.csv(res, file, row.names = FALSE)
    message(sprintf("cell %2d K=%2d m=%d eps=%.3f rho=%+.1f %-19s %-28s %6.0f s  median n %6.0f  identified %.2f  loss %.2f",
                    ci, cl$K, cl$m, cl$eps, cl$rho, cl$boundary, pn, proc.time()[["elapsed"]] - t0,
                    median(out$n_evaluations), mean(out$identified), mean(out$frontier_loss)))
  }
}

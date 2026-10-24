# Study 1b: the tie configuration of Proposition 1(b) and a boundary case -------
#
# Four configurations in which a wrong certificate is easy to issue, four
# procedures, R replications each. Every certificate issued at any round is
# checked against the true means (common.R, run_one).

source("paper/code/common.R")
R <- as.integer(Sys.getenv("PI_R", "200"))

# Additional configuration (run after the main study): three Pareto-optimal
# alternatives with two epsilon-ties and one one-directional certificate (m = 2),
# Proposition 1(b).
scenarios <- list(
  ties = list(mu = rbind(A1 = c(0.30, 0.50), A2 = c(0.34, 0.44), A3 = c(0.38, 0.38)),
              eps = 0.10, kappa = 200),
  # A2 is dominated by exactly epsilon in both objectives, so it is not
  # epsilon-Pareto-optimal, but no box can ever certify it as accepted with
  # valid intervals: an acceptance of A2 is always a false statement.
  boundary = list(mu = rbind(A1 = c(0.40, 0.40), A2 = c(0.42, 0.42), A3 = c(0.30, 0.60), A4 = c(0.60, 0.30)),
                  eps = 0.02, kappa = 20)
)
procedures <- list(
  `paretoinfer 0.2.0` = list(rule = "v020", boundary = "betting", correction = TRUE),
  `epsilon discarding (0.1.0)` = list(rule = "v010", boundary = "betting", correction = TRUE),
  `repeated t intervals` = list(rule = "v020", boundary = "naive_fixed", correction = TRUE),
  `no union bound` = list(rule = "v020", boundary = "betting", correction = FALSE)
)

res <- list()
for (sn in names(scenarios)) for (pn in names(procedures)) {
  sc <- scenarios[[sn]]; pr <- procedures[[pn]]
  sim <- make_beta_sim(sc$mu, kappa = sc$kappa)
  t0 <- proc.time()[["elapsed"]]
  out <- mc(R, function() run_one(sc$mu, sim, eps = sc$eps, rule = pr$rule, boundary = pr$boundary,
                                  correction = pr$correction, batch = 5L, initial = 5L,
                                  max_evaluations = 1e4), seed = 20261023)
  out$scenario <- sn; out$procedure <- pn
  res[[length(res) + 1L]] <- out
  message(sprintf("%-6s %-28s %6.0f s  any error %.3f  loss %.3f", sn, pn, proc.time()[["elapsed"]] - t0,
                  mean(out$false_discard | out$false_accept | out$false_optimal | out$cover_fail),
                  mean(out$frontier_loss)))
  write.csv(do.call(rbind, res), "paper/results/study1b_validity.csv", row.names = FALSE)
}

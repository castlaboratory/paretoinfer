# Study 1: validity of the certificates under optional stopping ----------------
#
# Four configurations in which a wrong certificate is easy to issue, four
# procedures, R replications each. Every certificate issued at any round is
# checked against the true means (common.R, run_one).

source("paper/code/common.R")
R <- as.integer(Sys.getenv("PI_R", "200"))

scenarios <- list(
  # Four Pareto-optimal alternatives on a flat front, two pairs closer than eps.
  flat = list(mu = rbind(A1 = c(0.30, 0.62), A2 = c(0.38, 0.52), A3 = c(0.40, 0.50), A4 = c(0.52, 0.40)),
              eps = 0.02, kappa = 20),
  # A dominated alternative just beyond eps (A2, by 0.03) next to optimal ones:
  # a false acceptance and a false discard are both possible.
  close = list(mu = rbind(A1 = c(0.40, 0.40), A2 = c(0.43, 0.43), A3 = c(0.37, 0.44), A4 = c(0.44, 0.37)),
               eps = 0.02, kappa = 20),
  # Three Pareto-optimal alternatives forming a cycle of epsilon-dominance (m = 3).
  cycle = list(mu = rbind(A1 = c(0.400, 0.452, 0.504), A2 = c(0.504, 0.400, 0.452), A3 = c(0.452, 0.504, 0.400)),
               eps = 0.10, kappa = 200),
  # A chain p <- j1 <- j2 of one-directional epsilon-dominance (m = 2); p is
  # covered within eps only by j1.
  chain = list(mu = rbind(A1 = c(0.46, 0.40), A2 = c(0.38, 0.55), A3 = c(0.30, 0.70), A4 = c(0.10, 0.95)),
               eps = 0.10, kappa = 200)
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
  write.csv(do.call(rbind, res), "paper/results/study1_validity.csv", row.names = FALSE)
}

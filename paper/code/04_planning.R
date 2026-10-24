# Planning horizon of Theorem 2 for the predictable plug-in Hoeffding sequence:
# K * (n*(eps/4) + b) evaluations, n*(x) = min{n : 2 h(n) < x}, where h is the
# deterministic half-width at level alpha / (K m) (Waudby-Smith and Ramdas, 2024).
hw <- function(n, alpha) {
  t <- seq_len(n); A <- log(2 / alpha)
  lam <- pmin(sqrt(8 * A / (t * log(t + 1))), 1)
  (A + cumsum(lam^2 / 8)) / cumsum(lam)
}
nstar <- function(x, alpha, nmax = 5e7) {
  n <- 1e3
  repeat { w <- 2 * hw(n, alpha); i <- which(w < x); if (length(i)) return(i[1]); n <- n * 4; if (n > nmax) return(NA) }
}
grid <- expand.grid(K = c(5, 10, 20), m = c(2, 3), eps = c(0.025, 0.05, 0.10), alpha = 0.05)
grid$nstar <- mapply(function(K, m, eps, alpha) nstar(eps / 4, alpha / (K * m)), grid$K, grid$m, grid$eps, grid$alpha)
grid$horizon <- grid$K * (grid$nstar + 5)
write.csv(grid, "paper/results/planning_horizon.csv", row.names = FALSE)
print(grid)

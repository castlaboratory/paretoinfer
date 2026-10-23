# Interval estimators for a binomial proportion ---------------------------------
#
# Eight 95% intervals; each returns a two-column matrix (lower, upper) for a
# vector of counts x out of n.

z95 <- stats::qnorm(0.975)

ci_wald <- function(x, n, z = z95) {
  p <- x / n; h <- z * sqrt(p * (1 - p) / n)
  cbind(pmax(p - h, 0), pmin(p + h, 1))
}
ci_wilson <- function(x, n, z = z95) {
  p <- x / n; c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(pmax(c0 - h, 0), pmin(c0 + h, 1))
}
ci_wilson_cc <- function(x, n, z = z95) {
  # Newcombe (1998), method 4.
  p <- x / n; q <- 1 - p; d <- 2 * (n + z^2)
  lo <- (2 * n * p + z^2 - 1 - z * sqrt(pmax(z^2 - 2 - 1 / n + 4 * p * (n * q + 1), 0))) / d
  up <- (2 * n * p + z^2 + 1 + z * sqrt(pmax(z^2 + 2 - 1 / n + 4 * p * (n * q - 1), 0))) / d
  lo[x == 0] <- 0; up[x == n] <- 1
  cbind(pmax(lo, 0), pmin(up, 1))
}
ci_agresti_coull <- function(x, n, z = z95) {
  nt <- n + z^2; pt <- (x + z^2 / 2) / nt; h <- z * sqrt(pt * (1 - pt) / nt)
  cbind(pmax(pt - h, 0), pmin(pt + h, 1))
}
ci_clopper_pearson <- function(x, n, a = 0.05) {
  lo <- ifelse(x == 0, 0, stats::qbeta(a / 2, x, n - x + 1))
  up <- ifelse(x == n, 1, stats::qbeta(1 - a / 2, x + 1, n - x))
  cbind(lo, up)
}
ci_jeffreys <- function(x, n, a = 0.05) {
  # Brown, Cai and DasGupta (2001): lower 0 at x = 0, upper 1 at x = n.
  lo <- ifelse(x == 0, 0, stats::qbeta(a / 2, x + 0.5, n - x + 0.5))
  up <- ifelse(x == n, 1, stats::qbeta(1 - a / 2, x + 0.5, n - x + 0.5))
  cbind(lo, up)
}
ci_arcsine <- function(x, n, z = z95) {
  pc <- (x + 0.375) / (n + 0.75); s <- asin(sqrt(pc)); h <- z / (2 * sqrt(n))
  cbind(sin(pmax(s - h, 0))^2, sin(pmin(s + h, pi / 2))^2)
}
ci_likelihood_ratio <- function(x, n, z = z95) {
  t(vapply(x, function(xx) {
    ph <- xx / n
    ll <- function(p) (if (xx > 0) xx * log(p) else 0) + (if (xx < n) (n - xx) * log1p(-p) else 0)
    lmax <- ll(min(max(ph, 1e-12), 1 - 1e-12))
    f <- function(p) 2 * (lmax - ll(p)) - z^2
    lo <- if (xx == 0) 0 else stats::uniroot(f, c(1e-12, ph), tol = 1e-10)$root
    up <- if (xx == n) 1 else stats::uniroot(f, c(ph, 1 - 1e-12), tol = 1e-10)$root
    c(lo, up)
  }, numeric(2)))
}

methods <- list(Wald = ci_wald, Wilson = ci_wilson, `Wilson-cc` = ci_wilson_cc,
                `Agresti-Coull` = ci_agresti_coull, `Clopper-Pearson` = ci_clopper_pearson,
                Jeffreys = ci_jeffreys, Arcsine = ci_arcsine, `Likelihood ratio` = ci_likelihood_ratio)

# Exact average miscoverage and width when p ~ U(0, 1): the posterior of p given
# x is Beta(x + 1, n - x + 1) and P(X = x) = 1 / (n + 1).
binomial_truth <- function(n) {
  x <- 0:n
  t(vapply(methods, function(f) {
    ci <- f(x, n)
    cover <- sum(stats::pbeta(ci[, 2], x + 1, n - x + 1) - stats::pbeta(ci[, 1], x + 1, n - x + 1)) / (n + 1)
    c(miscoverage = 1 - cover, width = sum(ci[, 2] - ci[, 1]) / (n + 1))
  }, numeric(2)))
}

# One evaluation of a method: the mean, over b simulated studies with
# p ~ U(0, 1) and X ~ Bin(n, p), of the miscoverage indicator and the width.
# Both lie in [0, 1]; the intervals are tabulated once for x = 0..n.
make_binomial_sim <- function(n, b = 50L) {
  tabs <- lapply(methods, function(f) f(0:n, n))
  function(k, nn) {
    ci <- tabs[[k]]
    t(vapply(seq_len(nn), function(r) {
      p <- stats::runif(b); x <- stats::rbinom(b, n, p)
      lo <- ci[x + 1, 1]; up <- ci[x + 1, 2]
      c(mean(p < lo | p > up), mean(up - lo))
    }, numeric(2)))
  }
}

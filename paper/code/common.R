# Shared code for the experiments of the paper ---------------------------------
#
# Every identification is run with the paretoinfer 0.2.0 state and kernels; the
# harness below replaces only the loop of pareto_run() so that (i) the
# discarding rule of version 0.1.0 can be run on the same state for
# comparison, (ii) uniform allocation can be compared with the adaptive rule,
# and (iii) every certificate issued at any round is checked against the truth.

# The package under study is installed in paper/.lib/current by
# paper/code/00_install.R, so that the experiments do not depend on whatever
# version happens to be installed in the user library.
if (dir.exists("paper/.lib/current")) .libPaths(c("paper/.lib/current", .libPaths()))
suppressPackageStartupMessages({
  library(paretoinfer)
  library(parallel)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Truth ------------------------------------------------------------------------

dominates <- function(a, b) all(a <= b) && any(a < b)

pareto_index <- function(mu) {
  K <- nrow(mu)
  which(vapply(seq_len(K), function(k) {
    !any(vapply(setdiff(seq_len(K), k), function(j) dominates(mu[j, ], mu[k, ]), logical(1)))
  }, logical(1)))
}

# k is epsilon-Pareto-optimal when no j has mu[j] <= mu[k] - eps everywhere.
eps_optimal_index <- function(mu, eps) {
  K <- nrow(mu)
  which(vapply(seq_len(K), function(k) {
    !any(vapply(setdiff(seq_len(K), k), function(j) all(mu[j, ] <= mu[k, ] - eps), logical(1)))
  }, logical(1)))
}

# Does the set S epsilon-cover the Pareto set (every p has s with mu[s] <= mu[p] + eps)?
covers_front <- function(mu, S, eps) {
  P <- pareto_index(mu)
  all(vapply(P, function(p) any(vapply(S, function(s) all(mu[s, ] <= mu[p, ] + eps), logical(1))), logical(1)))
}

# The discarding rule of version 0.1.0 ------------------------------------------
# (copied from R/sets.R at commit 0d1029c): k is discarded when some j satisfies
# U[j] <= L[k] + eps; epsilon-ties discard the later one in design order.
compute_sets_v010 <- function(state) {
  d <- state$design; K <- d$K
  L <- state$lower; U <- state$upper; eps <- d$epsilon
  dom <- matrix(FALSE, K, K)
  for (j in seq_len(K)) for (k in seq_len(K)) if (j != k) dom[j, k] <- all(U[j, ] <= L[k, ] + eps)
  discarded <- logical(K)
  for (k in seq_len(K)) {
    js <- which(dom[, k]); js <- js[order(dom[k, js], js)]
    for (j in js) if (!dom[k, j] || j < k) { discarded[k] <- TRUE; break }
  }
  survivors <- which(!discarded)
  status <- ifelse(discarded, "certified_dominated", "uncertain")
  for (k in survivors) {
    others <- setdiff(survivors, k)
    if (all(vapply(others, function(j) any(L[j, ] > U[k, ] + eps), logical(1)))) status[k] <- "certified_optimal"
  }
  status
}

# Harness ----------------------------------------------------------------------

# rule:    "v020" (exact-dominance discarding, epsilon acceptance) or "v010".
# sampler: "adaptive" (choose_next_evaluation of 0.2.0; for v010 the 0.1.0 rule,
#          widest uncertain box) or "uniform" (round robin over the alternatives
#          that are not certified dominated).
# boundary: any seqbench boundary, including the invalid "naive_fixed".
# correction: FALSE uses alpha for every confidence sequence (no union bound).
run_one <- function(mu, sim, eps, alpha = 0.05, boundary = "betting", rule = "v020",
                    sampler = "adaptive", batch = 5L, initial = 5L, max_evaluations = 2e4,
                    cost = 1, correction = TRUE, bounds = c(0, 1), check_every = 1L,
                    refine = FALSE) {
  K <- nrow(mu); m <- ncol(mu)
  alts <- rownames(mu) %||% paste0("A", seq_len(K))
  b0 <- if (boundary == "naive_fixed") "hoeffding" else boundary
  d <- pareto_design(alternatives = alts, objectives = m, bounds = bounds, alpha = alpha,
                     epsilon = eps, boundary = b0, cost = cost, max_evaluations = Inf,
                     refine = refine)
  d$boundary <- boundary
  if (!correction) d$alpha_each <- alpha
  st <- initialize_pareto(d)
  epsv <- d$epsilon
  P <- pareto_index(mu); E <- eps_optimal_index(mu, epsv)
  err <- c(false_discard = FALSE, false_accept = FALSE, false_optimal = FALSE,
           cover_fail = FALSE, frontier_loss = FALSE)
  feed <- function(st, k, n) {
    y <- as.data.frame(sim(k, n)); names(y) <- d$objectives
    y$alternative <- alts[k]
    if (length(cost) > 1L) y$cost <- cost[k]
    st$stopping_reason <- NA_character_
    st <- update_objectives(st, y)
    st$stopping_reason <- NA_character_
    st
  }
  status_of <- function(st) {
    if (rule == "v020") paretoinfer:::compute_sets(st)$status else compute_sets_v010(st)
  }
  check <- function(st, status) {
    disc <- which(status == "certified_dominated"); surv <- which(status != "certified_dominated")
    if (rule == "v020") {
      # A discard is false when the alternative is not truly dominated.
      err["false_discard"] <<- err["false_discard"] || any(!vapply(disc, function(k) any(vapply(setdiff(seq_len(K), k), function(j) dominates(mu[j, ], mu[k, ]), logical(1))), logical(1)))
      acc <- which(status %in% c("certified_optimal", "epsilon_optimal"))
      err["false_accept"] <<- err["false_accept"] || any(!(acc %in% E))
      err["false_optimal"] <<- err["false_optimal"] || any(!(which(status == "certified_optimal") %in% P))
      tab <- paretoinfer:::compute_sets(st)
      R <- which(tab$representative)
      if (all(status != "uncertain")) err["cover_fail"] <<- err["cover_fail"] || !covers_front(mu, R, epsv)
    } else {
      # v010 certificates are epsilon-dominance; a discard is false when the
      # alternative is not truly epsilon-dominated by any other.
      err["false_discard"] <<- err["false_discard"] || any(!vapply(disc, function(k) any(vapply(setdiff(seq_len(K), k), function(j) all(mu[j, ] <= mu[k, ] + epsv), logical(1))), logical(1)))
      err["false_optimal"] <<- err["false_optimal"] || any(!(which(status == "certified_optimal") %in% E))
    }
    # Common target: the plausible set must epsilon-cover the Pareto set.
    err["frontier_loss"] <<- err["frontier_loss"] || !covers_front(mu, surv, epsv)
  }
  for (k in seq_len(K)) st <- feed(st, k, initial)
  rounds <- 0L; rr <- 0L
  repeat {
    status <- status_of(st)
    if (rounds %% check_every == 0L || all(status != "uncertain")) check(st, status)
    if (all(status != "uncertain") || st$n_evaluations >= max_evaluations) break
    if (sampler == "uniform") {
      surv <- which(status != "certified_dominated")
      for (k in surv) st <- feed(st, k, batch)
    } else if (rule == "v020") {
      nxt <- choose_next_evaluation(st, n = 1L)
      if (nrow(nxt) == 0L) break
      st <- feed(st, match(nxt$alternative, alts), batch)
    } else {
      unc <- which(status == "uncertain")
      w <- vapply(unc, function(k) if (st$n[k] == 0L) 1 else max(st$upper[k, ] - st$lower[k, ]), numeric(1))
      cst <- pmax(d$cost[unc], .Machine$double.eps)
      st <- feed(st, unc[order(-w / cst, st$n[unc])[1]], batch)
    }
    rounds <- rounds + 1L
  }
  status <- status_of(st)
  data.frame(n_evaluations = st$n_evaluations, total_cost = st$total_cost,
             identified = all(status != "uncertain"),
             n_plausible = sum(status != "certified_dominated"),
             n_accepted = sum(status %in% c("certified_optimal", "epsilon_optimal")),
             as.list(err))
}

# Correlated bounded noise: Gaussian copula with Beta margins of mean mu and
# concentration kappa (variance mu (1 - mu) / (kappa + 1)).
make_beta_sim <- function(mu, kappa = 20, rho = 0) {
  m <- ncol(mu)
  S <- matrix(rho, m, m); diag(S) <- 1; C <- chol(S)
  function(k, n) {
    z <- matrix(stats::rnorm(n * m), n, m) %*% C
    u <- stats::pnorm(z)
    vapply(seq_len(m), function(i) stats::qbeta(u[, i], mu[k, i] * kappa, (1 - mu[k, i]) * kappa), numeric(n)) |>
      matrix(n, m)
  }
}

# Run R replications in parallel with reproducible streams.
mc <- function(R, fun, seed, cores = max(1L, parallel::detectCores() - 0L)) {
  RNGkind("L'Ecuyer-CMRG"); set.seed(seed)
  out <- parallel::mclapply(seq_len(R), function(r) cbind(rep = r, fun()), mc.cores = cores,
                            mc.set.seed = TRUE)
  bad <- vapply(out, inherits, logical(1), "try-error")
  if (any(bad)) stop("replication failed: ", as.character(out[[which(bad)[1]]]))
  do.call(rbind, out)
}

# Instances --------------------------------------------------------------------

# K alternatives, m objectives on [0, 1]: about half lie on a linear front
# sum(mu) = c, the rest are front points shifted up by U(gmin, gmax) in every
# objective (dominated with gap at least gmin).
make_instance <- function(K, m = 2L, n_front = ceiling(K / 2), gmin = 0.02, gmax = 0.15) {
  front <- matrix(stats::rexp(n_front * m), n_front, m)
  front <- front / rowSums(front)                    # on the simplex
  front <- 0.2 + 0.5 * front * (m / 2)               # inside [0.2, 0.7] for m = 2
  front <- pmin(front, 0.8)
  dom <- front[sample(n_front, K - n_front, replace = TRUE), , drop = FALSE] +
    matrix(stats::runif((K - n_front) * m, gmin, gmax), K - n_front, m)
  mu <- rbind(front, pmin(dom, 0.9))
  rownames(mu) <- paste0("A", seq_len(K))
  mu
}

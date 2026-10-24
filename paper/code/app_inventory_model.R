# (s, S) inventory policies -----------------------------------------------------
#
# Periodic review with backlogging, after the classic model of Law (2015,
# Section 1.5) with bounded demand so that both objectives have known bounds.
# Demand per period D ~ Binomial(50, 1/2) (mean 25); at the end of a period
# with inventory level I < s an order of S - I units is placed and arrives at
# the start of the next period. Per-period cost: setup 32 plus 3 per unit
# ordered, holding 1 per unit on hand, backlog 5 per unit short. One
# replication runs a warm-up of 20 periods and records T = 60 periods.
# Objectives (minimised): average cost per period and the fraction of periods
# that end with a backlog.

policies <- expand.grid(s = c(10, 20, 30, 40), d = c(20, 40, 60, 80))
policies$S <- policies$s + policies$d
rownames(policies) <- sprintf("(%d,%d)", policies$s, policies$S)

# Known bounds: the start-of-period level is at least s_min, so the level after
# demand is at least s_min - 50; orders are at most S_max - s_min + 50.
inv_bounds <- local({
  smin <- min(policies$s); Smax <- max(policies$S)
  list(period_cost = c(0, 32 + 3 * (Smax - smin + 50) + Smax + 5 * (50 - smin)), backlog = c(0, 1))
})

simulate_policy <- function(s, S, n, T = 60L, warmup = 20L) {
  t(vapply(seq_len(n), function(r) {
    I <- S; cost <- 0; back <- 0
    D <- stats::rbinom(T + warmup, 50, 0.5)
    for (t in seq_len(T + warmup)) {
      I <- I - D[t]
      q <- if (I < s) S - I else 0
      c_t <- (if (q > 0) 32 + 3 * q else 0) + max(I, 0) + 5 * max(-I, 0)
      if (t > warmup) { cost <- cost + c_t; back <- back + (I < 0) }
      I <- I + q
    }
    c(cost / T, back / T)
  }, numeric(2)))
}

make_inventory_sim <- function() {
  function(k, n) simulate_policy(policies$s[k], policies$S[k], n)
}

# Exact objectives: the level at the start of a period is a Markov chain on
# s..S started at S, so the expected average cost and backlog fraction over
# periods warmup + 1, ..., warmup + T are finite sums.
policy_exact <- function(s, S, T = 60L, warmup = 20L) {
  lev <- s:S; p <- c(numeric(length(lev) - 1L), 1)   # start at S
  dd <- 0:50; pd <- stats::dbinom(dd, 50, 0.5)
  cost <- 0; back <- 0
  for (t in seq_len(T + warmup)) {
    p_new <- numeric(length(lev)); ec <- 0; eb <- 0
    for (a in seq_along(lev)) if (p[a] > 0) {
      I <- lev[a] - dd; q <- ifelse(I < s, S - I, 0)
      c_t <- ifelse(q > 0, 32 + 3 * q, 0) + pmax(I, 0) + 5 * pmax(-I, 0)
      ec <- ec + p[a] * sum(pd * c_t); eb <- eb + p[a] * sum(pd * (I < 0))
      nxt <- ifelse(I < s, S, I) - s + 1L
      for (u in seq_along(nxt)) p_new[nxt[u]] <- p_new[nxt[u]] + p[a] * pd[u]
    }
    if (t > warmup) { cost <- cost + ec; back <- back + eb }
    p <- p_new
  }
  c(period_cost = cost / T, backlog = back / T)
}

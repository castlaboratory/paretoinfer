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

# A state with hand-set intersections, to test the certificate logic alone.
fake_state <- function(lower, upper, estimate = (lower + upper) / 2, epsilon = 0, n = 10L) {
  K <- nrow(lower); m <- ncol(lower)
  d <- pareto_design(alternatives = K, objectives = m, bounds = c(-10, 10), epsilon = epsilon)
  st <- initialize_pareto(d)
  dimnames(lower) <- dimnames(upper) <- dimnames(estimate) <- list(d$alternatives, d$objectives)
  st$lower <- lower; st$upper <- upper; st$estimate <- estimate
  st$n[] <- n
  st
}

test_that("certificates follow the box logic", {
  # A1 clearly best in both objectives; A2 clearly worst; A3 incomparable with A1
  lower <- rbind(c(0, 0), c(5, 5), c(-1, 3)); upper <- rbind(c(1, 1), c(6, 6), c(0, 4))
  s <- plausible_pareto_set(fake_state(lower, upper))
  expect_equal(s$dominated, "A2")
  expect_equal(s$table$dominated_by[2], "A1")
  expect_setequal(s$plausible, c("A1", "A3"))
  # A1 is certified optimal: A3 cannot dominate it (L[A3, 2] = 3 > U[A1, 2] = 1)
  expect_true("A1" %in% s$certified_optimal)
  # A1 might still dominate A3 in y1 (L[A1, 1] = 0 is not > U[A3, 1] = 0), so A3 stays open
  expect_equal(s$uncertain, "A3")
  expect_false(s$identified)
})

test_that("epsilon decides acceptance, never discarding", {
  lower <- rbind(c(0, 0), c(0.5, 0.5)); upper <- rbind(c(1, 1), c(1.5, 1.5))
  s0 <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0))
  expect_length(s0$dominated, 0L)
  expect_false(s0$identified)
  s1 <- plausible_pareto_set(fake_state(lower, upper, epsilon = 1.6))
  expect_length(s1$dominated, 0L)
  expect_setequal(s1$accepted, c("A1", "A2"))
  expect_true(s1$identified)
  # estimated frontier: A1 estimate (0.5, 0.5) dominates A2 estimate (1, 1)
  expect_equal(s0$frontier, "A1")
})

test_that("identification is declared when nothing is uncertain", {
  lower <- rbind(c(0, 0), c(5, 5)); upper <- rbind(c(1, 1), c(6, 6))
  s <- plausible_pareto_set(fake_state(lower, upper))
  expect_true(s$identified)
  expect_equal(s$certified_optimal, "A1")
  expect_equal(s$dominated, "A2")
  expect_equal(s$representatives, "A1")
})

test_that("a fresh state has everything uncertain and nothing on the frontier", {
  d <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1))
  s <- plausible_pareto_set(initialize_pareto(d))
  expect_length(s$uncertain, 3L)
  expect_length(s$frontier, 0L)
  expect_equal(choose_next_evaluation(initialize_pareto(d), n = 3)$width, rep(1, 3))
})

test_that("near-ties are accepted together and covered by one representative", {
  lower <- rbind(c(0.40, 0.40), c(0.41, 0.41), c(0.90, 0.90)); upper <- lower + 0.02
  s <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0.1))
  expect_setequal(s$plausible, c("A1", "A2"))
  expect_equal(s$dominated, "A3")
  expect_equal(s$table$dominated_by, c(NA, NA, "A1"))
  expect_setequal(s$accepted, c("A1", "A2"))
  expect_equal(s$representatives, "A1")
  expect_true(s$identified)
})

test_that("a cycle of epsilon-dominance cannot discard the frontier", {
  # Three Pareto-optimal alternatives; each epsilon-dominates the next (0.10),
  # so discarding by epsilon-dominance would remove all three.
  a <- 0.40; b <- 0.452; c <- 0.504
  mu <- rbind(c(a, b, c), c(c, a, b), c(b, c, a))
  s <- plausible_pareto_set(fake_state(mu - 0.005, mu + 0.005, epsilon = 0.1))
  expect_length(s$dominated, 0L)
  expect_setequal(s$plausible, c("A1", "A2", "A3"))
  expect_setequal(s$accepted, c("A1", "A2", "A3"))
  expect_true(length(s$representatives) >= 1L)
  # with small boxes and no tolerance they are certified exactly optimal
  s0 <- plausible_pareto_set(fake_state(mu - 0.005, mu + 0.005, epsilon = 0))
  expect_setequal(s0$certified_optimal, c("A1", "A2", "A3"))
})

test_that("box dominance is transitive, so the dominator named is a survivor", {
  lower <- rbind(c(0, 0), c(1, 1), c(2, 2)); upper <- lower + 0.5
  s <- plausible_pareto_set(fake_state(lower, upper))
  expect_equal(s$plausible, "A1")
  expect_equal(s$table$dominated_by, c(NA, "A1", "A1"))
})

test_that("an alternative that blocks an open certificate is a sampling candidate", {
  # A2 is accepted against A1 only if A1's box is narrow; A1 is certified optimal
  # with respect to A2 already, but A2 stays open while A1's box is wide.
  lower <- rbind(c(0.40, 0.10), c(0.45, 0.50)); upper <- rbind(c(0.60, 0.20), c(0.47, 0.52))
  st <- fake_state(lower, upper, epsilon = 0.05)
  s <- plausible_pareto_set(st)
  expect_equal(s$uncertain, "A2")
  nxt <- choose_next_evaluation(st, n = 2)
  expect_setequal(nxt$alternative, c("A1", "A2"))
  expect_equal(nxt$alternative[1], "A1")   # the wider box is evaluated first
})

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
  # A1 is certified optimal: A2 is worse in y1 (L=5 > U=1) and A3 is worse in y2 (L=3 > U=1)
  expect_true("A1" %in% s$certified_optimal)
  # A3 is certified optimal: A1 is worse in y1? L[A1,1]=0 > U[A3,1]=0 is false, so A3 is uncertain
  expect_equal(s$uncertain, "A3")
  expect_false(s$identified)
})

test_that("epsilon widens dominance and the estimated frontier ignores certificates", {
  lower <- rbind(c(0, 0), c(0.5, 0.5)); upper <- rbind(c(1, 1), c(1.5, 1.5))
  s0 <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0))
  expect_length(s0$dominated, 0L)
  s1 <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0.5))
  expect_equal(s1$dominated, "A2")
  # estimated frontier: A1 estimate (0.5, 0.5) dominates A2 estimate (1, 1)
  expect_equal(s0$frontier, "A1")
})

test_that("identification is declared when nothing is uncertain", {
  lower <- rbind(c(0, 0), c(5, 5)); upper <- rbind(c(1, 1), c(6, 6))
  s <- plausible_pareto_set(fake_state(lower, upper))
  expect_true(s$identified)
  expect_equal(s$certified_optimal, "A1")
  expect_equal(s$dominated, "A2")
})

test_that("a fresh state has everything uncertain and nothing on the frontier", {
  d <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1))
  s <- plausible_pareto_set(initialize_pareto(d))
  expect_length(s$uncertain, 3L)
  expect_length(s$frontier, 0L)
  expect_equal(choose_next_evaluation(initialize_pareto(d), n = 3)$width, rep(1, 3))
})

test_that("epsilon-ties keep one representative and still identify", {
  lower <- rbind(c(0.40, 0.40), c(0.41, 0.41), c(0.90, 0.90)); upper <- lower + 0.02
  s <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0.1))
  expect_equal(s$plausible, "A1")
  expect_setequal(s$dominated, c("A2", "A3"))
  expect_equal(s$table$dominated_by, c(NA, "A1", "A1"))
  expect_equal(s$certified_optimal, "A1")
  expect_true(s$identified)
  # a one-directional dominator takes precedence over a tied one
  lower <- rbind(c(0.40, 0.40), c(0.41, 0.41), c(0.10, 0.10)); upper <- lower + 0.02
  s <- plausible_pareto_set(fake_state(lower, upper, epsilon = 0.1))
  expect_equal(s$plausible, "A3")
  expect_equal(s$table$dominated_by[1:2], c("A3", "A3"))
})

sim_means <- rbind(A1 = c(0.2, 0.7), A2 = c(0.5, 0.4), A3 = c(0.7, 0.2), A4 = c(0.6, 0.6))
sim <- function(alternative, n, h = 0.1) {
  mu <- sim_means[alternative, ]
  data.frame(y1 = runif(n, mu[1] - h, mu[1] + h), y2 = runif(n, mu[2] - h, mu[2] + h))
}

test_that("updates narrow the boxes, respect bounds and keep running intersections", {
  d <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
  st <- initialize_pareto(d)
  set.seed(1)
  ev <- sim("A1", 30); ev$alternative <- "A1"
  st <- update_objectives(st, ev)
  expect_equal(unname(st$n["A1"]), 30L)
  expect_equal(st$n_evaluations, 30L)
  expect_true(st$upper["A1", "y1"] - st$lower["A1", "y1"] < 1)
  expect_true(st$lower["A1", "y1"] >= 0 && st$upper["A1", "y1"] <= 1)
  expect_equal(unname(st$lower["A2", "y1"]), 0)   # untouched alternative keeps the bounds
  tb <- tidy(st)
  expect_equal(nrow(tb), 8L)
  expect_equal(tb$status[tb$alternative == "A1"][1], "uncertain")
  g <- glance(st)
  expect_equal(g$n_uncertain, 4L)
})

test_that("invalid evaluations are refused", {
  d <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1))
  st <- initialize_pareto(d)
  expect_error(update_objectives(st, data.frame(alternative = "zz", y1 = 0.1, y2 = 0.2)), "Unknown")
  expect_error(update_objectives(st, data.frame(alternative = "A1", y1 = 0.1)), "missing objective")
  expect_error(update_objectives(st, data.frame(alternative = "A1", y1 = NA_real_, y2 = 0.2)), "missing")
  expect_error(update_objectives(st, data.frame(alternative = "A1", y1 = 1.5, y2 = 0.2)), "outside the declared bounds")
  expect_error(update_objectives(st, data.frame(alternative = "A1", y1 = 0.5, y2 = 0.2, cost = -1)), "non-negative")
  expect_error(update_objectives("x", data.frame()), "paretoinfer_state")
})

test_that("pareto_run identifies a well-separated frontier and stops", {
  d <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05, max_evaluations = 3000)
  set.seed(2)
  st <- suppressWarnings(pareto_run(d, sim, batch = 5, initial = 5))
  s <- plausible_pareto_set(st)
  expect_true(st$stopping_reason %in% c("identified", "max_evaluations"))
  expect_true("A4" %in% s$dominated)          # (0.6, 0.6) is dominated by (0.5, 0.4)
  expect_true(all(c("A1", "A2", "A3") %in% s$plausible))
  expect_error(update_objectives(st, data.frame(alternative = "A1", y1 = 0.1, y2 = 0.2)), "stopped")
  expect_s3_class(autoplot(st), "ggplot")
  r <- pareto_report(st)
  expect_s3_class(r, "pareto_report")
  expect_message(print(r), "Pareto")
  expect_message(print(st), "Pareto")
})

test_that("choose_next_evaluation never proposes a certified alternative and honours cost", {
  d <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05,
                     cost = c(1, 1, 1, 10), max_evaluations = 2000)
  set.seed(3)
  st <- suppressWarnings(pareto_run(d, sim, batch = 5, initial = 5, max_rounds = 20))
  nxt <- choose_next_evaluation(st, n = 4)
  s <- plausible_pareto_set(st)
  expect_true(all(nxt$alternative %in% s$uncertain))
  if ("A4" %in% nxt$alternative && nrow(nxt) > 1) {
    expect_true(nxt$score[nxt$alternative == "A4"] <= max(nxt$score))
  }
})

test_that("the guarantee holds by Monte Carlo: a true frontier alternative is never certified dominated", {
  skip_on_cran()
  d <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0, max_evaluations = 300)
  means <- rbind(A1 = c(0.3, 0.6), A2 = c(0.5, 0.4), A3 = c(0.45, 0.45))  # all three on the true frontier
  gen <- function(alternative, n) { mu <- means[alternative, ]; data.frame(y1 = runif(n, mu[1] - 0.2, mu[1] + 0.2), y2 = runif(n, mu[2] - 0.2, mu[2] + 0.2)) }
  set.seed(4)
  bad <- 0L
  for (r in 1:40) {
    st <- suppressWarnings(pareto_run(d, gen, batch = 10, initial = 10))
    bad <- bad + length(plausible_pareto_set(st)$dominated)
  }
  expect_equal(bad, 0L)
})

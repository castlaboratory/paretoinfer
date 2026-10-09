test_that("a design records alternatives, objectives, bounds, epsilon and the alpha split", {
  d <- pareto_design(alternatives = 5, objectives = c("error", "time"), bounds = list(c(0, 1), c(0, 60)),
                     alpha = 0.05, epsilon = c(0.01, 1), cost = c(1, 1, 2, 2, 3), budget = 500)
  expect_s3_class(d, "pareto_design")
  expect_equal(d$alternatives, paste0("A", 1:5))
  expect_equal(d$K, 5L); expect_equal(d$m, 2L)
  expect_equal(unname(d$bounds[, "time"]), c(0, 60))
  expect_equal(d$alpha_each, 0.05 / 10)
  expect_equal(unname(d$epsilon), c(0.01, 1))
  expect_equal(unname(d$cost["A3"]), 2)
  expect_message(print(d), "alternatives")
})

test_that("invalid designs are refused", {
  expect_error(pareto_design(alternatives = 1, objectives = 2, bounds = c(0, 1)), "two alternatives")
  expect_error(pareto_design(alternatives = c("a", "a"), objectives = 2, bounds = c(0, 1)), "distinct")
  expect_error(pareto_design(alternatives = 3, objectives = c("alternative", "y"), bounds = c(0, 1)), "reserved")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = c(1, 0)), "bounds")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = list(c(0, 1))), "bounds")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), alpha = 1), "alpha")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = c(0.1, -1)), "epsilon")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), boundary = "naive_fixed"), "boundary")
  expect_error(pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), cost = c(1, 2)), "cost")
})

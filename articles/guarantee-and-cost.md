# Checking the guarantee and the cost

``` r

library(paretoinfer)
```

Two numbers describe an identification procedure: how often it discards
an alternative that should have stayed, which the guarantee bounds by
`alpha`, and how many evaluations it needs, which the guarantee does not
bound at all. This vignette measures both on simulated problems.

``` r

means <- rbind(A1 = c(0.15, 0.70), A2 = c(0.40, 0.40), A3 = c(0.70, 0.15),
               A4 = c(0.42, 0.43), A5 = c(0.60, 0.60), A6 = c(0.30, 0.80))
simulate <- function(alternative, n, h = 0.1) {
  mu <- means[alternative, ]
  data.frame(y1 = runif(n, mu[1] - h, mu[1] + h), y2 = runif(n, mu[2] - h, mu[2] + h))
}
design <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                        bounds = c(0, 1), alpha = 0.05, epsilon = 0.05, max_evaluations = 3000)
```

## Adaptive versus uniform evaluation

[`pareto_run()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_run.md)
evaluates the most uncertain plausible alternative next. The alternative
is to evaluate every alternative in turn until the same stopping rule
fires. Both are valid; the adaptive rule spends fewer evaluations
because it stops evaluating alternatives whose status is already
certified.

``` r

set.seed(1)
adaptive <- pareto_run(design, simulate, batch = 5, initial = 5)
glance(adaptive)[, c("n_evaluations", "n_certified_optimal", "n_dominated", "identified")]
#> # A tibble: 1 × 4
#>   n_evaluations n_certified_optimal n_dominated identified
#>           <int>               <int>       <int> <lgl>     
#> 1           645                   3           3 TRUE
```

``` r

set.seed(1)
uniform <- initialize_pareto(design)
while (is.na(uniform$stopping_reason)) {
  for (k in design$alternatives) {
    ev <- simulate(k, 5); ev$alternative <- k
    uniform <- update_objectives(uniform, ev)
    if (!is.na(uniform$stopping_reason)) break
  }
}
glance(uniform)[, c("n_evaluations", "n_certified_optimal", "n_dominated", "identified")]
#> # A tibble: 1 × 4
#>   n_evaluations n_certified_optimal n_dominated identified
#>           <int>               <int>       <int> <lgl>     
#> 1          1190                   3           3 TRUE
```

The evaluations per alternative show where the adaptive rule saved: the
clearly dominated alternatives were certified early and left alone.

``` r

data.frame(alternative = design$alternatives,
           adaptive = unname(adaptive$n), uniform = unname(uniform$n))
#>   alternative adaptive uniform
#> 1          A1       85     200
#> 2          A2      175     200
#> 3          A3       70     200
#> 4          A4      175     200
#> 5          A5       55     195
#> 6          A6       85     195
```

## The guarantee: no false discard

Three alternatives, all on the true frontier, with noise of half-width
0.2 and `epsilon = 0`: nothing may ever be certified dominated. Twelve
independent identifications with a budget of 300 evaluations each are
run and the number of discards counted. The guarantee says the expected
number of runs with at least one false discard is at most `alpha` times
twelve, below one.

``` r

frontier <- rbind(A1 = c(0.30, 0.60), A2 = c(0.50, 0.40), A3 = c(0.45, 0.45))
sim_frontier <- function(alternative, n) {
  mu <- frontier[alternative, ]
  data.frame(y1 = runif(n, mu[1] - 0.2, mu[1] + 0.2), y2 = runif(n, mu[2] - 0.2, mu[2] + 0.2))
}
design0 <- pareto_design(alternatives = rownames(frontier), objectives = c("y1", "y2"),
                         bounds = c(0, 1), alpha = 0.05, epsilon = 0, max_evaluations = 300)
set.seed(2)
discards <- replicate(12, {
  state <- suppressWarnings(pareto_run(design0, sim_frontier, batch = 10, initial = 10))
  length(plausible_pareto_set(state)$dominated)
})
table(discards)
#> discards
#>  0 
#> 12
```

With `epsilon = 0` and alternatives that are genuinely close, the
procedure cannot certify anything and runs to its budget; that is the
correct outcome, not a failure. The plausible set is the honest answer:
all three.

## The price of the tolerance

The tolerance `epsilon` is what lets the procedure stop on near-ties. A
larger tolerance certifies the near-tied `A4` sooner and ends the
identification with fewer evaluations; a smaller one needs more evidence
to separate `A4` from `A2`.

``` r

set.seed(3)
by_epsilon <- sapply(c(0.02, 0.05, 0.10), function(eps) {
  d <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                     bounds = c(0, 1), alpha = 0.05, epsilon = eps, max_evaluations = 4000)
  s <- pareto_run(d, simulate, batch = 5, initial = 5)
  c(evaluations = s$n_evaluations, dominated = length(plausible_pareto_set(s)$dominated))
})
colnames(by_epsilon) <- paste0("eps = ", c(0.02, 0.05, 0.10))
by_epsilon
#>             eps = 0.02 eps = 0.05 eps = 0.1
#> evaluations        870        605       535
#> dominated            3          3         3
```

## What the level buys

A smaller `alpha` widens every confidence box and delays every
certificate. The cost is roughly logarithmic in `1 / alpha`, which is
why `alpha = 0.05` and `alpha = 0.01` are not far apart.

``` r

set.seed(4)
by_alpha <- sapply(c(0.10, 0.05, 0.01), function(a) {
  d <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                     bounds = c(0, 1), alpha = a, epsilon = 0.05, max_evaluations = 4000)
  pareto_run(d, simulate, batch = 5, initial = 5)$n_evaluations
})
data.frame(alpha = c(0.10, 0.05, 0.01), evaluations = by_alpha)
#>   alpha evaluations
#> 1  0.10         575
#> 2  0.05         695
#> 3  0.01         815
```

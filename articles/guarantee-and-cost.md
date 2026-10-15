# Checking the guarantee and the cost

``` r

library(paretoinfer)
library(dplyr)
library(purrr)
```

Two numbers describe an identification procedure: how often it discards
an alternative that should have stayed, which the guarantee bounds by
`alpha`, and how many evaluations it needs, which the guarantee does not
bound at all. This vignette measures both on simulated problems.

``` r

means <- tribble(
  ~alternative,   ~y1,  ~y2,
  "A1",          0.15, 0.70,
  "A2",          0.40, 0.40,
  "A3",          0.70, 0.15,
  "A4",          0.42, 0.43,
  "A5",          0.60, 0.60,
  "A6",          0.30, 0.80
)
simulate <- function(alternative, n, h = 0.1) {
  mu <- means |> filter(alternative == .env$alternative)
  tibble(y1 = runif(n, mu$y1 - h, mu$y1 + h),
         y2 = runif(n, mu$y2 - h, mu$y2 + h))
}
design <- pareto_design(alternatives = means$alternative, objectives = c("y1", "y2"),
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
glance(adaptive) |>
  select(n_evaluations, n_certified_optimal, n_dominated, identified)
#> # A tibble: 1 × 4
#>   n_evaluations n_certified_optimal n_dominated identified
#>           <int>               <int>       <int> <lgl>     
#> 1           645                   3           3 TRUE
```

``` r

one_round <- function() {
  design$alternatives |>
    map(\(k) simulate(k, 5) |> mutate(alternative = k)) |>
    list_rbind()
}
set.seed(1)
uniform <- initialize_pareto(design)
while (is.na(uniform$stopping_reason)) {
  uniform <- uniform |> update_objectives(one_round())
}
glance(uniform) |>
  select(n_evaluations, n_certified_optimal, n_dominated, identified)
#> # A tibble: 1 × 4
#>   n_evaluations n_certified_optimal n_dominated identified
#>           <int>               <int>       <int> <lgl>     
#> 1          1200                   3           3 TRUE
```

The evaluations per alternative show where the adaptive rule saved: the
clearly dominated alternatives were certified early and left alone.

``` r

tibble(alternative = design$alternatives,
       adaptive = adaptive$n,
       uniform = uniform$n)
#> # A tibble: 6 × 3
#>   alternative adaptive uniform
#>   <chr>          <int>   <int>
#> 1 A1                85     200
#> 2 A2               175     200
#> 3 A3                70     200
#> 4 A4               175     200
#> 5 A5                55     200
#> 6 A6                85     200
```

## The guarantee: no false discard

Three alternatives, all on the true frontier, with noise of half-width
0.2 and `epsilon = 0`: nothing may ever be certified dominated. Twelve
independent identifications with a budget of 300 evaluations each are
run and the number of discards counted. The guarantee says the expected
number of runs with at least one false discard is at most `alpha` times
twelve, below one.

``` r

frontier <- tribble(
  ~alternative,   ~y1,  ~y2,
  "A1",          0.30, 0.60,
  "A2",          0.50, 0.40,
  "A3",          0.45, 0.45
)
sim_frontier <- function(alternative, n, h = 0.2) {
  mu <- frontier |> filter(alternative == .env$alternative)
  tibble(y1 = runif(n, mu$y1 - h, mu$y1 + h),
         y2 = runif(n, mu$y2 - h, mu$y2 + h))
}
design0 <- pareto_design(alternatives = frontier$alternative, objectives = c("y1", "y2"),
                         bounds = c(0, 1), alpha = 0.05, epsilon = 0, max_evaluations = 300)
set.seed(2)
discards <- tibble(run = 1:12) |>
  mutate(n_discarded = map_int(run, \(i) {
    state <- suppressWarnings(pareto_run(design0, sim_frontier, batch = 10, initial = 10))
    length(plausible_pareto_set(state)$dominated)
  }))
discards |> count(n_discarded)
#> # A tibble: 1 × 2
#>   n_discarded     n
#>         <int> <int>
#> 1           0    12
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

run_with <- function(alpha = 0.05, epsilon = 0.05) {
  pareto_design(alternatives = means$alternative, objectives = c("y1", "y2"),
                bounds = c(0, 1), alpha = alpha, epsilon = epsilon,
                max_evaluations = 4000) |>
    pareto_run(simulate, batch = 5, initial = 5)
}
set.seed(3)
tibble(epsilon = c(0.02, 0.05, 0.10)) |>
  mutate(state = map(epsilon, \(eps) run_with(epsilon = eps)),
         evaluations = map_int(state, \(s) s$n_evaluations),
         dominated = map_int(state, \(s) length(plausible_pareto_set(s)$dominated))) |>
  select(-state)
#> # A tibble: 3 × 3
#>   epsilon evaluations dominated
#>     <dbl>       <int>     <int>
#> 1    0.02         870         3
#> 2    0.05         605         3
#> 3    0.1          535         3
```

## What the level buys

A smaller `alpha` widens every confidence box and delays every
certificate. The cost is roughly logarithmic in `1 / alpha`, which is
why `alpha = 0.05` and `alpha = 0.01` are not far apart.

``` r

set.seed(4)
tibble(alpha = c(0.10, 0.05, 0.01)) |>
  mutate(evaluations = map_int(alpha, \(a) run_with(alpha = a)$n_evaluations))
#> # A tibble: 3 × 2
#>   alpha evaluations
#>   <dbl>       <int>
#> 1  0.1          575
#> 2  0.05         695
#> 3  0.01         815
```

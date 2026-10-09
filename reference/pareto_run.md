# Run an identification to completion with a simulator

Repeats "choose the next alternative, evaluate it, update" until every
alternative is certified or the budget is exhausted.

## Usage

``` r
pareto_run(design, simulate, batch = 1L, initial = 2L, max_rounds = 1e+05)
```

## Arguments

- design:

  A
  [`pareto_design()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_design.md).

- simulate:

  A function `function(alternative, n)` returning a data frame (or
  matrix) with `n` rows and one column per objective, in the design's
  objective order or with the objective names.

- batch:

  Evaluations per call for the chosen alternative.

- initial:

  Evaluations of every alternative before the adaptive phase.

- max_rounds:

  Safety cap on the number of adaptive rounds.

## Value

The final `paretoinfer_state`.

## Examples

``` r
design <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05,
                        max_evaluations = 400)
means <- rbind(c(0.2, 0.7), c(0.5, 0.4), c(0.7, 0.2), c(0.6, 0.6))
sim <- function(alternative, n) {
  k <- match(alternative, design$alternatives)
  data.frame(y1 = runif(n, means[k, 1] - 0.1, means[k, 1] + 0.1),
             y2 = runif(n, means[k, 2] - 0.1, means[k, 2] + 0.1))
}
set.seed(1)
state <- pareto_run(design, sim)
state
#> 
#> ── Pareto identification ───────────────────────────────────────────────────────
#> 4 alternatives, 2 objectives, alpha 0.05, epsilon 0.05, 0.05.
#> 307 evaluations, cost 307.
#> • Estimated frontier: "A1", "A2", and "A3"
#> • Plausible Pareto set: "A1", "A2", and "A3" (certified optimal: A1, A2, A3)
#> • Certified dominated: A4
#> ℹ Stopped: identified.
```

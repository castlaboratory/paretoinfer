# The three sets of a Pareto identification

The three sets of a Pareto identification

## Usage

``` r
plausible_pareto_set(state)
```

## Arguments

- state:

  A `paretoinfer_state`.

## Value

A list of class `pareto_sets` with `frontier` (alternatives whose point
estimates are not dominated by the estimate of any other plausible
alternative), `plausible` (not certified dominated), `certified_optimal`
(plausible and certified not epsilon-dominated by any other),
`uncertain` (plausible but not certified), `dominated` (certified
epsilon-dominated), `identified` (no uncertain alternative left) and
`table`, a tibble with one row per alternative: `status`,
`on_estimated_frontier`, `dominated_by`, `n_evaluations`, `cost`,
`cs_empty`.

## Examples

``` r
design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
state <- initialize_pareto(design)
set.seed(1)
ev <- data.frame(alternative = rep(c("A1", "A2", "A3"), each = 40),
                 y1 = runif(120, c(0.1, 0.5, 0.6), c(0.3, 0.7, 0.8)),
                 y2 = runif(120, c(0.6, 0.1, 0.6), c(0.8, 0.3, 0.8)))
plausible_pareto_set(update_objectives(state, ev))
#> 
#> ── Pareto sets ──
#> 
#> • Estimated frontier: "A1"
#> • Plausible: "A1", "A2", and "A3"
#> • Certified optimal: none
#> • Uncertain: A1, A2, A3
#> • Certified dominated: none
```

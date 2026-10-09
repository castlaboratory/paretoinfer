# Update the identification with new evaluations

Feeds one or more evaluations, each a vector of objective values for one
alternative, to the confidence sequences. The running intersections are
updated and the three sets are recomputed.

## Usage

``` r
update_objectives(state, evaluations)
```

## Arguments

- state:

  A `paretoinfer_state`.

- evaluations:

  Data frame with a column `alternative` (name as in the design) and one
  numeric column per objective; an optional `cost` column overrides the
  design cost for those rows.

## Value

The updated state.

## Examples

``` r
design <- pareto_design(alternatives = c("a", "b"), objectives = c("y1", "y2"),
                        bounds = c(0, 1), epsilon = 0.05)
state <- initialize_pareto(design)
state <- update_objectives(state, data.frame(alternative = c("a", "b", "a"),
                                             y1 = c(0.2, 0.5, 0.25), y2 = c(0.6, 0.3, 0.55)))
state
#> 
#> ── Pareto identification ───────────────────────────────────────────────────────
#> 2 alternatives, 2 objectives, alpha 0.05, epsilon 0.05, 0.05.
#> 3 evaluations, cost 3.
#> • Estimated frontier: "a" and "b"
#> • Plausible Pareto set: "a" and "b"
#> • Certified dominated: none
#> Running; 2 alternatives still uncertain.
```

# Initialise a Pareto identification

Initialise a Pareto identification

## Usage

``` r
initialize_pareto(design)
```

## Arguments

- design:

  A
  [`pareto_design()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_design.md).

## Value

An object of class `paretoinfer_state`.

## Examples

``` r
design <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
state <- initialize_pareto(design)
state
#> 
#> ── Pareto identification ───────────────────────────────────────────────────────
#> 4 alternatives, 2 objectives, alpha 0.05, epsilon 0.05, 0.05.
#> No evaluations yet.
```

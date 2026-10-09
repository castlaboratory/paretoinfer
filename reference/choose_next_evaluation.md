# Choose the next alternative(s) to evaluate

Scores every uncertain alternative by the width of its simultaneous
confidence box, normalised by the objective ranges and divided by its
evaluation cost, and returns the highest-scoring ones. Certified
alternatives (dominated or optimal) are never proposed: more evaluations
cannot change their certificate. The rule is a heuristic that targets
the ambiguous comparisons; it does not affect the validity of the
certificates, which holds for any adaptive choice.

## Usage

``` r
choose_next_evaluation(state, n = 1L)
```

## Arguments

- state:

  A `paretoinfer_state`.

- n:

  Number of alternatives to propose.

## Value

A tibble with columns `alternative`, `score` (normalised width per unit
cost), `width` (largest normalised width over objectives) and
`n_evaluations`; zero rows when nothing is uncertain.

## Examples

``` r
design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
state <- initialize_pareto(design)
choose_next_evaluation(state, n = 2)
#> # A tibble: 2 × 4
#>   alternative score width n_evaluations
#>   <chr>       <dbl> <dbl>         <int>
#> 1 A1              1     1             0
#> 2 A2              1     1             0
```

# Glance at an identification

Glance at an identification

## Usage

``` r
# S3 method for class 'paretoinfer_state'
glance(x, ...)
```

## Arguments

- x:

  A `paretoinfer_state`.

- ...:

  Unused.

## Value

A one-row tibble: `K`, `m`, `alpha`, `n_evaluations`, `total_cost`,
`n_frontier`, `n_plausible`, `n_certified_optimal`, `n_uncertain`,
`n_dominated`, `identified`, `stopping_reason`.

## Examples

``` r
design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
glance(initialize_pareto(design))
#> # A tibble: 1 × 12
#>       K     m alpha n_evaluations total_cost n_frontier n_plausible
#>   <int> <int> <dbl>         <int>      <dbl>      <int>       <int>
#> 1     2     2  0.05             0          0          0           2
#> # ℹ 5 more variables: n_certified_optimal <int>, n_uncertain <int>,
#> #   n_dominated <int>, identified <lgl>, stopping_reason <chr>
```

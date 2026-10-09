# Tidy the confidence boxes

One row per alternative and objective with the point estimate, the
running intersection of the confidence sequence, the number of
evaluations and the status of the alternative.

## Usage

``` r
# S3 method for class 'paretoinfer_state'
tidy(x, ...)
```

## Arguments

- x:

  A `paretoinfer_state`.

- ...:

  Unused.

## Value

A tibble with columns `alternative`, `objective`, `estimate`, `lower`,
`upper`, `n_evaluations`, `status`.

## Examples

``` r
design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
state <- update_objectives(initialize_pareto(design),
  data.frame(alternative = c("A1", "A2"), y1 = c(0.2, 0.8), y2 = c(0.3, 0.9)))
tidy(state)
#> # A tibble: 4 × 7
#>   alternative objective estimate lower upper n_evaluations status   
#>   <chr>       <chr>        <dbl> <dbl> <dbl>         <int> <chr>    
#> 1 A1          y1             0.2     0     1             1 uncertain
#> 2 A2          y1             0.8     0     1             1 uncertain
#> 3 A1          y2             0.3     0     1             1 uncertain
#> 4 A2          y2             0.9     0     1             1 uncertain
```

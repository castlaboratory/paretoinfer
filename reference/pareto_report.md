# The full report of an identification

Everything needed to audit the result: the design (alternatives,
objectives, bounds, tolerance, level and its split, boundary, budget),
the evaluations and cost per alternative, the three sets with their
certificates, the simultaneous confidence boxes, the stopping reason and
the software versions.

## Usage

``` r
pareto_report(state)
```

## Arguments

- state:

  A `paretoinfer_state`.

## Value

A list of class `pareto_report`.

## Examples

``` r
design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
state <- update_objectives(initialize_pareto(design),
  data.frame(alternative = c("A1", "A2"), y1 = c(0.2, 0.8), y2 = c(0.3, 0.9)))
pareto_report(state)
#> 
#> ── Pareto identification report ────────────────────────────────────────────────
#> 2 alternatives, objectives y1 and y2, epsilon 0.1, 0.1, alpha 0.05, boundary
#> "betting".
#> 2 evaluations, cost 2; running.
#> 
#> ── Pareto sets ──
#> 
#> • Estimated frontier: "A1"
#> • Plausible: "A1" and "A2"
#> • Certified optimal: none
#> • Uncertain: A1, A2
#> • Certified dominated: none
#> 
#> ── Assumptions ──
#> 
#> • Every objective value lies in its declared bounds; conditionally on the past,
#> each evaluation of an alternative has the alternative's mean objective vector
#> (fresh draws, any dependence between objectives).
#> • The 4 confidence sequences share alpha = 0.05 by a union bound (0.0125 each);
#> with probability at least 0.95 all true means lie in their running
#> intersections at every time, and every certificate below is then true.
#> • Dominance is epsilon-dominance with epsilon = (0.1, 0.1): j dominates k when
#> mu[j] <= mu[k] + epsilon in every objective.
#> • The estimated frontier is a point estimate without a certificate; the
#> plausible set and the certified sets carry the guarantee.
#> paretoinfer 0.0.0.9000, seqbench 0.1.0, R version 4.6.1 (2026-06-24)
```

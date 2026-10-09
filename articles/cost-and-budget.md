# Heterogeneous cost and budgets

``` r

library(paretoinfer)
```

Evaluating an alternative is rarely free, and rarely equally expensive
across alternatives: a heuristic with more restarts, a model with more
parameters, a field trial at a remote site. The design records a cost
per alternative, the next-evaluation rule divides uncertainty by cost,
and a budget stops the identification with an honest “inconclusive” when
it runs out.

``` r

means <- rbind(A1 = c(0.15, 0.70), A2 = c(0.40, 0.40), A3 = c(0.70, 0.15),
               A4 = c(0.42, 0.43), A5 = c(0.60, 0.60), A6 = c(0.30, 0.80))
simulate <- function(alternative, n, h = 0.1) {
  mu <- means[alternative, ]
  data.frame(y1 = runif(n, mu[1] - h, mu[1] + h), y2 = runif(n, mu[2] - h, mu[2] + h))
}
```

## A budget that runs out

`A5` and `A6` cost five units per evaluation, the others one. With a
budget of 400 units the procedure stops before anything is certified.

``` r

design <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                        bounds = c(0, 1), alpha = 0.05, epsilon = 0.05,
                        cost = c(1, 1, 1, 1, 5, 5), budget = 400)
set.seed(2)
state <- pareto_run(design, simulate, batch = 5, initial = 5)
state
#> 
#> ── Pareto identification ───────────────────────────────────────────────────────
#> 6 alternatives, 2 objectives, alpha 0.05, epsilon 0.05, 0.05.
#> 320 evaluations, cost 400.
#> • Estimated frontier: "A1", "A2", and "A3"
#> • Plausible Pareto set: "A1", "A2", "A3", "A4", "A5", and "A6"
#> • Certified dominated: none
#> ℹ Stopped: budget.
plausible_pareto_set(state)$table[, c("alternative", "status", "n_evaluations", "cost")]
#> # A tibble: 6 × 4
#>   alternative status    n_evaluations  cost
#>   <chr>       <chr>             <int> <dbl>
#> 1 A1          uncertain            75    75
#> 2 A2          uncertain            70    70
#> 3 A3          uncertain            80    80
#> 4 A4          uncertain            75    75
#> 5 A5          uncertain            10    50
#> 6 A6          uncertain            10    50
```

The expensive alternatives received only their initial evaluations: the
rule preferred to narrow the cheap boxes. The outcome is inconclusive,
with the estimated frontier as the point estimate and every alternative
still plausible. The report says so and records the budget.

``` r

pareto_report(state)
#> 
#> ── Pareto identification report ────────────────────────────────────────────────
#> 6 alternatives, objectives y1 and y2, epsilon 0.05, 0.05, alpha 0.05, boundary
#> "betting".
#> 320 evaluations, cost 400; stopped: budget.
#> 
#> ── Pareto sets ──
#> 
#> • Estimated frontier: "A1", "A2", and "A3"
#> • Plausible: "A1", "A2", "A3", "A4", "A5", and "A6"
#> • Certified optimal: none
#> • Uncertain: A1, A2, A3, A4, A5, A6
#> • Certified dominated: none
#> 
#> ── Assumptions ──
#> 
#> • Every objective value lies in its declared bounds; conditionally on the past,
#> each evaluation of an alternative has the alternative's mean objective vector
#> (fresh draws, any dependence between objectives).
#> • The 12 confidence sequences share alpha = 0.05 by a union bound (0.00417
#> each); with probability at least 0.95 all true means lie in their running
#> intersections at every time, and every certificate below is then true.
#> • Dominance is epsilon-dominance with epsilon = (0.05, 0.05): j dominates k
#> when mu[j] <= mu[k] + epsilon in every objective.
#> • Certified optimal means not epsilon-dominated by any other plausible
#> alternative; epsilon-ties keep their earliest member in design order.
#> • The estimated frontier is a point estimate without a certificate; the
#> plausible set and the certified sets carry the guarantee.
#> paretoinfer 0.1.0, seqbench 0.1.0, R version 4.6.1 (2026-06-24)
```

## Continuing with a larger budget

A stopped identification accepts no further evaluations, so that a
report always describes a design that was fixed in advance. To continue,
a new design with a larger budget is run; the guarantee is per
identification, and the sampling of the first run cannot be reused
without a new design that includes it.

``` r

design2 <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                         bounds = c(0, 1), alpha = 0.05, epsilon = 0.05,
                         cost = c(1, 1, 1, 1, 5, 5), budget = 4000)
set.seed(2)
state2 <- pareto_run(design2, simulate, batch = 5, initial = 5)
glance(state2)[, c("n_evaluations", "total_cost", "n_dominated", "identified", "stopping_reason")]
#> # A tibble: 1 × 5
#>   n_evaluations total_cost n_dominated identified stopping_reason
#>           <int>      <dbl>       <int> <lgl>      <chr>          
#> 1           810       1190           3 TRUE       identified
plausible_pareto_set(state2)$table[, c("alternative", "status", "dominated_by", "n_evaluations", "cost")]
#> # A tibble: 6 × 5
#>   alternative status              dominated_by n_evaluations  cost
#>   <chr>       <chr>               <chr>                <int> <dbl>
#> 1 A1          certified_optimal   NA                     275   275
#> 2 A2          certified_optimal   NA                     170   170
#> 3 A3          certified_optimal   NA                     100   100
#> 4 A4          certified_dominated A2                     170   170
#> 5 A5          certified_dominated A2                      30   150
#> 6 A6          certified_dominated A1                      65   325
```

`A5` and `A6` were certified dominated with few evaluations: a clearly
dominated alternative needs little evidence, whatever it costs.

## Driving the loop by hand

When evaluations come from outside R, or when the stopping rule is not
the design’s budget but a judgement call, the loop is written by the
user.
[`choose_next_evaluation()`](https://castlaboratory.github.io/paretoinfer/reference/choose_next_evaluation.md)
proposes,
[`update_objectives()`](https://castlaboratory.github.io/paretoinfer/reference/update_objectives.md)
consumes, and
[`plausible_pareto_set()`](https://castlaboratory.github.io/paretoinfer/reference/plausible_pareto_set.md)
can be inspected at any time without invalidating anything.

``` r

design3 <- pareto_design(alternatives = rownames(means), objectives = c("y1", "y2"),
                         bounds = c(0, 1), alpha = 0.05, epsilon = 0.05, cost = c(1, 1, 1, 1, 5, 5))
set.seed(3)
state3 <- initialize_pareto(design3)
for (k in design3$alternatives) {
  ev <- simulate(k, 5); ev$alternative <- k
  state3 <- update_objectives(state3, ev)
}
repeat {
  sets <- plausible_pareto_set(state3)
  if (sets$identified || state3$total_cost >= 1500) break
  nxt <- choose_next_evaluation(state3)
  ev <- simulate(nxt$alternative, 5); ev$alternative <- nxt$alternative
  state3 <- update_objectives(state3, ev)
}
state3
#> 
#> ── Pareto identification ───────────────────────────────────────────────────────
#> 6 alternatives, 2 objectives, alpha 0.05, epsilon 0.05, 0.05.
#> 945 evaluations, cost 1325.
#> • Estimated frontier: "A1", "A2", and "A3"
#> • Plausible Pareto set: "A1", "A2", and "A3" (certified optimal: A1, A2, A3)
#> • Certified dominated: A4, A5, A6
#> ℹ Stopped: identified.
```

Stopping by hand at any time keeps every certificate valid: the
guarantee is simultaneous over time, so no choice of when to look or
when to stop can break it. What the user gives up by stopping early is
only the size of the plausible set.

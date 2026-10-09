# Design a sequential Pareto identification problem

Fixes, before any evaluation is seen, the finite set of alternatives,
the objectives (all minimised) with their known bounds, the tolerance
`epsilon` that defines practical dominance, the error level, the
confidence-sequence boundary and the budget.

## Usage

``` r
pareto_design(
  alternatives,
  objectives = 2L,
  bounds,
  alpha = 0.05,
  epsilon = 0,
  boundary = "betting",
  cost = 1,
  budget = Inf,
  max_evaluations = Inf,
  c = 0.5
)
```

## Arguments

- alternatives:

  Character vector of alternative names, or a single integer `K` (names
  `"A1"`, ..., `"AK"`).

- objectives:

  Character vector of objective names, or a single integer `m` (names
  `"y1"`, ..., `"ym"`). All objectives are minimised.

- bounds:

  Known bounds of each objective: a list of `m` vectors `c(a, b)`, or
  one vector `c(a, b)` used for all. Evaluations outside the bounds are
  refused, never clipped.

- alpha:

  Error level of the simultaneous guarantee.

- epsilon:

  Tolerance of practical dominance, per objective (length `m`) or a
  single value, in objective units. Zero means exact dominance; positive
  values operationalise ties and are required for the procedure to stop
  on near-ties.

- boundary:

  Confidence-sequence boundary from
  [`seqbench::seqbench_boundaries()`](https://rdrr.io/pkg/seqbench/man/seqbench_boundaries.html)
  (`"betting"` by default; `"naive_fixed"` is refused because it is not
  anytime-valid).

- cost:

  Evaluation cost per alternative (length `K` or a single value).

- budget:

  Total cost allowed; `Inf` for none.

- max_evaluations:

  Maximum number of evaluations; `Inf` for none.

- c:

  Truncation constant of the betting and empirical-Bernstein bets,
  passed to
  [`seqbench::boundary_init()`](https://rdrr.io/pkg/seqbench/man/boundary_kernels.html).

## Value

An object of class `pareto_design`.

## Dominance and guarantee

Alternative `j` *epsilon-dominates* `k` when, for every objective `i`,
`mu[j, i] <= mu[k, i] + epsilon[i]`: `j` is at least as good as `k` up
to the tolerance everywhere. The package maintains one confidence
sequence per alternative and objective, each at level `alpha / (K * m)`,
so that with probability at least `1 - alpha` every true mean lies in
its running intersection at every time. On that event every certificate
the package issues is true: an alternative reported as *certified
dominated* is truly epsilon-dominated by the alternative named, and one
reported as *certified optimal* is truly not epsilon-dominated by any
other plausible alternative. When two alternatives certifiably
epsilon-dominate each other (an epsilon-tie), only the later one in
design order is discarded, so every tie keeps one representative; the
discarded one is still truly epsilon-dominated by the alternative named.
The guarantee holds under optional stopping, adaptive choice of what to
evaluate next and arbitrary dependence between the objectives of one
evaluation.

## References

Waudby-Smith, I. and Ramdas, A. (2024). Estimating means of bounded
random variables by betting. *Journal of the Royal Statistical Society:
Series B*, 86(1), 1–27.
[doi:10.1093/jrsssb/qkad009](https://doi.org/10.1093/jrsssb/qkad009)

Auer, P., Chiang, C.-K., Ortner, R. and Drugan, M. (2016). Pareto front
identification from stochastic bandit feedback. *AISTATS 2016*, 939–947.

## Examples

``` r
pareto_design(alternatives = 6, objectives = c("error", "time"), bounds = c(0, 1),
              alpha = 0.05, epsilon = 0.02)
#> 
#> ── Pareto identification design ────────────────────────────────────────────────
#> 6 alternatives, 2 objectives (error and time), all minimised.
#> Bounds: error in [0, 1]; time in [0, 1].
#> Tolerance epsilon: 0.02, 0.02; alpha 0.05 shared over 12 confidence sequences
#> (0.00417 each); boundary "betting".
```

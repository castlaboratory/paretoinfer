# paretoinfer

<!-- badges: start -->
[![R-CMD-check](https://github.com/castlaboratory/paretoinfer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/castlaboratory/paretoinfer/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Sequential identification of approximately Pareto-optimal alternatives when
the objectives are noisy.**

Given a finite set of `K` alternatives evaluated on `m` objectives through
noisy (and possibly correlated) simulation, an alternative can look dominated
purely by sampling error. `paretoinfer` maintains simultaneous confidence
sequences for objective differences and reports, at any time, three disjoint
sets:

- the **estimated frontier**,
- the **plausible Pareto set** (alternatives that cannot yet be certified as
  `ε`-dominated), and
- the **certified-dominated** alternatives.

It also proposes the next alternative to evaluate, concentrating effort on the
ambiguous comparisons.

## Status

Pre-alpha. The API below is a design target, not yet implemented.

```r
design <- pareto_design(K = 12, m = 2, alpha = 0.05, epsilon = c(0.01, 0.01))
state  <- update_objectives(state, new_evaluations)
plausible_pareto_set(state)
choose_next_evaluation(state)
plot_uncertain_frontier(state)
```

## Installation

```r
# install.packages("pak")
pak::pak("castlaboratory/paretoinfer")
```

## Related software

`paretoinfer` does not do multi-objective *optimisation* over a continuous
space; for that see [BoTorch](https://botorch.org),
[GPareto](https://cran.r-project.org/package=GPareto),
[rmoo](https://cran.r-project.org/package=rmoo) or
[moocore](https://cran.r-project.org/package=moocore). It targets the
finite-alternative *identification* problem (Pareto set identification /
multi-objective ranking and selection) with anytime-valid guarantees, and
shares its sequential core with
[seqbench](https://github.com/castlaboratory/seqbench).

## License

GPL (>= 3). © CAST Lab, Universidade Federal de Pernambuco.

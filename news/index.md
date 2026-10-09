# Changelog

## paretoinfer 0.1.0

First CRAN release.

- [`pareto_design()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_design.md):
  finite alternatives, minimised objectives with known bounds,
  epsilon-dominance, alpha shared by a union bound over the K x m
  confidence sequences, boundary from `seqbench`, cost and budget.
- [`initialize_pareto()`](https://castlaboratory.github.io/paretoinfer/reference/initialize_pareto.md),
  [`update_objectives()`](https://castlaboratory.github.io/paretoinfer/reference/update_objectives.md):
  one anytime-valid confidence sequence per alternative and objective,
  running intersections, auditable history and stopping reason.
- [`plausible_pareto_set()`](https://castlaboratory.github.io/paretoinfer/reference/plausible_pareto_set.md):
  estimated frontier, plausible Pareto set, certified optimal and
  certified dominated alternatives (each with the alternative that
  dominates it); epsilon-ties keep one representative.
- [`choose_next_evaluation()`](https://castlaboratory.github.io/paretoinfer/reference/choose_next_evaluation.md)
  and
  [`pareto_run()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_run.md):
  adaptive choice of the next alternative by confidence-box width per
  unit cost, and a run loop with a user simulator.
- [`pareto_report()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_report.md);
  S3 class `paretoinfer_state` with `print`, `summary`, `tidy`,
  `glance`, `autoplot` /
  [`plot_uncertain_frontier()`](https://castlaboratory.github.io/paretoinfer/reference/autoplot.paretoinfer_state.md).
- Getting-started vignette.

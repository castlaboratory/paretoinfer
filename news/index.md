# Changelog

## paretoinfer 0.0.0.9000

- First implementation:
  [`pareto_design()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_design.md)
  (finite alternatives, minimised objectives with known bounds,
  epsilon-dominance, alpha split over the K x m confidence sequences,
  budget),
  [`initialize_pareto()`](https://castlaboratory.github.io/paretoinfer/reference/initialize_pareto.md),
  [`update_objectives()`](https://castlaboratory.github.io/paretoinfer/reference/update_objectives.md),
  [`plausible_pareto_set()`](https://castlaboratory.github.io/paretoinfer/reference/plausible_pareto_set.md)
  (estimated frontier, plausible set, certified optimal, certified
  dominated),
  [`choose_next_evaluation()`](https://castlaboratory.github.io/paretoinfer/reference/choose_next_evaluation.md),
  [`pareto_run()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_run.md),
  [`pareto_report()`](https://castlaboratory.github.io/paretoinfer/reference/pareto_report.md);
  S3 class `paretoinfer_state` with `print`, `summary`, `tidy`,
  `glance`, `autoplot` /
  [`plot_uncertain_frontier()`](https://castlaboratory.github.io/paretoinfer/reference/autoplot.paretoinfer_state.md).
- Confidence sequences come from `seqbench` (betting by default).
- Getting-started vignette; pkgdown site; hex logo.

# paretoinfer 0.0.0.9000

* First implementation: `pareto_design()` (finite alternatives, minimised
  objectives with known bounds, epsilon-dominance, alpha split over the K x m
  confidence sequences, budget), `initialize_pareto()`, `update_objectives()`,
  `plausible_pareto_set()` (estimated frontier, plausible set, certified optimal,
  certified dominated), `choose_next_evaluation()`, `pareto_run()`,
  `pareto_report()`; S3 class `paretoinfer_state` with `print`, `summary`, `tidy`,
  `glance`, `autoplot` / `plot_uncertain_frontier()`.
* Confidence sequences come from `seqbench` (betting by default).
* Getting-started vignette; pkgdown site; hex logo.

# paretoinfer 0.1.0

First CRAN release.

* `pareto_design()`: finite alternatives, minimised objectives with known
  bounds, epsilon-dominance, alpha shared by a union bound over the K x m
  confidence sequences, boundary from `seqbench`, cost and budget.
* `initialize_pareto()`, `update_objectives()`: one anytime-valid confidence
  sequence per alternative and objective, running intersections, auditable
  history and stopping reason.
* `plausible_pareto_set()`: estimated frontier, plausible Pareto set, certified
  optimal and certified dominated alternatives (each with the alternative that
  dominates it); epsilon-ties keep one representative.
* `choose_next_evaluation()` and `pareto_run()`: adaptive choice of the next
  alternative by confidence-box width per unit cost, and a run loop with a
  user simulator.
* `pareto_report()`; S3 class `paretoinfer_state` with `print`, `summary`,
  `tidy`, `glance`, `autoplot` / `plot_uncertain_frontier()`.
* Getting-started vignette.

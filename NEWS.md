# paretoinfer 0.2.0

First CRAN release. Version 0.1.0 was submitted and withdrawn before
publication because of the flaw corrected here.

* Discarding uses exact (strict) Pareto dominance certified by the boxes,
  never epsilon-dominance. Epsilon-dominance is not transitive and can be
  cyclic; in 0.1.0 a cycle of individually true certificates could discard
  every Pareto-optimal alternative and stop as "identified" with an empty
  plausible set. Now, on the event of the guarantee, the plausible set always
  contains the Pareto set, and the dominator named for a discarded
  alternative is itself plausible.
* `epsilon` now governs acceptance: a plausible alternative is accepted
  (status `"epsilon_optimal"`, or `"certified_optimal"` when exact optimality
  is certified) once no other plausible alternative can improve on it by more
  than `epsilon` in every objective. The identification stops when every
  plausible alternative is accepted.
* `plausible_pareto_set()` gains `accepted` and `representatives`, a greedy
  certified epsilon-cover of the plausible set; `glance()` gains
  `n_accepted` and `n_representatives`; the history records `n_accepted`.
* `choose_next_evaluation()` proposes both members of every open pair (a
  plausible alternative that may still improve on another by more than
  `epsilon` without a dominance certificate), so that alternatives whose box
  blocks another certificate keep being evaluated.

## Features carried over from the 0.1.0 submission


* `pareto_design()`: finite alternatives, minimised objectives with known
  bounds, a tolerance epsilon, alpha shared by a union bound over the K x m
  confidence sequences, boundary from `seqbench`, cost and budget.
* `initialize_pareto()`, `update_objectives()`: one anytime-valid confidence
  sequence per alternative and objective, running intersections, auditable
  history and stopping reason.
* `plausible_pareto_set()`: estimated frontier, plausible Pareto set, certified
  optimal and certified dominated alternatives.
* `choose_next_evaluation()` and `pareto_run()`: adaptive choice of the next
  alternative by confidence-box width per unit cost, and a run loop with a
  user simulator.
* `pareto_report()`; S3 class `paretoinfer_state` with `print`, `summary`,
  `tidy`, `glance`, `autoplot` / `plot_uncertain_frontier()`.
* Getting-started vignette.

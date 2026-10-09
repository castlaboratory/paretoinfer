# Choice of the next evaluation ------------------------------------------------

#' Choose the next alternative(s) to evaluate
#'
#' Scores every uncertain alternative by the width of its simultaneous
#' confidence box, normalised by the objective ranges and divided by its
#' evaluation cost, and returns the highest-scoring ones. Certified
#' alternatives (dominated or optimal) are never proposed: more evaluations
#' cannot change their certificate. The rule is a heuristic that targets the
#' ambiguous comparisons; it does not affect the validity of the certificates,
#' which holds for any adaptive choice.
#'
#' @param state A `paretoinfer_state`.
#' @param n Number of alternatives to propose.
#' @return A tibble with columns `alternative`, `score` (normalised width per
#'   unit cost), `width` (largest normalised width over objectives) and
#'   `n_evaluations`; zero rows when nothing is uncertain.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
#' state <- initialize_pareto(design)
#' choose_next_evaluation(state, n = 2)
choose_next_evaluation <- function(state, n = 1L) {
  assert_state(state)
  d <- state$design
  tab <- compute_sets(state)
  unc <- which(tab$status == "uncertain")
  if (length(unc) == 0L) {
    return(tibble::tibble(alternative = character(), score = numeric(), width = numeric(), n_evaluations = integer()))
  }
  range <- d$bounds[2, ] - d$bounds[1, ]
  width <- vapply(unc, function(k) {
    if (state$n[k] == 0L) return(1)
    max((state$upper[k, ] - state$lower[k, ]) / range)
  }, numeric(1))
  cost <- pmax(d$cost[unc], .Machine$double.eps)
  out <- tibble::tibble(alternative = d$alternatives[unc], score = width / cost, width = width,
                        n_evaluations = unname(state$n[unc]))
  out <- out[order(-out$score, out$n_evaluations), ]
  utils::head(out, n)
}

#' Run an identification to completion with a simulator
#'
#' Repeats "choose the next alternative, evaluate it, update" until every
#' alternative is certified or the budget is exhausted.
#'
#' @param design A [pareto_design()].
#' @param simulate A function `function(alternative, n)` returning a data frame
#'   (or matrix) with `n` rows and one column per objective, in the design's
#'   objective order or with the objective names.
#' @param batch Evaluations per call for the chosen alternative.
#' @param initial Evaluations of every alternative before the adaptive phase.
#' @param max_rounds Safety cap on the number of adaptive rounds.
#' @return The final `paretoinfer_state`.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05,
#'                         max_evaluations = 400)
#' means <- rbind(c(0.2, 0.7), c(0.5, 0.4), c(0.7, 0.2), c(0.6, 0.6))
#' sim <- function(alternative, n) {
#'   k <- match(alternative, design$alternatives)
#'   data.frame(y1 = runif(n, means[k, 1] - 0.1, means[k, 1] + 0.1),
#'              y2 = runif(n, means[k, 2] - 0.1, means[k, 2] + 0.1))
#' }
#' set.seed(1)
#' state <- pareto_run(design, sim)
#' state
pareto_run <- function(design, simulate, batch = 1L, initial = 2L, max_rounds = 1e5) {
  assert_design(design)
  if (!is.function(simulate)) cli::cli_abort("{.arg simulate} must be a function {.code function(alternative, n)}.")
  batch <- as.integer(batch); initial <- as.integer(initial)
  state <- initialize_pareto(design)
  evaluate <- function(state, alternative, n) {
    y <- simulate(alternative, n)
    if (is.matrix(y)) y <- as.data.frame(y)
    if (!is.data.frame(y) || nrow(y) != n) {
      cli::cli_abort("{.arg simulate} must return a data frame with {n} row{?s}.")
    }
    if (!all(design$objectives %in% names(y))) {
      if (ncol(y) < design$m) cli::cli_abort("{.arg simulate} must return {design$m} objective column{?s}.")
      names(y)[seq_len(design$m)] <- design$objectives
    }
    y <- y[, design$objectives, drop = FALSE]
    y$alternative <- alternative
    update_objectives(state, y)
  }
  if (initial > 0L) {
    for (k in design$alternatives) {
      if (!is.na(state$stopping_reason)) break
      state <- evaluate(state, k, initial)
    }
  }
  rounds <- 0L
  while (is.na(state$stopping_reason) && rounds < max_rounds) {
    nxt <- choose_next_evaluation(state, n = 1L)
    if (nrow(nxt) == 0L) break
    state <- evaluate(state, nxt$alternative[1], batch)
    rounds <- rounds + 1L
  }
  state
}

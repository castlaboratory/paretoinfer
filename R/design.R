# Design of a Pareto identification problem -----------------------------------

#' Design a sequential Pareto identification problem
#'
#' Fixes, before any evaluation is seen, the finite set of alternatives, the
#' objectives (all minimised) with their known bounds, the tolerance `epsilon`
#' that defines practical dominance, the error level, the confidence-sequence
#' boundary and the budget.
#'
#' @section Dominance and guarantee:
#' Alternative `j` *epsilon-dominates* `k` when, for every objective `i`,
#' `mu[j, i] <= mu[k, i] + epsilon[i]`: `j` is at least as good as `k` up to the
#' tolerance everywhere. The package maintains one confidence sequence per
#' alternative and objective, each at level `alpha / (K * m)`, so that with
#' probability at least `1 - alpha` every true mean lies in its running
#' intersection at every time. On that event every certificate the package
#' issues is true: an alternative reported as *certified dominated* is truly
#' epsilon-dominated by the alternative named, and one reported as *certified
#' optimal* is truly not epsilon-dominated by any other. The guarantee holds
#' under optional stopping, adaptive choice of what to evaluate next and
#' arbitrary dependence between the objectives of one evaluation.
#'
#' @param alternatives Character vector of alternative names, or a single integer
#'   `K` (names `"A1"`, ..., `"AK"`).
#' @param objectives Character vector of objective names, or a single integer `m`
#'   (names `"y1"`, ..., `"ym"`). All objectives are minimised.
#' @param bounds Known bounds of each objective: a list of `m` vectors `c(a, b)`,
#'   or one vector `c(a, b)` used for all. Evaluations outside the bounds are
#'   refused, never clipped.
#' @param alpha Error level of the simultaneous guarantee.
#' @param epsilon Tolerance of practical dominance, per objective (length `m`) or
#'   a single value, in objective units. Zero means exact dominance; positive
#'   values operationalise ties and are required for the procedure to stop on
#'   near-ties.
#' @param boundary Confidence-sequence boundary from [seqbench::seqbench_boundaries()]
#'   (`"betting"` by default; `"naive_fixed"` is refused because it is not
#'   anytime-valid).
#' @param cost Evaluation cost per alternative (length `K` or a single value).
#' @param budget Total cost allowed; `Inf` for none.
#' @param max_evaluations Maximum number of evaluations; `Inf` for none.
#' @param c Truncation constant of the betting and empirical-Bernstein bets,
#'   passed to [seqbench::boundary_init()].
#'
#' @return An object of class `pareto_design`.
#' @references Waudby-Smith, I. and Ramdas, A. (2024). Estimating means of
#'   bounded random variables by betting. *Journal of the Royal Statistical
#'   Society: Series B*, 86(1), 1--27. \doi{10.1093/jrsssb/qkad009}
#'
#'   Auer, P., Chiang, C.-K., Ortner, R. and Drugan, M. (2016). Pareto front
#'   identification from stochastic bandit feedback. *AISTATS 2016*, 939--947.
#' @export
#' @examples
#' pareto_design(alternatives = 6, objectives = c("error", "time"), bounds = c(0, 1),
#'               alpha = 0.05, epsilon = 0.02)
pareto_design <- function(alternatives, objectives = 2L, bounds, alpha = 0.05, epsilon = 0,
                          boundary = "betting", cost = 1, budget = Inf,
                          max_evaluations = Inf, c = 0.5) {
  if (is.numeric(alternatives) && length(alternatives) == 1L) {
    k <- as.integer(alternatives)
    if (k < 2L) cli::cli_abort("At least two alternatives are needed.")
    alternatives <- paste0("A", seq_len(k))
  }
  if (!is.character(alternatives) || anyDuplicated(alternatives) || length(alternatives) < 2L) {
    cli::cli_abort("{.arg alternatives} must be two or more distinct names, or a count.")
  }
  if (is.numeric(objectives) && length(objectives) == 1L) {
    objectives <- paste0("y", seq_len(as.integer(objectives)))
  }
  if (!is.character(objectives) || anyDuplicated(objectives) || length(objectives) < 1L) {
    cli::cli_abort("{.arg objectives} must be one or more distinct names, or a count.")
  }
  if (any(objectives %in% c("alternative", "cost"))) {
    cli::cli_abort("Objective names {.val alternative} and {.val cost} are reserved.")
  }
  K <- length(alternatives); m <- length(objectives)
  if (is.numeric(bounds) && length(bounds) == 2L) bounds <- rep(list(bounds), m)
  if (!is.list(bounds) || length(bounds) != m ||
      !all(vapply(bounds, function(b) is.numeric(b) && length(b) == 2L && all(is.finite(b)) && b[1] < b[2], logical(1)))) {
    cli::cli_abort("{.arg bounds} must be one vector {.code c(a, b)} or a list of {m} such vectors with {.code a < b}.")
  }
  bounds <- matrix(unlist(bounds), nrow = 2, dimnames = list(c("lower", "upper"), objectives))
  check_number(alpha, "alpha")
  if (alpha <= 0 || alpha >= 1) cli::cli_abort("{.arg alpha} must be in (0, 1).")
  if (length(epsilon) == 1L) epsilon <- rep(epsilon, m)
  if (!is.numeric(epsilon) || length(epsilon) != m || anyNA(epsilon) || any(epsilon < 0)) {
    cli::cli_abort("{.arg epsilon} must be a non-negative number or a vector of {m} non-negative numbers.")
  }
  epsilon <- stats::setNames(epsilon, objectives)
  boundary <- rlang::arg_match(boundary, setdiff(seqbench::seqbench_boundaries(), "naive_fixed"))
  if (length(cost) == 1L) cost <- rep(cost, K)
  if (!is.numeric(cost) || length(cost) != K || anyNA(cost) || any(cost < 0)) {
    cli::cli_abort("{.arg cost} must be a non-negative number or a vector of {K} non-negative numbers.")
  }
  cost <- stats::setNames(cost, alternatives)
  check_number(budget, "budget", lower = 0)
  check_number(max_evaluations, "max_evaluations", lower = 1)
  check_number(c, "c")
  if (c <= 0 || c >= 1) cli::cli_abort("{.arg c} must be in (0, 1).")
  structure(list(
    alternatives = alternatives, objectives = objectives, K = K, m = m,
    bounds = bounds, alpha = alpha, alpha_each = alpha / (K * m), epsilon = epsilon,
    boundary = boundary, c = c, cost = cost, budget = budget,
    max_evaluations = max_evaluations, created = Sys.time(),
    version = as.character(utils::packageVersion("paretoinfer"))
  ), class = "pareto_design")
}

#' @export
print.pareto_design <- function(x, ...) {
  cli::cli_h1("Pareto identification design")
  cli::cli_text("{x$K} alternative{?s}, {x$m} objective{?s} ({.field {x$objectives}}), all minimised.")
  cli::cli_text("Bounds: {paste(sprintf('%s in [%s, %s]', x$objectives, x$bounds[1, ], x$bounds[2, ]), collapse = '; ')}.")
  cli::cli_text("Tolerance epsilon: {paste(signif(x$epsilon, 3), collapse = ', ')}; alpha {x$alpha} shared over {x$K * x$m} confidence sequences ({signif(x$alpha_each, 3)} each); boundary {.val {x$boundary}}.")
  if (is.finite(x$budget) || is.finite(x$max_evaluations)) {
    cli::cli_text("Budget: {if (is.finite(x$budget)) paste('cost', x$budget) else 'no cost limit'}; {if (is.finite(x$max_evaluations)) paste(x$max_evaluations, 'evaluations at most') else 'no evaluation limit'}.")
  }
  invisible(x)
}

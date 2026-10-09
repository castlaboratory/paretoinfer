# Report, broom methods and plot ------------------------------------------------

#' The full report of an identification
#'
#' Everything needed to audit the result: the design (alternatives, objectives,
#' bounds, tolerance, level and its split, boundary, budget), the evaluations
#' and cost per alternative, the three sets with their certificates, the
#' simultaneous confidence boxes, the stopping reason and the software
#' versions.
#'
#' @param state A `paretoinfer_state`.
#' @return A list of class `pareto_report`.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
#' state <- update_objectives(initialize_pareto(design),
#'   data.frame(alternative = c("A1", "A2"), y1 = c(0.2, 0.8), y2 = c(0.3, 0.9)))
#' pareto_report(state)
pareto_report <- function(state) {
  assert_state(state)
  d <- state$design
  sets <- plausible_pareto_set(state)
  structure(list(
    alternatives = d$alternatives, objectives = d$objectives, bounds = d$bounds,
    alpha = d$alpha, alpha_each = d$alpha_each, epsilon = d$epsilon, boundary = d$boundary,
    budget = d$budget, max_evaluations = d$max_evaluations,
    n_evaluations = state$n_evaluations, total_cost = state$total_cost,
    sets = sets, boxes = tidy(state), stopping_reason = state$stopping_reason,
    assumptions = c(
      sprintf("Every objective value lies in its declared bounds; conditionally on the past, each evaluation of an alternative has the alternative's mean objective vector (fresh draws, any dependence between objectives)."),
      sprintf("The %d confidence sequences share alpha = %s by a union bound (%s each); with probability at least %s all true means lie in their running intersections at every time, and every certificate below is then true.",
              d$K * d$m, format(d$alpha), format(signif(d$alpha_each, 3)), format(1 - d$alpha)),
      sprintf("Dominance is epsilon-dominance with epsilon = (%s): j dominates k when mu[j] <= mu[k] + epsilon in every objective.",
              paste(signif(d$epsilon, 3), collapse = ", ")),
      "Certified optimal means not epsilon-dominated by any other plausible alternative; epsilon-ties keep their earliest member in design order.",
      "The estimated frontier is a point estimate without a certificate; the plausible set and the certified sets carry the guarantee."),
    versions = state$versions, created = d$created
  ), class = "pareto_report")
}

#' @export
print.pareto_report <- function(x, ...) {
  cli::cli_h1("Pareto identification report")
  cli::cli_text("{length(x$alternatives)} alternatives, objectives {.field {x$objectives}}, epsilon {paste(signif(x$epsilon, 3), collapse = ', ')}, alpha {x$alpha}, boundary {.val {x$boundary}}.")
  cli::cli_text("{x$n_evaluations} evaluations, cost {signif(x$total_cost, 4)}; {if (is.na(x$stopping_reason)) 'running' else paste('stopped:', x$stopping_reason)}.")
  print(x$sets)
  cli::cli_h2("Assumptions")
  cli::cli_ul(x$assumptions)
  cli::cli_text("{.emph paretoinfer {x$versions$paretoinfer}, seqbench {x$versions$seqbench}, {x$versions$R}}")
  invisible(x)
}

#' Tidy the confidence boxes
#'
#' One row per alternative and objective with the point estimate, the running
#' intersection of the confidence sequence, the number of evaluations and the
#' status of the alternative.
#'
#' @param x A `paretoinfer_state`.
#' @param ... Unused.
#' @return A tibble with columns `alternative`, `objective`, `estimate`,
#'   `lower`, `upper`, `n_evaluations`, `status`.
#' @exportS3Method generics::tidy paretoinfer_state
#' @examples
#' design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
#' state <- update_objectives(initialize_pareto(design),
#'   data.frame(alternative = c("A1", "A2"), y1 = c(0.2, 0.8), y2 = c(0.3, 0.9)))
#' tidy(state)
tidy.paretoinfer_state <- function(x, ...) {
  assert_state(x)
  d <- x$design; tab <- compute_sets(x)
  L <- x$lower; U <- x$upper
  tibble::tibble(
    alternative = rep(d$alternatives, times = d$m),
    objective = rep(d$objectives, each = d$K),
    estimate = as.vector(x$estimate), lower = as.vector(L), upper = as.vector(U),
    n_evaluations = rep(unname(x$n), times = d$m),
    status = rep(tab$status, times = d$m))
}

#' Glance at an identification
#'
#' @param x A `paretoinfer_state`.
#' @param ... Unused.
#' @return A one-row tibble: `K`, `m`, `alpha`, `n_evaluations`, `total_cost`,
#'   `n_frontier`, `n_plausible`, `n_certified_optimal`, `n_uncertain`,
#'   `n_dominated`, `identified`, `stopping_reason`.
#' @exportS3Method generics::glance paretoinfer_state
#' @examples
#' design <- pareto_design(alternatives = 2, objectives = 2, bounds = c(0, 1), epsilon = 0.1)
#' glance(initialize_pareto(design))
glance.paretoinfer_state <- function(x, ...) {
  assert_state(x)
  s <- plausible_pareto_set(x)
  tibble::tibble(K = x$design$K, m = x$design$m, alpha = x$design$alpha,
                 n_evaluations = x$n_evaluations, total_cost = x$total_cost,
                 n_frontier = length(s$frontier), n_plausible = length(s$plausible),
                 n_certified_optimal = length(s$certified_optimal), n_uncertain = length(s$uncertain),
                 n_dominated = length(s$dominated), identified = s$identified,
                 stopping_reason = x$stopping_reason)
}

#' Plot the uncertain frontier
#'
#' Draws, for two objectives, the simultaneous confidence box of every
#' alternative (the product of its two running intersections), its point
#' estimate, and the estimated frontier. The boxes correspond to the actual
#' guarantee: with probability at least `1 - alpha` every true mean vector lies
#' in its box at every time. Alternatives are coloured by status.
#'
#' @param object A `paretoinfer_state`.
#' @param objectives The two objectives to draw (names or positions); default
#'   the first two.
#' @param ... Unused.
#' @return A ggplot.
#' @exportS3Method ggplot2::autoplot paretoinfer_state
#' @examples
#' design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
#' set.seed(1)
#' ev <- data.frame(alternative = rep(c("A1", "A2", "A3"), each = 40),
#'                  y1 = runif(120, c(0.1, 0.5, 0.6), c(0.3, 0.7, 0.8)),
#'                  y2 = runif(120, c(0.6, 0.1, 0.6), c(0.8, 0.3, 0.8)))
#' autoplot(update_objectives(initialize_pareto(design), ev))
autoplot.paretoinfer_state <- function(object, objectives = c(1L, 2L), ...) {
  plot_uncertain_frontier(object, objectives = objectives)
}

#' @rdname autoplot.paretoinfer_state
#' @param state A `paretoinfer_state`.
#' @export
plot_uncertain_frontier <- function(state, objectives = c(1L, 2L)) {
  assert_state(state)
  d <- state$design
  if (d$m < 2L) cli::cli_abort("The plot needs at least two objectives.")
  if (is.numeric(objectives)) objectives <- d$objectives[objectives]
  if (length(objectives) != 2L || !all(objectives %in% d$objectives)) {
    cli::cli_abort("{.arg objectives} must name two objectives of the design.")
  }
  tb <- tidy(state)
  wide <- function(col) {
    a <- tb[tb$objective == objectives[1], ]; b <- tb[tb$objective == objectives[2], ]
    tibble::tibble(alternative = a$alternative, status = a$status, n = a$n_evaluations,
                   x = a[[col]], y = b[[col]])
  }
  est <- wide("estimate"); lo <- wide("lower"); up <- wide("upper")
  boxes <- tibble::tibble(alternative = est$alternative, status = est$status,
                          xmin = lo$x, xmax = up$x, ymin = lo$y, ymax = up$y)
  sets <- plausible_pareto_set(state)
  fr <- est[est$alternative %in% sets$frontier & !is.na(est$x), ]
  fr <- fr[order(fr$x), ]
  status_levels <- c("certified_optimal", "uncertain", "certified_dominated")
  boxes$status <- factor(boxes$status, status_levels); est$status <- factor(est$status, status_levels)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = boxes, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                                                  ymax = .data$ymax, fill = .data$status, colour = .data$status),
                       alpha = 0.15, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(certified_optimal = "#1B9E77", uncertain = "#7570B3", certified_dominated = "#B0B0B0"), drop = FALSE, name = NULL) +
    ggplot2::scale_colour_manual(values = c(certified_optimal = "#1B9E77", uncertain = "#7570B3", certified_dominated = "#7A7A7A"), drop = FALSE, name = NULL) +
    ggplot2::labs(x = objectives[1], y = objectives[2]) +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
  if (nrow(fr) > 1L) p <- p + ggplot2::geom_step(data = fr, ggplot2::aes(x = .data$x, y = .data$y), colour = "#D95F02", direction = "vh", linewidth = 0.5)
  est_known <- est[!is.na(est$x), ]
  p + ggplot2::geom_point(data = est_known, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$status), size = 2) +
    ggplot2::geom_text(data = est_known, ggplot2::aes(x = .data$x, y = .data$y, label = .data$alternative), vjust = -0.8, size = 3)
}

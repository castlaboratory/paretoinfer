# State: K x m confidence sequences and their running intersections --------------

#' Initialise a Pareto identification
#'
#' @param design A [pareto_design()].
#' @return An object of class `paretoinfer_state`.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 4, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
#' state <- initialize_pareto(design)
#' state
initialize_pareto <- function(design) {
  assert_design(design)
  K <- design$K; m <- design$m
  kernels <- vector("list", K * m)
  for (idx in seq_len(K * m)) {
    kernels[[idx]] <- seqbench::boundary_init(design$boundary, alpha = design$alpha_each, c = design$c)
  }
  dim(kernels) <- c(K, m)
  dimnames(kernels) <- list(design$alternatives, design$objectives)
  empty <- matrix(0, K, m, dimnames = list(design$alternatives, design$objectives))
  lower <- empty + rep(design$bounds[1, ], each = K)
  upper <- empty + rep(design$bounds[2, ], each = K)
  structure(list(
    design = design,
    kernels = kernels,
    lower = lower, upper = upper, estimate = empty + NA_real_, cs_empty = empty > 1,
    n = stats::setNames(integer(K), design$alternatives),
    cost = stats::setNames(numeric(K), design$alternatives),
    n_evaluations = 0L, total_cost = 0,
    stopping_reason = NA_character_,
    history = tibble::tibble(n_evaluations = integer(), total_cost = numeric(),
                             n_plausible = integer(), n_dominated = integer(),
                             n_certified_optimal = integer()),
    versions = list(paretoinfer = as.character(utils::packageVersion("paretoinfer")),
                    seqbench = as.character(utils::packageVersion("seqbench")),
                    R = R.version.string)
  ), class = "paretoinfer_state")
}

#' Update the identification with new evaluations
#'
#' Feeds one or more evaluations, each a vector of objective values for one
#' alternative, to the confidence sequences. The running intersections are
#' updated and the three sets are recomputed.
#'
#' @param state A `paretoinfer_state`.
#' @param evaluations Data frame with a column `alternative` (name as in the
#'   design) and one numeric column per objective; an optional `cost` column
#'   overrides the design cost for those rows.
#' @return The updated state.
#' @export
#' @examples
#' design <- pareto_design(alternatives = c("a", "b"), objectives = c("y1", "y2"),
#'                         bounds = c(0, 1), epsilon = 0.05)
#' state <- initialize_pareto(design)
#' state <- update_objectives(state, data.frame(alternative = c("a", "b", "a"),
#'                                              y1 = c(0.2, 0.5, 0.25), y2 = c(0.6, 0.3, 0.55)))
#' state
update_objectives <- function(state, evaluations) {
  assert_state(state)
  if (!is.na(state$stopping_reason)) {
    cli::cli_abort(c("This identification stopped ({state$stopping_reason}) and accepts no further evaluations.",
                     "i" = "Start a new one with {.fn initialize_pareto} or raise the budget in a new design."))
  }
  d <- state$design
  ev <- validate_evaluations(evaluations, d)
  for (r in seq_len(nrow(ev))) {
    k <- ev$alternative[r]
    for (i in seq_len(d$m)) {
      obj <- d$objectives[i]; b <- d$bounds[, i]
      x <- (ev[[obj]][r] - b[1]) / (b[2] - b[1])
      st <- seqbench::boundary_update(state$kernels[[k, obj]], x)
      state$kernels[[k, obj]] <- st
      ci <- seqbench::boundary_interval(st)
      state$estimate[k, obj] <- b[1] + (b[2] - b[1]) * ci[["estimate"]]
      if (is.na(ci[["lower"]]) || is.na(ci[["upper"]])) {
        state$cs_empty[k, obj] <- TRUE
      } else {
        lo <- b[1] + (b[2] - b[1]) * ci[["lower"]]; up <- b[1] + (b[2] - b[1]) * ci[["upper"]]
        state$lower[k, obj] <- max(state$lower[k, obj], lo)
        state$upper[k, obj] <- min(state$upper[k, obj], up)
        if (state$upper[k, obj] < state$lower[k, obj]) state$cs_empty[k, obj] <- TRUE
      }
    }
    state$n[k] <- state$n[k] + 1L
    state$cost[k] <- state$cost[k] + ev$cost[r]
    state$n_evaluations <- state$n_evaluations + 1L
    state$total_cost <- state$total_cost + ev$cost[r]
  }
  sets <- compute_sets(state)
  state$history <- rbind(state$history, tibble::tibble(
    n_evaluations = state$n_evaluations, total_cost = state$total_cost,
    n_plausible = sum(sets$status != "certified_dominated"),
    n_dominated = sum(sets$status == "certified_dominated"),
    n_certified_optimal = sum(sets$status == "certified_optimal")))
  state$stopping_reason <- stopping_reason(state, sets)
  state
}

#' @export
print.paretoinfer_state <- function(x, ...) {
  d <- x$design
  cli::cli_h1("Pareto identification")
  cli::cli_text("{d$K} alternative{?s}, {d$m} objective{?s}, alpha {d$alpha}, epsilon {paste(signif(d$epsilon, 3), collapse = ', ')}.")
  if (x$n_evaluations == 0L) {
    cli::cli_text("No evaluations yet.")
    return(invisible(x))
  }
  s <- plausible_pareto_set(x)
  cli::cli_text("{x$n_evaluations} evaluation{?s}, cost {signif(x$total_cost, 4)}.")
  cli::cli_ul(c(
    "Estimated frontier: {.val {s$frontier}}",
    "Plausible Pareto set: {.val {s$plausible}}{if (length(s$certified_optimal)) paste0(' (certified optimal: ', paste(s$certified_optimal, collapse = ', '), ')') else ''}",
    "Certified dominated: {if (length(s$dominated)) paste(s$dominated, collapse = ', ') else 'none'}"))
  if (!is.na(x$stopping_reason)) {
    cli::cli_alert_info("Stopped: {x$stopping_reason}.")
  } else {
    cli::cli_text("Running; {length(s$uncertain)} alternative{?s} still uncertain.")
  }
  invisible(x)
}

#' @export
summary.paretoinfer_state <- function(object, ...) pareto_report(object)

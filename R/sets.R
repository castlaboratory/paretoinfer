# The three sets and their certificates -----------------------------------------
#
# With all K x m running intersections [L, U] valid simultaneously:
#   j certifiably epsilon-dominates k  <=>  U[j, i] <= L[k, i] + eps[i] for all i
#   k is certified optimal            <=>  k is not discarded and, for every other
#                                           survivor j, there is an i with
#                                           L[j, i] > U[k, i] + eps[i]
#                                           (j cannot epsilon-dominate k)
# Mutual certificates (epsilon-ties) keep one representative, the earliest in
# design order. The estimated frontier is the set of survivors whose point
# estimates are not dominated (exactly) by the estimate of any other survivor.

compute_sets <- function(state) {
  d <- state$design; K <- d$K
  L <- state$lower; U <- state$upper; E <- state$estimate
  eps <- d$epsilon
  # dom[j, k]: j certifiably epsilon-dominates k
  dom <- matrix(FALSE, K, K)
  for (j in seq_len(K)) for (k in seq_len(K)) {
    if (j != k) dom[j, k] <- all(U[j, ] <= L[k, ] + eps)
  }
  # Discard k when some j certifiably dominates it, unless the two certifiably
  # dominate each other (an epsilon-tie): then only the later one in design order
  # is discarded, so that every tie keeps one representative. A one-directional
  # dominator takes precedence over a tied one.
  discarded <- logical(K); dominated_by <- rep(NA_character_, K)
  for (k in seq_len(K)) {
    js <- which(dom[, k])
    js <- js[order(dom[k, js], js)]          # one-directional first, then by design order
    for (j in js) {
      if (!dom[k, j] || j < k) { discarded[k] <- TRUE; dominated_by[k] <- d$alternatives[j]; break }
    }
  }
  survivors <- which(!discarded)
  status <- ifelse(discarded, "certified_dominated", "uncertain")
  for (k in survivors) {
    others <- setdiff(survivors, k)
    ok <- vapply(others, function(j) any(L[j, ] > U[k, ] + eps), logical(1))
    if (all(ok)) status[k] <- "certified_optimal"
  }
  on_frontier <- rep(FALSE, K)
  for (k in survivors) {
    if (any(is.na(E[k, ]))) next
    dominated <- FALSE
    for (j in survivors) {
      if (j == k || any(is.na(E[j, ]))) next
      if (all(E[j, ] <= E[k, ]) && any(E[j, ] < E[k, ])) { dominated <- TRUE; break }
    }
    on_frontier[k] <- !dominated
  }
  tibble::tibble(alternative = d$alternatives, status = status,
                 on_estimated_frontier = on_frontier, dominated_by = dominated_by,
                 n_evaluations = unname(state$n), cost = unname(state$cost),
                 cs_empty = unname(apply(state$cs_empty, 1, any)))
}

stopping_reason <- function(state, sets) {
  d <- state$design
  if (all(sets$status != "uncertain")) return("identified")
  if (state$total_cost >= d$budget) return("budget")
  if (state$n_evaluations >= d$max_evaluations) return("max_evaluations")
  NA_character_
}

#' The three sets of a Pareto identification
#'
#' @param state A `paretoinfer_state`.
#' @return A list of class `pareto_sets` with `frontier` (alternatives whose
#'   point estimates are not dominated by the estimate of any other plausible
#'   alternative), `plausible` (not certified dominated), `certified_optimal`
#'   (plausible and certified not epsilon-dominated by any other plausible
#'   alternative),
#'   `uncertain` (plausible but not certified), `dominated` (certified
#'   epsilon-dominated), `identified` (no uncertain alternative left) and
#'   `table`, a tibble with one row per alternative: `status`,
#'   `on_estimated_frontier`, `dominated_by`, `n_evaluations`, `cost`,
#'   `cs_empty`.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
#' state <- initialize_pareto(design)
#' set.seed(1)
#' ev <- data.frame(alternative = rep(c("A1", "A2", "A3"), each = 40),
#'                  y1 = runif(120, c(0.1, 0.5, 0.6), c(0.3, 0.7, 0.8)),
#'                  y2 = runif(120, c(0.6, 0.1, 0.6), c(0.8, 0.3, 0.8)))
#' plausible_pareto_set(update_objectives(state, ev))
plausible_pareto_set <- function(state) {
  assert_state(state)
  tab <- compute_sets(state)
  structure(list(
    frontier = tab$alternative[tab$on_estimated_frontier],
    plausible = tab$alternative[tab$status != "certified_dominated"],
    certified_optimal = tab$alternative[tab$status == "certified_optimal"],
    uncertain = tab$alternative[tab$status == "uncertain"],
    dominated = tab$alternative[tab$status == "certified_dominated"],
    identified = all(tab$status != "uncertain"),
    table = tab
  ), class = "pareto_sets")
}

#' @export
print.pareto_sets <- function(x, ...) {
  cli::cli_h2("Pareto sets")
  cli::cli_ul(c(
    "Estimated frontier: {.val {x$frontier}}",
    "Plausible: {.val {x$plausible}}",
    "Certified optimal: {if (length(x$certified_optimal)) paste(x$certified_optimal, collapse = ', ') else 'none'}",
    "Uncertain: {if (length(x$uncertain)) paste(x$uncertain, collapse = ', ') else 'none'}",
    "Certified dominated: {if (length(x$dominated)) paste(x$dominated, collapse = ', ') else 'none'}"))
  if (x$identified) cli::cli_alert_success("Identified: every alternative is certified.")
  invisible(x)
}

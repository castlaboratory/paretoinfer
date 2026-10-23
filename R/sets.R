# The sets and their certificates ----------------------------------------------
#
# With all K x m running intersections [L, U] valid simultaneously (the event G):
#   j certifiably dominates k     <=>  U[j, i] <= L[k, i] for all i and
#                                       U[j, i] <  L[k, i] for some i
#                                       (then mu[j] <= mu[k], mu[j] != mu[k])
#   j cannot dominate k at all    <=>  L[j, i] > U[k, i] for some i
#   j cannot dominate k by > eps  <=>  L[j, i] > U[k, i] - eps[i] for some i
# Discarding uses exact (strict) dominance only. Box dominance is transitive and
# acyclic (U[j'] <= L[j] <= U[j] <= L[k]), so every discarded alternative has a
# surviving dominator and, on G, no Pareto-optimal alternative is ever discarded.
# Discarding by epsilon-dominance would not have this property: epsilon-dominance
# is not transitive and can be cyclic, and a chain of epsilon-certificates can
# discard the whole frontier while every single certificate is true.
# A survivor is accepted when no other survivor can dominate it by more than eps
# (it is then epsilon-Pareto-optimal on G); it is certified optimal when no other
# survivor can dominate it at all. Checking survivors suffices: a discarded j has
# a survivor s with mu[s] <= mu[j]. The representatives are a greedy subset of
# the survivors that epsilon-covers all of them with certificates
# (U[r] <= L[s] + eps). The estimated frontier is the set of survivors whose
# point estimates are not dominated (exactly) by the estimate of any other
# survivor; it carries no certificate.

box_dominates <- function(U, L, j, k) all(U[j, ] <= L[k, ]) && any(U[j, ] < L[k, ])

compute_sets <- function(state) {
  d <- state$design; K <- d$K
  L <- state$lower; U <- state$upper; E <- state$estimate
  eps <- d$epsilon
  # dom[j, k]: j certifiably dominates k
  dom <- matrix(FALSE, K, K)
  for (j in seq_len(K)) for (k in seq_len(K)) {
    if (j != k) dom[j, k] <- box_dominates(U, L, j, k)
  }
  discarded <- colSums(dom) > 0
  survivors <- which(!discarded)
  dominated_by <- rep(NA_character_, K)
  for (k in which(discarded)) {
    js <- intersect(which(dom[, k]), survivors)
    # An empty box (an event of probability at most alpha) can break transitivity.
    if (length(js) == 0L) js <- which(dom[, k])
    dominated_by[k] <- d$alternatives[js[1]]
  }
  status <- ifelse(discarded, "certified_dominated", "uncertain")
  n_blocking <- integer(K)
  for (k in survivors) {
    others <- setdiff(survivors, k)
    beyond_eps <- vapply(others, function(j) any(L[j, ] > U[k, ] - eps), logical(1))
    at_all <- vapply(others, function(j) any(L[j, ] > U[k, ]), logical(1))
    if (all(at_all)) status[k] <- "certified_optimal"
    else if (all(beyond_eps)) status[k] <- "epsilon_optimal"
    n_blocking[k] <- sum(!beyond_eps)
  }
  # Greedy certified epsilon-cover of the survivors: repeatedly take the survivor
  # that covers most of the uncovered ones (ties by design order).
  covers <- matrix(FALSE, K, K)
  for (r in survivors) for (s in survivors) covers[r, s] <- r == s || all(U[r, ] <= L[s, ] + eps)
  representative <- rep(FALSE, K); uncovered <- survivors
  while (length(uncovered)) {
    gain <- vapply(survivors, function(r) sum(covers[r, uncovered]), integer(1))
    r <- survivors[which.max(gain)]
    representative[r] <- TRUE
    uncovered <- uncovered[!covers[r, uncovered]]
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
                 on_estimated_frontier = on_frontier, representative = representative,
                 dominated_by = dominated_by,
                 n_evaluations = unname(state$n), cost = unname(state$cost),
                 cs_empty = unname(apply(state$cs_empty, 1, any)))
}

# Alternatives whose certificate is still open, and the survivors that keep it
# open: k is uncertain when some survivor j may still dominate it by more than
# epsilon without being certified to dominate it. Both members of such a pair
# are candidates for the next evaluation.
open_pairs <- function(state) {
  d <- state$design; K <- d$K
  L <- state$lower; U <- state$upper; eps <- d$epsilon
  tab <- compute_sets(state)
  survivors <- which(tab$status != "certified_dominated")
  unc <- which(tab$status == "uncertain")
  pairs <- list()
  for (k in unc) for (j in setdiff(survivors, k)) {
    if (!any(L[j, ] > U[k, ] - eps)) pairs[[length(pairs) + 1L]] <- c(j, k)
  }
  pairs
}

stopping_reason <- function(state, sets) {
  d <- state$design
  if (all(sets$status != "uncertain")) return("identified")
  if (state$total_cost >= d$budget) return("budget")
  if (state$n_evaluations >= d$max_evaluations) return("max_evaluations")
  NA_character_
}

#' The sets of a Pareto identification
#'
#' @section Certificates:
#' On the event that every true mean lies in its running confidence interval
#' (probability at least `1 - alpha`): an alternative in `dominated` is truly
#' Pareto-dominated by the alternative named in `dominated_by`; every
#' Pareto-optimal alternative is in `plausible`; every alternative in
#' `accepted` is epsilon-Pareto-optimal (no alternative improves on it by more
#' than `epsilon` in every objective); every alternative in `certified_optimal`
#' is Pareto-optimal; and every alternative in `plausible`, hence every
#' Pareto-optimal alternative, is epsilon-dominated by one of the
#' `representatives`. Discarding uses exact dominance only; `epsilon` decides
#' when to stop, not what to discard.
#'
#' @param state A `paretoinfer_state`.
#' @return A list of class `pareto_sets` with `frontier` (alternatives whose
#'   point estimates are not dominated by the estimate of any other plausible
#'   alternative), `plausible` (not certified dominated), `accepted`
#'   (plausible and certified epsilon-Pareto-optimal), `certified_optimal`
#'   (the accepted alternatives certified exactly Pareto-optimal),
#'   `uncertain` (plausible but not accepted), `dominated` (certified
#'   Pareto-dominated), `representatives` (a certified epsilon-cover of the
#'   plausible set), `identified` (no uncertain alternative left) and `table`,
#'   a tibble with one row per alternative: `status` (`"certified_optimal"`,
#'   `"epsilon_optimal"`, `"uncertain"` or `"certified_dominated"`),
#'   `on_estimated_frontier`, `representative`, `dominated_by`,
#'   `n_evaluations`, `cost`, `cs_empty`.
#' @export
#' @examples
#' design <- pareto_design(alternatives = 3, objectives = 2, bounds = c(0, 1), epsilon = 0.05)
#' set.seed(1)
#' ev <- tibble::tibble(alternative = rep(c("A1", "A2", "A3"), each = 40),
#'                      y1 = runif(120, c(0.1, 0.5, 0.6), c(0.3, 0.7, 0.8)),
#'                      y2 = runif(120, c(0.6, 0.1, 0.6), c(0.8, 0.3, 0.8)))
#' initialize_pareto(design) |>
#'   update_objectives(ev) |>
#'   plausible_pareto_set()
plausible_pareto_set <- function(state) {
  assert_state(state)
  tab <- compute_sets(state)
  structure(list(
    frontier = tab$alternative[tab$on_estimated_frontier],
    plausible = tab$alternative[tab$status != "certified_dominated"],
    accepted = tab$alternative[tab$status %in% c("certified_optimal", "epsilon_optimal")],
    certified_optimal = tab$alternative[tab$status == "certified_optimal"],
    uncertain = tab$alternative[tab$status == "uncertain"],
    dominated = tab$alternative[tab$status == "certified_dominated"],
    representatives = tab$alternative[tab$representative],
    identified = all(tab$status != "uncertain"),
    table = tab
  ), class = "pareto_sets")
}

#' @export
print.pareto_sets <- function(x, ...) {
  none <- function(v) if (length(v)) paste(v, collapse = ", ") else "none"
  cli::cli_h2("Pareto sets")
  cli::cli_ul(c(
    "Estimated frontier: {none(x$frontier)}",
    "Plausible: {none(x$plausible)}",
    "Accepted (epsilon-Pareto-optimal): {none(x$accepted)}",
    "Certified optimal: {none(x$certified_optimal)}",
    "Uncertain: {none(x$uncertain)}",
    "Certified dominated: {none(x$dominated)}",
    "Representatives (epsilon-cover): {none(x$representatives)}"))
  if (x$identified) cli::cli_alert_success("Identified: every plausible alternative is accepted.")
  invisible(x)
}

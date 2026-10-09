# Input validation -------------------------------------------------------------
#
# The package never converts invalid input into a valid-looking result: unknown
# alternatives, missing objective values, values outside the declared bounds and
# negative costs are errors, not silent fixes.

check_number <- function(x, arg, lower = -Inf, upper = Inf, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single number.", call = call)
  }
  if (x < lower || x > upper) {
    cli::cli_abort("{.arg {arg}} must be between {lower} and {upper}, not {x}.", call = call)
  }
  invisible(x)
}

assert_design <- function(design, call = rlang::caller_env()) {
  if (!inherits(design, "pareto_design")) {
    cli::cli_abort("{.arg design} must be created by {.fn pareto_design}.", call = call)
  }
  invisible(design)
}

assert_state <- function(state, call = rlang::caller_env()) {
  if (!inherits(state, "paretoinfer_state")) {
    cli::cli_abort("{.arg state} must be a {.cls paretoinfer_state} from {.fn initialize_pareto}.", call = call)
  }
  invisible(state)
}

validate_evaluations <- function(evaluations, design, call = rlang::caller_env()) {
  if (!is.data.frame(evaluations)) {
    cli::cli_abort("{.arg evaluations} must be a data frame with a column {.field alternative} and one column per objective ({.field {design$objectives}}).",
                   call = call)
  }
  if (!"alternative" %in% names(evaluations)) {
    cli::cli_abort("{.arg evaluations} needs a column {.field alternative}.", call = call)
  }
  missing <- setdiff(design$objectives, names(evaluations))
  if (length(missing)) {
    cli::cli_abort("{.arg evaluations} is missing objective column{?s} {.field {missing}}.", call = call)
  }
  if (nrow(evaluations) == 0L) {
    cli::cli_abort("{.arg evaluations} has no rows.", call = call)
  }
  ev <- tibble::as_tibble(evaluations)
  ev$alternative <- as.character(ev$alternative)
  unknown <- setdiff(unique(ev$alternative), design$alternatives)
  if (length(unknown)) {
    cli::cli_abort(c("Unknown alternative{?s} {.val {head(unknown, 5)}}.",
                     "i" = "The design declares {.val {design$alternatives}}."), call = call)
  }
  if (!"cost" %in% names(ev)) ev$cost <- design$cost[ev$alternative]
  for (col in c(design$objectives, "cost")) {
    if (!is.numeric(ev[[col]])) {
      cli::cli_abort("Column {.field {col}} must be numeric.", call = call)
    }
    if (anyNA(ev[[col]]) || any(!is.finite(ev[[col]]))) {
      bad <- which(is.na(ev[[col]]) | !is.finite(ev[[col]]))
      cli::cli_abort(c("Column {.field {col}} has {cli::qty(length(bad))}{length(bad)} missing or non-finite value{?s} (row{?s} {head(bad, 5)}).",
                       "i" = "paretoinfer does not impute or drop evaluations; decide on the value of a failed run before updating."),
                     call = call)
    }
  }
  for (i in seq_along(design$objectives)) {
    col <- design$objectives[i]; b <- design$bounds[, i]
    out <- which(ev[[col]] < b[1] | ev[[col]] > b[2])
    if (length(out)) {
      cli::cli_abort(c("{cli::qty(length(out))}{length(out)} value{?s} of {.field {col}} fall{?s/} outside the declared bounds [{b[1]}, {b[2]}] (row{?s} {head(out, 5)}).",
                       "i" = "Objectives are never clipped. Fix the data or declare wider bounds in {.fn pareto_design}."),
                     call = call)
    }
  }
  if (any(ev$cost < 0)) cli::cli_abort("Column {.field cost} must be non-negative.", call = call)
  ev
}

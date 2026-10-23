# Applications: one identification each with the public API, then R repeated
# identifications checked against the truth --------------------------------------

source("paper/code/common.R")
source("paper/code/app_binomial_methods.R")
source("paper/code/app_inventory_model.R")
R <- as.integer(Sys.getenv("PI_R", "50"))
dir.create("paper/results/cache", showWarnings = FALSE, recursive = TRUE)

# Binomial intervals: n = 30, each evaluation averages b = 100 simulated studies.
bin_truth <- binomial_truth(30)
bin_sim <- make_binomial_sim(30, b = 100L)
bin_design <- pareto_design(alternatives = rownames(bin_truth), objectives = c("miscoverage", "width"),
                            bounds = c(0, 1), alpha = 0.05, epsilon = 0.005, max_evaluations = 4e4)
bin_simulate <- function(alternative, n) {
  y <- bin_sim(match(alternative, bin_design$alternatives), n)
  data.frame(miscoverage = y[, 1], width = y[, 2])
}

# Inventory: 16 (s, S) policies; truth from 10^5 replications per policy.
truth_file <- "paper/results/cache/inventory_truth.rds"
if (!file.exists(truth_file)) {
  set.seed(7)
  inv_truth <- t(vapply(seq_len(nrow(policies)), function(k)
    colMeans(simulate_policy(policies$s[k], policies$S[k], 1e5)), numeric(2)))
  dimnames(inv_truth) <- list(rownames(policies), c("period_cost", "backlog"))
  saveRDS(inv_truth, truth_file)
}
inv_truth <- readRDS(truth_file)
inv_design <- pareto_design(alternatives = rownames(policies), objectives = c("period_cost", "backlog"),
                            bounds = inv_bounds, alpha = 0.05, epsilon = c(1, 0.005),
                            max_evaluations = 4e4)
inv_simulate <- function(alternative, n) {
  k <- match(alternative, inv_design$alternatives)
  y <- simulate_policy(policies$s[k], policies$S[k], n)
  data.frame(period_cost = y[, 1], backlog = y[, 2])
}

apps <- list(binomial = list(design = bin_design, simulate = bin_simulate, truth = bin_truth),
             inventory = list(design = inv_design, simulate = inv_simulate, truth = inv_truth))

for (an in names(apps)) {
  a <- apps[[an]]
  f <- sprintf("paper/results/cache/%s_state.rds", an)
  if (!file.exists(f)) {
    set.seed(2026)
    t0 <- proc.time()[["elapsed"]]
    st <- pareto_run(a$design, a$simulate, batch = 5L, initial = 5L)
    message(sprintf("%s: %d evaluations, %s, %.0f s", an, st$n_evaluations, st$stopping_reason,
                    proc.time()[["elapsed"]] - t0))
    saveRDS(st, f)
  }
}

# Repeated identifications: the same checks as Study 1, against the truth.
for (an in names(apps)) {
  f <- sprintf("paper/results/%s_repeated.csv", an)
  if (file.exists(f)) next
  a <- apps[[an]]; mu <- a$truth
  out <- mc(R, function() {
    st <- pareto_run(a$design, a$simulate, batch = 5L, initial = 5L)
    tab <- plausible_pareto_set(st)$table
    disc <- which(tab$status == "certified_dominated")
    acc <- which(tab$status %in% c("certified_optimal", "epsilon_optimal"))
    eps <- a$design$epsilon
    data.frame(n_evaluations = st$n_evaluations, stopping = st$stopping_reason,
               n_plausible = sum(tab$status != "certified_dominated"), n_accepted = length(acc),
               n_representatives = sum(tab$representative),
               false_discard = any(disc %in% pareto_index(mu)),
               false_accept = any(!(acc %in% eps_optimal_index(mu, eps))),
               cover_fail = !covers_front(mu, which(tab$representative), eps),
               plausible = paste(tab$alternative[tab$status != "certified_dominated"], collapse = ";"),
               representatives = paste(tab$alternative[tab$representative], collapse = ";"))
  }, seed = 99)
  write.csv(out, f, row.names = FALSE)
  message(sprintf("%s repeated: median n %.0f, any error %.3f", an, median(out$n_evaluations),
                  mean(out$false_discard | out$false_accept | out$cover_fail)))
}

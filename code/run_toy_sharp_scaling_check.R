# Small toy run to check whether sharp stability selection reacts to scaling.

source(file.path("code", "simulation_functions.R"))

toy_cores <- min(default_n_cores(), 4L)

print_stability_summary <- function(results, label) {
  stability_rows <- subset(results, algorithm %in% c("ncat_null", "ncat_3"))

  cat("\n====================\n")
  cat(label, "\n")
  cat("====================\n")
  print(
    aggregate(
      cbind(f1_score, recall, precision) ~ algorithm + scaling_method + binary_top_fraction,
      data = stability_rows,
      FUN = mean
    )
  )
}

run_toy_check <- function(label, output_prefix, scaling_methods, binary_fraction) {
  toy_run <- run_imbalance_benchmark(
    seeds = 1:3,
    scaling_methods = scaling_methods,
    dimension_scenarios = data.frame(n = 1000, pk = 100),
    binary_fraction_values = binary_fraction,
    binary_top_fractions = c(0.5, 0.05),
    pk_imbalance_fractions = c(0.2),
    active_predictors = 10,
    ev_xy_values = c(0.2),
    ev_xx_values = c(0.5),
    nfolds = 3,
    nlambda = 10,
    include_stability = TRUE,
    stability_repetitions = 20,
    sharp_n_cat_values = list(NULL, 3),
    n_cores = toy_cores,
    collect_predictor_metadata = FALSE
  )

  write_imbalance_outputs(
    imbalance_run = toy_run,
    output_prefix = output_prefix,
    write_metadata = FALSE
  )

  print_stability_summary(toy_run$results, label)
}

cat("Toy sharp scaling check using ", toy_cores, " cores.\n", sep = "")

run_toy_check(
  label = "All categorical toy check",
  output_prefix = "toy_sharp_scaling_check_all_cat",
  scaling_methods = c("none", "zscore"),
  binary_fraction = 1
)

run_toy_check(
  label = "Mixed toy check",
  output_prefix = "toy_sharp_scaling_check_mixed",
  scaling_methods = c("cont", "zscore", "2sd"),
  binary_fraction = 0.5
)

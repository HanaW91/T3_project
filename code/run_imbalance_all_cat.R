# All-categorical imbalance benchmark: n = 1000, p = 100.

source(file.path("code", "simulation_functions.R"))

imbalance_run <- run_imbalance_benchmark(
  seeds = 1:10,
  scaling_methods = c("none", "zscore"),
  dimension_scenarios = data.frame(n = 1000, pk = 100),
  binary_fraction_values = c(1),
  binary_top_fractions = c(0.5, 0.2, 0.1, 0.05),
  pk_imbalance_fractions = c(0.1, 0.2, 0.5, 0.8),
  active_predictors = 10,
  ev_xy_values = c(0.5, 0.2, 0.05),
  ev_xx_values = c(0, 0.1, 0.5, 0.9),
  stability_repetitions = 100,
  sharp_n_cat_values = list(NULL, 3),
  n_cores = default_n_cores(),
  collect_predictor_metadata = TRUE
)

write_imbalance_outputs(
  imbalance_run = imbalance_run,
  output_prefix = "imbalance_all_cat",
  write_metadata = TRUE
)

# Sanity checks for binary predictor creation.

binarise_by_percentile <- function(x, top_fraction = 0.2) {
  cutoff <- stats::quantile(x, probs = 1 - top_fraction, na.rm = TRUE)
  as.integer(x > cutoff)
}

make_some_predictors_binary <- function(X,
                                        binary_fraction = 1.0,
                                        binary_top_fraction = 0.2,
                                        pk_imbalance_fraction = 0.2,
                                        balanced_top_fraction = 0.5,
                                        seed = 1) {
  set.seed(seed)
  X <- as.data.frame(X)
  p <- ncol(X)
  n_binary <- round(p * binary_fraction)
  binary_columns <- sort(sample(seq_len(p), size = n_binary))
  n_imbalanced <- round(n_binary * pk_imbalance_fraction)
  imbalanced_columns <- sort(sample(binary_columns, size = n_imbalanced))
  balanced_columns <- setdiff(binary_columns, imbalanced_columns)

  for (j in imbalanced_columns) {
    X[[j]] <- binarise_by_percentile(
      X[[j]],
      top_fraction = binary_top_fraction
    )
  }

  for (j in balanced_columns) {
    X[[j]] <- binarise_by_percentile(
      X[[j]],
      top_fraction = balanced_top_fraction
    )
  }

  list(
    xdata = as.matrix(X),
    binary_columns = binary_columns,
    imbalanced_columns = imbalanced_columns,
    balanced_columns = balanced_columns
  )
}

mean_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

max_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}

is_binary_matrix <- function(X) {
  if (length(X) == 0) {
    return(TRUE)
  }

  all(X %in% c(0, 1))
}

check_one_scenario <- function(seed,
                               n = 1000,
                               p = 100,
                               binary_fraction,
                               binary_top_fraction,
                               pk_imbalance_fraction,
                               balanced_top_fraction = 0.5) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)

  modified <- make_some_predictors_binary(
    X = X,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    balanced_top_fraction = balanced_top_fraction,
    seed = seed + 1000
  )

  X_modified <- modified$xdata
  continuous_columns <- setdiff(seq_len(p), modified$binary_columns)

  imbalanced_props <- if (length(modified$imbalanced_columns) > 0) {
    colMeans(X_modified[, modified$imbalanced_columns, drop = FALSE])
  } else {
    numeric(0)
  }

  balanced_props <- if (length(modified$balanced_columns) > 0) {
    colMeans(X_modified[, modified$balanced_columns, drop = FALSE])
  } else {
    numeric(0)
  }

  continuous_unchanged <- if (length(continuous_columns) > 0) {
    isTRUE(all.equal(
      X[, continuous_columns, drop = FALSE],
      X_modified[, continuous_columns, drop = FALSE],
      check.attributes = FALSE
    ))
  } else {
    TRUE
  }

  data.frame(
    seed = seed,
    n = n,
    p = p,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    expected_binary_columns = round(p * binary_fraction),
    observed_binary_columns = length(modified$binary_columns),
    expected_imbalanced_columns = round(round(p * binary_fraction) * pk_imbalance_fraction),
    observed_imbalanced_columns = length(modified$imbalanced_columns),
    observed_balanced_columns = length(modified$balanced_columns),
    observed_continuous_columns = length(continuous_columns),
    imbalanced_mean_one_proportion = mean_or_na(imbalanced_props),
    balanced_mean_one_proportion = mean_or_na(balanced_props),
    imbalanced_max_abs_error = max_or_na(abs(imbalanced_props - binary_top_fraction)),
    balanced_max_abs_error = max_or_na(abs(balanced_props - balanced_top_fraction)),
    binary_columns_are_0_1 = is_binary_matrix(
      X_modified[, modified$binary_columns, drop = FALSE]
    ),
    continuous_columns_unchanged = continuous_unchanged
  )
}

scaling_scenarios <- expand.grid(
  binary_fraction = c(0, 0.25, 0.5, 0.75, 1.0),
  binary_top_fraction = 0.05,
  pk_imbalance_fraction = 0.2,
  stringsAsFactors = FALSE
)

balanced_scenarios <- expand.grid(
  binary_fraction = c(1.0, 0.5),
  binary_top_fraction = 0.5,
  pk_imbalance_fraction = 0,
  stringsAsFactors = FALSE
)

rare_imbalance_scenarios <- expand.grid(
  binary_fraction = c(1.0, 0.5),
  binary_top_fraction = c(0.2, 0.1, 0.05),
  pk_imbalance_fraction = c(0.1, 0.2, 0.5, 0.8),
  stringsAsFactors = FALSE
)

check_grid <- unique(rbind(
  scaling_scenarios,
  balanced_scenarios,
  rare_imbalance_scenarios
))

sanity_results <- do.call(
  rbind,
  lapply(seq_len(nrow(check_grid)), function(i) {
    check_one_scenario(
      seed = 123,
      binary_fraction = check_grid$binary_fraction[i],
      binary_top_fraction = check_grid$binary_top_fraction[i],
      pk_imbalance_fraction = check_grid$pk_imbalance_fraction[i]
    )
  })
)

sanity_results$count_check_passed <-
  sanity_results$expected_binary_columns == sanity_results$observed_binary_columns &
  sanity_results$expected_imbalanced_columns == sanity_results$observed_imbalanced_columns

sanity_results$proportion_check_passed <-
  (is.na(sanity_results$imbalanced_max_abs_error) |
     sanity_results$imbalanced_max_abs_error <= 0.01) &
  (is.na(sanity_results$balanced_max_abs_error) |
     sanity_results$balanced_max_abs_error <= 0.01)

sanity_results$all_checks_passed <-
  sanity_results$count_check_passed &
  sanity_results$proportion_check_passed &
  sanity_results$binary_columns_are_0_1 &
  sanity_results$continuous_columns_unchanged

print(sanity_results)

cat("\nSanity check summary:\n")
cat("Scenarios checked:", nrow(sanity_results), "\n")
cat("Passed:", sum(sanity_results$all_checks_passed), "\n")
cat("Failed:", sum(!sanity_results$all_checks_passed), "\n")

if (!dir.exists("results")) {
  dir.create("results")
}

utils::write.csv(
  sanity_results,
  file = file.path("results", "binary_creation_sanity_check.csv"),
  row.names = FALSE
)

if (!all(sanity_results$all_checks_passed)) {
  stop(
    "Some binary creation sanity checks failed. ",
    "Inspect results/binary_creation_sanity_check.csv."
  )
}

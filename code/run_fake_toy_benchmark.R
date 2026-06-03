# Toy benchmark for fake-generated regression data.
#
# Goal:
# - Generate one toy fake dataset per seed/scenario.
# - Try several scaling methods.
# - Fit LASSO with several scaling methods.
# - Save prediction and variable-selection performance in one results table.

required_packages <- c("fake", "glmnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the missing package(s) first:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

binarise_by_percentile <- function(x, top_fraction = 0.2) {
  cutoff <- stats::quantile(x, probs = 1 - top_fraction, na.rm = TRUE)
  as.integer(x > cutoff)
}

make_some_predictors_binary <- function(X,
                                        binary_fraction = 1.0,
                                        binary_top_fraction = 0.2,
                                        seed = 1) {
  set.seed(seed)
  X <- as.data.frame(X)
  p <- ncol(X)
  n_binary <- round(p * binary_fraction)
  binary_columns <- sort(sample(seq_len(p), size = n_binary))

  for (j in binary_columns) {
    X[[j]] <- binarise_by_percentile(
      X[[j]],
      top_fraction = binary_top_fraction
    )
  }

  list(
    xdata = as.matrix(X),
    binary_columns = binary_columns
  )
}

scale_train_test <- function(x_train, x_test, method) {
  if (method == "none") {
    return(list(train = x_train, test = x_test))
  }

  if (method == "zscore") {
    center <- colMeans(x_train)
    spread <- apply(x_train, 2, stats::sd)
  } else if (method == "2sd") {
    center <- colMeans(x_train)
    spread <- 2 * apply(x_train, 2, stats::sd)
  } else {
    stop("Unknown scaling method: ", method)
  }

  spread[is.na(spread) | spread == 0] <- 1

  list(
    train = sweep(sweep(x_train, 2, center, "-"), 2, spread, "/"),
    test = sweep(sweep(x_test, 2, center, "-"), 2, spread, "/")
  )
}

selection_metrics <- function(selected, active, p) {
  selected <- sort(unique(selected))
  active <- sort(unique(active))

  tp <- length(intersect(selected, active))
  fp <- length(setdiff(selected, active))
  fn <- length(setdiff(active, selected))
  tn <- p - tp - fp - fn
  precision <- tp / max(tp + fp, 1)
  recall <- tp / max(tp + fn, 1)
  f1_score <- if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }

  data.frame(
    selected_n = length(selected),
    active_n = length(active),
    true_positive = tp,
    false_positive = fp,
    false_negative = fn,
    true_negative = tn,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    sensitivity = tp / max(length(active), 1),
    false_discovery_rate = fp / max(length(selected), 1),
    specificity = tn / max(p - length(active), 1)
  )
}

prediction_metrics <- function(observed, predicted) {
  residuals <- observed - predicted
  sse <- sum(residuals^2)
  sst <- sum((observed - mean(observed))^2)

  data.frame(
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    r_squared = 1 - (sse / sst)
  )
}

fit_lasso <- function(x_train, y_train, x_test) {
  fit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = 1,
    family = "gaussian",
    standardize = FALSE
  )

  beta_hat <- as.matrix(stats::coef(fit, s = "lambda.1se"))[-1, 1]

  list(
    prediction = as.numeric(stats::predict(fit, newx = x_test, s = "lambda.1se")),
    selected = which(abs(beta_hat) > 1e-8),
    lambda = fit$lambda.1se
  )
}

simulate_one_dataset <- function(seed,
                                 n = 1000,
                                 pk = 100,
                                 binary_fraction = 1.0,
                                 binary_top_fraction = 0.2,
                                 nu_xy = 0.10,
                                 ev_xy = 0.7,
                                 ev_xx = 0.5) {
  set.seed(seed)

  x_graph <- fake::SimulateGraphical(
    n = n,
    pk = pk,
    nu_within = 0.8,
    nu_between = 0,
    ev_xx = ev_xx,
    v_sign = -1
  )

  modified <- make_some_predictors_binary(
    X = x_graph$data,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    seed = seed + 1000
  )

  sim <- fake::SimulateRegression(
    xdata = modified$xdata,
    family = "gaussian",
    q = 1,
    nu_xy = nu_xy,
    beta_abs = c(0.5, 1),
    beta_sign = c(-1, 1),
    continuous = TRUE,
    ev_xy = ev_xy
  )

  list(
    x = as.matrix(sim$xdata),
    y = as.numeric(sim$ydata[, 1]),
    active = which(sim$theta[, 1] != 0),
    binary_columns = modified$binary_columns
  )
}

run_one_benchmark <- function(seed,
                              scaling_method,
                              binary_top_fraction,
                              nu_xy,
                              ev_xy,
                              ev_xx,
                              binary_fraction = 1.0) {
  dat <- simulate_one_dataset(
    seed = seed,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    nu_xy = nu_xy,
    ev_xy = ev_xy,
    ev_xx = ev_xx
  )

  set.seed(seed + 2000)
  train_index <- sample(seq_len(nrow(dat$x)), size = floor(0.7 * nrow(dat$x)))
  test_index <- setdiff(seq_len(nrow(dat$x)), train_index)

  x_train <- dat$x[train_index, , drop = FALSE]
  x_test <- dat$x[test_index, , drop = FALSE]
  y_train <- dat$y[train_index]
  y_test <- dat$y[test_index]

  scaled <- scale_train_test(x_train, x_test, method = scaling_method)

  elapsed <- system.time({
    fit <- fit_lasso(
      x_train = scaled$train,
      y_train = y_train,
      x_test = scaled$test
    )
  })

  cbind(
    data.frame(
      seed = seed,
      algorithm = "lasso",
      scaling_method = scaling_method,
      n = nrow(dat$x),
      p = ncol(dat$x),
      binary_fraction = binary_fraction,
      binary_top_fraction = binary_top_fraction,
      nu_xy = nu_xy,
      ev_xy = ev_xy,
      ev_xx = ev_xx,
      binary_predictors = length(dat$binary_columns),
      lambda_1se = fit$lambda,
      elapsed_seconds = unname(elapsed[["elapsed"]])
    ),
    prediction_metrics(observed = y_test, predicted = fit$prediction),
    selection_metrics(selected = fit$selected, active = dat$active, p = ncol(dat$x))
  )
}

run_toy_benchmark <- function(
    seeds = 1:5,
    scaling_methods = c("none", "zscore", "2sd"),
    binary_top_fractions = c(0.5, 0.2, 0.1, 0.05),
    nu_xy_values = c(0.10),
    ev_xy_values = c(0.3, 0.5, 0.7),
    ev_xx_values = c(0.4, 0.5, 0.7, 0.9),
    binary_fraction = 1.0) {
  grid <- expand.grid(
    seed = seeds,
    scaling_method = scaling_methods,
    binary_top_fraction = binary_top_fractions,
    nu_xy = nu_xy_values,
    ev_xy = ev_xy_values,
    ev_xx = ev_xx_values,
    stringsAsFactors = FALSE
  )

  results <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    message("Running benchmark ", i, " of ", nrow(grid))
    results[[i]] <- run_one_benchmark(
      seed = grid$seed[i],
      scaling_method = grid$scaling_method[i],
      binary_top_fraction = grid$binary_top_fraction[i],
      nu_xy = grid$nu_xy[i],
      ev_xy = grid$ev_xy[i],
      ev_xx = grid$ev_xx[i],
      binary_fraction = binary_fraction
    )
  }

  do.call(rbind, results)
}

benchmark_results <- run_toy_benchmark()

print(benchmark_results)

summary_results <- aggregate(
  cbind(
    rmse,
    mae,
    r_squared,
    precision,
    recall,
    f1_score,
    sensitivity,
    false_discovery_rate,
    specificity
  ) ~
    scaling_method + binary_fraction + binary_top_fraction + nu_xy + ev_xy + ev_xx,
  data = benchmark_results,
  FUN = mean
)

print(summary_results)

if (!dir.exists("results")) {
  dir.create("results")
}

utils::write.csv(
  benchmark_results,
  file = file.path("results", "fake_toy_benchmark_results.csv"),
  row.names = FALSE
)

utils::write.csv(
  summary_results,
  file = file.path("results", "fake_toy_benchmark_summary.csv"),
  row.names = FALSE
)

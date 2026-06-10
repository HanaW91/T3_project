# Correlation-focused benchmark using fake-generated regression data.
# Main check: compare scaling methods across predictor-correlation settings.

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
                                        binary_fraction = 0.5,
                                        binary_top_fraction = 0.05,
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

scale_train_test <- function(x_train,
                             x_test,
                             method,
                             binary_columns = integer(0)) {
  if (method == "none") {
    return(list(train = x_train, test = x_test))
  }

  scale_columns <- seq_len(ncol(x_train))

  if (method == "zscore") {
    center <- colMeans(x_train[, scale_columns, drop = FALSE])
    spread <- apply(x_train[, scale_columns, drop = FALSE], 2, stats::sd)
  } else if (method == "2sd") {
    scale_columns <- setdiff(scale_columns, binary_columns)

    if (length(scale_columns) == 0) {
      return(list(train = x_train, test = x_test))
    }

    center <- colMeans(x_train[, scale_columns, drop = FALSE])
    spread <- 2 * apply(x_train[, scale_columns, drop = FALSE], 2, stats::sd)
  } else {
    stop("Unknown scaling method: ", method)
  }

  spread[is.na(spread) | spread == 0] <- 1
  scaled_train <- x_train
  scaled_test <- x_test
  scaled_train[, scale_columns] <- sweep(
    sweep(x_train[, scale_columns, drop = FALSE], 2, center, "-"),
    2,
    spread,
    "/"
  )
  scaled_test[, scale_columns] <- sweep(
    sweep(x_test[, scale_columns, drop = FALSE], 2, center, "-"),
    2,
    spread,
    "/"
  )

  list(train = scaled_train, test = scaled_test)
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

fit_lasso <- function(x_train,
                      y_train,
                      x_test,
                      foldid,
                      nfolds = 5,
                      nlambda = 50) {
  fit <- glmnet::cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    foldid = foldid,
    nfolds = nfolds,
    nlambda = nlambda
  )

  extract_fit <- function(lambda_name) {
    beta_hat <- as.matrix(stats::coef(fit, s = lambda_name))[-1, 1]

    list(
      prediction = as.numeric(stats::predict(fit, newx = x_test, s = lambda_name)),
      selected = which(abs(beta_hat) > 1e-8),
      lambda = fit[[lambda_name]]
    )
  }

  list(
    cv_lasso_min = extract_fit("lambda.min"),
    cv_lasso_1se = extract_fit("lambda.1se")
  )
}

simulate_one_dataset <- function(seed,
                                 n = 1000,
                                 pk = 100,
                                 binary_fraction = 0.5,
                                 binary_top_fraction = 0.05,
                                 pk_imbalance_fraction = 0.2,
                                 nu_xy = 0.10,
                                 ev_xy = 0.7,
                                 ev_xx = 0.4) {
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
    pk_imbalance_fraction = pk_imbalance_fraction,
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
    binary_columns = modified$binary_columns,
    imbalanced_columns = modified$imbalanced_columns,
    balanced_columns = modified$balanced_columns
  )
}

evaluate_one_scaling <- function(dat,
                                 seed,
                                 scaling_method,
                                 binary_fraction,
                                 binary_top_fraction,
                                 pk_imbalance_fraction,
                                 nu_xy,
                                 ev_xy,
                                 ev_xx,
                                 train_index,
                                 test_index,
                                 foldid,
                                 nfolds = 5,
                                 nlambda = 50) {
  x_train <- dat$x[train_index, , drop = FALSE]
  x_test <- dat$x[test_index, , drop = FALSE]
  y_train <- dat$y[train_index]
  y_test <- dat$y[test_index]

  scaled <- scale_train_test(
    x_train,
    x_test,
    method = scaling_method,
    binary_columns = dat$binary_columns
  )

  elapsed <- system.time({
    fits <- fit_lasso(
      x_train = scaled$train,
      y_train = y_train,
      x_test = scaled$test,
      foldid = foldid,
      nfolds = nfolds,
      nlambda = nlambda
    )
  })

  rows <- lapply(names(fits), function(algorithm_name) {
    fit <- fits[[algorithm_name]]

    cbind(
      data.frame(
        seed = seed,
        algorithm = algorithm_name,
        scaling_method = scaling_method,
        n = nrow(dat$x),
        p = ncol(dat$x),
        binary_fraction = binary_fraction,
        binary_top_fraction = binary_top_fraction,
        pk_imbalance_fraction = pk_imbalance_fraction,
        nu_xy = nu_xy,
        ev_xy = ev_xy,
        ev_xx = ev_xx,
        binary_predictors = length(dat$binary_columns),
        imbalanced_predictors = length(dat$imbalanced_columns),
        balanced_predictors = length(dat$balanced_columns),
        continuous_predictors = ncol(dat$x) - length(dat$binary_columns),
        lambda = fit$lambda,
        elapsed_seconds = unname(elapsed[["elapsed"]])
      ),
      prediction_metrics(observed = y_test, predicted = fit$prediction),
      selection_metrics(selected = fit$selected, active = dat$active, p = ncol(dat$x))
    )
  })

  do.call(rbind, rows)
}

run_one_correlation_scenario <- function(seed,
                                         scaling_methods,
                                         binary_fraction,
                                         binary_top_fraction,
                                         pk_imbalance_fraction,
                                         nu_xy,
                                         ev_xy,
                                         ev_xx,
                                         nfolds = 5,
                                         nlambda = 50) {
  dat <- simulate_one_dataset(
    seed = seed,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    nu_xy = nu_xy,
    ev_xy = ev_xy,
    ev_xx = ev_xx
  )

  set.seed(seed + 2000)
  train_index <- sample(seq_len(nrow(dat$x)), size = floor(0.7 * nrow(dat$x)))
  test_index <- setdiff(seq_len(nrow(dat$x)), train_index)

  set.seed(seed + 3000)
  foldid <- sample(rep(seq_len(nfolds), length.out = length(train_index)))

  results <- vector("list", length(scaling_methods))

  for (i in seq_along(scaling_methods)) {
    results[[i]] <- evaluate_one_scaling(
      dat = dat,
      seed = seed,
      scaling_method = scaling_methods[i],
      binary_fraction = binary_fraction,
      binary_top_fraction = binary_top_fraction,
      pk_imbalance_fraction = pk_imbalance_fraction,
      nu_xy = nu_xy,
      ev_xy = ev_xy,
      ev_xx = ev_xx,
      train_index = train_index,
      test_index = test_index,
      foldid = foldid,
      nfolds = nfolds,
      nlambda = nlambda
    )
  }

  do.call(rbind, results)
}

run_correlation_benchmark <- function(
    seeds = 1:10,
    scaling_methods = c("none", "zscore", "2sd"),
    binary_fraction_values = c(0.5),
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2,
    nu_xy = 0.10,
    ev_xy_values = c(0.1, 0.3, 0.5, 0.7, 0.9),
    ev_xx_values = c(0.5, 0.7, 0.9),
    nfolds = 5,
    nlambda = 50) {
  grid <- expand.grid(
    seed = seeds,
    binary_fraction = binary_fraction_values,
    ev_xy = ev_xy_values,
    ev_xx = ev_xx_values,
    stringsAsFactors = FALSE
  )

  message(
    "Correlation benchmark setup: ",
    nrow(grid),
    " generated datasets x ",
    length(scaling_methods),
    " scaling methods x 2 CV lambda choices = ",
    nrow(grid) * length(scaling_methods),
    " CV fits and ",
    nrow(grid) * length(scaling_methods) * 2,
    " result rows."
  )

  results <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    message(
      "Running correlation scenario ",
      i,
      " of ",
      nrow(grid),
      " (",
      length(scaling_methods),
      " scaling methods)"
    )

    results[[i]] <- run_one_correlation_scenario(
      seed = grid$seed[i],
      scaling_methods = scaling_methods,
      binary_fraction = grid$binary_fraction[i],
      binary_top_fraction = binary_top_fraction,
      pk_imbalance_fraction = pk_imbalance_fraction,
      nu_xy = nu_xy,
      ev_xy = grid$ev_xy[i],
      ev_xx = grid$ev_xx[i],
      nfolds = nfolds,
      nlambda = nlambda
    )
  }

  do.call(rbind, results)
}

correlation_results <- run_correlation_benchmark()

print(head(correlation_results))

correlation_summary <- aggregate(
  cbind(f1_score, precision, recall) ~
    algorithm + scaling_method + binary_fraction + binary_top_fraction +
      pk_imbalance_fraction + nu_xy + ev_xy + ev_xx,
  data = correlation_results,
  FUN = mean
)

print(correlation_summary)

if (!dir.exists("results")) {
  dir.create("results")
}

utils::write.csv(
  correlation_results,
  file = file.path("results", "correlation_benchmark_results.csv"),
  row.names = FALSE
)

utils::write.csv(
  correlation_summary,
  file = file.path("results", "correlation_benchmark_summary.csv"),
  row.names = FALSE
)

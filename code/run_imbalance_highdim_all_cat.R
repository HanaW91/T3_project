# Imbalance-focused benchmark using fake-generated regression data.
# Adds stability-selection LASSO with ncut = 0 and ncut = 3.

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

select_binary_columns <- function(p, binary_fraction = 1.0, seed = 1) {
  set.seed(seed)
  n_binary <- round(p * binary_fraction)
  sort(sample(seq_len(p), size = n_binary))
}

sample_fraction <- function(x, fraction) {
  n_sample <- round(length(x) * fraction)

  if (n_sample == 0) {
    return(integer(0))
  }

  sort(sample(x, size = n_sample))
}

make_predictors_binary_with_matched_imbalance <- function(X,
                                                         binary_columns,
                                                         active_columns = NULL,
                                                         binary_top_fraction = 0.2,
                                                         pk_imbalance_fraction = 0.2,
                                                         balanced_top_fraction = 0.5,
                                                         seed = 1) {
  set.seed(seed)
  X <- as.data.frame(X)
  if (is.null(active_columns)) {
    active_imbalanced <- integer(0)
    noise_imbalanced <- integer(0)
    imbalanced_columns <- sample_fraction(binary_columns, pk_imbalance_fraction)
  } else {
    active_binary <- intersect(active_columns, binary_columns)
    noise_binary <- setdiff(binary_columns, active_binary)
    active_imbalanced <- sample_fraction(active_binary, pk_imbalance_fraction)
    noise_imbalanced <- sample_fraction(noise_binary, pk_imbalance_fraction)
    imbalanced_columns <- sort(c(active_imbalanced, noise_imbalanced))
  }
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
    balanced_columns = balanced_columns,
    active_imbalanced_columns = active_imbalanced,
    noise_imbalanced_columns = noise_imbalanced
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

selection_metrics <- function(selected,
                              active,
                              p,
                              na_when_no_active = FALSE) {
  selected <- sort(unique(selected))
  active <- sort(unique(active))

  tp <- length(intersect(selected, active))
  fp <- length(setdiff(selected, active))
  fn <- length(setdiff(active, selected))
  tn <- p - tp - fp - fn
  precision <- tp / max(tp + fp, 1)
  recall <- if (na_when_no_active && length(active) == 0) {
    NA_real_
  } else {
    tp / max(tp + fn, 1)
  }
  f1_score <- if (is.na(recall)) {
    NA_real_
  } else if ((precision + recall) == 0) {
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

prefix_columns <- function(data, prefix) {
  names(data) <- paste(prefix, names(data), sep = "_")
  data
}

selection_metrics_for_indices <- function(selected, active, indices) {
  indices <- sort(unique(indices))

  selection_metrics(
    selected = intersect(selected, indices),
    active = intersect(active, indices),
    p = length(indices),
    na_when_no_active = TRUE
  )
}

grouped_selection_metrics <- function(selected,
                                      active,
                                      p,
                                      binary_columns,
                                      imbalanced_columns,
                                      balanced_columns) {
  continuous_columns <- setdiff(seq_len(p), binary_columns)

  cbind(
    selection_metrics(selected = selected, active = active, p = p),
    prefix_columns(
      selection_metrics_for_indices(
        selected = selected,
        active = active,
        indices = binary_columns
      ),
      "binary"
    ),
    prefix_columns(
      selection_metrics_for_indices(
        selected = selected,
        active = active,
        indices = continuous_columns
      ),
      "continuous"
    ),
    prefix_columns(
      selection_metrics_for_indices(
        selected = selected,
        active = active,
        indices = imbalanced_columns
      ),
      "rare_binary"
    ),
    prefix_columns(
      selection_metrics_for_indices(
        selected = selected,
        active = active,
        indices = balanced_columns
      ),
      "nonrare_binary"
    )
  )
}

predictor_imbalance_composition <- function(active,
                                            p,
                                            binary_columns,
                                            imbalanced_columns,
                                            balanced_columns,
                                            target_rare_fraction) {
  active <- sort(unique(active))
  all_columns <- seq_len(p)
  noise <- setdiff(all_columns, active)
  continuous_columns <- setdiff(all_columns, binary_columns)

  active_binary <- intersect(active, binary_columns)
  active_rare_binary <- intersect(active, imbalanced_columns)
  active_nonrare_binary <- intersect(active, balanced_columns)
  active_continuous <- intersect(active, continuous_columns)

  noise_binary <- intersect(noise, binary_columns)
  noise_rare_binary <- intersect(noise, imbalanced_columns)
  noise_nonrare_binary <- intersect(noise, balanced_columns)
  noise_continuous <- intersect(noise, continuous_columns)

  data.frame(
    active_predictors = length(active),
    active_binary_predictors = length(active_binary),
    active_continuous_predictors = length(active_continuous),
    active_rare_binary_predictors = length(active_rare_binary),
    active_nonrare_binary_predictors = length(active_nonrare_binary),
    active_rare_binary_fraction = length(active_rare_binary) /
      max(length(active_binary), 1),
    active_rare_binary_target_fraction = target_rare_fraction,
    noise_predictors = length(noise),
    noise_binary_predictors = length(noise_binary),
    noise_continuous_predictors = length(noise_continuous),
    noise_rare_binary_predictors = length(noise_rare_binary),
    noise_nonrare_binary_predictors = length(noise_nonrare_binary),
    noise_rare_binary_fraction = length(noise_rare_binary) /
      max(length(noise_binary), 1),
    noise_rare_binary_target_fraction = target_rare_fraction,
    rare_fraction_difference_active_minus_noise =
      (length(active_rare_binary) / max(length(active_binary), 1)) -
        (length(noise_rare_binary) / max(length(noise_binary), 1))
  )
}

predictor_selection_metadata <- function(dat,
                                         selected,
                                         seed,
                                         algorithm,
                                         scaling_method,
                                         binary_fraction,
                                         binary_top_fraction,
                                         pk_imbalance_fraction,
                                         nu_xy,
                                         ev_xy,
                                         ev_xx) {
  variable <- seq_len(ncol(dat$x))
  is_binary <- variable %in% dat$binary_columns
  is_rare_binary <- variable %in% dat$imbalanced_columns
  is_nonrare_binary <- variable %in% dat$balanced_columns

  predictor_group <- ifelse(
    is_rare_binary,
    "rare_binary",
    ifelse(
      is_nonrare_binary,
      "nonrare_binary",
      ifelse(is_binary, "binary_other", "continuous")
    )
  )

  data.frame(
    seed = seed,
    algorithm = algorithm,
    scaling_method = scaling_method,
    n = nrow(dat$x),
    p = ncol(dat$x),
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    nu_xy = nu_xy,
    ev_xy = ev_xy,
    ev_xx = ev_xx,
    variable = variable,
    is_active = variable %in% dat$active,
    is_binary = is_binary,
    is_rare_binary = is_rare_binary,
    is_nonrare_binary = is_nonrare_binary,
    is_continuous = !is_binary,
    is_selected = variable %in% selected,
    predictor_group = predictor_group
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

generate_outcome_from_beta <- function(xdata, beta, ev_xy, seed) {
  set.seed(seed)
  signal <- as.numeric(xdata %*% beta)
  signal <- signal - mean(signal)
  signal_variance <- stats::var(signal)

  if (is.na(signal_variance) || signal_variance == 0 || ev_xy >= 1) {
    noise <- rep(0, length(signal))
  } else {
    noise_variance <- signal_variance * (1 - ev_xy) / ev_xy
    noise <- stats::rnorm(length(signal), mean = 0, sd = sqrt(noise_variance))
  }

  signal + noise
}

simulate_predictor_matrix <- function(n, pk, ev_xx, seed) {
  set.seed(seed)

  if (ev_xx == 0) {
    x_graph <- fake::SimulateGraphical(
      n = n,
      pk = pk,
      nu_within = 0,
      v_within = c(0, 1),
      v_sign = c(-1, 1),
      ev_xx = 0,
      u_list = c(1e-10, 1000),
      pd_strategy = "min_eigenvalue"
    )

    return(x_graph$data)
  }

  x_graph <- fake::SimulateGraphical(
    n = n,
    pk = pk,
    nu_within = 1,
    v_within = c(0, 1),
    v_sign = c(-1, 1),
    ev_xx = ev_xx,
    u_list = c(1e-10, 1000),
    pd_strategy = "min_eigenvalue"
  )

  x_graph$data
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

predict_from_selected_linear <- function(x_train, y_train, x_test, selected) {
  selected <- sort(unique(selected))

  if (length(selected) == 0) {
    return(rep(mean(y_train), nrow(x_test)))
  }

  train_design <- cbind("(Intercept)" = 1, x_train[, selected, drop = FALSE])
  test_design <- cbind("(Intercept)" = 1, x_test[, selected, drop = FALSE])
  fit <- stats::lm.fit(x = train_design, y = y_train)
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0

  as.numeric(test_design %*% coefficients)
}

fit_stability_lasso <- function(x_train,
                                y_train,
                                x_test,
                                ncuts = c(0, 3),
                                repetitions = 25,
                                subsample_fraction = 0.5,
                                nfolds = 5,
                                nlambda = 50,
                                seed = 1) {
  p <- ncol(x_train)
  n_train <- nrow(x_train)
  subsample_size <- max(2, floor(n_train * subsample_fraction))
  selection_counts <- integer(p)

  for (b in seq_len(repetitions)) {
    set.seed(seed + b)
    subsample_index <- sample(seq_len(n_train), size = subsample_size)
    x_sub <- x_train[subsample_index, , drop = FALSE]
    y_sub <- y_train[subsample_index]

    foldid <- sample(rep(seq_len(nfolds), length.out = length(subsample_index)))
    fit <- glmnet::cv.glmnet(
      x = x_sub,
      y = y_sub,
      alpha = 1,
      family = "gaussian",
      standardize = FALSE,
      foldid = foldid,
      nfolds = nfolds,
      nlambda = nlambda
    )

    beta_hat <- as.matrix(stats::coef(fit, s = "lambda.1se"))[-1, 1]
    selected <- which(abs(beta_hat) > 1e-8)
    selection_counts[selected] <- selection_counts[selected] + 1L
  }

  fits <- lapply(ncuts, function(ncut) {
    selected <- which(selection_counts > ncut)

    list(
      prediction = predict_from_selected_linear(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        selected = selected
      ),
      selected = selected,
      lambda = NA_real_,
      ncut = ncut,
      stability_repetitions = repetitions,
      stability_subsample_fraction = subsample_fraction,
      max_selection_count = max(selection_counts),
      mean_selection_count = mean(selection_counts)
    )
  })

  names(fits) <- paste0("stability_lasso_ncut_", ncuts)
  fits
}

simulate_one_dataset <- function(seed,
                                 n = 1000,
                                 pk = 100,
                                 binary_fraction = 0.5,
                                 binary_top_fraction = 0.2,
                                 pk_imbalance_fraction = 0.2,
                                 nu_xy = 0.01,
                                 ev_xy = 0.7,
                                 ev_xx = 0.4) {
  set.seed(seed)

  x_data <- simulate_predictor_matrix(
    n = n,
    pk = pk,
    ev_xx = ev_xx,
    seed = seed + 900
  )

  binary_columns <- select_binary_columns(
    p = pk,
    binary_fraction = binary_fraction,
    seed = seed + 1000
  )

  modified <- make_predictors_binary_with_matched_imbalance(
    X = x_data,
    binary_columns = binary_columns,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    seed = seed + 1100
  )

  truth_sim <- fake::SimulateRegression(
    xdata = modified$xdata,
    family = "gaussian",
    q = 1,
    nu_xy = nu_xy,
    beta_abs = c(0.5, 1),
    beta_sign = c(-1, 1),
    continuous = TRUE,
    ev_xy = ev_xy
  )
  active_columns <- which(truth_sim$theta[, 1] != 0)

  beta <- as.numeric(truth_sim$beta[, 1])
  y <- generate_outcome_from_beta(
    xdata = modified$xdata,
    beta = beta,
    ev_xy = ev_xy,
    seed = seed + 1200
  )

  list(
    x = as.matrix(modified$xdata),
    y = y,
    active = active_columns,
    binary_columns = modified$binary_columns,
    imbalanced_columns = modified$imbalanced_columns,
    balanced_columns = modified$balanced_columns,
    active_imbalanced_columns = intersect(active_columns, modified$imbalanced_columns),
    noise_imbalanced_columns = setdiff(modified$imbalanced_columns, active_columns)
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
                                 nlambda = 50,
                                 include_stability = TRUE,
                                 stability_ncuts = c(0, 3),
                                 stability_repetitions = 25,
                                 stability_subsample_fraction = 0.5) {
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
    cv_fits <- fit_lasso(
      x_train = scaled$train,
      y_train = y_train,
      x_test = scaled$test,
      foldid = foldid,
      nfolds = nfolds,
      nlambda = nlambda
    )

    stability_fits <- if (include_stability) {
      fit_stability_lasso(
        x_train = scaled$train,
        y_train = y_train,
        x_test = scaled$test,
        ncuts = stability_ncuts,
        repetitions = stability_repetitions,
        subsample_fraction = stability_subsample_fraction,
        nfolds = nfolds,
        nlambda = nlambda,
        seed = seed + 4000
      )
    } else {
      list()
    }

    fits <- c(cv_fits, stability_fits)
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
        ncut = if (!is.null(fit$ncut)) fit$ncut else NA_real_,
        stability_repetitions = if (!is.null(fit$stability_repetitions)) {
          fit$stability_repetitions
        } else {
          NA_real_
        },
        stability_subsample_fraction =
          if (!is.null(fit$stability_subsample_fraction)) {
            fit$stability_subsample_fraction
          } else {
            NA_real_
          },
        max_selection_count = if (!is.null(fit$max_selection_count)) {
          fit$max_selection_count
        } else {
          NA_real_
        },
        mean_selection_count = if (!is.null(fit$mean_selection_count)) {
          fit$mean_selection_count
        } else {
          NA_real_
        },
        elapsed_seconds = unname(elapsed[["elapsed"]])
      ),
      predictor_imbalance_composition(
        active = dat$active,
        p = ncol(dat$x),
        binary_columns = dat$binary_columns,
        imbalanced_columns = dat$imbalanced_columns,
        balanced_columns = dat$balanced_columns,
        target_rare_fraction = pk_imbalance_fraction
      ),
      prediction_metrics(observed = y_test, predicted = fit$prediction),
      grouped_selection_metrics(
        selected = fit$selected,
        active = dat$active,
        p = ncol(dat$x),
        binary_columns = dat$binary_columns,
        imbalanced_columns = dat$imbalanced_columns,
        balanced_columns = dat$balanced_columns
      )
    )
  })

  metadata_rows <- lapply(names(fits), function(algorithm_name) {
    fit <- fits[[algorithm_name]]

    predictor_selection_metadata(
      dat = dat,
      selected = fit$selected,
      seed = seed,
      algorithm = algorithm_name,
      scaling_method = scaling_method,
      binary_fraction = binary_fraction,
      binary_top_fraction = binary_top_fraction,
      pk_imbalance_fraction = pk_imbalance_fraction,
      nu_xy = nu_xy,
      ev_xy = ev_xy,
      ev_xx = ev_xx
    )
  })

  list(
    results = do.call(rbind, rows),
    predictor_metadata = do.call(rbind, metadata_rows)
  )
}

run_one_imbalance_scenario <- function(seed,
                                       scaling_methods,
                                       n,
                                       pk,
                                       binary_fraction,
                                       binary_top_fraction,
                                       pk_imbalance_fraction,
                                       nu_xy = 0.01,
                                       ev_xy = 0.7,
                                       ev_xx = 0.4,
                                       nfolds = 5,
                                       nlambda = 50,
                                       include_stability = TRUE,
                                       stability_ncuts = c(0, 3),
                                       stability_repetitions = 25,
                                       stability_subsample_fraction = 0.5,
                                       collect_predictor_metadata = FALSE) {
  dat <- simulate_one_dataset(
    seed = seed,
    n = n,
    pk = pk,
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
      nlambda = nlambda,
      include_stability = include_stability,
      stability_ncuts = stability_ncuts,
      stability_repetitions = stability_repetitions,
      stability_subsample_fraction = stability_subsample_fraction
    )
  }

  list(
    results = do.call(rbind, lapply(results, `[[`, "results")),
    predictor_metadata = do.call(
      rbind,
      lapply(results, `[[`, "predictor_metadata")
    )
  )
}

run_imbalance_benchmark <- function(
    seeds = 1:10,
    scaling_methods = c("none", "zscore", "2sd"),
    dimension_scenarios = data.frame(n = 100, pk = 1000),
    binary_fraction_values = c(1),
    binary_top_fractions = c(0.5, 0.2, 0.1, 0.05),
    pk_imbalance_fractions = c(0.1, 0.2, 0.5, 0.8),
    nu_xy = 0.01,
    ev_xy_values = c(0.5, 0.2, 0.05),
    ev_xx_values = c(0, 0.1, 0.5, 0.9),
    nfolds = 5,
    nlambda = 50,
    include_stability = TRUE,
    stability_ncuts = c(0, 3),
    stability_repetitions = 25,
    stability_subsample_fraction = 0.5,
    collect_predictor_metadata = FALSE) {
  balanced_grid <- expand.grid(
    dimension_scenario = seq_len(nrow(dimension_scenarios)),
    seed = seeds,
    binary_fraction = binary_fraction_values,
    binary_top_fraction = 0.5,
    pk_imbalance_fraction = 0,
    ev_xy = ev_xy_values,
    ev_xx = ev_xx_values,
    stringsAsFactors = FALSE
  )
  balanced_grid$n <- dimension_scenarios$n[balanced_grid$dimension_scenario]
  balanced_grid$pk <- dimension_scenarios$pk[balanced_grid$dimension_scenario]

  rare_binary_top_fractions <- setdiff(binary_top_fractions, 0.5)

  rare_grid <- expand.grid(
    dimension_scenario = seq_len(nrow(dimension_scenarios)),
    seed = seeds,
    binary_fraction = binary_fraction_values,
    binary_top_fraction = rare_binary_top_fractions,
    pk_imbalance_fraction = pk_imbalance_fractions,
    ev_xy = ev_xy_values,
    ev_xx = ev_xx_values,
    stringsAsFactors = FALSE
  )
  rare_grid$n <- dimension_scenarios$n[rare_grid$dimension_scenario]
  rare_grid$pk <- dimension_scenarios$pk[rare_grid$dimension_scenario]

  grid <- rbind(balanced_grid, rare_grid)

  message(
    "Imbalance benchmark setup: ",
    nrow(grid),
    " generated datasets x ",
    length(scaling_methods),
    " scaling methods x ",
    2 + if (include_stability) length(stability_ncuts) else 0,
    " algorithms = ",
    nrow(grid) * length(scaling_methods),
    " CV fits",
    if (include_stability) {
      paste0(
        " plus ",
        nrow(grid) * length(scaling_methods) * stability_repetitions,
        " stability subsample CV fits"
      )
    } else {
      ""
    },
    " and ",
    nrow(grid) * length(scaling_methods) *
      (2 + if (include_stability) length(stability_ncuts) else 0),
    " result rows."
  )

  results <- vector("list", nrow(grid))
  predictor_metadata <- if (collect_predictor_metadata) {
    vector("list", nrow(grid))
  } else {
    NULL
  }

  for (i in seq_len(nrow(grid))) {
    message(
      "Running imbalance scenario ",
      i,
      " of ",
      nrow(grid),
      " (",
      length(scaling_methods),
      " scaling methods)"
    )

    scenario_result <- run_one_imbalance_scenario(
      seed = grid$seed[i],
      scaling_methods = scaling_methods,
      n = grid$n[i],
      pk = grid$pk[i],
      binary_fraction = grid$binary_fraction[i],
      binary_top_fraction = grid$binary_top_fraction[i],
      pk_imbalance_fraction = grid$pk_imbalance_fraction[i],
      nu_xy = nu_xy,
      ev_xy = grid$ev_xy[i],
      ev_xx = grid$ev_xx[i],
      nfolds = nfolds,
      nlambda = nlambda,
      include_stability = include_stability,
      stability_ncuts = stability_ncuts,
      stability_repetitions = stability_repetitions,
      stability_subsample_fraction = stability_subsample_fraction,
      collect_predictor_metadata = collect_predictor_metadata
    )

    results[[i]] <- scenario_result$results

    if (collect_predictor_metadata) {
      predictor_metadata[[i]] <- scenario_result$predictor_metadata
    }

    rm(scenario_result)

    if (i %% 25 == 0) {
      gc()
    }
  }

  list(
    results = do.call(rbind, results),
    predictor_metadata = if (collect_predictor_metadata) {
      do.call(rbind, predictor_metadata)
    } else {
      NULL
    }
  )
}

imbalance_run <- run_imbalance_benchmark(
  seeds = 1:10,
  binary_top_fractions = c(0.5, 0.2, 0.1, 0.05),
  pk_imbalance_fractions = c(0.1, 0.2, 0.5, 0.8),
  ev_xy_values = c(0.5, 0.2, 0.05),
  ev_xx_values = c(0, 0.1, 0.5, 0.9),
  stability_repetitions = 25
)
imbalance_results <- imbalance_run$results
imbalance_predictor_metadata <- imbalance_run$predictor_metadata

print(head(imbalance_results))
if (!is.null(imbalance_predictor_metadata)) {
  print(head(imbalance_predictor_metadata))
}

imbalance_summary <- aggregate(
  cbind(
    f1_score,
    precision,
    recall,
    binary_f1_score,
    binary_precision,
    binary_recall,
    continuous_f1_score,
    continuous_precision,
    continuous_recall,
    rare_binary_f1_score,
    rare_binary_precision,
    rare_binary_recall,
    nonrare_binary_f1_score,
    nonrare_binary_precision,
    nonrare_binary_recall,
    active_binary_predictors,
    active_continuous_predictors,
    active_rare_binary_predictors,
    active_nonrare_binary_predictors,
    active_rare_binary_fraction,
    active_rare_binary_target_fraction,
    noise_binary_predictors,
    noise_continuous_predictors,
    noise_rare_binary_predictors,
    noise_nonrare_binary_predictors,
    noise_rare_binary_fraction,
    noise_rare_binary_target_fraction,
    rare_fraction_difference_active_minus_noise
  ) ~
    algorithm + scaling_method + n + p + binary_fraction + binary_top_fraction +
      pk_imbalance_fraction + nu_xy + ev_xy + ev_xx,
  data = imbalance_results,
  na.action = na.pass,
  FUN = function(x) mean(x, na.rm = TRUE)
)

print(imbalance_summary)

if (!dir.exists("results")) {
  dir.create("results")
}

utils::write.csv(
  imbalance_results,
  file = file.path("results", "imbalance_highdim_all_cat_results.csv"),
  row.names = FALSE
)

utils::write.csv(
  imbalance_summary,
  file = file.path("results", "imbalance_highdim_all_cat_summary.csv"),
  row.names = FALSE
)

if (!is.null(imbalance_predictor_metadata)) {
  utils::write.csv(
    imbalance_predictor_metadata,
    file = file.path("results", "imbalance_highdim_all_cat_metadata.csv"),
    row.names = FALSE
  )
}

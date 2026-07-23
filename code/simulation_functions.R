# Shared simulation and model-fitting functions for the imbalance benchmarks.

required_packages <- c("fake", "glmnet", "sharp")
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

# Small sampling wrappers used throughout the scenario setup.
sample_fraction <- function(x, fraction) {
  n_sample <- round(length(x) * fraction)

  if (n_sample == 0 || length(x) == 0) {
    return(integer(0))
  }

  sort(sample(x, size = min(n_sample, length(x))))
}

sample_n_or_all <- function(x, n) {
  if (n <= 0 || length(x) == 0) {
    return(integer(0))
  }

  sort(sample(x, size = min(n, length(x))))
}

default_n_cores <- function(max_cores = 40L) {
  # Prefer the scheduler allocation; do not use the whole HPC node by accident.
  scheduler_vars <- c("PBS_NCPUS", "NCPUS", "OMP_NUM_THREADS", "SLURM_CPUS_PER_TASK")
  scheduler_cores <- suppressWarnings(as.integer(Sys.getenv(scheduler_vars, "")))
  scheduler_cores <- scheduler_cores[!is.na(scheduler_cores) & scheduler_cores > 0]

  if (length(scheduler_cores) > 0) {
    return(min(scheduler_cores[1], max_cores))
  }

  nodefile <- Sys.getenv("PBS_NODEFILE", "")
  if (nzchar(nodefile) && file.exists(nodefile)) {
    nodefile_cores <- length(readLines(nodefile, warn = FALSE))
    if (nodefile_cores > 0) {
      return(min(nodefile_cores, max_cores))
    }
  }

  local_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(local_cores)) {
    return(1L)
  }

  min(max_cores, max(1L, local_cores))
}

select_binary_columns <- function(p, binary_fraction = 1.0, seed = 1) {
  set.seed(seed)
  n_binary <- round(p * binary_fraction)
  sort(sample(seq_len(p), size = n_binary))
}

# Pick true predictors manually, then encode them in theta.
choose_active_columns <- function(binary_columns,
                                  imbalanced_columns,
                                  balanced_columns,
                                  continuous_columns,
                                  active_predictors = 10,
                                  binary_fraction = 0.5,
                                  pk_imbalance_fraction = 0.2,
                                  seed = 1) {
  set.seed(seed)

  # Split the active set into binary and continuous predictors.
  if (length(continuous_columns) == 0) {
    n_active_binary <- active_predictors
    n_active_continuous <- 0
  } else if (length(binary_columns) == 0) {
    n_active_binary <- 0
    n_active_continuous <- active_predictors
  } else {
    n_active_binary <- round(active_predictors * binary_fraction)
    n_active_binary <- max(1, min(n_active_binary, length(binary_columns)))
    n_active_continuous <- active_predictors - n_active_binary
    n_active_continuous <- max(1, min(n_active_continuous, length(continuous_columns)))
  }

  # Match the rare proportion among active binary predictors as closely as possible.
  n_active_rare <- round(n_active_binary * pk_imbalance_fraction)
  if (pk_imbalance_fraction > 0 && n_active_binary > 1 && length(imbalanced_columns) > 0) {
    n_active_rare <- max(1, n_active_rare)
  }
  n_active_rare <- min(n_active_rare, length(imbalanced_columns), n_active_binary)

  n_active_nonrare <- n_active_binary - n_active_rare
  active_rare <- sample_n_or_all(imbalanced_columns, n_active_rare)
  active_nonrare <- sample_n_or_all(balanced_columns, n_active_nonrare)

  if (length(active_nonrare) < n_active_nonrare) {
    extra_binary <- setdiff(binary_columns, c(active_rare, active_nonrare))
    active_nonrare <- sort(c(
      active_nonrare,
      sample_n_or_all(extra_binary, n_active_nonrare - length(active_nonrare))
    ))
  }

  active_continuous <- sample_n_or_all(continuous_columns, n_active_continuous)
  active_columns <- sort(unique(c(active_rare, active_nonrare, active_continuous)))

  # fake::SimulateRegression uses theta to identify true predictors instead of nu_xy.
  theta <- matrix(0, nrow = length(binary_columns) + length(continuous_columns), ncol = 1)
  theta[active_columns, 1] <- 1

  list(
    active_columns = active_columns,
    active_rare_binary = active_rare,
    active_nonrare_binary = active_nonrare,
    active_continuous = active_continuous,
    theta = theta
  )
}

make_predictors_binary <- function(X,
                                   binary_columns,
                                   imbalanced_columns,
                                   balanced_columns,
                                   binary_top_fraction = 0.2,
                                   balanced_top_fraction = 0.5) {
  X <- as.data.frame(X)

  # Rare binary predictors have lower prevalence
  for (j in imbalanced_columns) {
    X[[j]] <- binarise_by_percentile(X[[j]], top_fraction = binary_top_fraction)
  }

  # Balanced binary predictors use a 50/50 split by default.
  for (j in balanced_columns) {
    X[[j]] <- binarise_by_percentile(X[[j]], top_fraction = balanced_top_fraction)
  }

  as.matrix(X)
}

scale_matrix <- function(x, method, binary_columns = integer(0)) {
  if (method == "none") {
    return(x)
  }

  scale_columns <- seq_len(ncol(x))

  if (method %in% c("cont", "2sd")) {
    # For mixed data, keep binary predictors unchanged and scale continuous ones.
    scale_columns <- setdiff(scale_columns, binary_columns)
  }

  if (length(scale_columns) == 0) {
    return(x)
  }

  center <- colMeans(x[, scale_columns, drop = FALSE])
  spread <- apply(x[, scale_columns, drop = FALSE], 2, stats::sd)

  if (method == "2sd") {
    spread <- 2 * spread
  } else if (!method %in% c("zscore", "cont")) {
    stop("Unknown scaling method: ", method)
  }

  spread[is.na(spread) | spread == 0] <- 1
  x_scaled <- x
  x_scaled[, scale_columns] <- sweep(
    sweep(x[, scale_columns, drop = FALSE], 2, center, "-"),
    2,
    spread,
    "/"
  )
  x_scaled
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
    recall = recall,
    precision = precision,
    f1_score = f1_score
  )
}

# Sanity check for the simulated signal, not a prediction metric.
signal_r_squared <- function(x, beta, y) {
  signal <- as.numeric(x %*% beta)

  if (stats::sd(signal) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(signal, y)^2
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

  # Main metrics plus the subgroup metrics used in the diagnostic plots.
  cbind(
    selection_metrics(selected = selected, active = active, p = p),
    prefix_columns(selection_metrics_for_indices(selected, active, binary_columns), "binary"),
    prefix_columns(selection_metrics_for_indices(selected, active, continuous_columns), "continuous"),
    prefix_columns(selection_metrics_for_indices(selected, active, imbalanced_columns), "rare_binary"),
    prefix_columns(selection_metrics_for_indices(selected, active, balanced_columns), "nonrare_binary")
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

  # Track whether the active and noise predictors follow the intended rarity mix.
  data.frame(
    active_predictors = length(active),
    active_binary_predictors = length(active_binary),
    active_continuous_predictors = length(active_continuous),
    active_rare_binary_predictors = length(active_rare_binary),
    active_nonrare_binary_predictors = length(active_nonrare_binary),
    active_rare_binary_fraction = length(active_rare_binary) / max(length(active_binary), 1),
    active_rare_binary_target_fraction = target_rare_fraction,
    noise_predictors = length(noise),
    noise_binary_predictors = length(noise_binary),
    noise_continuous_predictors = length(noise_continuous),
    noise_rare_binary_predictors = length(noise_rare_binary),
    noise_nonrare_binary_predictors = length(noise_nonrare_binary),
    noise_rare_binary_fraction = length(noise_rare_binary) / max(length(noise_binary), 1),
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
                                         active_predictors,
                                         ev_xy,
                                         ev_xx) {
  variable <- seq_len(ncol(dat$x))
  is_binary <- variable %in% dat$binary_columns
  is_rare_binary <- variable %in% dat$imbalanced_columns
  is_nonrare_binary <- variable %in% dat$balanced_columns
  predictor_group <- ifelse(
    is_rare_binary,
    "rare_binary",
    ifelse(is_nonrare_binary, "nonrare_binary", ifelse(is_binary, "binary_other", "continuous"))
  )

  # One row per predictor, useful for rare/non-rare subgroup checks.
  data.frame(
    seed = seed,
    algorithm = algorithm,
    scaling_method = scaling_method,
    n = nrow(dat$x),
    p = ncol(dat$x),
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    target_active_predictors = active_predictors,
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

simulate_predictor_matrix <- function(n, pk, ev_xx, seed) {
  set.seed(seed)

  # No-correlation case uses the fake settings suggested for ev_xx = 0.
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

  # Correlated cases use the requested ev_xx value.
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

fit_lasso <- function(x, y, nfolds = 5, nlambda = 50, seed = 1) {
  set.seed(seed)
  foldid <- sample(rep(seq_len(nfolds), length.out = nrow(x)))
  fit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    foldid = foldid,
    nfolds = nfolds,
    nlambda = nlambda
  )

  # Keep both common CV choices for comparison.
  extract_fit <- function(lambda_name) {
    beta_hat <- as.matrix(stats::coef(fit, s = lambda_name))[-1, 1]
    list(
      selected = which(abs(beta_hat) > 1e-8),
      lambda = fit[[lambda_name]]
    )
  }

  list(
    cv_lasso_min = extract_fit("lambda.min"),
    cv_lasso_1se = extract_fit("lambda.1se")
  )
}

fit_sharp_lasso <- function(x,
                            y,
                            repetitions = 100,
                            subsample_fraction = 0.5,
                            nlambda = 50,
                            n_cat_values = list(NULL, 3),
                            n_cores = 1,
                            seed = 1) {
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }

  fits <- lapply(seq_along(n_cat_values), function(i) {
    n_cat <- n_cat_values[[i]]
    algorithm_name <- if (is.null(n_cat)) "ncat_null" else paste0("ncat_", n_cat)

    # Stability selection is handled by sharp rather than custom subsampling code.
    stability <- sharp::VariableSelection(
      xdata = x,
      ydata = y,
      family = "gaussian",
      K = repetitions,
      tau = subsample_fraction,
      Lambda_cardinal = nlambda,
      seed = seed,
      n_cat = n_cat,
      n_cores = n_cores,
      standardize = FALSE,
      verbose = FALSE
    )
    selected_raw <- sharp::SelectedVariables(stability)

    # SelectedVariables can return names, indices, or a selection vector.
    selected_vector <- as.vector(selected_raw)
    if (is.logical(selected_vector) && length(selected_vector) == ncol(x)) {
      selected <- which(selected_vector)
    } else if (
      is.numeric(selected_vector) &&
        length(selected_vector) == ncol(x) &&
        all(stats::na.omit(selected_vector) %in% c(0, 1))
    ) {
      selected <- which(selected_vector != 0)
    } else if (is.numeric(selected_vector)) {
      selected <- as.integer(selected_vector)
    } else {
      selected <- match(as.character(selected_vector), colnames(x))
    }
    selected <- sort(unique(selected[!is.na(selected) & selected >= 1 & selected <= ncol(x)]))

    fit <- list(
      selected = selected,
      lambda = NA_real_,
      n_cat = if (is.null(n_cat)) NA_real_ else n_cat,
      stability_repetitions = repetitions,
      stability_subsample_fraction = subsample_fraction,
      max_selection_count = NA_real_,
      mean_selection_count = NA_real_
    )
    stats::setNames(list(fit), algorithm_name)
  })

  do.call(c, fits)
}

simulate_one_dataset <- function(seed,
                                 n = 1000,
                                 pk = 100,
                                 binary_fraction = 0.5,
                                 binary_top_fraction = 0.2,
                                 pk_imbalance_fraction = 0.2,
                                 active_predictors = 10,
                                 ev_xy = 0.7,
                                 ev_xx = 0.4) {
  set.seed(seed)
  x_data <- simulate_predictor_matrix(n = n, pk = pk, ev_xx = ev_xx, seed = seed + 900)
  binary_columns <- select_binary_columns(p = pk, binary_fraction = binary_fraction, seed = seed + 1000)

  # Choose which binary predictors are rare before generating y.
  set.seed(seed + 1100)
  imbalanced_columns <- sample_fraction(binary_columns, pk_imbalance_fraction)
  balanced_columns <- setdiff(binary_columns, imbalanced_columns)

  x_final <- make_predictors_binary(
    X = x_data,
    binary_columns = binary_columns,
    imbalanced_columns = imbalanced_columns,
    balanced_columns = balanced_columns,
    binary_top_fraction = binary_top_fraction
  )
  continuous_columns <- setdiff(seq_len(pk), binary_columns)

  # Active predictors are fixed through theta, not chosen by nu_xy.
  active_design <- choose_active_columns(
    binary_columns = binary_columns,
    imbalanced_columns = imbalanced_columns,
    balanced_columns = balanced_columns,
    continuous_columns = continuous_columns,
    active_predictors = active_predictors,
    binary_fraction = binary_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    seed = seed + 1200
  )

  # Use beta_abs = 1 and beta_sign = 1
  truth_sim <- fake::SimulateRegression(
    xdata = x_final,
    family = "gaussian",
    q = 1,
    theta = active_design$theta,
    beta_abs = 1,
    beta_sign = 1,
    continuous = FALSE,
    ev_xy = ev_xy
  )
  ydata <- truth_sim$ydata
  y <- if (is.null(dim(ydata))) {
    as.numeric(ydata)
  } else {
    as.numeric(ydata[, 1])
  }
  active <- active_design$active_columns
  beta <- as.numeric(truth_sim$beta[, 1])

  list(
    x = as.matrix(x_final),
    y = y,
    active = active,
    beta = beta,
    signal_r_squared = signal_r_squared(x = x_final, beta = beta, y = y),
    binary_columns = binary_columns,
    imbalanced_columns = imbalanced_columns,
    balanced_columns = balanced_columns,
    active_imbalanced_columns = intersect(active, imbalanced_columns),
    noise_imbalanced_columns = setdiff(imbalanced_columns, active),
    theta = active_design$theta
  )
}

evaluate_one_scaling <- function(dat,
                                 seed,
                                 scaling_method,
                                 binary_fraction,
                                 binary_top_fraction,
                                 pk_imbalance_fraction,
                                 active_predictors,
                                 ev_xy,
                                 ev_xx,
                                 nfolds = 5,
                                 nlambda = 50,
                                 include_stability = TRUE,
                                 stability_repetitions = 100,
                                 stability_subsample_fraction = 0.5,
                                 sharp_n_cat_values = list(NULL, 3),
                                 n_cores = 1,
                                 collect_predictor_metadata = TRUE) {
  # Fit all methods to one scaled version of the same simulated dataset.
  x_scaled <- scale_matrix(dat$x, method = scaling_method, binary_columns = dat$binary_columns)

  elapsed <- system.time({
    cv_fits <- fit_lasso(x = x_scaled, y = dat$y, nfolds = nfolds, nlambda = nlambda, seed = seed + 3000)
    stability_fits <- if (include_stability) {
      fit_sharp_lasso(
        x = x_scaled,
        y = dat$y,
        repetitions = stability_repetitions,
        subsample_fraction = stability_subsample_fraction,
        nlambda = nlambda,
        n_cat_values = sharp_n_cat_values,
        n_cores = n_cores,
        seed = seed + 4000
      )
    } else {
      list()
    }
    fits <- c(cv_fits, stability_fits)
  })

  # One result row per algorithm.
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
        target_active_predictors = active_predictors,
        ev_xy = ev_xy,
        ev_xx = ev_xx,
        binary_predictors = length(dat$binary_columns),
        imbalanced_predictors = length(dat$imbalanced_columns),
        balanced_predictors = length(dat$balanced_columns),
        continuous_predictors = ncol(dat$x) - length(dat$binary_columns),
        lambda = fit$lambda,
        ncut = NA_real_,
        n_cat = if (!is.null(fit$n_cat)) fit$n_cat else NA_real_,
        stability_repetitions = if (!is.null(fit$stability_repetitions)) fit$stability_repetitions else NA_real_,
        stability_subsample_fraction = if (!is.null(fit$stability_subsample_fraction)) {
          fit$stability_subsample_fraction
        } else {
          NA_real_
        },
        max_selection_count = if (!is.null(fit$max_selection_count)) fit$max_selection_count else NA_real_,
        mean_selection_count = if (!is.null(fit$mean_selection_count)) fit$mean_selection_count else NA_real_,
        elapsed_seconds = unname(elapsed[["elapsed"]]),
        signal_r_squared = dat$signal_r_squared
      ),
      predictor_imbalance_composition(
        active = dat$active,
        p = ncol(dat$x),
        binary_columns = dat$binary_columns,
        imbalanced_columns = dat$imbalanced_columns,
        balanced_columns = dat$balanced_columns,
        target_rare_fraction = pk_imbalance_fraction
      ),
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

  # Optional row-level metadata for subgroup checks.
  metadata_rows <- if (collect_predictor_metadata) {
    lapply(names(fits), function(algorithm_name) {
      predictor_selection_metadata(
        dat = dat,
        selected = fits[[algorithm_name]]$selected,
        seed = seed,
        algorithm = algorithm_name,
        scaling_method = scaling_method,
        binary_fraction = binary_fraction,
        binary_top_fraction = binary_top_fraction,
        pk_imbalance_fraction = pk_imbalance_fraction,
        active_predictors = active_predictors,
        ev_xy = ev_xy,
        ev_xx = ev_xx
      )
    })
  } else {
    list()
  }

  list(
    results = do.call(rbind, rows),
    predictor_metadata = if (collect_predictor_metadata) do.call(rbind, metadata_rows) else NULL
  )
}

run_one_imbalance_scenario <- function(seed,
                                       scaling_methods,
                                       n,
                                       pk,
                                       binary_fraction,
                                       binary_top_fraction,
                                       pk_imbalance_fraction,
                                       active_predictors = 10,
                                       ev_xy = 0.7,
                                       ev_xx = 0.4,
                                       nfolds = 5,
                                       nlambda = 50,
                                       include_stability = TRUE,
                                       stability_repetitions = 100,
                                       stability_subsample_fraction = 0.5,
                                       sharp_n_cat_values = list(NULL, 3),
                                       n_cores = 1,
                                       collect_predictor_metadata = TRUE) {
  # Generate the dataset once, then evaluate every scaling method on it.
  dat <- simulate_one_dataset(
    seed = seed,
    n = n,
    pk = pk,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    pk_imbalance_fraction = pk_imbalance_fraction,
    active_predictors = active_predictors,
    ev_xy = ev_xy,
    ev_xx = ev_xx
  )

  results <- vector("list", length(scaling_methods))
  for (i in seq_along(scaling_methods)) {
    results[[i]] <- evaluate_one_scaling(
      dat = dat,
      seed = seed,
      scaling_method = scaling_methods[i],
      binary_fraction = binary_fraction,
      binary_top_fraction = binary_top_fraction,
      pk_imbalance_fraction = pk_imbalance_fraction,
      active_predictors = active_predictors,
      ev_xy = ev_xy,
      ev_xx = ev_xx,
      nfolds = nfolds,
      nlambda = nlambda,
      include_stability = include_stability,
      stability_repetitions = stability_repetitions,
      stability_subsample_fraction = stability_subsample_fraction,
      sharp_n_cat_values = sharp_n_cat_values,
      n_cores = n_cores,
      collect_predictor_metadata = collect_predictor_metadata
    )
  }

  list(
    results = do.call(rbind, lapply(results, `[[`, "results")),
    predictor_metadata = if (collect_predictor_metadata) {
      do.call(rbind, lapply(results, `[[`, "predictor_metadata"))
    } else {
      NULL
    }
  )
}

run_imbalance_benchmark <- function(
    seeds = 1:10,
    scaling_methods = c("cont", "zscore", "2sd"),
    dimension_scenarios = data.frame(n = 1000, pk = 100),
    binary_fraction_values = c(0.5),
    binary_top_fractions = c(0.5, 0.2, 0.1, 0.05),
    pk_imbalance_fractions = c(0.2),
    active_predictors = 10,
    ev_xy_values = c(0.5, 0.2, 0.05),
    ev_xx_values = c(0, 0.1, 0.5, 0.9),
    nfolds = 5,
    nlambda = 50,
    include_stability = TRUE,
    stability_repetitions = 100,
    stability_subsample_fraction = 0.5,
    sharp_n_cat_values = list(NULL, 3),
    n_cores = default_n_cores(),
    collect_predictor_metadata = TRUE) {
  # Balanced baseline plus the rare-category scenarios
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
    2 + if (include_stability) length(sharp_n_cat_values) else 0,
    " algorithms = ",
    nrow(grid) * length(scaling_methods) * (2 + if (include_stability) length(sharp_n_cat_values) else 0),
    " result rows."
  )

  results <- vector("list", nrow(grid))
  predictor_metadata <- if (collect_predictor_metadata) vector("list", nrow(grid)) else NULL

  # Run each generated dataset/scenario combination
  for (i in seq_len(nrow(grid))) {
    message("Running imbalance scenario ", i, " of ", nrow(grid))
    scenario_result <- run_one_imbalance_scenario(
      seed = grid$seed[i],
      scaling_methods = scaling_methods,
      n = grid$n[i],
      pk = grid$pk[i],
      binary_fraction = grid$binary_fraction[i],
      binary_top_fraction = grid$binary_top_fraction[i],
      pk_imbalance_fraction = grid$pk_imbalance_fraction[i],
      active_predictors = active_predictors,
      ev_xy = grid$ev_xy[i],
      ev_xx = grid$ev_xx[i],
      nfolds = nfolds,
      nlambda = nlambda,
      include_stability = include_stability,
      stability_repetitions = stability_repetitions,
      stability_subsample_fraction = stability_subsample_fraction,
      sharp_n_cat_values = sharp_n_cat_values,
      n_cores = n_cores,
      collect_predictor_metadata = collect_predictor_metadata
    )
    results[[i]] <- scenario_result$results
    if (collect_predictor_metadata) {
      predictor_metadata[[i]] <- scenario_result$predictor_metadata
    }
  }

  list(
    results = do.call(rbind, results),
    predictor_metadata = if (collect_predictor_metadata) do.call(rbind, predictor_metadata) else NULL
  )
}

summarise_imbalance_results <- function(imbalance_results) {
  # Average over seeds for the compact summary csv
  aggregate(
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
      signal_r_squared,
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
        pk_imbalance_fraction + target_active_predictors + ev_xy + ev_xx,
    data = imbalance_results,
    na.action = na.pass,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
}

write_imbalance_outputs <- function(imbalance_run, output_prefix, write_metadata = TRUE) {
  # Write full results, summary results, and optional predictor-level metadata.
  if (!dir.exists("results")) {
    dir.create("results")
  }

  imbalance_results <- imbalance_run$results
  imbalance_summary <- summarise_imbalance_results(imbalance_results)

  print(head(imbalance_results))
  print(imbalance_summary)

  utils::write.csv(
    imbalance_results,
    file = file.path("results", paste0(output_prefix, "_results.csv")),
    row.names = FALSE
  )

  utils::write.csv(
    imbalance_summary,
    file = file.path("results", paste0(output_prefix, "_summary.csv")),
    row.names = FALSE
  )

  if (write_metadata && !is.null(imbalance_run$predictor_metadata)) {
    utils::write.csv(
      imbalance_run$predictor_metadata,
      file = file.path("results", paste0(output_prefix, "_metadata.csv")),
      row.names = FALSE
    )
  }
}

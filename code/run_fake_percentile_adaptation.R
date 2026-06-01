# Prototype adaptation using the fake package plus percentile binary conversion.
#
# This script keeps fake as the simulation framework:
# 1. fake::SimulateGraphical() generates correlated continuous predictors.
# 2. Selected predictor columns are converted to binary using percentile cutoffs.
# 3. fake::SimulateRegression(xdata = modified_X) generates y, theta, and beta.
# 4. LASSO is fitted and selected variables are compared with fake's theta.

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

selection_metrics <- function(selected, active, p) {
  selected <- sort(unique(selected))
  active <- sort(unique(active))

  tp <- length(intersect(selected, active))
  fp <- length(setdiff(selected, active))
  fn <- length(setdiff(active, selected))
  tn <- p - tp - fp - fn

  data.frame(
    selected_n = length(selected),
    active_n = length(active),
    true_positive = tp,
    false_positive = fp,
    false_negative = fn,
    true_negative = tn,
    sensitivity = tp / max(length(active), 1),
    false_discovery_rate = fp / max(length(selected), 1),
    specificity = tn / max(p - length(active), 1)
  )
}

run_fake_percentile_example <- function(seed = 1,
                                        binary_fraction = 0.5,
                                        binary_top_fraction = 0.2,
                                        nu_xy = 0.2,
                                        ev_xy = 0.7) {
  set.seed(seed)

  x_graph <- fake::SimulateGraphical(
    pk = rep(10, 5),
    nu_within = 0.8,
    nu_between = 0,
    v_sign = -1
  )

  modified <- make_some_predictors_binary(
    X = x_graph$data,
    binary_fraction = binary_fraction,
    binary_top_fraction = binary_top_fraction,
    seed = seed + 100
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

  cv_fit <- glmnet::cv.glmnet(
    x = as.matrix(sim$xdata),
    y = as.numeric(sim$ydata[, 1]),
    alpha = 1,
    family = "gaussian",
    standardize = TRUE
  )

  beta_hat <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))[-1, 1]
  selected <- which(abs(beta_hat) > 1e-8)
  active <- which(sim$theta[, 1] != 0)

  cbind(
    data.frame(
      n = nrow(sim$xdata),
      pk = ncol(sim$xdata),
      binary_fraction = binary_fraction,
      binary_top_fraction = binary_top_fraction,
      nu_xy = nu_xy,
      ev_xy = ev_xy,
      binary_predictors = length(modified$binary_columns)
    ),
    selection_metrics(selected = selected, active = active, p = ncol(sim$xdata))
  )
}

example_result <- run_fake_percentile_example()
print(example_result)

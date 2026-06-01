# Direct familiarisation script for fake::SimulateRegression.
#
# This script uses the requested fake package function directly. It is separate
# from the prototype scripts so the package workflow is easy to explain in a
# meeting.

required_packages <- c("fake")
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

set.seed(1)

# Example 1: direct SimulateRegression call.
# n = sample size, pk = number of predictors, q = number of outcomes,
# nu_xy = expected proportion of predictors associated with the outcome,
# ev_xy = expected proportion of outcome variance explained by predictors.
sim_direct <- fake::SimulateRegression(
  n = 200,
  pk = 50,
  family = "gaussian",
  q = 1,
  nu_xy = 0.2,
  beta_abs = c(0.5, 1),
  beta_sign = c(-1, 1),
  continuous = TRUE,
  ev_xy = 0.7
)

# Example 2: generate correlated predictors first, then pass them into
# SimulateRegression as xdata.
set.seed(2)
x_graph <- fake::SimulateGraphical(
  pk = rep(10, 5),
  nu_within = 0.8,
  nu_between = 0,
  v_sign = -1
)

sim_graph <- fake::SimulateRegression(
  xdata = x_graph$data,
  family = "gaussian",
  q = 1,
  nu_xy = 0.2,
  beta_abs = c(0.5, 1),
  beta_sign = c(-1, 1),
  continuous = TRUE,
  ev_xy = 0.7
)

summarise_fake_simulation <- function(sim, label) {
  xdata <- as.data.frame(sim$xdata)
  ydata <- as.data.frame(sim$ydata)
  theta <- as.matrix(sim$theta)
  beta <- as.matrix(sim$beta)

  data.frame(
    simulation = label,
    n = nrow(xdata),
    pk = ncol(xdata),
    q = ncol(ydata),
    active_predictors = sum(theta[, 1] != 0),
    active_fraction = mean(theta[, 1] != 0),
    nonzero_beta = sum(beta[, 1] != 0),
    y_variance = stats::var(ydata[[1]])
  )
}

fake_summary <- rbind(
  summarise_fake_simulation(sim_direct, "direct_independent_predictors"),
  summarise_fake_simulation(sim_graph, "graph_correlated_predictors")
)

print(fake_summary)

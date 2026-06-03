# Plot toy benchmark results.
#
# This script reads the benchmark summary table and saves simple plots to
# the plots/ folder. Run after code/run_fake_toy_benchmark.R has created:
# results/fake_toy_benchmark_summary.csv

required_packages <- c("ggplot2")
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

summary_path <- file.path("results", "fake_toy_benchmark_summary.csv")
results_path <- file.path("results", "fake_toy_benchmark_results.csv")

if (!file.exists(summary_path)) {
  stop(
    "Cannot find ", summary_path, ".\n",
    "Run source(\"code/run_fake_toy_benchmark.R\") first."
  )
}

if (!file.exists(results_path)) {
  stop(
    "Cannot find ", results_path, ".\n",
    "Run source(\"code/run_fake_toy_benchmark.R\") first."
  )
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

summary_results <- utils::read.csv(summary_path)
benchmark_results <- utils::read.csv(results_path)

summary_results$scaling_method <- factor(
  summary_results$scaling_method,
  levels = c("none", "zscore", "2sd")
)

summary_results$ev_xy <- factor(
  summary_results$ev_xy,
  levels = sort(unique(summary_results$ev_xy))
)

summary_results$ev_xx <- factor(
  summary_results$ev_xx,
  levels = sort(unique(summary_results$ev_xx))
)

summary_results$binary_top_fraction <- factor(
  summary_results$binary_top_fraction,
  levels = sort(unique(summary_results$binary_top_fraction))
)

summary_results$ev_xy_label <- factor(
  paste0(
    "ev_xy = ",
    summary_results$ev_xy,
    ifelse(
      summary_results$ev_xy == "0.3",
      " (noisy)",
      ifelse(summary_results$ev_xy == "0.5", " (medium)", " (strong signal)")
    )
  ),
  levels = c(
    "ev_xy = 0.3 (noisy)",
    "ev_xy = 0.5 (medium)",
    "ev_xy = 0.7 (strong signal)"
  )
)

benchmark_results$ev_xy_label <- factor(
  paste0(
    "ev_xy = ",
    benchmark_results$ev_xy,
    ifelse(
      benchmark_results$ev_xy == 0.3,
      " (noisy)",
      ifelse(benchmark_results$ev_xy == 0.5, " (medium)", " (strong signal)")
    )
  ),
  levels = c(
    "ev_xy = 0.3 (noisy)",
    "ev_xy = 0.5 (medium)",
    "ev_xy = 0.7 (strong signal)"
  )
)

save_plot <- function(plot, filename, width = 8, height = 5) {
  ggplot2::ggsave(
    filename = file.path("plots", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

precision_by_correlation <- ggplot2::ggplot(
  summary_results,
  ggplot2::aes(
    x = ev_xx,
    y = precision,
    colour = scaling_method,
    group = scaling_method
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "Precision by Correlation and Signal Strength",
    subtitle = "Panels show signal strength (ev_xy); x-axis shows predictor correlation (ev_xx)",
    x = "Predictor correlation strength (ev_xx)",
    y = "Precision",
    colour = "Scaling"
  ) +
  ggplot2::theme_minimal()

recall_by_correlation <- ggplot2::ggplot(
  summary_results,
  ggplot2::aes(
    x = ev_xx,
    y = recall,
    colour = scaling_method,
    group = scaling_method
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "Recall by Correlation and Signal Strength",
    subtitle = "Panels show signal strength (ev_xy); x-axis shows predictor correlation (ev_xx)",
    x = "Predictor correlation strength (ev_xx)",
    y = "Recall",
    colour = "Scaling"
  ) +
  ggplot2::theme_minimal()

f1_by_scaling <- ggplot2::ggplot(
  summary_results,
  ggplot2::aes(x = scaling_method, y = f1_score, fill = scaling_method)
) +
  ggplot2::geom_boxplot(alpha = 0.75, width = 0.6) +
  ggplot2::labs(
    title = "F1 Score by Scaling Method",
    subtitle = "F1 balances precision and recall",
    x = "Scaling method",
    y = "F1 score",
    fill = "Scaling"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

r2_by_signal <- ggplot2::ggplot(
  summary_results,
  ggplot2::aes(
    x = ev_xy,
    y = r_squared,
    colour = scaling_method,
    group = scaling_method
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(
    title = "Prediction Performance by Signal Strength",
    subtitle = "Higher ev_xy means more of y is explained by X",
    x = "Outcome signal strength (ev_xy)",
    y = "R-squared",
    colour = "Scaling"
  ) +
  ggplot2::theme_minimal()

precision_recall_tradeoff <- ggplot2::ggplot(
  summary_results,
  ggplot2::aes(
    x = recall,
    y = precision,
    colour = ev_xx,
    shape = scaling_method
  )
) +
  ggplot2::geom_point(size = 2.4, alpha = 0.8) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "Precision and Recall Trade-off",
    subtitle = "Panels show signal strength (ev_xy); each point is one averaged scenario",
    x = "Recall",
    y = "Precision",
    colour = "ev_xx",
    shape = "Scaling"
  ) +
  ggplot2::theme_minimal()

split_summary <- stats::aggregate(
  cbind(precision, recall, f1_score) ~ binary_top_fraction + ev_xx + ev_xy_label,
  data = summary_results,
  FUN = mean
)

precision_by_split <- ggplot2::ggplot(
  split_summary,
  ggplot2::aes(
    x = binary_top_fraction,
    y = precision,
    colour = ev_xx,
    group = ev_xx
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "Precision by Binary Split Imbalance",
    subtitle = "Averaged over scaling methods; panels show signal strength (ev_xy)",
    x = "Binary top fraction",
    y = "Precision",
    colour = "ev_xx"
  ) +
  ggplot2::theme_minimal()

recall_by_split <- ggplot2::ggplot(
  split_summary,
  ggplot2::aes(
    x = binary_top_fraction,
    y = recall,
    colour = ev_xx,
    group = ev_xx
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "Recall by Binary Split Imbalance",
    subtitle = "Averaged over scaling methods; panels show signal strength (ev_xy)",
    x = "Binary top fraction",
    y = "Recall",
    colour = "ev_xx"
  ) +
  ggplot2::theme_minimal()

f1_by_split <- ggplot2::ggplot(
  split_summary,
  ggplot2::aes(
    x = binary_top_fraction,
    y = f1_score,
    colour = ev_xx,
    group = ev_xx
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ ev_xy_label) +
  ggplot2::labs(
    title = "F1 Score by Binary Split Imbalance",
    subtitle = "Averaged over scaling methods; panels show signal strength (ev_xy)",
    x = "Binary top fraction",
    y = "F1 score",
    colour = "ev_xx"
  ) +
  ggplot2::theme_minimal()

make_metric_band_data <- function(data, metric_name) {
  formula <- stats::as.formula(
    paste(metric_name, "~ binary_top_fraction + ev_xx + ev_xy + ev_xy_label")
  )

  aggregate_result <- stats::aggregate(
    formula,
    data = data,
    FUN = function(x) {
      n <- length(x)
      mean_x <- mean(x)
      se_x <- stats::sd(x) / sqrt(n)
      c(
        mean = mean_x,
        lower = max(0, mean_x - 1.96 * se_x),
        upper = min(1, mean_x + 1.96 * se_x)
      )
    }
  )

  metric_values <- do.call(rbind, aggregate_result[[metric_name]])
  aggregate_result[[metric_name]] <- NULL
  data.frame(
    aggregate_result,
    metric = metric_name,
    mean = metric_values[, "mean"],
    lower = metric_values[, "lower"],
    upper = metric_values[, "upper"]
  )
}

metric_band_data <- rbind(
  make_metric_band_data(benchmark_results, "f1_score"),
  make_metric_band_data(benchmark_results, "recall"),
  make_metric_band_data(benchmark_results, "precision")
)

metric_band_data$metric <- factor(
  metric_band_data$metric,
  levels = c("f1_score", "recall", "precision"),
  labels = c("F1 score", "Recall", "Precision")
)

make_noise_metric_data <- function(data, metric_name) {
  formula <- stats::as.formula(
    paste(metric_name, "~ ev_xy + scaling_method")
  )

  aggregate_result <- stats::aggregate(
    formula,
    data = data,
    FUN = function(x) {
      n <- length(x)
      mean_x <- mean(x)
      se_x <- stats::sd(x) / sqrt(n)
      c(
        mean = mean_x,
        lower = max(0, mean_x - 1.96 * se_x),
        upper = min(1, mean_x + 1.96 * se_x)
      )
    }
  )

  metric_values <- do.call(rbind, aggregate_result[[metric_name]])
  aggregate_result[[metric_name]] <- NULL
  data.frame(
    aggregate_result,
    metric = metric_name,
    mean = metric_values[, "mean"],
    lower = metric_values[, "lower"],
    upper = metric_values[, "upper"]
  )
}

make_scaling_correlation_metric_data <- function(data, metric_name) {
  formula <- stats::as.formula(
    paste(metric_name, "~ ev_xy + ev_xx + scaling_method")
  )

  aggregate_result <- stats::aggregate(
    formula,
    data = data,
    FUN = function(x) {
      n <- length(x)
      mean_x <- mean(x)
      se_x <- stats::sd(x) / sqrt(n)
      c(
        mean = mean_x,
        lower = max(0, mean_x - 1.96 * se_x),
        upper = min(1, mean_x + 1.96 * se_x)
      )
    }
  )

  metric_values <- do.call(rbind, aggregate_result[[metric_name]])
  aggregate_result[[metric_name]] <- NULL
  data.frame(
    aggregate_result,
    metric = metric_name,
    mean = metric_values[, "mean"],
    lower = metric_values[, "lower"],
    upper = metric_values[, "upper"]
  )
}

noise_metric_data <- rbind(
  make_noise_metric_data(benchmark_results, "f1_score"),
  make_noise_metric_data(benchmark_results, "recall"),
  make_noise_metric_data(benchmark_results, "precision")
)

noise_metric_data$metric <- factor(
  noise_metric_data$metric,
  levels = c("f1_score", "recall", "precision"),
  labels = c("F1 score", "Recall", "Precision")
)

noise_metric_data$scaling_method <- factor(
  noise_metric_data$scaling_method,
  levels = c("none", "zscore", "2sd")
)

scaling_correlation_metric_data <- rbind(
  make_scaling_correlation_metric_data(benchmark_results, "f1_score"),
  make_scaling_correlation_metric_data(benchmark_results, "recall"),
  make_scaling_correlation_metric_data(benchmark_results, "precision")
)

scaling_correlation_metric_data$metric <- factor(
  scaling_correlation_metric_data$metric,
  levels = c("f1_score", "recall", "precision"),
  labels = c("F1 score", "Recall", "Precision")
)

scaling_correlation_metric_data$scaling_method <- factor(
  scaling_correlation_metric_data$scaling_method,
  levels = c("none", "zscore", "2sd")
)

noise_analysis_by_scaling <- ggplot2::ggplot(
  noise_metric_data,
  ggplot2::aes(
    x = ev_xy,
    y = mean,
    colour = scaling_method,
    fill = scaling_method,
    group = scaling_method
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    alpha = 0.18,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ metric, nrow = 1) +
  ggplot2::labs(
    title = "Noise Analysis by Scaling Method",
    subtitle = "x-axis shows outcome signal strength (ev_xy); lower ev_xy means noisier outcomes",
    x = "Outcome signal strength (ev_xy)",
    y = "Performance",
    colour = "Scaling",
    fill = "Scaling"
  ) +
  ggplot2::theme_minimal()

scaling_metric_data <- rbind(
  data.frame(
    scaling_method = benchmark_results$scaling_method,
    metric = "F1 score",
    performance = benchmark_results$f1_score
  ),
  data.frame(
    scaling_method = benchmark_results$scaling_method,
    metric = "Recall",
    performance = benchmark_results$recall
  ),
  data.frame(
    scaling_method = benchmark_results$scaling_method,
    metric = "Precision",
    performance = benchmark_results$precision
  )
)

scaling_metric_data$scaling_method <- factor(
  scaling_metric_data$scaling_method,
  levels = c("none", "zscore", "2sd")
)

scaling_metric_data$metric <- factor(
  scaling_metric_data$metric,
  levels = c("F1 score", "Recall", "Precision")
)

scaling_analysis <- ggplot2::ggplot(
  scaling_metric_data,
  ggplot2::aes(
    x = scaling_method,
    y = performance,
    fill = scaling_method
  )
) +
  ggplot2::geom_boxplot(alpha = 0.75, width = 0.6) +
  ggplot2::facet_wrap(~ metric, nrow = 1) +
  ggplot2::labs(
    title = "Scaling Analysis",
    subtitle = "Distribution across noise, correlation, split settings, and seeds",
    x = "Scaling method",
    y = "Performance",
    fill = "Scaling"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

split_metric_bands <- ggplot2::ggplot(
  metric_band_data,
  ggplot2::aes(
    x = binary_top_fraction,
    y = mean,
    colour = factor(ev_xx),
    fill = factor(ev_xx),
    group = ev_xx
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    alpha = 0.18,
    colour = NA
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_grid(ev_xy_label ~ metric) +
  ggplot2::labs(
    title = "Binary Split Imbalance Analysis",
    subtitle = "Lines show mean performance; shaded bands show 95% confidence intervals",
    x = "Binary top fraction",
    y = "Performance",
    colour = "ev_xx",
    fill = "ev_xx"
  ) +
  ggplot2::theme_minimal()

make_single_signal_plot <- function(ev_xy_level, filename) {
  plot_data <- metric_band_data[metric_band_data$ev_xy == ev_xy_level, ]
  ev_xy_text <- unique(as.character(plot_data$ev_xy_label))

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = binary_top_fraction,
      y = mean,
      colour = factor(ev_xx),
      fill = factor(ev_xx),
      group = ev_xx
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.18,
      colour = NA
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ metric, nrow = 1) +
    ggplot2::labs(
      title = paste("Binary Split Imbalance Analysis -", ev_xy_text),
      subtitle = "Lines show mean performance; shaded bands show 95% confidence intervals",
      x = "Binary top fraction",
      y = "Performance",
      colour = "ev_xx",
      fill = "ev_xx"
    ) +
    ggplot2::theme_minimal()

  save_plot(plot, filename, width = 11, height = 4)
}

make_scaling_by_correlation_plot <- function(ev_xy_level, filename) {
  plot_data <- scaling_correlation_metric_data[
    scaling_correlation_metric_data$ev_xy == ev_xy_level,
  ]
  ev_xy_text <- unique(as.character(
    benchmark_results$ev_xy_label[benchmark_results$ev_xy == ev_xy_level]
  ))

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = ev_xx,
      y = mean,
      colour = scaling_method,
      fill = scaling_method,
      group = scaling_method
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.18,
      colour = NA
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ metric, nrow = 1) +
    ggplot2::labs(
      title = paste("Scaling Analysis by Correlation -", ev_xy_text),
      subtitle = "Averaged over binary split settings; shaded bands show 95% confidence intervals",
      x = "Predictor correlation strength (ev_xx)",
      y = "Performance",
      colour = "Scaling",
      fill = "Scaling"
    ) +
    ggplot2::theme_minimal()

  save_plot(plot, filename, width = 11, height = 4)
}

save_plot(precision_by_correlation, "precision_by_correlation.png")
save_plot(recall_by_correlation, "recall_by_correlation.png")
save_plot(f1_by_scaling, "f1_by_scaling.png")
save_plot(r2_by_signal, "r2_by_signal.png")
save_plot(precision_recall_tradeoff, "precision_recall_tradeoff.png")
save_plot(precision_by_split, "precision_by_split.png")
save_plot(recall_by_split, "recall_by_split.png")
save_plot(f1_by_split, "f1_by_split.png")
save_plot(split_metric_bands, "split_metric_bands.png", width = 11, height = 8)
save_plot(noise_analysis_by_scaling, "noise_analysis_by_scaling.png", width = 11, height = 4)
save_plot(scaling_analysis, "scaling_analysis.png", width = 11, height = 4)
make_single_signal_plot(0.3, "split_metric_bands_evxy_0_3.png")
make_single_signal_plot(0.5, "split_metric_bands_evxy_0_5.png")
make_single_signal_plot(0.7, "split_metric_bands_evxy_0_7.png")
make_scaling_by_correlation_plot(0.3, "scaling_by_correlation_evxy_0_3.png")
make_scaling_by_correlation_plot(0.5, "scaling_by_correlation_evxy_0_5.png")
make_scaling_by_correlation_plot(0.7, "scaling_by_correlation_evxy_0_7.png")

message("Saved plots to the plots/ folder.")

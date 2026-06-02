# Plot toy benchmark results for supervisor meeting.
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

if (!file.exists(summary_path)) {
  stop(
    "Cannot find ", summary_path, ".\n",
    "Run source(\"code/run_fake_toy_benchmark.R\") first."
  )
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

summary_results <- utils::read.csv(summary_path)

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
  ggplot2::facet_wrap(~ ev_xy) +
  ggplot2::labs(
    title = "Precision by Predictor Correlation",
    subtitle = "Higher precision means fewer false positive selections",
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
  ggplot2::facet_wrap(~ ev_xy) +
  ggplot2::labs(
    title = "Recall by Predictor Correlation",
    subtitle = "Higher recall means more true active predictors were recovered",
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
  ggplot2::facet_wrap(~ ev_xy) +
  ggplot2::labs(
    title = "Precision and Recall Trade-off",
    subtitle = "Each point is one averaged scenario",
    x = "Recall",
    y = "Precision",
    colour = "ev_xx",
    shape = "Scaling"
  ) +
  ggplot2::theme_minimal()

save_plot(precision_by_correlation, "precision_by_correlation.png")
save_plot(recall_by_correlation, "recall_by_correlation.png")
save_plot(f1_by_scaling, "f1_by_scaling.png")
save_plot(r2_by_signal, "r2_by_signal.png")
save_plot(precision_recall_tradeoff, "precision_recall_tradeoff.png")

message("Saved plots to the plots/ folder.")

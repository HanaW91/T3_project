# Plots for the scaling-focused benchmark.

results_file <- file.path("results", "scaling_benchmark_results.csv")

if (!file.exists(results_file)) {
  stop(
    "Cannot find ",
    results_file,
    ". Run source(\"code/run_scaling_benchmark.R\") first."
  )
}

raw_results <- utils::read.csv(results_file)

required_columns <- c(
  "seed",
  "scaling_method",
  "binary_fraction",
  "f1_score",
  "precision",
  "recall"
)

missing_columns <- setdiff(required_columns, names(raw_results))

if (length(missing_columns) > 0) {
  stop(
    "The results file is missing these column(s): ",
    paste(missing_columns, collapse = ", ")
  )
}

plot_dir <- file.path("plots", "scaling")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

metric_columns <- c("f1_score", "recall", "precision")
metric_labels <- c("F1 Score", "Recall", "Precision")
scaling_levels <- c("none", "zscore", "2sd")
scaling_colours <- c(none = "grey45", zscore = "#2563eb", `2sd` = "#dc2626")
scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)

summarise_metric <- function(data, metric) {
  split_keys <- interaction(
    data$scaling_method,
    data$binary_fraction,
    drop = TRUE
  )

  pieces <- split(data, split_keys)

  summary_rows <- lapply(pieces, function(piece) {
    values <- piece[[metric]]
    n_values <- length(values)
    mean_value <- mean(values, na.rm = TRUE)
    se_value <- stats::sd(values, na.rm = TRUE) / sqrt(n_values)
    ci_width <- stats::qt(0.975, df = max(n_values - 1, 1)) * se_value

    data.frame(
      scaling_method = piece$scaling_method[1],
      binary_fraction = piece$binary_fraction[1],
      metric = metric,
      mean = mean_value,
      ci_low = max(0, mean_value - ci_width),
      ci_high = min(1, mean_value + ci_width),
      n = n_values
    )
  })

  do.call(rbind, summary_rows)
}

plot_results <- do.call(
  rbind,
  lapply(metric_columns, function(metric) summarise_metric(raw_results, metric))
)

format_binary_fraction_axis <- function(binary_fraction_values) {
  paste0(round(binary_fraction_values * 100), "%")
}

plot_metric_panel <- function(plot_data, metric, metric_label, show_y_label = FALSE) {
  x_values <- sort(unique(plot_data$binary_fraction))
  metric_data <- plot_data[plot_data$metric == metric, ]

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = "Proportion of predictors that are binary (binary_fraction)",
    ylab = if (show_y_label) "Mean performance" else "",
    main = metric_label,
    xaxt = "n"
  )

  graphics::axis(
    1,
    at = x_values,
    labels = format_binary_fraction_axis(x_values)
  )
  graphics::grid(col = "grey88")

  for (scaling_method in scaling_levels) {
    line_data <- metric_data[metric_data$scaling_method == scaling_method, ]

    if (nrow(line_data) == 0) {
      next
    }

    line_data <- line_data[order(line_data$binary_fraction), ]

    graphics::polygon(
      x = c(line_data$binary_fraction, rev(line_data$binary_fraction)),
      y = c(line_data$ci_low, rev(line_data$ci_high)),
      col = grDevices::adjustcolor(scaling_colours[scaling_method], alpha.f = 0.18),
      border = NA
    )

    graphics::lines(
      line_data$binary_fraction,
      line_data$mean,
      col = scaling_colours[scaling_method],
      lwd = 2.2
    )

    graphics::points(
      line_data$binary_fraction,
      line_data$mean,
      col = scaling_colours[scaling_method],
      pch = scaling_symbols[scaling_method],
      cex = 1.1
    )
  }
}

plot_scaling_by_predictor_mix <- function(
    file_name = "scaling_analysis_by_predictor_mix.png") {
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2400,
    height = 1200,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::par(
    mfrow = c(1, 3),
    mar = c(5, 5, 4, 1.5),
    oma = c(0, 0, 4.5, 8)
  )

  for (i in seq_along(metric_columns)) {
    plot_metric_panel(
      plot_data = plot_results,
      metric = metric_columns[i],
      metric_label = metric_labels[i],
      show_y_label = i == 1
    )
  }

  fixed_details <- character(0)

  if ("binary_top_fraction" %in% names(raw_results)) {
    fixed_details <- c(
      fixed_details,
      paste0(
        "rare category fraction = ",
        paste(sort(unique(raw_results$binary_top_fraction)), collapse = ", ")
      )
    )
  }

  if ("pk_imbalance_fraction" %in% names(raw_results)) {
    fixed_details <- c(
      fixed_details,
      paste0(
        "imbalanced predictor fraction = ",
        paste(sort(unique(raw_results$pk_imbalance_fraction)), collapse = ", ")
      )
    )
  }

  graphics::mtext(
    "Scaling Analysis",
    outer = TRUE,
    side = 3,
    line = 2.4,
    cex = 1.3,
    font = 2
  )

  graphics::mtext(
    paste(
      "x-axis shows predictor mix; lines show scaling methods;",
      "shaded bands show 95% confidence intervals across seeds",
      if (length(fixed_details) > 0) {
        paste0("(", paste(fixed_details, collapse = "; "), ")")
      } else {
        ""
      }
    ),
    outer = TRUE,
    side = 3,
    line = 0.7,
    cex = 0.92
  )

  graphics::par(xpd = NA)
  graphics::legend(
    "right",
    inset = c(-0.18, 0),
    legend = scaling_levels,
    title = "Scaling",
    col = scaling_colours[scaling_levels],
    pch = scaling_symbols[scaling_levels],
    lwd = 2.2,
    bty = "n"
  )

  invisible(output_file)
}

created_files <- c(
  plot_scaling_by_predictor_mix()
)

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

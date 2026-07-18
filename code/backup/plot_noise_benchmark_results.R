# Plots for the noise-focused benchmark.

results_file <- file.path("results", "noise_benchmark_results.csv")

if (!file.exists(results_file)) {
  stop(
    "Cannot find ",
    results_file,
    ". Run source(\"code/run_noise_benchmark.R\") first."
  )
}

raw_results <- utils::read.csv(results_file)

required_columns <- c(
  "seed",
  "scaling_method",
  "binary_fraction",
  "ev_xy",
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

plot_dir <- file.path("plots", "noise")

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
    data$ev_xy,
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
      ev_xy = piece$ev_xy[1],
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

format_binary_fraction <- function(binary_fraction) {
  if (binary_fraction == 1) {
    return("All binary predictors")
  }

  paste0(
    round(binary_fraction * 100),
    "% binary / ",
    round((1 - binary_fraction) * 100),
    "% continuous predictors"
  )
}

plot_metric_panel <- function(plot_data, metric, metric_label, show_y_label = FALSE) {
  x_values <- sort(unique(plot_data$ev_xy))
  metric_data <- plot_data[plot_data$metric == metric, ]

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = "Explained variance / signal strength (ev_xy)",
    ylab = if (show_y_label) "Mean performance" else "",
    main = metric_label,
    xaxt = "n"
  )

  graphics::axis(1, at = x_values, labels = x_values)
  graphics::grid(col = "grey88")

  for (scaling_method in scaling_levels) {
    line_data <- metric_data[metric_data$scaling_method == scaling_method, ]

    if (nrow(line_data) == 0) {
      next
    }

    line_data <- line_data[order(line_data$ev_xy), ]

    graphics::polygon(
      x = c(line_data$ev_xy, rev(line_data$ev_xy)),
      y = c(line_data$ci_low, rev(line_data$ci_high)),
      col = grDevices::adjustcolor(scaling_colours[scaling_method], alpha.f = 0.18),
      border = NA
    )

    graphics::lines(
      line_data$ev_xy,
      line_data$mean,
      col = scaling_colours[scaling_method],
      lwd = 2.2
    )

    graphics::points(
      line_data$ev_xy,
      line_data$mean,
      col = scaling_colours[scaling_method],
      pch = scaling_symbols[scaling_method],
      cex = 1.1
    )
  }
}

plot_noise_for_binary_fraction <- function(binary_fraction, file_name) {
  plot_data <- plot_results[plot_results$binary_fraction == binary_fraction, ]

  if (nrow(plot_data) == 0) {
    warning("No results found for binary_fraction = ", binary_fraction)
    return(invisible(NULL))
  }

  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2400,
    height = 1350,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::layout(
    matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE),
    heights = c(1, 0.22)
  )

  graphics::par(
    mar = c(5, 5, 4, 1.5),
    oma = c(0, 0, 4.5, 0)
  )

  for (i in seq_along(metric_columns)) {
    plot_metric_panel(
      plot_data = plot_data,
      metric = metric_columns[i],
      metric_label = metric_labels[i],
      show_y_label = i == 1
    )
  }

  graphics::mtext(
    paste0(
      "Noise Analysis - ",
      format_binary_fraction(binary_fraction)
    ),
    outer = TRUE,
    side = 3,
    line = 2.4,
    cex = 1.25,
    font = 2
  )

  graphics::mtext(
    "Lines show mean performance; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 0.7,
    cex = 1
  )

  graphics::par(mar = c(0, 0, 0, 0), xpd = NA)
  graphics::plot.new()
  graphics::legend(
    "center",
    legend = scaling_levels,
    title = "Scaling",
    col = scaling_colours[scaling_levels],
    pch = scaling_symbols[scaling_levels],
    lwd = 2.8,
    bty = "n",
    ncol = length(scaling_levels),
    cex = 1.25,
    title.cex = 1.25
  )

  invisible(output_file)
}

plot_combined_noise <- function(file_name = "noise_analysis_combined.png") {
  binary_fractions <- sort(unique(plot_results$binary_fraction), decreasing = TRUE)
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2400,
    height = 1900,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_fractions) * length(metric_columns)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_fractions),
    ncol = length(metric_columns),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(metric_columns)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1, length(binary_fractions)), 0.24)
  )

  graphics::par(
    mar = c(4.5, 5, 3, 1.5),
    oma = c(0, 0, 5, 0)
  )

  for (binary_fraction in binary_fractions) {
    plot_data <- plot_results[plot_results$binary_fraction == binary_fraction, ]

    for (i in seq_along(metric_columns)) {
      plot_metric_panel(
        plot_data = plot_data,
        metric = metric_columns[i],
        metric_label = paste0(
          metric_labels[i],
          "\n",
          format_binary_fraction(binary_fraction)
        ),
        show_y_label = i == 1
      )
    }
  }

  graphics::mtext(
    "Noise Analysis",
    outer = TRUE,
    side = 3,
    line = 3,
    cex = 1.35,
    font = 2
  )

  graphics::mtext(
    "x-axis varies signal strength; lines show scaling methods; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 1.4,
    cex = 1
  )

  graphics::par(mar = c(0, 0, 0, 0), xpd = NA)
  graphics::plot.new()
  graphics::legend(
    "center",
    legend = scaling_levels,
    title = "Scaling",
    col = scaling_colours[scaling_levels],
    pch = scaling_symbols[scaling_levels],
    lwd = 2.8,
    bty = "n",
    ncol = length(scaling_levels),
    cex = 1.25,
    title.cex = 1.25
  )

  invisible(output_file)
}

created_files <- c(
  plot_noise_for_binary_fraction(
    binary_fraction = 1.0,
    file_name = "noise_analysis_all_binary.png"
  ),
  plot_noise_for_binary_fraction(
    binary_fraction = 0.5,
    file_name = "noise_analysis_mixed_binary_continuous.png"
  ),
  plot_combined_noise()
)

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

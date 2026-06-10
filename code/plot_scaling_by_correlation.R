# Scaling comparison split by predictor correlation.

preferred_results_file <- file.path("results", "correlation_benchmark_results.csv")
fallback_results_file <- file.path("results", "fake_toy_benchmark_results.csv")
results_file <- if (file.exists(preferred_results_file)) {
  preferred_results_file
} else {
  fallback_results_file
}

if (!file.exists(results_file)) {
  stop(
    "Cannot find ",
    results_file,
    ". Run source(\"code/run_correlation_benchmark.R\") first."
  )
}

raw_results <- utils::read.csv(results_file)

required_columns <- c(
  "seed",
  "scaling_method",
  "ev_xy",
  "ev_xx",
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

plot_dir <- file.path("plots", "correlation")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

metric_columns <- c("f1_score", "recall", "precision")
metric_labels <- c("F1 Score", "Recall", "Precision")
scaling_levels <- c("none", "zscore", "2sd")
algorithm_levels <- c("cv_lasso_min", "cv_lasso_1se")
scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
combo_colours <- c(
  cv_lasso_min_none = "#0072B2",
  cv_lasso_min_zscore = "#F0E442",
  cv_lasso_min_2sd = "#009E73",
  cv_lasso_1se_none = "#D55E00",
  cv_lasso_1se_zscore = "#CC79A7",
  cv_lasso_1se_2sd = "#000000"
)

if (!"binary_fraction" %in% names(raw_results)) {
  raw_results$binary_fraction <- 1
}

if (!"algorithm" %in% names(raw_results)) {
  raw_results$algorithm <- "cv_lasso_1se"
}

summarise_metric <- function(data, metric) {
  split_keys <- interaction(
    data$algorithm,
    data$scaling_method,
    data$binary_fraction,
    data$ev_xy,
    data$ev_xx,
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
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_fraction = piece$binary_fraction[1],
      ev_xy = piece$ev_xy[1],
      ev_xx = piece$ev_xx[1],
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

algorithm_levels <- intersect(algorithm_levels, unique(plot_results$algorithm))

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

plot_metric_by_correlation <- function(plot_data,
                                       metric,
                                       metric_label,
                                       ev_xx,
                                       show_y_label = FALSE,
                                       show_x_label = FALSE) {
  panel_data <- plot_data[
    plot_data$metric == metric &
      plot_data$ev_xx == ev_xx,
  ]
  x_values <- sort(unique(plot_data$ev_xy))

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = if (show_x_label) {
      "Explained variance / signal strength (ev_xy)"
    } else {
      ""
    },
    ylab = if (show_y_label) {
      paste0("ev_xx = ", ev_xx, "\nMean performance")
    } else {
      ""
    },
    main = metric_label,
    xaxt = "n",
    cex.lab = 1.25,
    cex.main = 1.25,
    cex.axis = 1.15
  )

  graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.15)
  graphics::grid(col = "grey88")

  for (algorithm in algorithm_levels) {
    for (scaling_method in scaling_levels) {
      combo_key <- paste(algorithm, scaling_method, sep = "_")
      line_data <- panel_data[
        panel_data$algorithm == algorithm &
          panel_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$ev_xy), ]

      graphics::polygon(
        x = c(line_data$ev_xy, rev(line_data$ev_xy)),
        y = c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.12),
        border = NA
      )

      graphics::lines(
        line_data$ev_xy,
        line_data$mean,
        col = combo_colours[combo_key],
        lty = 1,
        lwd = 2.2
      )

      graphics::points(
        line_data$ev_xy,
        line_data$mean,
        col = combo_colours[combo_key],
        pch = scaling_symbols[scaling_method],
        cex = 1
      )
    }
  }
}

plot_correlation_for_binary_fraction <- function(binary_fraction, file_name) {
  plot_data <- plot_results[
    plot_results$binary_fraction == binary_fraction,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction
    )
    return(invisible(NULL))
  }

  ev_xx_values <- sort(unique(plot_data$ev_xx))
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2600,
    height = 2550,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(ev_xx_values) * length(metric_columns)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(ev_xx_values),
    ncol = length(metric_columns),
    byrow = TRUE
  )
  layout_matrix <- rbind(
    layout_matrix,
    rep(panel_count + 1, length(metric_columns))
  )

  graphics::layout(
    layout_matrix,
    heights = c(rep(1, length(ev_xx_values)), 0.34)
  )

  graphics::par(
    mar = c(4.2, 5.5, 3, 1.5),
    oma = c(0, 0, 5, 0)
  )

  for (row_index in seq_along(ev_xx_values)) {
    for (metric_index in seq_along(metric_columns)) {
      plot_metric_by_correlation(
        plot_data = plot_data,
        metric = metric_columns[metric_index],
        metric_label = metric_labels[metric_index],
        ev_xx = ev_xx_values[row_index],
        show_y_label = metric_index == 1,
        show_x_label = row_index == length(ev_xx_values)
      )
    }
  }

  graphics::mtext(
    paste0(
      "Scaling by Signal Strength, Split by Correlation - ",
      format_binary_fraction(binary_fraction)
    ),
    outer = TRUE,
    side = 3,
    line = 3,
    cex = 1.45,
    font = 2
  )

  graphics::mtext(
    "Rows show predictor correlation; x-axis shows signal strength; lines show CV lambda choice + scaling method",
    outer = TRUE,
    side = 3,
    line = 1.3,
    cex = 1.08
  )

  graphics::par(xpd = NA)
  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    ifelse(
      legend_grid$algorithm == "cv_lasso_min",
      "CV LASSO lambda.min",
      "CV LASSO lambda.1se"
    ),
    "+",
    legend_grid$scaling_method
  )

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::legend(
    "center",
    legend = legend_labels,
    title = "Model + scaling",
    col = combo_colours[paste(
      legend_grid$algorithm,
      legend_grid$scaling_method,
      sep = "_"
    )],
    lty = 1,
    pch = scaling_symbols[legend_grid$scaling_method],
    lwd = 2.2,
    cex = 1.08,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

binary_fractions_to_plot <- sort(unique(plot_results$binary_fraction), decreasing = TRUE)

created_files <- vapply(
  seq_along(binary_fractions_to_plot),
  function(i) {
    binary_fraction <- binary_fractions_to_plot[i]
    file_stub <- if (binary_fraction == 1) {
      "all_binary"
    } else {
      paste0(
        round(binary_fraction * 100),
        "pct_binary_",
        round((1 - binary_fraction) * 100),
        "pct_continuous"
      )
    }

    plot_correlation_for_binary_fraction(
      binary_fraction = binary_fraction,
      file_name = paste0(
        "scaling_by_correlation_",
        file_stub,
        ".png"
      )
    )
  },
  character(1)
)

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

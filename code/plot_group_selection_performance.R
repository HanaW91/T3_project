# Group-level selection plots for the imbalance benchmark.
# Main check: are rare binary true predictors picked up differently?

results_file <- file.path("results", "imbalance_results.csv")

if (!file.exists(results_file)) {
  stop(
    "Cannot find ",
    results_file,
    ". Run source(\"code/run_imbalance_benchmark.R\") first."
  )
}

raw_results <- utils::read.csv(results_file)

required_columns <- c(
  "seed",
  "algorithm",
  "scaling_method",
  "binary_fraction",
  "binary_top_fraction",
  "pk_imbalance_fraction",
  "ev_xy",
  "ev_xx",
  "binary_recall",
  "continuous_recall",
  "rare_binary_recall",
  "nonrare_binary_recall",
  "rare_binary_f1_score",
  "nonrare_binary_f1_score"
)

missing_columns <- setdiff(required_columns, names(raw_results))

if (length(missing_columns) > 0) {
  stop(
    "The results file is missing these column(s): ",
    paste(missing_columns, collapse = ", "),
    "\nRerun source(\"code/run_imbalance_benchmark.R\") first."
  )
}

plot_dir <- file.path("plots", "imbalance", "group_performance")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

algorithm_levels <- c("cv_lasso_min", "cv_lasso_1se")
scaling_levels <- c("none", "zscore", "2sd")
scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
combo_colours <- c(
  cv_lasso_min_none = "#0072B2",
  cv_lasso_min_zscore = "#E69F00",
  cv_lasso_min_2sd = "#009E73",
  cv_lasso_1se_none = "#D55E00",
  cv_lasso_1se_zscore = "#CC79A7",
  cv_lasso_1se_2sd = "#000000"
)

group_metrics <- data.frame(
  group = c("binary", "continuous", "rare_binary", "nonrare_binary"),
  label = c("All binary", "Continuous", "Rare binary", "Non-rare binary"),
  recall_column = c(
    "binary_recall",
    "continuous_recall",
    "rare_binary_recall",
    "nonrare_binary_recall"
  ),
  stringsAsFactors = FALSE
)

summarise_group_recall <- function(data) {
  pieces <- vector("list", nrow(group_metrics))

  for (i in seq_len(nrow(group_metrics))) {
    recall_column <- group_metrics$recall_column[i]
    group_data <- data
    group_data$group <- group_metrics$group[i]
    group_data$group_label <- group_metrics$label[i]
    group_data$recall_value <- group_data[[recall_column]]
    pieces[[i]] <- group_data
  }

  long_data <- do.call(rbind, pieces)
  split_keys <- interaction(
    long_data$algorithm,
    long_data$scaling_method,
    long_data$binary_top_fraction,
    long_data$pk_imbalance_fraction,
    long_data$group,
    drop = TRUE
  )

  summary_rows <- lapply(split(long_data, split_keys), function(piece) {
    values <- piece$recall_value
    n_values <- sum(!is.na(values))
    mean_value <- if (n_values == 0) {
      NA_real_
    } else {
      mean(values, na.rm = TRUE)
    }
    se_value <- if (n_values <= 1) {
      0
    } else {
      stats::sd(values, na.rm = TRUE) / sqrt(n_values)
    }
    ci_width <- if (n_values == 0) {
      NA_real_
    } else {
      stats::qt(0.975, df = max(n_values - 1, 1)) * se_value
    }

    data.frame(
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
      group = piece$group[1],
      group_label = piece$group_label[1],
      mean = mean_value,
      ci_low = if (is.na(mean_value)) NA_real_ else max(0, mean_value - ci_width),
      ci_high = if (is.na(mean_value)) NA_real_ else min(1, mean_value + ci_width),
      n = n_values
    )
  })

  do.call(rbind, summary_rows)
}

format_binary_fraction <- function(binary_fraction) {
  paste0(
    round(binary_fraction * 100),
    "% binary / ",
    round((1 - binary_fraction) * 100),
    "% continuous predictors"
  )
}

format_file_value <- function(value) {
  gsub("\\.", "_", format(value, trim = TRUE, scientific = FALSE))
}

plot_group_recall <- function(binary_fraction = 0.5,
                              ev_xy_to_plot = 0.7,
                              ev_xx_to_plot = 0.7,
                              file_name = "group_recall_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot &
      raw_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  plot_summary <- summarise_group_recall(plot_data)
  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  group_labels <- group_metrics$label
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 4000,
    height = 2600,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(group_labels)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(group_labels),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(group_labels)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1, length(binary_top_fractions)), 0.30)
  )
  graphics::par(mar = c(4.8, 5.5, 4, 1.5), oma = c(0, 0, 6, 0))

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

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (group_index in seq_along(group_labels)) {
      group_label <- group_labels[group_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$group_label == group_label,
      ]
      x_values <- sort(unique(panel_data$pk_imbalance_fraction))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
        } else {
          ""
        },
        ylab = if (group_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nRecall")
        } else {
          ""
        },
        main = group_label,
        xaxt = "n",
        cex.lab = 1.20,
        cex.main = 1.25,
        cex.axis = 1.10
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.05)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::polygon(
          x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
          border = NA
        )
      }

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::lines(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          lwd = 3
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.15
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Recall by Predictor Group - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 4,
    cex = 1.30,
    font = 2
  )

  graphics::mtext(
    "Panels split true-predictor recovery by predictor type and rarity; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 2.2,
    cex = 0.98
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
    lwd = 3,
    cex = 1.05,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_rare_vs_nonrare_recall <- function(binary_fraction = 0.5,
                                        ev_xy_to_plot = 0.7,
                                        ev_xx_to_plot = 0.7,
                                        file_name = "rare_vs_nonrare_recall_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot &
      raw_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  plot_summary <- summarise_group_recall(plot_data)
  plot_summary <- plot_summary[
    plot_summary$group %in% c("rare_binary", "nonrare_binary"),
  ]

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  group_labels <- c("Rare binary", "Non-rare binary")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 2350,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(group_labels)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(group_labels),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(group_labels)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1.15, length(binary_top_fractions)), 0.20)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

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

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (group_index in seq_along(group_labels)) {
      group_label <- group_labels[group_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$group_label == group_label,
      ]
      x_values <- sort(unique(panel_data$pk_imbalance_fraction))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
        } else {
          ""
        },
        ylab = if (group_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nRecall")
        } else {
          ""
        },
        main = if (row_index == 1) group_label else "",
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.25,
        cex.axis = 1.05
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.0)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::polygon(
          x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
          border = NA
        )
      }

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::lines(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          lwd = 3
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.1
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare vs Non-rare Binary Recall - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.25,
    font = 2
  )

  graphics::mtext(
    "Focused view of true binary predictor recovery; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
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
    lwd = 3,
    cex = 0.98,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

summarise_rare_nonrare_metric <- function(data,
                                          metric_column,
                                          metric_label,
                                          group_label) {
  group_data <- data
  group_data$metric_label <- metric_label
  group_data$group_label <- group_label
  group_data$value <- group_data[[metric_column]]

  split_keys <- interaction(
    group_data$algorithm,
    group_data$scaling_method,
    group_data$binary_top_fraction,
    group_data$pk_imbalance_fraction,
    group_data$metric_label,
    group_data$group_label,
    drop = TRUE
  )

  summary_rows <- lapply(split(group_data, split_keys), function(piece) {
    values <- piece$value
    n_values <- sum(!is.na(values))
    mean_value <- if (n_values == 0) {
      NA_real_
    } else {
      mean(values, na.rm = TRUE)
    }
    se_value <- if (n_values <= 1) {
      0
    } else {
      stats::sd(values, na.rm = TRUE) / sqrt(n_values)
    }
    ci_width <- if (n_values == 0) {
      NA_real_
    } else {
      stats::qt(0.975, df = max(n_values - 1, 1)) * se_value
    }

    data.frame(
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
      metric_label = piece$metric_label[1],
      group_label = piece$group_label[1],
      mean = mean_value,
      ci_low = if (is.na(mean_value)) NA_real_ else max(0, mean_value - ci_width),
      ci_high = if (is.na(mean_value)) NA_real_ else min(1, mean_value + ci_width),
      n = n_values
    )
  })

  do.call(rbind, summary_rows)
}

plot_rare_vs_nonrare_recall_f1 <- function(
    binary_fraction = 0.5,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "rare_vs_nonrare_recall_f1_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot &
      raw_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  plot_summary <- rbind(
    summarise_rare_nonrare_metric(plot_data, "rare_binary_recall", "Recall", "Rare binary"),
    summarise_rare_nonrare_metric(plot_data, "nonrare_binary_recall", "Recall", "Non-rare binary"),
    summarise_rare_nonrare_metric(plot_data, "rare_binary_f1_score", "F1 Score", "Rare binary"),
    summarise_rare_nonrare_metric(plot_data, "nonrare_binary_f1_score", "F1 Score", "Non-rare binary")
  )

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  metric_labels_to_plot <- c("Recall", "F1 Score")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3300,
    height = 2350,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(metric_labels_to_plot)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(metric_labels_to_plot),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(metric_labels_to_plot)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1.15, length(binary_top_fractions)), 0.22)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    group_label = c("Rare binary", "Non-rare binary"),
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    ifelse(
      legend_grid$algorithm == "cv_lasso_min",
      "CV min",
      "CV 1se"
    ),
    "+",
    legend_grid$scaling_method,
    "+",
    ifelse(legend_grid$group_label == "Rare binary", "rare", "non-rare")
  )
  group_lty <- c("Rare binary" = 1, "Non-rare binary" = 2)
  group_pch_offset <- c("Rare binary" = 0, "Non-rare binary" = 1)

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (metric_index in seq_along(metric_labels_to_plot)) {
      metric_label <- metric_labels_to_plot[metric_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$metric_label == metric_label,
      ]
      x_values <- sort(unique(panel_data$pk_imbalance_fraction))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
        } else {
          ""
        },
        ylab = if (metric_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nPerformance")
        } else {
          ""
        },
        main = if (row_index == 1) metric_label else "",
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.25,
        cex.axis = 1.05
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.0)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        group_label <- legend_grid$group_label[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method &
            panel_data$group_label == group_label,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::lines(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          lty = group_lty[group_label],
          lwd = if (group_label == "Rare binary") 3 else 2.4
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method] + group_pch_offset[group_label],
          cex = 1.05
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare vs Non-rare Binary Recall and F1 - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.20,
    font = 2
  )

  graphics::mtext(
    "Solid lines show rare binary; dashed lines show non-rare binary",
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
  )

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::legend(
    "center",
    legend = legend_labels,
    title = "Model + scaling + group",
    col = combo_colours[paste(
      legend_grid$algorithm,
      legend_grid$scaling_method,
      sep = "_"
    )],
    lty = group_lty[legend_grid$group_label],
    pch = scaling_symbols[legend_grid$scaling_method] +
      group_pch_offset[legend_grid$group_label],
    lwd = 2.6,
    cex = 0.80,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_rare_nonrare_recall_f1_four_panel <- function(
    binary_fraction = 0.5,
    binary_top_fraction_to_plot = 0.05,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "rare_nonrare_recall_f1_four_panel_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$binary_top_fraction == binary_top_fraction_to_plot &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", binary_top_fraction = ",
      binary_top_fraction_to_plot,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  plot_summary <- rbind(
    summarise_rare_nonrare_metric(plot_data, "rare_binary_f1_score", "F1 Score", "Rare binary"),
    summarise_rare_nonrare_metric(plot_data, "nonrare_binary_f1_score", "F1 Score", "Non-rare binary"),
    summarise_rare_nonrare_metric(plot_data, "rare_binary_recall", "Recall", "Rare binary"),
    summarise_rare_nonrare_metric(plot_data, "nonrare_binary_recall", "Recall", "Non-rare binary")
  )

  metric_labels_to_plot <- c("F1 Score", "Recall")
  group_labels_to_plot <- c("Rare binary", "Non-rare binary")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 2100,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  layout_matrix <- matrix(seq_len(4), nrow = 2, ncol = 2, byrow = TRUE)
  layout_matrix <- rbind(layout_matrix, c(5, 5))

  graphics::layout(layout_matrix, heights = c(1, 1, 0.20))
  graphics::par(mar = c(4.8, 5.5, 2.1, 1.5), oma = c(0, 0, 5.3, 0))

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

  for (metric_index in seq_along(metric_labels_to_plot)) {
    metric_label <- metric_labels_to_plot[metric_index]

    for (group_index in seq_along(group_labels_to_plot)) {
      group_label <- group_labels_to_plot[group_index]
      panel_data <- plot_summary[
        plot_summary$metric_label == metric_label &
          plot_summary$group_label == group_label,
      ]
      x_values <- sort(unique(panel_data$pk_imbalance_fraction))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (metric_index == length(metric_labels_to_plot)) {
          "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
        } else {
          ""
        },
        ylab = metric_label,
        main = if (metric_index == 1) group_label else "",
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.25,
        cex.axis = 1.05
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.0)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::polygon(
          x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
          border = NA
        )
      }

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::lines(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          lwd = 3
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.1
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare vs Non-rare Binary Variable Selection - ",
      format_binary_fraction(binary_fraction),
      "; rare level = ",
      binary_top_fraction_to_plot,
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.20,
    font = 2
  )

  graphics::mtext(
    "Rows compare F1 and recall; columns compare rare and non-rare true binary predictors",
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
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
    lwd = 3,
    cex = 0.95,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_binary_group_recall_f1 <- function(
    group_to_plot,
    binary_fraction = 0.5,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "binary_group_recall_f1_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot &
      raw_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  if (group_to_plot == "rare_binary") {
    group_label <- "Rare binary"
    recall_column <- "rare_binary_recall"
    f1_column <- "rare_binary_f1_score"
  } else if (group_to_plot == "nonrare_binary") {
    group_label <- "Non-rare binary"
    recall_column <- "nonrare_binary_recall"
    f1_column <- "nonrare_binary_f1_score"
  } else {
    stop("Unknown group_to_plot: ", group_to_plot)
  }

  plot_summary <- rbind(
    summarise_rare_nonrare_metric(plot_data, recall_column, "Recall", group_label),
    summarise_rare_nonrare_metric(plot_data, f1_column, "F1 Score", group_label)
  )

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  metric_labels_to_plot <- c("Recall", "F1 Score")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 2350,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(metric_labels_to_plot)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(metric_labels_to_plot),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(metric_labels_to_plot)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1.15, length(binary_top_fractions)), 0.22)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

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

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (metric_index in seq_along(metric_labels_to_plot)) {
      metric_label <- metric_labels_to_plot[metric_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$metric_label == metric_label,
      ]
      x_values <- sort(unique(panel_data$pk_imbalance_fraction))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
        } else {
          ""
        },
        ylab = if (metric_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nPerformance")
        } else {
          ""
        },
        main = if (row_index == 1) metric_label else "",
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.25,
        cex.axis = 1.05
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.0)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::polygon(
          x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
          border = NA
        )
      }

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- panel_data[
          panel_data$algorithm == algorithm &
            panel_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        graphics::lines(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          lwd = 3
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.1
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      group_label,
      " Recall and F1 - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.20,
    font = 2
  )

  graphics::mtext(
    "Panels show true predictor recovery for one binary group; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
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
    lwd = 3,
    cex = 0.95,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_binary_group_f1_with_recall_table <- function(
    group_to_plot,
    binary_fraction = 0.5,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "binary_group_f1_with_recall_table_by_pk.png") {
  plot_data <- raw_results[
    raw_results$binary_fraction == binary_fraction &
      raw_results$ev_xy == ev_xy_to_plot &
      raw_results$ev_xx == ev_xx_to_plot &
      raw_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  if (group_to_plot == "rare_binary") {
    group_label <- "Rare binary"
    recall_column <- "rare_binary_recall"
    f1_column <- "rare_binary_f1_score"
  } else if (group_to_plot == "nonrare_binary") {
    group_label <- "Non-rare binary"
    recall_column <- "nonrare_binary_recall"
    f1_column <- "nonrare_binary_f1_score"
  } else {
    stop("Unknown group_to_plot: ", group_to_plot)
  }

  f1_summary <- summarise_rare_nonrare_metric(
    plot_data,
    f1_column,
    "F1 Score",
    group_label
  )
  recall_summary <- summarise_rare_nonrare_metric(
    plot_data,
    recall_column,
    "Recall",
    group_label
  )

  binary_top_fractions <- sort(unique(f1_summary$binary_top_fraction), decreasing = TRUE)
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3100,
    height = 2350,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * 2
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = 2,
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, 2))

  graphics::layout(
    layout_matrix,
    widths = c(1.55, 1),
    heights = c(rep(1.15, length(binary_top_fractions)), 0.22)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

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

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]
    f1_panel <- f1_summary[f1_summary$binary_top_fraction == binary_top_fraction, ]
    recall_panel <- recall_summary[
      recall_summary$binary_top_fraction == binary_top_fraction,
    ]
    x_values <- sort(unique(f1_panel$pk_imbalance_fraction))

    graphics::plot(
      x_values,
      rep(NA_real_, length(x_values)),
      type = "n",
      ylim = c(0, 1),
      xlab = if (row_index == length(binary_top_fractions)) {
        "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
      } else {
        ""
      },
      ylab = paste0("Rare category = ", binary_top_fraction, "\nF1 score"),
      main = if (row_index == 1) "F1 Score" else "",
      xaxt = "n",
      cex.lab = 1.15,
      cex.main = 1.25,
      cex.axis = 1.05
    )
    graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.0)
    graphics::grid(col = "grey88")

    for (i in seq_len(nrow(legend_grid))) {
      algorithm <- legend_grid$algorithm[i]
      scaling_method <- legend_grid$scaling_method[i]
      combo_key <- paste(algorithm, scaling_method, sep = "_")
      line_data <- f1_panel[
        f1_panel$algorithm == algorithm &
          f1_panel$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$pk_imbalance_fraction), ]
      line_data <- line_data[!is.na(line_data$mean), ]

      if (nrow(line_data) == 0) {
        next
      }

      graphics::polygon(
        x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
        y = c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
        border = NA
      )
      graphics::lines(
        line_data$pk_imbalance_fraction,
        line_data$mean,
        col = combo_colours[combo_key],
        lwd = 3
      )
      graphics::points(
        line_data$pk_imbalance_fraction,
        line_data$mean,
        col = combo_colours[combo_key],
        pch = scaling_symbols[scaling_method],
        cex = 1.1
      )
    }

    table_rows <- aggregate(
      mean ~ algorithm + scaling_method,
      data = recall_panel,
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    table_rows$algorithm <- factor(table_rows$algorithm, levels = algorithm_levels)
    table_rows$scaling_method <- factor(table_rows$scaling_method, levels = scaling_levels)
    table_rows <- table_rows[order(table_rows$algorithm, table_rows$scaling_method), ]

    graphics::plot.new()
    graphics::text(
      0,
      0.96,
      if (row_index == 1) "Mean recall" else "",
      adj = c(0, 1),
      cex = 1.20,
      font = 2
    )
    graphics::text(0, 0.83, "Model + scaling", adj = c(0, 1), cex = 0.92, font = 2)
    graphics::text(0.96, 0.83, "Recall", adj = c(1, 1), cex = 0.92, font = 2)
    y_positions <- seq(0.72, 0.08, length.out = nrow(table_rows))

    for (j in seq_len(nrow(table_rows))) {
      label <- paste(
        ifelse(
          table_rows$algorithm[j] == "cv_lasso_min",
          "CV min",
          "CV 1se"
        ),
        "+",
        table_rows$scaling_method[j]
      )
      combo_key <- paste(
        as.character(table_rows$algorithm[j]),
        as.character(table_rows$scaling_method[j]),
        sep = "_"
      )
      graphics::text(
        0,
        y_positions[j],
        label,
        adj = c(0, 0.5),
        cex = 0.82,
        col = combo_colours[combo_key]
      )
      graphics::text(
        0.96,
        y_positions[j],
        sprintf("%.2f", table_rows$mean[j]),
        adj = c(1, 0.5),
        cex = 0.82,
        col = combo_colours[combo_key]
      )
    }
  }

  graphics::mtext(
    paste0(
      group_label,
      " F1 with Recall Table - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.20,
    font = 2
  )

  graphics::mtext(
    "Left: F1 across imbalance proportions; right: mean recall averaged across pk_imbalance_fraction values",
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
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
    lwd = 3,
    cex = 0.95,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

binary_fraction_to_plot <- 0.5
available_slices <- unique(
  raw_results[
    raw_results$binary_fraction == binary_fraction_to_plot,
    c("ev_xy", "ev_xx")
  ]
)
available_slices <- available_slices[
  order(available_slices$ev_xx, available_slices$ev_xy),
]

created_files <- unlist(
  lapply(seq_len(nrow(available_slices)), function(i) {
    ev_xy_to_plot <- available_slices$ev_xy[i]
    ev_xx_to_plot <- available_slices$ev_xx[i]

    c(
      plot_rare_vs_nonrare_recall(
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "rare_vs_nonrare_recall_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_rare_nonrare_recall_f1_four_panel(
        binary_fraction = binary_fraction_to_plot,
        binary_top_fraction_to_plot = 0.05,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "rare_nonrare_recall_f1_four_panel_rare_0_05_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_binary_group_recall_f1(
        group_to_plot = "rare_binary",
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "rare_binary_recall_f1_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_binary_group_f1_with_recall_table(
        group_to_plot = "rare_binary",
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "rare_binary_f1_with_recall_table_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_binary_group_recall_f1(
        group_to_plot = "nonrare_binary",
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "nonrare_binary_recall_f1_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_binary_group_f1_with_recall_table(
        group_to_plot = "nonrare_binary",
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "nonrare_binary_f1_with_recall_table_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      )
    )
  }),
  use.names = FALSE
)

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

# Group-level selection plots for the imbalance benchmark with stability selection.
# Main check: are rare binary true predictors picked up differently?

results_file <- file.path("results", "imbalance_stability_results.csv")

if (!file.exists(results_file)) {
  stop(
    "Cannot find ",
    results_file,
    ". Run source(\"code/run_imbalance_benchmark_with_stability.R\") first."
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
    "\nRerun source(\"code/run_imbalance_benchmark_with_stability.R\") first."
  )
}

plot_dir <- file.path("plots", "imbalance_with_stability", "group_performance")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

algorithm_levels <- c(
  "cv_lasso_min",
  "cv_lasso_1se",
  "stability_lasso_ncut_0",
  "stability_lasso_ncut_3"
)
scaling_levels <- c("none", "zscore", "2sd")
scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
combo_colours <- c(
  cv_lasso_min_none = "#0072B2",
  cv_lasso_min_zscore = "#E69F00",
  cv_lasso_min_2sd = "#009E73",
  cv_lasso_1se_none = "#D55E00",
  cv_lasso_1se_zscore = "#CC79A7",
  cv_lasso_1se_2sd = "#000000",
  stability_lasso_ncut_0_none = "#56B4E9",
  stability_lasso_ncut_0_zscore = "#F0E442",
  stability_lasso_ncut_0_2sd = "#117733",
  stability_lasso_ncut_3_none = "#CC6677",
  stability_lasso_ncut_3_zscore = "#AA4499",
  stability_lasso_ncut_3_2sd = "#332288"
)
algorithm_labels <- c(
  cv_lasso_min = "CV LASSO lambda.min",
  cv_lasso_1se = "CV LASSO lambda.1se",
  stability_lasso_ncut_0 = "Stability LASSO ncut=0",
  stability_lasso_ncut_3 = "Stability LASSO ncut=3"
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
    algorithm_labels[legend_grid$algorithm],
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
    cex = 1.40,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_rare_vs_nonrare_recall <- function(binary_fraction = 0.5,
                                        ev_xy_to_plot = 0.7,
                                        ev_xx_to_plot = 0.7,
                                        file_name = "rare_vs_nonrare_recall_with_stability_by_pk.png") {
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
    height = 3200,
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
    heights = c(rep(1.15, length(binary_top_fractions)), 0.24)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
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
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.08),
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
          lwd = 2.7
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.05
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare vs Non-rare Binary Recall",
      " - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.40,
    font = 2
  )

  graphics::mtext(
    "Focused view of true binary predictor selection; shaded bands show 95% confidence intervals across seeds",
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
    lwd = 2.7,
    cex = 0.80,
    ncol = 4,
    bty = "n"
  )

  invisible(output_file)
}

plot_binary_vs_continuous_recall <- function(binary_fraction = 0.5,
                                             ev_xy_to_plot = 0.7,
                                             ev_xx_to_plot = 0.7,
                                             file_name = "binary_vs_continuous_recall_with_stability_by_pk.png") {
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
  plot_summary <- plot_summary[plot_summary$group %in% c("binary", "continuous"), ]

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  group_labels <- c("All binary", "Continuous")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 3200,
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
    heights = c(rep(1.15, length(binary_top_fractions)), 0.24)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
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
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.08),
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
          lwd = 2.7
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.05
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Binary vs Continuous Recall",
      " - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.05,
    font = 2
  )

  graphics::mtext(
    "Overall comparison of true binary and true continuous predictor selection",
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
    lwd = 2.7,
    cex = 0.80,
    ncol = 4,
    bty = "n"
  )

  invisible(output_file)
}

plot_rare_recall_gap <- function(binary_fraction = 0.5,
                                 ev_xy_to_plot = 0.7,
                                 ev_xx_to_plot = 0.7,
                                 file_name = "rare_recall_gap_by_algorithm.png") {
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

  plot_data$rare_recall_gap <-
    plot_data$rare_binary_recall - plot_data$nonrare_binary_recall
  split_keys <- interaction(
    plot_data$algorithm,
    plot_data$scaling_method,
    plot_data$binary_top_fraction,
    plot_data$pk_imbalance_fraction,
    drop = TRUE
  )

  summary_rows <- lapply(split(plot_data, split_keys), function(piece) {
    values <- piece$rare_recall_gap
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
      mean = mean_value,
      ci_low = if (is.na(mean_value)) NA_real_ else mean_value - ci_width,
      ci_high = if (is.na(mean_value)) NA_real_ else mean_value + ci_width,
      n = n_values
    )
  })

  plot_summary <- do.call(rbind, summary_rows)
  plot_summary$algorithm <- factor(plot_summary$algorithm, levels = algorithm_levels)
  plot_summary$scaling_method <- factor(plot_summary$scaling_method, levels = scaling_levels)

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  pk_values <- sort(unique(plot_summary$pk_imbalance_fraction))
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3600,
    height = 2100,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(pk_values)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(pk_values),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(pk_values)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1, length(binary_top_fractions)), 0.26)
  )
  graphics::par(mar = c(5.8, 5.8, 4, 1.5), oma = c(0, 0, 6, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
    "+",
    legend_grid$scaling_method
  )

  x_values <- seq_along(algorithm_levels)
  offsets <- c(none = -0.18, zscore = 0, `2sd` = 0.18)

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (col_index in seq_along(pk_values)) {
      pk_value <- pk_values[col_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$pk_imbalance_fraction == pk_value,
      ]

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(-1, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Algorithm"
        } else {
          ""
        },
        ylab = if (col_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nRecall gap")
        } else {
          ""
        },
        main = paste0("pk imbalance = ", pk_value),
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.20,
        cex.axis = 1.0
      )
      graphics::abline(h = 0, col = "grey35", lwd = 2, lty = 2)
      graphics::grid(col = "grey90")
      graphics::axis(
        1,
        at = x_values,
        labels = c("CV min", "CV 1se", "Stab 0", "Stab 3"),
        cex.axis = 0.95
      )

      for (scaling_method in scaling_levels) {
        line_data <- panel_data[panel_data$scaling_method == scaling_method, ]
        line_data <- line_data[order(line_data$algorithm), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        x_positions <- match(as.character(line_data$algorithm), algorithm_levels) +
          offsets[as.character(line_data$scaling_method)]
        colours <- combo_colours[paste(
          as.character(line_data$algorithm),
          as.character(line_data$scaling_method),
          sep = "_"
        )]

        graphics::segments(
          x0 = x_positions,
          y0 = line_data$ci_low,
          x1 = x_positions,
          y1 = line_data$ci_high,
          col = colours,
          lwd = 2
        )
        graphics::points(
          x_positions,
          line_data$mean,
          col = colours,
          pch = scaling_symbols[as.character(line_data$scaling_method)],
          cex = 1.25
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare Binary Recall Gap by Algorithm - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 4,
    cex = 1.25,
    font = 2
  )

  graphics::mtext(
    "Recall gap = rare_binary_recall - nonrare_binary_recall; values below zero mean rare binary predictors are recovered worse",
    outer = TRUE,
    side = 3,
    line = 2.2,
    cex = 0.94
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
    pch = scaling_symbols[legend_grid$scaling_method],
    lwd = 2,
    cex = 0.92,
    ncol = 4,
    bty = "n"
  )

  invisible(output_file)
}

summarise_gap_metric <- function(data,
                                 rare_column,
                                 nonrare_column,
                                 metric_label) {
  gap_data <- data
  gap_data$gap <- gap_data[[rare_column]] - gap_data[[nonrare_column]]
  gap_data$metric_label <- metric_label

  split_keys <- interaction(
    gap_data$algorithm,
    gap_data$scaling_method,
    gap_data$binary_top_fraction,
    gap_data$pk_imbalance_fraction,
    gap_data$metric_label,
    drop = TRUE
  )

  summary_rows <- lapply(split(gap_data, split_keys), function(piece) {
    values <- piece$gap
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
      mean = mean_value,
      ci_low = if (is.na(mean_value)) NA_real_ else mean_value - ci_width,
      ci_high = if (is.na(mean_value)) NA_real_ else mean_value + ci_width,
      n = n_values
    )
  })

  do.call(rbind, summary_rows)
}

plot_rare_performance_gap <- function(
    binary_fraction = 0.5,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "rare_performance_gap_by_algorithm.png") {
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
    summarise_gap_metric(
      plot_data,
      rare_column = "rare_binary_recall",
      nonrare_column = "nonrare_binary_recall",
      metric_label = "Recall gap"
    ),
    summarise_gap_metric(
      plot_data,
      rare_column = "rare_binary_f1_score",
      nonrare_column = "nonrare_binary_f1_score",
      metric_label = "F1 gap"
    )
  )
  plot_summary$algorithm <- factor(plot_summary$algorithm, levels = algorithm_levels)
  plot_summary$scaling_method <- factor(plot_summary$scaling_method, levels = scaling_levels)

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  metric_labels_to_plot <- c("Recall gap", "F1 gap")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 3200,
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
    heights = c(rep(1.15, length(binary_top_fractions)), 0.24)
  )
  graphics::par(mar = c(5.8, 5.8, 2.2, 1.5), oma = c(0, 0, 5.8, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
    "+",
    legend_grid$scaling_method
  )
  x_values <- seq_along(algorithm_levels)
  offsets <- c(none = -0.18, zscore = 0, `2sd` = 0.18)

  for (row_index in seq_along(binary_top_fractions)) {
    binary_top_fraction <- binary_top_fractions[row_index]

    for (metric_index in seq_along(metric_labels_to_plot)) {
      metric_label <- metric_labels_to_plot[metric_index]
      panel_data <- plot_summary[
        plot_summary$binary_top_fraction == binary_top_fraction &
          plot_summary$metric_label == metric_label,
      ]

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(-1, 1),
        xlab = if (row_index == length(binary_top_fractions)) {
          "Algorithm"
        } else {
          ""
        },
        ylab = if (metric_index == 1) {
          paste0("Rare category = ", binary_top_fraction, "\nRare - non-rare")
        } else {
          ""
        },
        main = if (row_index == 1) metric_label else "",
        xaxt = "n",
        cex.lab = 1.15,
        cex.main = 1.22,
        cex.axis = 1.0
      )
      graphics::abline(h = 0, col = "grey35", lwd = 2, lty = 2)
      graphics::grid(col = "grey90")
      graphics::axis(
        1,
        at = x_values,
        labels = c("CV min", "CV 1se", "Stab 0", "Stab 3"),
        cex.axis = 0.95
      )

      for (scaling_method in scaling_levels) {
        line_data <- panel_data[panel_data$scaling_method == scaling_method, ]
        line_data <- line_data[order(line_data$algorithm), ]
        line_data <- line_data[!is.na(line_data$mean), ]

        if (nrow(line_data) == 0) {
          next
        }

        x_positions <- match(as.character(line_data$algorithm), algorithm_levels) +
          offsets[as.character(line_data$scaling_method)]
        colours <- combo_colours[paste(
          as.character(line_data$algorithm),
          as.character(line_data$scaling_method),
          sep = "_"
        )]

        graphics::segments(
          x0 = x_positions,
          y0 = line_data$ci_low,
          x1 = x_positions,
          y1 = line_data$ci_high,
          col = colours,
          lwd = 2
        )
        graphics::points(
          x_positions,
          line_data$mean,
          col = colours,
          pch = scaling_symbols[as.character(line_data$scaling_method)],
          cex = 1.25
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Rare Binary Recall and F1 Gaps by Algorithm - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 4,
    cex = 1.20,
    font = 2
  )

  graphics::mtext(
    "Gap = rare binary performance - non-rare binary performance; below zero means rare binary predictors perform worse",
    outer = TRUE,
    side = 3,
    line = 2.2,
    cex = 0.94
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
    pch = scaling_symbols[legend_grid$scaling_method],
    lwd = 2,
    cex = 0.88,
    ncol = 4,
    bty = "n"
  )

  invisible(output_file)
}

summarise_binary_group_metric <- function(data,
                                          metric_column,
                                          metric_label) {
  metric_data <- data
  metric_data$metric_label <- metric_label
  metric_data$value <- metric_data[[metric_column]]

  split_keys <- interaction(
    metric_data$algorithm,
    metric_data$scaling_method,
    metric_data$binary_top_fraction,
    metric_data$pk_imbalance_fraction,
    metric_data$metric_label,
    drop = TRUE
  )

  summary_rows <- lapply(split(metric_data, split_keys), function(piece) {
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
      mean = mean_value,
      ci_low = if (is.na(mean_value)) NA_real_ else max(0, mean_value - ci_width),
      ci_high = if (is.na(mean_value)) NA_real_ else min(1, mean_value + ci_width),
      n = n_values
    )
  })

  do.call(rbind, summary_rows)
}

summarise_binary_group_metric_with_label <- function(data,
                                                     metric_column,
                                                     metric_label,
                                                     group_label) {
  metric_summary <- summarise_binary_group_metric(data, metric_column, metric_label)
  metric_summary$group_label <- group_label
  metric_summary
}

plot_rare_nonrare_recall_f1_four_panel <- function(
    binary_fraction = 0.5,
    binary_top_fraction_to_plot = 0.05,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "rare_nonrare_recall_f1_four_panel_with_stability.png") {
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
    summarise_binary_group_metric_with_label(
      plot_data,
      "rare_binary_f1_score",
      "F1 Score",
      "Rare binary"
    ),
    summarise_binary_group_metric_with_label(
      plot_data,
      "nonrare_binary_f1_score",
      "F1 Score",
      "Non-rare binary"
    ),
    summarise_binary_group_metric_with_label(
      plot_data,
      "rare_binary_recall",
      "Recall",
      "Rare binary"
    ),
    summarise_binary_group_metric_with_label(
      plot_data,
      "nonrare_binary_recall",
      "Recall",
      "Non-rare binary"
    )
  )
  plot_summary$algorithm <- factor(plot_summary$algorithm, levels = algorithm_levels)
  plot_summary$scaling_method <- factor(plot_summary$scaling_method, levels = scaling_levels)

  metric_labels_to_plot <- c("F1 Score", "Recall")
  group_labels_to_plot <- c("Rare binary", "Non-rare binary")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3600,
    height = 2200,
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

  graphics::layout(layout_matrix, heights = c(1, 1, 0.24))
  graphics::par(mar = c(4.8, 5.5, 2.1, 1.5), oma = c(0, 0, 5.3, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
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
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.08),
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
          lwd = 2.7
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.05
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
    lwd = 2.7,
    cex = 0.80,
    ncol = 4,
    bty = "n"
  )

  invisible(output_file)
}

plot_binary_group_recall_f1 <- function(
    group_to_plot,
    binary_fraction = 0.5,
    ev_xy_to_plot = 0.7,
    ev_xx_to_plot = 0.7,
    file_name = "binary_group_recall_f1_with_stability.png") {
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
    summarise_binary_group_metric(plot_data, recall_column, "Recall"),
    summarise_binary_group_metric(plot_data, f1_column, "F1 Score")
  )
  plot_summary$algorithm <- factor(plot_summary$algorithm, levels = algorithm_levels)
  plot_summary$scaling_method <- factor(plot_summary$scaling_method, levels = scaling_levels)

  binary_top_fractions <- sort(unique(plot_summary$binary_top_fraction), decreasing = TRUE)
  metric_labels_to_plot <- c("Recall", "F1 Score")
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3600,
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
    heights = c(rep(1.15, length(binary_top_fractions)), 0.24)
  )
  graphics::par(mar = c(4.8, 5.5, 2.0, 1.5), oma = c(0, 0, 5.4, 0))

  legend_grid <- expand.grid(
    algorithm = algorithm_levels,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
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
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.08),
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
          lwd = 2.7
        )
        graphics::points(
          line_data$pk_imbalance_fraction,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.05
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      group_label,
      " Recall and F1 by Algorithm - ",
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
    "Panels show one binary group only; shaded bands show 95% confidence intervals across seeds",
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
    lwd = 2.7,
    cex = 0.86,
    ncol = 4,
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
          "rare_vs_nonrare_recall_with_stability_by_pk_evxy_",
          format_file_value(ev_xy_to_plot),
          "_evxx_",
          format_file_value(ev_xx_to_plot),
          ".png"
        )
      ),
      plot_binary_vs_continuous_recall(
        binary_fraction = binary_fraction_to_plot,
        ev_xy_to_plot = ev_xy_to_plot,
        ev_xx_to_plot = ev_xx_to_plot,
        file_name = paste0(
          "binary_vs_continuous_recall_with_stability_by_pk_evxy_",
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
          "rare_nonrare_recall_f1_four_panel_with_stability_rare_0_05_evxy_",
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
          "rare_binary_recall_f1_with_stability_evxy_",
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
          "nonrare_binary_recall_f1_with_stability_evxy_",
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

# Plots for the imbalance-focused benchmark with stability-selection LASSO.

results_file <- file.path("results", "imbalance_stability_highdim_results.csv")

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
  "scaling_method",
  "binary_fraction",
  "binary_top_fraction",
  "pk_imbalance_fraction",
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

plot_dir <- file.path("plots", "imbalance_stability_highdim")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

metric_columns <- c("f1_score", "recall", "precision")
metric_labels <- c("F1 Score", "Recall", "Precision")
scaling_levels <- c("none", "zscore", "2sd")
scaling_colours <- c(none = "grey45", zscore = "#2563eb", `2sd` = "#dc2626")
scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
algorithm_levels <- c(
  "cv_lasso_min",
  "cv_lasso_1se",
  "stability_lasso_ncut_0",
  "stability_lasso_ncut_3"
)
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

if (!"algorithm" %in% names(raw_results)) {
  raw_results$algorithm <- "cv_lasso_1se"
}

summarise_metric <- function(data, metric) {
  split_keys <- interaction(
    data$algorithm,
    data$scaling_method,
    data$binary_fraction,
    data$binary_top_fraction,
    data$pk_imbalance_fraction,
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
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
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

scenario_plot_results <- do.call(
  rbind,
  lapply(metric_columns, function(metric) summarise_metric(raw_results, metric))
)

expand_balanced_baseline <- function(plot_data) {
  x_values <- sort(unique(plot_data$pk_imbalance_fraction))
  balanced_rows <- plot_data[
    plot_data$binary_top_fraction == 0.5 &
      plot_data$pk_imbalance_fraction == 0,
  ]

  if (nrow(balanced_rows) == 0) {
    return(plot_data)
  }

  expanded_rows <- do.call(
    rbind,
    lapply(seq_len(nrow(balanced_rows)), function(i) {
      repeated_row <- balanced_rows[rep(i, length(x_values)), ]
      repeated_row$pk_imbalance_fraction <- x_values
      repeated_row
    })
  )

  non_balanced_rows <- plot_data[
    !(plot_data$binary_top_fraction == 0.5),
  ]

  rbind(non_balanced_rows, expanded_rows)
}

plot_results <- expand_balanced_baseline(scenario_plot_results)

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

format_rare_fraction <- function(binary_top_fraction) {
  paste0(
    "rare category fraction = ",
    binary_top_fraction
  )
}

plot_metric_panel <- function(plot_data,
                              metric,
                              metric_label,
                              binary_top_fraction,
                              show_y_label = FALSE,
                              show_x_label = FALSE) {
  panel_data <- plot_data[
    plot_data$metric == metric &
      plot_data$binary_top_fraction == binary_top_fraction,
  ]
  x_values <- sort(unique(plot_data$pk_imbalance_fraction))

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = if (show_x_label) {
      "Proportion of binary predictors imbalanced (pk_imbalance_fraction)"
    } else {
      ""
    },
    ylab = if (show_y_label) {
      paste0(format_rare_fraction(binary_top_fraction), "\nMean performance")
    } else {
      ""
    },
    main = metric_label,
    xaxt = "n"
  )

  graphics::axis(1, at = x_values, labels = x_values)
  graphics::grid(col = "grey88")

  for (scaling_method in scaling_levels) {
    line_data <- panel_data[panel_data$scaling_method == scaling_method, ]

    if (nrow(line_data) == 0) {
      next
    }

    line_data <- line_data[order(line_data$pk_imbalance_fraction), ]

    graphics::polygon(
      x = c(line_data$pk_imbalance_fraction, rev(line_data$pk_imbalance_fraction)),
      y = c(line_data$ci_low, rev(line_data$ci_high)),
      col = grDevices::adjustcolor(scaling_colours[scaling_method], alpha.f = 0.18),
      border = NA
    )

    graphics::lines(
      line_data$pk_imbalance_fraction,
      line_data$mean,
      col = scaling_colours[scaling_method],
      lwd = 2.2
    )

    graphics::points(
      line_data$pk_imbalance_fraction,
      line_data$mean,
      col = scaling_colours[scaling_method],
      pch = scaling_symbols[scaling_method],
      cex = 1
    )
  }
}

plot_imbalance_for_binary_fraction <- function(binary_fraction, file_name) {
  plot_data <- plot_results[plot_results$binary_fraction == binary_fraction, ]

  if (nrow(plot_data) == 0) {
    warning("No results found for binary_fraction = ", binary_fraction)
    return(invisible(NULL))
  }

  binary_top_fractions <- sort(unique(plot_data$binary_top_fraction))
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2600,
    height = 2200,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::par(
    mfrow = c(length(binary_top_fractions), 3),
    mar = c(4.2, 5.5, 3, 1.5),
    oma = c(0, 0, 5, 8)
  )

  for (row_index in seq_along(binary_top_fractions)) {
    for (metric_index in seq_along(metric_columns)) {
      plot_metric_panel(
        plot_data = plot_data,
        metric = metric_columns[metric_index],
        metric_label = metric_labels[metric_index],
        binary_top_fraction = binary_top_fractions[row_index],
        show_y_label = metric_index == 1,
        show_x_label = row_index == length(binary_top_fractions)
      )
    }
  }

  graphics::mtext(
    paste0(
      "Imbalance Analysis - ",
      format_binary_fraction(binary_fraction)
    ),
    outer = TRUE,
    side = 3,
    line = 3,
    cex = 1.3,
    font = 2
  )

  graphics::mtext(
    "x-axis shows how many binary predictors are imbalanced; rows show rare-category severity; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 1.3,
    cex = 0.95
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

make_scenario_labels <- function(plot_data) {
  scenarios <- unique(
    plot_data[, c("binary_top_fraction", "pk_imbalance_fraction")]
  )
  scenarios$scenario_order <- ifelse(
    scenarios$binary_top_fraction == 0.5,
    0,
    (0.5 - scenarios$binary_top_fraction) * 10 +
      scenarios$pk_imbalance_fraction
  )
  scenarios <- scenarios[order(scenarios$scenario_order), ]
  scenarios$scenario_index <- seq_len(nrow(scenarios))
  scenarios$scenario_label <- ifelse(
    scenarios$binary_top_fraction == 0.5,
    "50/50\nbaseline",
    paste0(
      round(scenarios$binary_top_fraction * 100),
      "% rare\n",
      round(scenarios$pk_imbalance_fraction * 100),
      "% pred."
    )
  )

  merge(
    plot_data,
    scenarios[, c(
      "binary_top_fraction",
      "pk_imbalance_fraction",
      "scenario_index",
      "scenario_label"
    )],
    by = c("binary_top_fraction", "pk_imbalance_fraction"),
    all.x = TRUE
  )
}

plot_scenario_metric_panel <- function(plot_data,
                                       metric,
                                       metric_label,
                                       scenario_labels,
                                       show_y_label = FALSE) {
  metric_data <- plot_data[plot_data$metric == metric, ]
  x_values <- seq_along(scenario_labels)

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = "Imbalance scenario",
    ylab = if (show_y_label) "Mean performance" else "",
    main = metric_label,
    xaxt = "n"
  )

  graphics::axis(
    1,
    at = x_values,
    labels = scenario_labels,
    las = 2,
    cex.axis = 0.75
  )
  graphics::grid(col = "grey88")

  for (scaling_method in scaling_levels) {
    line_data <- metric_data[metric_data$scaling_method == scaling_method, ]

    if (nrow(line_data) == 0) {
      next
    }

    line_data <- line_data[order(line_data$scenario_index), ]

    graphics::polygon(
      x = c(line_data$scenario_index, rev(line_data$scenario_index)),
      y = c(line_data$ci_low, rev(line_data$ci_high)),
      col = grDevices::adjustcolor(scaling_colours[scaling_method], alpha.f = 0.18),
      border = NA
    )

    graphics::lines(
      line_data$scenario_index,
      line_data$mean,
      col = scaling_colours[scaling_method],
      lwd = 2.2
    )

    graphics::points(
      line_data$scenario_index,
      line_data$mean,
      col = scaling_colours[scaling_method],
      pch = scaling_symbols[scaling_method],
      cex = 1.1
    )
  }
}

plot_imbalance_scenarios_for_binary_fraction <- function(binary_fraction, file_name) {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction,
  ]

  if (nrow(plot_data) == 0) {
    warning("No results found for binary_fraction = ", binary_fraction)
    return(invisible(NULL))
  }

  plot_data <- make_scenario_labels(plot_data)
  scenario_labels <- unique(
    plot_data[order(plot_data$scenario_index), c("scenario_index", "scenario_label")]
  )$scenario_label

  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3000,
    height = 1300,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::par(
    mfrow = c(1, 3),
    mar = c(7, 5, 4, 1.5),
    oma = c(0, 0, 4.5, 8)
  )

  for (i in seq_along(metric_columns)) {
    plot_scenario_metric_panel(
      plot_data = plot_data,
      metric = metric_columns[i],
      metric_label = metric_labels[i],
      scenario_labels = scenario_labels,
      show_y_label = i == 1
    )
  }

  graphics::mtext(
    paste0(
      "Imbalance Scenario Analysis - ",
      format_binary_fraction(binary_fraction)
    ),
    outer = TRUE,
    side = 3,
    line = 2.4,
    cex = 1.3,
    font = 2
  )

  graphics::mtext(
    "x-axis shows the 13 imbalance scenarios; lines show scaling methods; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 0.8,
    cex = 0.95
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

plot_heatmap_for_metric <- function(binary_fraction, metric, metric_label, file_name) {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$metric == metric,
  ]

  if (nrow(plot_data) == 0) {
    warning("No results found for binary_fraction = ", binary_fraction)
    return(invisible(NULL))
  }

  output_file <- file.path(plot_dir, file_name)
  x_values <- sort(unique(plot_data$pk_imbalance_fraction))
  y_values <- sort(unique(plot_data$binary_top_fraction), decreasing = TRUE)

  grDevices::png(
    filename = output_file,
    width = 2400,
    height = 900,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::par(
    mfrow = c(1, length(scaling_levels)),
    mar = c(5, 5, 4, 1.5),
    oma = c(0, 0, 4.5, 5)
  )

  colour_palette <- grDevices::colorRampPalette(
    c("#f7fbff", "#6baed6", "#08306b")
  )(100)

  for (scaling_method in scaling_levels) {
    panel_data <- plot_data[plot_data$scaling_method == scaling_method, ]
    z_matrix <- matrix(
      NA_real_,
      nrow = length(x_values),
      ncol = length(y_values),
      dimnames = list(x_values, y_values)
    )

    for (i in seq_len(nrow(panel_data))) {
      x_index <- match(panel_data$pk_imbalance_fraction[i], x_values)
      y_index <- match(panel_data$binary_top_fraction[i], y_values)
      z_matrix[x_index, y_index] <- panel_data$mean[i]
    }

    graphics::image(
      x = seq_along(x_values),
      y = seq_along(y_values),
      z = z_matrix,
      zlim = c(0, 1),
      col = colour_palette,
      xlab = "Proportion of binary predictors imbalanced",
      ylab = "Rare category fraction",
      main = scaling_method,
      xaxt = "n",
      yaxt = "n"
    )

    graphics::axis(1, at = seq_along(x_values), labels = x_values)
    graphics::axis(2, at = seq_along(y_values), labels = y_values, las = 1)

    for (x_index in seq_along(x_values)) {
      for (y_index in seq_along(y_values)) {
        value <- z_matrix[x_index, y_index]

        if (!is.na(value)) {
          graphics::text(
            x_index,
            y_index,
            labels = sprintf("%.2f", value),
            cex = 0.85
          )
        }
      }
    }
  }

  graphics::mtext(
    paste0(
      "Imbalance Heatmap - ",
      metric_label,
      " - ",
      format_binary_fraction(binary_fraction)
    ),
    outer = TRUE,
    side = 3,
    line = 2.4,
    cex = 1.25,
    font = 2
  )

  graphics::mtext(
    "Colour and cell labels show mean performance across seeds; blank cells are not simulated duplicate balanced scenarios",
    outer = TRUE,
    side = 3,
    line = 0.8,
    cex = 0.9
  )

  graphics::par(fig = c(0.93, 0.96, 0.18, 0.78), new = TRUE, mar = c(0, 0, 0, 0))
  graphics::image(
    x = 1,
    y = seq(0, 1, length.out = 100),
    z = matrix(seq(0, 1, length.out = 100), nrow = 1),
    col = colour_palette,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  graphics::axis(4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1)
  graphics::mtext("Mean", side = 4, line = 2.5)

  invisible(output_file)
}

plot_scenario_lines_for_metric <- function(binary_fraction, metric, metric_label, file_name) {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$metric == metric,
  ]

  if (nrow(plot_data) == 0) {
    warning("No results found for binary_fraction = ", binary_fraction)
    return(invisible(NULL))
  }

  plot_data <- make_scenario_labels(plot_data)
  scenario_info <- unique(
    plot_data[
      order(plot_data$scenario_index),
      c(
        "scenario_index",
        "scenario_label",
        "binary_top_fraction",
        "pk_imbalance_fraction"
      )
    ]
  )

  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2100,
    height = 1300,
    res = 180
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::par(
    mar = c(5, 5, 5, 13),
    xpd = FALSE
  )

  x_values <- seq_along(scaling_levels)
  rare_colours <- c(
    `0.5` = "black",
    `0.2` = "#2563eb",
    `0.1` = "#f97316",
    `0.05` = "#dc2626"
  )
  line_types <- c(
    `0` = 1,
    `0.1` = 1,
    `0.2` = 2,
    `0.5` = 3,
    `0.8` = 4
  )

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = "Scaling method",
    ylab = paste0("Mean ", metric_label),
    main = paste0(
      "13 Imbalance Scenarios - ",
      metric_label,
      "\n",
      format_binary_fraction(binary_fraction)
    ),
    xaxt = "n"
  )

  graphics::axis(1, at = x_values, labels = scaling_levels)
  graphics::grid(col = "grey88")

  for (i in seq_len(nrow(scenario_info))) {
    scenario_index <- scenario_info$scenario_index[i]
    line_data <- plot_data[plot_data$scenario_index == scenario_index, ]
    line_data$scaling_method <- factor(
      line_data$scaling_method,
      levels = scaling_levels
    )
    line_data <- line_data[order(line_data$scaling_method), ]
    rare_key <- as.character(scenario_info$binary_top_fraction[i])
    pk_key <- as.character(scenario_info$pk_imbalance_fraction[i])

    graphics::lines(
      x_values,
      line_data$mean,
      col = rare_colours[rare_key],
      lty = line_types[pk_key],
      lwd = if (scenario_info$binary_top_fraction[i] == 0.5) 2.8 else 1.8
    )

    graphics::points(
      x_values,
      line_data$mean,
      col = rare_colours[rare_key],
      pch = if (scenario_info$binary_top_fraction[i] == 0.5) 16 else 1,
      cex = 0.9
    )
  }

  graphics::par(xpd = NA)
  graphics::legend(
    "right",
    inset = c(-0.42, 0.2),
    legend = c(
      "50/50 baseline",
      "20% rare",
      "10% rare",
      "5% rare"
    ),
    title = "Rare category",
    col = c("black", "#2563eb", "#f97316", "#dc2626"),
    lty = 1,
    lwd = c(2.8, 1.8, 1.8, 1.8),
    bty = "n"
  )

  graphics::legend(
    "right",
    inset = c(-0.42, -0.18),
    legend = c("0%", "10%", "20%", "50%", "80%"),
    title = "% predictors imbalanced",
    col = "grey30",
    lty = c(1, 1, 2, 3, 4),
    lwd = 1.8,
    bty = "n"
  )

  invisible(output_file)
}

plot_cv_imbalance_scenario_lines <- function(binary_fraction = 0.5,
                                             ev_xy_to_plot = 0.7,
                                             ev_xx_to_plot = 0.7,
                                             file_name = "imbalance_cv_lasso_scenarios.png") {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$ev_xy == ev_xy_to_plot &
      scenario_plot_results$ev_xx == ev_xx_to_plot,
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

  plot_data <- make_scenario_labels(plot_data)
  scenario_labels <- unique(
    plot_data[order(plot_data$scenario_index), c("scenario_index", "scenario_label")]
  )$scenario_label
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3600,
    height = 1700,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::layout(
    matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE),
    heights = c(1, 0.28)
  )
  graphics::par(mar = c(8.5, 5.5, 4, 1.5), oma = c(0, 0, 4, 0))

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

  for (metric_index in seq_along(metric_columns)) {
    metric_data <- plot_data[plot_data$metric == metric_columns[metric_index], ]
    x_values <- seq_along(scenario_labels)

    graphics::plot(
      x_values,
      rep(NA_real_, length(x_values)),
      type = "n",
      ylim = c(0, 1),
      xlab = "Imbalance scenario",
      ylab = if (metric_index == 1) "Mean performance" else "",
      main = metric_labels[metric_index],
      xaxt = "n",
      cex.lab = 1.35,
      cex.main = 1.35,
      cex.axis = 1.2
    )
    graphics::axis(1, at = x_values, labels = scenario_labels, las = 2, cex.axis = 0.7)
    graphics::grid(col = "grey88")

    for (i in seq_len(nrow(legend_grid))) {
      algorithm <- legend_grid$algorithm[i]
      scaling_method <- legend_grid$scaling_method[i]
      combo_key <- paste(algorithm, scaling_method, sep = "_")
      line_data <- metric_data[
        metric_data$algorithm == algorithm &
          metric_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$scenario_index), ]

      graphics::polygon(
        x = c(line_data$scenario_index, rev(line_data$scenario_index)),
        y = c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
        border = NA
      )
    }

    for (i in seq_len(nrow(legend_grid))) {
      algorithm <- legend_grid$algorithm[i]
      scaling_method <- legend_grid$scaling_method[i]
      combo_key <- paste(algorithm, scaling_method, sep = "_")
      line_data <- metric_data[
        metric_data$algorithm == algorithm &
          metric_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$scenario_index), ]

      graphics::lines(
        line_data$scenario_index,
        line_data$mean,
        col = combo_colours[combo_key],
        lwd = 3
      )
      graphics::points(
        line_data$scenario_index,
        line_data$mean,
        col = combo_colours[combo_key],
        pch = scaling_symbols[scaling_method],
        cex = 1.2
      )
    }
  }

  graphics::mtext(
    paste0(
      "Imbalance Scenario Analysis - ",
      format_binary_fraction(binary_fraction),
      "; ev_xy = ",
      ev_xy_to_plot,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 2,
    cex = 1.3,
    font = 2
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
    cex = 1.1,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_cv_imbalance_by_pk <- function(binary_fraction = 0.5,
                                    ev_xy_to_plot = 0.7,
                                    ev_xx_to_plot = 0.7,
                                    file_name = "imbalance_cv_lasso_by_pk.png") {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$ev_xy == ev_xy_to_plot &
      scenario_plot_results$ev_xx == ev_xx_to_plot &
      scenario_plot_results$binary_top_fraction != 0.5,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No rare-category results found for binary_fraction = ",
      binary_fraction,
      ", ev_xy = ",
      ev_xy_to_plot,
      ", ev_xx = ",
      ev_xx_to_plot
    )
    return(invisible(NULL))
  }

  binary_top_fractions <- sort(unique(plot_data$binary_top_fraction), decreasing = TRUE)
  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 3600,
    height = 2450,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  panel_count <- length(binary_top_fractions) * length(metric_columns)
  layout_matrix <- matrix(
    seq_len(panel_count),
    nrow = length(binary_top_fractions),
    ncol = length(metric_columns),
    byrow = TRUE
  )
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(metric_columns)))

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

    for (metric_index in seq_along(metric_columns)) {
      metric_data <- plot_data[
        plot_data$binary_top_fraction == binary_top_fraction &
          plot_data$metric == metric_columns[metric_index],
      ]
      x_values <- sort(unique(metric_data$pk_imbalance_fraction))

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
          paste0(
            "Rare category = ",
            binary_top_fraction,
            "\nMean performance"
          )
        } else {
          ""
        },
        main = metric_labels[metric_index],
        xaxt = "n",
        cex.lab = 1.25,
        cex.main = 1.35,
        cex.axis = 1.15
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.1)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- metric_data[
          metric_data$algorithm == algorithm &
            metric_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]

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
        line_data <- metric_data[
          metric_data$algorithm == algorithm &
            metric_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$pk_imbalance_fraction), ]

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
          cex = 1.2
        )
      }
    }
  }

  graphics::mtext(
    paste0(
      "Imbalance Analysis by Proportion of Affected Predictors - ",
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
    "Rows show rare-category severity; x-axis shows how many binary predictors are imbalanced; shaded bands show 95% confidence intervals across seeds",
    outer = TRUE,
    side = 3,
    line = 2.2,
    cex = 0.95
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
    cex = 1.1,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

plot_scaling_analysis_by_correlation <- function(
    binary_fraction = 0.5,
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2,
    file_name = "imbalance_scaling_analysis_with_stability_by_correlation.png",
    ev_xx_subset = NULL) {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$binary_top_fraction == binary_top_fraction &
      scenario_plot_results$pk_imbalance_fraction == pk_imbalance_fraction,
  ]

  if (!is.null(ev_xx_subset)) {
    plot_data <- plot_data[plot_data$ev_xx == ev_xx_subset, ]
  }

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", binary_top_fraction = ",
      binary_top_fraction,
      ", pk_imbalance_fraction = ",
      pk_imbalance_fraction,
      if (!is.null(ev_xx_subset)) {
        paste0(", ev_xx = ", ev_xx_subset)
      } else {
        ""
      }
    )
    return(invisible(NULL))
  }

  ev_xx_values <- sort(unique(plot_data$ev_xx))
  output_file <- file.path(plot_dir, file_name)
  single_correlation_plot <- length(ev_xx_values) == 1

  grDevices::png(
    filename = output_file,
    width = 4000,
    height = if (single_correlation_plot) 1700 else 2600,
    res = 180,
    pointsize = 18
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
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, length(metric_columns)))

  graphics::layout(
    layout_matrix,
    heights = c(rep(1, length(ev_xx_values)), 0.24)
  )
  graphics::par(
    mar = c(4.8, 5.5, if (single_correlation_plot) 3.2 else 4, 1.5),
    oma = c(0, 0, if (single_correlation_plot) 4.4 else 6, 0)
  )

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

  for (row_index in seq_along(ev_xx_values)) {
    ev_xx <- ev_xx_values[row_index]

    for (metric_index in seq_along(metric_columns)) {
      metric_data <- plot_data[
        plot_data$ev_xx == ev_xx &
          plot_data$metric == metric_columns[metric_index],
      ]
      x_values <- sort(unique(metric_data$ev_xy))

      graphics::plot(
        x_values,
        rep(NA_real_, length(x_values)),
        type = "n",
        ylim = c(0, 1),
        xlab = if (row_index == length(ev_xx_values)) {
          "Explained variance / signal strength (ev_xy)"
        } else {
          ""
        },
        ylab = if (metric_index == 1) {
          paste0("ev_xx = ", ev_xx, "\nMean performance")
        } else {
          ""
        },
        main = metric_labels[metric_index],
        xaxt = "n",
        cex.lab = 1.25,
        cex.main = 1.35,
        cex.axis = 1.15
      )
      graphics::axis(1, at = x_values, labels = x_values, cex.axis = 1.1)
      graphics::grid(col = "grey88")

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- metric_data[
          metric_data$algorithm == algorithm &
            metric_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$ev_xy), ]

        graphics::polygon(
          x = c(line_data$ev_xy, rev(line_data$ev_xy)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
          border = NA
        )
      }

      for (i in seq_len(nrow(legend_grid))) {
        algorithm <- legend_grid$algorithm[i]
        scaling_method <- legend_grid$scaling_method[i]
        combo_key <- paste(algorithm, scaling_method, sep = "_")
        line_data <- metric_data[
          metric_data$algorithm == algorithm &
            metric_data$scaling_method == scaling_method,
        ]

        if (nrow(line_data) == 0) {
          next
        }

        line_data <- line_data[order(line_data$ev_xy), ]

        graphics::lines(
          line_data$ev_xy,
          line_data$mean,
          col = combo_colours[combo_key],
          lwd = 3
        )
        graphics::points(
          line_data$ev_xy,
          line_data$mean,
          col = combo_colours[combo_key],
          pch = scaling_symbols[scaling_method],
          cex = 1.2
        )
      }
    }
  }

  graphics::mtext(
    "Scaling Analysis",
    outer = TRUE,
    side = 3,
    line = if (single_correlation_plot) 3.0 else 4,
    cex = 1.35,
    font = 2
  )

  graphics::mtext(
    paste0(
      "Setup: ",
      format_binary_fraction(binary_fraction),
      "; binary_top_fraction = ",
      binary_top_fraction,
      "; pk_imbalance_fraction = ",
      pk_imbalance_fraction,
      if (!is.null(ev_xx_subset)) {
        paste0("; ev_xx = ", ev_xx_subset)
      } else {
        ""
      }
    ),
    outer = TRUE,
    side = 3,
    line = if (single_correlation_plot) 1.5 else 2.2,
    cex = 1.00
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
    cex = 1.1,
    ncol = 3,
    bty = "n"
  )

  invisible(output_file)
}

format_file_value <- function(value) {
  gsub("\\.", "_", format(value, trim = TRUE, scientific = FALSE))
}

format_metric_file_value <- function(metric_name) {
  switch(
    metric_name,
    f1_score = "f1",
    recall = "recall",
    precision = "precision",
    metric_name
  )
}

plot_scaling_analysis_by_correlation_slices <- function(
    binary_fraction = 0.5,
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2) {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$binary_top_fraction == binary_top_fraction &
      scenario_plot_results$pk_imbalance_fraction == pk_imbalance_fraction,
  ]

  if (nrow(plot_data) == 0) {
    return(character(0))
  }

  ev_xx_values <- sort(unique(plot_data$ev_xx))

  unlist(
    lapply(ev_xx_values, function(ev_xx_value) {
      plot_scaling_analysis_by_correlation(
        binary_fraction = binary_fraction,
        binary_top_fraction = binary_top_fraction,
        pk_imbalance_fraction = pk_imbalance_fraction,
        ev_xx_subset = ev_xx_value,
        file_name = paste0(
          "imbalance_scaling_analysis_with_stability_evxx_",
          format_file_value(ev_xx_value),
          ".png"
        )
      )
    }),
    use.names = FALSE
  )
}

plot_algorithm_scaling_five_panel <- function(
    binary_fraction = 0.5,
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2,
    ev_xx_to_plot = 0.6,
    metric_to_plot = "f1_score",
    file_name = "algorithm_scaling_five_panel.png") {
  plot_data <- scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction &
      scenario_plot_results$binary_top_fraction == binary_top_fraction &
      scenario_plot_results$pk_imbalance_fraction == pk_imbalance_fraction &
      scenario_plot_results$ev_xx == ev_xx_to_plot &
      scenario_plot_results$metric == metric_to_plot,
  ]

  if (nrow(plot_data) == 0) {
    warning(
      "No results found for binary_fraction = ",
      binary_fraction,
      ", binary_top_fraction = ",
      binary_top_fraction,
      ", pk_imbalance_fraction = ",
      pk_imbalance_fraction,
      ", ev_xx = ",
      ev_xx_to_plot,
      ", metric = ",
      metric_to_plot
    )
    return(invisible(NULL))
  }

  metric_label <- metric_labels[match(metric_to_plot, metric_columns)]
  if (is.na(metric_label)) {
    metric_label <- metric_to_plot
  }

  best_scaling_rows <- aggregate(
    mean ~ algorithm + scaling_method,
    data = plot_data,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  best_scaling_rows$algorithm <- factor(
    best_scaling_rows$algorithm,
    levels = algorithm_levels
  )
  best_scaling_rows <- best_scaling_rows[
    order(best_scaling_rows$algorithm, -best_scaling_rows$mean),
  ]
  best_scaling_rows <- best_scaling_rows[
    !duplicated(best_scaling_rows$algorithm),
  ]

  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 2400,
    height = 3200,
    res = 180,
    pointsize = 18
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::layout(
    matrix(c(1, 2, 3, 4, 5, 5, 6, 6), nrow = 4, byrow = TRUE),
    widths = c(1, 1),
    heights = c(1.00, 1.00, 1.25, 0.24)
  )
  graphics::par(mar = c(5.0, 5.4, 3.0, 1.6), oma = c(0, 0, 5.4, 0))

  x_values <- sort(unique(plot_data$ev_xy))

  for (algorithm in algorithm_levels) {
    algorithm_data <- plot_data[plot_data$algorithm == algorithm, ]

    graphics::plot(
      x_values,
      rep(NA_real_, length(x_values)),
      type = "n",
      ylim = c(0, 1),
      xlab = "Explained variance / signal strength (ev_xy)",
      ylab = metric_label,
      main = algorithm_labels[algorithm],
      xaxt = "n",
      cex.lab = 1.10,
      cex.main = 1.15,
      cex.axis = 1.00
    )
    graphics::axis(1, at = x_values, labels = x_values, cex.axis = 0.95)
    graphics::grid(col = "grey88")

    for (scaling_method in scaling_levels) {
      line_data <- algorithm_data[
        algorithm_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$ev_xy), ]

      graphics::polygon(
        x = c(line_data$ev_xy, rev(line_data$ev_xy)),
        y = c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(scaling_colours[scaling_method], alpha.f = 0.12),
        border = NA
      )
      graphics::lines(
        line_data$ev_xy,
        line_data$mean,
        col = scaling_colours[scaling_method],
        lwd = 3
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

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlab = "Explained variance / signal strength (ev_xy)",
    ylab = metric_label,
    main = "Best scaling per algorithm",
    xaxt = "n",
    cex.lab = 1.10,
    cex.main = 1.15,
    cex.axis = 1.00
  )
  graphics::axis(1, at = x_values, labels = x_values, cex.axis = 0.95)
  graphics::grid(col = "grey88")

  best_legend_labels <- character(0)
  best_legend_colours <- character(0)
  best_legend_symbols <- integer(0)

  for (algorithm in algorithm_levels) {
    best_row <- best_scaling_rows[best_scaling_rows$algorithm == algorithm, ]

    if (nrow(best_row) == 0) {
      next
    }

    best_scaling <- as.character(best_row$scaling_method[1])
    line_data <- plot_data[
      plot_data$algorithm == algorithm &
        plot_data$scaling_method == best_scaling,
    ]
    line_data <- line_data[order(line_data$ev_xy), ]
    combo_key <- paste(algorithm, best_scaling, sep = "_")

    graphics::polygon(
      x = c(line_data$ev_xy, rev(line_data$ev_xy)),
      y = c(line_data$ci_low, rev(line_data$ci_high)),
      col = grDevices::adjustcolor(combo_colours[combo_key], alpha.f = 0.10),
      border = NA
    )
    graphics::lines(
      line_data$ev_xy,
      line_data$mean,
      col = combo_colours[combo_key],
      lwd = 3
    )
    graphics::points(
      line_data$ev_xy,
      line_data$mean,
      col = combo_colours[combo_key],
      pch = scaling_symbols[best_scaling],
      cex = 1.1
    )

    best_legend_labels <- c(
      best_legend_labels,
      paste0(algorithm_labels[algorithm], " + ", best_scaling)
    )
    best_legend_colours <- c(best_legend_colours, combo_colours[combo_key])
    best_legend_symbols <- c(best_legend_symbols, scaling_symbols[best_scaling])
  }

  graphics::mtext(
    paste0("Scaling Choice by Algorithm - ", metric_label),
    outer = TRUE,
    side = 3,
    line = 3.4,
    cex = 1.25,
    font = 2
  )

  graphics::mtext(
    paste0(
      "Setup: ",
      format_binary_fraction(binary_fraction),
      "; binary_top_fraction = ",
      binary_top_fraction,
      "; pk_imbalance_fraction = ",
      pk_imbalance_fraction,
      "; ev_xx = ",
      ev_xx_to_plot
    ),
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 0.92
  )

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(
    0.5,
    0.94,
    "First four panels: scaling within algorithm; fifth panel: best scaling",
    cex = 0.92,
    font = 2
  )
  graphics::legend(
    x = 0.5,
    y = 0.66,
    legend = paste("Scaling:", scaling_levels),
    col = scaling_colours[scaling_levels],
    lty = 1,
    pch = scaling_symbols[scaling_levels],
    lwd = 3,
    cex = 0.86,
    ncol = 3,
    bty = "n",
    xjust = 0.5,
    yjust = 0.5
  )
  graphics::legend(
    x = 0.5,
    y = 0.38,
    legend = best_legend_labels,
    col = best_legend_colours,
    lty = 1,
    pch = best_legend_symbols,
    lwd = 3,
    cex = 0.78,
    ncol = 4,
    bty = "n",
    xjust = 0.5,
    yjust = 0.5
  )

  invisible(output_file)
}

binary_fraction_to_plot <- 0.5
available_slices <- unique(
  scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction_to_plot,
    c("ev_xy", "ev_xx")
  ]
)
available_slices <- available_slices[
  order(available_slices$ev_xx, available_slices$ev_xy),
]

created_slice_files <- unlist(
  lapply(seq_len(nrow(available_slices)), function(i) {
    ev_xy_to_plot <- available_slices$ev_xy[i]
    ev_xx_to_plot <- available_slices$ev_xx[i]

    plot_cv_imbalance_by_pk(
      binary_fraction = binary_fraction_to_plot,
      ev_xy_to_plot = ev_xy_to_plot,
      ev_xx_to_plot = ev_xx_to_plot,
      file_name = paste0(
        "imbalance_lasso_with_stability_by_pk_evxy_",
        format_file_value(ev_xy_to_plot),
        "_evxx_",
        format_file_value(ev_xx_to_plot),
        ".png"
      )
    )
  }),
  use.names = FALSE
)

available_algorithm_scenarios <- unique(
  scenario_plot_results[
    scenario_plot_results$binary_fraction == binary_fraction_to_plot,
    c("binary_top_fraction", "pk_imbalance_fraction", "ev_xx")
  ]
)
available_algorithm_scenarios <- available_algorithm_scenarios[
  order(
    available_algorithm_scenarios$ev_xx,
    available_algorithm_scenarios$binary_top_fraction,
    available_algorithm_scenarios$pk_imbalance_fraction
  ),
]

created_five_panel_files <- unlist(
  lapply(seq_len(nrow(available_algorithm_scenarios)), function(i) {
    binary_top_fraction_to_plot <- available_algorithm_scenarios$binary_top_fraction[i]
    pk_imbalance_fraction_to_plot <- available_algorithm_scenarios$pk_imbalance_fraction[i]
    ev_xx_to_plot <- available_algorithm_scenarios$ev_xx[i]

    unlist(
      lapply(metric_columns, function(metric_to_plot) {
        plot_algorithm_scaling_five_panel(
          binary_fraction = binary_fraction_to_plot,
          binary_top_fraction = binary_top_fraction_to_plot,
          pk_imbalance_fraction = pk_imbalance_fraction_to_plot,
          ev_xx_to_plot = ev_xx_to_plot,
          metric_to_plot = metric_to_plot,
          file_name = paste0(
            format_metric_file_value(metric_to_plot),
            "_bt",
            format_file_value(binary_top_fraction_to_plot),
            "_pk_",
            format_file_value(pk_imbalance_fraction_to_plot),
            "_xx",
            format_file_value(ev_xx_to_plot),
            "_fp",
            ".png"
          )
        )
      }),
      use.names = FALSE
    )
  }),
  use.names = FALSE
)

created_files <- c(
  plot_scaling_analysis_by_correlation(
    binary_fraction = binary_fraction_to_plot,
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2,
    file_name = "imbalance_scaling_analysis_with_stability_by_correlation.png"
  ),
  plot_scaling_analysis_by_correlation_slices(
    binary_fraction = binary_fraction_to_plot,
    binary_top_fraction = 0.05,
    pk_imbalance_fraction = 0.2
  ),
  created_five_panel_files,
  created_slice_files
)

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

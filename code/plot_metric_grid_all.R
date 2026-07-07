# Metric grids for LASSO and stability-selection LASSO.
#
# Layout per method:
#   columns = F1, recall, precision
#   rows    = no / low / high correlation, repeated for low and high noise
#   lines   = algorithm and scaling combinations
#   x-axis  = rarity severity by default

result_sets <- list(
  list(
    id = "mixed",
    label = "Mixed predictors",
    file = file.path("results", "imbalance_mixed_results.csv"),
    binary_fraction = 0.5
  ),
  list(
    id = "all_cat",
    label = "All categorical predictors",
    file = file.path("results", "imbalance_all_cat_results.csv"),
    binary_fraction = 1
  ),
  list(
    id = "highdim_mixed",
    label = "High-dimensional mixed predictors",
    file = file.path("results", "imbalance_highdim_mixed_results.csv"),
    binary_fraction = 0.5
  ),
  list(
    id = "highdim_all_cat",
    label = "High-dimensional all categorical predictors",
    file = file.path("results", "imbalance_highdim_all_cat_results.csv"),
    binary_fraction = 1
  )
)

required_columns <- c(
  "seed",
  "algorithm",
  "scaling_method",
  "binary_fraction",
  "binary_top_fraction",
  "pk_imbalance_fraction",
  "ev_xy",
  "ev_xx",
  "f1_score",
  "recall",
  "precision"
)

plot_dir <- file.path("plots", "metric_grid")

if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

pk_imbalance_fraction_to_plot <- 0.2
current_result_label <- NULL
binary_fraction_to_plot <- NULL
ev_xy_blocks <- c(`low noise\nevxy = 0.5` = 0.5, `high noise\nevxy = 0.05` = 0.05)
ev_xx_rows <- c(`no corr` = 0, `low corr` = 0.1, `high corr` = 0.9)

method_specs <- list(
  list(
    method_id = "lasso",
    method_label = "LASSO",
    algorithms = c("cv_lasso_min", "cv_lasso_1se")
  ),
  list(
    method_id = "stab_lasso",
    method_label = "Stability LASSO",
    algorithms = c("stability_lasso_ncut_0", "stability_lasso_ncut_3")
  )
)

metric_specs <- data.frame(
  metric = c("f1_score", "recall", "precision"),
  label = c("F1", "Recall", "Precision"),
  stringsAsFactors = FALSE
)

scaling_levels <- c("none", "zscore", "2sd")
scaling_labels <- c(none = "No scaling", zscore = "Z-score", `2sd` = "2 SD")
algorithm_labels <- c(
  cv_lasso_min = "lambda.min",
  cv_lasso_1se = "lambda.1se",
  stability_lasso_ncut_0 = "ncut=0",
  stability_lasso_ncut_3 = "ncut=3"
)
line_colours <- c(
  cv_lasso_min__none = "#1f77b4",
  cv_lasso_min__zscore = "#17becf",
  cv_lasso_min__2sd = "#9467bd",
  cv_lasso_1se__none = "#ff7f0e",
  cv_lasso_1se__zscore = "#bcbd22",
  cv_lasso_1se__2sd = "#d62728",
  stability_lasso_ncut_0__none = "#2ca02c",
  stability_lasso_ncut_0__zscore = "#20a386",
  stability_lasso_ncut_0__2sd = "#8dd3c7",
  stability_lasso_ncut_3__none = "#e377c2",
  stability_lasso_ncut_3__zscore = "#f781bf",
  stability_lasso_ncut_3__2sd = "#7f7f7f"
)

format_file_value <- function(value) {
  gsub("\\.", "_", format(value, trim = TRUE, scientific = FALSE))
}

mean_ci <- function(values) {
  values <- values[!is.na(values)]
  n_values <- length(values)

  if (n_values == 0) {
    return(c(mean = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n = 0))
  }

  mean_value <- mean(values)
  se_value <- if (n_values <= 1) 0 else stats::sd(values) / sqrt(n_values)
  ci_width <- stats::qt(0.975, df = max(n_values - 1, 1)) * se_value

  c(
    mean = mean_value,
    ci_low = max(0, mean_value - ci_width),
    ci_high = min(1, mean_value + ci_width),
    n = n_values
  )
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

  summary_rows <- lapply(split(data, split_keys), function(piece) {
    stats <- mean_ci(piece[[metric]])

    data.frame(
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_fraction = piece$binary_fraction[1],
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
      ev_xy = piece$ev_xy[1],
      ev_xx = piece$ev_xx[1],
      metric = metric,
      mean = stats[["mean"]],
      ci_low = stats[["ci_low"]],
      ci_high = stats[["ci_high"]],
      n = stats[["n"]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_rows)
}

make_rarity_plot_data <- function(data,
                                  algorithms,
                                  binary_fraction,
                                  pk_imbalance_fraction) {
  rare_rows <- data[
    data$algorithm %in% algorithms &
      data$binary_fraction == binary_fraction &
      data$pk_imbalance_fraction == pk_imbalance_fraction &
      data$binary_top_fraction != 0.5,
  ]

  baseline_rows <- data[
    data$algorithm %in% algorithms &
      data$binary_fraction == binary_fraction &
      data$binary_top_fraction == 0.5 &
      data$pk_imbalance_fraction == 0,
  ]

  plot_data <- rbind(baseline_rows, rare_rows)

  if (nrow(plot_data) == 0) {
    return(plot_data)
  }

  rarity_levels <- sort(unique(plot_data$binary_top_fraction), decreasing = TRUE)
  plot_data$rarity_x <- match(plot_data$binary_top_fraction, rarity_levels)
  plot_data$rarity_label <- ifelse(
    plot_data$binary_top_fraction == 0.5,
    "balanced",
    paste0("rare=", plot_data$binary_top_fraction)
  )

  plot_data
}

plot_metric_panel <- function(plot_data,
                              algorithms,
                              ev_xy,
                              ev_xx,
                              metric,
                              metric_label,
                              row_label,
                              show_x_label,
                              show_y_label) {
  panel_data <- plot_data[
    plot_data$ev_xy == ev_xy &
      plot_data$ev_xx == ev_xx &
      plot_data$metric == metric,
  ]

  x_values <- sort(unique(plot_data$rarity_x))
  x_labels <- plot_data$rarity_label[match(x_values, plot_data$rarity_x)]

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlim = range(x_values),
    xlab = if (show_x_label) "Rarity severity" else "",
    ylab = if (show_y_label) row_label else "",
    main = metric_label,
    xaxt = "n",
    cex.lab = 1.05,
    cex.main = 1.25,
    cex.axis = 0.95
  )

  graphics::axis(1, at = x_values, labels = x_labels, cex.axis = 0.85)
  graphics::grid(col = "grey88")

  for (algorithm in algorithms) {
    for (scaling_method in scaling_levels) {
      line_data <- panel_data[
        panel_data$algorithm == algorithm &
          panel_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$rarity_x), ]
      line_key <- paste(algorithm, scaling_method, sep = "__")
      line_colour <- line_colours[line_key]

      graphics::polygon(
        x = c(line_data$rarity_x, rev(line_data$rarity_x)),
        y = c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(line_colour, alpha.f = 0.10),
        border = NA
      )

      graphics::lines(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        lty = 1,
        lwd = 2.4
      )

      graphics::points(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        pch = 16,
        cex = 0.85
      )
    }
  }
}

plot_method_grid <- function(method_id,
                             method_label,
                             algorithms,
                             file_name = NULL) {
  plot_data <- make_rarity_plot_data(
    data = summary_results,
    algorithms = algorithms,
    binary_fraction = binary_fraction_to_plot,
    pk_imbalance_fraction = pk_imbalance_fraction_to_plot
  )

  if (nrow(plot_data) == 0) {
    warning("No plot data found for algorithm(s) = ", paste(algorithms, collapse = ", "))
    return(invisible(NULL))
  }

  available_ev_xy <- sort(unique(plot_data$ev_xy))
  missing_ev_xy <- setdiff(as.numeric(ev_xy_blocks), available_ev_xy)
  if (length(missing_ev_xy) > 0) {
    warning(
      "Skipping unavailable ev_xy value(s): ",
      paste(missing_ev_xy, collapse = ", ")
    )
  }

  ev_xy_blocks_to_plot <- ev_xy_blocks[as.numeric(ev_xy_blocks) %in% available_ev_xy]

  available_ev_xx <- sort(unique(plot_data$ev_xx))
  ev_xx_rows_to_plot <- ev_xx_rows[as.numeric(ev_xx_rows) %in% available_ev_xx]

  if (length(ev_xy_blocks_to_plot) == 0 || length(ev_xx_rows_to_plot) == 0) {
    warning(
      "No matching ev_xy / ev_xx slices found for algorithm(s) = ",
      paste(algorithms, collapse = ", ")
    )
    return(invisible(NULL))
  }

  if (is.null(file_name)) {
    file_name <- paste0(
      method_id,
      "_metric_grid_pk_",
      format_file_value(pk_imbalance_fraction_to_plot),
      ".png"
    )
  }

  output_file <- file.path(plot_dir, file_name)

  grDevices::png(
    filename = output_file,
    width = 4000,
    height = 4200,
    res = 220,
    pointsize = 16
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  n_metric <- nrow(metric_specs)
  n_rows <- length(ev_xy_blocks_to_plot) * length(ev_xx_rows_to_plot)
  panel_count <- n_rows * n_metric
  layout_matrix <- matrix(seq_len(panel_count), nrow = n_rows, ncol = n_metric, byrow = TRUE)
  layout_matrix <- rbind(layout_matrix, rep(panel_count + 1, n_metric))

  graphics::layout(layout_matrix, heights = c(rep(1, n_rows), 0.35))
  graphics::par(mar = c(4.3, 7.0, 3.2, 1.2), oma = c(0, 0, 6.2, 0))

  row_index <- 0

  for (block_index in seq_along(ev_xy_blocks_to_plot)) {
    ev_xy_value <- as.numeric(ev_xy_blocks_to_plot[block_index])
    block_label <- names(ev_xy_blocks_to_plot)[block_index]

    for (corr_index in seq_along(ev_xx_rows_to_plot)) {
      ev_xx_value <- as.numeric(ev_xx_rows_to_plot[corr_index])
      row_index <- row_index + 1

      for (metric_index in seq_len(n_metric)) {
        row_label <- paste0(
          if (metric_index == 1) paste0(block_label, "\n") else "",
          names(ev_xx_rows_to_plot)[corr_index]
        )

        plot_metric_panel(
          plot_data = plot_data,
          algorithms = algorithms,
          ev_xy = ev_xy_value,
          ev_xx = ev_xx_value,
          metric = metric_specs$metric[metric_index],
          metric_label = if (row_index == 1) metric_specs$label[metric_index] else "",
          row_label = row_label,
          show_x_label = row_index == n_rows,
          show_y_label = metric_index == 1
        )
      }
    }
  }

  graphics::mtext(
    paste0(method_label, " selection performance across rarity, correlation, and noise"),
    outer = TRUE,
    side = 3,
    line = 4.2,
    cex = 1.35,
    font = 2
  )

  graphics::mtext(
    paste0(
      "Algorithms: ",
      paste(algorithm_labels[algorithms], collapse = " and "),
      "; dataset = ",
      current_result_label,
      "; binary fraction = ",
      binary_fraction_to_plot,
      "; pk imbalance = ",
      pk_imbalance_fraction_to_plot,
      "; ribbons show 95% CI over seeds"
    ),
    outer = TRUE,
    side = 3,
    line = 2.5,
    cex = 0.88
  )

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  legend_grid <- expand.grid(
    algorithm = algorithms,
    scaling_method = scaling_levels,
    stringsAsFactors = FALSE
  )
  legend_labels <- paste(
    algorithm_labels[legend_grid$algorithm],
    "+",
    scaling_labels[legend_grid$scaling_method]
  )

  graphics::legend(
    "center",
    legend = legend_labels,
    col = line_colours[paste(legend_grid$algorithm, legend_grid$scaling_method, sep = "__")],
    pch = 16,
    lty = 1,
    lwd = 2.5,
    ncol = 3,
    bty = "n",
    cex = 0.9
  )

  invisible(output_file)
}

created_files <- character()

for (result_set in result_sets) {
  results_file <- result_set$file

  if (!file.exists(results_file)) {
    warning("Skipping missing results file: ", results_file)
    next
  }

  raw_results <- utils::read.csv(results_file)
  missing_columns <- setdiff(required_columns, names(raw_results))

  if (length(missing_columns) > 0) {
    stop(
      "The results file ",
      results_file,
      " is missing these column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  summary_results <- do.call(
    rbind,
    lapply(metric_specs$metric, function(metric) summarise_metric(raw_results, metric))
  )

  current_result_label <- result_set$label
  binary_fraction_to_plot <- result_set$binary_fraction

  created_files <- c(
    created_files,
    unlist(
      lapply(seq_along(method_specs), function(i) {
        method_spec <- method_specs[[i]]

        plot_method_grid(
          method_id = method_spec$method_id,
          method_label = method_spec$method_label,
          algorithms = method_spec$algorithms,
          file_name = paste0(
            result_set$id,
            "_",
            method_spec$method_id,
            "_metric_grid_pk_",
            format_file_value(pk_imbalance_fraction_to_plot),
            ".png"
          )
        )
      }),
      use.names = FALSE
    )
  )
}

message("Created plot files:")
for (created_file in created_files) {
  message(" - ", created_file)
}

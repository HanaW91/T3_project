# Main-result F1 plots for Research Question 1.
#
# This script keeps the main Results figures simpler than the full metric grids:
#   rows    = predictor correlation
#   columns = noise level
#   x-axis  = rarity severity
#   y-axis  = F1 score
#
# Recall and precision can stay in the appendix using the full metric plots.

result_sets <- list(
  list(
    id = "all_cat",
    label = "All categorical predictors",
    file = file.path("results", "imbalance_all_cat_results.csv"),
    binary_fraction = 1
  ),
  list(
    id = "highdim_all_cat",
    label = "High-dimensional all categorical predictors",
    file = file.path("results", "imbalance_highdim_all_cat_results.csv"),
    binary_fraction = 1
  )
)

method_specs <- list(
  list(
    method_id = "lasso",
    method_label = "LASSO",
    algorithms = c("cv_lasso_min", "cv_lasso_1se")
  ),
  list(
    method_id = "stab_lasso",
    method_label = "Stability LASSO",
    algorithms = c("ncat_null", "ncat_3")
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
  "f1_score"
)

plot_dir <- file.path("plots", "main_f1")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

pk_imbalance_fraction_to_plot <- 0.2

ev_xy_cols <- c(
  `Low noise\n(evxy = 0.5)` = 0.5,
  `Medium noise\n(evxy = 0.2)` = 0.2,
  `High noise\n(evxy = 0.05)` = 0.05
)

ev_xx_rows <- c(
  `No corr (evxx = 0)` = 0,
  `Low corr (evxx = 0.1)` = 0.1,
  `Medium corr (evxx = 0.5)` = 0.5,
  `High corr (evxx = 0.9)` = 0.9
)

scaling_methods_to_plot <- c("none", "zscore")

scaling_labels <- c(
  none = "No scaling",
  zscore = "Z-score"
)

scaling_symbols <- c(
  none = 16,
  zscore = 17
)

algorithm_labels <- c(
  cv_lasso_min = "lambda.min",
  cv_lasso_1se = "lambda.1se",
  ncat_null = "n_cat=NULL",
  ncat_3 = "n_cat=3"
)

line_colours <- c(
  cv_lasso_min__none = "#1f77b4",
  cv_lasso_min__zscore = "#17becf",
  cv_lasso_1se__none = "#ff7f0e",
  cv_lasso_1se__zscore = "#bcbd22",
  ncat_null__none = "#2ca02c",
  ncat_null__zscore = "#20a386",
  ncat_3__none = "#CC79A7",
  ncat_3__zscore = "#D65F9E"
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

summarise_f1 <- function(data) {
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

  rows <- lapply(split(data, split_keys), function(piece) {
    stats <- mean_ci(piece$f1_score)

    data.frame(
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_fraction = piece$binary_fraction[1],
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
      ev_xy = piece$ev_xy[1],
      ev_xx = piece$ev_xx[1],
      mean = stats[["mean"]],
      ci_low = stats[["ci_low"]],
      ci_high = stats[["ci_high"]],
      n = stats[["n"]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

make_rarity_plot_data <- function(data, algorithms, binary_fraction) {
  rare_rows <- data[
    data$algorithm %in% algorithms &
      data$binary_fraction == binary_fraction &
      data$pk_imbalance_fraction == pk_imbalance_fraction_to_plot &
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

plot_f1_panel <- function(plot_data,
                          algorithms,
                          ev_xy,
                          ev_xx,
                          row_label,
                          col_label,
                          show_x_label,
                          show_y_label) {
  panel_data <- plot_data[
    plot_data$ev_xy == ev_xy &
      plot_data$ev_xx == ev_xx,
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
    main = col_label,
    xaxt = "n",
    cex.lab = 1.35,
    cex.main = 1.45,
    cex.axis = 1.05
  )

  graphics::axis(1, at = x_values, labels = x_labels, cex.axis = 1.12)
  graphics::grid(col = "grey90")

  for (algorithm in algorithms) {
    for (scaling_method in scaling_methods_to_plot) {
      line_data <- panel_data[
        panel_data$algorithm == algorithm &
          panel_data$scaling_method == scaling_method,
      ]

      if (nrow(line_data) == 0 || all(is.na(line_data$mean))) {
        next
      }

      line_data <- line_data[order(line_data$rarity_x), ]
      line_data <- line_data[is.finite(line_data$mean), , drop = FALSE]

      if (nrow(line_data) == 0) {
        next
      }

      line_key <- paste(algorithm, scaling_method, sep = "__")
      line_colour <- line_colours[line_key]

      if (nrow(line_data) >= 2 && all(is.finite(line_data$ci_low)) && all(is.finite(line_data$ci_high))) {
        graphics::polygon(
          x = c(line_data$rarity_x, rev(line_data$rarity_x)),
          y = c(line_data$ci_low, rev(line_data$ci_high)),
          col = grDevices::adjustcolor(line_colour, alpha.f = 0.10),
          border = NA
        )
      }

      graphics::lines(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        lty = 1,
        lwd = 2.7
      )

      graphics::points(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        pch = scaling_symbols[scaling_method],
        cex = 1.05
      )
    }
  }
}

plot_f1_grid <- function(summary_results, result_set, method_spec) {
  plot_data <- make_rarity_plot_data(
    data = summary_results,
    algorithms = method_spec$algorithms,
    binary_fraction = result_set$binary_fraction
  )

  plot_data <- plot_data[plot_data$scaling_method %in% scaling_methods_to_plot, ]

  if (nrow(plot_data) == 0) {
    warning("No F1 plot data found for ", result_set$id, " / ", method_spec$method_id)
    return(invisible(NULL))
  }

  available_ev_xy <- sort(unique(plot_data$ev_xy))
  available_ev_xx <- sort(unique(plot_data$ev_xx))
  ev_xy_cols_to_plot <- ev_xy_cols[as.numeric(ev_xy_cols) %in% available_ev_xy]
  ev_xx_rows_to_plot <- ev_xx_rows[as.numeric(ev_xx_rows) %in% available_ev_xx]

  if (length(ev_xy_cols_to_plot) == 0 || length(ev_xx_rows_to_plot) == 0) {
    warning("No matching noise/correlation slices found for ", result_set$id)
    return(invisible(NULL))
  }

  output_file <- file.path(
    plot_dir,
    paste0(
      result_set$id,
      "_",
      method_spec$method_id,
      "_f1_main_pk_",
      format_file_value(pk_imbalance_fraction_to_plot),
      ".png"
    )
  )

  grDevices::png(
    filename = output_file,
    width = 3200,
    height = 2600,
    res = 220,
    pointsize = 14
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  n_corr <- length(ev_xx_rows_to_plot)
  n_noise <- length(ev_xy_cols_to_plot)
  layout_matrix <- rbind(
    matrix(seq_len(n_corr * n_noise), nrow = n_corr, ncol = n_noise, byrow = TRUE),
    rep(n_corr * n_noise + 1, n_noise)
  )

  graphics::layout(layout_matrix, heights = c(rep(1, n_corr), 0.35))
  graphics::par(oma = c(0, 0, 5.9, 0))

  for (corr_index in seq_along(ev_xx_rows_to_plot)) {
    for (noise_index in seq_along(ev_xy_cols_to_plot)) {
      graphics::par(mar = c(4.1, 5.8, 2.7, 1.0))

      plot_f1_panel(
        plot_data = plot_data,
        algorithms = method_spec$algorithms,
        ev_xy = as.numeric(ev_xy_cols_to_plot[noise_index]),
        ev_xx = as.numeric(ev_xx_rows_to_plot[corr_index]),
        row_label = names(ev_xx_rows_to_plot)[corr_index],
        col_label = if (corr_index == 1) names(ev_xy_cols_to_plot)[noise_index] else "",
        show_x_label = corr_index == n_corr,
        show_y_label = noise_index == 1
      )
    }
  }

  graphics::mtext(
    paste0(method_spec$method_label, " F1 selection performance across rarity, noise, and correlation"),
    outer = TRUE,
    side = 3,
    line = 4.2,
    cex = 1.45,
    font = 2
  )

  graphics::mtext(
    paste0(
      "Dataset = ",
      result_set$label,
      "; algorithms = ",
      paste(algorithm_labels[method_spec$algorithms], collapse = " and "),
      "; pk imbalance = ",
      pk_imbalance_fraction_to_plot,
      "; ribbons show 95% CI over seeds"
    ),
    outer = TRUE,
    side = 3,
    line = 2.4,
    cex = 0.95
  )

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  legend_grid <- expand.grid(
    algorithm = method_spec$algorithms,
    scaling_method = scaling_methods_to_plot,
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
    pch = scaling_symbols[legend_grid$scaling_method],
    lty = 1,
    lwd = 3.0,
    ncol = 2,
    bty = "n",
    cex = 1.18,
    pt.cex = 1.25
  )

  invisible(output_file)
}

created_files <- character()

for (result_set in result_sets) {
  if (!file.exists(result_set$file)) {
    warning("Skipping missing results file: ", result_set$file)
    next
  }

  raw_results <- utils::read.csv(result_set$file)
  missing_columns <- setdiff(required_columns, names(raw_results))

  if (length(missing_columns) > 0) {
    stop(
      "The results file ",
      result_set$file,
      " is missing these column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  summary_results <- summarise_f1(raw_results)

  for (method_spec in method_specs) {
    created_file <- plot_f1_grid(
      summary_results = summary_results,
      result_set = result_set,
      method_spec = method_spec
    )

    if (!is.null(created_file)) {
      created_files <- c(created_files, created_file)
    }
  }
}

message("Created main F1 plot files:")
for (file in created_files) {
  message(" - ", file)
}

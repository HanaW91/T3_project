# Quick plots for the toy sharp scaling check.

toy_result_sets <- list(
  list(
    id = "all_cat",
    label = "All categorical predictors",
    file = file.path("results", "toy_sharp_scaling_check_all_cat_results.csv"),
    scaling_methods = c("none", "zscore")
  ),
  list(
    id = "mixed",
    label = "Mixed predictors",
    file = file.path("results", "toy_sharp_scaling_check_mixed_results.csv"),
    scaling_methods = c("cont", "zscore", "2sd")
  )
)

plot_dir <- file.path("plots", "toy_sharp_scaling_check")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

metric_specs <- data.frame(
  metric = c("f1_score", "recall", "precision"),
  label = c("F1", "Recall", "Precision"),
  stringsAsFactors = FALSE
)

algorithm_labels <- c(ncat_null = "n_cat=NULL", ncat_3 = "n_cat=3")
scaling_labels <- c(none = "No scaling", cont = "Continuous only", zscore = "Z-score", `2sd` = "2 SD")
scaling_symbols <- c(none = 16, cont = 16, zscore = 17, `2sd` = 15)
line_colours <- c(
  ncat_null__none = "#2ca02c",
  ncat_null__cont = "#2ca02c",
  ncat_null__zscore = "#20a386",
  ncat_null__2sd = "#8dd3c7",
  ncat_3__none = "#CC79A7",
  ncat_3__cont = "#CC79A7",
  ncat_3__zscore = "#D65F9E",
  ncat_3__2sd = "#B07AA1"
)

mean_ci <- function(values) {
  values <- values[!is.na(values)]
  n_values <- length(values)
  if (n_values == 0) {
    return(c(mean = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }

  mean_value <- mean(values)
  se_value <- if (n_values <= 1) 0 else stats::sd(values) / sqrt(n_values)
  ci_width <- stats::qt(0.975, df = max(n_values - 1, 1)) * se_value

  c(
    mean = mean_value,
    ci_low = max(0, mean_value - ci_width),
    ci_high = min(1, mean_value + ci_width)
  )
}

summarise_metric <- function(data, metric) {
  split_keys <- interaction(
    data$algorithm,
    data$scaling_method,
    data$binary_top_fraction,
    data$pk_imbalance_fraction,
    data$ev_xy,
    data$ev_xx,
    drop = TRUE
  )

  rows <- lapply(split(data, split_keys), function(piece) {
    stats <- mean_ci(piece[[metric]])
    data.frame(
      algorithm = piece$algorithm[1],
      scaling_method = piece$scaling_method[1],
      binary_top_fraction = piece$binary_top_fraction[1],
      pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
      ev_xy = piece$ev_xy[1],
      ev_xx = piece$ev_xx[1],
      metric = metric,
      mean = stats[["mean"]],
      ci_low = stats[["ci_low"]],
      ci_high = stats[["ci_high"]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

make_plot_data <- function(data) {
  data <- subset(data, algorithm %in% c("ncat_null", "ncat_3"))
  summary_data <- do.call(
    rbind,
    lapply(metric_specs$metric, function(metric) summarise_metric(data, metric))
  )

  rarity_levels <- sort(unique(summary_data$binary_top_fraction), decreasing = TRUE)
  summary_data$rarity_x <- match(summary_data$binary_top_fraction, rarity_levels)
  summary_data$rarity_label <- ifelse(
    summary_data$binary_top_fraction == 0.5,
    "balanced",
    paste0("rare=", summary_data$binary_top_fraction)
  )

  summary_data
}

plot_panel <- function(plot_data, metric_name, metric_label, row_label, scaling_methods, show_y_label) {
  panel_data <- plot_data[plot_data$metric == metric_name, ]
  x_values <- sort(unique(plot_data$rarity_x))
  x_labels <- plot_data$rarity_label[match(x_values, plot_data$rarity_x)]

  graphics::plot(
    x_values,
    rep(NA_real_, length(x_values)),
    type = "n",
    ylim = c(0, 1),
    xlim = range(x_values),
    xlab = "Rarity severity",
    ylab = if (show_y_label) row_label else "",
    main = metric_label,
    xaxt = "n",
    cex.lab = 1.2,
    cex.main = 1.45,
    cex.axis = 1.0
  )
  graphics::axis(1, at = x_values, labels = x_labels, cex.axis = 0.95)
  graphics::grid(col = "grey88")

  for (algorithm in c("ncat_null", "ncat_3")) {
    for (scaling_method in scaling_methods) {
      line_data <- subset(
        panel_data,
        algorithm == algorithm & scaling_method == scaling_method
      )
      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$rarity_x), ]
      line_key <- paste(algorithm, scaling_method, sep = "__")
      line_colour <- line_colours[line_key]

      graphics::polygon(
        c(line_data$rarity_x, rev(line_data$rarity_x)),
        c(line_data$ci_low, rev(line_data$ci_high)),
        col = grDevices::adjustcolor(line_colour, alpha.f = 0.14),
        border = NA
      )
      graphics::lines(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        lwd = 3
      )
      graphics::points(
        line_data$rarity_x,
        line_data$mean,
        col = line_colour,
        pch = scaling_symbols[scaling_method],
        cex = 1.1
      )
    }
  }
}

plot_toy_result <- function(result_set) {
  if (!file.exists(result_set$file)) {
    warning("Skipping missing toy results file: ", result_set$file)
    return(NULL)
  }

  raw_results <- utils::read.csv(result_set$file)
  plot_data <- make_plot_data(raw_results)
  ev_xy <- unique(plot_data$ev_xy)[1]
  ev_xx <- unique(plot_data$ev_xx)[1]
  noise_label <- paste0("Medium noise (evxy = ", ev_xy, ")")
  corr_label <- paste0("Medium corr (evxx = ", ev_xx, ")")

  output_file <- file.path(
    plot_dir,
    paste0("toy_sharp_scaling_check_", result_set$id, ".png")
  )

  grDevices::png(output_file, width = 2600, height = 1600, res = 220, pointsize = 14)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  })

  graphics::layout(
    matrix(c(1, 1, 1, 2, 3, 4, 5, 5, 5), nrow = 3, byrow = TRUE),
    heights = c(0.42, 1, 0.34)
  )
  graphics::par(oma = c(0, 0, 5.5, 0))

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0.01, 0.08, 0.99, 0.92, border = "grey70", lwd = 1.2)
  graphics::text(
    0.5,
    0.5,
    noise_label,
    cex = 1.75,
    font = 2
  )

  for (metric_index in seq_len(nrow(metric_specs))) {
    graphics::par(mar = c(4.4, 5.4, 2.6, 1.2))
    plot_panel(
      plot_data = plot_data,
      metric_name = metric_specs$metric[metric_index],
      metric_label = metric_specs$label[metric_index],
      row_label = corr_label,
      scaling_methods = result_set$scaling_methods,
      show_y_label = metric_index == 1
    )
  }

  legend_grid <- expand.grid(
    algorithm = c("ncat_null", "ncat_3"),
    scaling_method = result_set$scaling_methods,
    stringsAsFactors = FALSE
  )
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::legend(
    "center",
    legend = paste(
      algorithm_labels[legend_grid$algorithm],
      "+",
      scaling_labels[legend_grid$scaling_method]
    ),
    col = line_colours[paste(legend_grid$algorithm, legend_grid$scaling_method, sep = "__")],
    pch = scaling_symbols[legend_grid$scaling_method],
    lty = 1,
    lwd = 3,
    ncol = length(result_set$scaling_methods),
    bty = "n",
    cex = 1.12
  )

  graphics::mtext(
    "Stability LASSO selection performance across rarity and correlation",
    outer = TRUE,
    side = 3,
    line = 3.8,
    cex = 1.45,
    font = 2
  )
  graphics::mtext(
    paste0(
      "Algorithms: ",
      paste(algorithm_labels[c("ncat_null", "ncat_3")], collapse = " and "),
      "; dataset = ",
      result_set$label,
      "; pk imbalance = 0.2; ribbons show 95% CI over seeds"
    ),
    outer = TRUE,
    side = 3,
    line = 2.2,
    cex = 0.95
  )

  output_file
}

created_files <- unlist(lapply(toy_result_sets, plot_toy_result))
cat("Created toy plot files:\n")
cat(paste0(" - ", created_files, collapse = "\n"), "\n")

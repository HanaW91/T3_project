# Shared plotting functions for metric and subgroup benchmark plots.
#
# Main updates:
# - moved metric and subgroup plotting code into this shared file
# - keep the plot scripts as short drivers
# - plot F1, recall, and precision for selection performance
# - compare sharp stability selection with n_cat = NULL and n_cat = 3

run_metric_plots <- function() {
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

  pk_imbalance_fractions_to_plot <- c(0.2)
  pk_imbalance_fraction_to_plot <- NULL
  current_result_label <- NULL
  binary_fraction_to_plot <- NULL
  ev_xy_blocks <- c(`Low noise (evxy = 0.5)` = 0.5, `Medium noise (evxy = 0.2)` = 0.2, `High noise (evxy = 0.05)` = 0.05)
  ev_xx_rows <- c(`No corr (evxx = 0)` = 0, `Low corr (evxx = 0.1)` = 0.1, `Medium corr (evxx = 0.5)` = 0.5, `High corr (evxx = 0.9)` = 0.9)

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

  metric_specs <- data.frame(
    metric = c("f1_score", "recall", "precision"),
    label = c("F1", "Recall", "Precision"),
    stringsAsFactors = FALSE
  )

  scaling_levels <- c("none", "zscore", "2sd")
  scaling_labels <- c(none = "No scaling", zscore = "Z-score", `2sd` = "2 SD")
  scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
  algorithm_labels <- c(
    cv_lasso_min = "lambda.min",
    cv_lasso_1se = "lambda.1se",
    ncat_null = "n_cat=NULL",
    ncat_3 = "n_cat=3"
  )
  line_colours <- c(
    cv_lasso_min__none = "#1f77b4",
    cv_lasso_min__zscore = "#17becf",
    cv_lasso_min__2sd = "#9467bd",
    cv_lasso_1se__none = "#ff7f0e",
    cv_lasso_1se__zscore = "#bcbd22",
    cv_lasso_1se__2sd = "#d62728",
    ncat_null__none = "#2ca02c",
    ncat_null__zscore = "#20a386",
    ncat_null__2sd = "#8dd3c7",
    ncat_3__none = "#CC79A7",
    ncat_3__zscore = "#D65F9E",
    ncat_3__2sd = "#B07AA1"
  )

  format_file_value <- function(value) {
    gsub("\\.", "_", format(value, trim = TRUE, scientific = FALSE))
  }

  format_noise_id <- function(noise_label) {
    noise_id <- sub(" noise.*", "", tolower(noise_label))
    gsub("[^a-z0-9]+", "_", noise_id)
  }

  scaling_levels_for_binary_fraction <- function(binary_fraction) {
    if (binary_fraction == 1) {
      return(c("none", "zscore"))
    }

    scaling_levels
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
                                show_y_label,
                                scaling_methods_to_plot) {
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
      for (scaling_method in scaling_methods_to_plot) {
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
          pch = scaling_symbols[scaling_method],
          cex = 0.85
        )
      }
    }
  }

  plot_method_grid <- function(method_id,
                               method_label,
                               algorithms,
                               ev_xy_block,
                               file_name = NULL) {
    plot_data <- make_rarity_plot_data(
      data = summary_results,
      algorithms = algorithms,
      binary_fraction = binary_fraction_to_plot,
      pk_imbalance_fraction = pk_imbalance_fraction_to_plot
    )
    scaling_methods_to_plot <- scaling_levels_for_binary_fraction(binary_fraction_to_plot)
    plot_data <- plot_data[plot_data$scaling_method %in% scaling_methods_to_plot, ]

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

    ev_xy_blocks_to_plot <- ev_xy_block[as.numeric(ev_xy_block) %in% available_ev_xy]

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
        "_pk_",
        format_file_value(pk_imbalance_fraction_to_plot),
        "_",
        format_noise_id(names(ev_xy_block)),
        ".png"
      )
    }

    output_file <- file.path(plot_dir, file_name)

    grDevices::png(
      filename = output_file,
      width = 3000,
      height = 3600,
      res = 220,
      pointsize = 14
    )

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
      graphics::par(old_par)
      grDevices::dev.off()
    })

    n_metric <- nrow(metric_specs)
    n_blocks <- length(ev_xy_blocks_to_plot)
    n_corr <- length(ev_xx_rows_to_plot)
    n_panel_rows <- n_blocks * n_corr

    layout_rows <- list()
    next_figure_id <- 1

    for (block_index in seq_len(n_blocks)) {
      header_id <- next_figure_id
      next_figure_id <- next_figure_id + 1
      layout_rows[[length(layout_rows) + 1]] <- rep(header_id, n_metric)

      for (corr_index in seq_len(n_corr)) {
        panel_ids <- next_figure_id:(next_figure_id + n_metric - 1)
        next_figure_id <- next_figure_id + n_metric
        layout_rows[[length(layout_rows) + 1]] <- panel_ids
      }
    }

    legend_id <- next_figure_id
    layout_rows[[length(layout_rows) + 1]] <- rep(legend_id, n_metric)
    layout_matrix <- do.call(rbind, layout_rows)
    row_heights <- c(rep(c(0.42, rep(1, n_corr)), n_blocks), 0.58)

    graphics::layout(layout_matrix, heights = row_heights)
    graphics::par(oma = c(0, 0, 7.2, 0))

    row_index <- 0

    for (block_index in seq_along(ev_xy_blocks_to_plot)) {
      ev_xy_value <- as.numeric(ev_xy_blocks_to_plot[block_index])
      block_label <- names(ev_xy_blocks_to_plot)[block_index]

      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::rect(
        xleft = 0.01,
        ybottom = 0.08,
        xright = 0.99,
        ytop = 0.92,
        border = "grey70",
        lwd = 1.2
      )
      graphics::text(
        x = 0.5,
        y = 0.5,
        labels = block_label,
        cex = 1.95,
        font = 2
      )

      for (corr_index in seq_along(ev_xx_rows_to_plot)) {
        ev_xx_value <- as.numeric(ev_xx_rows_to_plot[corr_index])
        row_index <- row_index + 1

        for (metric_index in seq_len(n_metric)) {
          row_label <- names(ev_xx_rows_to_plot)[corr_index]

          graphics::par(mar = c(3.7, 5.6, 2.2, 0.9))

          plot_metric_panel(
            plot_data = plot_data,
            algorithms = algorithms,
            ev_xy = ev_xy_value,
            ev_xx = ev_xx_value,
            metric = metric_specs$metric[metric_index],
            metric_label = if (row_index == 1) metric_specs$label[metric_index] else "",
            row_label = row_label,
            show_x_label = row_index == n_panel_rows,
            show_y_label = metric_index == 1,
            scaling_methods_to_plot = scaling_methods_to_plot
          )
        }
      }
    }

    graphics::mtext(
      paste0(method_label, " selection performance across rarity and correlation"),
      outer = TRUE,
      side = 3,
      line = 5.4,
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
      line = 3.5,
      cex = 0.88
    )

    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    legend_grid <- expand.grid(
      algorithm = algorithms,
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
      ncol = 3,
      bty = "n",
      cex = 1.18,
      pt.cex = 1.25
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

    for (pk_value in pk_imbalance_fractions_to_plot) {
      pk_imbalance_fraction_to_plot <- pk_value

      for (ev_xy_index in seq_along(ev_xy_blocks)) {
        ev_xy_block <- ev_xy_blocks[ev_xy_index]

        created_files <- c(
          created_files,
          unlist(
            lapply(seq_along(method_specs), function(i) {
              method_spec <- method_specs[[i]]

              plot_method_grid(
                method_id = method_spec$method_id,
                method_label = method_spec$method_label,
                algorithms = method_spec$algorithms,
                ev_xy_block = ev_xy_block,
                file_name = paste0(
                  result_set$id,
                  "_",
                  method_spec$method_id,
                  "_",
                  format_noise_id(names(ev_xy_block)),
                  "_noise_pk_",
                  format_file_value(pk_imbalance_fraction_to_plot),
                  ".png"
                )
              )
            }),
            use.names = FALSE
          )
        )
      }
    }
  }

  message("Created plot files:")
  for (created_file in created_files) {
    message(" - ", created_file)
  }
}

run_subgroup_plots <- function() {
  # Subgroup selection-performance grids.
  #
  # These plots diagnose whether performance differs by predictor group:
  #   rare vs non-rare binary predictors
  #   binary vs continuous predictors
  #
  # One plot is created for each dataset, method family (LASSO or stability
  # LASSO), subgroup comparison, and pk imbalance level. Algorithm settings
  # and scaling methods are shown together within each plot.

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

  plot_dir <- file.path("plots", "subgroup")

  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }

  pk_imbalance_fractions_to_plot <- c(0.2)
  ev_xy_blocks <- c(
    `Low noise (evxy = 0.5)` = 0.5,
    `Medium noise (evxy = 0.2)` = 0.2,
    `High noise (evxy = 0.05)` = 0.05
  )
  ev_xx_rows <- c(
    `No corr (evxx = 0)` = 0,
    `Low corr (evxx = 0.1)` = 0.1,
    `Medium corr (evxx = 0.5)` = 0.5,
    `High corr (evxx = 0.9)` = 0.9
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

  metric_specs <- data.frame(
    metric = c("f1_score", "recall", "precision"),
    label = c("F1", "Recall", "Precision"),
    stringsAsFactors = FALSE
  )

  subgroup_specs <- list(
    list(
      id = "rare_vs_nonrare",
      label = "Rare vs non-rare binary predictors",
      groups = data.frame(
        group = c("rare_binary", "nonrare_binary"),
        label = c("Rare binary", "Non-rare binary"),
        colour_none = c("#D55E00", "#0072B2"),
        colour_zscore = c("#E69F00", "#56B4E9"),
        colour_2sd = c("#A65E00", "#004C8C"),
        stringsAsFactors = FALSE
      )
    ),
    list(
      id = "binary_vs_continuous",
      label = "Binary vs continuous predictors",
      groups = data.frame(
        group = c("binary", "continuous"),
        label = c("Binary", "Continuous"),
        colour_none = c("#009E73", "#CC79A7"),
        colour_zscore = c("#66C2A5", "#F781BF"),
        colour_2sd = c("#006D4F", "#984EA3"),
        stringsAsFactors = FALSE
      )
    )
  )

  scaling_levels <- c("none", "zscore", "2sd")
  scaling_labels <- c(none = "No scaling", zscore = "Z-score", `2sd` = "2 SD")
  scaling_symbols <- c(none = 16, zscore = 17, `2sd` = 15)
  algorithm_labels <- c(
    cv_lasso_min = "lambda.min",
    cv_lasso_1se = "lambda.1se",
    ncat_null = "n_cat=NULL",
    ncat_3 = "n_cat=3"
  )
  algorithm_line_types <- c(
    cv_lasso_min = 1,
    cv_lasso_1se = 2,
    ncat_null = 1,
    ncat_3 = 2
  )

  required_base_columns <- c(
    "seed",
    "algorithm",
    "scaling_method",
    "binary_fraction",
    "binary_top_fraction",
    "pk_imbalance_fraction",
    "ev_xy",
    "ev_xx"
  )

  format_file_value <- function(value) {
    gsub("\\.", "_", format(value, trim = TRUE, scientific = FALSE))
  }

  format_noise_id <- function(noise_label) {
    noise_id <- sub(" noise.*", "", tolower(noise_label))
    gsub("[^a-z0-9]+", "_", noise_id)
  }

  scaling_levels_for_binary_fraction <- function(binary_fraction) {
    if (binary_fraction == 1) {
      return(c("none", "zscore"))
    }

    scaling_levels
  }

  subgroup_scaling_colour <- function(subgroup_spec, group_index, scaling_method) {
    colour_column <- paste0("colour_", scaling_method)

    if (!colour_column %in% names(subgroup_spec$groups)) {
      colour_column <- "colour_none"
    }

    subgroup_spec$groups[[colour_column]][group_index]
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

  subgroup_metric_column <- function(group, metric) {
    paste(group, metric, sep = "_")
  }

  summarise_subgroup_metric <- function(data, subgroup_spec, metric) {
    rows <- list()

    for (group_index in seq_len(nrow(subgroup_spec$groups))) {
      group_id <- subgroup_spec$groups$group[group_index]
      metric_column <- subgroup_metric_column(group_id, metric)

      if (!metric_column %in% names(data)) {
        next
      }

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

      group_rows <- lapply(split(data, split_keys), function(piece) {
        stats <- mean_ci(piece[[metric_column]])

        data.frame(
          algorithm = piece$algorithm[1],
          scaling_method = piece$scaling_method[1],
          binary_fraction = piece$binary_fraction[1],
          binary_top_fraction = piece$binary_top_fraction[1],
          pk_imbalance_fraction = piece$pk_imbalance_fraction[1],
          ev_xy = piece$ev_xy[1],
          ev_xx = piece$ev_xx[1],
          subgroup = group_id,
          subgroup_label = subgroup_spec$groups$label[group_index],
          subgroup_colour = subgroup_spec$groups$colour_none[group_index],
          metric = metric,
          mean = stats[["mean"]],
          ci_low = stats[["ci_low"]],
          ci_high = stats[["ci_high"]],
          n = stats[["n"]],
          stringsAsFactors = FALSE
        )
      })

      rows <- c(rows, group_rows)
    }

    if (length(rows) == 0) {
      return(data.frame())
    }

    do.call(rbind, rows)
  }

  make_rarity_plot_data <- function(data,
                                    algorithms,
                                    scaling_method,
                                    binary_fraction,
                                    pk_imbalance_fraction) {
    rare_rows <- data[
      data$algorithm %in% algorithms &
        data$scaling_method %in% scaling_method &
        data$binary_fraction == binary_fraction &
        data$pk_imbalance_fraction == pk_imbalance_fraction &
        data$binary_top_fraction != 0.5,
    ]

    baseline_rows <- data[
      data$algorithm %in% algorithms &
        data$scaling_method %in% scaling_method &
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

  plot_subgroup_panel <- function(plot_data,
                                  ev_xy,
                                  ev_xx,
                                  metric,
                                  metric_label,
                                  row_label,
                                  show_x_label,
                                  show_y_label,
                                  subgroup_spec,
                                  algorithms,
                                  scaling_methods_to_plot) {
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
      cex.lab = 1.0,
      cex.main = 1.2,
      cex.axis = 0.86
    )

    graphics::axis(1, at = x_values, labels = x_labels, cex.axis = 0.78)
    graphics::grid(col = "grey88")

    for (group_index in seq_len(nrow(subgroup_spec$groups))) {
      group_id <- subgroup_spec$groups$group[group_index]

      for (algorithm in algorithms) {
        for (scaling_method in scaling_methods_to_plot) {
          line_data <- panel_data[
            panel_data$subgroup == group_id &
              panel_data$algorithm == algorithm &
              panel_data$scaling_method == scaling_method,
          ]

          if (nrow(line_data) == 0 || all(is.na(line_data$mean))) {
            next
          }

          line_data <- line_data[order(line_data$rarity_x), ]
          line_type <- algorithm_line_types[algorithm]
          line_colour <- subgroup_scaling_colour(
            subgroup_spec = subgroup_spec,
            group_index = group_index,
            scaling_method = scaling_method
          )

          graphics::polygon(
            x = c(line_data$rarity_x, rev(line_data$rarity_x)),
            y = c(line_data$ci_low, rev(line_data$ci_high)),
            col = grDevices::adjustcolor(line_colour, alpha.f = 0.04),
            border = NA
          )

          graphics::lines(
            line_data$rarity_x,
            line_data$mean,
            col = line_colour,
            lty = line_type,
            lwd = 2.1
          )

          graphics::points(
            line_data$rarity_x,
            line_data$mean,
            col = line_colour,
            pch = scaling_symbols[scaling_method],
            cex = 0.80
          )
        }
      }
    }
  }

  plot_subgroup_grid <- function(summary_results,
                                 result_set,
                                 method_spec,
                                 subgroup_spec,
                                 pk_imbalance_fraction,
                                 ev_xy_block) {
    scaling_methods_to_plot <- scaling_levels_for_binary_fraction(result_set$binary_fraction)
    plot_data <- make_rarity_plot_data(
      data = summary_results,
      algorithms = method_spec$algorithms,
      scaling_method = scaling_methods_to_plot,
      binary_fraction = result_set$binary_fraction,
      pk_imbalance_fraction = pk_imbalance_fraction
    )

    if (nrow(plot_data) == 0 || all(is.na(plot_data$mean))) {
      warning(
        "No subgroup plot data for ",
        result_set$id,
        ", ",
        subgroup_spec$id,
        ", pk=",
        pk_imbalance_fraction
      )
      return(invisible(NULL))
    }

    available_ev_xy <- sort(unique(plot_data$ev_xy))
    ev_xy_blocks_to_plot <- ev_xy_block[as.numeric(ev_xy_block) %in% available_ev_xy]
    available_ev_xx <- sort(unique(plot_data$ev_xx))
    ev_xx_rows_to_plot <- ev_xx_rows[as.numeric(ev_xx_rows) %in% available_ev_xx]

    if (length(ev_xy_blocks_to_plot) == 0 || length(ev_xx_rows_to_plot) == 0) {
      return(invisible(NULL))
    }

    file_name <- paste0(
      result_set$id,
      "_",
      method_spec$method_id,
      "_",
      subgroup_spec$id,
      "_",
      format_noise_id(names(ev_xy_block)),
      "_noise",
      "_pk_",
      format_file_value(pk_imbalance_fraction),
      ".png"
    )
    file_name <- gsub("[^A-Za-z0-9_\\.\\-]+", "_", file_name)
    output_file <- file.path(plot_dir, file_name)

    grDevices::png(
      filename = output_file,
      width = 3000,
      height = 3600,
      res = 220,
      pointsize = 14
    )

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
      graphics::par(old_par)
      grDevices::dev.off()
    })

    n_metric <- nrow(metric_specs)
    n_blocks <- length(ev_xy_blocks_to_plot)
    n_corr <- length(ev_xx_rows_to_plot)
    n_panel_rows <- n_blocks * n_corr

    layout_rows <- list()
    next_figure_id <- 1

    for (block_index in seq_len(n_blocks)) {
      header_id <- next_figure_id
      next_figure_id <- next_figure_id + 1
      layout_rows[[length(layout_rows) + 1]] <- rep(header_id, n_metric)

      for (corr_index in seq_len(n_corr)) {
        panel_ids <- next_figure_id:(next_figure_id + n_metric - 1)
        next_figure_id <- next_figure_id + n_metric
        layout_rows[[length(layout_rows) + 1]] <- panel_ids
      }
    }

    legend_id <- next_figure_id
    layout_rows[[length(layout_rows) + 1]] <- rep(legend_id, n_metric)
    layout_matrix <- do.call(rbind, layout_rows)
    row_heights <- c(rep(c(0.42, rep(1, n_corr)), n_blocks), 0.58)

    graphics::layout(layout_matrix, heights = row_heights)
    graphics::par(oma = c(0, 0, 7.2, 0))

    row_index <- 0

    for (block_index in seq_along(ev_xy_blocks_to_plot)) {
      ev_xy_value <- as.numeric(ev_xy_blocks_to_plot[block_index])
      block_label <- names(ev_xy_blocks_to_plot)[block_index]

      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::rect(0.01, 0.08, 0.99, 0.92, border = "grey70", lwd = 1.2)
      graphics::text(0.5, 0.5, labels = block_label, cex = 1.95, font = 2)

      for (corr_index in seq_along(ev_xx_rows_to_plot)) {
        ev_xx_value <- as.numeric(ev_xx_rows_to_plot[corr_index])
        row_index <- row_index + 1

        for (metric_index in seq_len(n_metric)) {
          graphics::par(mar = c(3.7, 5.6, 2.2, 0.9))

          plot_subgroup_panel(
            plot_data = plot_data,
            ev_xy = ev_xy_value,
            ev_xx = ev_xx_value,
            metric = metric_specs$metric[metric_index],
            metric_label = if (row_index == 1) metric_specs$label[metric_index] else "",
            row_label = names(ev_xx_rows_to_plot)[corr_index],
            show_x_label = row_index == n_panel_rows,
            show_y_label = metric_index == 1,
            subgroup_spec = subgroup_spec,
            algorithms = method_spec$algorithms,
            scaling_methods_to_plot = scaling_methods_to_plot
          )
        }
      }
    }

    graphics::mtext(
      paste0(subgroup_spec$label, " in ", method_spec$method_label),
      outer = TRUE,
      side = 3,
      line = 5.4,
      cex = 1.35,
      font = 2
    )

    graphics::mtext(
      paste0(
        "Algorithms: ",
        paste(algorithm_labels[method_spec$algorithms], collapse = " and "),
        "; scaling methods shown together",
        "; dataset = ",
        result_set$label,
        "; pk imbalance = ",
        pk_imbalance_fraction,
        "; ribbons show 95% CI over seeds"
      ),
      outer = TRUE,
      side = 3,
      line = 3.5,
      cex = 0.88
    )

    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    legend_grid <- expand.grid(
      subgroup_index = seq_len(nrow(subgroup_spec$groups)),
      algorithm = method_spec$algorithms,
      scaling_method = scaling_methods_to_plot,
      stringsAsFactors = FALSE
    )
    legend_labels <- paste(
      subgroup_spec$groups$label[legend_grid$subgroup_index],
      "+",
      algorithm_labels[legend_grid$algorithm],
      "+",
      scaling_labels[legend_grid$scaling_method]
    )
    legend_colours <- mapply(
      function(group_index, scaling_method) {
        subgroup_scaling_colour(
          subgroup_spec = subgroup_spec,
          group_index = group_index,
          scaling_method = scaling_method
        )
      },
      group_index = legend_grid$subgroup_index,
      scaling_method = legend_grid$scaling_method
    )
    graphics::legend(
      "center",
      legend = legend_labels,
      col = legend_colours,
      pch = scaling_symbols[legend_grid$scaling_method],
      lty = algorithm_line_types[legend_grid$algorithm],
      lwd = 3.0,
      ncol = 3,
      bty = "n",
      cex = 0.86,
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
    missing_columns <- setdiff(required_base_columns, names(raw_results))

    if (length(missing_columns) > 0) {
      stop(
        "The results file ",
        result_set$file,
        " is missing these column(s): ",
        paste(missing_columns, collapse = ", ")
      )
    }

    for (subgroup_spec in subgroup_specs) {
      if (subgroup_spec$id == "binary_vs_continuous" && result_set$binary_fraction == 1) {
        next
      }

      subgroup_columns <- unlist(lapply(
        subgroup_spec$groups$group,
        function(group) subgroup_metric_column(group, metric_specs$metric)
      ))

      if (!all(subgroup_columns %in% names(raw_results))) {
        warning(
          "Skipping ",
          subgroup_spec$id,
          " for ",
          result_set$id,
          " because required subgroup columns are missing."
        )
        next
      }

      summary_results <- do.call(
        rbind,
        lapply(metric_specs$metric, function(metric) {
          summarise_subgroup_metric(raw_results, subgroup_spec, metric)
        })
      )

      if (nrow(summary_results) == 0 || all(is.na(summary_results$mean))) {
        warning("Skipping empty subgroup comparison: ", result_set$id, " / ", subgroup_spec$id)
        next
      }

      for (method_spec in method_specs) {
        for (pk_value in pk_imbalance_fractions_to_plot) {
          for (ev_xy_index in seq_along(ev_xy_blocks)) {
            created_file <- plot_subgroup_grid(
              summary_results = summary_results,
              result_set = result_set,
              method_spec = method_spec,
              subgroup_spec = subgroup_spec,
              pk_imbalance_fraction = pk_value,
              ev_xy_block = ev_xy_blocks[ev_xy_index]
            )

            if (!is.null(created_file)) {
              created_files <- c(created_files, created_file)
            }
          }
        }
      }
    }
  }

  message("Created subgroup plot files:")
  for (created_file in created_files) {
    message(" - ", created_file)
  }
}

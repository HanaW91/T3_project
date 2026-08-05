# Sanity check for the realised signal R-squared in the simulated datasets.
# This checks whether the generated outcome strength is close to the target ev_xy.

result_sets <- data.frame(
  dataset_id = c("mixed", "all_cat", "highdim_mixed", "highdim_all_cat"),
  dataset_label = c(
    "Mixed predictors",
    "All categorical predictors",
    "High-dimensional mixed predictors",
    "High-dimensional all categorical predictors"
  ),
  file = file.path(
    "results",
    c(
      "imbalance_mixed_results.csv",
      "imbalance_all_cat_results.csv",
      "imbalance_highdim_mixed_results.csv",
      "imbalance_highdim_all_cat_results.csv"
    )
  ),
  stringsAsFactors = FALSE
)

plot_dir <- file.path("plots", "sanity_check")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

summarise_realised_signal <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n == 0) {
    return(c(n = 0, mean = NA_real_, median = NA_real_, sd = NA_real_, se = NA_real_, iqr_low = NA_real_, iqr_high = NA_real_))
  }

  x_mean <- mean(x)
  x_median <- stats::median(x)
  x_sd <- if (n > 1) stats::sd(x) else NA_real_
  x_se <- if (n > 1) x_sd / sqrt(n) else NA_real_
  x_iqr_bounds <- stats::quantile(
    x,
    probs = c(0.25, 0.75),
    names = FALSE,
    type = 7
  )

  c(
    n = n,
    mean = x_mean,
    median = x_median,
    sd = x_sd,
    se = x_se,
    iqr_low = x_iqr_bounds[1],
    iqr_high = x_iqr_bounds[2]
  )
}

noise_labels <- c(
  "0.5" = "Low noise\n(evxy = 0.5)",
  "0.2" = "Medium noise\n(evxy = 0.2)",
  "0.05" = "High noise\n(evxy = 0.05)"
)

correlation_labels <- c(
  "0" = "No corr (evxx = 0)",
  "0.1" = "Low corr (evxx = 0.1)",
  "0.5" = "Medium corr (evxx = 0.5)",
  "0.9" = "High corr (evxx = 0.9)"
)

rarity_levels <- c(0.5, 0.2, 0.1, 0.05)
rarity_labels <- c(
  "0.5" = "balanced",
  "0.2" = "rare=0.2",
  "0.1" = "rare=0.1",
  "0.05" = "rare=0.05"
)

rarity_colours <- c(
  "0.5" = "#1f78b4",
  "0.2" = "#33a02c",
  "0.1" = "#ff7f00",
  "0.05" = "#6a3d9a"
)

read_signal_data <- function(info) {
  if (!file.exists(info$file)) {
    warning("Skipping missing results file: ", info$file, call. = FALSE)
    return(NULL)
  }

  dat <- utils::read.csv(info$file, stringsAsFactors = FALSE)
  required <- c("seed", "binary_top_fraction", "pk_imbalance_fraction", "ev_xy", "ev_xx")
  missing_required <- setdiff(required, names(dat))
  if (length(missing_required) > 0) {
    warning(
      "Skipping ", info$file, " because it is missing: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
    return(NULL)
  }

  if ("signal_r_squared" %in% names(dat)) {
    dat$realised_r_squared <- dat$signal_r_squared
  } else if ("r_squared" %in% names(dat)) {
    warning(
      "Using r_squared from ", info$file,
      " because signal_r_squared was not found. This is mainly for older result files.",
      call. = FALSE
    )
    dat$realised_r_squared <- dat$r_squared
  } else {
    warning("Skipping ", info$file, " because no R-squared column was found.", call. = FALSE)
    return(NULL)
  }

  dedup_cols <- intersect(
    c(
      "seed", "n", "p", "binary_fraction", "binary_top_fraction",
      "pk_imbalance_fraction", "ev_xy", "ev_xx", "realised_r_squared"
    ),
    names(dat)
  )

  dat <- unique(dat[dedup_cols])
  dat$dataset_id <- info$dataset_id
  dat$dataset_label <- info$dataset_label
  dat
}

summarise_signal_data <- function(dat) {
  groups <- split(
    dat,
    interaction(
      dat$dataset_id,
      dat$dataset_label,
      dat$binary_top_fraction,
      dat$pk_imbalance_fraction,
      dat$ev_xy,
      dat$ev_xx,
      drop = TRUE
    )
  )

  out <- do.call(rbind, lapply(groups, function(group) {
    signal_summary <- summarise_realised_signal(group$realised_r_squared)
    data.frame(
      dataset_id = group$dataset_id[1],
      dataset_label = group$dataset_label[1],
      binary_top_fraction = group$binary_top_fraction[1],
      pk_imbalance_fraction = group$pk_imbalance_fraction[1],
      ev_xy = group$ev_xy[1],
      ev_xx = group$ev_xx[1],
      n_seeds = unname(signal_summary[["n"]]),
      mean_realised_r_squared = unname(signal_summary[["mean"]]),
      median_realised_r_squared = unname(signal_summary[["median"]]),
      sd_realised_r_squared = unname(signal_summary[["sd"]]),
      se_realised_r_squared = unname(signal_summary[["se"]]),
      iqr_low = unname(signal_summary[["iqr_low"]]),
      iqr_high = unname(signal_summary[["iqr_high"]]),
      difference_from_target = unname(signal_summary[["mean"]]) - group$ev_xy[1],
      abs_difference_from_target = abs(unname(signal_summary[["mean"]]) - group$ev_xy[1]),
      stringsAsFactors = FALSE
    )
  }))

  row.names(out) <- NULL
  out[order(out$dataset_id, out$ev_xx, out$ev_xy, out$binary_top_fraction), ]
}

plot_signal_check <- function(summary_data, dataset_id, dataset_label) {
  dat <- subset(summary_data, dataset_id == dataset_id)
  if (nrow(dat) == 0) {
    return(NULL)
  }

  ev_xx_values <- sort(unique(dat$ev_xx))
  ev_xy_values <- sort(unique(dat$ev_xy))
  y_max <- max(c(dat$iqr_high, dat$ev_xy), na.rm = TRUE)
  y_limit <- c(0, min(1, max(0.6, y_max * 1.08)))

  output_file <- file.path(plot_dir, paste0(dataset_id, "_signal_r_squared_sanity_check.png"))
  grDevices::png(output_file, width = 2600, height = 1900, res = 180)
  on.exit(grDevices::dev.off(), add = TRUE)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mfrow = c(2, 2), oma = c(5, 5.5, 5.5, 1), mar = c(4.2, 4.8, 3.2, 1))

  for (ev_xx_value in ev_xx_values) {
    panel <- subset(dat, ev_xx == ev_xx_value)

    plot(
      NA,
      xlim = range(ev_xy_values),
      ylim = y_limit,
      xaxt = "n",
      xlab = "",
      ylab = "",
      main = correlation_labels[as.character(ev_xx_value)],
      cex.main = 1.25
    )
    axis(1, at = ev_xy_values, labels = names(noise_labels)[match(ev_xy_values, as.numeric(names(noise_labels)))])
    grid(col = "#e6e6e6", lty = 1)
    abline(a = 0, b = 1, col = "#333333", lty = 2, lwd = 1.4)

    for (rarity_value in rarity_levels) {
      line_data <- subset(panel, binary_top_fraction == rarity_value)
      if (nrow(line_data) == 0) {
        next
      }

      line_data <- line_data[order(line_data$ev_xy), ]
      colour <- rarity_colours[as.character(rarity_value)]

      polygon(
        c(line_data$ev_xy, rev(line_data$ev_xy)),
        c(line_data$iqr_low, rev(line_data$iqr_high)),
        col = grDevices::adjustcolor(colour, alpha.f = 0.16),
        border = NA
      )
      lines(
        line_data$ev_xy,
        line_data$mean_realised_r_squared,
        col = colour,
        lwd = 2.2,
        type = "b",
        pch = 16
      )
    }
  }

  mtext("Target signal level (evxy)", side = 1, outer = TRUE, line = 2.8, cex = 1.2)
  mtext("Realised signal R-squared", side = 2, outer = TRUE, line = 3.5, cex = 1.2)
  mtext(
    paste0("Signal R-squared sanity check: ", dataset_label),
    side = 3,
    outer = TRUE,
    line = 2.6,
    cex = 1.55,
    font = 2
  )
  mtext(
    "Dashed line shows target evxy; ribbons show IQR over seeds",
    side = 3,
    outer = TRUE,
    line = 0.8,
    cex = 1.0
  )

  legend(
    "bottom",
    inset = -0.21,
    xpd = NA,
    horiz = TRUE,
    bty = "n",
    title = "Rarity severity",
    legend = unname(rarity_labels[as.character(rarity_levels)]),
    col = rarity_colours[as.character(rarity_levels)],
    pch = 16,
    lwd = 2.2,
    cex = 0.95
  )

  output_file
}

loaded <- do.call(rbind, lapply(seq_len(nrow(result_sets)), function(i) {
  read_signal_data(result_sets[i, ])
}))

if (is.null(loaded) || nrow(loaded) == 0) {
  stop("No usable result files found for the signal R-squared sanity check.")
}

summary_data <- summarise_signal_data(loaded)
summary_file <- file.path("results", "signal_r_squared_sanity_summary.csv")
utils::write.csv(summary_data, summary_file, row.names = FALSE)

plot_files <- unlist(lapply(unique(summary_data$dataset_id), function(dataset_id) {
  dataset_label <- unique(summary_data$dataset_label[summary_data$dataset_id == dataset_id])[1]
  plot_signal_check(summary_data, dataset_id, dataset_label)
}))

cat("Created signal R-squared sanity outputs:\n")
cat(" - ", summary_file, "\n", sep = "")
for (plot_file in plot_files) {
  cat(" - ", plot_file, "\n", sep = "")
}

cat("\nLargest average absolute differences from target evxy:\n")
print(utils::head(
  summary_data[order(-summary_data$abs_difference_from_target), c(
    "dataset_label", "binary_top_fraction", "ev_xy", "ev_xx",
    "mean_realised_r_squared", "difference_from_target", "n_seeds"
  )],
  12
))

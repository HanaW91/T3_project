# Merge seed-batch outputs after parallel HPC runs.

source(file.path("code", "simulation_functions.R"))

merge_seed_batch_outputs <- function(output_prefix,
                                     write_metadata = TRUE,
                                     results_dir = "results") {
  result_pattern <- paste0("^", output_prefix, "_seed_[0-9]{3}_[0-9]{3}_results\\.csv$")
  result_files <- list.files(results_dir, pattern = result_pattern, full.names = TRUE)

  if (length(result_files) == 0) {
    warning("No seed-batch result files found for ", output_prefix)
    return(invisible(NULL))
  }

  message("Merging ", length(result_files), " result files for ", output_prefix)

  result_files <- sort(result_files)
  merged_results <- do.call(rbind, lapply(result_files, utils::read.csv))
  merged_summary <- summarise_imbalance_results(merged_results)

  utils::write.csv(
    merged_results,
    file = file.path(results_dir, paste0(output_prefix, "_results.csv")),
    row.names = FALSE
  )

  utils::write.csv(
    merged_summary,
    file = file.path(results_dir, paste0(output_prefix, "_summary.csv")),
    row.names = FALSE
  )

  if (write_metadata) {
    metadata_pattern <- paste0("^", output_prefix, "_seed_[0-9]{3}_[0-9]{3}_metadata\\.csv$")
    metadata_files <- list.files(results_dir, pattern = metadata_pattern, full.names = TRUE)

    if (length(metadata_files) > 0) {
      metadata_files <- sort(metadata_files)
      merged_metadata <- do.call(rbind, lapply(metadata_files, utils::read.csv))

      utils::write.csv(
        merged_metadata,
        file = file.path(results_dir, paste0(output_prefix, "_metadata.csv")),
        row.names = FALSE
      )
    } else {
      warning("No seed-batch metadata files found for ", output_prefix)
    }
  }

  invisible(list(
    results = merged_results,
    summary = merged_summary
  ))
}

merge_seed_batch_outputs("imbalance_mixed", write_metadata = TRUE)
merge_seed_batch_outputs("imbalance_all_cat", write_metadata = TRUE)
merge_seed_batch_outputs("imbalance_highdim_mixed", write_metadata = FALSE)
merge_seed_batch_outputs("imbalance_highdim_all_cat", write_metadata = FALSE)

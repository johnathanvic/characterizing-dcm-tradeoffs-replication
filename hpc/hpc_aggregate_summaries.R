# hpc/hpc_aggregate_summaries.R

args <- commandArgs(trailingOnly = TRUE)
job_dir <- args[1]

if (is.na(job_dir)) {
  stop("Usage: Rscript aggregate_results.R <job_directory>")
}

# Find all model_summary.csv files
summary_files <- list.files(path = job_dir, 
                            pattern = "model_summary.csv", 
                            recursive = TRUE, 
                            full.names = TRUE)

if (length(summary_files) > 0) {
  cat("Found", length(summary_files), "model summary files\n")
  
  # Read and combine all CSV files
  all_summaries <- list()
  
  for (i in seq_along(summary_files)) {
    df <- read.csv(summary_files[i], stringsAsFactors = FALSE)
    # Add model name from directory structure
    df$model_name <- basename(dirname(summary_files[i]))
    df$source_file <- summary_files[i]
    all_summaries[[i]] <- df
  }
  
  # Combine into single data frame using base R
  combined_df <- do.call(rbind, all_summaries)
  
  # Save aggregated results
  output_file <- file.path(job_dir, "aggregated_model_summary.csv")
  write.csv(combined_df, output_file, row.names = FALSE)
  
  cat("Aggregated results saved to:", output_file, "\n")
  cat("Total rows:", nrow(combined_df), "\n")
  cat("Models included:", paste(unique(combined_df$model_name), collapse = ", "), "\n")
} else {
  cat("No model_summary.csv files found in", job_dir, "\n")
}
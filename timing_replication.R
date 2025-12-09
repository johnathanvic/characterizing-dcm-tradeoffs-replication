# Timing Replication Script
# Runs function evaluation timing tests on pre-estimated models

library(apollo)
library(tidyverse)

source("core/calc_speedTest.R")
source("core/load_apollo_funcs.R")

# Configuration ----

data_folder <- "solutions/main2_11-11-25"
device_name <- "jv_laptop"  # Options: "jv_laptop", "whitefoot-1", "whitefoot-2"

speedTest_settings <- list(nCoresTry = c(1, 2, 4), nRep = 100)

# Create output folder
sample_name <- paste0("timing_replication_", device_name)
output_folder <- file.path(data_folder, sample_name)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)


# Load Model Files ----

# Get all model files, excluding cross-validation folds
model_files <- list.files(
  data_folder, 
  pattern = "_model\\.rds$", 
  recursive = TRUE, 
  full.names = TRUE
)
model_files <- model_files[!grepl("_CV\\d+_", model_files)]

cat("Found", length(model_files), "models to test\n")
cat("Models:\n")
print(basename(model_files))


# Run Timing Tests ----

for (i in seq_along(model_files)) {
  model_file <- model_files[i]
  model_name <- basename(model_file)
  
  cat("\n==================================================\n")
  cat("Processing model", i, "of", length(model_files), ":", model_name, "\n")
  cat("==================================================\n")
  
  # Load model
  cat("Loading model...\n")
  model <- readRDS(model_file)
  model$apollo_inputs$apollo_control$outputDirectory <- output_folder
  
  # Run speed test
  cat("Running speed test...\n")
  speedTest_start_time <- proc.time()
  speedTest_results_df <- calc_speedTest(model, speedTest_settings = speedTest_settings)
  speedTest_end_time <- proc.time()
  elapsed_time <- speedTest_end_time - speedTest_start_time
  
  cat("Speed test completed in", elapsed_time[3], "seconds\n")
  
  # Clean up
  rm(model)
  gc()
}

cat("\nTiming replication complete!\n")
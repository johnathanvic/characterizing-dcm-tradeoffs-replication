# ===============================
# SETUP - LOAD PART 1 RESULTS
# ===============================

# Parse command-line inputs
args      <- commandArgs(trailingOnly=TRUE)
model_idx <- as.integer(args[1])
n_cores   <- as.integer(args[2])
mem_per_core <- as.integer(args[3])
original_job_id <- args[4]  # Job ID from original setup/multistart

# load libraries & functions
library(parallel)
library(apollo)
library(tidyverse)
source("core/load_data.R")
source("core/load_apollo_inputs.R")
source("core/load_apollo_funcs.R")

# Get SLURM job ID for this estimation job
current_job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
if (current_job_id == "") {
  stop("SLURM_ARRAY_JOB_ID not set. Cannot locate current job ID.")
}

# Use original job ID to find the model configuration files and multistart results
log_dir <- file.path("logs", paste0("job_", original_job_id))
all_model_settings <- readRDS(file.path(log_dir, "all_model_settings.rds"))
model_name <- all_model_settings[[model_idx]]$name
out_dir <- file.path(log_dir, model_name)

# Load Part 1 results
part1_file <- file.path(out_dir, "part1_multistart_results.rds")
if (!file.exists(part1_file)) {
  stop("Part 1 results not found. Run part1_multistart.R first.")
}

part1_results <- readRDS(part1_file)
cat("Loaded Part 1 results for:", part1_results$model_name, "\n")

# Extract what we need from Part 1
apollo_beta_best       <- part1_results$apollo_beta_best
model_settings         <- part1_results$model_settings
prepared_model         <- part1_results$prepared_model
original_apollo_inputs <- part1_results$original_apollo_inputs
# apollo_inputs          <<- part1_results$apollo_inputs
exists("part1_results$apollo_inputs")
apollo_inputs <<- original_apollo_inputs


# Extract the individual components that were used in the original script
# apollo_beta          <- prepared_model$apollo_beta
apollo_beta          <<- apollo_beta_best
apollo_fixed         <<- prepared_model$apollo_fixed
apollo_probabilities <<- prepared_model$apollo_probabilities
apollo_control       <<- prepared_model$apollo_control
constraints          <- prepared_model$constraints

# ===============================
# BEGIN EXECUTION - PART 2: ESTIMATION
# ===============================
cat("Starting Part 2 (Estimation):", model_name, "\n")
tm_start <- Sys.time()

# Reset apollo inputs for estimation (similar to original script)

if (apollo_control$mixing){
  # Restore original draws for MXL
  apollo_draws <<- original_apollo_inputs$apollo_draws
  cat("Restored to ", apollo_draws$interNDraws, " draws. \n")
  memory_factor <- 1/8
  estimation_cores <- min(n_cores * memory_factor, 4)
  apollo_control$nCores <- estimation_cores
  cat("Using ", estimation_cores, " cores. \n")
} else {
  apollo_control$nCores <- n_cores
  cat("Using ", n_cores, " cores. \n")
}

# Re-validate inputs with best starting parameters from multistart (matching original approach)
apollo_inputs <<- apollo_validateInputs(
  apollo_beta      = apollo_beta_best,
  apollo_fixed     = prepared_model$apollo_fixed,
  database         = original_apollo_inputs$database,
  apollo_control   = apollo_control,
  apollo_HB        = original_apollo_inputs$apollo_HB,
  apollo_randCoeff = original_apollo_inputs$apollo_randCoeff,
  apollo_lcPars    = original_apollo_inputs$apollo_lcPars,
  silent           = TRUE,
  recycle          = TRUE
)

# Main estimation
est_start_time <- proc.time()
cat("Starting main estimation \n")
maxLik_settings <- model_settings$maxLik_settings
estimate_settings <- list(
  estimationRoutine = model_settings$estimationRoutine,
  maxLik_settings   = maxLik_settings,
  constraints       = constraints
)
model <- apollo_estimate(
  apollo_beta, apollo_fixed,
  apollo_probabilities, apollo_inputs,
  estimate_settings = estimate_settings
)

# Save & reconfig
apollo_beta <- model$apollo_beta
model$apollo_inputs <- apollo_inputs
model$estimate_settings <- estimate_settings
apollo_saveOutput(model)
# Copy the RDS file with _est suffix incase overwritten later
og_model_name <- apollo_inputs$apollo_control$modelName
rds_path <- paste0(apollo_inputs$apollo_control$outputDirectory, og_model_name, "_model.rds")
est_rds_path <- paste0(apollo_inputs$apollo_control$outputDirectory, og_model_name, "_model_est.rds")
file.copy(rds_path, est_rds_path, overwrite = TRUE)

est_end_time <- proc.time()
tm_end <- Sys.time()

cat("Main estimation complete \n")

# ===============================
# SAVE PART 2 RESULTS & PREPARE FOR PART 3
# ===============================

# Calculate timing
est_timing_df <- data.frame(
  step    = "estimation",
  user    = est_end_time["user.self"]   - est_start_time["user.self"],
  system  = est_end_time["sys.self"]    - est_start_time["sys.self"],
  elapsed = est_end_time["elapsed"]     - est_start_time["elapsed"],
  tot_cpu = (est_end_time["user.self"] - est_start_time["user.self"]) + 
    (est_end_time["sys.self"] - est_start_time["sys.self"])
)

write.csv(est_timing_df, file.path(out_dir, "part2_estimation_timing.csv"), row.names = FALSE)

# Prepare state for Part 3
part2_results <- list(
  # Inherit from Part 1
  part1_results       = part1_results,
  
  # Part 2 results
  model               = model,
  apollo_beta_final   = apollo_beta,
  apollo_inputs_final = apollo_inputs,
  
  # Timing
  est_timing          = est_timing_df,
  part2_total_time    = as.numeric(difftime(tm_end, tm_start, units="secs")),
  
  # Job tracking
  part2_job_id        = current_job_id
)

# Save state for Part 3
saveRDS(part2_results, file.path(out_dir, "part2_estimation_results.rds"))

# Save Part 2 summary
part2_summary <- list(
  model               = model_name,
  part2_elapsed       = as.numeric(difftime(tm_end, tm_start, units="secs")),
  est_timeEst         = model$timeEst,
  est_successful      = model$successfulEstimation,
  est_code            = model$code,
  est_message         = model$message,
  est_LL              = model$LLout[1],
  est_AIC             = model$AIC,
  est_BIC             = model$BIC,
  part2_complete      = TRUE,
  part2_job_id        = current_job_id,
  timestamp           = Sys.time()
)

part2_summary_df <- as.data.frame(part2_summary)
write.csv(part2_summary_df, file.path(out_dir, "part2_summary.csv"), row.names = FALSE)

cat("Part 2 (Estimation) complete for:", model_name, "\n")
cat("Results saved to:", out_dir, "\n")
cat("Total time:", round(as.numeric(difftime(tm_end, tm_start, units="hours")), 2), "hours\n")
cat("Ready for Part 3 (Cross-validation + remaining)\n")
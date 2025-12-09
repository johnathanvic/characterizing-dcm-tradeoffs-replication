# hpc/hpc_run_model_part1_multistart.R

# ===============================
# SETUP
# ===============================

# Parse command-line inputs
args      <- commandArgs(trailingOnly=TRUE)
model_idx <- as.integer(args[1])
n_cores   <- as.integer(args[2])
mem_per_core <- as.integer(args[3])

# load libraries & functions
library(parallel)
library(apollo)
library(tidyverse)
source("core/load_data.R")  # Runs the script, making the function available
source("core/load_apollo_inputs.R")
source("core/load_apollo_funcs.R")
source("core/custom_apollo_searchStart.R")
# source("core/custom_apollo_outOfSample.R")
# source("core/load_manual_simulation_funcs.R")
# source("core/load_toy_choice_sets.R")
# source("core/calc_func_eval_time.R")
# source("core/calc_psi_prime.R")

# Get SLURM job ID (same as original setup job for multistart)
job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
if (job_id == "") {
  stop("SLURM_ARRAY_JOB_ID not set. Cannot locate the job-specific logs folder.")
}

# Define the path to the logs for this job
log_dir <- file.path("logs", paste0("job_", job_id))

# Read the RDS files from the job-specific folder
all_model_settings <- readRDS(file.path(log_dir, "all_model_settings.rds"))
prepared_models    <- readRDS(file.path(log_dir, "prepared_models.rds"))

# Get the specific model to run
model_settings <- all_model_settings[[model_idx]]
prepared_model <- prepared_models[[model_idx]]
model_name <- model_settings$name
cat("Running Part 1 (Multistart):", model_name, "\n")

# ===============================
# BEGIN EXECUTION - PART 1: MULTISTART
# ===============================
cat("Starting Setup \n")
# Start sys timing
tm_start <- Sys.time()

# Extract settings from the model_settings
searchStart_settings <- list(
  nCandidates     = model_settings$n_searchStart,
  sampling_method = model_settings$sampling_method,
  bfgsIter        = model_settings$bfgsIter,
  maxStages       = model_settings$maxStages,
  # solverSettings  = model_settings$maxLik_settings,
  constraints     = prepared_model$constraints
)

# Extract Apollo parameters
apollo_beta          = prepared_model$apollo_beta
apollo_fixed         = prepared_model$apollo_fixed
apollo_probabilities = prepared_model$apollo_probabilities
apollo_inputs        = prepared_model$apollo_inputs
apollo_control       = prepared_model$apollo_control
constraints          = prepared_model$constraints
cat("Setup Complete \n")

# 1) Run search-start in parallel
cat("Starting Search-Start \n")
ms_start_time <- proc.time()
parallel_settings <- list(n_cores = n_cores, mem_per_core = mem_per_core)

# The original apollo_inputs is initilized to nCores = 1 for searchStart, changing
# nCores (or anything in apollo_control) will necessitate re-validation
# Save original apollo inputs
cat("Saving original apollo_inputs \n")
original_apollo_inputs <<- apollo_inputs

# For MXL, run w/ reduced draws for searchStart
if (apollo_control$mixing){
  original_n_draws <- apollo_inputs$n_draws
  reduced_n_draws <- max(floor(original_n_draws / 10), 100)  # Use 1/10 reduction, but 100 draw min
  apollo_draws <<- original_apollo_inputs$apollo_draws
  apollo_draws$interNDraws <- reduced_n_draws # Cannot use <<- directly on list components
  apollo_draws <<- apollo_draws # Reassign modified draws object to global environment
  
  # Re-validate inputs
  cat("Validating inputs \n")
  apollo_inputs <<- apollo_validateInputs(
    apollo_beta      = apollo_beta,
    apollo_fixed     = apollo_fixed,
    database         = apollo_inputs$database,
    apollo_control   = apollo_control,
    apollo_HB        = apollo_inputs$apollo_HB,
    apollo_randCoeff = apollo_inputs$apollo_randCoeff,
    apollo_lcPars    = apollo_inputs$apollo_lcPars,
    silent           = TRUE,
    recycle          = TRUE
  )
  cat("Running searchStart with ", apollo_draws$interNDraws, " draws. \n")
}

# SearchStart (in parallel)
ms_res <- custom_apollo_searchStart(
  apollo_beta, apollo_fixed,
  apollo_probabilities, apollo_inputs,
  searchStart_settings,
  parallel = TRUE,
  parallel_settings = parallel_settings
)

cat("Search-Start Execution Complete \n")

# Extract outputs
apollo_beta_best    <- ms_res$beta_start
candidate_outcomes  <- ms_res$candidate_outcomes
ms_point_summary    <- ms_res$point_summary
cat("Search-Start Post-Processing Complete \n")
ms_end_time <- proc.time()

# End sys timing for part 1
tm_end <- Sys.time()

# ===============================
# SAVE PART 1 RESULTS & PREPARE FOR PART 2
# ===============================

# Create model-specific directory
out_dir <- file.path(log_dir, model_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save multistart timing
ms_timing_df <- data.frame(
  step    = "multistart",
  user    = ms_end_time["user.self"]   - ms_start_time["user.self"],
  system  = ms_end_time["sys.self"]    - ms_start_time["sys.self"],
  elapsed = ms_end_time["elapsed"]     - ms_start_time["elapsed"],
  tot_cpu = (ms_end_time["user.self"] - ms_start_time["user.self"]) + 
    (ms_end_time["sys.self"] - ms_start_time["sys.self"])
)

write.csv(ms_timing_df, file.path(out_dir, "part1_multistart_timing.csv"), row.names = FALSE)

# Prepare state for Part 2 (Estimation)
part1_results <- list(
  # Multistart results
  apollo_beta_best    = apollo_beta_best,
  candidate_outcomes  = candidate_outcomes,
  ms_point_summary    = ms_point_summary,
  
  # Original model configuration (needed for part 2)
  model_settings      = model_settings,
  prepared_model      = prepared_model,
  original_apollo_inputs = original_apollo_inputs,
  ms_apollo_inputs    = apollo_inputs,
  
  # Timing info
  ms_timing           = ms_timing_df,
  part1_total_time    = as.numeric(difftime(tm_end, tm_start, units="secs")),
  
  # Metadata
  model_name          = model_name,
  model_idx           = model_idx,
  n_cores             = n_cores,
  mem_per_core        = mem_per_core,
  current_job_id      = job_id,        # ADD THIS LINE
  job_id              = job_id,         # Keep original for backward compatibility
  log_dir             = log_dir
)

# Save state for next part
saveRDS(part1_results, file.path(out_dir, "part1_multistart_results.rds"))

# Save summary of part 1
part1_summary <- list(
  model               = model_name,
  part1_elapsed       = as.numeric(difftime(tm_end, tm_start, units="secs")),
  ms_n_points         = ms_point_summary$n_points,
  ms_n_best           = ms_point_summary$n_best,
  ms_n_within_tol     = ms_point_summary$n_within_tol,
  ms_did_best_conv    = ms_point_summary$did_best_conv,
  best_beta_found     = !is.null(apollo_beta_best),
  part1_complete      = TRUE,
  timestamp           = Sys.time()
)

part1_summary_df <- as.data.frame(part1_summary)
write.csv(part1_summary_df, file.path(out_dir, "part1_summary.csv"), row.names = FALSE)

cat("Part 1 (Multistart) complete for:", model_name, "\n")
cat("Results saved to:", out_dir, "\n")
cat("Total time:", round(as.numeric(difftime(tm_end, tm_start, units="hours")), 2), "hours\n")
cat("Ready for Part 2 (Estimation)\n")
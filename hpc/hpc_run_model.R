# hpc/hpc_run_model.R


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
source("core/custom_apollo_outOfSample.R")
source("core/load_manual_simulation_funcs.R")
source("core/load_toy_choice_sets.R")
source("core/calc_func_eval_time.R")
source("core/calc_psi_prime.R")
source("core/calc_psi_prime_on_dataset.R")
source("core/calc_speedTest.R")

# Get SLURM job ID or fallback to timestamp if running interactively
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
cat("Running:", model_name, "\n")

# ===============================
# BEGIN EXECUTION
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
cat("Setup  Complete \n")

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



# Reset apollo inputs
if (apollo_control$mixing){
  # Restore og draws for MXL
  apollo_draws <<- original_apollo_inputs$apollo_draws
  cat("Restored to ", apollo_draws$interNDraws, " draws. \n")
  memory_factor <- 1/8
  estimation_cores <-  min(n_cores * memory_factor, 4) # To ensure enough memory per core
  apollo_control$nCores <- estimation_cores
  cat("Using ", estimation_cores, " cores. \n")
} else{
  # Reset number of cores for estimation
  apollo_control$nCores <- n_cores
  cat("Using ", n_cores, "cores. \n")
}

apollo_inputs <- apollo_validateInputs(
  apollo_beta      = ms_res$beta_start,
  apollo_fixed     = apollo_fixed,
  database         = original_apollo_inputs$database,
  apollo_control   = apollo_control,
  apollo_HB        = original_apollo_inputs$apollo_HB,
  apollo_randCoeff = original_apollo_inputs$apollo_randCoeff,
  apollo_lcPars    = original_apollo_inputs$apollo_lcPars,
  silent           = TRUE,
  recycle          = TRUE
)

# Extract outputs
apollo_beta         <- ms_res$beta_start
candidate_outcomes  <- ms_res$candidate_outcomes
ms_point_summary    <- ms_res$point_summary
cat("Search-Start Post-Processing Complete \n")
ms_end_time <- proc.time()

# 2) Main estimation
est_start_time <- proc.time()
cat("Starting main estimation \n")
maxLik_settings <- model_settings$maxLik_settings
estimate_settings = list(
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

# 3) Perform outOfSample
cv_start_time <- proc.time()
cat("Starting Cross-Validation \n")
cv_res <- custom_apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  # estimate_settings = list(estimationRoutine = model_settings$estimationRoutine, maxLik_settings = model_settings$maxLik_settings),
  estimate_settings = list(estimationRoutine = model_settings$estimationRoutine, maxIterations = maxLik_settings$iterlim, 
                           maxLik_settings = maxLik_settings, constraints = constraints),
  outOfSample_settings = list(
    nRep = model_settings$cv_nRep, 
    validationSize = model_settings$cv_validationSize
  )
)
cat("Cross-Validation Complete \n")
cv_end_time <- proc.time()

# 4) Psi' on dataset
cat("Starting Psi' on Dataset Test \n")
psi_data_start_time <- proc.time()
psi_prime_data_list <- calculate_psi_prime_on_data(model)
psi_data_end_time <- proc.time()
cat("Psi' on Dataset Test Complete\n")

# 5) Speed Test
cat("Starting Speed Test \n")
speedTest_start_time <- proc.time()
speedTest_results_df <- calc_speedTest(model)
speedTest_end_time <- proc.time()
cat("Speed Test Complete \n")

# 6) Perform psi calcs
psi_start_time <- proc.time()
cat("Starting Psi' Test \n")
toy_choice_sets <- get_toy_choice_sets()
choice_prob_func <- wrap_model(model)

psi_prime_small <- calc_psi_prime(choice_prob_func, toy_choice_sets$small)
psi_prime_big <- calc_psi_prime(choice_prob_func, toy_choice_sets$big)
psi_prime_list <- list(psi_prime_small, psi_prime_big)
cat("Psi' Test Complete \n")
psi_end_time <- proc.time()

# 7) Perform func eval time
feval_start_time <- proc.time()
cat("Starting Func Eval Timing \n")
cpu_time_small <- calc_func_eval_time(choice_prob_func, toy_choice_sets$small, model_settings$n_func_eval)
cpu_time_big <- calc_func_eval_time(choice_prob_func, toy_choice_sets$big, model_settings$n_func_eval)
func_eval_list <- list(cpu_time_small, cpu_time_big)
cat("Func Eval Timing Complete \n")
feval_end_time <- proc.time()

# End sys timing
tm_end <- Sys.time()


# ===============================
# SAVE RESULTS
# ===============================

# Save results
result_list <- list(
  model            = model_name,
  tot_elapsed      = as.numeric(difftime(tm_end, tm_start, units="secs")),
  est_timeEst      = model$timeEst,
  est_successful   = model$successfulEstimation,
  est_code         = model$code,
  est_message      = model$message,
  est_LL           = model$LLout[1],
  est_AIC          = model$AIC,
  est_BIC          = model$BIC,
  ms_n_points      = ms_point_summary$n_points,
  ms_n_best        = ms_point_summary$n_best,
  ms_n_within_tol  = ms_point_summary$n_within_tol,
  ms_did_best_conv = ms_point_summary$did_best_conv,
  cv_in_avgObsLL   = cv_res["Average", "avgObsLL_est"],
  cv_out_avgObsLL  = cv_res["Average", "avgLLObs_val"],
  cv_pctDiff_avgObsLL = cv_res["Average", "percentDiff"],
  cv_std_pctDiff  = sd(cv_res[!rownames(cv_res) %in% "Average", "percentDiff"]),
  cv_in_avgObsL   = cv_res["Average", "avgObsL_est"],
  cv_out_avgObsL  = cv_res["Average", "avgLObs_val"],
  cv_pctDiff_avgObsL = cv_res["Average", "percentDiffL"],
  cv_cpuSec_val    = cv_res["Average", "cpuSec_val"],
  psi_prime_mean_all    = psi_prime_data_list$mean_psi_prime,
  psi_prime_mean_chosen = psi_prime_data_list$mean_psi_prime_chosen,
  time_per_call_1core = speedTest_results_df$time_per_call[1],
  time_per_call_2core = speedTest_results_df$time_per_call[2],
  time_per_call_4core = speedTest_results_df$time_per_call[3],
  ram_mb_1core        = speedTest_results_df$ram_mb[1],
  ram_mb_2core        = speedTest_results_df$ram_mb[2],
  ram_mb_4core        = speedTest_results_df$ram_mb[3],
  psi_prime_ICE_small = psi_prime_small[1],
  psi_prime_PHEV_small = psi_prime_small[2],
  psi_prime_BEV_small = psi_prime_small[3],
  psi_prime_small_avg = mean(psi_prime_small),
  psi_prime_ICE_big = psi_prime_big[1],
  psi_prime_PHEV_big = psi_prime_big[2],
  psi_prime_BEV_big = psi_prime_big[3],
  psi_prime_big_avg = mean(psi_prime_big),
  feval_cpu_sec_small = func_eval_list[[1]],
  feval_cpu_sec_big = func_eval_list[[2]]
)


# Save timing to df
starts <- list(ms = ms_start_time, est = est_start_time, cv = cv_start_time, psi_data = psi_data_start_time, speedTest = speedTest_start_time,  psi_og = psi_start_time, feval_og = feval_start_time)
ends   <- list(ms = ms_end_time,   est = est_end_time,   cv = cv_end_time, psi_data = psi_data_end_time, speedTest = speedTest_end_time,  psi_og = psi_end_time,   feval_og = feval_end_time)


timing_df <- do.call(rbind, lapply(names(starts), function(nm) {
  s <- starts[[nm]]
  e <- ends[[nm]]
  data.frame(
    step    = nm,
    user    = e["user.self"]   - s["user.self"],
    system  = e["sys.self"]    - s["sys.self"],
    elapsed = e["elapsed"]     - s["elapsed"],
    tot_cpu = (e["user.self"] - s["user.self"]) + (e["sys.self"] - s["sys.self"])
  )
}))

timing_file_name <- paste0(prepared_model$apollo_control$outputDirectory, "/", prepared_model$apollo_control$modelName, "_detailed_timing.csv")
write.csv(timing_df, file.path(timing_file_name), row.names = FALSE)

# Save to model-specific directory (like original script)
out_dir <- file.path(log_dir, model_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save the full results object
saveRDS(result_list, file.path(out_dir, "model_results.rds"))

# Also save summary CSV
result_df <- as.data.frame(result_list)
write.csv(result_df, file.path(out_dir, "model_summary.csv"), row.names = FALSE)

cat(model_name, "done.\n")
cat("Results saved to:", out_dir, "\n")
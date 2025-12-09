# hpc/hpc_run_model.R

# 1) parse command-line
args      <- commandArgs(trailingOnly=TRUE)
model_idx <- as.integer(args[1])
n_cores   <- as.integer(args[2])
mem_per_core <- as.integer(args[3])

# 2) load libraries & your searchStart function
library(parallel)
library(apollo)
library(tidyverse)
source("core/custom_apollo_searchStart.R")

# 3) read in the pre-built settings & inputs
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

model_settings <- all_model_settings[[model_idx]]
prepared_model <- prepared_models[[model_idx]]

# 4) run the multi-start
apollo_beta          = prepared_model$apollo_beta
apollo_fixed         = prepared_model$apollo_fixed
apollo_probabilities = prepared_model$apollo_probabilities
apollo_inputs        = prepared_model$apollo_inputs

# Run in parallel
ms_res  <- custom_apollo_searchStart(
  apollo_beta, apollo_fixed,
  apollo_probabilities, apollo_inputs,
  searchStart_settings,
  parallel=TRUE,
  parallel_settings=list(n_cores = n_cores, mem_per_core = mem_per_core)
)
# Reset number of cores for estimation (need to re-validate)
apollo_control$nCores <<- parallel_settings$n_cores

if (apollo_control$mixing){     # Add draws if MXL is true
  apollo_draws <<- apollo_inputs$apollo_draws
}
apollo_inputs <<- apollo_validateInputs(
  apollo_beta      = ms_res$beta_start,
  apollo_fixed     = apollo_fixed,
  database         = apollo_inputs$database,
  apollo_control   = apollo_control,
  apollo_HB        = apollo_inputs$apollo_HB,
  apollo_randCoeff = apollo_inputs$apollo_randCoeff,
  apollo_lcPars    = apollo_inputs$apollo_lcPars,
  silent           = TRUE,
  recycle          = TRUE
)

# Extract outputs
apollo_beta         <- ms_res$beta_start
candidate_outcomes  <- ms_res$candidate_outcomes
ms_point_summary    <- ms_res$point_summary

# 2) Main estimation
# Estimate
model <- apollo_estimate(
  apollo_beta, apollo_fixed,
  apollo_probabilities, apollo_inputs,
  estimate_settings = list(
    estimationRoutine = estimationRoutine,
    maxLik_settings   = maxLik_settings,
    constraints       = constraints
  )
)
# Save & reconfig
apollo_beta <- model$apollo_beta
model$apollo_inputs <- apollo_inputs
apollo_saveOutput(model)

# 3) perform outOfSample
cv_res <- custom_apollo_outOfSample(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(estimationRoutine = estimationRoutine, maxLik_settings = maxLik_settings),
  outOfSample_settings = list(nRep = cv_nRep, validationSize = cv_validationSize)
)

# 4) perform psi calcs
toy_choice_sets <- get_toy_choice_sets()
choice_prob_func <- wrap_model(model)

# 4) perform psi calcs
psi_prime_small <- calc_psi_prime(choice_prob_func, toy_choice_sets$small)
psi_prime_big <- calc_psi_prime(choice_prob_func, toy_choice_sets$big)
psi_prime_list <- list(psi_prime_small, psi_prime_big)

# 5) perform func eval time
cpu_time_small <- calc_func_eval_time(choice_prob_func, toy_choice_sets$small, n_func_eval)
cpu_time_big <- calc_func_eval_time(choice_prob_func, toy_choice_sets$big, n_func_eval)
func_eval_list <- list(cpu_time_small, cpu_time_big)

# 6) Save results
this_result <- list(
  model            = model_name,
  tot_elapsed      = as.numeric(difftime(tm_end, tm_start, units="secs")),
  est_timeEst      = model$timeEst,
  est_successful   = model$successfulEstimation,
  est_code         = model$code,
  est_message      = model$message,
  est_LL           = model$LLout[1],
  ms_n_points      = ms_point_summary$n_points,
  ms_n_best        = ms_point_summary$n_best,
  ms_n_within_tol  = ms_point_summary$n_within_tol,
  ms_did_best_conv = ms_point_summary$did_best_conv,
  cv_in_avgObsLL   = cv_res["Average", "avgObsLL_est"],
  cv_out_avgObsLL  = cv_res["Average", "avgLLObs_val"],
  cv_pctDiff_avgObsLL = cv_res["Average", "percentDiff"],
  cv_cpuSec_val    = cv_res["Average", "cpuSec_val"],
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

# Save to model-specific directory (like your original script)
out_dir <- file.path(log_dir, model_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save the full results object
saveRDS(this_result, file.path(out_dir, "model_results.rds"))

# Also save a summary CSV for easy viewing
result_df <- as.data.frame(this_result)
write.csv(result_df, file.path(out_dir, "model_summary.csv"), row.names = FALSE)

cat(model_name, "done.\n")
cat("Results saved to:", out_dir, "\n")

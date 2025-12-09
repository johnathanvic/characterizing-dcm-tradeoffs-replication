# ===============================
# SETUP - LOAD PART 2 RESULTS
# ===============================

# Parse command-line inputs
args      <- commandArgs(trailingOnly=TRUE)
model_idx <- as.integer(args[1])
n_cores   <- as.integer(args[2])
mem_per_core <- as.integer(args[3])
original_job_id <- args[4]  # Job ID from original setup (where model configs were created)

# load libraries & functions
library(parallel)
library(apollo)
library(tidyverse)
source("core/load_data.R")
source("core/load_apollo_inputs.R")
source("core/load_apollo_funcs.R")
source("core/custom_apollo_outOfSample.R")
source("core/load_manual_simulation_funcs.R")
source("core/load_toy_choice_sets.R")
source("core/calc_func_eval_time.R")
source("core/calc_psi_prime.R")
source("core/calc_psi_prime_on_dataset.R")
source("core/calc_speedTest.R")

# Get SLURM job ID for this part (different from original setup job)
current_job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
if (current_job_id == "") {
  stop("SLURM_ARRAY_JOB_ID not set. Cannot locate current job ID.")
}

# Use original job ID to find the model configuration files
log_dir <- file.path("logs", paste0("job_", original_job_id))
all_model_settings <- readRDS(file.path(log_dir, "all_model_settings.rds"))
model_name <- all_model_settings[[model_idx]]$name
out_dir <- file.path(log_dir, model_name)

# Load Part 2 results
part2_file <- file.path(out_dir, "part2_estimation_results.rds")
if (!file.exists(part2_file)) {
  stop("Part 2 results not found. Run part2_estimation.R first.")
}

part2_results <- readRDS(part2_file)
cat("Loaded Part 2 results for:", part2_results$part1_results$model_name, "\n")

# Extract what we need from Part 2
model <- part2_results$model
apollo_beta <- part2_results$apollo_beta_final
apollo_inputs <- part2_results$apollo_inputs_final
model_settings <- part2_results$part1_results$model_settings
prepared_model <- part2_results$part1_results$prepared_model
constraints          <- prepared_model$constraints
cat("Constraints:\n")
print(constraints)


# Ensure apollo_draws is present in global env
apollo_control <- apollo_inputs$apollo_control
if (apollo_control$mixing){
  # Restore original draws for MXL
  apollo_draws <<- part2_results$apollo_inputs_final$apollo_draws
}

cat("Using apollo_beta \n")
print(apollo_beta)
cat("\n Final LL: ", model$finalLL)


# ===============================
# BEGIN EXECUTION - PART 3: CROSS-VALIDATION + REMAINING
# ===============================
cat("Starting Part 3 (Cross-validation + Remaining):", model_name, "\n")
tm_start <- Sys.time()

# 3) Perform outOfSample
cv_start_time <- proc.time()
cat("Starting Cross-Validation\n")
maxLik_settings <- model_settings$maxLik_settings
cv_res <- custom_apollo_outOfSample(
  apollo_beta,
  prepared_model$apollo_fixed,
  prepared_model$apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    estimationRoutine = model_settings$estimationRoutine, 
    maxIterations = maxLik_settings$iterlim,
    maxLik_settings = maxLik_settings, 
    constraints = constraints
  ),
  outOfSample_settings = list(
    nRep = model_settings$cv_nRep, 
    validationSize = model_settings$cv_validationSize
  )
)
cat("Cross-Validation Complete\n")
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
cat("Starting Psi' Tests\n")
toy_choice_sets <- get_toy_choice_sets()
choice_prob_func <- wrap_model(model)

psi_prime_small <- calc_psi_prime(choice_prob_func, toy_choice_sets$small)
psi_prime_big <- calc_psi_prime(choice_prob_func, toy_choice_sets$big)
psi_prime_list <- list(psi_prime_small, psi_prime_big)
cat("Psi' Tests Complete\n")
psi_end_time <- proc.time()

# 7) Perform func eval time
feval_start_time <- proc.time()
cat("Starting Function Evaluation Timing\n")
cpu_time_small <- calc_func_eval_time(choice_prob_func, toy_choice_sets$small, model_settings$n_func_eval)
cpu_time_big <- calc_func_eval_time(choice_prob_func, toy_choice_sets$big, model_settings$n_func_eval)
func_eval_list <- list(cpu_time_small, cpu_time_big)
cat("Function Evaluation Timing Complete\n")
feval_end_time <- proc.time()

# End sys timing
tm_end <- Sys.time()

# ===============================
# COMPILE FINAL RESULTS FROM ALL PARTS
# ===============================

# Get timing data from all parts
part1_timing <- part2_results$part1_results$ms_timing
part2_timing <- part2_results$est_timing

# Create Part 3 timing
part3_timing_list <- list(
  cv = list(start = cv_start_time, end = cv_end_time),
  psi_data = list(start = psi_data_start_time, end = psi_data_end_time),
  speedTest = list(start = speedTest_start_time, end = speedTest_end_time),
  psi_og = list(start = psi_start_time, end = psi_end_time),
  feval_og = list(start = feval_start_time, end = feval_end_time)
)

part3_timing_df <- do.call(rbind, lapply(names(part3_timing_list), function(nm) {
  s <- part3_timing_list[[nm]]$start
  e <- part3_timing_list[[nm]]$end
  data.frame(
    step    = nm,
    user    = e["user.self"]   - s["user.self"],
    system  = e["sys.self"]    - s["sys.self"],
    elapsed = e["elapsed"]     - s["elapsed"],
    tot_cpu = (e["user.self"] - s["user.self"]) + (e["sys.self"] - s["sys.self"])
  )
}))

write.csv(part3_timing_df, file.path(out_dir, "part3_validation_timing.csv"), row.names = FALSE)

# Compile comprehensive timing across all parts
all_timing_df <- rbind(
  part1_timing,
  part2_timing, 
  part3_timing_df
)

timing_file_name <- paste0(prepared_model$apollo_control$outputDirectory, "/", prepared_model$apollo_control$modelName, "_detailed_timing.csv")
write.csv(all_timing_df, file.path(timing_file_name), row.names = FALSE)

# ===============================
# SAVE FINAL COMPREHENSIVE RESULTS
# ===============================

# Extract multistart results from Part 1
part1_results <- part2_results$part1_results
ms_point_summary <- part1_results$ms_point_summary

# Calculate total time across all parts
total_elapsed <- part1_results$part1_total_time + 
  part2_results$part2_total_time + 
  as.numeric(difftime(tm_end, tm_start, units="secs"))

# Helper function to safely extract job IDs
safe_job_id <- function(job_id) {
  if (is.null(job_id) || length(job_id) == 0 || is.na(job_id) || job_id == "") {
    return("unknown")
  }
  return(as.character(job_id))
}

# Extract job IDs safely
part1_job_id <- safe_job_id(part1_results$current_job_id)
part2_job_id <- safe_job_id(part2_results$part2_job_id)

# Create final comprehensive results
result_list <- list(
  model            = model_name,
  tot_elapsed      = total_elapsed,
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
  feval_cpu_sec_big = func_eval_list[[2]],
  # Additional metadata about the split execution
  split_execution = TRUE,
  part1_job_id = part1_job_id,
  part2_job_id = part2_job_id,
  part3_job_id = current_job_id,
  original_job_id = original_job_id
)

# Save the final comprehensive results
saveRDS(result_list, file.path(out_dir, "model_results.rds"))

# Save summary CSV
result_df <- as.data.frame(result_list)
write.csv(result_df, file.path(out_dir, "model_summary.csv"), row.names = FALSE)

# Save Part 3 summary
part3_summary <- list(
  model               = model_name,
  part3_elapsed       = as.numeric(difftime(tm_end, tm_start, units="secs")),
  total_elapsed       = total_elapsed,
  cv_successful       = !is.null(cv_res),
  psi_tests_complete  = !is.null(psi_prime_list),
  feval_tests_complete = !is.null(func_eval_list),
  part3_complete      = TRUE,
  all_parts_complete  = TRUE,
  timestamp           = Sys.time()
)

part3_summary_df <- as.data.frame(part3_summary)
write.csv(part3_summary_df, file.path(out_dir, "part3_summary.csv"), row.names = FALSE)

cat("Part 3 (Cross-validation + Remaining) complete for:", model_name, "\n")
cat("All parts complete! Total execution time:", round(total_elapsed/3600, 2), "hours\n")
cat("Final results saved to:", out_dir, "\n")
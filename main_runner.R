# ' Vehicle Choice Model Estimation Pipeline (main_runner.R)
# ' 
# ' Estimates and evaluates discrete choice models for automotive choices
# ' using conjoint survey data. Compares model performance across multinomial logit,
# ' nested logit, cross-nested logit, latent class, and mixed logit specifications.
# '
# ' Core analyses:
# '   - Performs multi-start to find the best optima for non-convex models
# '   - Estimates a model, as defined by apollo_estimate()
# '   - Evaluates predictive validity via out-of-sample fit 
# '   - Evaluates substitution distortion via the psi' metric 
# '   - Evaluates function evaluation time
# '
# ' Structure:
# '   1. Setup: Load packages, functions, and data
# '   2. Configuration: Define estimation settings and model specifications
# '   3. Initialize: Prepare Apollo components for each model
# '   4. Run: Estimate models and compile results

# Setup -----------------------------------------------------------

# Load packages
rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(apollo)
library(tidyverse)
library(parallel)

# Load custom functions
source("core/load_data.R")                       # Loads and processes the data
source("core/load_apollo_inputs.R")              # Loads the inputs for Apollo
source("core/load_apollo_funcs.R")               # Defines the Apollo choice functions
source("core/custom_apollo_searchStart.R")       # Custom version of Apollo searchStart
source("core/custom_apollo_outOfSample.R")       # Custom version of Apollo outOfSample
source("core/load_manual_simulation_funcs.R")    # Manual model creation (outside Apollo)
source("core/load_toy_choice_sets.R")            # Toy dataset (not in final results)
source("core/calc_func_eval_time.R")             # Function eval time - toy dataset
source("core/calc_psi_prime.R")                  # Psi-prime calculation - toy dataset
source("core/calc_psi_prime_on_dataset.R")       # Psi-prime calculation - conjoint data
source("core/calc_speedTest.R")                  # Function eval time - conjoint data

# Load and unpack data
data <- load_and_process_data(run_local = TRUE, data_path = "inputs/PooledWeighted.xlsx")  # Calls the function from 'core/load_data.R'
list2env(data, envir = .GlobalEnv) # Unpacks cbc_short_veh_df, availCols, and other vars

# Create timestamped output directory
ts <- format(Sys.time(), "%m-%d_%H-%M")
output_directory <- file.path("output/results/", paste0("v4_", ts))
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)


# Configuration ---------------------------------------------------------------

# Session settings
database             = cbc_short_veh_df
n_cores              = detectCores() - 1

# Estimation Settings
estimationRoutine    = "BFGS"
maxLik_settings      = list(
  gradtol=1e-6,
  tol = 1e-100,
  reltol=1e-100,
  steptol=1e-100,
  lambdatol=1e-100,
  qrtol=1e-100,
  printLevel=2,
  iterlim=5000
  )

# Search Start
n_searchStart <- 101
bfgsIter <- 25
maxStages <- 50 
sampling_method <- "hybrid"

# Cross-validation and testing
cv_nRep              = 5
cv_validationSize    = 0.2
n_func_eval <- 10000    # For toy dataset function evaluation
speedTest_settings <- list(nCoresTry = c(1, 2, 4), nRep = 100)

# Default model settings
default_settings <- list(
  name                 = "MNL",
  heterogeneity_spec   = "none",
  nesting_spec         = "none",
  n_classes            = NA,
  n_draws              = 100,
  utility_space        = "pref",
  lognorm_price_dist   = FALSE,
  workInLogs           = TRUE,
  scalingFactor_dist   = TRUE,
  observed_attributes  = c("Price","Acceleration", "OpCost", "hev", "phev20", "phev40",
                           "bev", "bevRangeRel", "noBEVFC", "american", "chinese",
                           "japanese", "skorean"),
  output_directory     = output_directory
)

# Define models to run
all_model_specs <- list(
  # MNL Models
  list(name = "MNL_none_pref",       heterogeneity_spec = "none"),
  list(name = "MNL_NL-THREE_pref",   heterogeneity_spec = "none",   nesting_spec = "NL-THREE"),
  list(name = "MNL_CNL_pref",        heterogeneity_spec = "none",   nesting_spec = "CNL"),
  
  # Latent Class - 2 classes
  list(name = "LCL2_none_pref",      heterogeneity_spec = "latent", n_classes = 2),
  list(name = "LCL2_NL-THREE_pref",  heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 2),
  list(name = "LCL2_CNL_pref",       heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 2),
  
  # Latent Class - 4 classes
  list(name = "LCL4_none_pref",      heterogeneity_spec = "latent", n_classes = 4),
  list(name = "LCL4_NL-THREE_pref",  heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 4),
  list(name = "LCL4_CNL_pref",       heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 4),
  
  # Latent Class - 6 classes
  list(name = "LCL6_none_pref",      heterogeneity_spec = "latent", n_classes = 6),
  list(name = "LCL6_NL-THREE_pref",  heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 6),
  list(name = "LCL6_CNL_pref",       heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 6),
  
  # Latent Class - 8 classes
  list(name = "LCL8_none_pref",      heterogeneity_spec = "latent", n_classes = 8),
  list(name = "LCL8_NL-THREE_pref",  heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 8),
  list(name = "LCL8_CNL_pref",       heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 8),
  
  # Mixed Logit - Preference Space
  list(name = "MXL100_none_pref",    heterogeneity_spec = "mixed",  n_draws = 100,  utility_space = "pref"),
  list(name = "MXL500_none_pref",    heterogeneity_spec = "mixed",  n_draws = 500,  utility_space = "pref"),
  list(name = "MXL1000_none_pref",   heterogeneity_spec = "mixed",  n_draws = 1000, utility_space = "pref"),
  
  # Mixed Logit - WTP Space
  list(name = "MXL100_none_wtp",     heterogeneity_spec = "mixed",  n_draws = 100,  utility_space = "wtp"),
  list(name = "MXL500_none_wtp",     heterogeneity_spec = "mixed",  n_draws = 500,  utility_space = "wtp"),
  list(name = "MXL1000_none_wtp",    heterogeneity_spec = "mixed",  n_draws = 1000, utility_space = "wtp")
)


# Initialize -----------------------------------------------------------

# Initialize timing data
timings <- data.frame(
  model      = character(),
  elapsed    = numeric(),
  timeEst    = numeric(),
  successful = logical(),
  code       = integer(),
  message    = character(),
  stringsAsFactors = FALSE
)
models <- list()

# Merge model specs with defaults
n_models <- length(all_model_specs)
all_model_settings <- list()

for (i in seq_along(all_model_specs)) {
  spec <- all_model_specs[[i]]
  model <- default_settings

  # Overwrite non-default specs
  for (name in names(spec)) model[[name]] <- spec[[name]]

  all_model_settings[[i]] <- model
}

# Storage for Apollo components
prepared_models <- list()

# Variables to clear between model runs
vars_to_clear <- c("inputs","apollo_beta","apollo_fixed","apollo_probabilities",
                "apollo_inputs","apollo_lcPars","apollo_draws","apollo_randCoeff",
                "beta_start", "apollo_control")

# Prepare each Apollo model
for (j in seq_along(all_model_settings)) {
  
  model_settings <- all_model_settings[[j]]
  model_name <- model_settings$name
  cat("\n\n=== Running", model_name, "===\n")

  # Load Apollo inputs
  model_inputs <- load_apollo_inputs(
    logit_type          = model_name,
    heterogeneity_spec  = model_settings$heterogeneity_spec,
    nesting_spec        = model_settings$nesting_spec,
    n_classes           = model_settings$n_classes,
    utility_space       = model_settings$utility_space,
    lognorm_price_dist  = model_settings$lognorm_price_dist,
    workInLogs          = model_settings$workInLogs,
    scalingFactor_dist  = model_settings$scalingFactor_dist,
    observed_attributes = model_settings$observed_attributes,
    database            = model_settings$database,
    output_directory    = file.path(model_settings$output_directory, model_name),
    n_cores             = 1 # Apollo's internal nCores must be set to 1 to avoid conflicts
  )
  
  apollo_control <- model_inputs$apollo_control
  apollo_beta <- model_inputs$apollo_beta
  apollo_fixed <- model_inputs$apollo_fixed
  nesting_params <- model_inputs$nesting_params
  constraints <- model_inputs$constraints
  # returns "apollo_control" "apollo_beta"    "apollo_fixed"   "nesting_params" to global env

  # Build apollo_inputs list
  apollo_inputs <- list(
    n_classes           = model_settings$n_classes,
    n_draws             = model_settings$n_draws,
    availCols           = availCols,
    attrCols            = attrCols,
    observed_attributes = model_settings$observed_attributes,
    heterogeneity_spec  = model_settings$heterogeneity_spec,
    utility_space       = model_settings$utility_space,
    nesting_spec        = model_settings$nesting_spec,
    lognorm_price_dist  = model_settings$lognorm_price_dist,
    scalingFactor_dist  = model_settings$scalingFactor_dist,
    nesting_params      = nesting_params,
    constraints         = constraints
  )

  # Load model-specific probability function
  loaded_model <- load_apollo_model(apollo_inputs, apollo_beta)
  apollo_inputs        <- loaded_model$apollo_inputs
  apollo_probabilities <- loaded_model$apollo_probabilities

  # Store prepared model components
  prepared_models[[model_name]] <- list(
      apollo_beta           = apollo_beta,
      apollo_fixed          = apollo_fixed,
      apollo_probabilities  = apollo_probabilities,
      apollo_inputs         = apollo_inputs,
      apollo_control        = apollo_control,
      constraints           = constraints
  )
  
  # Clean up workspace
  to_rm <- intersect(vars_to_clear, ls(envir = .GlobalEnv))
  if(length(to_rm)) rm(list = to_rm, envir = .GlobalEnv)
}


# Run Models --------------------------------------------------------------

# Load each model & run (in sequence)

# Pre-allocate results storage
all_results <- vector("list", length(prepared_models))
names(all_results) <- names(prepared_models)

# Loop over each model
for (model_name in names(prepared_models)) {
  cat("Estimating:", model_name, "\n")

  # Clear workspace
  rm(list = intersect(vars_to_clear, ls()), envir = .GlobalEnv)

  # Load prepared model and settings
  prepared_model <- prepared_models[[model_name]]
  ms_settings <- list(
    nCandidates     = n_searchStart,
    sampling_method = sampling_method,
    bfgsIter        = bfgsIter,
    maxStages       = maxStages,
    # solverSettings = maxLik_settings,
    constraints = constraints
  )

  # Run estimation
  tm_start <- Sys.time()
  parallel_settings <- list(n_cores = n_cores)
  model_res <- run_model(prepared_model, ms_settings, parallel_settings)
  tm_end   <- Sys.time()

  # Extract results
  model            <- model_res$model
  summary_pts      <- model_res$ms_point_summary
  cv_res           <- model_res$cv_res
  psi_prime_data_list <- model_res$psi_prime_data_list
  speedTest_results_df <- model_res$speedTest_results_df
  psi_prime_list        <- model_res$psi_prime_list
  psi_prime_small <- psi_prime_list[[1]]
  psi_prime_big <- psi_prime_list[[2]]
  func_eval_list   <- model_res$func_eval_list   
  timing_df <- model_res$timing_df
  
  # Compile summary statistics
  this_result      <- list(
    model            = model_name,
    tot_elapsed      = as.numeric(difftime(tm_end, tm_start, units="secs")),
    est_timeEst      = model$timeEst,
    est_successful   = model$successfulEstimation,
    est_code         = model$code,
    est_message      = model$message,
    est_LL           = model$LLout[1],
    est_AIC          = model$AIC,
    est_BIC          = model$BIC,
    ms_n_points      = summary_pts$n_points,
    ms_n_best        = summary_pts$n_best,
    ms_n_within_tol  = summary_pts$n_within_tol,
    ms_did_best_conv = summary_pts$did_best_conv,
    cv_in_avgObsLL   = cv_res["Average", "avgObsLL_est"],
    cv_out_avgObsLL  = cv_res["Average", "avgLLObs_val"],
    cv_pctDiff_avgObsLL = cv_res["Average", "percentDiff"],
    cv_std_pctDiff   = sd(cv_res[!rownames(cv_res) %in% "Average", "percentDiff"]),
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

  # Store results
  all_results[[model_name]] <- this_result

  # Save each iteration
  summary_df <- do.call(rbind, lapply(all_results, as.data.frame))
  print(summary_df)

  write.csv(
    summary_df,
    file.path(output_directory, "summary_df.csv"),
    row.names = FALSE
  )
  
  # Stop writing console output
  sink()
}

# Print final summary
cat("All estimations complete\n")
print(summary_df)

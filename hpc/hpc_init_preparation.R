# 1) load packages & functions
rm(list=ls())
library(readxl); library(dplyr); library(tidyr)
library(apollo); library(tidyverse)
source("core/load_data.R"); source("core/load_apollo_inputs.R")
source("core/load_apollo_funcs.R"); source("core/custom_apollo_searchStart.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  # If running out of HPC .job file
  message("Reading files: ", paste(args, collapse = ", "))
  run_settings_path <- args[1]
  batch_list_path <- args[2]
  source(run_settings_path)
  source(batch_list_path)
  
  # Check that the number of SLURM array tasks matches the number of models
  n_models <- length(all_model_specs)
  n_tasks <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_COUNT", unset = NA))
  if (!is.na(n_tasks) && n_models != n_tasks)
    stop(paste0("Mismatch: ", n_models, " models vs ", n_tasks, " SLURM array tasks. Fix SBATCH --array."))
  message("SLURM task count matches model count: ", n_models)
}else{
  # Running out of R
  source("hpc/hpc_batch_settings_test.R")
  source("hpc/hpc_models_list_batch_test.R")
}

# 2) data + defaults + paths
# data <- load_and_process_data(run_local = TRUE, data_path = "inputs/PooledWeighted.xlsx") 
data <- load_and_process_data()
list2env(data, envir = .GlobalEnv)
database <- data[[default_settings$database]]

# Get SLURM job ID or fallback to timestamp if run interactively
job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
if (job_id == "") {
  job_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
}

# Create a logs directory specific to this job
log_dir <- file.path("logs", paste0("job_", job_id))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# 3) build full settings list
all_model_settings <- lapply(all_model_specs, function(spec){
  modifyList(default_settings, spec)
})

# 4) Prepare each Apollo model only once
prepared_models <- vector("list", length(all_model_settings))
names(prepared_models) <- vapply(all_model_settings, `[[`, "", "name")
vars_to_clear <- c("inputs","apollo_beta","apollo_fixed","apollo_probabilities",
                   "apollo_inputs","apollo_lcPars","apollo_draws","apollo_randCoeff",
                   "beta_start", "apollo_control")

for(j in seq_along(all_model_settings)){
  model_settings <- all_model_settings[[j]]
  
  # Specify type
  model_name <- model_settings$name
  cat("\n\n=== Running", model_name, "===\n")
  
  # # Call func to prepare apollo model
  # model_output <- prepare_apollo_model(model_settings, availCols)
  # prepared_models[[model_output$model_name]] <- model_output
  
  # 1) load inputs
  model_inputs <- load_apollo_inputs(
    logit_type          = model_name,
    heterogeneity_spec  = model_settings$heterogeneity_spec,
    nesting_spec        = model_settings$nesting_spec,
    n_classes           = model_settings$n_classes,
    utility_space       = model_settings$utility_space,
    lognorm_price_dist  = model_settings$lognorm_price_dist,
    scalingFactor_dist  = model_settings$scalingFactor_dist,
    workInLogs          = model_settings$workInLogs,
    observed_attributes = model_settings$observed_attributes,
    database            = model_settings$database,
    output_directory    = file.path(log_dir, model_settings$name),
    n_cores             = 1 # Apollo's internal nCores must be set to 1 to avoid conflicts
  )
  apollo_control <- model_inputs$apollo_control
  apollo_beta <- model_inputs$apollo_beta
  apollo_fixed <- model_inputs$apollo_fixed
  nesting_params <- model_inputs$nesting_params
  constraints <- model_inputs$constraints
  # returns "apollo_control" "apollo_beta"    "apollo_fixed"   "nesting_params" to global env

  # 2) build apollo_inputs list
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

  # 3) get model‐specific probability fn + updated inputs
  loaded_model <- load_apollo_model(apollo_inputs, apollo_beta)
  apollo_inputs        <- loaded_model$apollo_inputs
  apollo_probabilities <- loaded_model$apollo_probabilities

  prepared_models[[model_name]] <- list(
      apollo_beta           = apollo_beta,
      apollo_fixed          = apollo_fixed,
      apollo_probabilities  = apollo_probabilities,
      apollo_inputs         = apollo_inputs,
      apollo_control        = apollo_control,
      constraints           = constraints
  )

  to_rm <- intersect(vars_to_clear, ls(envir = .GlobalEnv))
  if(length(to_rm)) rm(list = to_rm, envir = .GlobalEnv)
}

# 5) save
# saveRDS(all_model_settings, "logs/all_model_settings.rds")
# saveRDS(prepared_models,    "logs/prepared_models.rds")

# Save the RDS files into the job-specific logs folder
job_id <- Sys.getenv("SLURM_ARRAY_JOB_ID")
log_dir <- file.path("logs", paste0("job_", job_id))

saveRDS(all_model_settings, file.path(log_dir, "all_model_settings.rds"))
saveRDS(prepared_models,    file.path(log_dir, "prepared_models.rds"))

cat("Saved to directory:", log_dir, "\n")
cat("Prep done: settings & models serialized.\n")


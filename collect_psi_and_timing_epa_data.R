# Unified pipeline:
#   1. Rebuild the EPA MY2030 choice set (sources build_epa_choice_set.R)
#   2. CPU timing on small / big / EPA for all 21 models
#   3. EPA psi' for all 21 models
#      - For the 6 MXL models, run psi' over 10 seeds at 1000 draws and
#        report mean + SD; non-MXL models run once with seed = 1.
#   4. Merge with the prior baseline (for psi' on the conjoint dataset and
#      the real-data CPU column) and write a single output CSV.
#
# Output: solutions/main2_11-11-25/psi_cpu_comparison_refactored_with_epa.csv
#
# Run from repo root: Rscript collect_psi_and_timing_epa_data.R

suppressPackageStartupMessages({
  library(apollo)
})

# ----------------------------------------------------------------------------
# 1. Rebuild the EPA choice set
# ----------------------------------------------------------------------------
cat("=== Step 1: Rebuild EPA choice set ===\n")
source("epa_data_scripts/build_epa_choice_set.R")

source("core/load_manual_simulation_funcs.R")
source("core/load_toy_choice_sets.R")
source("core/calc_func_eval_time.R")
source("core/calc_psi_prime.R")

N_FUNC_EVAL    <- 10000
MXL_EVAL_DRAWS <- 1000
MXL_SEEDS      <- 1:10
NONMXL_SEED    <- 1
SOLUTIONS_DIR  <- "solutions/main2_11-11-25"
BASELINE_CSV   <- file.path(SOLUTIONS_DIR, "psi_cpu_comparison.csv")
OUTPUT_CSV     <- file.path(SOLUTIONS_DIR, "psi_cpu_comparison_refactored_with_epa.csv")
EPA_CSV        <- "inputs/EPA_Final-Rule_2024/epa_choice_set_my2030.csv"

toy <- get_toy_choice_sets()
epa <- read.csv(EPA_CSV, stringsAsFactors = FALSE)
epa$fueling_class <- with(epa, ifelse(bev_nest_indicator == 1, "BEV",
                              ifelse(phev_nest_indicator == 1, "PHEV",
                              ifelse(hev_nest_indicator == 1, "HEV", "ICE"))))
cat(sprintf("\nEPA choice set: %d alts loaded.\n", nrow(epa)))

# ----------------------------------------------------------------------------
# Discover models
# ----------------------------------------------------------------------------
all_rds <- list.files(SOLUTIONS_DIR, pattern = "_model\\.rds$",
                      recursive = TRUE, full.names = TRUE)
model_paths <- all_rds[!grepl("(_CV[0-9]+|_est)_model\\.rds$", all_rds)]
model_paths <- model_paths[grepl("(job_[0-9]+|too_big_for_github)/", model_paths)]
cat(sprintf("Discovered %d models.\n\n", length(model_paths)))

is_mxl <- function(name) grepl("MXL", name)

# Subset of psi' by group helper
psi_summary <- function(psi_vec) {
  list(
    overall      = mean(psi_vec, na.rm = TRUE),
    ICE          = mean(psi_vec[epa$fueling_class == "ICE"],  na.rm = TRUE),
    HEV          = mean(psi_vec[epa$fueling_class == "HEV"],  na.rm = TRUE),
    PHEV         = mean(psi_vec[epa$fueling_class == "PHEV"], na.rm = TRUE),
    BEV          = mean(psi_vec[epa$fueling_class == "BEV"],  na.rm = TRUE),
    real_lineage = mean(psi_vec[epa$created_alternative == 0], na.rm = TRUE),
    created      = mean(psi_vec[epa$created_alternative == 1], na.rm = TRUE)
  )
}

# ----------------------------------------------------------------------------
# 2-3. Loop: CPU timing + EPA psi' per model
# ----------------------------------------------------------------------------
results <- vector("list", length(model_paths))

for (i in seq_along(model_paths)) {
  path       <- model_paths[i]
  model_name <- basename(dirname(path))
  job        <- basename(dirname(dirname(path)))
  cat(sprintf("[%2d/%d] %s\n", i, length(model_paths), model_name))

  model <- readRDS(path)

  # --- CPU timing (cached wrappers, deterministic for closed-form;
  #     MXL uses a single seed-1 wrapper for the timing benchmark to keep
  #     all numbers reproducible)
  set.seed(NONMXL_SEED)
  cpf_small <- wrap_model(model, init_choice_set = toy$small)
  set.seed(NONMXL_SEED)
  cpf_big   <- wrap_model(model, init_choice_set = toy$big)
  set.seed(NONMXL_SEED)
  cpf_epa   <- wrap_model(model, init_choice_set = epa)

  t_small <- as.numeric(calc_func_eval_time(cpf_small, toy$small, N_FUNC_EVAL))
  t_big   <- as.numeric(calc_func_eval_time(cpf_big,   toy$big,   N_FUNC_EVAL))
  t_epa   <- as.numeric(calc_func_eval_time(cpf_epa,   epa,       N_FUNC_EVAL))
  cat(sprintf("  cpu  small=%6.2fs  big=%6.2fs  epa=%6.2fs\n", t_small, t_big, t_epa))

  # --- EPA psi'
  if (is_mxl(model_name)) {
    # Run 10 seeds at 1000 draws, store mean + SD
    n_seeds <- length(MXL_SEEDS)
    seed_results <- vector("list", n_seeds)
    t_psi0 <- Sys.time()
    for (s in seq_along(MXL_SEEDS)) {
      seed <- MXL_SEEDS[s]
      set.seed(seed)
      cpf_psi <- wrap_mxl_prob_func(model, n_draws = MXL_EVAL_DRAWS)
      psi_vec <- calc_psi_prime(cpf_psi, epa)
      seed_results[[s]] <- psi_summary(psi_vec)
      cat(sprintf("  psi  seed=%2d  overall=%.5f\n", seed, seed_results[[s]]$overall))
    }
    psi_elapsed <- as.numeric(difftime(Sys.time(), t_psi0, units = "secs"))

    # Aggregate
    fields <- c("overall", "ICE", "HEV", "PHEV", "BEV", "real_lineage", "created")
    psi_mean <- sapply(fields, function(f) mean(sapply(seed_results, `[[`, f), na.rm = TRUE))
    psi_sd   <- sapply(fields, function(f) sd(  sapply(seed_results, `[[`, f), na.rm = TRUE))
    names(psi_mean) <- fields
    names(psi_sd)   <- fields
    cat(sprintf("  psi  (10-seed mean +/- SD)  overall=%.5f +/- %.5f  (%.0fs)\n",
                psi_mean["overall"], psi_sd["overall"], psi_elapsed))
  } else {
    # Single deterministic run
    set.seed(NONMXL_SEED)
    cpf_psi <- wrap_model(model)
    t_psi0 <- Sys.time()
    psi_vec <- calc_psi_prime(cpf_psi, epa)
    psi_elapsed <- as.numeric(difftime(Sys.time(), t_psi0, units = "secs"))
    psi_one <- psi_summary(psi_vec)
    fields <- c("overall", "ICE", "HEV", "PHEV", "BEV", "real_lineage", "created")
    psi_mean <- unlist(psi_one[fields])
    psi_sd   <- setNames(rep(0, length(fields)), fields)
    cat(sprintf("  psi  overall=%.5f  (%.0fs)\n", psi_mean["overall"], psi_elapsed))
  }

  results[[i]] <- data.frame(
    job                          = job,
    model                        = model_name,
    is_mxl                       = is_mxl(model_name),
    cpu_per_call_small_sec       = t_small / N_FUNC_EVAL,
    cpu_per_call_big_sec         = t_big   / N_FUNC_EVAL,
    cpu_per_call_epa_sec         = t_epa   / N_FUNC_EVAL,
    psi_prime_epa                = psi_mean["overall"],
    psi_prime_epa_sd             = psi_sd["overall"],
    psi_prime_epa_ICE            = psi_mean["ICE"],
    psi_prime_epa_ICE_sd         = psi_sd["ICE"],
    psi_prime_epa_HEV            = psi_mean["HEV"],
    psi_prime_epa_HEV_sd         = psi_sd["HEV"],
    psi_prime_epa_PHEV           = psi_mean["PHEV"],
    psi_prime_epa_PHEV_sd        = psi_sd["PHEV"],
    psi_prime_epa_BEV            = psi_mean["BEV"],
    psi_prime_epa_BEV_sd         = psi_sd["BEV"],
    psi_prime_epa_real_lineage   = psi_mean["real_lineage"],
    psi_prime_epa_real_lineage_sd= psi_sd["real_lineage"],
    psi_prime_epa_created        = psi_mean["created"],
    psi_prime_epa_created_sd     = psi_sd["created"],
    stringsAsFactors = FALSE
  )
}

new_df <- do.call(rbind, results)

# ----------------------------------------------------------------------------
# 4. Merge with baseline (preserves psi' on conjoint set + real-data CPU)
# ----------------------------------------------------------------------------
baseline <- read.csv(BASELINE_CSV, stringsAsFactors = FALSE)
baseline <- baseline[, c("model", "psi_prime_real", "psi_prime_small", "psi_prime_big",
                          "cpu_per_call_real_sec")]

merged <- merge(baseline, new_df, by = "model", all = TRUE, sort = FALSE)
merged <- merged[match(baseline$model, merged$model), ]

write.csv(merged, OUTPUT_CSV, row.names = FALSE)
cat(sprintf("\nWrote %s\n", OUTPUT_CSV))

cat("\nDone.\n")

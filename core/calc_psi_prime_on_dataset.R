psi_prime_func <- function(prob_original, prob_duplicate) {
  P_orig <- prob_original
  P_dup <- prob_duplicate
  
  numerator <- (P_orig + 1) * (P_orig - 2 * P_dup)
  denominator <- P_orig * (P_orig - 1)
  
  psi_prime <- ifelse(abs(denominator) < 1e-10, NA, 1 - numerator / denominator)
  return(round(psi_prime, 5))
}

calculate_psi <- function(prob_original, prob_duplicate){
  P_orig <- prob_original
  P_dup <- prob_duplicate
  
  psi <- (P_orig - P_dup) / P_dup
  
  return(round(psi, 5))
}

calculate_psi_prime_on_data <- function(model){
  original_database <- model$apollo_inputs$database
  # original_database <- head(model$apollo_inputs$database,20) # For debugging
  original_database$WeightUniform <- 1
  
  # Step 1A: Replicate each observation 3 times (once per choice)
  expanded_database <- original_database %>%
    slice(rep(1:n(), each = 3)) %>%
    group_by(across(all_of(c("ID", "Task")))) %>%  # Group by unique observation identifiers
    mutate(
      DupChoiceCol = rep(1:3, length.out = n()),  # Track which choice was replicated
      OgChoice = Choice  # Keep original choice for reference
    ) %>%
    ungroup()
  
  # Step 1B: Create C4 attribute columns based on replicated choice
  copy_chosen_to_c4 <- function(df, attr_name) {
    c1_col <- paste0(attr_name, "C1")
    c2_col <- paste0(attr_name, "C2") 
    c3_col <- paste0(attr_name, "C3")
    c4_col <- paste0(attr_name, "C4")
    
    df[[c4_col]] <- case_when(
      df$DupChoiceCol == 1 ~ df[[c1_col]],
      df$DupChoiceCol == 2 ~ df[[c2_col]],
      df$DupChoiceCol == 3 ~ df[[c3_col]]
    )
    return(df)
  }
  
  # Shortcut way to get Attr & Avail cols
  c1Cols <- grep("C1$", names(original_database), value = TRUE)
  attrCols <- sub("C1$", "", c1Cols) # Shortcut way to get attr cols
  availCols <- model$apollo_inputs$availCols
  # Apply to all attributes
  # attrCols <- model$apollo_inputs$attrCols
  
  
  for (attr in attrCols) {
    expanded_database <- copy_chosen_to_c4(expanded_database, attr)
  }
  
  # Create TypeC4 based on replicated choice
  expanded_database <- expanded_database %>%
    mutate(TypeC4 = case_when(
      DupChoiceCol == 1 ~ TypeC1,
      DupChoiceCol == 2 ~ TypeC2,
      DupChoiceCol == 3 ~ TypeC3
    ))

  
  # Step 1C: Create availCols_expanded by APPENDING C4 alternatives at the end
  vehicle_types <- unique(gsub("C[1-3]$", "", availCols))
  c4_alternatives <- paste0(vehicle_types, "C4")
  availCols_expanded <- c(availCols, c4_alternatives)
  
  # Step 1D: Create C4 availability columns
  for (veh_type in vehicle_types) {
    c1_col <- paste0(veh_type, "C1")
    c2_col <- paste0(veh_type, "C2")
    c3_col <- paste0(veh_type, "C3")
    c4_col <- paste0(veh_type, "C4")
    
    # C4 is available when:
    # - We're replicating choice 1 AND that vehicle type was in C1 AND TypeC1 matches
    # - We're replicating choice 2 AND that vehicle type was in C2 AND TypeC2 matches
    # - We're replicating choice 3 AND that vehicle type was in C3 AND TypeC3 matches
    expanded_database[[c4_col]] <- case_when(
      expanded_database$DupChoiceCol == 1 & 
        expanded_database[[c1_col]] == 1 & 
        expanded_database$TypeC1 == veh_type ~ 1,
      expanded_database$DupChoiceCol == 2 & 
        expanded_database[[c2_col]] == 1 & 
        expanded_database$TypeC2 == veh_type ~ 1,
      expanded_database$DupChoiceCol == 3 & 
        expanded_database[[c3_col]] == 1 & 
        expanded_database$TypeC3 == veh_type ~ 1,
      TRUE ~ 0
    )
  }
  
  ###  Perform original prediction
  # Extract og inputs
  apollo_beta <- model$estimate
  apollo_probabilities <- model$apollo_probabilities
  # apollo_probabilities <- nl_probability_function()
  original_apollo_inputs <- model$apollo_inputs
  apollo_inputs <- original_apollo_inputs
  
  # Re-validate
  apollo_control <- original_apollo_inputs$apollo_control
  apollo_control$nCores <- 1
  apollo_control$weights <- "WeightUniform"
  if (apollo_control$mixing){
    apollo_draws <<- original_apollo_inputs$apollo_draws
    cat("Restored to ", apollo_draws$interNDraws, " draws. \n")
  }

  apollo_inputs <- apollo_validateInputs(
    apollo_beta      = apollo_beta,
    apollo_fixed     = original_apollo_inputs$apollo_fixed,
    database         = original_database,
    apollo_control   = apollo_control,
    apollo_HB        = original_apollo_inputs$apollo_HB,
    apollo_randCoeff = original_apollo_inputs$apollo_randCoeff,
    apollo_lcPars    = original_apollo_inputs$apollo_lcPars,
    silent           = TRUE,
    recycle          = TRUE
  )
  
  # Takes longer
  # og_pred_og <- apollo_prediction(apollo_beta,apollo_probabilities,apollo_inputs)
  # Predict using prob func (faster)
  og_pred_manual <- apollo_probabilities(apollo_beta, apollo_inputs,functionality="prediction")
  og_pred <- cbind(
    data.frame(
      ID = apollo_inputs$database$ID,
      Observation = apollo_inputs$database$Task
    ),
    as.data.frame(og_pred_manual$model)
  )
  # print(all.equal(og_pred_og$model, og_pred))
  # browser()
  
  ### Perform expanded prediction
  apollo_inputs$availCols <- availCols_expanded
  if (apollo_inputs$nesting_spec != "none"){
    apollo_inputs$nesting_spec <- paste0("expanded_",apollo_inputs$nesting_spec)
  }

  apollo_inputs <- apollo_validateInputs(
    apollo_beta      = apollo_beta,
    apollo_fixed     = original_apollo_inputs$apollo_fixed,
    database         = expanded_database,
    apollo_control   = apollo_control,
    apollo_HB        = original_apollo_inputs$apollo_HB,
    apollo_randCoeff = original_apollo_inputs$apollo_randCoeff,
    apollo_lcPars    = original_apollo_inputs$apollo_lcPars,
    silent           = TRUE,
    recycle          = TRUE
  )

  # apollo_probabilities <- nl_probability_function()
  # Using pred func (slower)
  # expanded_pred <- apollo_prediction(apollo_beta,apollo_probabilities,apollo_inputs)
  # Using prob func (faster)
  expanded_pred_manual <- apollo_probabilities(apollo_beta, apollo_inputs,functionality="prediction")
  expanded_pred <- cbind(
    data.frame(
      ID = apollo_inputs$database$ID,
      Observation = apollo_inputs$database$Task
    ),
    as.data.frame(expanded_pred_manual$model)
  )

  # # Latent class will turn preds from df to list of dfs for each class
  # if (apollo_inputs$heterogeneity_spec == "latent"){
  #   # Capture originals
  #   og_pred_all_classes <- og_pred
  #   expanded_pred_all_classes <- expanded_pred
  #   # Overwrite list for just whole model df
  #   og_pred <- og_pred$model
  #   expanded_pred <- expanded_pred$model
  # }
  
  # Add identifiers to match predictions back to data
  expanded_pred_df <- expanded_pred %>%
    mutate(
      row_id = row_number(),
      ID = expanded_database$ID,
      Task = expanded_database$Task,
      DupChoiceCol = expanded_database$DupChoiceCol,
      OgChoice = expanded_database$OgChoice,
      TypeC4 = expanded_database$TypeC4
    )
  
  # Get the vehicle type for each duplicated choice
  expanded_database <- expanded_database %>%
    mutate(TypeDup = case_when(
      DupChoiceCol == 1 ~ TypeC1,
      DupChoiceCol == 2 ~ TypeC2,
      DupChoiceCol == 3 ~ TypeC3
    ))
  
  # Function to get probability of duplicated alternative from original prediction
  get_og_dup_prob <- function(row_idx, pred_df, dup_choice, type_dup, n_alts = 3) {
    og_row <- ceiling(row_idx / n_alts)
    alt_name <- paste0(type_dup, "C", dup_choice)
    
    if (alt_name %in% names(pred_df)) {
      # Case: Nested Logit (vehicle-specific columns like cvC1, hevC2)
      pred_df[[alt_name]][og_row]
    } else if (paste0("C", dup_choice) %in% names(pred_df)) {
      # Case: MNL (generic columns like C1, C2, C3)
      pred_df[[paste0("C", dup_choice)]][og_row]
    } else {
      NA
    }
  }
  
  # Function to get probability of duplicated alternative from expanded prediction (C4)
  get_expanded_dup_prob <- function(row_idx, pred_df, type_dup, n_alts = 3) {
    next_alt <- n_alts + 1
    alt_name <- paste0(type_dup, "C", next_alt)
    
    if (alt_name %in% names(pred_df)) {
      # Case: Nested Logit (vehicle-specific columns like cvC4, hevC4)
      pred_df[[alt_name]][row_idx]
    } else if (paste0("C", next_alt) %in% names(pred_df)) {
      # Case: MNL (generic columns like C4)
      pred_df[[paste0("C", next_alt)]][row_idx]
    } else {
      NA
    }
  }
  
  # Extract probabilities of duplicated alternatives from ORIGINAL prediction
  og_dup_probs <- sapply(1:nrow(expanded_database), function(i) {
    get_og_dup_prob(i, og_pred, 
                    expanded_database$DupChoiceCol[i], 
                    expanded_database$TypeDup[i])
  })

  # Extract probabilities of duplicated alternatives from EXPANDED prediction (C4 columns)
  expanded_dup_probs <- sapply(1:nrow(expanded_database), function(i) {
    get_expanded_dup_prob(i, expanded_pred, 
                          expanded_database$TypeC4[i])
  })
  
  # Calculate psi_prime (comparing duplicated alt prob in original vs expanded)
  psi_prime_vect <- psi_prime_func(og_dup_probs, expanded_dup_probs)
  
  # Results
  psi_prime_df <- tibble(
    ID = expanded_database$ID,
    Task = expanded_database$Task,
    DupChoiceCol = expanded_database$DupChoiceCol,
    OgChoice = expanded_database$OgChoice,
    is_chosen = DupChoiceCol == OgChoice,  # TRUE if this was the chosen alt
    TypeDup = expanded_database$TypeDup,
    prob_og = og_dup_probs,      # Prob of duplicated alt in original
    prob_dup = expanded_dup_probs, # Prob of duplicated alt in expanded (C4)
    psi_prime = psi_prime_vect
  )

  mean_psi_prime <- mean(psi_prime_df$psi_prime, na.rm = TRUE)
  mean_psi_prime_chosen <- mean(psi_prime_df$psi_prime[psi_prime_df$is_chosen], na.rm = TRUE)

  cat("Mean Psi' (all) = ", mean_psi_prime, "\n")
  cat("Mean Psi' (chosen only) = ", mean_psi_prime_chosen, "\n")

  # Save psi' df
  source_folder <- model$apollo_inputs$apollo_control$outputDirectory
  model_name <- model$apollo_inputs$apollo_control$modelName
  file_name <- paste0(source_folder, "/", model_name, "_psi_prime_dataset.csv")
  write.csv(psi_prime_df, file_name, row.names = FALSE)

  cat("Saved psi' dataset to: ", file_name, "\n")
  
  return(list(
    og_pred = og_pred,
    expanded_pred = expanded_pred,
    psi_prime_df = psi_prime_df,
    mean_psi_prime = mean_psi_prime,
    mean_psi_prime_chosen = mean_psi_prime_chosen
  ))
}

# model_path_mnl          <- "solutions/v4_2_07-19_05-56_collect-TRB/MNL_none_pref/MNL_none_pref_model.rds"
# model_path_nl     <- "solutions/v4_2_07-19_05-56_collect-TRB/MNL_NL-THREE_pref/MNL_NL-THREE_pref_model.rds"
# model_path_mxl     <- "solutions/v4_2_09-02_18-03_MXL100_profile/MXL_none_pref/MXL_none_pref_OLD1_model.rds"
# model_path_lcl <- "solutions/v4_2_07-19_05-56_collect-TRB/LCL2_none_pref/LCL2_none_pref_model.rds"
# model_path_lcnl <- "solutions/v4_2_07-19_05-56_collect-TRB/LCL2_NL-THREE_pref/LCL2_NL-THREE_pref_model.rds"

# model      <- readRDS(model_path_nl)
# model     <- readRDS(model_path_mxl)
# model     <- readRDS(model_path_mnl)
# model <- readRDS(model_path_lcl)
# model <- readRDS(model_path_lcnl)

# # From main2_recollectA
# # model_path          <- "solutions/main2_recollectA/job_35355830/MNL_none_pref/MNL_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/NL_none_pref/NL_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/LCL2_none_pref/LCL2_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/LC2_NL_pref/LC2_NL_pref_model.rds"
# model_path          <- "solutions/main2_recollectA/job_35355830/LC2_CNL_pref/LC2_CNL_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/MXL100_none_pref/MXL100_none_pref_model.rds"
# model <- readRDS(model_path)
# 
# res <- calculate_psi_prime_on_data(model)
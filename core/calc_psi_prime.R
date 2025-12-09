# Function to calculate psi prime (normalized substitution metric) for all alternatives
calc_psi_prime <- function(choice_prob_func, choice_set) {
  
  # Get original probabilities
  prob_original <- choice_prob_func(choice_set)
  
  # Get all alternatives
  alternatives <- names(prob_original)
  
  # Initialize results vector
  psi_prime_vector <- numeric(length(alternatives))
  names(psi_prime_vector) <- alternatives
  
  # Calculate psi prime for each alternative
  for (alt in alternatives) {
    
    # Create duplicated choice set
    row_to_dup <- which(choice_set$veh_id == alt)
    duplicated_row <- choice_set[row_to_dup, ]
    duplicated_row$veh_id <- paste0(alt, "_dup")
    choice_set_dup <- rbind(choice_set, duplicated_row)
    
    # Calculate probabilities with duplication
    prob_dup <- choice_prob_func(choice_set_dup)
    
    # Extract key probabilities
    P_orig <- prob_original[alt]              # Original probability
    P_dup <- prob_dup[alt]                    # Distorted
    num <- (P_orig + 1) * (P_orig - 2 * P_dup)
    denom <- P_orig * (P_orig - 1)
    psi_prime_vector[alt] <- round(1 - num / denom, 5)
  }
  
  return(psi_prime_vector)
}
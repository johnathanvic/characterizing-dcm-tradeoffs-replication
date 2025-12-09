mnl_utility <- function(choice_set, beta, observed_attributes,
                        utility_space, og_expV=0, intercept=NULL){
  
  if (utility_space == "pref"){
    # Organize attr into array (n_attr x n_veh)
    # coef_names <- names(beta)
    # dirty_attrs <- sub("^(b_|wtp_)", "", coef_names)
    
    bvec <- beta[paste0("b_", observed_attributes)]
    X <- as.matrix(choice_set[, observed_attributes, drop=FALSE]) # (n_veh x n_attr)
    V <- drop(X %*% bvec)
  } else {
    # WTP space
    attr_no_price   <- setdiff(observed_attributes, "Price")
    bwtp  <- beta[paste0("wtp_", attr_no_price)]
    Xwtp  <- as.matrix(choice_set[, attr_no_price, drop=FALSE])
    V     <- drop(beta["scalingFactor"] * (Xwtp %*% bwtp - choice_set$Price))
  }
  
  # Add intercept if relevant
  if (!is.null(intercept)) {
    cols <- paste0(names(intercept), "_indicator")
    V    <- V + drop(as.matrix(choice_set[, cols, drop=FALSE]) %*% intercept)
  }
  return(V)
}

mnl_prob <- function(choice_set, beta, observed_attributes,
                     utility_space, og_expV = 0, intercept = NULL) {
  # Calculate deterministic utilities
  utility <- mnl_utility(choice_set, beta, observed_attributes,
                         utility_space, intercept)
  
  # MNL probability calculation
  exp_utility <- exp(utility)
  prob <- exp_utility / (sum(exp_utility) + og_expV)
  
  # Add names to match NL function structure
  names(prob) <- choice_set$veh_id
  
  return(prob)
}

# MNL calc & wrapper
calculate_mnl_prob <- function(choice_set, model, og_expV = 0, intercept = NULL) {
  mnl_prob(choice_set = choice_set,
           beta = model$estimate,
           observed_attributes = model$apollo_inputs$observed_attributes,
           utility_space = model$apollo_inputs$utility_space,
           og_expV = og_expV,
           intercept = intercept)
}

wrap_mnl_prob_func <- function(model, og_expV = 0, intercept = NULL){
  choice_prob_func <- function(choice_set){
    mnl_prob(choice_set = choice_set,
             beta = model$estimate,
             observed_attributes = model$apollo_inputs$observed_attributes,
             utility_space = model$apollo_inputs$utility_space,
             og_expV = og_expV,
             intercept = intercept)
  }
  return(choice_prob_func)
}

###### NL ###### 

# Precompute nesting structure and parameters
build_nesting_structure <- function(choice_set, beta, nesting_spec) {
  
  # Helper to get parameter value
  get_param <- function(param_name) {
    if (param_name %in% names(beta)) {
      return(beta[param_name])
    } else {
      stop(paste("Parameter", param_name, "not found in model"))
    }
  }
  
  # Get vehicle types
  veh_types <- rep(NA, nrow(choice_set))
  veh_types[choice_set$icev_nest_indicator == 1] <- "icev"
  veh_types[choice_set$hev_nest_indicator == 1] <- "hev"
  veh_types[choice_set$phev_nest_indicator == 1] <- "phev"
  veh_types[choice_set$bev_nest_indicator == 1] <- "bev"
  
  n_alts <- nrow(choice_set)
  alt_ids <- choice_set$veh_id
  
  # Build allocation matrix and lambda vector
  if (nesting_spec == "CNL") {
    # CNL with fractional allocations
    alpha_PHEV_ICE_logistic <- get_param("alpha_PHEV_ICE_logistic")
    alpha_PHEV_ICE <- exp(alpha_PHEV_ICE_logistic) / (1 + exp(alpha_PHEV_ICE_logistic))
    
    nest_names <- c("GAS", "BEV")
    allocation_matrix <- matrix(0, nrow = length(nest_names), ncol = n_alts,
                                dimnames = list(nest_names, alt_ids))
    
    # GAS nest: ICEV/HEV=1, PHEV=alpha, BEV=0
    allocation_matrix["GAS", ] <- ifelse(veh_types %in% c("icev", "hev"), 1,
                                         ifelse(veh_types == "phev", alpha_PHEV_ICE, 0))
    
    # BEV nest: ICEV/HEV=0, PHEV=1-alpha, BEV=1
    allocation_matrix["BEV", ] <- ifelse(veh_types %in% c("icev", "hev"), 0,
                                         ifelse(veh_types == "phev", 1 - alpha_PHEV_ICE, 1))
    
    lambda_vec <- c(get_param("lambda_ICE"), get_param("lambda_BEV"))
    
  } else {
    # Standard NL with binary allocations
    nesting_rules <- switch(nesting_spec,
                            "NL-GAS" = list(GAS = c("icev", "hev", "phev"), BEV = c("bev")),
                            "NL-PLUG" = list(ICE = c("icev", "hev"), PLUG = c("phev", "bev")),
                            "NL-THREE" = list(ICE = c("icev", "hev"), PHEV = c("phev"), BEV = c("bev")),
                            stop("`nesting_spec` must be one of NL-GAS, NL-PLUG, NL-THREE, or CNL."))
    
    nest_names <- names(nesting_rules)
    allocation_matrix <- matrix(0, nrow = length(nest_names), ncol = n_alts,
                                dimnames = list(nest_names, alt_ids))
    
    # Binary allocation: each alternative belongs to exactly one nest
    for (i in seq_along(nest_names)) {
      nest_name <- nest_names[i]
      nest_veh_types <- nesting_rules[[nest_name]]
      allocation_matrix[nest_name, ] <- ifelse(veh_types %in% nest_veh_types, 1, 0)
    }
    
    lambda_vec <- sapply(nest_names, function(nest_name) {
      get_param(paste0("lambda_", nest_name))
    })
  }
  
  names(lambda_vec) <- nest_names
  
  return(list(
    allocation_matrix = allocation_matrix,  # J x K matrix (nests x alternatives)
    lambda_vec = lambda_vec,               # J vector (nest parameters)
    nest_names = nest_names,
    alt_ids = alt_ids,
    n_nests = length(nest_names),
    n_alts = n_alts
  ))
}

# Vectorized probability calculation
nl_prob <- function(choice_set, beta, observed_attributes, utility_space, og_expV = 0, intercept = NULL, 
                    nesting_spec, nesting_structure = NULL) {
  
  
  # Step 1: Calculate deterministic utilities
  utility <- mnl_utility(choice_set, beta, observed_attributes, utility_space, og_expV, intercept)
  names(utility) <- choice_set$veh_id
  
  # Step 2: Get or build nesting structure
  if (is.null(nesting_structure)) {
    nesting_structure <- build_nesting_structure(choice_set, beta, nesting_spec)
  }
  
  # Extract components
  A <- nesting_structure$allocation_matrix  # J x K matrix
  lambda <- nesting_structure$lambda_vec    # J vector
  alt_ids <- nesting_structure$alt_ids
  
  # Ensure utility order matches allocation matrix columns
  utility <- utility[alt_ids]
  
  # Step 3: Optimized nested logit calculation
  n_nests <- length(lambda)
  n_alts <- length(utility)
  
  # Pre-allocate final probabilities vector
  final_probs <- numeric(n_alts)
  
  # Calculate nest logsums and probabilities in one pass
  nest_logsums <- numeric(n_nests)
  
  for (j in seq_len(n_nests)) {
    # Get non-zero allocations for this nest (avoid full matrix operations)
    alloc_j <- A[j, ]
    active_alts <- which(alloc_j > 0)
    
    if (length(active_alts) > 0) {
      # Only compute for alternatives in this nest
      lambda_j <- lambda[j]
      utilities_j <- utility[active_alts]
      allocations_j <- alloc_j[active_alts]
      
      # Scaled exponentials: exp(V_k / lambda_j)
      exp_scaled <- exp(utilities_j / lambda_j)
      
      # Weighted sum for logsum
      weighted_sum <- sum(allocations_j * exp_scaled)
      nest_logsums[j] <- lambda_j * log(weighted_sum)
      
      # Conditional probabilities within nest
      conditional_probs <- (allocations_j * exp_scaled) / weighted_sum
      
      # Store for final calculation (will be multiplied by marginal prob)
      final_probs[active_alts] <- conditional_probs
    } else {
      nest_logsums[j] <- -Inf
    }
  }
  
  # Step 4: Marginal nest probabilities
  exp_nest_logsums <- exp(nest_logsums)
  marginal_nest_probs <- exp_nest_logsums / (sum(exp_nest_logsums) + og_expV)
  
  # Step 5: Final probabilities - multiply conditional probs by marginal probs
  final_probs_result <- numeric(n_alts)
  
  for (j in seq_len(n_nests)) {
    alloc_j <- A[j, ]
    active_alts <- which(alloc_j > 0)
    
    if (length(active_alts) > 0) {
      final_probs_result[active_alts] <- final_probs_result[active_alts] + 
        marginal_nest_probs[j] * final_probs[active_alts]
    }
  }
  
  names(final_probs_result) <- alt_ids
  
  return(final_probs_result)
}

# Wrapper
calculate_nl_prob <- function(choice_set, model) {
  # Get nesting structure
  nesting_structure <- build_nesting_structure(
    choice_set = init_choice_set, 
    beta=model$estimate, 
    nesting_spec = model$apollo_inputs$nesting_spec
  )
  
  # Calc choice prob
  choice_prob_func <- function(choice_set){
    nl_prob(choice_set = choice_set, 
            beta = model$estimate,
            observed_attributes = model$apollo_inputs$observed_attributes,
            utility_space = model$apollo_inputs$utility_space, 
            nesting_spec = model$apollo_inputs$nesting_spec,
            nesting_structure = nesting_structure
    )
  }
  return(choice_prob_func)
}

wrap_nl_prob_func <- function(model){
  # Calc choice prob
  choice_prob_func <- function(choice_set){
    nl_prob(choice_set = choice_set, 
            beta = model$estimate,
            observed_attributes = model$apollo_inputs$observed_attributes,
            utility_space = model$apollo_inputs$utility_space, 
            nesting_spec = model$apollo_inputs$nesting_spec
    )
  }
  return(choice_prob_func)
}

###### CNL ###### 
cnl_prob <- function(choice_set, beta, observed_attributes, utility_space, og_expV = 0, intercept = NULL, 
                     nesting_spec, nesting_structure = NULL) {
  
  # Step 1: Calculate deterministic utilities
  utility <- mnl_utility(choice_set, beta, observed_attributes, utility_space, og_expV, intercept)
  names(utility) <- choice_set$veh_id
  
  # Step 2: Get or build nesting structure
  if (is.null(nesting_structure)) {
    nesting_structure <- build_nesting_structure(choice_set, beta, nesting_spec)
  }
  
  # Extract components
  A <- nesting_structure$allocation_matrix  # J x K matrix (nests x alternatives)
  lambda <- nesting_structure$lambda_vec    # J vector
  alt_ids <- nesting_structure$alt_ids
  
  # Ensure utility order matches allocation matrix columns
  utility <- utility[alt_ids]
  
  n_nests <- length(lambda)
  n_alts <- length(utility)
  
  # Step 3: Calculate nest logsums (inclusive values)
  nest_logsums <- numeric(n_nests)
  
  for (j in seq_len(n_nests)) {
    lambda_j <- lambda[j]
    
    # CNL logsum: log(sum_k(alpha_jk * exp(V_k / lambda_j)))
    weighted_sum <- sum(A[j, ] * exp(utility / lambda_j))
    nest_logsums[j] <- lambda_j * log(weighted_sum)
  }
  
  # Step 4: Marginal nest probabilities
  exp_nest_logsums <- exp(nest_logsums)
  marginal_nest_probs <- exp_nest_logsums / (sum(exp_nest_logsums) + og_expV)
  
  # Step 5: Final probabilities using the CNL formula:
  # P(i) = sum_j [P(j) * alpha_ji * exp(V_i/lambda_j) / sum_k(alpha_jk * exp(V_k/lambda_j))]
  final_probs <- numeric(n_alts)
  
  for (j in seq_len(n_nests)) {
    lambda_j <- lambda[j]
    
    # Calculate denominator for conditional probabilities in nest j
    denom_j <- sum(A[j, ] * exp(utility / lambda_j))
    
    # Add contribution from nest j to each alternative
    for (i in seq_len(n_alts)) {
      if (A[j, i] > 0) {  # Only if alternative i is allocated to nest j
        conditional_prob <- A[j, i] * exp(utility[i] / lambda_j) / denom_j
        final_probs[i] <- final_probs[i] + marginal_nest_probs[j] * conditional_prob
      }
    }
  }
  
  names(final_probs) <- alt_ids
  return(final_probs)
}

wrap_cnl_prob_func <- function(model, init_choice_set){
  # # Get nesting structure
  # nesting_structure <- build_nesting_structure(
  #   choice_set = init_choice_set, 
  #   beta=model$estimate, 
  #   nesting_spec = model$apollo_inputs$nesting_spec
  #   )
  
  # Calc choice prob
  choice_prob_func <- function(choice_set){
    cnl_prob(choice_set = choice_set, 
             beta = model$estimate,
             observed_attributes = model$apollo_inputs$observed_attributes,
             utility_space = model$apollo_inputs$utility_space, 
             nesting_spec = model$apollo_inputs$nesting_spec
             # nesting_structure = nesting_structure
    )
  }
  return(choice_prob_func)
}

###### LCL ###### 

# Function 1: Extract class coefficients and intercepts from LCL model
lcl_extract_parameters <- function(beta) {
  
  # Get class intercepts
  class_intercept_names <- grep("^class_intercept_", names(beta), value = TRUE)
  class_intercepts <- beta[class_intercept_names]
  
  # Get betas (& names), shape to (n_classes x n_attr matrix)
  n_classes <- length(class_intercepts)
  rem_beta <- beta[ setdiff(names(beta), class_intercept_names) ]
  n_attrs <- length(rem_beta) / n_classes
  
  rem_names  <- names(rem_beta)
  attr_base  <- sub("_[0-9]+$", "", rem_names)
  attr_names <- unique(attr_base)
  
  pref_coeff <- matrix(rem_beta,
                       nrow    = n_classes,
                       ncol    = n_attrs,
                       byrow   = TRUE,
                       dimnames = list(
                         paste0("class_", seq_len(n_classes)), attr_names))
  
  return(list(
    pref_coeff = pref_coeff,
    class_intercepts = class_intercepts
  ))
}


# Function 2: Calculate choice probabilities for latent-class logit
lcl_prob <- function(choice_set, pref_coeff, class_intercepts, observed_attributes, 
                     utility_space, attr_intercept = NULL, og_expV = 0) {
  
  n_alts <- nrow(choice_set)
  n_classes <- nrow(pref_coeff)
  
  # Calculate choice probabilities within each class
  class_utility_matrix <- sapply(seq_len(n_classes), function(class_j){
    mnl_utility(choice_set          = choice_set,
                beta                = pref_coeff[class_j, ],
                observed_attributes = observed_attributes,
                utility_space       = utility_space,
                intercept           = attr_intercept)  # this may not work as intended
  })
  rownames(class_utility_matrix) = choice_set$veh_id
  colnames(class_utility_matrix) = rownames(pref_coeff)
  
  # Choice prob by class
  exp_utilities <- exp(class_utility_matrix)
  denominators <- colSums(exp_utilities) + og_expV
  class_choice_probs <- sweep(exp_utilities, 2, denominators, "/")      # n_alts × n_classes
  
  # Calculate class membership probabilities using softmax on intercepts
  exp_intercepts <- exp(class_intercepts)
  class_membership_probs <- exp_intercepts / sum(exp_intercepts)
  
  # Calculate overall probabilities (weighted by class membership)
  prob <- as.vector(class_choice_probs %*% class_membership_probs) # (n_veh x n_classes) * (n_classes x 1)
  names(prob) <- choice_set$veh_id
  return(prob)
}

# LCL wrapper function
calculate_lcl_prob <- function(choice_set, model) {
  # Extract class parameters
  lcl_params <- lcl_extract_parameters(model$estimate)
  
  # Calculate probabilities
  prob <- lcl_prob(choice_set = choice_set,
                   pref_coeff = lcl_params$pref_coeff,
                   class_intercepts = lcl_params$class_intercepts,
                   observed_attributes = model$apollo_inputs$observed_attributes,
                   utility_space = model$apollo_inputs$utility_space)
  return(prob)
}

wrap_lcl_prob_func <- function(model){
  # Extract class parameters
  lcl_params <- lcl_extract_parameters(model$estimate)
  
  # Calculate probabilities
  choice_prob_func <- function(choice_set){
    lcl_prob(choice_set = choice_set,
             pref_coeff = lcl_params$pref_coeff,
             class_intercepts = lcl_params$class_intercepts,
             observed_attributes = model$apollo_inputs$observed_attributes,
             utility_space = model$apollo_inputs$utility_space)
  }
  return(choice_prob_func)
}

###### MXL ###### 
# Function 1: Draw random coefficients and separate fixed coefficients
mxl_draw_coefficients <- function(beta, utility_space, n_draws = 100, 
                                  lognorm_price_dist = FALSE, scalingFactor_dist = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Ensure utility_space is single value
  if (length(utility_space) > 1) utility_space <- utility_space[1]
  
  all_params <- names(beta)
  rand_coeff <- list()
  
  if (utility_space == "pref") {
    # Find and draw preference coefficients
    mu_params <- all_params[grepl("^mu_b_", all_params)]
    random_attrs <- gsub("^mu_b_", "", mu_params)
    
    for (attr in random_attrs) {
      mu_name <- paste0("mu_b_", attr)
      sigma_name <- paste0("sigma_b_", attr)
      
      if (sigma_name %in% all_params) {
        mu_val <- beta[[mu_name]]
        sigma_val <- beta[[sigma_name]]
        draws <- rnorm(n_draws, mean = 0, sd = 1)
        
        if (lognorm_price_dist && identical(attr, "Price")) {
          rand_coeff[[paste0("b_", attr)]] <- -exp(mu_val + sigma_val * draws)
        } else {
          rand_coeff[[paste0("b_", attr)]] <- mu_val + sigma_val * draws
        }
      }
    }
  } else {
    # WTP space
    if (scalingFactor_dist) {
      mu_name <- paste0("mu_scalingFactor")
      sigma_name <- paste0("sigma_scalingFactor")
      mu_val <- beta[[mu_name]]
      sigma_val <- beta[[sigma_name]]
      draws <- rnorm(n_draws, mean = 0, sd = 1)
      rand_coeff[[paste0("scalingFactor")]] <- mu_val + sigma_val * draws
    }
    mu_params <- all_params[grepl("^mu_wtp_", all_params)]
    random_attrs <- gsub("^mu_wtp_", "", mu_params)
    
    for (attr in random_attrs) {
      mu_name <- paste0("mu_wtp_", attr)
      sigma_name <- paste0("sigma_wtp_", attr)
      
      if (sigma_name %in% all_params) {
        mu_val <- beta[[mu_name]]
        sigma_val <- beta[[sigma_name]]
        draws <- rnorm(n_draws, mean = 0, sd = 1)
        rand_coeff[[paste0("wtp_", attr)]] <- mu_val + sigma_val * draws
      }
    }
  }
  
  # Fixed coefficients and convert to matrix
  fixed_coeff <- beta[!grepl("^(mu_|sigma_)", all_params)]
  rand_coeff_matrix <- if (length(rand_coeff) > 0) do.call(rbind, rand_coeff) else matrix(nrow = 0, ncol = n_draws)
  
  return(list(
    rand_coeff = rand_coeff_matrix,
    fixed_coeff = fixed_coeff,
    utility_space = utility_space
  ))
}

# Function 2: Calculate choice probabilities
mxl_prob <- function(choice_set, rand_coeff, fixed_coeff, utility_space, og_expV = 0, scalingFactor_dist = FALSE) {
  
  n_alts <- nrow(choice_set)
  n_draws <- ncol(rand_coeff)
  
  if (utility_space == "pref") { # pref space
    attr_names <- gsub("^b_", "", rownames(rand_coeff))
    X <- as.matrix(choice_set[, attr_names, drop = FALSE])  # n_alts × n_attrs
    V_matrix <- X %*% rand_coeff  # n_alts × n_draws
    
  } else { # WTP space
    if (scalingFactor_dist){ # for a distributional scalingFactor
      all_param_names <- gsub("^wtp_", "", rownames(rand_coeff))
      attr_names <- all_param_names[all_param_names != "scalingFactor"]
      wtp_attr_names <- paste0("wtp_", attr_names)
      Xwtp <- as.matrix(choice_set[, attr_names, drop = FALSE])  # n_alts × n_attrs
      wtp_utility <- Xwtp %*% rand_coeff[wtp_attr_names, , drop = FALSE]  # n_alts × n_draws
      price_matrix <- matrix(rep(choice_set$Price, n_draws), nrow = n_alts, ncol = n_draws)
      scaling_matrix <- matrix(rep(rand_coeff["scalingFactor", ], each = n_alts), 
                               nrow = n_alts, ncol = n_draws)  # n_alts × n_draws
      V_matrix <- scaling_matrix * (wtp_utility - price_matrix)
    } else{
      attr_names <- gsub("^wtp_", "", rownames(rand_coeff))
      Xwtp <- as.matrix(choice_set[, attr_names, drop = FALSE])  # n_alts × n_attrs
      wtp_utility <- Xwtp %*% rand_coeff  # n_alts × n_draws
      price_matrix <- matrix(rep(choice_set$Price, n_draws), nrow = n_alts, ncol = n_draws)
      V_matrix <- fixed_coeff[["scalingFactor"]] * (wtp_utility - price_matrix)
    }
  }
  
  # Calculate probabilities that sum to 1 for each draw
  exp_V <- exp(V_matrix)  # n_alts × n_draws
  prob_matrix <- exp_V / matrix(rep(colSums(exp_V) + og_expV, each = n_alts), nrow = n_alts)
  
  prob_matrix <- t(prob_matrix)  # n_draws × n_alts
  colnames(prob_matrix) <- choice_set$veh_id
  
  prob = colMeans(prob_matrix)
  
  return(prob)
}

# MXL wrapper
calculate_mxl_prob <- function(choice_set, model, n_draws = 1000) {
  # Draw coefficients
  coeff_draws <- mxl_draw_coefficients(model$estimate,
                                       model$apollo_inputs$utility_space, 
                                       n_draws = n_draws,
                                       scalingFactor_dist = scalingFactor_dist)
  
  # Calculate probabilities
  mxl_prob(choice_set = choice_set,
           rand_coeff = coeff_draws$rand_coeff,
           fixed_coeff = coeff_draws$fixed_coeff,
           utility_space = model$apollo_inputs$utility_space,
           scalingFactor_dist = scalingFactor_dist)
}

wrap_mxl_prob_func <- function(model, n_draws = 1000){
  scalingFactor_dist = model$apollo_inputs$scalingFactor_dist
  # Draw coefficients
  coeff_draws <- mxl_draw_coefficients(model$estimate,
                                       model$apollo_inputs$utility_space, 
                                       n_draws = n_draws,
                                       scalingFactor_dist = scalingFactor_dist)
  # Return wrapped prob func
  choice_prob_func <- function(choice_set){
    mxl_prob(choice_set = choice_set,
             rand_coeff = coeff_draws$rand_coeff,
             fixed_coeff = coeff_draws$fixed_coeff,
             utility_space = model$apollo_inputs$utility_space,
             scalingFactor_dist = scalingFactor_dist)
  }
  return(choice_prob_func)
}

###### LC-NL ###### 
lcnl_extract_parameters <- function(beta) {
  # Get class intercepts
  class_intercept_names <- grep("^class_intercept_", names(beta), value = TRUE)
  class_intercepts <- beta[class_intercept_names]
  n_classes <- length(class_intercepts)
  
  # Get nesting parameters (lambda and alpha parameters)
  nesting_param_names <- grep("^(lambda_|alpha_)", names(beta), value = TRUE)
  nesting_params <- beta[nesting_param_names]
  
  # Get preference coefficients (everything else)
  pref_param_names <- setdiff(names(beta), c(class_intercept_names, nesting_param_names))
  pref_params <- beta[pref_param_names]
  
  # Shape preference coefficients to (n_classes x n_attrs matrix)
  n_attrs <- length(pref_params) / n_classes
  pref_names <- names(pref_params)
  attr_base <- sub("_[0-9]+$", "", pref_names)
  attr_names <- unique(attr_base)
  
  pref_coeff <- matrix(pref_params,
                       nrow = n_classes,
                       ncol = n_attrs,
                       byrow = TRUE,
                       dimnames = list(
                         paste0("class_", seq_len(n_classes)), 
                         attr_names))
  
  return(list(
    pref_coeff = pref_coeff,
    class_intercepts = class_intercepts,
    nesting_params = nesting_params,
    n_classes = n_classes
  ))
}

lcnl_prob <- function(choice_set, pref_coeff, class_intercepts, nesting_params, 
                      observed_attributes, utility_space, nesting_spec, 
                      attr_intercept = NULL, og_expV = 0, nesting_structure = NULL) {
  
  n_alts <- nrow(choice_set)
  n_classes <- nrow(pref_coeff)
  
  # Calculate choice probabilities within each class (using nested logit)
  class_choice_probs <- sapply(seq_len(n_classes), function(class_j) {
    # Get class-specific preference coefficients
    class_beta <- pref_coeff[class_j, ]
    
    # Get class-specific nesting parameters
    class_nesting_param_names <- grep(paste0("_", class_j, "$"), names(nesting_params), value = TRUE)
    class_nesting_params <- nesting_params[class_nesting_param_names]
    
    # Remove class suffix from parameter names for build_nesting_structure
    names(class_nesting_params) <- sub(paste0("_", class_j, "$"), "", names(class_nesting_params))
    
    # Create combined beta vector for this class (preferences + nesting params)
    class_beta_combined <- c(class_beta, class_nesting_params)
    
    # Calc choice prob
    # Use existing nl_prob function for this class
    if(nesting_spec == "CNL") {
        class_probs <- cnl_prob(choice_set = choice_set,
                              beta = class_beta_combined,
                              observed_attributes = observed_attributes,
                              utility_space = utility_space,
                              og_expV = og_expV,
                              intercept = attr_intercept,
                              nesting_spec = nesting_spec,
                              nesting_structure = nesting_structure)
    } else{
      # Calc choice prob
      class_probs <- nl_prob(choice_set = choice_set,
                             beta = class_beta_combined,
                             observed_attributes = observed_attributes,
                             utility_space = utility_space,
                             og_expV = og_expV,
                             intercept = attr_intercept,
                             nesting_spec = nesting_spec,
                             nesting_structure = nesting_structure)
    }
    
    return(class_probs[choice_set$veh_id])
  })
  
  rownames(class_choice_probs) <- choice_set$veh_id
  colnames(class_choice_probs) <- paste0("class_", seq_len(n_classes))
  
  # Calculate class membership probabilities using softmax on intercepts
  exp_intercepts <- exp(class_intercepts)
  class_membership_probs <- exp_intercepts / sum(exp_intercepts)
  
  # Calculate overall probabilities (weighted by class membership)
  prob <- as.vector(class_choice_probs %*% class_membership_probs)
  names(prob) <- choice_set$veh_id
  
  return(prob)
}

wrap_lcnl_prob_func <- function(model, init_choice_set) {
  # Extract class parameters
  lcnl_params <- lcnl_extract_parameters(model$estimate)
  
  # Return closure function
  choice_prob_func <- function(choice_set) {
    lcnl_prob(choice_set = choice_set,
              pref_coeff = lcnl_params$pref_coeff,
              class_intercepts = lcnl_params$class_intercepts,
              nesting_params = lcnl_params$nesting_params,
              observed_attributes = model$apollo_inputs$observed_attributes,
              utility_space = model$apollo_inputs$utility_space,
              nesting_spec = model$apollo_inputs$nesting_spec)
  }
  return(choice_prob_func)
}

###### Wrappers ###### 
wrap_model <- function(model){
  if (model$apollo_inputs$nesting_spec == "none"){
    choice_prob_func <- switch(model$apollo_inputs$heterogeneity_spec,
                               "none" = wrap_mnl_prob_func(model),
                               "latent"=wrap_lcl_prob_func(model),
                               "mixed" = wrap_mxl_prob_func(model, n_draws = model$apollo_inputs$n_draws),
                               stop("Unknown heterogeneity_spec: ", model$apollo_inputs$heterogeneity_spec))
  } else if (model$apollo_inputs$nesting_spec == "CNL") {
    # Cross-nesting
    choice_prob_func <- switch(model$apollo_inputs$heterogeneity_spec,
                               "none" = wrap_cnl_prob_func(model),
                               "latent" = wrap_lcnl_prob_func(model),
                               # "latent" = "lc-nl",
                               # "mixed" = "mx-nl",
                               stop("Unknown heterogeneity_spec: ", model$apollo_inputs$heterogeneity_spec))
  } else{
    # Nesting
    choice_prob_func <- switch(model$apollo_inputs$heterogeneity_spec,
                               "none" = wrap_nl_prob_func(model),
                               "latent" = wrap_lcnl_prob_func(model),
                               stop("Unknown heterogeneity_spec: ", model$apollo_inputs$heterogeneity_spec))
  }
  return(choice_prob_func)
}

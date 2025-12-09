

load_apollo_inputs <- function(
    logit_type           = "MNL",       # Name
    heterogeneity_spec   = "none",      # "none", "latent", "mixed"
    nesting_spec         = "none",      # "none", "NL-GAS", "NL-PLUG", "NL-THREE", "CNL"
    n_classes            = 2,           # NA or number
    utility_space        = "pref",      # "pref" or "WTP"
    lognorm_price_dist   = TRUE,
    scalingFactor_dist = TRUE,
    workInLogs           = FALSE,
    observed_attributes  = c("Price","Acceleration", "OpCost", "hev", "phev20", "phev40", "bev", "bevRangeRel", "noBEVFC", "american", "chinese", "japanese", "skorean"), 
    # c("Price","Acceleration", "OpCost", "hev", "phev20", "phev40", "bev", "bevRangeRel", "noBEVFC")
    database, 
    output_directory, 
    n_cores=1
    ){
  
  # Initialize Apollo
  apollo_initialise()
  # database <- database
  apollo_fixed <- c()
  base_beta_names <- c()
  
  # Set control parameters
  apollo_control <- list(
    modelName = logit_type,
    indivID         = "ID",
    panelData       = TRUE,
    weights         = 'Weight2018',
    outputDirectory = output_directory,
    nCores          = n_cores,
    noModification  = TRUE,
    calculateLLC    = FALSE,
    workInLogs      = workInLogs # FALSE
  )
  
  # Set mixing to T/F
  apollo_control$mixing <- tolower(heterogeneity_spec) == "mixed"
  
  # Load the MNL preference for initialization
  mnl_pref_estimates <- read.csv("inputs/MNL_pref_estimates.csv")
  
  ### Define preference parameters
  constraints <- c()
  
  # Specify utility in preference or WTP space
  if (tolower(utility_space) == "pref") {
    beta_names <- paste0("b_", observed_attributes)
    beta_init  <- numeric(length(beta_names))
    
    # Initialize
    for (attr in observed_attributes) {
      beta_name <- paste0("b_", attr)
      # Initialize the beta values based on the estimates from MNL
      if (beta_name %in% mnl_pref_estimates$X) {
        beta_init[match(beta_name, beta_names)] <- mnl_pref_estimates$Estimate[match(beta_name, mnl_pref_estimates$X)]
      }
    }
  } else if (tolower(utility_space) == "wtp") {
    # In WTP space, drop "Price" (scalingFactor covers price)
    attrs_no_price <- setdiff(observed_attributes, "Price")

    # Initialize beta_names for WTP space
    beta_names <- c("scalingFactor", paste0("wtp_", attrs_no_price))
    beta_init <- numeric(length(beta_names))
    
    # Compute scalingFactor from the MNL pref space (the negative of the price coefficient b_Price)
    scalingFactor <- -1 * mnl_pref_estimates$Estimate[mnl_pref_estimates$X == "b_Price"]
    beta_init[1] <- scalingFactor
    
    # Load MNL preference coefficients from CSV and update the WTP coefficients
    for (attr in attrs_no_price) {
      beta_name <- paste0("b_", attr)
      if (beta_name %in% mnl_pref_estimates$X) {
        # Find the index of the corresponding wtp_<attr> and update its value
        idx <- match(paste0("wtp_", attr), beta_names)
        beta_init[idx] <- mnl_pref_estimates$Estimate[mnl_pref_estimates$X == beta_name] / scalingFactor
      } else {
        warning(paste("Missing coefficient for", beta_name, "in MNL estimates"))
      }
    }
  } else {
    stop("`utility_space` must be either 'pref' or 'wtp'.")
  }
  
  
  # Add nesting parameters
  ns <- toupper(nesting_spec)
  # Prepare an empty nesting_params container
  nesting_params <- list(
    names = character(0),
    inits = numeric(0)
  )

  if (!is.na(ns)) {
    nesting_params <- switch(
      ns,
      "NONE" = nesting_params, # define even when empty to return uniform outputs 
      
      "NL-GAS" = list(
        names = c("lambda_GAS", "lambda_BEV"),
        inits = c(1, 1)
      ),
      
      "NL-PLUG" = list(
        names = c("lambda_ICE", "lambda_PLUG"),
        inits = c(1, 1)
      ),
      
      
      "NL-THREE" = list(
        names = c("lambda_ICE", "lambda_PHEV", "lambda_BEV"),
        inits = c(1, 1, 1)
      ),

      "CNL" = list(
        # names = c("lambda_BEV", "alpha_HEV_ICE_logistic", "alpha_PHEV_ICE_logistic"),
        names = c("lambda_ICE", "lambda_BEV", "alpha_PHEV_ICE_logistic"),
        inits = c(1, 1, 0.5)
      ),
      
      stop(sprintf("Unrecognized nesting_spec '%s'", nesting_spec))
    )
    # Append once
    beta_names <- c(beta_names, nesting_params$names)
    beta_init  <- c(beta_init,  nesting_params$inits)
  }
  
  # Add latent class parameters
  if (!is.na(heterogeneity_spec) && tolower(heterogeneity_spec) == "latent") {
    if (is.null(n_classes) || n_classes < 1) {
      stop("`n_classes` must be a positive integer when `heterogeneity_spec = 'latent'`.")
    }
    # 1) Save the originals
    base_beta_names <- beta_names
    base_inits <- beta_init

    # 2) Initialize latent class arrays
    beta_names <- character(0)
    beta_init  <- numeric(0)
    
    # 3) For each class, recreate every parameter (_i) + class_intercept_i
    for (i in 1:n_classes) {
      # 3a) copy every base parameter but add "_i"
      for (k in seq_along(base_beta_names)) {
        new_name <- paste0(base_beta_names[k], "_", i)
        new_init <- base_inits[k]       # keep same initial value
        beta_names <- c(beta_names, new_name)
        beta_init  <- c(beta_init,  new_init)
      }
      # 3b) then add class_intercept_i = 0
      class_name <- paste0("class_intercept_", i)
      beta_names <- c(beta_names, class_name)
      beta_init  <- c(beta_init, 0)
      # Fix first class param
      if (i == 1) apollo_fixed <- c(apollo_fixed, class_name)
    }
    
    # 4) Differentiate latent classes (required by apollo) by perturbing the first beta of each class
    first_base <- base_beta_names[1]
    for (i in 1:n_classes) {
      class_first_name <- paste0(first_base, "_", i)
      idx <- match(class_first_name, beta_names)
      if (!is.na(idx)) {
        # Perturb by multiplying the first param of each class by a decreasing factor (e.g., 100%, 99%, 98%, etc.)
        beta_init[idx] <- beta_init[idx] * (1 - 0.01 * (i - 1))
        # beta_init[idx] <- beta_init[idx] - 0.01 * i # perturb by 0.01
      }
    }
  } else if (!is.na(heterogeneity_spec) && tolower(heterogeneity_spec) == "mixed"){
  # Mixed logit
  # 1) Save the originals
  base_beta_names <- beta_names
  base_inits <- beta_init
  # 2) Initialize new containers for the mixed‐logit betas
  beta_names <- character(0)
  beta_init  <- numeric(0)
  # 3) Loop over each “base” parameter
  #    – In “pref” space, randomize every b_<attr> (but keep lambda_/alpha_ unchanged)
  #    – In “wtp” space, randomize only wtp_<attr> (but keep scalingFactor and lambda_/alpha_ unchanged)
  if (tolower(utility_space) == "pref") {
    for (i in seq_along(base_beta_names)) {
      param <- base_beta_names[i]
      init  <- base_inits[i]
      
      # If this is a lambda_ or alpha_ (nesting parameter), leave it fixed
      if (grepl("^(lambda_|alpha_)", param)) {
        beta_names <- c(beta_names, param)
        beta_init  <- c(beta_init,  init)
      } else { # if if price & MXL lognorm price, take log (& define sigma log(0.1))
        # Otherwise (any b_<attr>), create mu_ and sigma_ versions
        if (param == "b_Price" && lognorm_price_dist == TRUE){
          # Initialize mu for log-normal distribution as log(Price)
          mu_val <- exp(init)  # Log of the Price for normal space (mu)
          sigma_val <- 0     # Standard deviation for log-normal, set as 0
          # Add the log-normal parameters
          beta_names <- c(beta_names, paste0("mu_", param), paste0("sigma_", param))
          beta_init  <- c(beta_init, mu_val, sigma_val)
        }else{
          beta_names <- c(
            beta_names,
            paste0("mu_",    param),
            paste0("sigma_", param)
          )
          # Keep the same initial value for mu; set sigma’s init = 0
          beta_init <- c(beta_init, init, 0)
        }
      }
    }
  } else { # utility_space == "wtp"
    for (i in seq_along(base_beta_names)) {
      param <- base_beta_names[i]
      init  <- base_inits[i]
      # If this is scalingFactor or a nesting parameter, leave it as a point estimate
      if (grepl("^(lambda_|alpha_)", param)) {
        beta_names <- c(beta_names, param)
        beta_init  <- c(beta_init,  init)
      } else if (param == "scalingFactor") { 
        if (scalingFactor_dist) {
          # Otherwise, it must be wtp_<attr>; create mu_/sigma_ versions
          beta_names <- c(
            beta_names,
            paste0("mu_",    param),
            paste0("sigma_", param)
          )
          # Keep the same initial value for mu; set sigma’s init = 0
          beta_init <- c(beta_init, init, 0)
        } else{ # Deterministic scaling factor
          beta_names <- c(beta_names, param)
          beta_init  <- c(beta_init,  init)
        }

      } else {
        # Otherwise, it must be wtp_<attr>; create mu_/sigma_ versions
        beta_names <- c(
          beta_names,
          paste0("mu_",    param),
          paste0("sigma_", param)
        )
        # Keep the same initial value for mu; set sigma’s init = 0
        beta_init <- c(beta_init, init, 0)
        }
      }
    }
  }
  
  # Constrain CNL alpha term to prevent flat gradients from logistic func (e.g., exp(30)/(1 + exp(30)) = 1)
  if (toupper(nesting_spec) == "CNL") {
    if (is.na(heterogeneity_spec) || tolower(heterogeneity_spec) == "none") {
      # Regular CNL
      constraints <- c(constraints, 
                       "alpha_PHEV_ICE_logistic < 10", 
                       "alpha_PHEV_ICE_logistic > -10")
                       # "10 - alpha_PHEV_ICE_logistic > 0", # 10 > alpha
                       # "10 + alpha_PHEV_ICE_logistic > 0") # -10 < alpha
    } else if (tolower(heterogeneity_spec) == "latent") {
      # CNL with latent classes - add constraints for each class
      for (i in 1:n_classes) {
        alpha_param <- paste0("alpha_PHEV_ICE_logistic_", i)
        constraints <- c(constraints,
                         paste0(alpha_param, " < 10"),
                         paste0(alpha_param, " > -10"))
                         # paste0("10 - ", alpha_param, " > 0"),
                         # paste0("10 + ", alpha_param, " > 0"))
      }
    } else {
      stop("CNL not yet supported for the MXL")
    }
  } else{
    constraints <- c()
  }

  # Set names & values for preference parameters
  apollo_beta <- setNames(beta_init, beta_names)
  return(list(
    apollo_control = apollo_control,
    apollo_beta    = apollo_beta,
    apollo_fixed   = apollo_fixed,
    # base_beta_names= base_beta_names,
    nesting_params = nesting_params,
    constraints    = constraints
  ))
}

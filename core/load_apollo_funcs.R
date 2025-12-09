
# NOTE: Instead of putting conditional for nesting in apollo_probabilities, I 
# hardcoded for with & w/o nesting for performance (& readability), since these
# functions are called many times. The compact functions with variable nesting
# specificaiton can be found in 'load_apollo_funcs_variable_nesting.R'

# # Helpers to get functions for particular model
load_apollo_model <- function(apollo_inputs, apollo_beta){
  # Conditional to setup specific model
  if (!is.na(apollo_inputs$nesting_spec) && tolower(apollo_inputs$nesting_spec) == "none"){
    # Non-nested models
    if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "none"){
      # MNL
      apollo_inputs <- apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- mnl_probability_function(apollo_beta, apollo_inputs)
      
    } else if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "latent") {
      # LCL
      apollo_lcPars <<- lcl_lcPars_function(apollo_beta, apollo_inputs)
      apollo_inputs <- apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- lcl_probability_function(apollo_beta, apollo_inputs)
    } else if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "mixed") {
      # MXL
      apollo_draws <<- mxl_draws_function(apollo_inputs)  # Need to be in global env for apollo (hence <<-)
      apollo_randCoeff <<- mxl_randCoeff_function()
      apollo_inputs <-  apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- mxl_probability_function(apollo_beta, apollo_inputs)
    }
  } else{
    # Nested models
    if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "none"){
      # NL
      apollo_inputs <-  apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- nl_probability_function(apollo_beta, apollo_inputs)
    } else if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "latent") {
      # LC-NL
      apollo_lcPars <<- lcnl_lcPars_function()
      apollo_inputs <-  apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- lcnl_probability_function()
    } else if (!is.na(apollo_inputs$heterogeneity_spec) && tolower(apollo_inputs$heterogeneity_spec) == "mixed") {
      # MX-NL
      apollo_draws <<- mxl_draws_function(apollo_inputs)
      apollo_randCoeff <<- mxl_randCoeff_function()
      apollo_inputs <-  apollo_validateInputs(recycle=TRUE)
      apollo_probabilities <- mxnl_probability_function() 
    }
  }
  return(list(
    apollo_inputs = apollo_inputs,
    apollo_probabilities = apollo_probabilities
  ))
}

# ## Scrapped: helper functions to load each model
# define_mnl <- function(apollo_beta, apollo_inputs){
#   
#   apollo_inputs = apollo_validateInputs(recycle=TRUE)
#   
#   apollo_probabilities <- mnl_probability_function()
#   
#   return(list(
#     apollo_inputs = apollo_inputs, 
#     apollo_probabilities = apollo_probabilities
#   ))
#   
# }
# 
# define_lcl <- function(apollo_beta, apollo_inputs){
#   
#   apollo_lcPars <- lcl_lcPars_function()
#   
#   apollo_inputs <- apollo_validateInputs(recycle=TRUE)
#   
#   apollo_probabilities <- lcl_probability_function()
#   
#   return(list(
#     apollo_inputs = apollo_inputs, 
#     apollo_probabilities = apollo_probabilities
#   ))
# }
# 
# define_mxl <- function(apollo_beta, apollo_inputs, n_draws){
#   apollo_draws <- mxl_draws_function(n_draws, observed_attributes)
#   apollo_randCoeff <- mxl_randCoeff_function()
#   
#   # Define apollo inputs
#   apollo_inputs <-  apollo_validateInputs(recycle=TRUE)
#   
#   apollo_probabilities <- mxl_probability_function(apollo_beta, apollo_inputs)
#   
#   return(list(
#     apollo_inputs = apollo_inputs, 
#     apollo_probabilities = apollo_probabilities,
#     apollo_draws = apollo_draws
#   ))
# }

# Main apollo functions

## DECOMISSION MNL W/ NESTING STRUCTURE
mnl_probability_function_nested <- function(apollo_beta, apollo_inputs){
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    # Get utility & availability
    V <- list()
    A <- list()

    # Define equation based on utility space
    prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
    # Loop over alternatives
    for (altName in apollo_inputs$availCols) {
      alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
      # Get availability
      A[[altName]] <- get(altName)
      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
        # Build the string: scalingFactor*(wtp_OpCost*OpCost<suffix> + … – Price<suffix>)
        v_expr <- str_glue("
        scalingFactor * (
          {paste0(
             \"wtp_\", attrs_no_price,
             \" * \", attrs_no_price, alt_suffix,
             collapse = \" + \"
          )}
          - Price{alt_suffix}
        )")
      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        v_expr <- str_glue("{paste0(
             \"b_\", apollo_inputs$observed_attributes,
             \" * \", apollo_inputs$observed_attributes, alt_suffix,
             collapse = \" + \"
          )}")
      }
      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }

    # Logit-type specific settings
    mnl_settings <- list(
      alternatives = setNames(seq_along(apollo_inputs$availCols),
                              apollo_inputs$availCols),
      choiceVar    = AltChoice,
      utilities    = V,
      avail        = A
    )

    # Define probability P
    P <- list()
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_panelProd(P, apollo_inputs, functionality)
    P <- apollo_weighting(P, apollo_inputs, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}

mnl_probability_function <- function(apollo_beta, apollo_inputs) {
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    # Get utility
    V <- list()

    # Define alternatives
    # alternatives <- c("C1", "C2", "C3")
    alternatives <- unique(gsub("^.*C", "C", apollo_inputs$availCols))  # c("C1", "C2", "C3", "C4")
    alt_codes <- setNames(seq_along(alternatives), alternatives) # For capability with expanded set
    
    # Loop over alternatives
    for (altName in alternatives) {

      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")

        # Build utility expression for WTP space
        attr_terms <- paste0(
          "wtp_", attrs_no_price, " * ", attrs_no_price, altName,
          collapse = " + "
        )
        v_expr <- paste0("scalingFactor * (", attr_terms, " - Price", altName, ")")

      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        attr_terms <- paste0(
          "b_", apollo_inputs$observed_attributes, " * ",
          apollo_inputs$observed_attributes, altName,
          collapse = " + "
        )
        v_expr <- attr_terms
      }

      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }
    
    # Logit-type specific settings
    mnl_settings <- list(
      alternatives = alt_codes,
      choiceVar    = Choice,
      utilities    = V
    )

    # Define probability P
    P <- list()
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    P <- apollo_panelProd(P, apollo_inputs, functionality)
    P <- apollo_weighting(P, apollo_inputs, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}

lcl_lcPars_function <- function(apollo_beta, apollo_inputs){
  apollo_lcPars <- function(apollo_beta, apollo_inputs) {
    
    # Specify vars 
    n <- apollo_inputs$n_classes
    all_attrs <- apollo_inputs$observed_attributes
    uspace    <- tolower(apollo_inputs$utility_space)
    prefix    <- if (uspace == "pref") "b_" else "wtp_"
    
    # Determine which attributes to loop over
    attrs <- if (uspace == "wtp") {
      # In WTP space, drop Price, add scalingFactor
      c("scalingFactor", setdiff(all_attrs, "Price"))
    } else {
      all_attrs
    }
    
    lcpars <- list()
    
    # Taste parameters: one list per attribute, entries by class
    for (attr in attrs) {
      param_base <- if (attr == "scalingFactor") "scalingFactor" else paste0(prefix, attr)
      tmp <- vector("list", n)
      for (i in seq_len(n)) {
        tmp[[i]] <- apollo_beta[[ paste0(param_base, "_", i) ]]
      }
      names(tmp) <- paste0("class_", seq_len(n))
      lcpars[[ param_base ]] <- tmp
    }
    
    # Class intercepts: same pattern, but use “j” as the index
    intercepts <- vector("list", length = n)
    for (j in seq_len(n)) {
      intercepts[[j]] <- apollo_beta[[ paste0("class_intercept_", j) ]]
    }
    names(intercepts) <- paste0("class_", seq_len(n))
    lcpars[["class_intercept"]] <- intercepts
    
    # Build pi_values
    utilities <- intercepts
    classes   <- setNames(seq_len(n), names(intercepts))
    lcpars[["pi_values"]] <- apollo_classAlloc(
      list(classes = classes, utilities = utilities)
    )
    
    return(lcpars)
  }
}

lcl_probability_function_nested <- function(apollo_beta, apollo_inputs){
  # Define probabilities function for LCL with n classes
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
    
    # Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    # Create list of probabilities P
    P = list()
    
    # Define utilities dynamically based on availCols
    V = list()
    A = list()
    
    # Define availability condition for each alternative
    for (a in apollo_inputs$availCols){
      A[[a]] = get(a)
    }
    
    ### Loop over classes
    for (s in 1:apollo_inputs$n_classes) {
      
      # Loop over alts
      for (altName in apollo_inputs$availCols){
        # Extract last two characters (e.g., "C1", "C2", "C3") for correct attribute lookup
        alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
        
        # prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
        
        if (tolower(apollo_inputs$utility_space) == "wtp") {
          # Drop "Price" from observed_attributes for the wtp_ part
          attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
          # Build the string: scalingFactor_#*(wtp_OpCost_#*OpCost<suffix> + … – Price<suffix>)
          v_expr <- str_glue("
          scalingFactor_{s} * (
            {paste0(
               \"wtp_\", attrs_no_price, \"_\", s,
               \" * \", attrs_no_price, alt_suffix,
               collapse = \" + \"
            )}
            - Price{alt_suffix}
          )")
        } else {
          # PREF space: sum(b_attr * attr<alt_suffix>)
          v_expr <- str_glue("{paste0(
            \"b_\", apollo_inputs$observed_attributes, \"_\", s,
            \" * \", apollo_inputs$observed_attributes, alt_suffix,
            collapse = \" + \"
          )}"
          )
        }
        
        V[[altName]] <- eval(parse(text = v_expr))
      }
      
      # Define settings for MNL model component
      mnl_settings = list(
        alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A  # Availability conditions
      )
      
      # Compute within-class choice probabilities using MNL model
      P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)
      
      # Take product across observation for same individual
      P[[paste0("Class_", s)]] = apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
    }
    
    
    ### Compute latent class model probabilities (combining all classes)
    lc_settings = list(
      inClassProb = P,       # Probabilities from each class
      classProb   = pi_values # Class probabilities (e.g., `delta_1`, `delta_2`)
    )
    
    # Calculate the overall model probabilities using latent class model
    P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
    
    # Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
}

lcl_probability_function <- function(apollo_beta, apollo_inputs) {
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    
    # Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    # Create list of probabilities P
    P <- list()
    
    # Define alternatives
    # alternatives <- c("C1", "C2", "C3")
    alternatives <- unique(gsub("^.*C", "C", apollo_inputs$availCols))  # c("C1", "C2", "C3", "C4")
    alt_codes <- setNames(seq_along(alternatives), alternatives) # For capability with expanded set
    
    ### Loop over classes
    for (s in 1:apollo_inputs$n_classes) {
      
      # Get utility for this class
      V <- list()
      
      # Loop over alternatives
      for (altName in alternatives) {
        
        # Conditional on utility space
        if (tolower(apollo_inputs$utility_space) == "wtp") {
          # Drop "Price" from observed_attributes for the wtp_ part
          attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
          
          # Build utility expression for WTP space with class-specific parameters
          attr_terms <- paste0(
            "wtp_", attrs_no_price, "_", s, " * ", attrs_no_price, altName,
            collapse = " + "
          )
          v_expr <- paste0("scalingFactor_", s, " * (", attr_terms, " - Price", altName, ")")
          
        } else {
          # PREF space: sum(b_attr_class * attr<suffix>)
          attr_terms <- paste0(
            "b_", apollo_inputs$observed_attributes, "_", s, " * ", 
            apollo_inputs$observed_attributes, altName,
            collapse = " + "
          )
          v_expr <- attr_terms
        }
        
        # Expression for utility
        V[[altName]] <- eval(parse(text = v_expr))
      }
      
      # Define settings for MNL model component for this class
      mnl_settings <- list(
        alternatives = alt_codes,
        choiceVar    = Choice,
        utilities    = V
      )
      
      # Compute within-class choice probabilities using MNL model
      P[[paste0("Class_", s)]] <- apollo_mnl(mnl_settings, functionality)
      
      # Take product across observation for same individual
      P[[paste0("Class_", s)]] <- apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
    }
    
    ### Compute latent class model probabilities (combining all classes)
    lc_settings <- list(
      inClassProb = P,         # Probabilities from each class
      classProb   = pi_values  # Class probabilities (e.g., `delta_1`, `delta_2`)
    )
    
    # Calculate the overall model probabilities using latent class model
    P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
    
    # Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    # Prepare and return outputs of function
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}

mxl_draws_function <- function(apollo_inputs){
  # Define draws for mixed logit model
  if (apollo_inputs$scalingFactor_dist){
    draws_names_list <- c("draws_scalingFactor", paste0("draws_", apollo_inputs$observed_attributes))
  } else{
    draws_names_list <- paste0("draws_", apollo_inputs$observed_attributes)
  }
  apollo_draws = list(
    interDrawsType = "mlhs",  # Use MLHS for inter-individual draws
    interNDraws    = apollo_inputs$n_draws,
    interUnifDraws = c(),
    interNormDraws = draws_names_list
    # intraDrawsType = "sobolOwen",  # Use Sobol with Owen scrambling for intra-individual draws
    # intraNDraws    = 0,  # Ignore intra-individual heterogeneity (i.e. heterogeneity across observations for the same individual))  
    # intraUnifDraws = c(),
    # intraNormDraws = c()
  )
  return(apollo_draws)
}

mxl_randCoeff_function <- function(){
  
  # Note: need observed attributes & utility space as var in the global space, could produce issue when parallel processing
  apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
    randcoeff <- list()
    
    # Pull out utility_space and observed_attributes from apollo_inputs
    space <- tolower(apollo_inputs$utility_space) # these might need to be apollo_inputs$utility space, but since apollo_randCoeff is defined before apollo inputs, it cannot be passed in
    attrs <- apollo_inputs$observed_attributes
    
    # PREF space: randomize b_<attr> for every attribute
    if (space == "pref") {
      for (attr in attrs) {
        mu_name    <- paste0("mu_b_",    attr)
        sigma_name <- paste0("sigma_b_", attr)
        draw_name  <- paste0("draws_",   attr)
        out_name   <- paste0("b_",       attr)
        
        mu_val    <- apollo_beta[mu_name]
        sigma_val <- apollo_beta[sigma_name]
        draw_val  <- apollo_inputs$draws[[draw_name]]
        
        if (!is.null(apollo_inputs$lognorm_price_dist) && apollo_inputs$lognorm_price_dist == TRUE && attr == "Price") {
          # log-normal: always negative coeff for price
          randcoeff[[out_name]] <- -1* exp(mu_val + sigma_val * draw_val)
        } else {
          # normal distribution for all  other param
          randcoeff[[out_name]] <- mu_val + sigma_val * draw_val
        }
        # randcoeff[[out_name]] <- mu_val + sigma_val * draw_val
      
      }
      # WTP space: randomize wtp_<attr> for each attribute except "Price"
    } else if (space == "wtp") {
      
      if (apollo_inputs$scalingFactor_dist){ # add scalingFactor distribution if applicable
        mu_name <- paste0("mu_scalingFactor")
        sigma_name <- paste0("sigma_scalingFactor")
        draw_name  <- paste0("draws_scalingFactor")
        out_name   <- paste0("scalingFactor")
        
        mu_val    <- apollo_beta[mu_name]
        sigma_val <- apollo_beta[sigma_name]
        draw_val  <- apollo_inputs$draws[[draw_name]]
        randcoeff[[out_name]] <- mu_val + sigma_val * draw_val
      }
      attrs_no_price <- setdiff(attrs, "Price")
      for (attr2 in attrs_no_price) {# not sure why, but I have to give attr a different name or R gives an error about re-using loop indicies
        mu_name    <- paste0("mu_wtp_",    attr2)
        sigma_name <- paste0("sigma_wtp_", attr2)
        draw_name  <- paste0("draws_",      attr2)
        out_name   <- paste0("wtp_",       attr2)
        
        mu_val    <- apollo_beta[mu_name]
        sigma_val <- apollo_beta[sigma_name]
        draw_val  <- apollo_inputs$draws[[draw_name]]
        
        randcoeff[[out_name]] <- mu_val + sigma_val * draw_val
      }
    }
    return(randcoeff)
  }
}

mxl_probability_function_nested <- function(apollo_beta, apollo_inputs){
  # Define probabilities function
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate") {

    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    ### Create list of probabilities P
    P = list()

    ### Define utilities dynamically based on availCols
    V = list()
    A = list()

    # Define equation based on utility space
    prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
    # Loop over alternatives
    for (altName in apollo_inputs$availCols) {
      alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
      # Get availability
      A[[altName]] <- get(altName)
      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
        # Build the string: scalingFactor*(wtp_OpCost*OpCost<suffix> + … – Price<suffix>)
        v_expr <- str_glue("
        scalingFactor * (
          {paste0(
             \"wtp_\", attrs_no_price,
             \" * \", attrs_no_price, alt_suffix,
             collapse = \" + \"
          )}
          - Price{alt_suffix}
        )")
      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        v_expr <- str_glue("{paste0(
             \"b_\", apollo_inputs$observed_attributes,
             \" * \", apollo_inputs$observed_attributes, alt_suffix,
             collapse = \" + \"
          )}")
      }
      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }

    ### Define settings for MNL model component
    mnl_settings = list(
      alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
      choiceVar     = AltChoice,  # Choice variable column in data
      utilities     = V,  # Utilities for each alternative
      avail         = A  # Availability conditions
    )

    ### Compute probabilities using MNL model
    P[["model"]] = apollo_mnl(mnl_settings, functionality)

    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)

    ### Average across inter-individual draws
    P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)

    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)

    return(P)
  }
}

mxl_probability_function <- function(apollo_beta, apollo_inputs) {
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    # Create list of probabilities P
    P <- list()
    
    # Get utility
    V <- list()
    
    # Define alternatives
    # alternatives <- c("C1", "C2", "C3")
    alternatives <- unique(gsub("^.*C", "C", apollo_inputs$availCols))  # c("C1", "C2", "C3", "C4")
    alt_codes <- setNames(seq_along(alternatives), alternatives) # For capability with expanded set
    
    # Loop over alternatives
    for (altName in alternatives) {
      
      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
        
        # Build utility expression for WTP space
        attr_terms <- paste0(
          "wtp_", attrs_no_price, " * ", attrs_no_price, altName,
          collapse = " + "
        )
        v_expr <- paste0("scalingFactor * (", attr_terms, " - Price", altName, ")")
        # if (apollo_inputs$scalingFactor_dist){
        #   browser()
        # } else{
        #   attr_terms <- paste0(
        #     "wtp_", attrs_no_price, " * ", attrs_no_price, altName,
        #     collapse = " + "
        #   )
        #   v_expr <- paste0("scalingFactor * (", attr_terms, " - Price", altName, ")")
        # }
      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        attr_terms <- paste0(
          "b_", apollo_inputs$observed_attributes, " * ", 
          apollo_inputs$observed_attributes, altName,
          collapse = " + "
        )
        v_expr <- attr_terms
      }
      
      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }
    
    # Mixed logit specific settings
    mnl_settings <- list(
      alternatives = alt_codes,
      choiceVar    = Choice,
      utilities    = V
    )
    
    # Compute probabilities using MNL model
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    
    # Take product across observation for same individual
    P <- apollo_panelProd(P, apollo_inputs, functionality)
    
    # Average across inter-individual draws (key difference for mixed logit)
    P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    # Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    # Prepare and return outputs of function
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}


# # Function to return one of four nested‐logit specifications
### HARDCODED VERSION, DEPRECIATED
# get_nl_spec <- function(spec_type) {
#   # spec_type must be one of:
#   #   "NL-GAS"        → nests: GAS vs BEV
#   #   "NL-PLUG"       → nests: ICE vs PLUG
#   #   "NL-THREE"   → nests: ICE vs PHEV vs BEV
#   #   "CNL"            → cross‐nested logit (placeholder)
#   
#   spec_type <- toupper(spec_type)
#   
#   if (spec_type == "NL-GAS") { 
#     nlNests <- list(
#       root = 1,
#       GAS  = lambda_GAS,
#       BEV  = lambda_BEV
#     )
#     nlStructure <- list(
#       root = c("GAS", "BEV"),
#       GAS  = c(
#         "cvC1", "cvC2", "cvC3",
#         "hevC1","hevC2","hevC3",
#         "phev20C1","phev20C2","phev20C3",
#         "phev40C1","phev40C2","phev40C3"
#       ),
#       BEV  = c(
#         "bev100C1","bev100C2","bev100C3",
#         "bev150C1","bev150C2","bev150C3",
#         "bev300C1","bev300C2","bev300C3",
#         "bev400C1","bev400C2","bev400C3"
#       )
#     )
#   } else if (spec_type == "NL-PLUG") { 
#     nlNests <- list(
#       root = 1,
#       ICE  = lambda_ICE,
#       PLUG = lambda_PLUG
#     )
#     nlStructure <- list(
#       root = c("ICE", "PLUG"),
#       ICE  = c(
#         "cvC1", "cvC2", "cvC3",
#         "hevC1","hevC2","hevC3"
#       ),
#       PLUG = c(
#         "phev20C1","phev20C2","phev20C3",
#         "phev40C1","phev40C2","phev40C3",
#         "bev100C1","bev100C2","bev100C3",
#         "bev150C1","bev150C2","bev150C3",
#         "bev300C1","bev300C2","bev300C3",
#         "bev400C1","bev400C2","bev400C3"
#       )
#     )
#   } else if (spec_type == "NL-THREE") {
#     nlNests <- list(
#       root = 1,
#       ICE  = lambda_ICE,
#       PHEV = lambda_PHEV,
#       BEV  = lambda_BEV
#     )
#     nlStructure <- list(
#       root = c("ICE", "PHEV", "BEV"),
#       ICE  = c(
#         "cvC1", "cvC2", "cvC3",
#         "hevC1","hevC2","hevC3"
#       ),
#       PHEV = c(
#         "phev20C1","phev20C2","phev20C3",
#         "phev40C1","phev40C2","phev40C3"
#       ),
#       BEV  = c(
#         "bev100C1","bev100C2","bev100C3",
#         "bev150C1","bev150C2","bev150C3",
#         "bev300C1","bev300C2","bev300C3",
#         "bev400C1","bev400C2","bev400C3"
#       )
#     )
#   } else if (spec_type == "CNL") {
#     # Placeholder for a Cross‐Nested Logit specification.
#     # Users should define their own nlNests and nlStructure for a CNL model here.
#     warning("CNL specification must be defined manually outside get_nl_spec().")
#     nlNests     <- NULL
#     nlStructure <- NULL
#   } else {
#     stop("spec_type must be one of 'GAS_BEV', 'ICE_PLUG', 'ICE_PHEV_BEV', or 'CNL'.")
#   }
#   
#   return(list(
#     nlNests     = nlNests,
#     nlStructure = nlStructure
#   ))
# }

get_nl_spec <- function(spec_type, class_idx = NULL) {
  # Intermediate func to select proper nesting func
  
  # Check if is expanded
  is_expanded <- grepl("^expanded_", spec_type)
  
  # Call proper func
  if (!is_expanded) {
    get_nl_spec_original(spec_type, class_idx)
  } else {
    get_nl_spec_expanded(spec_type, class_idx)
  }
}

get_nl_spec_original <- function(spec_type, class_idx = NULL) {
  # spec_type <- toupper(spec_type)

  # helper to fetch either global or class-specific lambda
  fetch_lambda <- function(base, i) {
    if (is.null(i)) {
      # flat case: expect a scalar lambda_BASE in your env
      get(base)
    } else {
      # LCL case: expect lambda_BASE_i
      get(paste0(base, "_", i))
    }
  }

  ICEV_names = c("cvC1","cvC2","cvC3")
  HEV_names  = c("hevC1","hevC2","hevC3")
  PHEV_names = c("phev20C1","phev20C2","phev20C3","phev40C1","phev40C2","phev40C3")
  BEV_names  = c("bev100C1","bev100C2","bev100C3", "bev150C1","bev150C2","bev150C3",
                 "bev300C1","bev300C2","bev300C3","bev400C1","bev400C2","bev400C3")

  if (spec_type == "NL-GAS") {
    nlNests <- list(
      root = 1,
      GAS  = fetch_lambda("lambda_GAS",  class_idx),
      BEV  = fetch_lambda("lambda_BEV",  class_idx)
    )
    nlStructure <- list(
      root = c("GAS","BEV"),
      GAS  = c(ICEV_names, HEV_names, PHEV_names),
      BEV  = BEV_names
    )

  } else if (spec_type == "NL-PLUG") {
    nlNests <- list(
      root = 1,
      ICE  = fetch_lambda("lambda_ICE",  class_idx),
      PLUG = fetch_lambda("lambda_PLUG", class_idx)
    )
    nlStructure <- list(
      root = c("ICE","PLUG"),
      ICE  = c(ICEV_names, HEV_names),
      PLUG = c(PHEV_names, BEV_names)
    )

  } else if (spec_type == "NL-THREE") {
    nlNests <- list(
      root = 1,
      ICE  = fetch_lambda("lambda_ICE",  class_idx),
      PHEV = fetch_lambda("lambda_PHEV", class_idx),
      BEV  = fetch_lambda("lambda_BEV",  class_idx)
    )
    nlStructure <- list(
      root = c("ICE","PHEV","BEV"),
      ICE  = c(ICEV_names, HEV_names),
      PHEV = PHEV_names,
      BEV  = BEV_names
    )

  } else if (spec_type == "CNL") {
    # NOTE: Apollo will not estimate a model where, at the starting values, the
    # allocation parameters for a single alternative sum to a value different from 1

    # 1) Define CNL nests
    nlNests <- list(
      GAS = fetch_lambda("lambda_ICE", class_idx),
      BEV = fetch_lambda("lambda_BEV", class_idx)
    )

    # 2) Allocation parameters via logistic transform
    # alpha_HEV_ICE  <- exp(alpha_HEV_ICE_logistic)  / (1 + exp(alpha_HEV_ICE_logistic))
    alpha_PHEV_ICE <- exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)) /
      (1 + exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)))

    # 3) Build cross-nested structure matrix
    all_veh_names = c(ICEV_names, HEV_names, PHEV_names, BEV_names)
    nlStructure <- matrix(0,
                          nrow = length(nlNests),
                          ncol = length(all_veh_names),
                          dimnames = list(names(nlNests), all_veh_names))
    #   – GAS nest shares
    nlStructure["GAS", ] <- c(
      # rep(1, 3),                       # cvC1, cvC2, cvC3
      # rep(alpha_HEV_ICE, 3),           # hevC1, hevC2, hevC3
      # Ignoring HEVs
      rep(1, 6),                       # cvC1, cvC2, cvC3, hevC1, hevC2, hevC3
      rep(alpha_PHEV_ICE, 6),          # phev20C1…phev40C3
      rep(0, 12)                       # BEV alts
    )

    #   – BEV nest shares
    nlStructure["BEV", ] <- c(
      # rep(0, 3),
      # rep(1 - alpha_HEV_ICE, 3),
      # Ignoring HEVs
      rep(0, 6),
      rep(1 - alpha_PHEV_ICE, 6),
      rep(1, 12)
    )
  } else {
    stop("`spec_type` must be one of NL-GAS, NL-PLUG, NL-THREE, or CNL.")
  }
  list(nlNests = nlNests, nlStructure = nlStructure)
}

get_nl_spec_expanded <- function(spec_type, class_idx = NULL) {
  # Remove "expanded_" prefix if present
  spec_type <- sub("^expanded_", "", spec_type)
  
  # Helper to fetch either global or class-specific lambda
  fetch_lambda <- function(base, i) {
    if (is.null(i)) {
      get(base)
    } else {
      get(paste0(base, "_", i))
    }
  }
  
  # Define vehicle names - ORIGINAL 24 stay the same
  ICEV_names_original = c("cvC1","cvC2","cvC3")
  HEV_names_original  = c("hevC1","hevC2","hevC3")
  PHEV_names_original = c("phev20C1","phev20C2","phev20C3","phev40C1","phev40C2","phev40C3")
  BEV_names_original  = c("bev100C1","bev100C2","bev100C3", "bev150C1","bev150C2","bev150C3",
                          "bev300C1","bev300C2","bev300C3","bev400C1","bev400C2","bev400C3")
  
  # Add C4 alternatives at the END
  ICEV_names_C4 = "cvC4"
  HEV_names_C4  = "hevC4"
  PHEV_names_C4 = c("phev20C4","phev40C4")
  BEV_names_C4  = c("bev100C4","bev150C4","bev300C4","bev400C4")
  
  # Full names in the correct expanded order
  ICEV_names = c(ICEV_names_original, ICEV_names_C4)
  HEV_names  = c(HEV_names_original, HEV_names_C4)
  PHEV_names = c(PHEV_names_original, PHEV_names_C4)
  BEV_names  = c(BEV_names_original, BEV_names_C4)
  
  if (spec_type == "NL-GAS") {
    nlNests <- list(
      root = 1,
      GAS  = fetch_lambda("lambda_GAS",  class_idx),
      BEV  = fetch_lambda("lambda_BEV",  class_idx)
    )
    nlStructure <- list(
      root = c("GAS","BEV"),
      GAS  = c(ICEV_names, HEV_names, PHEV_names),
      BEV  = BEV_names
    )
  } else if (spec_type == "NL-PLUG") {
    nlNests <- list(
      root = 1,
      ICE  = fetch_lambda("lambda_ICE",  class_idx),
      PLUG = fetch_lambda("lambda_PLUG", class_idx)
    )
    nlStructure <- list(
      root = c("ICE","PLUG"),
      ICE  = c(ICEV_names, HEV_names),
      PLUG = c(PHEV_names, BEV_names)
    )
  } else if (spec_type == "NL-THREE") {
    nlNests <- list(
      root = 1,
      ICE  = fetch_lambda("lambda_ICE",  class_idx),
      PHEV = fetch_lambda("lambda_PHEV", class_idx),
      BEV  = fetch_lambda("lambda_BEV",  class_idx)
    )
    nlStructure <- list(
      root = c("ICE","PHEV","BEV"),
      ICE  = c(ICEV_names, HEV_names),
      PHEV = PHEV_names,
      BEV  = BEV_names
    )
  } else if (spec_type == "CNL") {
    # 1) Define CNL nests
    nlNests <- list(
      GAS = fetch_lambda("lambda_ICE", class_idx),
      BEV = fetch_lambda("lambda_BEV", class_idx)
    )
    
    # 2) Allocation parameters via logistic transform
    alpha_PHEV_ICE <- exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)) /
      (1 + exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)))
    
    # 3) Build cross-nested structure matrix
    # all_veh_names = c(ICEV_names, HEV_names, PHEV_names, BEV_names)
    all_veh_names = c(
      ICEV_names_original, HEV_names_original, PHEV_names_original, BEV_names_original,  # Original 24
      ICEV_names_C4, HEV_names_C4, PHEV_names_C4, BEV_names_C4                           # C4 additions
    )
    nlStructure <- matrix(0,
                          nrow = length(nlNests),
                          ncol = length(all_veh_names),
                          dimnames = list(names(nlNests), all_veh_names))
    
    # GAS nest shares
    # Original 24: 6 ICEV/HEV (all 1) + 6 PHEV (alpha) + 12 BEV (0)
    # Plus 8 C4: 2 ICEV/HEV (all 1) + 2 PHEV (alpha) + 4 BEV (0)
    nlStructure["GAS", ] <- c(
      rep(1, 6),                       # cvC1-C3, hevC1-C3
      rep(alpha_PHEV_ICE, 6),          # phev20C1-C3, phev40C1-C3
      rep(0, 12),                      # bev100C1-C3, bev150C1-C3, bev300C1-C3, bev400C1-C3
      rep(1, 2),                       # cvC4, hevC4
      rep(alpha_PHEV_ICE, 2),          # phev20C4, phev40C4
      rep(0, 4)                        # bev100C4, bev150C4, bev300C4, bev400C4
    )
    
    # BEV nest shares
    nlStructure["BEV", ] <- c(
      rep(0, 6),                       # cvC1-C3, hevC1-C3
      rep(1 - alpha_PHEV_ICE, 6),      # phev20C1-C3, phev40C1-C3
      rep(1, 12),                      # bev100C1-C3, bev150C1-C3, bev300C1-C3, bev400C1-C3
      rep(0, 2),                       # cvC4, hevC4
      rep(1 - alpha_PHEV_ICE, 2),      # phev20C4, phev40C4
      rep(1, 4)                        # bev100C4, bev150C4, bev300C4, bev400C4
    )
  } else {
    stop("`spec_type` must be one of NL-GAS, NL-PLUG, NL-THREE, or CNL.")
  }
  list(nlNests = nlNests, nlStructure = nlStructure)
}

## Attempted modified version
# get_nl_spec <- function(spec_type, class_idx = NULL, expanded = FALSE) {
#   
#   fetch_lambda <- function(base, i) {
#     if (is.null(i)) {
#       get(base)
#     } else {
#       get(paste0(base, "_", i))
#     }
#   }
#   
#   # Define base names
#   if (expanded) {
#     ICEV_names = c("cvC1","cvC2","cvC3","cvC4")
#     HEV_names  = c("hevC1","hevC2","hevC3","hevC4")
#     PHEV_names = c("phev20C1","phev20C2","phev20C3","phev40C1","phev40C2","phev40C3",
#                    "phev20C4","phev40C4")
#     BEV_names  = c("bev100C1","bev100C2","bev100C3","bev150C1","bev150C2","bev150C3",
#                    "bev300C1","bev300C2","bev300C3","bev400C1","bev400C2","bev400C3",
#                    "bev100C4","bev150C4","bev300C4","bev400C4")
#   } else {
#     ICEV_names = c("cvC1","cvC2","cvC3")
#     HEV_names  = c("hevC1","hevC2","hevC3")
#     PHEV_names = c("phev20C1","phev20C2","phev20C3","phev40C1","phev40C2","phev40C3")
#     BEV_names  = c("bev100C1","bev100C2","bev100C3","bev150C1","bev150C2","bev150C3",
#                    "bev300C1","bev300C2","bev300C3","bev400C1","bev400C2","bev400C3")
#   }
#   
#   if (spec_type == "NL-GAS") {
#     nlNests <- list(
#       root = 1,
#       GAS  = fetch_lambda("lambda_GAS",  class_idx),
#       BEV  = fetch_lambda("lambda_BEV",  class_idx)
#     )
#     nlStructure <- list(
#       root = c("GAS","BEV"),
#       GAS  = c(ICEV_names, HEV_names, PHEV_names),
#       BEV  = BEV_names
#     )
#     
#   } else if (spec_type == "NL-PLUG") {
#     nlNests <- list(
#       root = 1,
#       ICE  = fetch_lambda("lambda_ICE",  class_idx),
#       PLUG = fetch_lambda("lambda_PLUG", class_idx)
#     )
#     nlStructure <- list(
#       root = c("ICE","PLUG"),
#       ICE  = c(ICEV_names, HEV_names),
#       PLUG = c(PHEV_names, BEV_names)
#     )
#     
#   } else if (spec_type == "NL-THREE") {
#     nlNests <- list(
#       root = 1,
#       ICE  = fetch_lambda("lambda_ICE",  class_idx),
#       PHEV = fetch_lambda("lambda_PHEV", class_idx),
#       BEV  = fetch_lambda("lambda_BEV",  class_idx)
#     )
#     nlStructure <- list(
#       root = c("ICE","PHEV","BEV"),
#       ICE  = c(ICEV_names, HEV_names),
#       PHEV = PHEV_names,
#       BEV  = BEV_names
#     )
#     
#   } else if (spec_type == "CNL") {
#     nlNests <- list(
#       GAS = fetch_lambda("lambda_ICE", class_idx),
#       BEV = fetch_lambda("lambda_BEV", class_idx)
#     )
#     
#     alpha_PHEV_ICE <- exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)) / 
#       (1 + exp(fetch_lambda("alpha_PHEV_ICE_logistic", class_idx)))
#     
#     all_veh_names = c(ICEV_names, HEV_names, PHEV_names, BEV_names)
#     nlStructure <- matrix(0,
#                           nrow = length(nlNests),
#                           ncol = length(all_veh_names),
#                           dimnames = list(names(nlNests), all_veh_names))
#     
#     # Adjust counts based on expanded
#     n_cv_hev = if(expanded) 8 else 6  # cv + hev alternatives
#     n_phev = if(expanded) 8 else 6
#     n_bev = if(expanded) 16 else 12
#     
#     nlStructure["GAS", ] <- c(
#       rep(1, n_cv_hev),
#       rep(alpha_PHEV_ICE, n_phev),
#       rep(0, n_bev)
#     )
#     
#     nlStructure["BEV", ] <- c(
#       rep(0, n_cv_hev),
#       rep(1 - alpha_PHEV_ICE, n_phev),
#       rep(1, n_bev)
#     )
#   } else {
#     stop("`spec_type` must be one of NL-GAS, NL-PLUG, NL-THREE, or CNL.")
#   }
#   
#   list(nlNests = nlNests, nlStructure = nlStructure)
# }

nl_probability_function <- function(apollo_beta, apollo_inputs){
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    # Get utility & availability
    V <- list()
    A <- list()
    
    ### Create list of probabilities P
    P = list()
    
    # Define equation based on utility space
    prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
    # Loop over alternatives
    for (altName in apollo_inputs$availCols) {
      alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
      # Get availability
      A[[altName]] <- get(altName)
      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
        # Build the string: scalingFactor*(wtp_OpCost*OpCost<suffix> + … – Price<suffix>)
        v_expr <- str_glue("
        scalingFactor * (
          {paste0(
             \"wtp_\", attrs_no_price,
             \" * \", attrs_no_price, alt_suffix,
             collapse = \" + \"
          )}
          - Price{alt_suffix}
        )")
      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        v_expr <- str_glue("{paste0(
             \"b_\", apollo_inputs$observed_attributes,
             \" * \", apollo_inputs$observed_attributes, alt_suffix,
             collapse = \" + \"
          )}")
      }
      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }
    
    # Get nesting specification
    res <- get_nl_spec(apollo_inputs$nesting_spec)
    nlNests <- res$nlNests
    nlStructure <- res$nlStructure
    if (apollo_inputs$nesting_spec == "CNL" || apollo_inputs$nesting_spec == "expanded_CNL"){
      # Cross nested logit
      # Define settings for CNL model
      cnl_settings <- list(
        alternatives = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A,  # Availability conditions
        cnlNests     = nlNests,
        cnlStructure = nlStructure
      )
      
      ### Compute probabilities using MNL model
      P[["model"]] = apollo_cnl(cnl_settings, functionality)
    } else{
      # Standard nested logit
      ### Define settings for NL model
      nl_settings <- list(
        alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A,  # Availability conditions
        nlNests      = nlNests,
        nlStructure  = nlStructure
      )
      ### Compute probabilities using NL model
      P[["model"]] = apollo_nl(nl_settings, functionality)
    }
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    
    return(P)
  }
}

lcnl_lcPars_function <- function(){
  apollo_lcPars <- function(apollo_beta, apollo_inputs) {

    # Specify vars
    n <- apollo_inputs$n_classes
    all_attrs <- apollo_inputs$observed_attributes
    uspace    <- tolower(apollo_inputs$utility_space)
    prefix    <- if (uspace == "pref") "b_" else "wtp_"

    # Determine which attributes to loop over
    attrs <- if (uspace == "wtp") {
      # In WTP space, drop Price, add scalingFactor
      c("scalingFactor", setdiff(all_attrs, "Price"))
    } else {
      all_attrs
    }

    lcpars <- list()

    # Taste parameters: one list per attribute, entries by class
    for (attr in attrs) {
      param_base <- if (attr == "scalingFactor") "scalingFactor" else paste0(prefix, attr)
      tmp <- vector("list", n)
      for (i in seq_len(n)) {
        tmp[[i]] <- apollo_beta[[ paste0(param_base, "_", i) ]]
      }
      names(tmp) <- paste0("class_", seq_len(n))
      lcpars[[ param_base ]] <- tmp
    }

    # Nesting params
    for (l in apollo_inputs$nesting_params$names){
      tmp <- vector("list", n)
      for (k in seq_len(n)) {
        tmp[[k]] <- apollo_beta[[ paste0(l, "_", k) ]]
      }
      names(tmp) <- paste0("class_", seq_len(n))
      lcpars[[ l ]] <- tmp
    }

    # Class intercepts: same pattern, but use “j” as the index
    intercepts <- vector("list", length = n)
    for (j in seq_len(n)) {
      intercepts[[j]] <- apollo_beta[[ paste0("class_intercept_", j) ]]
    }
    names(intercepts) <- paste0("class_", seq_len(n))
    lcpars[["class_intercept"]] <- intercepts

    # Build pi_values
    utilities <- intercepts
    classes   <- setNames(seq_len(n), names(intercepts))
    lcpars[["pi_values"]] <- apollo_classAlloc(
      list(classes = classes, utilities = utilities)
    )

    return(lcpars)
  }
  return(apollo_lcPars)
}

lcnl_probability_function <- function(){
  # Define probabilities function for LCL with n classes
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
    
    # Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    # Create list of probabilities P
    P = list()
    
    # Define utilities dynamically based on availCols
    V = list()
    A = list()
    
    # Define availability condition for each alternative
    for (a in apollo_inputs$availCols){
      A[[a]] = get(a)
    }
    
    ### Loop over classes
    for (s in 1:apollo_inputs$n_classes) {
      
      # Loop over alts
      for (altName in apollo_inputs$availCols){
        # Extract last two characters (e.g., "C1", "C2", "C3") for correct attribute lookup
        alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
        
        # prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
        
        if (tolower(apollo_inputs$utility_space) == "wtp") {
          # Drop "Price" from observed_attributes for the wtp_ part
          attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
          # Build the string: scalingFactor_#*(wtp_OpCost_#*OpCost<suffix> + … – Price<suffix>)
          v_expr <- str_glue("
          scalingFactor_{s} * (
            {paste0(
               \"wtp_\", attrs_no_price, \"_\", s,
               \" * \", attrs_no_price, alt_suffix,
               collapse = \" + \"
            )}
            - Price{alt_suffix}
          )")
        } else {
          # PREF space: sum(b_attr * attr<alt_suffix>)
          v_expr <- str_glue("{paste0(
            \"b_\", apollo_inputs$observed_attributes, \"_\", s,
            \" * \", apollo_inputs$observed_attributes, alt_suffix,
            collapse = \" + \"
          )}"
          )
        }
        
        V[[altName]] <- eval(parse(text = v_expr))
      }
      
      # Get nesting specification
      res <- get_nl_spec(apollo_inputs$nesting_spec,s)
      nlNests <- res$nlNests
      nlStructure <- res$nlStructure

      if (apollo_inputs$nesting_spec == "CNL" || apollo_inputs$nesting_spec == "expanded_CNL"){
        # Cross nested logit
        # Define settings for CNL model
        cnl_settings <- list(
          alternatives = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),
          choiceVar     = AltChoice,  # Choice variable column in data
          utilities     = V,  # Utilities for each alternative
          avail         = A,  # Availability conditions
          cnlNests     = nlNests,
          cnlStructure = nlStructure
        )
        
        ### Compute probabilities using MNL model
        P[[paste0("Class_", s)]] = apollo_cnl(cnl_settings, functionality)
      } else{
        # Standard nested logit
        ### Define settings for NL model
        nl_settings <- list(
          alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
          choiceVar     = AltChoice,  # Choice variable column in data
          utilities     = V,  # Utilities for each alternative
          avail         = A,  # Availability conditions
          nlNests      = nlNests,
          nlStructure  = nlStructure
        )
        
        ### Compute probabilities using NL model
        P[[paste0("Class_", s)]] = apollo_nl(nl_settings, functionality)
      }
      
      # Take product across observation for same individual
      P[[paste0("Class_", s)]] = apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
    }
    
    
    ### Compute latent class model probabilities (combining all classes)
    lc_settings = list(
      inClassProb = P,       # Probabilities from each class
      classProb   = pi_values # Class probabilities (e.g., `delta_1`, `delta_2`)
    )
    
    # Calculate the overall model probabilities using latent class model
    P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
    
    # Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}

mxnl_probability_function <- function(){
  # Define probabilities function
  apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate") {
    
    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    ### Create list of probabilities P
    P = list()
    
    ### Define utilities dynamically based on availCols
    V = list()
    A = list()
    
    # Define equation based on utility space
    prefix <- if (tolower(apollo_inputs$utility_space) == "pref") "b_" else "wtp_"
    # Loop over alternatives
    for (altName in apollo_inputs$availCols) {
      alt_suffix <- substr(altName, nchar(altName) - 1, nchar(altName))
      # Get availability
      A[[altName]] <- get(altName)
      # Conditional on utility space
      if (tolower(apollo_inputs$utility_space) == "wtp") {
        # Drop "Price" from observed_attributes for the wtp_ part
        attrs_no_price <- setdiff(apollo_inputs$observed_attributes, "Price")
        # Build the string: scalingFactor*(wtp_OpCost*OpCost<suffix> + … – Price<suffix>)
        v_expr <- str_glue("
        scalingFactor * (
          {paste0(
             \"wtp_\", attrs_no_price,
             \" * \", attrs_no_price, alt_suffix,
             collapse = \" + \"
          )}
          - Price{alt_suffix}
        )")
      } else {
        # PREF space: sum(b_attr * attr<suffix>)
        v_expr <- str_glue("{paste0(
             \"b_\", apollo_inputs$observed_attributes,
             \" * \", apollo_inputs$observed_attributes, alt_suffix,
             collapse = \" + \"
          )}")
      }
      # Expression for utility
      V[[altName]] <- eval(parse(text = v_expr))
    }
    
    # Get nesting specification
    res <- get_nl_spec(apollo_inputs$nesting_spec)
    nlNests <- res$nlNests
    nlStructure <- res$nlStructure
    
    if (apollo_inputs$nesting_spec == "CNL" || apollo_inputs$nesting_spec == "expanded_CNL"){
      # Cross nested logit
      # Define settings for CNL model
      cnl_settings <- list(
        alternatives = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A,  # Availability conditions
        cnlNests     = nlNests,
        cnlStructure = nlStructure
      )
      
      ### Compute probabilities using MNL model
      P[["model"]] = apollo_cnl(cnl_settings, functionality)
    } else{
      # Standard nested logit
      ### Define settings for NL model
      nl_settings <- list(
        alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A,  # Availability conditions
        nlNests      = nlNests,
        nlStructure  = nlStructure
      )
      ### Compute probabilities using NL model
      P[["model"]] = apollo_nl(nl_settings, functionality)
    }
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Average across inter-individual draws
    P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    
    return(P)
  }
  return(apollo_probabilities)
}

prepare_apollo_model <- function(model_settings, availCols, vars_to_clear=NULL) {
  # Print header
  model_name <- model_settings$name
  cat("\n\n=== Running", model_name, "===\n")
  
  # 1) Load inputs
  model_inputs <- load_apollo_inputs(
    logit_type           = model_name,
    heterogeneity_spec   = model_settings$heterogeneity_spec,
    nesting_spec         = model_settings$nesting_spec,
    n_classes            = model_settings$n_classes,
    utility_space        = model_settings$utility_space,
    lognorm_price_dist   = model_settings$lognorm_price_dist,
    observed_attributes  = model_settings$observed_attributes,
    database             = model_settings$database,
    output_directory     = file.path(model_settings$output_directory, model_name),
    n_cores              = 1
  )
  
  apollo_control  <- model_inputs$apollo_control
  apollo_beta     <- model_inputs$apollo_beta
  apollo_fixed    <- model_inputs$apollo_fixed
  nesting_params  <- model_inputs$nesting_params
  constraints     <- model_inputs$constraints
  # Assign to the global workspace (requirement for apollo_validateInputs)
  assign("apollo_control",  model_inputs$apollo_control,  envir = .GlobalEnv)
  assign("apollo_beta",     model_inputs$apollo_beta,     envir = .GlobalEnv)
  assign("apollo_fixed",    model_inputs$apollo_fixed,    envir = .GlobalEnv)
  assign("nesting_params",  model_inputs$nesting_params,  envir = .GlobalEnv)
  assign("constraints",     model_inputs$constraints,     envir = .GlobalEnv)
  
  # 2) Build apollo_inputs list
  apollo_inputs <<- list(
    n_classes           = model_settings$n_classes,
    n_draws             = model_settings$n_draws,
    availCols           = availCols,
    observed_attributes = model_settings$observed_attributes,
    heterogeneity_spec  = model_settings$heterogeneity_spec,
    utility_space       = model_settings$utility_space,
    nesting_spec        = model_settings$nesting_spec,
    nesting_params      = nesting_params
  )
  
  # 3) Load model-specific probability function
  loaded_model         <- load_apollo_model(apollo_inputs, apollo_beta)
  apollo_inputs        <- loaded_model$apollo_inputs
  apollo_probabilities <- loaded_model$apollo_probabilities
  
  # 5) Return prepared model
  return(list(
    model_name            = model_name,
    apollo_beta           = apollo_beta,
    apollo_fixed          = apollo_fixed,
    apollo_probabilities  = apollo_probabilities,
    apollo_inputs         = apollo_inputs,
    apollo_control        = apollo_control,
    constraints           = constraints
  ))
}


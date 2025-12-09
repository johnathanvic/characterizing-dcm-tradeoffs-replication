
mnl_probability_function <- function(apollo_beta, apollo_inputs){
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
    
    if (tolower(apollo_inputs$nesting_spec) == "none"){
      # No Nesting
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
    } else{
      # With Nesting
      # Get nesting specification
      res <- get_nl_spec(apollo_inputs$nesting_spec)  # currently relying on pulling from env
      nlNests <- res$nlNests
      nlStructure <- res$nlStructure
      
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
    P <- apollo_weighting(P, apollo_inputs, functionality)
    P <- apollo_panelProd(P, apollo_inputs, functionality)
    P <- apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  return(apollo_probabilities)
}

lcl_lcPars_function <- function(){
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
    
    if (!tolower(apollo_inputs$nesting_spec) == "none"){
      # With Nesting
      # Nesting params
      for (l in apollo_inputs$nesting_params$names){
        tmp <- vector("list", n)
        for (k in seq_len(n)) {
          tmp[[k]] <- apollo_beta[[ paste0(l, "_", k) ]]
        }
        names(tmp) <- paste0("class_", seq_len(n))
        lcpars[[ l ]] <- tmp
      }
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

lcl_probability_function <- function(){
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
      
      if (tolower(apollo_inputs$nesting_spec) == "none"){
        # No Nesting
        # Define settings for MNL model component
        mnl_settings = list(
          alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
          choiceVar     = AltChoice,  # Choice variable column in data
          utilities     = V,  # Utilities for each alternative
          avail         = A  # Availability conditions
        )
        
        # Compute within-class choice probabilities using MNL model
        P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)
      } else{
        # With Nesting
        # Get nesting specification
        res <- get_nl_spec(apollo_inputs$nesting_spec,s)  # currently relying on pulling from env
        nlNests <- res$nlNests
        nlStructure <- res$nlStructure
        # print(nlNests)
        # print(nlStructure)
        
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

mxl_probability_function <- function(){
  
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
    
    if (tolower(apollo_inputs$nesting_spec) == "none"){
      # No Nesting
      ### Define settings for MNL model component
      mnl_settings = list(
        alternatives  = setNames(1:length(apollo_inputs$availCols), apollo_inputs$availCols),  # Assign numbers dynamically
        choiceVar     = AltChoice,  # Choice variable column in data
        utilities     = V,  # Utilities for each alternative
        avail         = A  # Availability conditions
      )
      
      ### Compute probabilities using MNL model
      P[["model"]] = apollo_mnl(mnl_settings, functionality)
    } else{
      # Nesting
      # Get nesting specification
      res <- get_nl_spec(apollo_inputs$nesting_spec)  # currently relying on pulling from env
      nlNests <- res$nlNests
      nlStructure <- res$nlStructure
      
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
    
    ### Weight individuals
    P <- apollo_weighting(P, apollo_inputs, functionality)
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Average across inter-individual draws
    P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    
    return(P)
  }
  return(apollo_probabilities)
}
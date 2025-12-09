#' Modified version of Apollo's searchStart procedure to:
#' (1) save outcome of each starting point
#' (2) customize the randomization procedure
#' (3) parallelize multistart search
#' 
#' Original code: https://rdrr.io/cran/apollo/src/R/apollo_searchStart.R
#' 
#' 
#' Searches for better starting values.
#'
#' Given a set of starting values and a range for them, searches for points with a better likelihood and steeper gradients.
#'
#' This function implements a simplified version of the algorithm proposed by Bierlaire, M., Themans, M. & Zufferey, N. (2010), A Heuristic for Nonlinear Global Optimization, INFORMS Journal on Computing, 22(1), pp.59-70. The main difference
#' lies in it implementing only two out of three tests on the candidates described by the authors. The implemented algorithm has the
#' following steps.
#' \enumerate{
#'   \item Randomly draw \code{nCandidates} candidates from an interval given by the user.
#'   \item Label all candidates with a valid log-likelihood (LL) as active.
#'   \item Apply \code{bfgsIter} iterations of the BFGS algorithm to each active candidate.
#'   \item Apply the following tests to each active candidate:
#'   \enumerate{
#'     \item Has the BGFS search converged?
#'     \item Are the candidate parameters after BFGS closer than \code{dTest} from any other candidate with higher LL?
#'     \item Is the LL of the candidate after BFGS further than \code{distLL} from a candidate with better LL, and its gradient smaller than \code{gTest}?
#'   }
#'   \item Mark any candidates for which at least one test results in yes as inactive.
#'   \item Go back to step 3, unless only one candidate is active, or the maximum number of iterations (\code{maxStages}) has been reached.
#' }
#' This function will write a CSV file to the working/output directory summarising progress. This file is called \code{modelName}_searchStart.csv .
#' @param apollo_beta Named numeric vector. Names and values for parameters.
#' @param apollo_fixed Character vector. Names (as defined in \code{apollo_beta}) of parameters whose value should not change during estimation.
#' @param apollo_probabilities Function. Returns probabilities of the model to be estimated. Must receive three arguments:
#'                          \itemize{
#'                            \item \strong{\code{apollo_beta}}: Named numeric vector. Names and values of model parameters.
#'                            \item \strong{\code{apollo_inputs}}: List containing options of the model. See \link{apollo_validateInputs}.
#'                            \item \strong{\code{functionality}}: Character. Can be either \strong{\code{"components"}}, \strong{\code{"conditionals"}}, \strong{\code{"estimate"}} (default), \strong{\code{"gradient"}}, \strong{\code{"output"}}, \strong{\code{"prediction"}}, \strong{\code{"preprocess"}}, \strong{\code{"raw"}}, \strong{\code{"report"}}, \strong{\code{"shares_LL"}}, \strong{\code{"validate"}} or \strong{\code{"zero_LL"}}.
#'                          }
#' @param apollo_inputs List grouping most common inputs. Created by function \link{apollo_validateInputs}.
#' @param searchStart_settings List. Contains settings for this function. User input is required for all settings except those with a default or marked as optional. 
#'                                   \itemize{
#'                                     \item \strong{\code{apolloBetaMax}}: Vector. Maximum possible value of parameters when generating candidates. Ignored if smartStart is TRUE. Default is \code{apollo_beta + 0.1}.
#'                                     \item \strong{\code{apolloBetaMin}}: Vector. Minimum possible value of parameters when generating candidates. Ignored if smartStart is TRUE. Default is \code{apollo_beta - 0.1}.
#'                                     \item \strong{\code{bfgsIter}}: Numeric scalar. Number od BFGS iterations to perform at each stage to each remaining candidate. Default is 20.
#'                                     \item \strong{\code{dTest}}: Numeric scalar. Tolerance for test 1. A candidate is discarded if its distance in parameter space to a better one is smaller than \code{dTest}. Default is 1.
#'                                     \item \strong{\code{gTest}}: Numeric scalar. Tolerance for test 2. A candidate is discarded if the norm of its gradient is smaller than \code{gTest} AND its LL is further than \code{llTest} from a better candidate. Default is \code{10^(-3)}.
#'                                     \item \strong{\code{llTest}}: Numeric scalar. Tolerance for test 2. A candidate is discarded if the norm of its gradient is smaller than \code{gTest} AND its LL is further than \code{llTest} from a better candidate. Default is 3.
#'                                     \item \strong{\code{maxStages}}: Numeric scalar. Maximum number of search stages. The algorithm will stop when there is only one candidate left, or if it reaches this number of stages. Default is 5.
#'                                     \item \strong{\code{nCandidates}}: Numeric scalar. Number of candidate sets of parameters to be used at the start. Should be an integer bigger than 1. Default is 100.
#'                                     \item \strong{\code{smartStart}}: Boolean. If TRUE, candidates are randomly generated with more chances in the directions the Hessian indicates improvement of the LL function. Default is FALSE.
#'                                   }
#' @return named vector of model parameters. These are the best values found.
#' @export
#' @importFrom numDeriv hessian
#' @importFrom maxLik maxLik

######### START MODIFICATION #########

# ────────────────────────────────────────────────────────────────────────────
# EXIT FLAGS (stop_ef) MAPPING
#
# "removed_nonfinite_LL"          -> stop_ef = -4
# "init"                          -> stop_ef = -2   (before any tests)
# "active"                        -> stop_ef = -1   (still active at end of stage)
#
# "pruned_test1_too_close"        -> stop_ef =  0
# "pruned_test0_duplicate_best"   -> stop_ef =  4
# "pruned_test0_worse_conv"       -> stop_ef =  3
# "pruned_test2_worse"            -> stop_ef =  3
# "pruned_test3_nonFinite_grad    -> stop_ef =  5   # added as practical measure, will cause code to fail
#
# "best"                          -> stop_ef =  1
# "best_no_conv"                  -> stop_ef =  2  (best & only one left, but not yet conv)

# PRUNING
# pruned_optima_row tracks which row it was pruned for, & that row determines the fate

# ────────────────────────────────────────────────────────────────────────────



sample_logit_random <- function(n_samples, beta_var_val, apollo_beta) {
  K <- length(beta_var_val)
  mat <- matrix(0, n_samples, K)
  
  for (i in seq_along(names(beta_var_val))) {
    param_name <- names(beta_var_val)[i]
    if (startsWith(param_name, "mu_")) {
      # Sample from mu=N(beta, beta) & sigma=N(0, beta)
      mnl_coeff <- apollo_beta[[param_name]]
      # OG
      mu_sd      <- abs(mnl_coeff)
      sigma_sd   <- abs(mnl_coeff)
      
      sampled_mu <- mnl_coeff + rnorm(n=n_samples, mean=0, sd=mu_sd)
      sampled_sigma <- rnorm(n=n_samples, mean=0, sd=sigma_sd)          # ok if negative

      mat[, i] <- sampled_mu
      mat[, i + 1] <- sampled_sigma
      
    } else if (startsWith(param_name, "sigma_")) {
      next
      
    } else if (startsWith(param_name, "lambda_")) {
      mat[, i] <- runif(n_samples, min=0.01, max=0.99)
      
    } else if (startsWith(param_name, "class_") || startsWith(param_name, "alpha_")) {
      mat[, i] <- rnorm(n_samples, mean=0, sd=0.5)
    #   
    # } else if (param_name == "scalingFactor") {
    #   mnl_coeff <- apollo_beta[[param_name]]
    #   sdlog <- abs(mnl_coeff)
    #   mat[, i] <- rlnorm(n_samples, meanlog=log(abs(mnl_coeff)+1e-6), sdlog=sdlog)
    #   
    } else if (startsWith(param_name, "b_") || startsWith(param_name, "wtp_") || param_name == "scalingFactor") {
      mnl_coeff <- apollo_beta[[param_name]]
      sd <- abs(mnl_coeff)
      mat[, i] <- mnl_coeff + rnorm(n_samples, mean=0, sd=sd)
      
    # } else {
      # stop(paste("Unhandled parameter:", param_name))
    }
  }
  
  return(mat)
}

sample_random <- function(n_samples, beta_var_val) {
  K <- length(beta_var_val)
  mat <- matrix(0, n_samples, K)
  
  for (i in seq_along(names(beta_var_val))) {
    param_name <- names(beta_var_val)[i]
    if (startsWith(param_name, "mu_")) {
      # Sample from N(0, 0.5)
      sampled_mu <- rnorm(n_samples, mean=0, sd=0.5)
      sampled_sigma <- rnorm(n_samples, mean=0, sd=0.5)
      # Draw sigma from log-normal distribution
      # target_mean <- 0.1
      # sdlog <- 0.5
      # meanlog <- log(target_mean) - (sdlog^2)/2
      # sampled_sigma <- rlnorm(n_samples, meanlog=meanlog, sdlog=sdlog)
      # Draw sigma from half-normal distribution 
      # sampled_sigma <- abs(rnorm(n_samples, mean=0, sd=0.5))
      
      # # Sample sigma from LN(0, 1)
      # sampled_sigma <- rlnorm(n = n_samples, meanlog = 0, sdlog = 1)
      
      mat[, i] <- sampled_mu
      mat[, i + 1] <- sampled_sigma
      
    } else if (startsWith(param_name, "sigma_")) {
      next
      
    } else if (startsWith(param_name, "lambda_")) {
      mat[, i] <- runif(n_samples, min=0.01, max=0.99)
      
    } else if (startsWith(param_name, "class_") || startsWith(param_name, "alpha_")) {
      mat[, i] <- rnorm(n_samples, mean=0, sd=0.5)
      
    # } else if (param_name == "scalingFactor") {
    #   target_mean <- 0.1
    #   sdlog <- 0.5
    #   meanlog <- log(target_mean) - (sdlog^2)/2
    #   mat[, i] <- rlnorm(n_samples, meanlog=meanlog, sdlog=sdlog)
      
    } else if (startsWith(param_name, "b_") || startsWith(param_name, "wtp_") || param_name == "scalingFactor") {
      mat[, i] <- rnorm(n_samples, mean=0, sd=0.5)
      
    # } else {
      # stop(paste("Unhandled parameter:", param_name))
    }
  }
  
  return(mat)
}
######### START MODIFICATION #########


custom_apollo_searchStart <- function(apollo_beta, apollo_fixed, 
                                      apollo_probabilities, apollo_inputs,
                                      searchStart_settings=NA, parallel=FALSE, 
                                      parallel_settings = list(n_cores = 1, mem_per_core = NA, cluster = NULL)){

  # # # # # # # # # # #
  #### Initialise  ####
  # # # # # # # # # # #

  ### Load defaults
  default <- list(nCandidates=100, apolloBetaMin=apollo_beta - 0.1, apolloBetaMax=apollo_beta + 0.1,
                  smartStart=FALSE, maxStages=5, dTest=0.1, gTest=10^-3, llTest=1, bfgsIter=20, 
                  solverSettings = NULL, constraints = NULL) # Added solver settings 10/22/25
  if(length(searchStart_settings)==1 && is.na(searchStart_settings)) searchStart_settings <- default
  tmp <- names(default)[!(names(default) %in% names(searchStart_settings))] # options missing in searchStart_settings
  for(i in tmp) searchStart_settings[[i]] <- default[[i]]

  ### Extract setings
  nCandidates   = searchStart_settings[["nCandidates"]]
  apolloBetaMin = searchStart_settings[["apolloBetaMin"]]
  apolloBetaMax = searchStart_settings[["apolloBetaMax"]]
  smartStart    = searchStart_settings[["smartStart"]]
  maxStages     = searchStart_settings[["maxStages"]]
  dTest         = searchStart_settings[["dTest"]]
  gTest         = searchStart_settings[["gTest"]]
  llTest        = searchStart_settings[["llTest"]]
  bfgsIter      = searchStart_settings[["bfgsIter"]]
  # Added solver settings 10/22/25
  solverSettings = searchStart_settings[["solverSettings"]]
  constraints    = searchStart_settings[["constraints"]]
  if (!is.null(solverSettings)) {
    solverSettings$printLevel <- NULL  # Remove printLevel
    solverSettings$iterlim <- NULL     # Remove iterlim
    # solverSettings$reltol <- sqrt(.Machine$double.eps) # Restore default reltol 10/24/25
  }
  if (!is.null(constraints)){
    # Convert string constraints to matrix format
    if(is.vector(constraints) && is.character(constraints)){
      nCon <- length(constraints)
      bVar <- names(apollo_beta)[!(names(apollo_beta) %in% apollo_fixed)]
      nVar <- length(bVar)
      bVar <- list2env(setNames(split(diag(nVar), rep(1:nVar,each=nVar)), bVar))
      bVal <- list2env(as.list(apollo_beta))
      A <- matrix(0, nrow=nCon, ncol=nVar, dimnames=list(NULL, names(apollo_beta)[!(names(apollo_beta) %in% apollo_fixed)]))
      b <- rep(0, nCon)
      mid0 <- ''
      
      for(i in 1:nCon){
        # Parse constraint
        e <- tryCatch(str2lang(constraints[i]), error=function(e) NULL)
        if(is.null(e) || !is.call(e) || length(e)!=3) stop('Invalid constraint: ', constraints[i])
        
        mid <- e[[1]]; lef <- e[[2]]; rig <- e[[3]]
        
        # Check operator
        if(!is.symbol(mid) || !(as.character(mid) %in% c(">", "=", "<"))) {
          stop('Constraint must contain >, <, or =: ', constraints[i])
        }
        mid0 <- as.character(mid)
        
        # Fill A & b
        A[i,] <- eval(lef, envir=bVar) * ifelse(mid0=="<", -1, 1)
        b[i]  <- -eval(rig) * ifelse(mid0=="<", -1, 1)
      }
      
      # Convert to list format
      if(mid0 %in% c(">", "<")) {
        constraints <- list(ineqA=A, ineqB=b)
      } else {
        constraints <- list(eqA=A, eqB=b)
      }
    }
  }
  # End solver settings 10/22/25
  start_time <- Sys.time()
  time_limit <- (72 - 1) * 60 * 60 # 72 hours - 1 for processing
  last_iter_time <- Sys.time()
  # End added 11/3/25
  
  if(!is.null(apollo_inputs$silent)) silent <- apollo_inputs$silent else silent <- FALSE
  if(!is.null(apollo_inputs$apollo_control$seed)) seed <- apollo_inputs$apollo_control$seed + 4 else seed <- 13 + 4

  ### Checks
  if(nCandidates<2) stop("SYNTAX ISSUE - Argument 'nCandidates' should be at least 2.")
  if(maxStages<1) stop("SYNTAX ISSUE - Argument 'maxStages' should be at least 1.")
  if(anyNA(c(apolloBetaMin,apolloBetaMax)) & !smartStart) stop("SYNTAX ISSUE - Invalid 'apolloBetaMin' and/or 'apolloBetaMax' parameters.")
  apollo_print("Testing probability function (apollo_probabilities)")
  apollo_inputs$apollo_control$noDiagnostics <- TRUE
  apollo_probabilitiesVal <- apollo_insertComponentName(apollo_probabilities)
  apollo_probabilitiesVal(apollo_beta, apollo_inputs, functionality="validate")
  rm(apollo_probabilitiesVal)

  ### Pre-process likelihood function
  if(!silent) apollo_print("Pre-processing likelihood function...")
  # Create multi-core version of likelihood (if needed)
  apollo_logLike <- apollo_makeLogLike(apollo_beta, apollo_fixed,
                                       apollo_probabilities, apollo_inputs,
                                       list(estimationRoutine='BHHH'))
  on.exit(if(exists('apollo_logLike') && apollo_inputs$apollo_control$nCores>1) parallel::stopCluster(environment(apollo_logLike)$cl))
  # Create gradient function if required (and possible)
  if(!is.null(apollo_inputs$apollo_control$analyticGrad) && apollo_inputs$apollo_control$analyticGrad){
    # apollo_makeGrad will create gradient function ONLY IF all components have analytical gradient.
    grad <- apollo_makeGrad(apollo_beta, apollo_fixed, apollo_logLike, validateGrad=TRUE)
    if(is.null(grad)) apollo_inputs$apollo_control$analyticGrad <- FALSE
  } else grad <- NULL

  ### Separate beta into variable and fixed part
  beta_var_val <- apollo_beta[!(names(apollo_beta) %in% apollo_fixed)]
  beta_fix_val <- apollo_beta[apollo_fixed]
  K <- length(beta_var_val)

  # # # # # # # # # # # # # # # # #
  #### Generate candidate set  ####
  # # # # # # # # # # # # # # # # #

  ### Only non-fixed parameters are considered.
  # Candidates AT THE START OF EACH ITERATION are stored in a list of matrices called 'candidates'.
  # Each element of the list is a matrix where each row is a candidate
  set.seed(seed)
  candidates <- list()
  apollo_print(paste("Creating initial set of",nCandidates,"candidate values."))
  if(smartStart){
    # Create neighbours of starting value using Hessian (Bierlaire et al. 2007)
    apollo_print(" (using Hessian, this might take a while).")
    if(!is.null(grad)){
      sumGradLL <- function(theta) colSums( grad(theta) )
      H <- numDeriv::jacobian(func=sumGradLL, x=beta_var_val)
    } else H <- numDeriv::hessian(apollo_logLike, beta_var_val, sumLL=TRUE)
    eig <- eigen(H)
    w <- apply(eig$vectors, MARGIN=2, function(wi) wi/sqrt(sum(wi^2)))
    w <- cbind(w,-w)
    p <- rep(exp(0.5*eig$values),2)
    p <- p/sum(p)
    w <- w[, sample(ncol(w), nCandidates-1, replace=TRUE, prob=p)]
    w <- split(w, rep(1:ncol(w), each = nrow(w)))
    a <- as.list(stats::runif(length(w), min=0.75, max=1))
    z <- beta_var_val + mapply(function(wi,ai) wi*ai , w, a)
    z <- cbind(beta_var_val,z) # matrix where each column is a candidate
    candidates[[1]] <- t(z)
    rm(H, eig, w, p, a, z)
  ######### START MODIFICATION #########
  } else if (tolower(searchStart_settings[["sampling_method"]]) == "logit_random") {
    n_samples <- nCandidates - 1
    candidates[[1]] <- sample_logit_random(n_samples, beta_var_val, apollo_beta)
    candidates[[1]] <- rbind(beta_var_val, candidates[[1]])
    cand_gen_method <- c("beta_var_val", rep("logit_random", n_samples))
    
  } else if (tolower(searchStart_settings[["sampling_method"]]) == "random") {
    n_samples <- nCandidates - 1
    candidates[[1]] <- sample_random(n_samples, beta_var_val)
    candidates[[1]] <- rbind(beta_var_val, candidates[[1]])
    cand_gen_method <- c("beta_var_val", rep("random", n_samples))
    
  } else if (tolower(searchStart_settings[["sampling_method"]]) == "hybrid") {
    n_samples <- nCandidates - 1
    sample_method_ratio <- 0.9
    n_logit  <- round(sample_method_ratio * (n_samples))
    n_random <- n_samples - n_logit
    # Sample from both logit_random & random
    logit_mat  <- sample_logit_random(n_logit, beta_var_val, apollo_beta)
    random_mat <- sample_random(n_random, beta_var_val)
    
    candidates[[1]] <- rbind(beta_var_val, logit_mat, random_mat)
    cand_gen_method <- c(
      "beta_var_val",
      rep("logit_random", n_logit),
      rep("random",      n_random)
    )
    
    ######### END MODIFICATION ######### (default modified latin hypercube sampling below)
  } else{
    # Create random starting values using apolloBetaMin & apolloBetaMax.
    apolloBetaMin <- apolloBetaMin[names(beta_var_val)]
    apolloBetaMax <- apolloBetaMax[names(beta_var_val)]
    candidates[[1]] <- t(apollo_mlhs(nCandidates-1,K,1)) #matrix(stats::runif((nCandidates-1)*K), nrow=K, ncol=nCandidates-1)
    candidates[[1]] <- apolloBetaMin + (apolloBetaMax-apolloBetaMin)*candidates[[1]]
    candidates[[1]] <- cbind(beta_var_val, candidates[[1]])
    candidates[[1]] <- t(candidates[[1]])
  }
  
  ######### START MODIFICATION #########
  # ───────────────────────────────────────────────────────────────────────────
  # Initialize tracking of each candidate
  stop_status <- rep("init", nCandidates)
  stop_ef <- rep(-2, nCandidates)  
  pruned_optima_row <- rep(0, nCandidates)
  end_conv <- rep(-2, nCandidates)
  end_LL <- rep(0, nCandidates)
  # ───────────────────────────────────────────────────────────────────────────
  ######### END MODIFICATION #########

  ### Calculate LL of all candidates AT THE START OF THE ITERATION
  LL <- rep(0, nCandidates)
  cat("Calculating LL of candidates 0%")
  for(j in 1:nCandidates){
    beta_test        <- as.vector(candidates[[1]][j,])
    names(beta_test) <- names(beta_var_val)
    LL[j]            <- apollo_logLike(beta_test, sumLL=TRUE)
    if(j==ceiling(nCandidates/2)) cat("50%") else if(j%%ceiling(nCandidates/10)==0) cat(".")
  }
  cat("100%\n")

  ### Remove candidates with infinite or NA loglike
  any_non_finite <- any(!is.finite(LL))
  if(any_non_finite){
    removeRows <- which(!is.finite(LL))
    ######### START MODIFICATION #########
    # Save non-finite candidates separately for ref
    candidates_non_finite <- candidates[[1]][removeRows, , drop = FALSE]
    cand_gen_method_nf <- cand_gen_method[removeRows]
    LL_non_finite          <- LL[removeRows]
    stop_ef_non_finite     <- rep(-4, length(removeRows))  # Define status code for non-finite LL
    stop_status_non_finite <- rep("removed_nonfinite_LL", length(removeRows))
    pruned_optima_row_nf   <- rep(0, length(removeRows))
    end_conv_nf            <- rep(0, length(removeRows))
    end_LL_nf              <- LL_non_finite  # NA or Inf
    end_ef_nf              <- stop_ef_non_finite
    end_outcome_status_nf  <- stop_status_non_finite
    
    # candidate_outcomes_nf <- data.frame(
    #   candidates_non_finite,
    #   LL = LL_non_finite,
    #   stop_ef = stop_ef_non_finite,
    #   stop_status = stop_status_non_finite,
    #   pruned_optima_row = pruned_optima_row_nf,
    #   end_conv = end_conv_nf,
    #   end_LL = end_LL_nf,
    #   row.names = seq_len(nrow(candidates_non_finite)) + nCandidates - nrow(candidates_non_finite)
    #   )
    
    # Update arrays to remove non-finite
    stop_ef           = stop_ef[-removeRows]
    stop_status       = stop_status[-removeRows]
    pruned_optima_row = pruned_optima_row[-removeRows]
    end_conv          = end_conv[-removeRows]
    end_LL            = end_LL[-removeRows]

    ######### END MODIFICATION #########
    
    # Update working set (finite only)
    candidates[[1]] <- candidates[[1]][-removeRows,]
    cand_gen_method <- cand_gen_method[-removeRows]
    LL <- LL[-removeRows]
    nCandidates <- nCandidates - length(removeRows)
    apollo_print(paste0(length(removeRows), " candidates removed due to non-finite starting LL. ", nCandidates, " remain."))
    if(nCandidates<1) stop("CALCULATION ISSUE - All initial candidates had non-finite starting LL.")
    # if(nCandidates==1) return(candidates[[1]])
  }

  ### Write candidates
  fileName <- paste0(apollo_inputs$apollo_control$modelName,"_searchStart.csv")
  if(dir.exists(apollo_inputs$apollo_control$outputDirectory)){
    lastChar <- nchar(apollo_inputs$apollo_control$outputDirectory)
    lastChar <- substr(apollo_inputs$apollo_control$outputDirectory, start=lastChar, stop=lastChar)
    if(lastChar=="/") fileName <- paste0(apollo_inputs$apollo_control$outputDirectory, fileName)
    if(lastChar!="/") fileName <- paste0(apollo_inputs$apollo_control$outputDirectory, "/", fileName)
    rm(lastChar)
  }
  ######### START MODIFICATION #########
  # utils::write.csv(cbind(candidates[[1]], LL=LL, stage=1), fileName, row.names=FALSE)
  candidate_outcomes <- data.frame(row_num=seq_len(nrow(candidates[[1]])), gen_method=cand_gen_method, candidates[[1]], LL=LL, stop_status=stop_status, stage=1)
  utils::write.csv(candidate_outcomes, fileName, row.names=FALSE)
  ######### END MODIFICATION #########
  
  


  # # # # # # # # # # # # # # # #
  #### Loop over candidates  ####
  # # # # # # # # # # # # # # # #
  
  ######### START MODIFICATION #########
  # Set up cluster once, before while loop
  if (parallel) {
    # IF PARALLEL, APOLLO's native nCores must be set to 1
    if (!is.null(parallel_settings$cluster)) {
      cl <- parallel_settings$cluster
    } else { # no cluster provided
      # If memory is specified
      if (!is.null(parallel_settings$mem_per_core) && !is.na(parallel_settings$mem_per_core)){
        
        # Specify memory allocation (gb) needed for each model
        total_mem_required <- switch(apollo_inputs$heterogeneity_spec,
                                     mixed  = 8,
                                     latent = 2,
                                     none   = 2)
        
        # Adjust number of cores so that each core gets at least mem_per_core GB
        n_cores_total <- parallel_settings$n_cores
        mem_per_core <- parallel_settings$mem_per_core
        n_cores_safe <- max(1, floor(n_cores_total * mem_per_core / total_mem_required))
        
        apollo_print(paste0("Total avail. cores = ", n_cores_total, " cores"))
        apollo_print(paste0("Avail. GB per core = ", mem_per_core, " GB"))
        apollo_print(paste0("Req. GB per cluster = ", total_mem_required, " GB"))
        apollo_print(paste0("Running searchStart on ", n_cores_safe, " cores."))
        
      } else{
        # n_cores_safe <- parallel_settings$n_cores
        # 
        # apollo_print(paste0("No min CPU memory specification."))
        cores_per_cluster <- switch(apollo_inputs$heterogeneity_spec,
                                        mixed  = 1,
                                        latent = 1,
                                        none   = 1)
        n_cores_safe <- max(1, floor(parallel_settings$n_cores / cores_per_cluster))
        apollo_print(paste0("Running locally with ", cores_per_cluster, " cores per cluster."))
        apollo_print(paste0("Running searchStart on ", n_cores_safe, " cores."))
      }
      cl <- makeCluster(n_cores_safe)
      clusterEvalQ(cl, {
        library(apollo)
        library(stringr)
        source("core/load_apollo_funcs.R")
      })
      clusterExport(cl, c("apollo_logLike","grad","beta_var_val", "apollo_inputs"), #c("apollo_logLike","grad","bfgsIter","beta_var_val")
                    envir = environment())
      on.exit(stopCluster(cl), add = TRUE)
    }
  }
  did_best_conv <- FALSE
  
  # Added 10/22/25
  gradientNorm_mat <- matrix(NA, nrow=length(LL), ncol=1)
  colnames(gradientNorm_mat)     <- paste0("gradNorm", 1:ncol(gradientNorm_mat))
  already_converged <- rep(FALSE, nCandidates)
  # End added 10/22/25
  ######### END MODIFICATION #########

  active    <- rep(TRUE, nCandidates)
  converged <- rep(FALSE, nCandidates)
  s <- 1
  original_bfgsIter <- bfgsIter
  while((sum(active)>1 | !did_best_conv) & s<=maxStages){ # MODIFIED: Force best point to converge
    activeRows <- which(active)
    # Adjust number of BFGS iterations based on stage number
    if (s >= 20){
      bfgsIter <- round(20 * original_bfgsIter)
    } else if (s >= 10){
      bfgsIter <- round(4 * original_bfgsIter)
    } else {
      bfgsIter <- original_bfgsIter
    }
    apollo_print('\n')
    apollo_print(paste0("Stage ", s, ", ", length(activeRows), " active candidates."))
    apollo_print(paste("Estimating", bfgsIter, "BFGS iteration(s) for each active candidate."))
    cat(" Candidate......LLstart.....LLfinish.....GradientNorm...Converged")

    # Apply BFGS for each active candidate
    LL               <- cbind(LL, NA)
    colnames(LL)     <- paste0("LL", 1:ncol(LL))
    candidates[[s+1]]<- matrix(NA, nrow=nCandidates, ncol=K)
    gradientNorm     <- rep(NA, nCandidates)
    ######### START MODIFICATION #########
    if (parallel){
      # Get data for this stage
      cand_mat  <- candidates[[s]]
      LL_vec    <- LL[, s]
      conv_vec  <- converged
      
      # Run maxLik on each active j in parallel
      res_list <- parLapplyLB(
        cl,
        activeRows,
        function(j, cand_mat, LL_vec, conv_vec) {
          LLstart <- as.character(round(LL_vec[j],0))
          # cat("\n ", rep(" ",9-nchar(as.character(j))), j,
          #     " ", rep(" ",12-nchar(LLstart)), LLstart, sep="")
          candidateParam <- as.vector(cand_mat[j,])
          names(candidateParam) <- names(beta_var_val)
          
          if (!conv_vec[j]) {  # not yet converged
            model <- maxLik::maxLik(apollo_logLike, start=candidateParam,
                                    method="bfgs", print.level=0, grad=grad,
                                    finalHessian=FALSE, iterlim=bfgsIter, 
                                    control=solverSettings,
                                    constraints = constraints) # Added solver settings 10/22/25
            estimate      <- model$estimate
            LLn           <- model$maximum
            gradNorm_j    <- sqrt(sum(model$gradient^2))
            conv_j        <- (model$code == 0)
          } else {            # already converged
            estimate      <- candidateParam
            LLn           <- LL_vec[j]
            gradNorm_j    <- 0
            conv_j        <- TRUE
          }
          
          # # mirror the finish‐line logging
          # LLfinish <- as.character(round(LLn,0))
          # gradFin  <- as.character(round(gradNorm_j,3))
          # cat(" ", rep(" ",12-nchar(LLfinish)), LLfinish,
          #     " ", rep(" ",16-nchar(gradFin)), gradFin,
          #     " ", rep(" ",10), 1*conv_j, sep="")
          
          # return exactly the pieces we need to unpack
          list(
            j         = j,
            estimate  = estimate,
            LLn       = LLn,
            gradNorm  = gradNorm_j,
            converged = conv_j
          )
        },
        # pass the per-stage data as extra args
        cand_mat, LL_vec, conv_vec
      )
      # Unpack into master matrices/vectors
      for (res in res_list) {
        j        <- res$j
        LLstart  <- as.character(round(LL[j, s],     0))
        LLfinish <- as.character(round(res$LLn,       0))
        gradFin  <- as.character(round(res$gradNorm,  3))
        conv_j   <- as.integer(res$converged)
        
        cat("\n ",rep(" ", 9  - nchar(j)),         j,
          " ", rep(" ",12 - nchar(LLstart)),  LLstart,
          " ", rep(" ",12 - nchar(LLfinish)), LLfinish,
          " ", rep(" ",16 - nchar(gradFin)),  gradFin,
          " ", rep(" ",10), conv_j, sep = "")
        
        # write back into main objects
        candidates[[s+1]][j,] <- res$estimate
        LL[j, s+1]             <- res$LLn
        gradientNorm[j]        <- res$gradNorm
        converged[j]           <- res$converged
      }
      cat("\n")
      rm(LLstart, LLfinish, gradFin, j, conv_j, res)
    }else{

      # Execute optimization sequentially (legacy code, moved inside if statement)
      for(j in activeRows){
        LLstart <- as.character(round(LL[j,s],0))
        cat("\n ",rep(" ",9-nchar(as.character(j))), j,
            " ", rep(" ",12-nchar(LLstart)),LLstart, sep="")
  
        if(!converged[j]){ # If it hasn't converged yet
          candidateParam <- as.vector(candidates[[s]][j,])
          names(candidateParam) <- names(beta_var_val)
          model <- maxLik::maxLik(apollo_logLike, start=candidateParam,
                                  method="bfgs", print.level=0, grad=grad,
                                  finalHessian=FALSE, iterlim=bfgsIter,
                                  control=solverSettings,
                                  constraints=constraints) # Added solver settings 10/22/25
          candidates[[s+1]][j,] <- model$estimate
          LL[j,s+1]             <- model$maximum
          gradientNorm[j]       <- sqrt(sum(model$gradient^2))
          converged[j]          <- ifelse(model$code==0, TRUE, FALSE)
        } else {# if it has converged already
          candidates[[s+1]][j,] <- candidates[[s]][j,]
          LL[j,s+1]             <- LL[j,s]
          gradientNorm[j]       <- 0 # approximation
          converged[j]          <- TRUE
        }
  
        LLfinish <- as.character(round(LL[j,s+1],0))
        gradFin  <- as.character(round(gradientNorm[j],3))
        cat(" ", rep(" ",12-nchar(LLfinish)), LLfinish,
            " ", rep(" ",16-nchar(gradFin)), gradFin,
            " ", rep(" ",10), 1*converged[j], sep="")
      }; cat('\n')
      rm(LLstart, candidateParam, model, LLfinish, gradFin)
    }
    # Added 10/22/25
    gradientNorm_mat <- cbind(gradientNorm_mat, NA)
    colnames(gradientNorm_mat)     <- paste0("gradNorm", 1:ncol(gradientNorm_mat))
    gradientNorm_temp <- gradientNorm  # Added 10/22/25
    gradientNorm_temp[already_converged] <- NA  # Added 10/22/25
    gradientNorm_mat[, s+1] <- gradientNorm_temp # Added 10/22/25
    
    already_converged <- converged # update for next iteration
    # End added 10/22/25
    
    ######### END MODIFICATION #########

    ######### Start MODIFICATION #########
    # SORT LATENT CLASS PARAM
    # Sort latent class models (to avoid class mirroring issues when doing distance calculations)
    if (apollo_inputs$heterogeneity_spec == "latent"){

      intercept_vars  <- paste0("class_intercept_", 2:apollo_inputs$n_classes)

      # 1) row-wise reorder function
      reorder_by_intercept <- function(p){
        # p is a named numeric vector (one candidate-row)

        # a) grab the intercepts for classes 2…n
        ints    <- p[intercept_vars]

        # b) sort those intercepts descending
        ord2    <- order(ints, decreasing = TRUE)      # positions in 1:(n_clases-1)
        sorted_cls <- (2:apollo_inputs$n_classes)[ord2]   # actual class numbers in new order

        # c) define full new class ordering, keeping class1 first
        new_classes <- c(1, sorted_cls)

        # e) now pull each class-block in the new order
        class_blocks <- unlist(lapply(new_classes, function(k){
          grep(paste0("_", k, "$"), names(p), value = TRUE)
        }), use.names = FALSE
        )

        return(p[class_blocks])
      }
      # 2) apply to every row and reattach as a matrix
      colnames(candidates[[s+1]]) <- colnames(candidates[[1]])
      mat_reordered <- t( apply(candidates[[s+1]], 1, reorder_by_intercept) )
      colnames(mat_reordered) <- colnames(candidates[[s+1]])
      candidates[[s+1]]           <- mat_reordered
    }
    ######### END MODIFICATION #########

    # Update active list
    for(j in activeRows){
      candParam    <- as.vector(candidates[[s+1]][j,])
      candLL       <- LL[j,s+1]
      ######### START MODIFICATION #########
      # betterLLRows <- activeRows[LL[activeRows,s+1]>=candLL & activeRows!=j]
      # Get better rows only for finite gradient
      betterLLRows <- activeRows[LL[activeRows,s+1]>=candLL & activeRows!=j & is.finite(gradientNorm[activeRows])]
      bestCandRow <- which.max(LL[,s+1])
      ######### END MODIFICATION #########

      ######### START MODIFICATION #########
      # Test 3: Undefined gradient
      if (!is.finite(gradientNorm[j])) {
        active[j]        <- FALSE
        stop_status[j]   <- "pruned_test3_bad_gradient"
        stop_ef[j]       <- 5
        end_conv[j]      <- FALSE
        end_LL[j]        <- candLL
        pruned_optima_row[j] <- 0
        next # Don't need to perform additional computations
      }
      ######### END MODIFICATION #########
      
      ######### START MODIFICATION #########
      # Test 0: Converged to a worst solution
      # if(converged[j] & length(betterLLRows)>0) active[j] <- FALSE
      # if(converged[j] & length(betterLLRows)>0) {
      if(converged[j] & length(betterLLRows)>0 & j != bestCandRow) { # Added 10/22/25
        active[j] <- FALSE
        # Compute distances in parameter space to all better candidates
        betterParams <- candidates[[s+1]][betterLLRows,, drop = FALSE]
        distParam <- apply(betterParams, MARGIN=1, function(x) sqrt(sum((x-candParam)^2)) )
        # Identify any within proximity to "better"/other solutions (duplicates)
        failedT1Rows <- betterLLRows[ distParam<dTest ]
        
        # Catch if multiple points converged to the same optima (often the best)
        if (any(distParam < dTest)) {
          stop_status[j] <- "dup_best_test0_conv"
          stop_ef[j] <- 4
          pruned_optima_row[j] <- failedT1Rows[1]
          end_conv[j] <- converged[j]
          end_LL[j] <- candLL
        # Distinct optima
        } else {
          stop_status[j] <- "test0_worse_conv"
          stop_ef[j] <- 3
          end_conv[j] <- converged[j]
          end_LL[j] <- candLL
        }
      }
      ######### END MODIFICATION #########
      

      # Apply tests only if it hasn't converged and is not the best LL of the stage
      if(length(betterLLRows)>0 & !converged[j]){
        # Test 1: Distance in parameter space to a better one
        betterParams <- candidates[[s+1]][betterLLRows,]
        if(length(betterLLRows)==1) betterParams <- matrix(betterParams, nrow=1)
        distParam <- apply(betterParams, MARGIN=1, function(x) sqrt(sum((x-candParam)^2)) )
        failedT1Rows <- betterLLRows[ distParam<dTest ]

        # Test 2: small gradient norm and close to another
        distLL <- abs(as.vector(LL[betterLLRows,s+1] - candLL))
        ######### START MODIFICATION #########
        # failedT2Rows <- betterLLRows[ gradientNorm[betterLLRows]<gTest & distLL>=llTest ] 
        failedT2Rows <- betterLLRows[ gradientNorm[j]<gTest & distLL>=llTest ]  # Changed gradientNorm on best candidate to j
        ######### END MODIFICATION #########
        
        ######### START MODIFICATION #########
        # if(length(failedT1Rows)>0 | length(failedT2Rows)>0){
        #   active[j] <- FALSE
        #   apollo_print(paste0("Candidate ", j, " dropped."))
        #   if(length(failedT1Rows)>0) apollo_print(paste0("- Failed test 1: Too close to ",
        #                                                  paste0(failedT1Rows, collapse=', '),
        #                                                  " in parameter space."))
        #   if(length(failedT2Rows)>0) apollo_print(paste0("- Failed test 2: Converging to a worse solution than",
        #                                                  paste0(failedT2Rows, collapse=', ')))
        # }
        # Log when candidates are dropped
        if(length(failedT1Rows)>0 | length(failedT2Rows)>0){
          active[j] <- FALSE
          apollo_print(paste0("Candidate ", j, " dropped."))
          if(length(failedT1Rows)>0) {
            apollo_print(paste0("- Failed test 1: Too close to ",
                                                         paste0(failedT1Rows, collapse=', '),
                                                         " in parameter space."))
            stop_status[j] <- "pruned_test1_too_close"
            stop_ef[j] <- 0
            end_conv[j] <- 0
            pruned_optima_row[j] <- failedT1Rows[1]
            # end_LL[j] <- LL[j,s+1]
          } else if(length(failedT2Rows)>0) {
            apollo_print(paste0("- Failed test 2: Converging to a worse solution than ",
                                                         paste0(failedT2Rows, collapse=', ')))
            stop_status[j] <- "pruned_test2_worse"
            stop_ef[j] <- 3
            end_conv[j] <- converged[j]
            pruned_optima_row[j] <- 0 # failedT2Rows[1]
            # end_LL[j] <- LL[j,s+1]
          }
        }
        ######### END MODIFICATION #########

        rm(betterParams, distParam, failedT1Rows, failedT2Rows)
      }
      
      rm(candParam, candLL, betterLLRows)
    }

    # Print best candidate to screen
    bestCandRow <- which.max(LL[,s+1])
    bestCandParam <- as.vector(candidates[[s+1]][bestCandRow,])
    names(bestCandParam) <- names(beta_var_val)
    bestCandParam <- c(bestCandParam, apollo_beta[apollo_fixed])
    bestCandParam <- bestCandParam[names(apollo_beta)]
    bestCandParam <- matrix(bestCandParam, ncol=1, dimnames = list(names(apollo_beta), 'Value'))
    apollo_print(paste0("Best candidate so far (LL=", round(LL[bestCandRow,s+1],1), ")"))
    # print(as.matrix(round(bestCandParam,4)))
    
    ######### START MODIFICATION #########
    # Log active candidates & the current best
    
    # stop_status[bestCandRow] <- "best"
    # stop_ef[bestCandRow] <- 1
    
    for(j in which(active)) {
      # Case: last candidate, didn't converge
      if (length(activeRows) == 1 && !converged[j]){
        stop_status[j] <- "best_no_conv"
        stop_ef[j]     <- 2       # you can pick any code you like for “no‐conv best”
        end_conv[j]    <- converged[j]
        end_LL[j]      <- LL[j, s+1]
      # Case: best candidate converged
      } else if (converged[j] && j == bestCandRow) {
        stop_status[j] <- "best"
        stop_ef[j]     <- 1
        end_conv[j]    <- converged[j]
        end_LL[j] <- LL[j,s+1]
      # Case: max stages 
      } else if (s == maxStages){
        stop_status[j] <- "max_stages"
        stop_ef[j] <- -1
        end_conv[j]    <- converged[j]
        end_LL[j] <- LL[j,s+1]
      # Case: multiple candidates
      } else {
        stop_status[j] <- "active"
        stop_ef[j]     <- -1
        end_conv[j]    <- converged[j]
        end_LL[j] <- LL[j,s+1]
      }
    }

    # Write current state to file
    # tryCatch(utils::write.csv(cbind(candidates[[1]], LL), fileName, row.names=FALSE),
    #          error=function(e) apollo_print(paste0("Stage update ",s," could not be written to file", fileName)))
    # Write current state to file
    candidate_outcomes <- data.frame(
      row_num=seq_len(nrow(candidates[[1]])), gen_method=cand_gen_method,
      candidates[[1]], LL, gradientNorm_mat, stop_ef, stop_status, pruned_optima_row, end_conv, end_LL, row.names=seq_len(nrow(candidates[[1]])))
    
    # Determine final outcome
    candidate_outcomes$end_ef <- candidate_outcomes$stop_ef
    candidate_outcomes$end_outcome_status <- candidate_outcomes$stop_status
    candidate_outcomes$end_index <- seq_len(nrow(candidate_outcomes))
    # Follow the chain for each row pruned
    max_steps <- nrow(candidate_outcomes)
    for(i in seq_len(nrow(candidate_outcomes))) {
      ptr    <- candidate_outcomes$pruned_optima_row[i]
      i_root <- i
      steps  <- 0
      # walk up the chain until you hit 0
      while(!is.na(ptr) && ptr != 0 && steps < max_steps) {
        i_root <- ptr
        ptr    <- candidate_outcomes$pruned_optima_row[i_root]
        steps  <- steps + 1
      }
      # assign the root stop_status as the outcome
      candidate_outcomes$end_ef[i] <- candidate_outcomes$stop_ef[i_root]
      candidate_outcomes$end_outcome_status[i] <- candidate_outcomes$stop_status[i_root]
      candidate_outcomes$end_conv[i] <- candidate_outcomes$end_conv[i_root]
      candidate_outcomes$end_LL[i] <- candidate_outcomes$end_LL[i_root]
      candidate_outcomes$end_index[i] <- i_root
      # if cycle detected, give bad status 
      # if(steps >= max_steps) candidate_outcomes$outcome[i] <- -1
      if(steps >= max_steps) {
        cat(sprintf("\n!!! Warning: Cycle detected in pruning: point %d, steps=%d, i_root=%d, pruned_optima_row[%d]=%d\n", 
                    i, steps, i_root, i, candidate_outcomes$pruned_optima_row[i]))
        candidate_outcomes$end_outcome_status[i] <- "cycle_detected_in_pruning"
        # candidate_outcomes$end_ef[i] <- -1
        # candidate_outcomes$end_index[i] <- 0 

        # candidate_outcomes$outcome[i] <- -1 # Original, bug bc non-existent column
      }
    }
    # Add non-finite points
    if(any_non_finite){
      ll_col_names <- colnames(LL)
      LL_nf_mat <- matrix(NA_real_, nrow = nrow(candidates_non_finite), ncol = length(ll_col_names))
      colnames(LL_nf_mat) <- ll_col_names
      # Added 10/22/25 for gradNorm
      gradNorm_col_names <- colnames(gradientNorm_mat)  # ADD THIS LINE
      gradNorm_nf_mat <- matrix(NA_real_, nrow = nrow(candidates_non_finite), ncol = length(gradNorm_col_names))  # ADD THIS LINE
      colnames(gradNorm_nf_mat) <- gradNorm_col_names  # ADD THIS LINE
      
      candidate_outcomes_nf <- data.frame(
        row_num=nrow(candidate_outcomes)+seq_len(nrow(candidates_non_finite)), 
        gen_method=cand_gen_method_nf,
        candidates_non_finite,
        LL_nf_mat,
        gradNorm_nf_mat,
        stop_ef = stop_ef_non_finite,
        stop_status = stop_status_non_finite,
        pruned_optima_row = pruned_optima_row_nf,
        end_conv = end_conv_nf,
        end_LL = end_LL_nf,
        end_ef = end_ef_nf,
        end_outcome_status = end_outcome_status_nf,
        end_index = nrow(candidate_outcomes) + seq_len(nrow(candidates_non_finite)),
        row.names = seq_len(nrow(candidates_non_finite)) + nrow(candidate_outcomes)
      )
      candidate_outcomes_save <- rbind(candidate_outcomes, candidate_outcomes_nf)
    }else{
      candidate_outcomes_save <- candidate_outcomes
    }
    
    
    # Save
    tryCatch(utils::write.csv(candidate_outcomes_save, fileName, row.names=FALSE),
             error=function(e) apollo_print(paste0("Stage update ",s," could not be written to file", fileName)))
    
    ### Write points to csv too
    # 1. reference names from the first stage
    names_ref <- colnames(candidates[[1]])
    candidate_stages <- do.call(rbind, lapply(seq_along(candidates), function(stage) {
        mat <- candidates[[stage]]
        
        # 2. force the colnames to match the first element
        colnames(mat) <- names_ref
        
        # 3. convert & tag
        df <- as.data.frame(mat, row.names = NULL)
        df$stage     <- stage
        df$candidate <- seq_len(nrow(df))
        
        # 3.5 add the LL for this stage
        # (assumes LL is a matrix with columns = stages)
        df$LL <- LL[, stage]
        df$gradNorm <- gradientNorm_mat[, stage]  # Added gradNorm 10/22/25
        
        
        # 4. reorder so stage, candidate, LL, then the params
        df[, c("stage", "candidate", "LL", "gradNorm", names_ref)] # Added gradNorm 10/22/25
      }))
    # rownames(candidate_stages) <- NULL
    stage_fileName <- sub("searchStart\\.csv$", "candidateStages.csv", fileName)
    tryCatch(utils::write.csv(candidate_stages, stage_fileName, row.names=FALSE),
             error=function(e) apollo_print(paste0("Stage update ",s," could not be written to file", stage_fileName)))
    
    did_best_conv <- candidate_outcomes$end_ef[bestCandRow] == 1
    ######### END MODIFICATION #########
    
    ######### Start MODIFICAITON 11/3/25 ###########
    # Calculate how long this iteration took
    current_time <- Sys.time()
    last_iter_duration <- as.numeric(difftime(current_time, last_iter_time, units = "secs"))
    last_iter_time <- current_time
    
    # Calculate elapsed time and project if we can fit another iteration
    elapsed_time <- as.numeric(difftime(current_time, start_time, units = "secs"))
    projected_completion <- elapsed_time + last_iter_duration
    
    # Break if the next iteration would likely exceed the time limit
    if (projected_completion > time_limit) {
      elapsed_hours <- elapsed_time / 3600
      message(sprintf("Stopping: Next iteration projected to exceed limit. Ran for %.2f hours", elapsed_hours))
      break
    }
    ######### END MODIFICAITON 11/3/25 ###########
    
    # Next iteration
    s <- s+1
  }
  
  ######### START MODIFICATION #########
  # Analyze convergence rates
  n_points     <- nrow(candidate_outcomes)
  # tol <- 1e-5
  bestLL <- LL[bestCandRow,s]
  did_best_conv <- candidate_outcomes$end_ef[bestCandRow] == 1
  # did_best_conv <- candidate_outcomes$end_ef[bestCandRow] %in% c(1, 4)
  
  
  # Get all final parameter values
  # allFinalParams <- candidates[[s]]              # Matrix: rows=candidates, cols=parameters
  allFinalParams <- candidates[[s]][candidate_outcomes$end_index, ]
  bestParams <- candidates[[s]][bestCandRow, ]   # Vector of best candidate's parameters
  
  # Vectorized L-inf distance to best point
  distParam <- apply(allFinalParams, MARGIN=1, 
                     function(x) max(abs(x - bestParams)))
  
  if (did_best_conv) {
    # normal case: only count converged points
    # n_best <- sum(candidate_outcomes$end_ef == 1, na.rm = TRUE)
    # n_within_tol <- sum(
    #   candidate_outcomes$end_ef == 1 &
    #     abs(candidate_outcomes$end_LL - bestLL) <= abs(bestLL) * tol,
    #   na.rm = TRUE
    # )
    n_best <- sum(candidate_outcomes$end_conv == 1 & distParam <= dTest, na.rm = TRUE)
    n_within_tol <- sum(
      candidate_outcomes$end_conv == 1 &
      abs(candidate_outcomes$end_LL - bestLL) <= llTest,
      na.rm = TRUE
    )
  } else {
    # alt case: best point hasn't yet converged
    # n_best <- sum(candidate_outcomes$end_ef == -1, na.rm = TRUE)
    # still count *all* points within tol of bestLL
    n_best <- sum(candidate_outcomes$end_conv == 1 & distParam <= dTest, na.rm = TRUE)
    n_within_tol <- sum(
        candidate_outcomes$end_conv == 1 &
        abs(candidate_outcomes$end_LL - bestLL) <= llTest,
      na.rm = TRUE
    )
    warning("Best candidate failed to converge (end_ef = -1).")
  }
  # n_best       <- sum(candidate_outcomes$end_ef == 1, na.rm = TRUE)
  # n_within_tol  <- ifelse(
  #   n_best > 0,
  #   sum(abs(candidate_outcomes$end_LL - bestLL) <= abs(bestLL) * 1e-5, na.rm = TRUE),
  #   0
  # )

  # 3) Put into a one‐row data frame
  point_summary <- data.frame(n_points, n_best, n_within_tol, did_best_conv)

  # 4) (Optionally) write it out
  summary_file <- sub(
    "searchStart\\.csv$",
    "searchStartSummary.csv",
    fileName
  )
  write.csv(point_summary, summary_file, row.names = FALSE)
  print(point_summary)
  ######### END MODIFICATION #########

  best_candidate_param <- as.vector(bestCandParam)
  names(best_candidate_param) <- rownames(bestCandParam)
  invisible(return(list(
    beta_start         = best_candidate_param,
    candidate_outcomes = candidate_outcomes_save,
    candidate_stages   = candidate_stages,
    point_summary      = point_summary
  )))
}

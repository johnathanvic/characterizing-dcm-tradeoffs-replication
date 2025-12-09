### Modified speed test, modifying source code from v0.3.5

#' Measures evaluation time of a model
#'
#' Measures the evaluation time of a model for different number of cores and draws.
#'
#' This function evaluates the function \code{apollo_probabilities} several times using different number of threads (a.k.a. processor cores),
#' and draws (if the model uses mixing). It then plots the estimation time for each combination.
#' Estimation time grows at least linearly with number of draws, while time savings decrease with the number of threads.
#' This function can help decide what number of draws and cores to use for estimation, though a high number of draws is always
#' recommended. If the computer will be used for additional activities during estimation, no more than (machine number of cores - 1) should be used.
#' Using more threads than cores available in the machine will lead to reduce dperformance.
#' The use of additional cores come at the expense of additional memory usage. If R uses more memory than the physical RAM available,
#' then significant slow-downs in processing time can be expected. This function can help avoiding such pitfalls.
#'
#' @param apollo_beta Named numeric vector. Names and values for parameters.
#' @param apollo_fixed Character vector. Names (as defined in \code{apollo_beta}) of parameters whose value should not change during estimation.
#' @param apollo_probabilities Function. Returns probabilities of the model to be estimated. Must receive three arguments:
#'                          \itemize{
#'                            \item \strong{\code{apollo_beta}}: Named numeric vector. Names and values of model parameters.
#'                            \item \strong{\code{apollo_inputs}}: List containing options of the model. See \link{apollo_validateInputs}.
#'                            \item \strong{\code{functionality}}: Character. Can be either \strong{\code{"components"}}, \strong{\code{"conditionals"}}, \strong{\code{"estimate"}} (default), \strong{\code{"gradient"}}, \strong{\code{"output"}}, \strong{\code{"prediction"}}, \strong{\code{"preprocess"}}, \strong{\code{"raw"}}, \strong{\code{"report"}}, \strong{\code{"shares_LL"}}, \strong{\code{"validate"}} or \strong{\code{"zero_LL"}}.
#'                          }
#' @param apollo_inputs List grouping most common inputs. Created by function \link{apollo_validateInputs}.
#' @param speedTest_settings List. Contains settings for this function. User input is required for all settings except those with a default or marked as optional. 
#'                                   \itemize{
#'                                     \item \strong{\code{nCoresTry}}: Numeric vector. Number of threads to try. Default is from 1 to the detected number of cores.
#'                                     \item \strong{\code{nDrawsTry}}: Numeric vector. Number of inter and intra-person draws to try. Default value is c(50, 100, 200).
#'                                     \item \strong{\code{nRep}}: Numeric scalar. Number of times the likelihood is evaluated for each combination of threads and draws. Default is 10.
#'                                   }
#' @return A data frame with columns: model_name, cores, inter_draws, intra_draws, calls, time_per_call, and ram_mb. A graph is also plotted.
#' @importFrom graphics matplot title legend
#' @export
custom_apollo_speedTest=function(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings=NA){
  
  # # # # # # # # # # #
  #### Initialise  ####
  # # # # # # # # # # #
  
  tmp <- parallel::detectCores(); if(is.na(tmp)) tmp <- 3
  default <- list(nDrawsTry = c(50, 100, 200),
                  nCoresTry = 1:tmp,
                  nRep      = 10,
                  n_rep_per_batch = 10)
  if(length(speedTest_settings)==1 && is.na(speedTest_settings)) speedTest_settings <- default
  tmp <- names(default)[ !(names(default) %in% names(speedTest_settings)) ]
  for(i in tmp)  speedTest_settings[[i]] <- default[[i]]
  rm(tmp, default)
  
  apollo_inputs$silent <- TRUE
  
  nDrawsTry        = speedTest_settings[["nDrawsTry"]]
  nCoresTry        = speedTest_settings[["nCoresTry"]]
  nRep             = speedTest_settings[["nRep"]]
  apollo_checkArguments(apollo_probabilities,apollo_inputs$apollo_randCoeff,apollo_inputs$apollo_lcPars)
  
  ### Checks
  if(apollo_inputs$apollo_control$HB) stop("INCORRECT FUNCTION/SETTING USE - The function 'apollo_speedTest' is not applicable for models estimated using HB.")
  if(apollo_inputs$apollo_control$mixing){
    if(anyNA(apollo_inputs$apollo_draws)) stop("SYNTAX ISSUE - Argument 'apollo_draws' must be provided when estimating mixture models.")
    if(!is.function(apollo_inputs$apollo_randCoeff)) stop("SYNTAX ISSUE - Argument 'apollo_randCoeff' must be provided when estimating mixture models.")
  }
  if(!apollo_inputs$apollo_control$mixing){
    apollo_inputs$apollo_draws <- list(interUnifDraws=c(), interNormDraws=c(), interNDraws=0,
                                       intraUnifDraws=c(), intraNormDraws=c(), intraNDraws=0)
    nDrawsTry <- 0
    apollo_inputs$apollo_randCoeff <- NA
  }
  
  usesInter <- (length(apollo_inputs$apollo_draws$interUnifDraws) + length(apollo_inputs$apollo_draws$interNormDraws))>0 &
    apollo_inputs$apollo_draws$interNDraws>0
  usesIntra <- (length(apollo_inputs$apollo_draws$intraUnifDraws) + length(apollo_inputs$apollo_draws$intraNormDraws))>0 &
    apollo_inputs$apollo_draws$intraNDraws>0
  if(!usesInter) apollo_inputs$apollo_draws$interNDraws <- 0
  if(!usesIntra) apollo_inputs$apollo_draws$intraNDraws <- 0
  
  # # # # # # # # # #
  #### Main Loop ####
  # # # # # # # # # #
  
  cat("       ___Draws___              sec/")
  cat("\nnCores inter intra  progress  LLCall RAM(MB)")
  
  if(nRep<=10) stepPrint <- 1 else stepPrint <- floor(nRep/10)
  timeTaken <- matrix(0, nrow=length(nCoresTry), ncol=length(nDrawsTry))
  ramUsage <- matrix(0, nrow=length(nCoresTry), ncol=length(nDrawsTry))  # ADDED
  
  detailed_results <- list() # ADDED
  result_counter <- 1 # ADDED
  
  for(j in 1:length(nDrawsTry)){
    
    for(i in 1:length(nCoresTry)){
      
      apollo_inputs$apollo_control$nCores <- nCoresTry[i]
      if(usesInter) apollo_inputs$apollo_draws$interNDraws <- nDrawsTry[j]
      if(usesIntra) apollo_inputs$apollo_draws$intraNDraws <- nDrawsTry[j]
      
      cat("\n")
      cat(sprintf("%6i",apollo_inputs$apollo_control$nCores), " ", 
          sprintf("%5i",apollo_inputs$apollo_draws$interNDraws), " ", 
          sprintf("%5i",apollo_inputs$apollo_draws$intraNDraws), " ", sep="")
      
      if(apollo_inputs$apollo_control$mixing){
        apollo_inputs$draws <- apollo_makeDraws(apollo_inputs, silent=TRUE)
      } else draws <- NA
      
      #if(apollo_inputs$apollo_control$nCores>1){
      #  cl <- apollo_makeCluster(apollo_probabilities, apollo_inputs, silent=TRUE)
      #} else cl <- NA
      
      theta_var_val <- apollo_beta[!(names(apollo_beta) %in% apollo_fixed)]
      theta_fix_val <- apollo_beta[apollo_fixed]
      theta_min <- 0.95*theta_var_val
      theta_max <- 1.05*theta_var_val
      U <- t(apollo_mlhs(nRep, length(theta_var_val), 1))
      deltaTheta <- theta_max - theta_min
      
      apollo_estSet  <- list(estimationRoutine="bgw", maxIterations=2, writeIter=FALSE, hessianRoutine="none", printLevel=0, silent=FALSE)
      apollo_logLike <- apollo_makeLogLike(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, apollo_estSet)
      
      t0 <- Sys.time()
      
      # ORIGINAL
      # for(k in 1:nRep){
      #   theta_test <- theta_min + as.vector(U[,k])*deltaTheta
      #   LL_test    <- apollo_logLike(theta_test, sumLL=TRUE)
      #   if(k%%stepPrint==0) cat('.')
      # }
      
      ### V1 (every iter, overlooking cpu time for fast models)
      # for(k in 1:nRep){
      #   theta_test <- theta_min + as.vector(U[,k])*deltaTheta
      #   
      #   # Time individual function call (both methods)
      #   sys_start <- Sys.time()
      #   proc_start <- proc.time()
      #   LL_test <- apollo_logLike(theta_test, sumLL=TRUE)
      #   proc_end <- proc.time()
      #   sys_end <- Sys.time()
      #   
      #   # Calculate Sys.time() measurement
      #   sys_elapsed <- as.numeric(difftime(sys_end, sys_start, units='secs'))
      #   
      #   # Calculate proc.time() measurements
      #   proc_user <- proc_end["user.self"] - proc_start["user.self"]
      #   proc_sys <- proc_end["sys.self"] - proc_start["sys.self"]
      #   proc_elapsed <- proc_end["elapsed"] - proc_start["elapsed"]
      #   proc_total_cpu <- proc_user + proc_sys
      #   
      #   # Store individual result
      #   detailed_results[[result_counter]] <- data.frame(
      #     model_name = apollo_inputs$apollo_control$modelName,
      #     cores = apollo_inputs$apollo_control$nCores,
      #     inter_draws = apollo_inputs$apollo_draws$interNDraws,
      #     intra_draws = apollo_inputs$apollo_draws$intraNDraws,
      #     rep_number = k,
      #     sys_elapsed_seconds = sys_elapsed,
      #     proc_user_seconds = as.numeric(proc_user),
      #     proc_system_seconds = as.numeric(proc_sys),
      #     proc_elapsed_seconds = as.numeric(proc_elapsed),
      #     proc_total_cpu_seconds = as.numeric(proc_total_cpu),
      #     timestamp = format(sys_start, "%Y-%m-%d %H:%M:%OS3"),
      #     stringsAsFactors = FALSE
      #   )
      #   result_counter <- result_counter + 1
      #   
      #   if(k%%stepPrint==0) cat('.')
      # }
      # # End modified version
      
      ###
      # Modified to time in batches
      for(k in 1:nRep){
        theta_test <- theta_min + as.vector(U[,k])*deltaTheta
        
        # Run multiple times to get measurable CPU time
        n_rep_per_batch <- 10
        
        # Measure both sys and CPU time for THE SAME set of executions
        sys_start <- Sys.time()
        proc_start <- proc.time()
        
        for(inner in 1:n_rep_per_batch){
          LL_test <- apollo_logLike(theta_test, sumLL=TRUE)
        }
        
        proc_end <- proc.time()
        sys_end <- Sys.time()
        
        # Calculate average times (divide by n_rep_per_batch)
        sys_elapsed <- as.numeric(difftime(sys_end, sys_start, units='secs')) / n_rep_per_batch
        proc_user <- (proc_end["user.self"] - proc_start["user.self"]) / n_rep_per_batch
        proc_sys <- (proc_end["sys.self"] - proc_start["sys.self"]) / n_rep_per_batch
        proc_elapsed <- (proc_end["elapsed"] - proc_start["elapsed"]) / n_rep_per_batch
        proc_total_cpu <- proc_user + proc_sys
        
        # Store individual result
        detailed_results[[result_counter]] <- data.frame(
          model_name = apollo_inputs$apollo_control$modelName,
          cores = apollo_inputs$apollo_control$nCores,
          inter_draws = apollo_inputs$apollo_draws$interNDraws,
          intra_draws = apollo_inputs$apollo_draws$intraNDraws,
          # rep_number = k,
          batch_number = k,
          sys_elapsed_seconds = sys_elapsed,
          proc_user_seconds = as.numeric(proc_user),
          proc_system_seconds = as.numeric(proc_sys),
          proc_elapsed_seconds = as.numeric(proc_elapsed),
          proc_total_cpu_seconds = as.numeric(proc_total_cpu),
          timestamp = format(sys_start, "%Y-%m-%d %H:%M:%OS3"),
          stringsAsFactors = FALSE
        )
        result_counter <- result_counter + 1
        
        if(k%%stepPrint==0) cat('.')
      }
      ###
      
      t1 <- Sys.time()
      
      
      timeTaken[i,j] <- as.numeric(difftime(t1,t0,units='secs'))/nRep
      cat(' ', sprintf("%6.2f", timeTaken[i,j]),sep='')
      
      mbRAM <- sum(gc()[,2])
      if(apollo_inputs$apollo_control$nCores>1){
        gcClusters <- parallel::clusterCall(environment(apollo_logLike)$cl, gc)
        gcClusters <- Reduce('+',lapply(gcClusters, function(x) sum(x[,2])))
        mbRAM <- mbRAM + gcClusters
      }
      ramUsage[i,j] <- mbRAM    # ADDED
      cat(' ', sprintf("%7.1f", mbRAM), sep='')
      
      if(exists('apollo_logLike') && !anyNA(environment(apollo_logLike)$cl)) parallel::stopCluster(environment(apollo_logLike)$cl)
    }
  }
  cat("\n")
  
  colnames(timeTaken) <- paste('draws', nDrawsTry, sep='')
  rownames(timeTaken) <- paste('cores', nCoresTry, sep='')
  colnames(ramUsage) <- paste('draws', nDrawsTry, sep='')   # ADDED
  rownames(ramUsage) <- paste('cores', nCoresTry, sep='')   # ADDED
  
  # graphics::matplot(x=nCoresTry, y=timeTaken,
  #                   type='o', lty=1, lwd=2, cex=1.5, fg='gray',
  #                   lab=c(length(nCoresTry), length(nDrawsTry), 1),
  #                   pch=1:length(nDrawsTry), col=1:length(nDrawsTry),
  #                   xlab='Number of cores', ylab='Avg. time per call to apollo_probabilities (sec)') 
  # graphics::title(apollo_inputs$apollo_control$modelName)
  # if(apollo_inputs$apollo_control$mixing) graphics::legend("topright", legend = colnames(timeTaken),
  #                                                          pch=1:length(nDrawsTry), col=1:length(nDrawsTry), lty=1,
  #                                                          bg='transparent', bty='n', horiz=TRUE) 
  
  cat('\nSummary of results (sec. per call to LL function)','\n', sep="")
  print(round(timeTaken,4))
  
  #### Added/Modified ending
  # Create data frame with results
  speedTest_df <- data.frame(
    model_name = rep(apollo_inputs$apollo_control$modelName, length(nCoresTry) * length(nDrawsTry)),
    cores = rep(nCoresTry, times = length(nDrawsTry)),
    inter_draws = rep(nDrawsTry * usesInter, each = length(nCoresTry)),
    intra_draws = rep(nDrawsTry * usesIntra, each = length(nCoresTry)),
    calls = nRep,
    time_per_call = as.vector(timeTaken),
    ram_mb = as.vector(ramUsage)
  )
  
  # Save time for each point
  # Combine all detailed results into one data frame
  detailed_df <- do.call(rbind, detailed_results)
  
  result <- list(
    summary = speedTest_df,
    detailed = detailed_df
  )
  
  invisible(result)
}

calc_speedTest <- function(model, speedTest_settings = NULL){

  # Set defaults
  if(model$apollo_inputs$apollo_control$mixing) {
    n_draws <- model$apollo_inputs$n_draws # default to original n_draws
  } else {
    n_draws <- 0
  }
  defaults <- list(nCoresTry = c(1, 2, 4), nDrawsTry = n_draws, nRep = 100)
  
  # Merge with provided settings (provided settings override defaults)
  if(is.null(speedTest_settings)) {
    speedTest_settings <- defaults
  } else {
    speedTest_settings <- modifyList(defaults, speedTest_settings)
  }
  
  # Run speed test and get results data frame
  speedTest_results <- custom_apollo_speedTest(
    model$estimate, 
    model$apollo_inputs$apollo_fixed, 
    model$apollo_probabilities, 
    model$apollo_inputs, 
    speedTest_settings
  )
  speedTest_df <- speedTest_results$summary
  detailed_speedTest_df <- speedTest_results$detailed
  
  print(speedTest_df)
  # Save results
  source_folder <- model$apollo_inputs$apollo_control$outputDirectory
  model_name <- model$apollo_inputs$apollo_control$modelName
  file_name <- paste0(source_folder, "/", model_name, "_speedTest.csv")
  write.csv(speedTest_df, file_name, row.names = FALSE)
  cat("Saved speed test results to:", file_name, "\n")

  detailed_file_name <- paste0(source_folder, "/", model_name, "_speedTest_detailed.csv")
  write.csv(detailed_speedTest_df, detailed_file_name, row.names = FALSE)
  cat("Saved detailed speed test results to:", detailed_file_name, "\n")
  
  return(speedTest_df)
}

# model_path          <- "solutions/main2_recollectA/job_35355830/MNL_none_pref/MNL_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/NL_none_pref/NL_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/LCL2_none_pref/LCL2_none_pref_model.rds"
# # model_path          <- "solutions/main2_recollectA/job_35355830/LC2_NL_pref/LC2_NL_pref_model.rds"
# # # model_path          <- "solutions/main2_recollectA/job_35355830/LC2_CNL_pref/LC2_CNL_pref_model.rds"
# model_path          <- "solutions/main2_recollectA/job_35355830/MXL100_none_pref/MXL100_none_pref_model.rds"
# 
# model <- readRDS(model_path)
# model$apollo_inputs$apollo_control$outputDirectory <- "output/debug"
# res <- calc_speedTest(model,
#                       speedTest_settings = list(nCoresTry = 1, nRep = 10))
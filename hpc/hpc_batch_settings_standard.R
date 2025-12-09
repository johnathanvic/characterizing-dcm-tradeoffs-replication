# 1) defaults  
default_settings <- list(
  # Model Settings
  name               = "MNL",
  heterogeneity_spec = "none",
  nesting_spec       = "none",
  n_classes          = NA,
  n_draws            = 1000,
  utility_space      = "pref",
  lognorm_price_dist = FALSE,
  workInLogs         = FALSE,
  observed_attributes= c("Price","Acceleration","OpCost",
                         "hev","phev20","phev40","bev",
                         "bevRangeRel","noBEVFC",
                         "american","chinese","japanese","skorean"),
  # output_directory     = "output/debug/",
  database             = "cbc_short_veh_df",
  
  # Estimation Settings
  n_searchStart        = 101,
  estimationRoutine    = "BFGS",
  maxLik_settings      = list(
                            gradtol=1e-6, 
                            tol = 1e-100, 
                            reltol=1e-100, 
                            steptol=1e-100, 
                            lambdatol=1e-100, 
                            qrtol=1e-100, 
                            printLevel=2, 
                            iterlim=1000
                          ),
  # Search Start
  bfgsIter            = 25, # 10  # default is 10
  maxStages           = 50, # 50 # default is 10
  sampling_method     = "hybrid",
  cv_nRep             = 5,
  cv_validationSize   = 0.2,
  n_func_eval         = 10000
)

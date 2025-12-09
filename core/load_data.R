
load_and_process_data <- function(
    vehicle_type = "car",
    data_path    = "C:/Users/johna/Documents/GitHub/DrivingElectrificationReplication/Estimation/Data/ChoiceData/Pooled/PooledWeighted.xlsx",
    pooled_url   = "https://raw.githubusercontent.com/crforsythe/DrivingElectrificationReplication/main/Estimation/Data/ChoiceData/Pooled/PooledWeighted.xlsx",
    run_local    = FALSE
    ) {
  # Load required packages
  if(!requireNamespace("readxl", quietly=TRUE))  stop("please install readxl")
  if(!requireNamespace("dplyr", quietly=TRUE))   stop("please install dplyr")
  if(!requireNamespace("tidyr", quietly=TRUE))   stop("please install tidyr")
  if(!requireNamespace("apollo", quietly=TRUE))  stop("please install apollo")
  
  # Get source file
  options(timeout = 300)
  print("300s Timeout on Loading Dataset")
  if(run_local) {
    message("Reading from local file: ", data_path)
    file_path <- data_path
  } else {
    message("Downloading from URL: ", pooled_url)
    file_path <- tempfile(fileext = ".xlsx")
    download.file(pooled_url, destfile = file_path, mode = "wb")
  }
  
  # Read excel file
  #  cbcShort-<type>, drop unwanted cols
  cbc_short_veh_df <- readxl::read_excel(
    file_path,
    sheet = paste0("cbcShort-",  vehicle_type),
    guess_max = 10000    # scan more rows before deciding on a type
    
  ) %>%
    dplyr::select(-`Unnamed: 0`, -householdSize) # Remove NA columns
  
  # Rescale BEV range to per/100 mi
  cbc_short_veh_df$bevRangeRelPerMileC1 <- cbc_short_veh_df$bevRangeRelC1
  cbc_short_veh_df$bevRangeRelC1      <- cbc_short_veh_df$bevRangeRelC1 / 100
  cbc_short_veh_df$bevRangeRelPerMileC2 <- cbc_short_veh_df$bevRangeRelC2
  cbc_short_veh_df$bevRangeRelC2      <- cbc_short_veh_df$bevRangeRelC2 / 100
  cbc_short_veh_df$bevRangeRelPerMileC3 <- cbc_short_veh_df$bevRangeRelC3
  cbc_short_veh_df$bevRangeRelC3      <- cbc_short_veh_df$bevRangeRelC3 / 100

  # # # List sheet names
  # # sheet_names <- excel_sheets(pooled_path)
  # # print(sheet_names)
  # 
  # # Preview the data
  # head(cbc_short_veh_df)
  
  # Define vehicle options
  cv_options <- c("cvC1", "cvC2", "cvC3")
  hev_options <- c("hevC1", "hevC2", "hevC3")
  phev_options <- c("phev20C1", "phev20C2", "phev20C3",
                    "phev40C1", "phev40C2", "phev40C3")
  bev_options <- c("bev100C1", "bev100C2", "bev100C3",
                   "bev150C1", "bev150C2", "bev150C3",
                   "bev300C1", "bev300C2", "bev300C3",
                   "bev400C1", "bev400C2", "bev400C3")
  availCols <- c(cv_options, hev_options, phev_options, bev_options)
  
  # Get attribute column names
  c1Cols <- grep("C1$", names(cbc_short_veh_df), value = TRUE)
  attrCols <- sub("C1$", "", c1Cols)
  # print("attrCols:")
  # print(attrCols)
  
  # Create AltChoice by matching Choice to availCols
  cbc_short_veh_df <- cbc_short_veh_df %>%
    mutate(
      # Create the selected type column based on Choice
      TypeAltChoice = case_when(
        Choice == 1 ~ TypeC1,
        Choice == 2 ~ TypeC2,
        Choice == 3 ~ TypeC3
      ),
      # Create the alternative name
      ChrAltChoice = paste0(TypeAltChoice, "C", Choice),
      # Find the index of the alternative name in availCols
      AltChoice = match(ChrAltChoice, availCols)
    )
  
  # Return outputs
  data = list(
    # weight_veh_df = weight_veh_df,
    cbc_short_veh_df = cbc_short_veh_df,
    # cbc_long_veh_df = cbc_long_veh_df,
    veh_options = list(cv_options, hev_options, phev_options, bev_options),
    cv_options = cv_options,
    hev_options = hev_options,
    phev_options = phev_options,
    bev_options = bev_options,
    availCols = availCols,
    attrCols = attrCols
  )
  return(data)
}
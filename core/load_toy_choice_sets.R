
get_toy_choice_sets <- function(){
  toy_choice_set <- data.frame(
    # row.names    = c("ICEV1","PHEV1","BEV1"),
    veh_id             = c("ICEV1","PHEV1","BEV1"),
    Price              = c(30,      35,      40),    # $k
    Acceleration       = c(8.0,     7.0,     6.0),  # 0–60 mph (s)
    OpCost             = c(10,      7.5,     5),    # cents/mile
    hev                = c(0,       0,       0),    # (ind) conventional hybrid
    phev20             = c(0,       0,       0),    # (ind) PHEV w/ 20 kWh battery
    phev40             = c(0,       1,       0),    # (ind) PHEV w/ 40 kWh battery
    bev                = c(0,       0,       1),    # (ind) full battery-electric
    bevRangeRel        = c(0,       0,       0),    # (BEV only) relative range (diff from 300)
    noBEVFC            = c(0,       0,       0),    # (BEV only) no BEV fast-charge
    american           = c(1,       1,       1),    # (ind) brand indicators
    chinese            = c(0,       0,       0),
    japanese           = c(0,       0,       0),
    skorean            = c(0,       0,       0),
    icev_nest_indicator= c(1,       0,       0),
    hev_nest_indicator = c(0,       0,       0),
    phev_nest_indicator= c(0,       1,       0),
    bev_nest_indicator = c(0,       0,       1)
  )
  
  # toy_choice_set has n rows
  n <- nrow(toy_choice_set)
  idx <- rep(seq_len(n), times = 100)
  big_choice_set <- toy_choice_set[idx, ]
  big_choice_set$replicate <- rep(seq_len(100), each = n)
  big_choice_set$og_veh_id <- big_choice_set$veh_id
  big_choice_set$veh_id <- paste0(big_choice_set$veh_id, big_choice_set$replicate)
  
  return(list(
    small = toy_choice_set,
    big   = big_choice_set
  ))
}
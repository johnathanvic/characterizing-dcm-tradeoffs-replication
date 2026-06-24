# Build a MY2030 car choice set from EPA OMEGA Final Rule 2024 vehicles output,
# matching the demand model's schema (13 attrs + 4 nest indicators + veh_id).
#
# Input: inputs/EPA_Final-Rule_2024/
# Output: inputs/EPA_Final-Rule_2024/epa_choice_set_my2030.csv
#
# Run from repo root: Rscript epa_data_scripts/build_epa_choice_set.R

EPA_DIR <- "inputs/EPA_Final-Rule_2024"
VEHICLES_CSV <- file.path(EPA_DIR, "LDV_central_my2030_cars_vehicles.csv")
FUEL_PRICES_CSV  <- file.path(EPA_DIR, "context_fuel_prices_20231010.csv")
ELEC_PRICES_CSV  <- file.path(EPA_DIR, "context_electricity_prices_aeo_20231010.csv")
ONROAD_FUELS_CSV <- file.path(EPA_DIR, "onroad_fuels_20240222.csv")
OUTPUT_CSV       <- file.path(EPA_DIR, "epa_choice_set_my2030.csv")

TARGET_YEAR <- 2030

# ----------------------------------------------------------------------------
# 1. Load vehicles, filter to MY2030 cars
# ----------------------------------------------------------------------------
df <- read.csv(VEHICLES_CSV, stringsAsFactors = FALSE)
cat(sprintf("Loaded %d total vehicle rows.\n", nrow(df)))

df <- df[df$model_year == TARGET_YEAR & df$reg_class_id == "car", ]
cat(sprintf("After filtering to MY%d cars: %d rows.\n", TARGET_YEAR, nrow(df)))

# Created-alternative flag: OMEGA-generated alternative powertrains have names
# starting with "BEV of ", "PHEV of ", "HEV of ", or "ICE of ".
df$created_alternative <- as.integer(grepl(" of ", df$name, fixed = TRUE))
cat(sprintf("  created_alternative: %d created / %d real-lineage.\n",
            sum(df$created_alternative == 1), sum(df$created_alternative == 0)))

# ----------------------------------------------------------------------------
# 2. Load lookup CSVs (skip metadata row 1) and grab MY2030 prices
# ----------------------------------------------------------------------------
read_lookup <- function(path) read.csv(path, stringsAsFactors = FALSE, skip = 1)

fuel_prices <- read_lookup(FUEL_PRICES_CSV)
elec_prices <- read_lookup(ELEC_PRICES_CSV)
onroad_fuels <- read_lookup(ONROAD_FUELS_CSV)

# Both price files have multiple AEO vintages and cases. Use the most recent
# AEO Reference case (AEO2023 Reference) -- the standard basis for the 2024
# Final Rule run that produced _Final_vehicles.csv.
AEO_CONTEXT <- "AEO2023"
AEO_CASE    <- "Reference case"

gas_row <- fuel_prices[fuel_prices$fuel_id == "pump gasoline" &
                       fuel_prices$calendar_year == TARGET_YEAR &
                       fuel_prices$context_id == AEO_CONTEXT &
                       fuel_prices$case_id == AEO_CASE, ]
if (nrow(gas_row) != 1) stop("Expected exactly one gas price row; got ", nrow(gas_row))
gas_price_per_gal <- gas_row$retail_dollars_per_unit[1]

elec_row <- elec_prices[elec_prices$fuel_id == "US electricity" &
                        elec_prices$calendar_year == TARGET_YEAR &
                        elec_prices$context_id == AEO_CONTEXT &
                        elec_prices$case_id == AEO_CASE, ]
if (nrow(elec_row) != 1) stop("Expected exactly one electricity price row; got ", nrow(elec_row))
elec_price_per_kwh <- elec_row$retail_dollars_per_unit[1]

co2e_row <- onroad_fuels[onroad_fuels$fuel_id == "pump gasoline", ]
co2e_g_per_gal_gas <- co2e_row$direct_co2e_grams_per_unit[1]

cat(sprintf("Gas $%.3f/gal, Electricity $%.4f/kWh, CO2e %.0f g/gal\n",
            gas_price_per_gal, elec_price_per_kwh, co2e_g_per_gal_gas))

# ----------------------------------------------------------------------------
# 3. Derive demand-model columns
# ----------------------------------------------------------------------------

# Price ($k)
Price <- df$price_dollars / 1000

# OpCost (cents/mi). For BEVs gas term is zero; for ICE/HEV elec term is zero;
# PHEVs already have UF-blended onroad values baked in.
gas_cost_per_mi  <- (df$onroad_direct_co2e_grams_per_mile / co2e_g_per_gal_gas) * gas_price_per_gal
elec_cost_per_mi <- df$onroad_direct_kwh_per_mile * elec_price_per_kwh
OpCost <- (gas_cost_per_mi + elec_cost_per_mi) * 100

# OMEGA's projected fueling_class only has {ICE, PHEV, BEV}, having folded HEV
# into ICE. Recover the HEV label from the base-year electrification class,
# encoded as the 3rd colon-separated token in `name` (after stripping any
# "BEV of " / "PHEV of " / "HEV of " / "ICE of " prefix).
# Only relabel projected-ICE vehicles whose ancestor was HEV: a 2030 BEV
# derived from a 2022 HEV ancestor is still a BEV powertrain-wise.
name_stripped <- sub("^(BEV|PHEV|HEV|ICE) of ", "", df$name)
base_year_class <- sapply(strsplit(name_stripped, ":", fixed = TRUE),
                          function(x) if (length(x) >= 3) x[3] else NA_character_)

effective_class <- df$fueling_class
hev_recovered_mask <- df$fueling_class == "ICE" & base_year_class == "HEV"
effective_class[hev_recovered_mask] <- "HEV"
cat(sprintf("  Recovered %d HEV vehicles (projected ICE with HEV ancestor).\n",
            sum(hev_recovered_mask)))

# Acceleration: power-to-weight regression from Greene et al. (2018), Eq. 9:
#   t_0-60 = (hp/W / 0.3542)^(-1/0.88)
# BEV : P = total_emachine_kw * 1.341
# HEV : P = rated_hp + total_emachine_kw * 1.341   (combined system power)
# PHEV: P = rated_hp + total_emachine_kw * 1.341   (combined system power)
# ICE : P = rated_hp
kw_to_hp <- 1.341
hp_effective <- ifelse(
  effective_class == "BEV",  df$total_emachine_kw * kw_to_hp,
  ifelse(effective_class %in% c("PHEV", "HEV"),
         df$rated_hp + df$total_emachine_kw * kw_to_hp,
         df$rated_hp)
)
Acceleration <- (hp_effective / df$etw_lbs / 0.3542)^(-1 / 0.88)

# Floor at 2.0 s. The Greene Eq. 9 regression extrapolates below the
# production-vehicle limit (~1.9 s) for very high power-to-weight rows;
# this clip removes the unphysical tail without affecting the median.
ACC_FLOOR <- 2.0
n_clipped <- sum(is.finite(Acceleration) & Acceleration < ACC_FLOOR)
Acceleration <- pmax(Acceleration, ACC_FLOOR)
cat(sprintf("  Acceleration: clipped %d vehicles at %.1f s floor.\n",
            n_clipped, ACC_FLOOR))

# Fallback for missing/invalid values: effective-class median, then global median
valid_mask <- is.finite(Acceleration) & Acceleration > 0
if (any(!valid_mask)) {
  med_by_class <- tapply(Acceleration[valid_mask], effective_class[valid_mask], median, na.rm = TRUE)
  for (cls in names(med_by_class)) {
    needs_fill <- !valid_mask & effective_class == cls
    Acceleration[needs_fill] <- med_by_class[[cls]]
  }
  still_invalid <- !is.finite(Acceleration) | Acceleration <= 0
  if (any(still_invalid)) {
    Acceleration[still_invalid] <- median(Acceleration[is.finite(Acceleration) & Acceleration > 0])
  }
  cat(sprintf("  Acceleration: filled %d missing values with effective-class median.\n",
              sum(!valid_mask)))
}

# Powertrain dummies (use effective class so HEV is populated)
hev    <- as.integer(effective_class == "HEV")
phev20 <- as.integer(effective_class == "PHEV" & df$onroad_charge_depleting_range_mi <  30)
phev40 <- as.integer(effective_class == "PHEV" & df$onroad_charge_depleting_range_mi >= 30)
bev    <- as.integer(effective_class == "BEV")

# BEV range relative to 300 mi
bevRangeRel <- ifelse(bev == 1, df$onroad_charge_depleting_range_mi - 300, 0)

# No BEV fast-charge indicator: not in OMEGA file, default 0
noBEVFC <- rep(0L, nrow(df))

# Brand dummies (hardcoded map from the data agent)
american_set <- c("Ford", "General Motors", "Stellantis", "Tesla", "Rivian", "Lucid")
japanese_set <- c("Toyota", "Honda", "Nissan", "Mazda", "Subaru", "Mitsubishi")
skorean_set  <- c("Hyundai", "Kia")
chinese_set  <- character(0)

american <- as.integer(df$manufacturer_id %in% american_set)
japanese <- as.integer(df$manufacturer_id %in% japanese_set)
skorean  <- as.integer(df$manufacturer_id %in% skorean_set)
chinese  <- as.integer(df$manufacturer_id %in% chinese_set)

# Nest indicators (one-hot from effective class). ICE -> icev, HEV -> hev,
# PHEV -> phev, BEV -> bev.
icev_nest_indicator <- as.integer(effective_class == "ICE")
hev_nest_indicator  <- as.integer(effective_class == "HEV")
phev_nest_indicator <- as.integer(effective_class == "PHEV")
bev_nest_indicator  <- as.integer(effective_class == "BEV")

# ----------------------------------------------------------------------------
# 4. Assemble output and write
# ----------------------------------------------------------------------------
out <- data.frame(
  veh_id              = as.character(df$vehicle_id),
  Price               = Price,
  Acceleration        = Acceleration,
  OpCost              = OpCost,
  hev                 = hev,
  phev20              = phev20,
  phev40              = phev40,
  bev                 = bev,
  bevRangeRel         = bevRangeRel,
  noBEVFC             = noBEVFC,
  american            = american,
  chinese             = chinese,
  japanese            = japanese,
  skorean             = skorean,
  icev_nest_indicator = icev_nest_indicator,
  hev_nest_indicator  = hev_nest_indicator,
  phev_nest_indicator = phev_nest_indicator,
  bev_nest_indicator  = bev_nest_indicator,
  created_alternative = df$created_alternative,
  stringsAsFactors    = FALSE
)

write.csv(out, OUTPUT_CSV, row.names = FALSE)
cat(sprintf("\nWrote %s (%d rows, %d cols)\n", OUTPUT_CSV, nrow(out), ncol(out)))

# ----------------------------------------------------------------------------
# 5. Sanity checks
# ----------------------------------------------------------------------------
cat("\nSanity checks:\n")

n_created <- sum(out$created_alternative == 1)
n_real    <- sum(out$created_alternative == 0)
cat(sprintf("  Total: %d (expected 1168)\n", nrow(out)))
cat(sprintf("  Created vs real: %d / %d (expected 742 / 426)\n", n_created, n_real))

bev_oc  <- out$OpCost[out$bev == 1]
ice_oc  <- out$OpCost[out$icev_nest_indicator == 1]
cat(sprintf("  BEV OpCost: median=%.2f c/mi, range=[%.2f, %.2f] (expect 3-6)\n",
            median(bev_oc), min(bev_oc), max(bev_oc)))
cat(sprintf("  ICE OpCost: median=%.2f c/mi, range=[%.2f, %.2f] (expect 8-15)\n",
            median(ice_oc), min(ice_oc), max(ice_oc)))

brand_sums <- out$american + out$chinese + out$japanese + out$skorean
cat(sprintf("  Brand dummies sum: max=%d (expect <=1)\n", max(brand_sums)))
if (max(brand_sums) > 1) warning("Some rows have multiple brand flags set.")

nest_sums <- out$icev_nest_indicator + out$hev_nest_indicator +
             out$phev_nest_indicator + out$bev_nest_indicator
cat(sprintf("  Nest indicators sum: min=%d, max=%d (expect exactly 1)\n",
            min(nest_sums), max(nest_sums)))
if (any(nest_sums != 1)) {
  n_bad <- sum(nest_sums != 1)
  warning(sprintf("%d rows do not have exactly one nest indicator set.", n_bad))
  bad_classes <- unique(df$fueling_class[nest_sums != 1])
  cat(sprintf("  Unmatched fueling_class values: %s\n",
              paste(bad_classes, collapse = ", ")))
}

cat(sprintf("  Acceleration: median=%.2f s, range=[%.2f, %.2f]\n",
            median(out$Acceleration), min(out$Acceleration), max(out$Acceleration)))

cat("\nDone.\n")

# One-time filter: extract MY2030 cars from the full EPA OMEGA vehicles output.
# The full file is ~114 MB (too big for GitHub); every downstream script filters
# to model_year == 2030 & reg_class_id == "car", so we pre-filter once and
# commit the small subset.
#
# Run from repo root: Rscript epa_data_scripts/filter_epa_vehicles.R

EPA_DIR <- "inputs/EPA_Final-Rule_2024"
SRC <- file.path(EPA_DIR, "2024_02_27_16_06_31_LDV_central_to2055_20240227_Final_vehicles.csv")
DST <- file.path(EPA_DIR, "LDV_central_my2030_cars_vehicles.csv")

df <- read.csv(SRC, stringsAsFactors = FALSE)
cat(sprintf("Loaded %d rows from full vehicles file.\n", nrow(df)))

df <- df[df$model_year == 2030 & df$reg_class_id == "car", ]
cat(sprintf("After MY2030 car filter: %d rows.\n", nrow(df)))

write.csv(df, DST, row.names = FALSE)
cat(sprintf("Wrote %s\n", DST))

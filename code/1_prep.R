
# Prepare data, formulas, etc, then source functions to run CEM and fit models.
# Produces:
#   - tbl_raw, tbl
#   - out_cem (out_imb)
#   - out_lm1, (out_glm), (out_tob), (out_lasso, cv_lasso)

# Dependencies ---

source("code/0_prelim.R")

# Settings ---

source("code/a_formulas.R")

STORED_CEM <- FALSE # if TRUE load existing matches
CALC_IMBALANCE <- FALSE # if TRUE calculate imbalance in out_imb
SCALE_CENTER <- FALSE # if TRUE scale and center explanatories
SAVE_MDL <- TRUE # whether to save model outputs
SAVE_MAT <- TRUE # whether to save X and y matrices
CALC_LASSO <- FALSE
CALC_TOBIT <- FALSE # may fail annoyingly due to singular VCOV (ECU)
CALC_LOGIT <- FALSE # demands 'geom = TRUE' in prep_data()

match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
  "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions")

# Run ---

# file <- files[grep("NIC", files)]
for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  # Data
  source("code/2_data.R")

  # CEM
  source("code/3_cem.R")

  # Estimate
  source("code/4_models.R")

}

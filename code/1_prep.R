
# Prepare data, formulas, etc, then source functions to run CEM and fit models.

# Dependencies -----

source("code/0_prelim.R")

source("code/9_formulas.R")


# Settings ---

STORED_CEM <- TRUE # if TRUE load existing matches
CALC_IMBALANCE <- FALSE # if TRUE calculate imbalance in out_imb
CALC_LASSO <- FALSE
SCALE_CENTER <- FALSE
CALC_TOBIT <- FALSE # may fail annoyingly due to singular VCOV (ECU)
CALC_LOGIT <- FALSE # demands 'geom = TRUE' in prep_data()


# Run ---

# file <- files[grep("NIC", files)]
for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE,
    sub_eco = "Tropical", geom = TRUE)

  # Add variables
  tbl <- add_vars(tbl,
    treated = c(-1, 5e4),
    dist_log = TRUE,
    dist_bool = TRUE, # _bool at 1km, then suffixed
    dist_decay = 0.5,
    dist_mine_dummy = TRUE)

  # CEM
  match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
    "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions")

  source("code/2_cem.R")

  # Estimate
  source("code/3_models.R")

}


# Prepare data, formulas, etc, then source functions to run CEM and fit models.

# Dependencies -----

library("dplyr")
library("sf")

source("code/9_helpers.R")
countries <- read.csv("input/countries.csv")

# Prepare data -----

# Cloud, home, or cluster?
path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
if(!dir.exists(path_in)) {
  path_in <- "data/"
  if(!dir.exists(path_in)) {stop("Feed me data!")}
}

# files <- list.files(path_in)
files <- paste0(countries$continent, "-", countries$iso, ".rds")
files <- files[grep("AGO|BRA|COL|COD|CIV|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MOZ|MYS|NIC|PNG|SLE|TZA|VEN|VNM|ZMB", files)] # No ECU cause it crashes
# files <- files[grep("BRA", files)]

# Store CEM outputs
STORE_CEM <- TRUE
CALC_LASSO <- FALSE
SCALE_CENTER <- FALSE
CALC_TOBIT <- TRUE # May fail annoyingly due to singular VCOV (ECU)

file <- files[grep("IDN", files)]
for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE,
    sub_eco = "Tropical", geom = FALSE)

  # Add variables
  tbl <- add_vars(tbl,
    treated = c(-1, 5e4),
    dist_log = TRUE,
    dist_bool = TRUE, # _bool at 1km, then suffixed
    dist_decay = 0.5)

  # CEM
  match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
    "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions_2017")

  source("code/2_cem.R")

  # Models
  source("code/9_formulas.R")

  source("code/3_models.R")

}

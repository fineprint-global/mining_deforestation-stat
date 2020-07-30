
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
# files <- c("south_america-BRA.rds", "oceania-IDN.rds", "oceania-MYS.rds")
# files <- c("south_america-BRA.rds", "oceania-IDN.rds", "oceania-MYS.rds",
#   "africa-AGO.rds", "south_america-COL.rds", "asia-LAO.rds", "asia-VNM.rds",
#   "south_america-SUR.rds")


file <- files[[2]]
# file <- files[grep("NIC", files)]

# Store CEM outputs
STORE_CEM <- TRUE
CALC_LASSO <- TRUE
SCALE_CENTER <- TRUE

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
  formulas <- list(
    "f_base" = area_accumulated_forest_loss_log ~
      distance_mine +
      elevation + slope +
      pop_2000 + area_forest_2000 +
      dist_road +
      dist_waterway +
      distance_protected_area +
      distance_cropland_2000 +
      soilgrid_grouped + esa_cci_2000,
    "f_base_log" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log +
      dist_waterway_log +
      distance_protected_area_log +
      distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
    "f_base_log_bool" = area_accumulated_forest_loss_log ~
      distance_mine_bool + distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_bool + dist_road_log +
      dist_waterway_bool + dist_waterway_log +
      distance_protected_area_bool + distance_protected_area_log +
      distance_cropland_2000_bool + distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
    "f_no_road_log" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_waterway_log +
      distance_protected_area_log +
      distance_cropland_2000_log +
    "f_no_pop_log" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope +
      area_forest_2000_log +
      dist_road_log +
      dist_waterway_log +
      distance_protected_area_log +
      distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
    "f_no_pa_log" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log +
      dist_waterway_log +
      distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
    "f_no_esa_log" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log +
      dist_waterway_log +
      distance_protected_area_log +
      distance_cropland_2000_log +
      soilgrid_grouped,
    "f_base_log_interactions" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      dist_road_log + I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
      dist_waterway_log +
      distance_protected_area_log + I(dist_protected_area_log * distance_mine_log) +
      distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000
  )

  source("code/3_models.R")

}

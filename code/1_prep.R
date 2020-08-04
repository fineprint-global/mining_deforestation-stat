
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
# files <- paste0(countries$continent, "-", countries$iso, ".rds")
files <- c("south_america-BRA.rds", "oceania-IDN.rds", "oceania-MYS.rds")

file <- files[[2]]
# file <- files[grep("LBR", files)]

# Store CEM outputs
STORE_CEM <- TRUE
CALC_LASSO <- FALSE
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
    "f_base_bool" = area_accumulated_forest_loss_log ~
      distance_mine_bool + distance_mine +
      elevation + slope +
      pop_2000 + area_forest_2000 +
      dist_road_bool + dist_road +
      dist_waterway_bool + dist_waterway +
      distance_protected_area_bool + distance_protected_area +
      distance_cropland_2000_bool + distance_cropland_2000 +
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
    "f_base_bool_log" = area_accumulated_forest_loss_log ~
      distance_mine_bool + distance_mine_log +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_bool + dist_road_log +
      dist_waterway_bool + dist_waterway_log +
      distance_protected_area_bool + distance_protected_area_log +
      distance_cropland_2000_bool + distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
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
    "f_esa_group" = area_accumulated_forest_loss_log ~
      distance_mine +
      elevation + slope +
      pop_2000 + area_forest_2000 +
      dist_road +
      dist_waterway +
      distance_protected_area +
      distance_cropland_2000 +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_base_decay" = area_accumulated_forest_loss_log ~
      distance_mine_decay +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_decay +
      dist_waterway_decay +
      distance_protected_area_decay +
      distance_cropland_2000_decay +
      soilgrid_grouped + esa_cci_2000,
    "f_base_log_interactions" = area_accumulated_forest_loss_log ~
      distance_mine_log +
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      dist_road_log + I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
      dist_waterway_log +
      distance_protected_area_log +
      distance_cropland_2000_log +
      soilgrid_grouped + esa_cci_2000,
    "f_base_decay_interactions" = area_accumulated_forest_loss_log ~
      distance_mine_decay +
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      dist_road_decay + I(dist_road_decay * pop_2000_log) + I(dist_road_decay * distance_cropland_2000_decay) +
      dist_waterway_decay +
      distance_protected_area_decay +
      distance_cropland_2000_decay +
      soilgrid_grouped + esa_cci_2000,
    "f_base_log_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
      soilgrid_grouped + esa_cci_2000,
    "f_base_bool_log_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_bool + distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_bool + dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
      dist_waterway_bool + dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
      distance_protected_area_bool + distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
      distance_cropland_2000_bool + distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
      soilgrid_grouped + esa_cci_2000,
    "f_base_log_interactions_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
      dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
      soilgrid_grouped + esa_cci_2000,
    "f_sub_5_20_log_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20) +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km20)+
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km20)+
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km20)+
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) +  I(distance_cropland_2000_log * distance_cropland_2000_km20)+
      soilgrid_grouped + esa_cci_2000,
    "f_sub_5_20_log_interactions_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20)+
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
      dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km20)+
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) +  I(dist_waterway_log * dist_waterway_km20)+
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) +  I(distance_protected_area_log * distance_protected_area_km20)+
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5)  + I(distance_cropland_2000_log * distance_cropland_2000_km20)+
      soilgrid_grouped + esa_cci_2000,
    "f_proposal_log_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20) +
      elevation + slope +
      pop_2000_log + area_forest_2000_log +
      dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km50)+ 
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km10) + 
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km20) + 
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
      soilgrid_grouped + esa_cci_2000,
    "f_proposal_log_interactions_nonlins" = area_accumulated_forest_loss_log ~
      distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20)+
      elevation + slope + I(elevation * slope) +
      pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
      I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) + 
      dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km50)+ 
      dist_waterway_log + I(dist_waterway_log * dist_waterway_km10) + 
      distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km20) + 
      distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km10)  +
      soilgrid_grouped + esa_cci_2000
    
  )

  source("code/3_models.R")

}

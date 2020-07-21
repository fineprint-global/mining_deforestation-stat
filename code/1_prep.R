
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
# files <- c("africa-GHA.rds", "central_america-NIC.rds", "africa-ZMB.rds")

# file <- files[[1]]
file <- files[grep("MYS", files)]



for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE,
    sub_eco = "Tropical", geom = FALSE)

  # Add variables
  tbl <- add_vars(tbl,
    treated = c(-1, 5e4),
    dist_log = TRUE, dist_bool = 1e3, dist_decay = 0.5)

  # CEM
  # match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
  #   "dist_waterway", "soilgrid_grouped", "esa_cci_2000_grouped")
  match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
    "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions_2017")
  source("code/2_cem.R")
  
  # Analyse matching
  # Add SEL R-Scripts (only the useful ones)  

  # Models
  formulas <- list(
    "f_base" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine * treated) +
      elevation + slope +
      pop_2000 + area_forest_2000 +
      dist_road +
      dist_waterway +
      distance_protected_area +
      distance_cropland_2000 +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_highway" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      distance_highway_motorway + I(distance_mine * distance_highway_motorway) + I(dist_waterway * distance_highway_motorway) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * distance_highway_motorway) +
      distance_cropland_2000 + I(distance_cropland_2000 * distance_highway_motorway) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_quad" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine^2) + I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_cub" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine^2) + I(distance_mine^3) + I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_more_quad" = area_accumulated_forest_loss ~ # still missing are: sub-national dummies
      on_mine + distance_mine + I(distance_mine^2) + I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +


  source("code/3_models.R")
  

}


# Create functions for organized summary stats of regressions
compare_models(path = "output/txt", 
               files = c("coef_NIC_f_more_quad.csv", "coef_ZMB_f_more_quad.csv", "coef_GHA_f_more_quad.csv", "coef_BRA_f_more_quad.csv"),
               coef_subs = c("on_mine", "distance_mine", "I(distance_mine^2)", "I(distance_mine * treated)"))

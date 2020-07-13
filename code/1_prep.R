
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

# file <- files[[1]]
file <- files[grep("ECU", files)]


for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE,
    sub_eco = "Tropical", geom = FALSE)

  # CEM
  tbl$treated <- calc_treatment(tbl,
    dist_treated = c(-1, 5e4), dist_control = 5e4)
  tbl$treated_far <- calc_treatment(tbl,
    dist_treated = c(-1, 1e5), dist_control = 1e5)
  tbl$treated_farer <- calc_treatment(tbl,
    dist_treated = c(-1, 2e5), dist_control = 2e5)

  # match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
  #   "dist_waterway", "soilgrid_grouped", "esa_cci_2000_grouped")
  match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
    "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions_2017")
  source("code/2_cem.R")

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
      distance_protected_area + I(distace_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped)

  source("code/3_models.R")

}

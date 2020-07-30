
# Prepare data, formulas, etc, then source functions to run CEM and fit models.

# Dependencies -----

library("dplyr")
library("sf")
library("cobalt") # for cem analysis

source("code/9_helpers.R")
countries <- read.csv("input/countries.csv")

# Prepare data -----

# select countries we want to cover
data_sm <- read.csv("output/country_data-summary-max.csv")
dat_sum <- data_sm %>% as.data.frame() %>% 
  dplyr::mutate(index = rep(seq(1, 6, 1), nrow(data_sm)/6)) %>%
  dplyr::filter(index == 6) %>%
  dplyr::mutate(area_forest_2000  = as.numeric(as.character(area_forest_2000))  / 1000000) %>%
  dplyr::mutate(area_accumulated_forest_loss =  as.numeric(as.character(area_accumulated_forest_loss)) / 1000000) %>%
  dplyr::mutate(relative_forest_loss = area_accumulated_forest_loss / area_forest_2000)
top_rff <- dat_sum %>% dplyr::arrange(-relative_forest_loss)
top_rff <- as.character(top_rff$iso[1:26]) # top 26 have more than 5% loss
countries <- countries %>% dplyr::filter(iso %in% top_rff)

# Cloud, home, or cluster?
path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
if(!dir.exists(path_in)) {
  path_in <- "data/"
  if(!dir.exists(path_in)) {stop("Feed me data!")}
}

files <- paste0(countries$continent, "-", countries$iso, ".rds")
# file <- files[grep("NIC", files)]


for(file in files) {
  
  cat("Running for ", get_iso(file), ".\n", sep = "")
  
  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE,
                   sub_eco = "Tropical", geom = FALSE)
  
  # Add variables
  tbl <- add_vars(tbl,
                  treated = c(-1, 2e4), # switch to 20km
                  dist_log = TRUE, dist_bool = 0, dist_decay = NULL)
  
  # Cut to 100km max dist from mine
  tbl <- tbl %>% dplyr::filter(distance_mine <= 50000)
  
  # CEM
  # match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
  #   "dist_waterway", "soilgrid_grouped", "esa_cci_2000_grouped")
  match_on <- c("elevation", "slope", "area_forest_2000", "pop_2000",
                "dist_waterway", "soilgrid_grouped", "esa_cci_2000", "ecoregions_2017")
  source("code/2_cem.R")
  
  # Analyse data: fit splines distance mine vs forest loss
  data <- tbl
  xs <- seq(0, max(data[["distance_mine"]]), length.out = 1000)
  
  out_spl <- lm(area_accumulated_forest_loss ~
                  bs(distance_mine), data = data, weights = out_cem[["w"]])
  smpl <- sample(nrow(data), min(10000, nrow(data) / 4))
  
  pred <- predict(out_spl, newdata = list(distance_mine = xs))
  png(paste0("output/plots/spline_matched_50km/",
             sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
  plot(x = data[["distance_mine"]][smpl],
       y = data[["area_accumulated_forest_loss"]][smpl],
       col = "grey", xlab = "Distance to mine", ylab = "Forest loss",
       main = paste0("Spline matched - ", gsub(".*([A-Z]{3}).rds", "\\1", file)),
       ylim = c(0, min(max(pred) * 5, max(data[["area_accumulated_forest_loss"]]))))
  points(xs, pred, col = "darkgreen", lwd = 2, type = "l")
  grid()
  dev.off()
  rm(data, out_spl); gc()
  
  # Models
  formulas <- list(
    "f_base" = area_accumulated_forest_loss ~
      distance_mine + #I(distance_mine * treated) +
      elevation + slope +
      pop_2000 + area_forest_2000 +
      dist_road +
      dist_waterway +
      distance_protected_area +
      distance_cropland_2000 +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions" = area_accumulated_forest_loss ~
      distance_mine + #I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    # "f_interactions_highway" = area_accumulated_forest_loss ~
    #   distance_mine + #I(distance_mine * treated) +
    #   elevation + slope + I(elevation * slope) +
    #   pop_2000 + area_forest_2000 +
    #   distance_highway_motorway + I(distance_mine * distance_highway_motorway) + I(dist_waterway * distance_highway_motorway) +
    #   dist_waterway +
    #   distance_protected_area + I(distance_protected_area * distance_highway_motorway) +
    #   distance_cropland_2000 + I(distance_cropland_2000 * distance_highway_motorway) +
    #   soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_quad" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine^2) + #I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_logdm" = area_accumulated_forest_loss ~
      distance_mine_log + #I(distance_mine^2) + #I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + #I(distance_mine_log * dist_road) + 
      I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_quad_logdm" = area_accumulated_forest_loss ~
      distance_mine_log + I(distance_mine_log^2) + #I(distance_mine_log * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine_log * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_cub" = area_accumulated_forest_loss ~
      distance_mine + I(distance_mine^2) + I(distance_mine^3) + #I(distance_mine * treated) +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road + I(distance_mine * dist_road) + I(dist_waterway * dist_road) +
      dist_waterway +
      distance_protected_area + I(distance_protected_area * dist_road) +
      distance_cropland_2000 + I(distance_cropland_2000 * dist_road) +
      soilgrid_grouped + esa_cci_2000_grouped,
    "f_interactions_logalldist" = area_accumulated_forest_loss ~
      distance_mine_log +
      elevation + slope + I(elevation * slope) +
      pop_2000 + area_forest_2000 +
      dist_road_log + I(distance_mine_log * dist_road_log) + I(dist_waterway_log * dist_road_log) +
      dist_waterway_log +
      distance_protected_area_log + I(distance_protected_area_log * dist_road_log) +
      distance_cropland_2000_log + I(distance_cropland_2000_log * dist_road_log) +
      soilgrid_grouped + esa_cci_2000_grouped
  )
  
  source("code/3_models.R")
  
  
}

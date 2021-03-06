
library("sf")
library("dplyr")

# Used to label factorised country column
cntry <- data.table::fread("/gpfs/home/home/vmaus/data/geoserver/fineprint_grid_30sec/countries_concordance.csv")


# Merge rasters to continents ---------------------------------------------

# Prep the regions' extent (S, N, E, W)
coords <- list(
  "south_america" = c(-50, 20, 40, 90),
  "central_america" = c(10, 40, 70, 130),
  "africa" = c(-30, 40, -50, 20),
  "asia" = c(10, 40, -130, -60),
  "oceania" = c(-20, 20, -170, -80)
)


repl_coord <- function(x, coord = c("S", "N"), width = 2) {
  x1 <- if(any(x < 0)) {
    gsub("^-([0-9]+)", paste0("\\1", coord[1]),
         formatC(x[x < 0], width = width + 1, flag = 0))} else {NULL}
  x2 <- if(any(x > 0)) {
    gsub("([0-9]+)", paste0("\\1", coord[2]),
         formatC(x[x > 0], width = width, flag = 0))} else {NULL}
  x3 <- if(any(x == 0)) {
    gsub("([0-9]+)", paste0("\\1", if("N" %in% coord) "N" else "E"),
      formatC(x[x == 0], width = width, flag = 0))} else {NULL}
  c(x1, x3, x2)
}


# Create a list of vectors with all applicable RDS files
files <- lapply(coords, function(x) {
  ns <- repl_coord(seq(x[1], x[2], 10L), c("S", "N"), 2)
  ew <- repl_coord(seq(x[3], x[4], 10L), c("E", "W"), 3)
  paste0(ns, rep(ew, each = length(ns)))
})

# Create merged (via `rbind`) files per region

# for(i in seq_along(files)) {
i <- job_id

rds <- sapply(files[[i]], function(x) {
  file <- paste0(data, x, ".rds")
  if(file.exists(file)) {
    out <- readRDS(file)
    # file.remove(file)
  } else {out <- NULL}
  return(out)
}, simplify = FALSE)

tbl <- do.call(rbind, rds)
rm(rds)

# To-do: lwgeom crashes
# Watch out for variable conversion to character
# area <- tbl %>%
#   mutate(area = sf::st_area(tbl)) %>%
#   .$area

tbl <- tbl %>%
  mutate(
    countries = factor(countries, levels = cntry[[3]], labels = cntry[[1]]),
    elevation = as.numeric(elevation),
    slope = as.numeric(slope),
    soilgrid = as.numeric(soilgrid),
    esa_cci_2000 = as.numeric(esa_cci_2000),
    pop_2000 = as.numeric(pop_2000),
    distance_waterway_canal = as.numeric(distance_waterway_canal),
    distance_waterway_river = as.numeric(distance_waterway_river),
    distance_sea = as.numeric(distance_sea),
    distance_highway_primary = as.numeric(distance_highway_primary),
    distance_highway_motorway = as.numeric(distance_highway_motorway),
    distance_highway_secondary = as.numeric(distance_highway_secondary),
    distance_highway_trunk = as.numeric(distance_highway_trunk),
    distance_mine = as.numeric(distance_mine))
tbl <- tbl %>%
  mutate(
    min_area_5arcmin = as.numeric(min_area_5arcmin),
    min_area_30arcmin = as.numeric(min_area_30arcmin),
    min_area_1degree = as.numeric(min_area_1degree),
    min_area_10degree = as.numeric(min_area_10degree),
    distance_protected_area = as.numeric(distance_protected_area),
    contiguous_land = as.numeric(contiguous_land),
    accessibility_cities_2015 = as.numeric(accessibility_cities_2015),
    area_forest_2000 = as.numeric(area_forest_2000),
    area_accumulated_forest_loss_2019 = as.numeric(area_accumulated_forest_loss_2019),
    area_accumulated_forest_loss_2010 = as.numeric(area_accumulated_forest_loss_2010),
    area_forest_2000_mine_lease = as.numeric(area_forest_2000_mine_lease),
    area_accumulated_loss_mine_lease_2019 = as.numeric(area_accumulated_loss_mine_lease_2019),
    area_accumulated_loss_mine_lease_2010 = as.numeric(area_accumulated_loss_mine_lease_2010),
    distance_cropland_2000 = as.numeric(distance_cropland_2000),
    ecoregions = as.numeric(ecoregions),
    # area = as.numeric(area),
    area_mine = as.numeric(area_mine)
  )

saveRDS(tbl, paste0(data, names(files)[i], ".rds"))
rm(tbl)
gc()
# }

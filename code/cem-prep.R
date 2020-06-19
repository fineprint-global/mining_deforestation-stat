
library("dplyr")
library("sf")
library("cem")

path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"

prep_country <- function(x, geom = FALSE) {
  if(!isTRUE(geom)) {x$geometry <- NULL}
  # Subset grids to ones with forest
  x <- x[x$area_forest_2000 > 0, ]
  # Fix accessibility to cities / kick errors
  x$accessibility_cities_2015[x$accessibility_cities_2015 < 0] <- NA
  x <- x[!is.na(x$accessibility_cities_2015), ]
  # Create distance to road and waterway value
  x <- x %>%
    dplyr::mutate(dist_road =
      pmin(distance_highway_motorway, distance_highway_primary,
        # distance_highway_secondary,
        na.rm = TRUE)) %>%
    dplyr::mutate(dist_waterway =
      pmin(distance_waterway_canal, distance_waterway_river, na.rm = TRUE))
  # Fix factors
  x <- x %>%
    dplyr::mutate(soilgrid = as.factor(soilgrid),
      esa_cci_2000 = as.factor(esa_cci_2000))
  return(x)
}

treat_country <- function(x, dist_treated, dist_control) {
  # Create treatment vector
  treated <- ifelse(x$distance_mine >= dist_treated[1] &
    x$distance_mine <= dist_treated[2], TRUE, NA)
  treated <- ifelse(x$distance_mine >= dist_control, FALSE, treated)
  return(treated)
}

files <- list.files(path_in)
# file <- files[[1]]
file <- files[grep("asia-VNM", files)]

x <- readRDS(paste0(path_in, file))
y <- prep_country(x)
y$treated <- treat_country(y, c(-1, 5e4), dist_control = 5e4)

plot(y[sample(nrow(y), 10000), ], max.plot = 1)

imb <- imbalance(y$treated, as.data.frame(y),
  drop = c("countries", "id_grid"))

out1 <- cem("treated", y,
  drop = names(y)[!grepl("elevation|slope|area_forest_2000|dist_road|pop_2000|dist_waterway", names(y))],
  keep.all = TRUE)

lm1 <- att(out1, area_accumulated_forest_loss ~
  poly(distance_mine, 2) + slope + elevation + area_forest_2000 + pop_2000,
  data = as.data.frame(y))
summary(lm1)

pair1 <- pair(out1, data = y)
out_r <- relax.cem(out1, data = as.data.frame(y))


lm2 <- lm(area_accumulated_forest_loss ~
  poly(distance_mine, 3) + slope + elevation + area_forest_2000 + pop_2000,
  data = as.data.frame(y), weights = out1$w)
summary(lm2)

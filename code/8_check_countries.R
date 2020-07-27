
# Summarise information about countries of interest

library("dplyr")
library("sf")

countries <- read.csv("input/countries.csv")

# Cloud, home, or cluster?
path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
if(!dir.exists(path_in)) {
  path_in <- "data/"
  if(!dir.exists(path_in)) {stop("Feed me data!")}
}

# files <- list.files(path_in)
files <- paste0(countries$continent, "-", countries$iso, ".rds")


# Summarise data -----

data_sm <- lapply(files, function(file) {
  y <- readRDS(paste0(path_in, file))
  y$geometry <- NULL
  cbind(
    "iso" = gsub(".*([A-Z]{3}).rds", "\\1", file),
    sapply(y[, c("distance_mine", "area_forest_2000",
    "area_accumulated_forest_loss", "elevation", "slope", "pop_2000")],
    function(x) {
      c("min" = min(x, na.rm = TRUE), quantile(x, 0.2, na.rm = TRUE),
        "mean" = mean(x, na.rm = TRUE), quantile(x, 0.8, na.rm = TRUE),
        "max" = max(x, na.rm = TRUE, "NAs" = sum(is.na(x))))
    })
  )
})
data_sm <- do.call(rbind, data_sm)

write.csv(data_sm, "output/country_data-summary.csv")


# Plot countries -----

for(file in files) {
  x <- readRDS(paste0(path_in, file))
  png(paste0("output/plots/draw_", sub(".*([A-Z]{3}).rds", "\\1", file), ".png"),
    width = 960, height = 960)
  x[sample(nrow(x), min(nrow(x) / 4, 100000)), ] %>%
    select(distance_mine) %>% plot()
  dev.off()
}


# Sort by forest loss -----------------------------------------------------

# summarise data
data_sm <- lapply(files, function(file) {
  y <- readRDS(paste0(path_in, file))
  y$geometry <- NULL
  cbind(
    "iso" = gsub(".*([A-Z]{3}).rds", "\\1", file),
    sapply(y[, c("area_forest_2000", "area_accumulated_forest_loss", "pop_2000")],
           function(x) {
             c("min" = min(x, na.rm = TRUE), quantile(x, 0.2, na.rm = TRUE),
               "mean" = mean(x, na.rm = TRUE), quantile(x, 0.8, na.rm = TRUE),
               "max" = max(x, na.rm = TRUE, "NAs" = sum(is.na(x))), 
               "sum" = sum(x, na.rm = TRUE))
           })
  )
})
data_sm <- do.call(rbind, data_sm)
# POPULATION VALUES DO NOT SUM UP EXACTLY TO REAL VALUES

write.csv(data_sm, "output/country_data-summary-max.csv")

# filter sum
dat_sum <- data_sm %>% as.data.frame() %>% 
  dplyr::mutate(index = rep(seq(1, 6, 1), length(files))) %>%
  dplyr::filter(index == 6) %>%
  dplyr::mutate(area_forest_2000  = as.numeric(as.character(area_forest_2000))  / 1000000) %>%
  dplyr::mutate(area_accumulated_forest_loss =  as.numeric(as.character(area_accumulated_forest_loss)) / 1000000) %>%
  dplyr::mutate(relative_forest_loss = area_accumulated_forest_loss / area_forest_2000)
  

# total forest loss area
dat_sum %>% dplyr::arrange(-area_accumulated_forest_loss)

# forest loss relative to forest 2000
top_16_rff <- dat_sum %>% dplyr::arrange(-relative_forest_loss)
top_16_rff <- as.character(top_16_rff$iso[1:16])


# Check countries to cover ------------------------------------------------

data_sm <- read.csv("output/country_data-summary-max.csv")

dat_sum <- data_sm %>% as.data.frame() %>%
  dplyr::mutate(index = rep(seq(1, 6, 1), nrow(data_sm)/6)) %>%
  dplyr::filter(index == 6) %>%
  dplyr::mutate(area_forest_2000  = as.numeric(as.character(area_forest_2000))  / 1000000) %>%
  dplyr::mutate(area_accumulated_forest_loss =  as.numeric(as.character(area_accumulated_forest_loss)) / 1000000) %>%
  dplyr::mutate(relative_forest_loss = area_accumulated_forest_loss / area_forest_2000)
top_rff <- dat_sum %>% dplyr::arrange(-relative_forest_loss)
# top_rff <- as.character(top_rff$iso[1:26]) # top 26 have more than 5% loss
top_rff <- c("BRA", "IDN", "MYS")
countries <- countries %>% dplyr::filter(iso %in% top_rff)

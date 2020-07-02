
library("dplyr")
library("sf")

countries <- read.csv("input/countries.csv")

path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
if(!dir.exists(path_in)) {
  path_in <- "data/"
  if(!dir.exists(path_in)) {stop("Feed me data!")}
}

files <- paste0(path_in, countries$continent, "-", countries$iso, ".rds")

# Summarise the data -----

data_sm <- lapply(files, function(x) {
  y <- readRDS(x)
  y$geometry <- NULL
  cbind(
    "iso" = gsub(".*([A-Z]{3}).rds", "\\1", x),
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

# Plot the countries

for(file in files) {
  x <- readRDS(file)
  png(paste0("output/plots/draw_", sub(".*([A-Z]{3}).rds", "\\1", file), ".png"),
    width = 960, height = 960)
  x[sample(nrow(x), min(nrow(x) / 4, 100000)), ] %>% 
    select(distance_mine) %>% plot()
  dev.off()
}

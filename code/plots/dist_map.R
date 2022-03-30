
library("ggplot2")
library("sf")

files <- c("BRA", "COD", "IDN", "GHA")
file <- files[4]

for(file in files) {

# Load ---

source("code/0_prelim.R")

if(file.exists(paste0("output/plots/map_coefs_data_", file, ".rds"))) {
  x <- readRDS(paste0("output/plots/map_coefs_data_", file, ".rds"))
} else {
  source("code/2_data.R")
  file <- get_iso(file)
  x <- tbl_raw[tbl_raw$area_accumulated_forest_loss_2019 > 0, ]
  x <- x %>% sf:::select.sf(distance = distance_mine)
  x %>% saveRDS(paste0("output/plots/map_coefs_data_", file, ".rds"))
}

# Color
x$Distance <- cut(-x$distance, 
  -c(0, 2500, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000,
    45000, seq(50000, 150000, by = 25000), 200000, 250000, Inf))
  # -c(0, 1000, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 30000, 35000, 40000,
    # 45000, 50000, 60000, 70000, 80000, 90000, 100000, Inf)) # km
cols <- colorspace::sequential_hcl(
  n = length(levels(x$Distance)) + 2, rev = TRUE, 
  h = c(-100, 85), c = c(0, 100, 10), l = c(1, 99), power = c(1.1, 0.9))
cols <- cols[-1:-2]
names(cols) <- levels(x$Distance)

# Crop for testing ---

y <- switch(file,
  BRA = st_crop(x, xmin = -45, xmax = 40, ymin = -22.5, ymax = -17.5),
  IDN = st_crop(x, xmin = 110, xmax = 115, ymin = -5, ymax = 0),
  COD = st_crop(x, xmin = 25, xmax = 30, ymin = -15, ymax = -10),
  GHA = st_crop(x, xmin = -4, xmax = 1, ymin = 4, ymax = 9))

# Plot ---
p <- ggplot(y) +
  geom_sf(aes(fill = Distance), lwd = 0) +
  scale_fill_manual(values = (cols), na.value = "white") +
  switch(file, 
    BRA = coord_sf(xlim = c(-60, -20), ylim = c(-40, -10), expand = FALSE),
    IDN = coord_sf(xlim = c(90, 130), ylim = c(-20, 10), expand = FALSE),
    COD = coord_sf(xlim = c(0, 40), ylim = c(-20, 10), expand = FALSE),
    GHA = coord_sf(xlim = c(-4, 1), ylim = c(4, 9), expand = FALSE)) +
  labs(fill = "Effect") +
  ggtitle(paste0("Forest loss ~ mine distance (", file, ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

saveRDS(p, file = paste0("output/plots/map_coefs_", file, ".rds"))

ggsave(paste0("output/plots/map_coefs_", file, ".png"),
  width = 10, height = 10)
gc()
}

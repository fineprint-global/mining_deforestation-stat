
library("dplyr")
library("raster")
library("rasterVis")

path <- "/mnt/nfs_fineprint/data/geoserver/fineprint_grid_30sec/"
list.files(path)

mains <- c("distance_highway_motorway" = "Distance to Motorway",
  "distance_highway_primary" = "Distance to Primary Roads",
  "distance_highway_secondary" = "Distance to Secondary Roads",
  "distance_highway_trunk" = "Distance to Tertiary Roads",
  "distance_mine" = "Distance to Mines",
  "distance_waterway_river" = "Distance to Rivers",
  "population_density_2000" = "Population Density",
  "accessibility_to_cities_2015" = "Accessibility to Cities",
  "distance_protected_areas" = "Distance to Protected Areas",
  "elevation" = "Elevation", "slope" = "Slope")
files <- names(mains)


# Prep ecoregion subset
path_eco <- "/mnt/nfs_fineprint/data/geoserver/ecoregions/"

tropical_id <- sf::st_read(file.path(path_eco, "Ecoregions2017.shp")) %>%
  dplyr::filter(stringr::str_detect(BIOME_NAME, "Tropical")) %>%
  select(ECO_ID) %>% .$ECO_ID

ecoregions <- raster::raster(paste0(path, "ecoregions_2017.tif"))
eco_vals <- values(ecoregions)
# Use this logical to subset raster values
tropical_pos <- eco_vals %in% tropical_id
rm(eco_vals, ecoregions)


# Plots -------------------------------------------------------------------

# Roads ---
f<- files[1]
f2 <- files[2]
f3 <- files[3]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))
r2 <- raster::raster(paste0(path, paste0(f2, ".tif")))
r3 <- raster::raster(paste0(path, paste0(f3, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <-
  values(r2)[!tropical_pos] <- values(r3)[!tropical_pos] <- NA

# Get minimum value
values(r) <- pmin(values(r), values(r2), values(r3), na.rm = TRUE)
rm(f2, f3, r2, r3)

# Log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# Prepare coloring in map (manually)
map_col <- c(-5, seq(2, 10, 2), 15, 20)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  colorkey = list(labels = list(at = map_col, labels = map_col)),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 5)))
dev.off()
par(op)

gc()


# Mine ---
f <- files[5]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- NA

# Log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# prepare coloring in map (manually)
map_col <- c(-2, seq(4, 15, 1))

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  colorkey = list(labels = list(at = map_col, labels = map_col)),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 5)))
dev.off()
par(op)

gc()


# Accessibility ---
f <- files[8]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- NA

# Fix NAs
values(r)[values(r) == -9999] <- NA

# Log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 5)))
dev.off()
par(op)

gc()


# Slope & Elev ---
for(f in files[10:11]) {

  # Load raster file
  cat("Reading in ", f, ".\n", sep = "")
  r <- raster::raster(paste0(path, paste0(f, ".tif")))

  # Subset it to relevant ecoregions
  values(r)[!tropical_pos] <- NA

  # Fix negatives and log
  # values(r) <- log(values(r) + 10000)

  # Zoom into the tropical belt
  cat("Cropping ", f, ".\n", sep = "")
  r <- crop(r, extent(-120, 160, -40, 40))

  # Plot the raster file and export as PNG
  cat("Plotting ", f, ".\n", sep = "")

  op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
  png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
  print(rasterVis::levelplot(r, margin = FALSE,
    col.regions = viridis::viridis(1000, direction = 1),
    maxpixels = 1e6, xlab = NULL, ylab = NULL,
    main = list(label = mains[f], cex = 5)))
  dev.off()
  par(op)

  gc()
}


# Rivers ---
f <- files[6]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- NA

# Fix negatives and log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# prepare coloring in map (manually)
map_col <- c(-5, seq(3, 9, 2), 10, 11, 15)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  colorkey = list(labels = list(at = b, labels = b)),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 5)))
dev.off()
par(op)

gc()


# Protected Areas ---
f <- files[9]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- NA

# Fix negatives and log
values(r) <- log(values(r)+1)

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 5)))
dev.off()
par(op)

gc()


# Population Density ---
f <- files[7]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- NA

# Fix negatives and log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1800, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = 1),
  maxpixels = 1e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex=5)))
dev.off()
par(op)

gc()



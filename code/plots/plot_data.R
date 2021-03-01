
# Generate a global plot for each of our variables

library("dplyr")
library("raster")
library("rasterVis")

path <- "/mnt/nfs_fineprint/data/geoserver/fineprint_grid_30sec/"
list.files(path)

mains <- c("distance_highway_motorway" = "Proximity to Roads",
  "distance_highway_primary" = "Proximity to Primary Roads",
  "distance_highway_secondary" = "Proximity to Secondary Roads",
  "distance_highway_trunk" = "Proximity to Tertiary Roads",
  "distance_mine" = "Proximity to Mines",
  "distance_waterway_river" = "Proximity to Waterways",
  "population_density_2000" = "Population Density",
  "distance_cropland_2000" = "Proximity to Croplands",
  "distance_protected_areas" = "Proximity to Protected Areas",
  "elevation" = "Elevation", "slope" = "Slope")
files <- names(mains)


# Prep ecoregion subset
path_eco <- "/mnt/nfs_fineprint/data/geoserver/ecoregions/"

tropical_id <- sf::st_read(file.path(path_eco, "Ecoregions2017.shp")) %>%
  dplyr::filter(stringr::str_detect(BIOME_NAME, "Tropical")) %>%
  sf:::select.sf(ECO_ID) %>% .$ECO_ID

ecoregions <- raster::raster(paste0(path, "ecoregions_2017.tif"))
eco_vals <- values(ecoregions)
# Use this logical to subset raster values
tropical_pos <- eco_vals %in% tropical_id
rm(eco_vals, ecoregions)


# Plots -------------------------------------------------------------------

# Roads ---
f <- files[1]
f2 <- files[4]

# Load raster file
cat("Reading in ", f, ".\n", sep = "")
r <- raster::raster(paste0(path, paste0(f, ".tif")))
r2 <- raster::raster(paste0(path, paste0(f2, ".tif")))

# Subset it to relevant ecoregions
values(r)[!tropical_pos] <- values(r2)[!tropical_pos] <- NA

# Get minimum value
values(r) <- pmin(values(r), values(r2), na.rm = TRUE)
rm(f2, r2)

# Log
values(r) <- log(values(r))

# Zoom into the tropical belt
cat("Cropping ", f, ".\n", sep = "")
r <- crop(r, extent(-120, 160, -40, 40))

# Prepare coloring in map (manually)
map_col <- c(-Inf, 1, 3, 5, 6, seq(7, 13, 0.5), 15, Inf)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.2),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 3)))
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
map_col <- c(-Inf, 1, 3, 5, 6, seq(7, 13, 0.5), 15, Inf)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.5),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 2.5)))
dev.off()
par(op)

gc()


# Croplands ---
f <- files[8]

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
map_col <- c(-Inf, 1, 3, 5, 6, seq(7, 13, 0.5), 15, Inf)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.5),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 2.5)))
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

  map_col2 <- if(grepl("elev", f)) {
    map_col_elev <- c(-Inf, 0, 50, 100, 250, 400, 550, 700, 850, 1000, 2000, Inf)
  } else {
    map_col
  }

  op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
  png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
  print(rasterVis::levelplot(r, margin = FALSE,
    col.regions = viridis::viridis(1000, direction = -1),
    at = map_col2,
    par.settings = list(axis.line = list(col = "transparent"),
      strip.background = list(col = "transparent"),
      strip.border = list(col = "transparent")),
    scales = list(col = "black", cex = 1.5),
    colorkey = list(labels = list(at = map_col2, labels = map_col2, cex = 1.2)),
    maxpixels = 2e6, xlab = NULL, ylab = NULL,
    main = list(label = mains[f], cex = 2.5)))
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
map_col <- c(-Inf, 1, 3, 5, 6, seq(7, 13, 0.5), 15, Inf)

# Plot the raster file and export as PNG
cat("Plotting ", f, ".\n", sep = "")

op <- par(mar = c(2, 2, 2, 0.5), mfrow = c(1, 1))
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.5),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 2.5)))
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
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.5),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 2.5)))
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
png(paste0("output/plots/", f, ".png"), width = 1600, height = 600)
print(rasterVis::levelplot(r, margin = FALSE,
  col.regions = viridis::viridis(1000, direction = -1),
  at = map_col,
  par.settings = list(axis.line = list(col = "transparent"),
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent")),
  scales = list(col = "black", cex = 1.5),
  colorkey = list(labels = list(at = map_col, labels = map_col, cex = 1.2)),
  maxpixels = 2e6, xlab = NULL, ylab = NULL,
  main = list(label = mains[f], cex = 2.5)))
dev.off()
par(op)

gc()




files <- files[c(-2, -3, -4)]

library("png") # for reading in PNGs

# setup plot
par(mar = rep(0,4)) # no margins

# layout the plots into a matrix w/ 12 columns, by row
layout(matrix(1:8, ncol = 2, byrow=TRUE))

# do the plotting
for(i in 1:8) {
  img <- readPNG(paste0("output/plots/", files[i], ".png"))
  plot(NA, xlim = 0:1, ylim = 0:1, xaxt = "n", yaxt = "n", bty = "n",
    xaxs = 'i', yaxs = 'i')
  rasterImage(img, 0, 0, 1, 1)
}

# write to PDF
dev.print(png, "output/plots/variables.png", width = 2400, height = 1600)

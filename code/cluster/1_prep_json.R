
library("sf")


# Prepare GeoJSONs --------------------------------------------------------

path <- "/gpfs/home/home/vmaus/data/geoserver/fineprint_grid_30sec/grid_20201209_30"
positions <- list.files(path)

pos <- positions[[job_id]]

files <- list.files(paste0(path, pos))
files <- files[grepl("[.]geojson$", files)]

# Read
x <- do.call(rbind, sapply(files, function(x) {
  read_sf(paste0(path, pos, "/", x))
}, simplify = FALSE))

# Don't talk to me or my JSON ever again
saveRDS(x, paste0(data, pos, ".rds"))
rm(x); gc()

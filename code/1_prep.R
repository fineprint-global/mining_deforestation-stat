
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
file <- files[grep("sou.*ECU", files)]

for(file in files) {
  
  cat("Running for ", sub(".*([A-Z]{3}).rds", "\\1", file), ".\n", sep = "")
  
  tbl_raw <- readRDS(paste0(path_in, file))
  tbl <- prep_data(tbl_raw, has_forest = FALSE, geom = FALSE)
  
  tbl$treated <- calc_treatment(tbl,
    dist_treated = c(-1, 5e4), dist_control = 5e4)

  # Create outputs
  source("code/2_analyse.R")
  source("code/5_summarise.R")
}

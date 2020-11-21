
# Create and source prelminaries

library("dplyr")
library("sf")

countries <- read.csv("input/countries.csv")

source("code/9_helpers.R")


# Prepare data -----

# Cloud, home, or cluster?
path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
if(!dir.exists(path_in)) {
  path_in <- "data/"
  if(!dir.exists(path_in)) {stop("Feed me data!")}
}

# All files
# files <- list.files(path_in)
files <- paste0(countries$continent, "-", countries$iso, ".rds")

# Subset
file_subset <- "AGO|BRA|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MOZ|MYS|NIC|PNG|SLE|TZA|VEN|VNM|ZMB"
files <- files[grep(file_subset, files)]

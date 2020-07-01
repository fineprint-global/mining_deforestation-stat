
# Dependencies -----

library("dplyr")
library("sf")

source("code/9_helpers.R")

# Prepare data -----

# Cloud or cluster
# path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
path_in <- "data/"
files <- list.files(path_in)

# file <- files[[1]]
file <- files[grep("africa-NGA", files)]

tbl_raw <- readRDS(paste0(path_in, file))
tbl <- prep_data(tbl_raw)
tbl$treated <- calc_treatment(tbl,
  dist_treated = c(-1, 5e4), dist_control = 5e4)

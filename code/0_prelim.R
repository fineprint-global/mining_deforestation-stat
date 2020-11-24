
# Create and source preliminaries
# Set `file_subset` beforehand to subset countries manually


# Dependencies ---

library("dplyr")
library("sf")

countries <- read.csv("input/countries.csv")


# Functions ---

#' @title Helper to get ISO from a file-string.
#'
#' @param x Object with the string.
#'
#' @return Returns a character vector with just the ISO
get_iso <- function(x) {

  sub(".*([A-Z]{3}).*.rds", "\\1", x)
}

#' @title Get CEM info about the data
#'
#' @param x CEM object.
#'
#' @return Returns a dataframe with the group indicator and weight.
cem_data <- function(x) {

  if(!inherits(x, "cem.match")) stop("Provide a 'cem.match' object.")

  out <- data.frame(cem_treated = x[["groups"]], cem_weight = x[["w"]])

  return(out)
}


# Prepare files ---

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
if(!exists("file_subset")) {
  file_subset <- "AGO|BRA|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MEX|MOZ|MYS|NIC|PNG|SLE|TZA|VEN|VNM|ZMB"
}
files <- files[grep(file_subset, files)]

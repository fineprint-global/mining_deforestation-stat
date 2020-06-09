
library("dplyr")
library("sf")


# Isolate countries -------------------------------------------------------

separate_countries <- function(file, path, continent) {
  x <- readRDS(file)
  countries <- unique(x[["countries"]])
  x_list <- lapply(countries, function(y) {dplyr::filter(x, countries == y)})

  for(i in seq_along(x_list)) {
    saveRDS(x_list[[i]], paste0(path, continent, "-", countries[i], ".rds"))
  }
  NULL
}

path_in <- data
path_out <- paste0(data, "/countries/")
continents <- c("africa", "asia", "central_america", "oceania", "south_america")

# for(continent in continents) {
i <- job_id

separate_countries(
  file = paste0(path_in, continents[[i]], ".rds"),
  path = path_out,
  continent = continents[[i]]
)

# }

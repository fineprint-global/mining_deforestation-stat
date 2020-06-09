library("dplyr")
library("sf")
library("MatchIt")

prep_country <- function(x, soilgrids_grouped = NULL) {
  x$geometry <- NULL
  # Subset grids to ones with forest
  x <- x[x$area_forest_2000 > 0, ]
  # Fix accessibility to cities / kick errors
  x$accessibility_cities_2015[x$accessibility_cities_2015 < 0] <- NA
  x <- x[!is.na(x$accessibility_cities_2015), ]
  
  # Create distance to road and waterway value
  x <- x %>%
    dplyr::mutate(dist_road = pmin(distance_highway_motorway,
                                   distance_highway_primary,
                                   # distance_highway_secondary,
                                   na.rm = TRUE)) %>%
    dplyr::mutate(dist_waterway = pmin(distance_waterway_canal,
                                       distance_waterway_river, na.rm = TRUE))
  
  if(!is.null(soilgrids_grouped)) {
    x <- x %>% mutate(soilgrid = soilgrids_grouped[soilgrid, "WRB_group"])
  }
  
  return(x)
}


treat_country <- function(x, dist_treated, dist_control) {
  # Create treatment vector
  treated <- ifelse(x$distance_mine >= dist_treated[1] &
                      x$distance_mine <= dist_treated[2], TRUE, NA)
  treated <- ifelse(x$distance_mine >= dist_control, FALSE, treated)
  return(treated)
}


get_matchit <- function(x, dist = "lease", models, methods) {
  
  x$treated <- x[[dist]]
  # Kick out observations that are neither treated nor controls
  x <- x %>% dplyr::filter(!is.na(treated))
  
  out <- vector("list", length = length(models) * length(methods)) # + 1)
  # out[[1]] <- x
  names(out) <- paste0(
    # "data",
    rep(methods, each = length(models)), "-",
    rep(names(models), length(methods))
  )
  
  for(method in methods) {
    for(model_name in names(models)) {
      out[[paste0(method, "-", model_name)]] <- tryCatch(
        MatchIt::matchit(models[[model_name]],
                         data = x[, all.vars(models[[model_name]])],
                         method = method),
        error = function(e) {paste0(e)})
    }
  }
  return(out)
}


# Prepare environment -----------------------------------------------------

path_in <- paste0(data, "/countries/")
path_out <- paste0(home, "/results/")

# Coded in 1km steps
dist_control <- 5e4 # (50,000, Inf)
dist_treated <- list(
  "lease" = c(-1, 500), # Within the mine
  "00-10" = c(500 + 1e-12, 1e4), # (500, 10,000]
  "10-20" = c(1e4 + 1e-12, 2e4),
  "20-30" = c(2e4 + 1e-12, 3e4),
  "30-40" = c(3e4 + 1e-12, 4e4),
  "40-50" = c(4e4 + 1e-12, 5e4) # (40,000, 50,000]
)

# Use grouped soilgrids
soilgrids_grouped <- read.csv(paste0(home, "soilgrids_grouped.csv"), row.names = NULL)

models <- list(
  "fuller" = treated ~ dist_road + dist_waterway + elevation + accessibility_cities_2015 + slope + distance_protected_area,
  "full" = treated ~ dist_road + dist_waterway + elevation + accessibility_cities_2015 + slope + distance_protected_area,
  "no_relief" = treated ~ dist_road + dist_waterway + accessibility_cities_2015 + distance_protected_area
)
# methods <- list("cem", "nearest")
methods <- list("cem")


# Exceute -----------------------------------------------------------------

files <- list.files(path_in)
# We really only need some countries:
# files <- c("south_america-BRA.rds")

# for(file in files) {
file <- files[[job_id]]

x <- readRDS(paste0(path_in, file))
x <- prep_country(x)

out <- vector("list", length(dist_treated) + 1  )
names(out) <- c("data", names(dist_treated))

for(treated_name in names(dist_treated)) {
  x[[treated_name]] <- treat_country(x, dist_treated[[treated_name]], dist_control)
  
  out[[treated_name]] <- if(sum(!is.na(x[[treated_name]])) > 1000) {
    get_matchit(x, treated_name, models, methods)
  } else {NULL}
}

out[["data"]] <- x # Now includes all treatment vars

saveRDS(out, paste0(path_out, "/", file))

# }


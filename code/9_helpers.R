#' @title Prepare raw data
#'
#' @param x Tibble with data. Variable names are assumed to be known.
#' @param has_forest Logical. Whether to subset to `forest > 0`.
#' @param has_access Logical. Whether to kick accessiblity NAs (coded as < 0).
#' @param calc_dist Logical. Whether to calculate additional distances.
#' @param adjust_soil Logical. Whether to group soilgrids.
#' @param adjust_esa Logical. Whether to group and label land use.
#' @param adjust_eco Logical. Whether to label ecoregions.
#' @param sub_eco String for regex to subset ecoregions. NULL to skip.
#' @param geom Logical. Whether to keep sf's geometry column.
#'
#' @return Returns a modified x.

prep_data <- function(x,
  has_forest = FALSE, has_access = FALSE, calc_dist = TRUE,
  adjust_soil = TRUE, adjust_esa = TRUE,
  adjust_eco = TRUE, sub_eco = "Tropical",
  country_specific = TRUE,
  geom = FALSE) {

  # Note: Can remove this after next run
  x$distance_cropland_2000 <- as.numeric(x$distance_cropland_2000)
  x$ecoregions_2017 <- as.numeric(x$ecoregions_2017)

  if(!isTRUE(geom)) {x$geometry <- NULL}
  # Subset grids to ones with forest
  if(isTRUE(has_forest)) {x <- x[x$area_forest_2000 > 0, ]}
  # Fix accessibility to cities / kick errors
  # Note that NAs are erroneously coded as < 0
  x$accessibility_cities_2015[x$accessibility_cities_2015 < 0] <- NA
  if(isTRUE(has_access)) {
    x <- x[!is.na(x$accessibility_cities_2015), ]
  }
  # Create distance to road and waterway value
  if(isTRUE(calc_dist)) {
    x$dist_road <- pmin(
      x$distance_highway_motorway, x$distance_highway_trunk, na.rm = TRUE)
    x$dist_waterway <- pmin(
      x$distance_waterway_canal, x$distance_waterway_river, na.rm = TRUE)
  }
  # Create a grouped soilgrid variable
  if(isTRUE(adjust_soil)) {
    groups <- read.csv("input/soilgrid_grouped.csv")
    x$soilgrid_grouped <- droplevels(as.factor(
      groups[match(x$soilgrid, groups$Number), "WRB_group"]))
    x$soilgrid <- as.factor(x$soilgrid)
  }
  # Create a grouped ESA CCI variable, add labels to ESA CCI
  if(isTRUE(adjust_esa)) {
    groups <- read.csv("input/esa_cci_legend.csv")
    x$esa_cci_2000_grouped <- as.factor(
      groups[match(x$esa_cci_2000, groups$Value), "Group_custom"])
    x$esa_cci_2000_grouped <- as.factor(
      groups[match(x$esa_cci_2000, groups$Value), "Group_custom"])
    x$esa_cci_2000 <- droplevels(factor(x$esa_cci_2000,
      levels = groups$Value, labels = groups$Label))
  }

  if(isTRUE(adjust_eco)) {
    groups <- read.csv("input/ecoregions_2017_concordance.csv")
    x$ecoregions_2017 <- droplevels(factor(x$ecoregions_2017,
      levels = groups$ECO_ID, label = groups$ECO_NAME))
    x$biomes_2017 <- as.factor(
      groups[match(x$ecoregions_2017, groups$ECO_NAME), "BIOME_NAME"])
    if(!is.null(sub_eco)) {
      x <- x[grep(sub_eco, x$biomes_2017), ]
    }
  }

  if(isTRUE(country_specific)) {
    if(all(x$countries == "ECU")) {
      x <- x[x$distance_mine <= 1e6, ] # Cut islands
    }
    if(all(x$countries == "IND")) {
      x <- x[x$distance_mine <= 750000, ] # Cut islands
    }
    if(all(x$countries == "NIC")) {
      x <- x[x$distance_mine <= 200000, ] # Cut islands
    }
  }

  return(x)
}

#' @title Calculate a treatment vector
#'
#' @param x Tibble with data.
#' @param dist_treated Numeric vector with (inclusive) bounds for treatment.
#' @param dist_control Numeric scalar with control distance.
#' @param treat_var Character scalar with the name of the treatment variable.
#'
#' @return Returns a logical vector that is TRUE if treat_var is within
#' dist_treated, FALSE if it is outside of dist_control and NA otherwise.

calc_treatment <- function(x,
  dist_treated, dist_control, treat_var = "distance_mine") {

  # Check whether an observation is treated
  treated <- ifelse(x[[treat_var]] >= dist_treated[1] &
    x[[treat_var]] <= dist_treated[2], TRUE, NA)
  # Check whether it is not
  treated <- ifelse(x$distance_mine > dist_control, FALSE, treated)

  return(treated)
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

#' @title Helper for CEM's drop
#'
#' @param x Object with names.
#' @param but Character vector with names to exclude.
#'
#' @return Returns a character vector with all names but but.
drop_but <- function(x, but) {

  names(x)[!grepl(paste0(but, collapse = "|"), names(x))]
}


#' @title Helper to get ISO from a file-string.
#'
#' @param x Object with the string.
#'
#' @return Returns a character vector with just the ISO
get_iso <- function(x) {

  sub(".*([A-Z]{3}).*.rds", "\\1", x)
}


#' @title Derive variables for modelling.
#'
#' @param x Tibble with the data.
#' @param treated Numeric vector of length two with bounds for treatment.
#' @param dist_log Boolean. Whether to create logged distances (with min(1, d)).
#' @param dist_bool Numeric scalar. Whether and where to create a boolean for
#' for being within `dist_bool` distance.
#' @param dist_decay Numeric scalar. Whether and hwo to apply distance decay to
#' distances as 1 / d ^ `dist_decay` (with min(1, d)).
#'
#' @return Returns a character vector with just the ISO
add_vars <- function(x,
  treated = c(-1, 5e4),
  dist_log = TRUE, dist_bool = 1e3, dist_decay = 0.5) {

  x$treated <- calc_treatment(tbl,
    dist_treated = treated, dist_control = treated[2])

  # Screw you mutate_at
  if(dist_log) {
    x <- mutate_at(x, vars(starts_with("dist")),
      list(log = function(.) log(pmax(., 1))))
  }
  if(!is.null(dist_bool)) {
    x <- mutate_at(x, vars(starts_with("dist")),
      list(bool = function(.) . > dist_bool))
  }
  if(!is_null(dist_decay)) {
    x <- mutate_at(x, vars(starts_with("dist")),
      list(decay = function(.) 1 / pmin(., 1) ^ dist_decay))
  }

  return(x)
}

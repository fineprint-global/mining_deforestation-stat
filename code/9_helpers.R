#' @title Prepare raw data
#'
#' @param x Tibble with data. Variable names are assumed to be known.
#' @param has_forest Logical. Whether to subset to `forest > 0`.
#' @param has_access Logical. Whether to kick accessiblity NAs (coded as < 0).
#' @param calc_dist Logical. Whether to calculate additional distances.
#' @param adjust_soil Logical. Whether to group soilgrids.
#' @param adjust_esa Logical. Whether to group and label land use.
#' @param geom Logical. Whether to keep sf's geometry column.
#'
#' @return Returns a modified x.

prep_data <- function(x,
  has_forest = TRUE, has_access = TRUE, calc_dist = TRUE,
  adjust_soil = TRUE, adjust_esa = TRUE,
  geom = FALSE) {

  if(!isTRUE(geom)) {x$geometry <- NULL}
  # Subset grids to ones with forest
  if(!isTRUE(has_forest)) {x <- x[x$area_forest_2000 > 0, ]}
  # Fix accessibility to cities / kick errors
  if(isTRUE(has_access)) {
    # Note that NAs are erroneously coded as < 0
    x$accessibility_cities_2015[x$accessibility_cities_2015 < 0] <- NA
    x <- x[!is.na(x$accessibility_cities_2015), ]
  }
  # Create distance to road and waterway value
  if(isTRUE(calc_dist)) {
    x$dist_road <- pmin(
      x$distance_highway_motorway, x$distance_highway_primary, na.rm = TRUE)
    x$dist_waterway = pmin(
      x$distance_waterway_canal, x$distance_waterway_river, na.rm = TRUE)
  }
  # Create a grouped soilgrid variable
  if(isTRUE(adjust_soil)) {
    groups <- read.csv("input/soilgrid_grouped.csv")
    x$soilgrid_grouped <- as.factor(
      groups[match(tbl_raw$soilgrid, groups$Number), "WRB_group"])
    x$soilgrid <- as.factor(x$soilgrid)
  }
  # Create a grouped ESA CCI variable, add labels to ESA CCI
  if(isTRUE(adjust_esa)) {
    groups <- read.csv("input/esa_cci_legend.csv")
    x$esa_cci_2000_grouped <- as.factor(
      groups[match(tbl_raw$esa_cci_2000, groups$Value), "Group_custom"])
    x$esa_cci_2000_grouped <- as.factor(
      groups[match(tbl_raw$esa_cci_2000, groups$Value), "Group_custom"])
    x$esa_cci_2000 <- droplevels(factor(x$esa_cci_2000,
      levels = groups$Value, labels = groups$Label))
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

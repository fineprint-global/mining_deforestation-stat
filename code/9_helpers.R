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
  x$ecoregions <- as.numeric(x$ecoregions)

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
    x$dist_waterway <- pmin(pmin(
      x$distance_waterway_canal, x$distance_waterway_river, na.rm = TRUE),
      x$distance_sea, na.rm = TRUE)
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
    x$ecoregions <- droplevels(factor(x$ecoregions,
      levels = groups$ECO_ID, label = groups$ECO_NAME))
    x$biomes_2017 <- as.factor(
      groups[match(x$ecoregions, groups$ECO_NAME), "BIOME_NAME"])
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
  
  # Add areas
  if(isTRUE(geom)) {
    as(x, "Spatial")
    x$area <- sf::st_area(x)
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


#' @title Merge model outputs for comparison
#'
#' @param path Path to csv files from code/3_models.R
#' @param files Character vector. Selection of csv files to be read.
#' @param coef_subs Character vector. Subset of coefficients.
#'
#' @return Returns a tibble.
compare_models_merge <- function(path, files, coef_subs = NULL){

  # read data
  data <- list.files(path = path,
               pattern = paste(files, collapse = "|"),
               full.names = T) %>%
    purrr::map_df(~readr::read_csv(.))

  # subset
  if(!is.null(coef_subs)) {
    data <- data %>% dplyr::filter(vars %in% coef_subs)
  }

  return(data)
}

#' @title Merge model info outputs for comparison
#'
#' @param path Path to csv files from code/3_models.R
#' @param files Character vector. Selection of csv files to be read.
#'
#' @return Returns a tibble.
compare_models_info <- function(path, files){

  # read data
  data <- list.files(path = path,
                     pattern = paste(files, collapse = "|"),
                     full.names = T) %>%
    purrr::map_df(~readr::read_csv(.))

  return(data)
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
  dist_log = TRUE, dist_bool = TRUE, dist_decay = 0.5, dist_mine_dummy = TRUE) {

  x$treated <- calc_treatment(tbl,
    dist_treated = treated, dist_control = treated[2])

  # Distance variables ---

  distance_vars <- c("distance_mine", "dist_road", "dist_waterway",
    "distance_cropland_2000", "distance_protected_area")

  # Screw you mutate_at
  if(!is.null(dist_decay)) {
    x <- mutate_at(x, distance_vars,
      list(decay = function(.) 1 / pmax(., 100) ^ dist_decay))
  }
  if(dist_log) {
    x <- mutate_at(x, distance_vars,
      list(log = function(.) log(pmax(., 1))))
  }
  if(dist_bool) {
    x <- mutate_at(x, distance_vars,
      list(bool = function(.) . <= 1e3))
    x <- mutate_at(x, distance_vars, list(
      km5 = function(.) . <= 5e3, km10 = function(.) . <= 1e4,
      km20 = function(.) . <= 2e4, km25 = function(.) . <= 2e4,
      km50 = function(.) . <= 5e4))
  }

  # Other variables
  x <- mutate_at(x, c("area_accumulated_forest_loss_2010", "area_accumulated_forest_loss_2019", 
                      "area_forest_2000",
                      "pop_2000", "elevation"),
    list(log = function(.) log(pmax(., 1))))
  
  # dummies for dist mine
  if(dist_mine_dummy){
    x <- x %>% dplyr::mutate(
      km_inside = ifelse(-1 < distance_mine & distance_mine <= 1000, TRUE, FALSE),
      km1_10 = ifelse(1000 < distance_mine & distance_mine <= 10000, TRUE, FALSE),
      km10_20 = ifelse(10000 < distance_mine & distance_mine <= 20000, TRUE, FALSE),
      km20_30 = ifelse(20000 < distance_mine & distance_mine <= 30000, TRUE, FALSE),
      km30_40 = ifelse(30000 < distance_mine & distance_mine <= 40000, TRUE, FALSE),
      km40_50 = ifelse(40000 < distance_mine & distance_mine <= 50000, TRUE, FALSE),
      km50_60 = ifelse(50000 < distance_mine & distance_mine <= 60000, TRUE, FALSE),
      km60_70 = ifelse(60000 < distance_mine & distance_mine <= 70000, TRUE, FALSE),
      km70_80 = ifelse(70000 < distance_mine & distance_mine <= 80000, TRUE, FALSE),
      km80_90 = ifelse(80000 < distance_mine & distance_mine <= 90000, TRUE, FALSE),
      km90_100 = ifelse(90000 < distance_mine & distance_mine <= 100000, TRUE, FALSE),
      km50_up = ifelse(50000 < distance_mine, FALSE, TRUE),
      km100_up = ifelse(100000 < distance_mine, FALSE, TRUE)
    )
  }

  return(x)
}



#' @title Plot coefficients from merged outputs
#'
#' @param data Merged tibble obtained from compare_models_merge()
#' @param coef_type (Vector of) Selected column(s) from data input, e.g. "lm_coef", "tob_coef", or "logit_coef"
#'
#' @return Returns a ggplot object.
compare_models_plot <- function(data, coef_type){
  
  if (length(coef_type) == 1){
    
    data <- data %>% dplyr::select(vars, coef_type, country, model)
    colnames(data) <- c("vars", "coef", "country", "model")
    
    p <- data %>%
      ggplot2::ggplot(aes(x = vars, y = coef, color = country, shape = model)) +
      ggplot2::geom_point() +
      ggplot2::labs(y = coef_type) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))
    
  } else {
    
    data <- data %>% dplyr::select(vars, coef_type, country, model)
    data <- data %>% tidyr::gather("type", "coef", -vars, -country, -model)
    
    p <- data %>%
      ggplot2::ggplot(aes(x = vars, y = coef, color = country, shape = model)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(. ~ type) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))

  }
  
  return(p)
}



#' @title Compute fitted values (wrt distance from mine) from merged outputs
#'
#' @param path Path to csv files from code/3_models.R
#' @param files Character vector. Selection of csv files to be read. Need to stem from the same model!
#' @param countries ISO3 character vector for country subset.
#' @param npred No. of steps in x
#' @param log_dist Logical. Whether distance from mine variable is log transformed.
#'
#' @return Returns a matrix. Col 1 refers to x, col 2 to fitted value deforestation, col 3 to country
get_fitted <- function(path, files, countries, npred, log_dist = FALSE){

  dat <- compare_models_merge(path = path,
                              files = files) %>%
    dplyr::filter(country %in% countries) %>%
    dplyr::filter(stringr::str_detect(vars, "distance_mine")) %>%
    dplyr::filter(! stringr::str_detect(vars, " * ")) %>% # remove interactiona - how to deal with them??
    dplyr::filter(! stringr::str_detect(vars, "bool")) # remove bool - how to deal with them??

  pred_matrix <- matrix(NA, npred*length(unique(dat$country)), 3)
  for(i in seq_along(unique(dat$country))){

    dat_sub <- dat %>% dplyr::filter(country == unique(dat$country)[i])
    pols <- matrix(NA, nrow(dat_sub), npred)

    if (log_dist == FALSE){
      for(p in c(1:nrow(pols))){
        pols[p,] <- dat_sub$lm_coef[p] * seq(1, npred, 1)^p
      }
    } else {
      for(p in c(1:nrow(pols))){
        pols[p,] <- dat_sub$lm_coef[p] * log(seq(1, npred, 1)^p)
      }
    }

    pred <- colSums(pols)
    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),1] <- seq(1, npred, 1)
    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),2] <- pred
    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),3] <- unique(dat$country)[i]

  }

  return(pred_matrix)

}


#' @title Compute fitted values (wrt distance from mine) from merged outputs, v2 log-log model only
#'
#' @param path Path to csv files from code/3_models.R
#' @param files Character vector. Selection of csv files to be read. Need to stem from the same model!
#' @param countries ISO3 character vector for country subset.
#' @param npred No. of steps in x
#' @param breaks Numeric vector. Selection of interaction distances according to the model.
#'
#' @return Returns a matrix. Col 1 refers to x, col 2 to fitted value deforestation, col 3 to country
get_fitted2 <- function(path, files, countries, npred, breaks){

  dat <- compare_models_merge(path = path,
                              files = files) %>%
    dplyr::filter(country %in% countries) %>%
    dplyr::filter(stringr::str_detect(vars, "distance_mine"))

  pred_matrix <- matrix(NA, npred*length(unique(dat$country)), 3)
  for(i in seq_along(unique(dat$country))){

    dat_sub <- dat %>% dplyr::filter(country == unique(dat$country)[i])
    mat <- matrix(NA, nrow(dat_sub), npred)

    for(j in 1:nrow(dat_sub)){
      mat[j, c(1:c(npred, breaks)[j])] <- dat_sub$lm_coef[j] * log(seq(1, c(npred, breaks)[j], 1))
    }

    # mat[1,] <- dat_sub$lm_coef[1] * log(seq(1, npred, 1))
    # mat[2,c(1:5000)] <- dat_sub$lm_coef[2] * log(seq(1, 5000, 1))
    # mat[3,c(1:20000)] <- dat_sub$lm_coef[3] * log(seq(1, 20000, 1))
    mat[is.na(mat)] <- 0
    pred <- colSums(mat)

    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),1] <- seq(1, npred, 1)
    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),2] <- pred
    pred_matrix[c((npred*(i-1) + 1) : (i*npred)),3] <- unique(dat$country)[i]

  }

  return(pred_matrix)

}


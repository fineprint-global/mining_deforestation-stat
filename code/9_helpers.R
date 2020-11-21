

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


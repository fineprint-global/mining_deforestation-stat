library(tidyverse)
library(xtable)
library(countrycode)

source("code/9_helpers.R")
path_out <- "output/change_szenario/"
countries <- c("AGO", "BRA", "COL", "COD", "CIV", "ECU", "GAB", "GHA", "GTM", "GIN", "GUY", "HND", "IND", "IDN", "KEN", 
               "LBR", "MEX", "MOZ", "MYS", "NIC", "PHL", "PNG", "SLE", "SUR", "TZA", "THA", "VEN", "VNM", "ZMB")
model_name <- "f_base_log_minesize"
coef_names <- c("distance_mine_log")

country <- "NIC"
for(country in countries){
  
  cat("\n Processing", country, "...")
  
  # load data used for regression -------------------------------------------
  
  load(paste0("output/reg_out_rda/lm_", country, "_", model_name, ".RData"))
  
  # prepare new data --------------------------------------------------------
  
  X <- out_lm1$model$X
  y <- out_lm1$model$y

  # failed try --------------------------------------------------------------

  # note that predict(out_lm1) - predict(out_lm0) approach did not work, because of huge uncertainty when predicting the model:
  
  # X_new <- X %>% as.data.frame() %>%
  #   dplyr::mutate(distance_mine = exp(distance_mine_log)) %>%
  #   dplyr::mutate(distance_mine_new = distance_mine - 100) %>%
  #   dplyr::mutate(distance_mine_log = log(pmax(distance_mine_new - 100, 1))) %>%
  #   dplyr::select(colnames(X))
  # X_new <- as.matrix(X_new)
  # 
  # out_lm2 <- out_lm1
  # out_lm2$model$X <- X_new
  #
  # predict and calculate effect
  #
  # def_orig <- sum(exp(y)) / 1000000
  # 
  # y_0 <- predict(out_lm1)
  # pred_lm_0 <- sum(exp(y_0)) / 1000000
  # y_1 <- predict(out_lm2)
  # pred_lm_1 <- sum(exp(y_1)) / 1000000
  # y_1_int <- predict(out_lm2, interval = "conf")
  # pred_low <- sum(exp(y_1_int[, "lwr"])) / 1000000
  # pred_upp <- sum(exp(y_1_int[, "upr"])) / 1000000
  # 
  # 
  # t(data.frame(
  #   "y_1 vs y_0" = pred_lm_1 - pred_lm_0,
  #   "y_1_lwr vs y" = pred_low - def_orig,
  #   "y_1 vs y" = pred_lm_1 - def_orig,
  #   "y_1_upr vs y" = pred_upp - def_orig
  #            ))
  # 
  # # problematic, that actual deforestation is higher than predicted deforestation
  # # also HUGE uncertainty
  #   
  # # manual bounds do not change much:
  # 
  # pred_lm_0 <- sum(exp(pmin(13.65615, pmax(0, y_0)))) / 1000000
  # pred_lm_1 <- sum(exp(pmin(13.65615, pmax(0, y_1)))) / 1000000
  # pred_low <- sum(exp(pmin(13.65615, pmax(0, y_1_int[, "lwr"])))) / 1000000
  # pred_upp <- sum(exp(pmin(13.65615, pmax(0, y_1_int[, "upr"])))) / 1000000
  # 
  # t(data.frame(
  #   "y_1 vs y_0" = pred_lm_1 - pred_lm_0,
  #   "y_1_lwr vs y" = pred_low - def_orig,
  #   "y_1 vs y" = pred_lm_1 - def_orig,
  #   "y_1_upr vs y" = pred_upp - def_orig
  # ))
  # # The manual upper limit of 1km2 deforestation does not change much
  # 
  # # hence, we continue with:

  
  # predict and calculate manually ------------------------------------------
  
  # set up df with distances and relative change
  df <- data.frame(
    distance_mine_log = X[, "distance_mine_log"],
    distance_mine = exp(X[, "distance_mine_log"]),
    def_m2_log = y,
    def_m2 = exp(y)) %>%
    dplyr::filter(distance_mine > 100) %>% # only look at outside 100, within 100 would not be indirect but turn to direct def
    dplyr::mutate(
      distance_mine_new = distance_mine - 100, 
      distance_mine_log_new = log(distance_mine_new),
      log_change = (distance_mine_log - distance_mine_log_new) * -1)
  
  # extract dist_mine coeffs and confidence bounds
  beta <- coef(out_lm1)["Xdistance_mine_log"]
  beta_ci <- confint(out_lm1)["Xdistance_mine_log", ]
  
  # merge into data frame
  df <- df %>% dplyr::mutate(beta_mean = beta, beta_lwr = beta_ci[2], beta_upr = beta_ci[1])
  
  # calculate deforestation effect and 95 confidence bound
  df <- df %>%
    dplyr::mutate(def_m2_new_lwr = exp(log_change*beta_lwr + def_m2_log)) %>%
    dplyr::mutate(deforestation_change_lwr = def_m2_new_lwr - def_m2) %>%
    dplyr::mutate(def_m2_new = exp(log_change*beta_mean + def_m2_log)) %>%
    dplyr::mutate(deforestation_change_mean = def_m2_new - def_m2) %>%
    dplyr::mutate(def_m2_new_upr = exp(log_change*beta_upr + def_m2_log)) %>%
    dplyr::mutate(deforestation_change_upr = def_m2_new_upr - def_m2)
  
  # colSums(df)[c(4, 12, 14, 16)] / 1000000
  
  # write
  readr::write_csv(df, path = paste0(path_out, country, "_100m.csv"))
  cat("Done.")
  
}

# summarise szenario ------------------------------------------------------

country <- "MEX"
store <- list()

for (country in countries){
  
  df <- readr::read_csv(file.path(path_out, paste0(country, "_100m.csv")))
  df <- df %>% dplyr::mutate("country" = country)
  store[[country]] <- df
  
}

df <- dplyr::bind_rows(store)

# note that lower and upper bound are switched
df_summary <- df %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(extra_deforestation_beta_025 = sum(deforestation_change_upr) / 1000000,
                   extra_deforestation_beta = sum(deforestation_change_mean) / 1000000,
                   extra_deforestation_beta_975 = sum(deforestation_change_lwr) / 1000000) %>%
  dplyr::arrange(-extra_deforestation_beta)


# latex -------------------------------------------------------------------

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- "& \\multicolumn{3}{c}{Deforestation effect (100 m mine expansion)}"

dfx <- df_summary %>% 
  dplyr::select(-extra_deforestation_beta) %>%
  dplyr::mutate(country = countrycode::countrycode(country, "iso3c", "eurostat.name")) %>%
  `colnames<-`(c("Country", "95% lower", "95% upper"))
print(xtable::xtable(dfx,
                     align = "llrr",
                     digits = 0,
                     caption = "Estimated indirect deforestation in a scenario where all mines expand their borders by 100 m; lower and upper bound of 95\\% confidence interval.", 
                     label = "tab:effects_100m"), 
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row=addtorow, include.rownames=FALSE, size="\\footnotesize")






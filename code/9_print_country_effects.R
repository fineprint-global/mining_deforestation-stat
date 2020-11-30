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
  
  X_new <- X %>% as.data.frame() %>%
    dplyr::mutate(distance_mine = exp(distance_mine_log)) %>%
    dplyr::mutate(distance_mine_new = distance_mine - 100) %>%
    dplyr::mutate(distance_mine_log = log(pmax(distance_mine_new - 100, 1))) %>%
    dplyr::select(colnames(X))
  X_new <- as.matrix(X_new)

  out_lm2 <- out_lm1
  out_lm2$model$X <- X_new

  # predict and calculate effect --------------------------------------------

  sum(exp(y))

  y_0 <- predict(out_lm1)
  sum(exp(y_0))
  y_1 <- predict(out_lm2)
  sum(exp(y_1))
  y_1_int <- predict(out_lm2, interval = "conf")

  sum(exp(y_1)) - sum(exp(y_0))
  sum(exp(y_1)) - sum(exp(y))
  
  sum(exp(y_1_int[,2])) - sum(exp(y))
  sum(exp(y_1_int[,3])) - sum(exp(y))
  
  # problematic, that actual deforestation is higher than predicted deforestation

  # predict and calculate manually ------------------------------------------

  # set up df with distances and relative change
  df <- data.frame(distance_mine = exp(X[, "distance_mine_log"]),
                   def_m2 = exp(y)) %>%
    dplyr::filter(distance_mine > 100) %>% # is there a better solution???
    dplyr::mutate(distance_mine_new = distance_mine - 100,
                  relative_change = (distance_mine - distance_mine_new) / distance_mine * -1 * 100) 
  
  # extract dist_mine coeffs and confidence bounds
  beta <- coef(out_lm1)["Xdistance_mine_log"]
  beta_ci <- confint(out_lm1)["Xdistance_mine_log", ]
  
  # merge into data frame
  df <- df %>% dplyr::mutate(beta_mean = beta, beta_lwr = beta_ci[1], beta_upr = beta_ci[2])
  
  # calculate deforestation effect and 95 confidence bound
  df <- df %>%
    dplyr::mutate(beta_change_lwr = relative_change * beta_lwr) %>%
    dplyr::mutate(beta_change_mean = relative_change * beta_mean) %>%
    dplyr::mutate(beta_change_upr = relative_change * beta_upr) %>%
    dplyr::mutate(deforestation_change_lwr = beta_change_lwr * def_m2) %>%
    dplyr::mutate(deforestation_change_mean = beta_change_mean * def_m2) %>%
    dplyr::mutate(deforestation_change_upr = beta_change_upr * def_m2)
  
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
  dplyr::summarise(extra_deforestation_beta_025 = sum(deforestation_change_beta_975) / 1000000,
                   extra_deforestation_beta = sum(deforestation_change_beta) / 1000000,
                   extra_deforestation_beta_975 = sum(deforestation_change_beta_025) / 1000000) %>%
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
                     caption = "Estimated deforestation in a scenario where all mines expand their borders by 100 m; lower and upper bound of 95\\% confidence interval.", 
                     label = "tab:effects_100m"), 
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row=addtorow, include.rownames=FALSE, size="\\footnotesize")






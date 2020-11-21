
library(tidyverse)
library(xtable)
library(countrycode)

source("code/9_helpers.R")
path_out <- "output/change_szenario/"
countries <- c("AGO", "BRA", "COL", "COD", "CIV", "ECU", "GAB", "GHA", "GTM", "GIN", "GUY", "HND", "IND", "IDN", "KEN", "LBR", "MOZ", "MYS", "NIC", "PNG", "SLE", "TZA", "VEN", "VNM", "ZMB")
model_name <- "f_base_log_minesize"
coef_names <- c("distance_mine_log")


country <- "IDN"
for(country in countries){
  
cat("\n Processing", country, "...")

# load data used for regression -------------------------------------------

load(paste0("output/reg_out_rda/lm_", country, "_", model_name, ".RData"))
load(paste0("output/reg_out_rda/y_", country, "_", model_name, ".RData"))
load(paste0("output/reg_out_rda/X_", country, "_", model_name, ".RData"))

# extract distances to mine
distance_mine <- X %>% as.data.frame() %>% 
  dplyr::mutate(distance_mine = exp(distance_mine_log)) %>%
  .$distance_mine

# extract absolute deforestation of each observation
def_m2 <- exp(y)-1 

# load output csv ---------------------------------------------------------

coeffs <- compare_models_merge(path = "output/txt",
                           files = paste0("coef_", country, "_", model_name, ".csv"),
                           coef_subs = coef_names)

# extract dist_mine coeffs
betas <- coeffs$lm_coef
betas_025 <- coeffs$lm_coef - 2*coeffs$lm_se
betas_975 <- coeffs$lm_coef + 2*coeffs$lm_se

# set up data for computations --------------------------------------------

# merge into data frame
df <- data.frame(distance_mine = distance_mine,
                 def_m2 = def_m2)

df <- df %>% dplyr::mutate(beta = betas, beta_025 = betas_025, beta_975 = betas_975)

# df <- df %>% dplyr::mutate(beta = ifelse(distance_mine < 5000, betas[1], NA))
# df <- df %>% dplyr::mutate(beta = ifelse(distance_mine < 25000 & is.na(beta), betas[1] + betas[2], beta))
# df <- df %>% dplyr::mutate(beta = ifelse(distance_mine >= 25000, betas[1] + betas[2] + betas[3],  beta))

# hypothetically shift all distances by 1 km and compute relative change
df <- df %>%
  dplyr::filter(distance_mine > 1000) %>% # is there a better solution???
  dplyr::mutate(new_dist = distance_mine - 1000) %>%
  dplyr::mutate(relative_change = (distance_mine - new_dist) / distance_mine * -1 * 100) 

# calculate deforestation effect 95 confidence bound
df <- df %>%
  dplyr::mutate(beta_change_025 = relative_change * beta_025) %>%
  dplyr::mutate(beta_change = relative_change * beta) %>%
  dplyr::mutate(beta_change_975 = relative_change * beta_975) %>%
  dplyr::mutate(deforestation_change_beta_025 = beta_change_025 * def_m2) %>%
  dplyr::mutate(deforestation_change_beta = beta_change * def_m2) %>%
  dplyr::mutate(deforestation_change_beta_975 = beta_change_975 * def_m2)

# write
readr::write_csv(df, path = paste0(path_out, country, "_1km.csv"))

cat("Done.")

}



# summarise szenario ------------------------------------------------------

country <- "NIC"
store <- list()

for (country in countries){
  
  df <- readr::read_csv(file.path(path_out, paste0(country, "_1km.csv")))
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
addtorow$command <- "& \\multicolumn{3}{c}{Deforestation effect 1 km }"

dfx <- df_summary %>% 
  dplyr::select(-extra_deforestation_beta) %>%
  dplyr::mutate(country = countrycode::countrycode(country, "iso3c", "eurostat.name")) %>%
  `colnames<-`(c("Country", "95% lower", "95% upper"))
print(xtable::xtable(dfx,
                     align = "llrr",
                     digits = 0,
                     caption = "Estimated deforestation in a scenario where all observations were 1 km closer to a mine; lower and upper bound of 95\\% confidence interval.", 
                     label = "tab:effects_1km"), 
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row=addtorow, include.rownames=FALSE, size="\\footnotesize")




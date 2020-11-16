
library(dplyr)
library(tidyr)
library(xtable)

source("code/9_helpers.R")
countries <- c("AGO", "BRA", "COL", "COD", "CIV", "ECU", "GAB", "GHA", "GTM", "GIN", "GUY", "HND", "IND", "IDN", "KEN", "LBR", "MOZ", "MYS", "NIC", "PNG", "SLE", "TZA", "VEN", "VNM", "ZMB")


# base log minesize (main table) ------------------------------------------

model_names <- c("f_base_log_minesize")
coef_names <- c("distance_mine_log")
column_names <- c("lm_coef", "lm_se")

### load output csv 
df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"),
                           coef_subs = coef_names)

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

### tidy data
# select columns
df <- df %>% dplyr::select("country", "model", "vars", all_of(column_names))

# spread by coef
A <- df %>% 
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm"))

B <- df %>% 
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "dm_se"))

C <- info %>%
  dplyr::select(country, N, N_tr_matched, R2) %>%
  dplyr::mutate(N_tr_perc = N_tr_matched / N)

# merge
df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(country, dm, dm_se, N, N_tr_perc, R2)

### export excel
readr::write_csv(df, path = paste0("output/result_table_base_log_minesize.csv"))

### latex
A <- df %>% dplyr::select(country, dm, N, N_tr_perc, R2) %>%
  dplyr::mutate(dm = as.character(round(dm, 3)))
B <- df %>% dplyr::select(country, dm_se, N, N_tr_perc, R2) %>%
  dplyr::mutate(dm_se = paste0("(", round(dm_se, 3), ")"))
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>%
  `colnames<-`(c("country", "dm", "N", "N_tr_perc", "R2"))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "DM", "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx, 
                     align = "llcrrr",
                     digits = c(0, 0, 0, 0, 2, 2),
                     caption = "\\small Indirect deforestation effects induced by mining in 25 tropical countries; OLS estimates; standard errors in parentheses; see supplementary material for control variables", 
                     label = "tab:coefs"), include.rownames=FALSE, size="\\footnotesize")

# base log minesize (full appendix table) ---------------------------------

model_names <- c("f_base_log_minesize")
latex_names <- c("aforest", "droad", "dwater", "dcrop", "dmine", "dpa", "elev", "amine", "pop", "slope")
column_names <- c("lm_coef", "lm_se")

### load output csv 
df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"))

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

### tidy data
# select columns
df <- df %>% dplyr::select("country", "model", "vars", all_of(column_names))

# spread by coef
A <- df %>% 
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef")

B <- df %>% 
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") 

C <- info %>%
  dplyr::select(country, N, N_tr_matched, R2) %>%
  dplyr::mutate(N_tr_perc = N_tr_matched / N)

# merge
cnames <- colnames(dplyr::left_join(A, B, by = "country"))
cnames <- cnames[!grepl("soilgrid", cnames)]
cnames <- cnames[!grepl("biome", cnames)]
cnames <- cnames[!grepl("esa_cci", cnames)]
cnames <- cnames[!grepl("I\\(", cnames)]
cnames <- cnames[!grepl("constant", cnames)]
cnames <- c(cnames[1], sort(cnames[-1]))

df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(cnames, N, N_tr_perc, R2)

### export excel
readr::write_csv(df, path = paste0("output/result_table_base_log_minesize_full.csv"))

### latex
A <- df %>% dplyr::select(country, grep("\\.x$", cnames), N, N_tr_perc, R2)
B <- df %>% dplyr::select(country, grep("\\.y$", cnames), N, N_tr_perc, R2)
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>% `colnames<-`(colnames(A))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", latex_names, "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx,
                     # align = "llcrrr",
                     digits = c(0, 0, 3, 3, 3, 3, 3, 3, 4, 3, 3, 3, 0, 2, 2),
                     caption = "\\small OLS estimates; standard errors in parentheses; incl. all control variables except interaction terms and categorical landcover, soilgrid and biome variables", 
                     label = "tab:all_coefs"), 
      floating = TRUE, floating.environment = "sidewaystable",
      include.rownames=FALSE, size="\\footnotesize")


# vary log minesize (5 and 25 km interactions) ----------------------------

model_names <- c("f_vary_minesize")
coef_names <- c("distance_mine_log", "I(distance_mine_log * distance_mine_km5)", "I(distance_mine_log * distance_mine_km25)")
column_names <- c("lm_coef", "lm_se")

### load output csv
df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"),
                           coef_subs = coef_names)

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

### tidy data
# select columns
df <- df %>% dplyr::select("country", "model", "vars", all_of(column_names))

# spread by coef
A <- df %>% 
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm", "dm_km25", "dm_km5"))

B <- df %>% 
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "dm_se", "dm_km25_se", "dm_km5_se"))

C <- info %>%
  dplyr::select(country, N, N_tr_matched, R2) %>%
  dplyr::mutate(N_tr_perc = N_tr_matched / N)

# merge
df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(country, dm, dm_se, dm_km5, dm_km5_se, dm_km25, dm_km25_se, N, N_tr_perc, R2)

### export excel
readr::write_csv(df, path = paste0("output/result_table_vary_log_minesize.csv"))

### latex
A <- df %>% dplyr::select(country, dm, dm_km5, dm_km25, N, N_tr_perc, R2) %>%
  dplyr::mutate(dm = as.character(round(dm, 3)),
                dm_km5 = as.character(round(dm_km5, 3)),
                dm_km25 = as.character(round(dm_km25, 3)))
B <- df %>% dplyr::select(country, dm_se, dm_km5_se, dm_km25_se, N, N_tr_perc, R2) %>%
  dplyr::mutate(dm_se = paste0("(", round(dm_se, 3), ")"),
                dm_km5_se = paste0("(", round(dm_km5_se, 3), ")"),
                dm_km25_se = paste0("(", round(dm_km25_se, 3), ")"))
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>%
  `colnames<-`(c("country", "dm", "dm_km5", "dm_km25", "N", "N_tr_perc", "R2"))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "DM", "DM $\\times$ 5 km", "DM $\\times$ 25 km", "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx,
                     align = "llcccrrr",
                     digits = c(0, 0, 0, 0, 0, 0, 2, 2),
                     caption = "Indirect deforestation effects induced by mining in 25 tropical countries; OLS estimates; standard errors in parentheses",
                     label = "tab:vary_coefs"), include.rownames=FALSE, size="\\scriptsize")

# base log minesize vs logit ----------------------------------------------

model_names <- c("f_base_log_minesize")
coef_names <- c("distance_mine_log")
column_names <- c("lm_coef", "lm_se", "logit_coef", "logit_se")

### load output csv 
df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"),
                           coef_subs = coef_names)

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

### tidy data
# select columns
df <- df %>% dplyr::select("country", "model", "vars", all_of(column_names))

# spread by coef
A <- df %>% 
  dplyr::select(-lm_se, -logit_se, -model, -vars)

B <- df %>% 
  dplyr::select(-lm_coef, -logit_coef, -model, -vars)

C <- info %>%
  dplyr::select(country, N, N_tr_matched, R2) %>%
  dplyr::mutate(N_tr_perc = N_tr_matched / N)

# merge
df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(country, lm_coef, lm_se, logit_coef, logit_se, N, N_tr_perc, R2)

### export excel
readr::write_csv(df, path = paste0("output/result_table_compare_log_minesize.csv"))

### latex
A <- df %>% dplyr::select(country, lm_coef, logit_coef, N, N_tr_perc, R2) %>%
  dplyr::mutate(lm_coef = as.character(round(lm_coef, 3)),
                logit_coef = as.character(round(logit_coef, 3)))
B <- df %>% dplyr::select(country, lm_se, logit_se, N, N_tr_perc, R2) %>%
  dplyr::mutate(lm_se = paste0("(", round(lm_se, 3), ")")) %>%
  dplyr::mutate(logit_se = paste0("(", round(logit_se, 3), ")"))
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>%
  `colnames<-`(c("country", "lm_coef", "logit_coef", "N", "N_tr_perc", "R2"))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "DM (lm)", "DM (logit)", "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx, 
                     align = "llccrrr",
                     digits = c(0, 0, 0, 0, 0, 2, 2),
                     caption = "\\small Indirect deforestation effects induced by mining in 25 tropical countries; OLS and logit estimates; standard errors in parentheses", 
                     label = "tab:compare_coefs"), include.rownames=FALSE, size="\\footnotesize")


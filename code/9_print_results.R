
library(dplyr)
library(tidyr)
library(xtable)
library(countrycode)

source("code/9_helpers.R")
countries <- c("AGO", "BRA", "COL", "COD", "CIV", "ECU", "GAB", "GHA", "GTM", "GIN", "GUY", "HND", "IND", "IDN", "KEN", 
               "LBR", "MEX", "MOZ", "MYS", "NIC", "PHL", "PNG", "SLE", "SUR", "TZA", "THA", "VEN", "VNM", "ZMB")

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
readr::write_csv(df, path = paste0("output/results_tables/base_log_minesize.csv"))

### latex se below coeffs
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
                     caption = "\\small Indirect deforestation effects induced by mining in 29 tropical countries; OLS estimates; standard errors in parentheses; see supplementary material for control variables", 
                     label = "tab:coefs"), include.rownames=FALSE, size="\\footnotesize")

### latex se next to
dfx <- df %>% dplyr::select(country, dm, dm_se, N, N_tr_perc, R2) %>%
  dplyr::mutate(country = countrycode::countrycode(country, "iso3c", "eurostat.name")) %>%
  `colnames<-`(c("Country", "DM", "SE", "N", "Share treated", "$R^2$"))
print(xtable::xtable(dfx, 
                     align = "llrrrrr",
                     digits = c(0, 0, 3, 3, 0, 2, 2),
                     caption = "\\small Indirect deforestation effects induced by mining in 29 tropical countries. Dependent variable is the log-transformed accumulated forest loss area between 2000 and 2019, DM (log) represents OLS estimates for the log-transformed distance to mine, and SE indicates standard errors. See Extended Data Table \\ref{tab:all_coefs} for control variables.", 
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
readr::write_csv(df, path = paste0("output/results_tables/base_log_minesize_full.csv"))

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
      include.rownames=FALSE, size="\\scriptsize")


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
readr::write_csv(df, path = paste0("output/results_tables/vary_log_minesize.csv"))

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
                     caption = "Indirect deforestation effects induced by mining in 29 tropical countries; OLS estimates; standard errors in parentheses",
                     label = "tab:vary_coefs"), include.rownames=FALSE, size="\\scriptsize")

# base log minesize vs logit vs tobit --------------------------------------

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
readr::write_csv(df, path = paste0("output/results_tables/compare_log_minesize.csv"))

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
                     caption = "\\small Indirect deforestation effects induced by mining in 29 tropical countries; OLS and logit estimates; standard errors in parentheses; logit model uses share of forest loss relative to total area as dependent variable", 
                     label = "tab:compare_coefs_ols_logit"), include.rownames=FALSE, size="\\scriptsize")

### export excel
readr::write_csv(df, path = paste0("output/results_tables/compare_log_minesize_add_tobit.csv"))

### latex
A <- df %>% dplyr::select(country, lm_coef, logit_coef, tob_coef, N, N_tr_perc, R2) %>%
  dplyr::mutate(lm_coef = as.character(round(lm_coef, 3)),
                logit_coef = as.character(round(logit_coef, 3)),
                tob_coef = as.character(round(tob_coef, 3)))
B <- df %>% dplyr::select(country, lm_se, logit_se, tob_se, N, N_tr_perc, R2) %>%
  dplyr::mutate(lm_se = paste0("(", round(lm_se, 3), ")")) %>%
  dplyr::mutate(logit_se = paste0("(", round(logit_se, 3), ")")) %>%
  dplyr::mutate(tob_se = paste0("(", round(tob_se, 3), ")"))
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>%
  `colnames<-`(c("country", "lm_coef", "logit_coef", "tob_coef", "N", "N_tr_perc", "R2"))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "DM (lm)", "DM (logit)", "DM (tobit)", "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx, 
                     align = "llcccrrr",
                     digits = c(0, 0, 0, 0, 0, 0, 2, 2),
                     caption = "\\small Indirect deforestation effects induced by mining in 29 tropical countries; OLS, logit and tobit estimates; standard errors in parentheses; logit model uses share of forest loss relative to total area as dependent variable", 
                     label = "tab:compare_coefs_ols_logit_tobit"), include.rownames=FALSE, size="\\scriptsize")


# add tobit
dat <- readr::read_csv("output/results_tables/compare_log_minesize.csv")
column_names <- c("tob_coef", "tob_se")

### load output csv 
df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"),
                           coef_subs = coef_names)

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

# select columns and merge
df <- df %>% dplyr::select("country", all_of(column_names))
df <- dplyr::left_join(dat, df, by = "country") %>%
  dplyr::select(country, lm_coef, lm_se, logit_coef, logit_se, tob_coef, tob_se, N, N_tr_perc, R2)



# alternative specifications ----------------------------------------------

model_names <- c("f_base_log_minesize_2010", # main model, change deforestation to 2010 instead of 2019
                 "f_minimal_log_minesize", # minimal model: few variables
                 "f_base_log_no_interactions", # no interactions
                 "f_base_log_minesize_no-road_log") # main model, exclude road variables
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
A1 <- df %>% 
  dplyr::filter(model == "f_base_log_minesize_2010") %>%
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm 2010"))
A2 <- df %>% 
  dplyr::filter(model == "f_minimal_log_minesize") %>%
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm minimal"))
A3 <- df %>% 
  dplyr::filter(model == "f_base_log_no_interactions") %>%
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm no interactions"))
A4 <- df %>% 
  dplyr::filter(model == "f_base_log_minesize_no-road_log") %>%
  dplyr::select(-lm_se, -model) %>%
  tidyr::spread(key = "vars", value = "lm_coef") %>%
  `colnames<-`(c("country", "dm no road"))
A <- dplyr::left_join(A1, A2, by = "country") %>% dplyr::left_join(A3, by = "country") %>% dplyr::left_join(A4, by = "country")
rm(A1, A2, A3, A4)

B1 <- df %>% 
  dplyr::filter(model == "f_base_log_minesize_2010") %>%
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "se 2010"))
B2 <- df %>% 
  dplyr::filter(model == "f_minimal_log_minesize") %>%
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "se minimal"))
B3 <- df %>% 
  dplyr::filter(model == "f_base_log_no_interactions") %>%
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "se no interactions"))
B4 <- df %>% 
  dplyr::filter(model == "f_base_log_minesize_no-road_log") %>%
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", "se no road"))
B <- dplyr::left_join(B1, B2, by = "country") %>% dplyr::left_join(B3, by = "country") %>% dplyr::left_join(B4, by = "country")
rm(B1, B2, B3, B4)

C <- info %>%
  dplyr::select(country, R2, model) %>%
  tidyr::spread(key = "model", value = "R2") %>%
  `colnames<-`(c("country", "R2 2010", "R2 no road", "R2 no interactions", "R2 minimal"))

# merge
df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(country, 
                "dm 2010", "se 2010", "R2 2010", 
                "dm minimal", "se minimal", "R2 minimal",
                "dm no interactions", "se no interactions", "R2 no interactions",
                "dm no road", "se no road", "R2 no road")

### export excel
readr::write_csv(df, path = paste0("output/results_tables/alternative_spec.csv"))

### latex se below coeffs
A <- df %>% dplyr::select(country, 
                          "dm 2010", "R2 2010",
                          "dm minimal", "R2 minimal",
                          "dm no interactions", "R2 no interactions",
                          "dm no road", "R2 no road") %>%
  dplyr::mutate("dm 2010" = as.character(round(`dm 2010`, 3)),
                "dm minimal" = as.character(round(`dm minimal`, 3)),
                "dm no interactions" = as.character(round(`dm no interactions`, 3)),
                "dm no road" = as.character(round(`dm no road`, 3)))
B <- df %>% dplyr::select(country, 
                          "se 2010", "R2 2010",
                          "se minimal", "R2 minimal",
                          "se no interactions", "R2 no interactions",
                          "se no road", "R2 no road") %>%
  dplyr::mutate("se 2010" = paste0("(", round(`se 2010`, 3), ")"),
                "se minimal" = paste0("(", round(`se minimal`, 3), ")"),
                "se no interactions" = paste0("(", round(`se no interactions`, 3), ")"),
                "se no road" = paste0("(", round(`se no road`, 3), ")"))
B <- B %>% dplyr::mutate("R2 2010" = NA, "R2 minimal" = NA, "R2 no interactions" = NA, "R2 no road" = NA) %>%
  `colnames<-`(c("country", 
                 "dm 2010", "R2 2010",
                 "dm minimal", "R2 minimal",
                 "dm no interactions", "R2 no interactions",
                 "dm no road", "R2 no road"))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "DM 2010", "R2 2010", "DM minimal", "R2 minimal", "DM no interact", "R2 no interact", "DM no road", "R2 no road"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx, 
                     align = "llcccccccc",
                     digits = c(0, 0, 0, 2, 0, 2, 0, 2, 0, 2),
                     caption = "\\small Indirect deforestation effects induced by mining in 29 tropical countries; OLS estimates; standard errors in parentheses; see supplementary material for control variables", 
                     label = "tab:coefs"), include.rownames=FALSE, size="\\scriptsize")



# dummy model -------------------------------------------------------------


model_names <- c("f_dummy_minesize")
coef_names <- c("km_insideTRUE", "km1_10TRUE", "km10_20TRUE", "km20_30TRUE", "km30_40TRUE", "km40_50TRUE", "min_area_1degree")
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
  tidyr::spread(key = "vars", value = "lm_coef")

B <- df %>% 
  dplyr::select(-lm_coef, -model) %>%
  tidyr::spread(key = "vars", value = "lm_se") %>%
  `colnames<-`(c("country", paste0(colnames(A)[-1], "_se")))

C <- info %>%
  dplyr::select(country, N, N_tr_matched, R2) %>%
  dplyr::mutate(N_tr_perc = N_tr_matched / N)

# merge
df <- dplyr::left_join(A, B, by = "country") %>%
  dplyr::left_join(C, by = "country") %>%
  dplyr::select(country, 
                "km_insideTRUE", "km_insideTRUE_se", "km1_10TRUE", "km1_10TRUE_se", "km10_20TRUE", "km10_20TRUE_se", 
                "km20_30TRUE", "km20_30TRUE_se", "km30_40TRUE", "km30_40TRUE_se", "km40_50TRUE", "km40_50TRUE_se", "min_area_1degree", "min_area_1degree_se",
                N, N_tr_perc, R2)

### export excel
readr::write_csv(df, path = paste0("output/results_tables/dummy_minesize.csv"))

### latex
A <- df %>% dplyr::select(country, "km_insideTRUE", "km1_10TRUE", "km10_20TRUE", "km20_30TRUE", "km30_40TRUE", "km40_50TRUE", "min_area_1degree", N, N_tr_perc, R2) %>%
  dplyr::mutate(km_insideTRUE = as.character(round(km_insideTRUE, 3)),
                km1_10TRUE = as.character(round(km1_10TRUE, 3)),
                km10_20TRUE = as.character(round(km10_20TRUE, 3)),
                km20_30TRUE = as.character(round(km20_30TRUE, 3)),
                km30_40TRUE = as.character(round(km30_40TRUE, 3)),
                km40_50TRUE = as.character(round(km40_50TRUE, 3)),
                min_area_1degree = as.character(round(min_area_1degree, 3)))
B <- df %>% dplyr::select(country, "km_insideTRUE_se", "km1_10TRUE_se", "km10_20TRUE_se", "km20_30TRUE_se", "km30_40TRUE_se", "km40_50TRUE_se", "min_area_1degree_se", N, N_tr_perc, R2) %>%
  dplyr::mutate(km_insideTRUE_se = as.character(paste0("(", round(km_insideTRUE_se, 3), ")")),
                km1_10TRUE_se = as.character(paste0("(", round(km1_10TRUE_se, 3), ")")),
                km10_20TRUE_se = as.character(paste0("(", round(km10_20TRUE_se, 3), ")")),
                km20_30TRUE_se = as.character(paste0("(", round(km20_30TRUE_se, 3), ")")),
                km30_40TRUE_se = as.character(paste0("(", round(km30_40TRUE_se, 3), ")")),
                km40_50TRUE_se = as.character(paste0("(", round(km40_50TRUE_se, 3), ")")),
                min_area_1degree_se = as.character(paste0("(", round(min_area_1degree_se, 3), ")")))
B <- B %>% dplyr::mutate(N = NA, N_tr_perc = NA, R2 = NA) %>%
  `colnames<-`(colnames(A))
dfx <- dplyr::bind_rows(A, B) %>% dplyr::arrange(country) %>%
  `colnames<-`(c("Country", "Within", "1-10 km", "10-20 km", "20-30 km", "30-40 km", "40-50 km", "Mine area", "Observations", "Share treated", "$R^2$"))
dfx$Country[seq(2, nrow(dfx), 2)] <- NA
print(xtable::xtable(dfx, 
                     align = "llcccccccrrr",
                     digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2),
                     caption = "\\small OLS estimates; standard errors in parentheses; dummy variable for buffered distances to mine", 
                     label = "tab:dummies"), 
      include.rownames=FALSE, size="\\scriptsize")



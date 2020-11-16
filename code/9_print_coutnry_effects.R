
source("code/9_helpers.R")
country <- "IDN"
model_name <- "f_vary_minesize"
coef_names <- c("distance_mine_log", 
                "I(distance_mine_log * distance_mine_km5)", 
                "I(distance_mine_log * distance_mine_km25)")

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

# set up data for computations --------------------------------------------

# merge into data frame
df <- data.frame(distance_mine = distance_mine,
                 def_m2 = def_m2)

df <- df %>% dplyr::mutate(beta = ifelse(distance_mine < 5000, betas[1], NA))
df <- df %>% dplyr::mutate(beta = ifelse(distance_mine < 25000 & is.na(beta), betas[1] + betas[2], beta))
df <- df %>% dplyr::mutate(beta = ifelse(distance_mine >= 25000, betas[1] + betas[2] + betas[3],  beta))

# hypothetically shift all distances by 1 km and compute relative change
df <- df %>%
  dplyr::filter(distance_mine > 1000) %>% # is there a better solution???
  dplyr::mutate(new_dist = distance_mine - 1000) %>%
  dplyr::mutate(relative_change = (distance_mine - new_dist) / distance_mine * -1) 

# calculate deforestation effect
df <- df %>%
  dplyr::mutate(beta_change = relative_change * beta) %>%
  dplyr::mutate(deforestation_change = beta_change * def_m2)

sum(df$deforestation_change) / 1000000



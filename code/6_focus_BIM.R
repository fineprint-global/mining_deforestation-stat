
library("tidyverse")

# BRA, IDN, MYS analysis --------------------------------------------------

BIM <- c("BRA", "IDN", "MYS")
BIM <- c("BRA", "IDN", "MYS", "AGO", "BFA", "CHN", "CIV", "COD", "COL", "ECU",
  "GAB", "GHA", "GIN", "GTM", "GUY", "HND", "KEN", "LAO", "LBR", "MDG", "MOZ",
  "PER", "PHL", "PNG", "SLE", "SUR", "THA", "TZA", "VEN", "VNM", "ZMB")

info <- compare_models_info(path = "output/txt",
                    paste0("info_", rep(BIM, length( names(formulas))), "_", names(formulas), ".csv"))

coefs <- compare_models_merge(path = "output/txt",
                    paste0("coef_", rep(BIM, length( names(formulas))), "_", names(formulas), ".csv"))

compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = c("coef_MYS_f_base.csv", "coef_MYS_f_base_log.csv"),
                       coef_subs = c("distance_mine", "distance_mine_log")))

compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = list.files("output/txt/", pattern = paste(BIM, collapse = "|")),
                       coef_subs = c("distance_mine", "distance_mine_log", "distance_mine_boolTRUE", "distance_mine_decay",
                                     "I(distance_mine_log * dist_road_log)", "I(distance_mine_log * dist_road")) %>%
    dplyr::filter(model %in% names(formulas)[c(1:6)]))

compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = list.files("output/txt/", pattern = paste(BIM, collapse = "|")),
                       coef_subs = c("distance_mine", "distance_mine_log", "distance_mine_boolTRUE", "distance_mine_decay",
                                     "I(distance_mine_log * dist_road_log)", "I(distance_mine_log * dist_road")) %>%
    dplyr::filter(model %in% names(formulas)[c(7:11)]))



out_merged <- compare_models_merge(path = "output/txt",
                                   files = list.files("output/txt/", pattern = paste(BIM, collapse = "|"))) %>%
  dplyr::filter(model %in% names(formulas))

# base vs base_bool
dat_comb <- rbind(
  data.frame(get_fitted(path = "output/txt",
                        files = list.files("output/txt/", pattern = "coef.*_f_base.csv"),
                        countries = BIM,
                        npred = 50,
                        log_dist = FALSE)) %>%
    dplyr::mutate(model = "base"),
  data.frame(get_fitted(path = "output/txt",
                        files = list.files("output/txt/", pattern = "coef.*_f_base_bool.csv"),
                        countries = BIM,
                        npred = 50,
                        log_dist = FALSE)) %>%
    dplyr::mutate(model = "base_bool")
)

dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line(aes(linetype=model))

# f_base_log vs base_bool_log
dat_comb <- rbind(
  data.frame(get_fitted(path = "output/txt",
                        files = list.files("output/txt/", pattern = "coef.*_f_base_log.csv"),
                        countries = BIM,
                        npred = 200,
                        log_dist = TRUE)) %>%
    dplyr::mutate(model = "base-log"),
  data.frame(get_fitted(path = "output/txt",
                        files = list.files("output/txt/", pattern = "coef.*_f_base_bool_log.csv"),
                        countries = BIM,
                        npred = 200,
                        log_dist = TRUE)) %>%
    dplyr::mutate(model = "base_bool_log")
)

dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line(aes(linetype=model))

# decay models



# interactions ------------------------------------------------------------

### distance mine all dummies (5, 10, 20 and 50km)
model_names <- c("f_base_log_nonlins", "f_base_log_interactions_nonlins")

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(BIM, length( model_names)), "_"), 
                                           rep(model_names, each = length(BIM)), ".csv"))

country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("distance_mine_boolTRUE", 
                                     "distance_mine_log", 
                                     "I(distance_mine_log * distance_mine_km5)",
                                     "I(distance_mine_log * distance_mine_km10)",
                                     "I(distance_mine_log * distance_mine_km20)",
                                     "I(distance_mine_log * distance_mine_km50)")))

# distance road  all dummies (5, 10, 20 and 50km)
model_names <- c("f_base_log_nonlins", "f_base_log_interactions_nonlins")
country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("dist_road_boolTRUE", 
                                     "dist_road_log", 
                                     "I(dist_road_log * dist_road_km5)",
                                     "I(dist_road_log * dist_road_km10)",
                                     "I(dist_road_log * dist_road_km20)",
                                     "I(dist_road_log * dist_road_km50)")))

# distance waterway  all dummies (5, 10, 20 and 50km)
model_names <- c("f_base_log_nonlins", "f_base_log_interactions_nonlins")
country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("dist_waterway_boolTRUE", 
                                     "dist_waterway_log", 
                                     "I(dist_waterway_log * dist_waterway_km5)",
                                     "I(dist_waterway_log * dist_waterway_km10)",
                                     "I(dist_waterway_log * dist_waterway_km20)",
                                     "I(dist_waterway_log * dist_waterway_km50)")))

# distance protected area  all dummies (5, 10, 20 and 50km)
model_names <- c("f_base_log_nonlins", "f_base_log_interactions_nonlins")
country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("distance_protected_area_boolTRUE", 
                                     "distance_protected_area_log", 
                                     "I(distance_protected_area_log * distance_protected_area_km5)",
                                     "I(distance_protected_area_log * distance_protected_area_km10)",
                                     "I(distance_protected_area_log * distance_protected_area_km20)",
                                     "I(distance_protected_area_log * distance_protected_area_km50)")))

# distance cropland  all dummies (5, 10, 20 and 50km)
model_names <- c("f_base_log_nonlins", "f_base_log_interactions_nonlins")
country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("distance_cropland_2000_boolTRUE", 
                                     "distance_cropland_2000_log", 
                                     "I(distance_cropland_2000_log * distance_cropland_2000_km5)",
                                     "I(distance_cropland_2000_log * distance_cropland_2000_km10)",
                                     "I(distance_cropland_2000_log * distance_cropland_2000_km20)",
                                     "I(distance_cropland_2000_log * distance_cropland_2000_km50)")))


# distance mine 5 and 50 km
model_names <- c("f_sub_5_20_log_nonlins", "f_sub_5_20_log_interactions_nonlins")

info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(BIM, length( model_names)), "_"), 
                                           rep(model_names, each = length(BIM)), ".csv"))

country <- "IDN"
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0("coef_", rep(country, length( model_names)), "_", model_names, ".csv"),
                       coef_subs = c("distance_mine_boolTRUE", 
                                     "distance_mine_log", 
                                     "I(distance_mine_log * distance_mine_km5)",
                                     "I(distance_mine_log * distance_mine_km10)",
                                     "I(distance_mine_log * distance_mine_km20)",
                                     "I(distance_mine_log * distance_mine_km50)")))

# first proposal model:
# - Distance Mine Dummies für 5 und 20km
# - Road Dummy für 5 und 50km
# - Waterway Dummy für 10km
# - Protected Area Dummy für 20km
# - Cropland Dummy für 10km

### distance mine
model_names <- c("f_proposal_log_nonlins", "f_proposal_log_interactions_nonlins")
info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(BIM, length( model_names)), "_"), 
                                           rep(model_names, each = length(BIM)), ".csv"))

df <- compare_models_merge(path = "output/txt",
                     files = paste0(paste0("coef_", rep(BIM, length( model_names)), "_"), 
                                    rep(model_names, each = length(BIM)), ".csv"))
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0(paste0("coef_", rep(BIM, length( model_names)), "_"), 
                                      rep(model_names, each = length(BIM)), ".csv")))
# plot mine coeffs
p_dat <- compare_models_merge(path = "output/txt",
                            files = paste0(paste0("coef_", rep(BIM, length( model_names)), "_"), 
                                           rep(model_names, each = length(BIM)), ".csv"),
                            coef_subs = c("distance_mine_log", "I(distance_mine_log * distance_mine_km5)", "I(distance_mine_log * distance_mine_km20)"))

compare_models_plot(p_dat)

# plot various coeffs
p_dat <- compare_models_merge(path = "output/txt",
                              files = paste0(paste0("coef_", rep(BIM, length( model_names)), "_"), 
                                             rep(model_names, each = length(BIM)), ".csv"),
                              coef_subs = c("distance_mine_log", "I(distance_mine_log * distance_mine_km5)", "I(distance_mine_log * distance_mine_km20)",
                                            "dist_road_log", "I(dist_road_log * dist_road_km5)", "I(dist_road_log * dist_road_km50)",
                                            "dist_waterway_log", "I(dist_waterway_log * dist_waterway_km10)",
                                            "distance_protected_area_log", "I(distance_protected_area_log * distance_protected_area_km20)"#,
                                            #"distance_cropland_2000_log", "I(distance_cropland_2000_log * distance_cropland_2000_km10)"
                                            ))

p_dat$vars <- factor(p_dat$vars, 
                     levels = c("distance_mine_log", "I(distance_mine_log * distance_mine_km5)", "I(distance_mine_log * distance_mine_km20)",
                                "dist_road_log", "I(dist_road_log * dist_road_km5)", "I(dist_road_log * dist_road_km50)",
                                "dist_waterway_log", "I(dist_waterway_log * dist_waterway_km10)",
                                "distance_protected_area_log", "I(distance_protected_area_log * distance_protected_area_km20)",
                                "distance_cropland_2000_log", "I(distance_cropland_2000_log * distance_cropland_2000_km10)"))
p <- compare_models_plot(p_dat) + ggplot2::geom_hline(yintercept = 0, color = "black")
ggplot2::ggsave("output/plots/coefs/proposal_1_BIM.png", 
                plot = p, device = "png", 
                scale = 1, width = 300, height = 200, units = "mm")

# plot all coeffs
p_dat <- compare_models_merge(path = "output/txt",
                              files = paste0(paste0("coef_", rep(BIM, length( model_names)), "_"), 
                                             rep(model_names, each = length(BIM)), ".csv"))

compare_models_plot(p_dat) 







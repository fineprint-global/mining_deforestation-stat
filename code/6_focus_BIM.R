
library("tidyverse")

# BRA, IDN, MYS analysis --------------------------------------------------

BIM <- c("BRA", "IDN", "MYS")
# BIM <- c("BRA", "IDN", "MYS", "AGO", "BFA", "CHN", "CIV", "COG", "COL", "ECU",
#   "GAB", "GHA", "GIN", "GTM", "GUY", "HND", "KEN", "LAO", "LBR", "MDG", "MOZ",
#   "PER", "PHL", "PNG", "SLE", "SUR", "THA", "TZA", "VEN", "VNM", "ZMB")

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


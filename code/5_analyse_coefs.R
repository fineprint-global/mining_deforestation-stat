
# Evaluate coefficients from fitted models on data summaries.

library("dplyr")

country_sm <- read.csv("output/country_data-summary.csv")
files <- list.files("output/txt/", pattern = "coef")
files <- list.files("output/txt/", pattern = "coef.*_f_base")
files <- list.files("output/txt/", pattern = "coef.*_f_interactions")

files <- list.files("output/txt/", pattern = "coef.*_f_base")
files <- files[1:4]
x <- files[1]

lapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  xs <- seq(info[1, 3], 1e5, length.out = 10000)
  f <- function() {
    xs * y[2, 2] + (xs >= 50000) * xs * y[3, 2]
  }
  png(paste0("output/ex-post/coefs-", sub("coef_([A-Z]+).*", "\\1", x)),
    width = 960, height = 720)
  plot(x = xs, y = f(), type = "l", main = sub("coef_([A-Z]+).*", "\\1", x),
    xlab = "distance", ylab = "forest loss")
  dev.off()
})

(sign <- t(sapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  c(sign(y[2, 4]), sign(y[3, 2]))
})))
rownames(sign) <- sub(".*([A-Z]{3}).*.csv", "\\1", files)
(sign[, 1] == -1) | (sign[, 1] == 1 & sign[, 2] == -1)


# alternative approach: use compare_models_merge() and compare_models_plot() functions:

library("ggplot2")

compare_models_plot(compare_models_merge(path = "output/txt",
               files = c("coef_NIC_f_more_quad.csv", "coef_ZMB_f_more_quad.csv", "coef_GHA_f_more_quad.csv"),
               coef_subs = c("distance_mine", "I(distance_mine^2)")))

compare_models_plot(compare_models_merge(path = "output/txt",
                             files = c("coef_NIC_f_base.csv", "coef_ZMB_f_base.csv", "coef_GHA_f_base.csv",
                                       "coef_NIC_f_interactions.csv", "coef_ZMB_f_interactions.csv", "coef_GHA_f_interactions.csv",
                                       "coef_NIC_f_interactions_quad.csv", "coef_ZMB_f_interactions_quad.csv", "coef_GHA_f_interactions_quad.csv",
                                       "coef_NIC_f_more_quad.csv", "coef_ZMB_f_more_quad.csv", "coef_GHA_f_more_quad.csv"),
                             coef_subs = c("distance_mine_boolTRUE", "distance_mine", "I(distance_mine^2)")))

compare_models_plot(compare_models_merge(path = "output/txt",
                                    files = list.files("output/txt/", pattern = "coef.*_f_base"),
                                    coef_subs = c("distance_mine", "I(distance_mine^2)")))

compare_models_plot(compare_models_merge(path = "output/txt",
                                         files = c("coef_NIC_f_interactions_bool_logdm.csv",
                                                   "coef_ZMB_f_interactions_bool_logdm.csv",
                                                   "coef_GHA_f_interactions_bool_logdm.csv"),
                                         coef_subs = c("distance_mine_log", "distance_mine_boolTRUE")))

compare_models_plot(compare_models_merge(path = "output/txt",
                                         files = c("coef_NIC_f_interactions_bool_quad.csv", "coef_NIC_f_interactions_bool_logdm.csv",
                                                   "coef_ZMB_f_interactions_bool_quad.csv", "coef_ZMB_f_interactions_bool_logdm.csv",
                                                   "coef_GHA_f_interactions_bool_quad.csv", "coef_GHA_f_interactions_bool_logdm.csv"),
                                         coef_subs = c("distance_mine_boolTRUE", "distance_mine", "distance_mine_log", "I(distance_mine^2)")))



# apply functions for fitted values and plot them -------------------------

# select countries we want to cover
data_sm <- read.csv("output/country_data-summary-max.csv")
dat_sum <- data_sm %>% as.data.frame() %>%
  dplyr::mutate(index = rep(seq(1, 6, 1), nrow(data_sm)/6)) %>%
  dplyr::filter(index == 6) %>%
  dplyr::mutate(area_forest_2000  = as.numeric(as.character(area_forest_2000))  / 1000000) %>%
  dplyr::mutate(area_accumulated_forest_loss =  as.numeric(as.character(area_accumulated_forest_loss)) / 1000000) %>%
  dplyr::mutate(relative_forest_loss = area_accumulated_forest_loss / area_forest_2000)
top_rff <- dat_sum %>% dplyr::arrange(-relative_forest_loss)
top_rff <- as.character(top_rff$iso[1:26]) # top 26 have more than 5% loss

# linear decay
data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_base.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_base.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")

# quadratic decay
data.frame(get_fitted(path = "output/txt",
           files <- list.files("output/txt/", pattern = "coef.*_f_interactions_quad.csv"),
           countries = top_rff,
           npred = 50,
           log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_quad.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")


# quadratic decay distance to mine log
data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_quad_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_quad_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")


# logarithmic decay
data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")



# logarithmic decay 2: log all distances
data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logalldist.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt",
                      files <- list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logalldist.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")

# combine
dat_comb <- rbind(
  data.frame(get_fitted(path = "output/txt",
             files <- list.files("output/txt/", pattern = "coef.*_f_base.csv"),
             countries = top_rff,
             npred = 50,
             log_dist = FALSE)) %>%
    dplyr::mutate(model = "base"),
  data.frame(get_fitted(path = "output/txt",
                        files <- list.files("output/txt/", pattern = "coef.*_f_interactions_quad.csv"),
                        countries = top_rff,
                        npred = 50,
                        log_dist = FALSE)) %>%
    dplyr::mutate(model = "int_quad"),
  data.frame(get_fitted(path = "output/txt",
                        files <- list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                        countries = top_rff,
                        npred = 50,
                        log_dist = TRUE)) %>%
    dplyr::mutate(model = "int_bool_logdm")
)

dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line(aes(linetype=model)) +
  ggplot2::ylim(c(-50, 100)) +
  ggplot2::xlim(c(0, 50))

dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), fill = as.character(X3), color = model)) +
  ggplot2::geom_line() +
  ggplot2::ylim(c(-50, 100)) +
  ggplot2::xlim(c(0, 50))



# 100km -------------------------------------------------------------------

dat_comb <- rbind(
  data.frame(get_fitted(path = "output/txt_100km",
                        files <- list.files("output/txt_100km/", pattern = "coef.*_f_base.csv"),
                        countries = top_rff,
                        npred = 50,
                        log_dist = FALSE)) %>%
    dplyr::mutate(model = "base"),
  data.frame(get_fitted(path = "output/txt_100km",
                        files <- list.files("output/txt_100km/", pattern = "coef.*_f_interactions_quad.csv"),
                        countries = top_rff,
                        npred = 50,
                        log_dist = FALSE)) %>%
    dplyr::mutate(model = "int_quad"),
  data.frame(get_fitted(path = "output/txt_100km",
                        files <- list.files("output/txt_100km/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                        countries = top_rff,
                        npred = 50,
                        log_dist = TRUE)) %>%
    dplyr::mutate(model = "int_bool_logdm")
)

dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line(aes(linetype=model)) +
  ggplot2::ylim(c(-50, 100)) +
  ggplot2::xlim(c(0, 50))


dat_comb %>%   ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), fill = as.character(X3), color = model)) +
  ggplot2::geom_line() +
  ggplot2::ylim(c(-50, 100)) +
  ggplot2::xlim(c(0, 50))


data.frame(get_fitted(path = "output/txt_100km",
                      files <- list.files("output/txt_100km/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt_100km",
                      files <- list.files("output/txt_100km/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                      countries = top_rff,
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)))) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(X3 ~ ., scales = "free_y")


# log all distances, compare full vs 100km subset -------------------------

# MDG totally different just because of model selection?

compare_models_merge(path = "output/txt",
                     files = "coef.MDG_f_interactions_bool_logalldist.csv",
                     coef_subs = c("distance_mine_log"))

compare_models_merge(path = "output/txt_100km",
                     files = "coef.MDG_f_interactions_logalldist.csv",
                     coef_subs = c("distance_mine_log"))

data.frame(get_fitted(path = "output/txt",
                      files = list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logalldist.csv"),
                      countries = top_rff[-6], # remove MDG
                      npred = 50,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt_100km",
                      files = list.files("output/txt_100km/", pattern = "coef.*_f_interactions_logalldist.csv"),
                      countries = top_rff[-6], # remove MDG
                      npred = 20,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::labs(title = "coef.*_f_interactions_logalldist") +
  ggplot2::geom_line()


# level-level 50km --------------------------------------------------------

compare_models_merge(path = "output/txt_50km_level",
                     files = "coef.MDG_f_interactions_logalldist.csv",
                     coef_subs = c("distance_mine_log"))

data.frame(get_fitted(path = "output/txt_50km_level",
                      files = list.files("output/txt_50km_level/", pattern = "coef.*_f_interactions_logalldist.csv"),
                      countries = top_rff[-6], # remove MDG
                      npred = 20,
                      log_dist = TRUE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::labs(title = "coef.*_f_interactions_logalldist") +
  ggplot2::geom_line()


data.frame(get_fitted(path = "output/txt_50km_level",
                      files = list.files("output/txt_50km_level/", pattern = "coef.*_f_interactions.csv"),
                      countries = top_rff[-6], # remove MDG
                      npred = 20,
                      log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::labs(title = "coef.*_f_interactions.csv") +
  ggplot2::geom_line()

data.frame(get_fitted(path = "output/txt_50km_level",
                      files = list.files("output/txt_50km_level/", pattern = "coef.*_f_interactions_quad.csv"),
                      countries = top_rff[-6], # remove MDG
                      npred = 20,
                      log_dist = FALSE)) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::labs(title = "coef.*_f_interactions_quad") +
  ggplot2::geom_line()




# 2020-08-04 --------------------------------------------------------------

# countries <- c("AGO", "BRA", "COL", "COG", "CIV", "ECU", "GAB", "GHA", "GTM", "GIN", "GUY", "HND", "IND", "IDN", "KEN", "LBR", "MOZ", "NIC", "PNG", "SLE", "TZA", "VEN", "VNM", "ZMB")

# exclude some countries because of small fit
countries <- c("BRA", "COL", "COG", "CIV", "ECU", "GHA", "GTM", "HND", "IND", "IDN", "KEN", "LBR", "MOZ", "NIC", "SLE", "TZA", "VNM", "ZMB")

# first proposal model:
# - Distance Mine Dummies für 5 und 20km
# - Road Dummy für 5 und 50km
# - Waterway Dummy für 10km
# - Protected Area Dummy für 20km
# - Cropland Dummy für 10km
model_names <- c("f_proposal_log_nonlins")
# model_names <- c("f_proposal_log_nonlins", "f_proposal_log_interactions_nonlins")

### distance mine
info <- compare_models_info(path = "output/txt",
                            files = paste0(paste0("info_", rep(countries, length( model_names)), "_"), 
                                           rep(model_names, each = length(countries)), ".csv"))

df <- compare_models_merge(path = "output/txt",
                           files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                          rep(model_names, each = length(countries)), ".csv"))
compare_models_plot(
  compare_models_merge(path = "output/txt",
                       files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                      rep(model_names, each = length(countries)), ".csv")))
# plot mine coeffs
p_dat <- compare_models_merge(path = "output/txt",
                              files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                             rep(model_names, each = length(countries)), ".csv"),
                              coef_subs = c("distance_mine_log", "I(distance_mine_log * distance_mine_km5)", "I(distance_mine_log * distance_mine_km20)"))

compare_models_plot(p_dat)

# plot various coeffs
p_dat <- compare_models_merge(path = "output/txt",
                              files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                             rep(model_names, each = length(countries)), ".csv"),
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
ggplot2::ggsave("output/plots/coefs/proposal_1.png", 
                plot = p, device = "png", 
                scale = 1, width = 300, height = 200, units = "mm")

# plot all coeffs
p_dat <- compare_models_merge(path = "output/txt",
                              files = paste0(paste0("coef_", rep(countries, length( model_names)), "_"), 
                                             rep(model_names, each = length(countries)), ".csv"))

compare_models_plot(p_dat) 



# IDEAS -------------------------------------------------------------------


# intercept for "distance_mine_boolTRUE"
# include scatter to fitted plot



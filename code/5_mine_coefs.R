
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
library(ggplot2)

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
                                    coef_subs = c("distance_mine_boolTRUE", "distance_mine", "I(distance_mine^2)")))

compare_models_plot(compare_models_merge(path = "output/txt", 
                                         files = c("coef_NIC_f_interactions_bool_logdm.csv", 
                                                   "coef_ZMB_f_interactions_bool_logdm.csv", 
                                                   "coef_GHA_f_interactions_bool_logdm.csv"),
                                         coef_subs = c("distance_mine_log", "distance_mine_boolTRUE")))

compare_models_plot(compare_models_merge(path = "output/txt", 
                                         files = c("coef_NIC_f_interactions_bool_quad.csv", "coef_NIC_f_interactions_bool_logdm.csv",
                                                   "coef_ZMB_f_interactions_bool_quad.csv", "coef_ZMB_f_interactions_bool_logdm.csv", 
                                                   "coef_GHA_f_interactions_bool_quad.csv", "coef_GHA_f_interactions_bool_logdm.csv"),
                                         coef_subs = c("distance_mine", "distance_mine_log", "I(distance_mine^2)")))

# linear decay
dat_lin <- compare_models_merge(path = "output/txt",
                            files = list.files("output/txt/", pattern = "coef.*_f_base.csv"),
                            coef_subs = c("distance_mine"))

p_dat_lin <- data.frame()
for(i in unique(dat$country)){
  
  dat_sub <- dat_lin %>% dplyr::filter(country == i)
  pred <- dat_sub$lm_coef * seq(0, 50, 1)
  dat_sub <- data.frame("forest_loss_fitted" = pred,
                      "distance_mine" = seq(0, 50, 1),
                      "country" = i,
                      "model" = dat_sub$model[1]) 
  p_dat_lin <- dplyr::bind_rows(p_dat_lin, dat_sub)
  
}

p_dat_lin %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted, color = country)) +
  ggplot2::geom_line()

p_dat_lin %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(country ~ ., scales = "free_y")

# quadratic decay
dat_quad <- compare_models_merge(path = "output/txt",
                            files = list.files("output/txt/", pattern = "coef.*_f_interactions_quad.csv"),
                            coef_subs = c("distance_mine", "I(distance_mine^2)"))

p_dat_quad <- data.frame()
for(i in unique(dat$country)){
  
  dat_sub <- dat_quad %>%dplyr::filter(country == i)
  pred <- dat_sub$lm_coef[1] * seq(0, 50, 1) + dat_sub$lm_coef[2] * seq(0, 50, 1)^2
  dat_sub <- data.frame("forest_loss_fitted" = pred,
                        "distance_mine" = seq(0, 50, 1),
                        "country" = i,
                        "model" = dat_sub$model[1]) 
  p_dat_quad <- dplyr::bind_rows(p_dat_quad, dat_sub)
  
}

p_dat_quad %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted, color = country)) +
  ggplot2::geom_line()

p_dat_quad %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(country ~ ., scales = "free_y")



# logarithmic decay
dat_log <- compare_models_merge(path = "output/txt",
                            files = list.files("output/txt/", pattern = "coef.*_f_interactions_bool_logdm.csv"),
                            coef_subs = c("distance_mine_log"))

p_dat_log <- data.frame()
for(i in unique(dat$country)){
  
  dat_sub <- dat_log %>%dplyr::filter(country == i)
  pred <- dat_sub$lm_coef * seq(0, 50, 1)
  dat_sub <- data.frame("forest_loss_fitted" = pred,
                        "distance_mine" = seq(0, 50, 1),
                        "country" = i,
                        "model" = dat_sub$model[1]) 
  p_dat_log <- dplyr::bind_rows(p_dat_log, dat_sub)
  
}

p_dat_log %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted, color = country)) +
  ggplot2::geom_line()

p_dat_log %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(country ~ ., scales = "free_y")

# combine
p_dat_comb <- dplyr::bind_rows(p_dat_lin, p_dat_quad, p_dat_log)
rm(dat_lin, dat_quad, dat_log, p_dat_lin, p_dat_quad, p_dat_log)

p_dat_comb %>% ggplot2::ggplot(aes(x = distance_mine, y = forest_loss_fitted, color = country)) +
  ggplot2::geom_line(aes(linetype=model)) +
  ggplot2::ylim(c(-50, 100))



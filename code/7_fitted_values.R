

# simulated example -------------------------------------------------------


# create artifical results part 1: distance_log coefficient (I set them similar to BRA)
distance_mine_log <- -0.25

# set no of predictions (in m)
npred <- 50000
pred_matrix <- matrix(NA, npred, 2)

# first matrix columns is just a sequence of steps (in m)
pred_matrix[,1] <- seq(1, npred, 1)

# fill matrix with predictions
pred_matrix[,2] <- distance_mine_log * log(seq(1, npred, 1))


library(tidyverse)

source("code/9_helpers.R")

# plot
plot(pred_matrix)

# create artifical results part 2: distance_log times interaction dummy coefficients (I set them similar to BRA)
distance_mine_log_km5 <- -0.05
distance_mine_log_km20 <- -0.05

# fill matrix with predictions
pred_matrix[c(1:5000),2] <- pred_matrix[c(1:5000),2] + distance_mine_log_km5 * log(seq(1, 5000, 1))
pred_matrix[c(1:20000),2] <- pred_matrix[c(1:20000),2] + distance_mine_log_km20 * log(seq(1, 20000, 1))

# plot
plot(pred_matrix, type = "l")


# make it a function ------------------------------------------------------

# see helpers.R

# apply to real results BIM -----------------------------------------------

pred_matrix <- get_fitted2(path = "output/txt", 
                           files = list.files("output/txt/", pattern = "coef.*_f_proposal_log_nonlins.csv"),
                           countries = c("BRA", "IDN", "MYS"), 
                           npred = 50000, breaks = c(5000, 20000))

data.frame(pred_matrix) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "distance from mine", y = "fitted values") +
  ggplot2::theme(legend.title = element_blank())


# compare models ----------------------------------------------------------

pred_matrix <- dplyr::bind_rows(
  data.frame(get_fitted2(path = "output/txt", 
                           files = list.files("output/txt/", pattern = "coef.*_f_proposal_log_nonlins.csv"),
                           countries = c("BRA", "IDN", "MYS"), 
                           npred = 60000, breaks = c(5000, 20000))) %>%
  dplyr::mutate(model = "log_nonlins_2breaks"), 
  data.frame(get_fitted2(path = "output/txt", 
                         files = list.files("output/txt/", pattern = "coef.*_f_base_log_nonlins.csv"),
                         countries = c("BRA", "IDN", "MYS"), 
                         npred = 60000, breaks = c(5000, 10000, 20000, 50000))) %>%
    dplyr::mutate(model = "log_nonlins_4breaks")
)

p <- data.frame(pred_matrix) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line(aes(linetype=model)) +
  ggplot2::geom_hline(yintercept = 0, color = "black") + 
  ggplot2::labs(x = "distance from mine", y = "fitted values") +
  ggplot2::theme(legend.title = element_blank())

ggplot2::ggsave("output/plots/coefs/proposal_1_fitted_BIM.png", 
                plot = p, device = "png", 
                scale = 1, width = 300, height = 200, units = "mm")

# multiple countries ------------------------------------------------------

pred_matrix <- get_fitted2(path = "output/txt", 
                           files = list.files("output/txt/", pattern = "coef.*_f_proposal_log_nonlins.csv"),
                           countries = c("BRA", "COL", "COG", "CIV", "ECU", "GHA", "GTM", "HND", "IND", "IDN", "KEN", "LBR", "MOZ", "NIC", "SLE", "TZA", "VNM", "ZMB"), 
                           npred = 50000, breaks = c(5000, 20000))

p <- data.frame(pred_matrix) %>%
  ggplot2::ggplot(aes(x = as.numeric(as.character(X1)), y = as.numeric(as.character(X2)), color = as.character(X3))) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0, color = "black") + 
  ggplot2::labs(x = "distance from mine", y = "fitted values") +
  ggplot2::theme(legend.title = element_blank())

ggplot2::ggsave("output/plots/coefs/proposal_1_fitted.png", 
                plot = p, device = "png", 
                scale = 1, width = 300, height = 200, units = "mm")



# fitted vs actual deforestation ------------------------------------------

source("code/9_helpers.R")
source("code/9_formulas.R")

# case GHA

# # look at the data # #

# tbl_raw <- readRDS(paste0(path_in, file))
# tbl <- prep_data(tbl_raw, has_forest = TRUE,
#                  sub_eco = "Tropical", geom = TRUE)
# tbl <- filter(tbl, area_accumulated_forest_loss > 0)
# ggplot(tbl) +
#   geom_sf(aes(fill = log(area_accumulated_forest_loss)), lwd = 0) +
#   scale_fill_viridis_c() +
#   labs(fill = "Log loss") +
#   ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
#   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
#         panel.background = element_rect(fill = "transparent"))
# 
# plot(tbl$distance_mine, log(tbl$area_accumulated_forest_loss))

# # load actual regression data and output # #
countries <- read.csv("input/countries.csv")
path_in <- "/mnt/nfs_fineprint/tmp/mining_def/"
files <- paste0(countries$continent, "-", countries$iso, ".rds")
files <- files[grep("AGO|BRA|COD|GUY|HND|IDN|MYS|NIC|ZMB", files)] 

store1 <- list()
store2 <- list()
i <- 1
file <- files[grep("HND", files)]

for(i in seq(formulas)) {

for(file in files){
  
  cat("Plotting", get_iso(file))
  
  load(paste0("output/reg_out_rda/lm_", get_iso(file), "_", names(formulas)[i], ".RData"))
  summary(out_lm1)
  load(paste0("output/reg_out_rda/y_", get_iso(file), "_", names(formulas)[i], ".RData"))
  load(paste0("output/reg_out_rda/X_", get_iso(file), "_", names(formulas)[i], ".RData"))
  
  # # get fitted values # #
  y_hat <- predict(out_lm1)
  # plot(y, y_hat)
  
  # correct bias from log (see https://stats.stackexchange.com/questions/140713/making-predictions-with-log-log-regression-model)
  adj <- sd(y - y_hat)
  y_hat <- 0.5 * adj ^ 2 + y_hat
  # points(y, y_hat, col=2)
  # plot(y, y_hat)
  # abline(0,1, col = "red")
  p <- ggplot2::ggplot(data.frame(y, y_hat), aes(x = y, y = y_hat)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
    ggplot2::labs(title = paste(get_iso(file), names(formulas[i]))) 
  store1[[file]] <- p
  
  X_cf <- X
  X_cf[, "distance_mine_log"] <- mean(X[, "distance_mine_log"])
  X_cf[, "I(distance_mine_log * distance_mine_km5)"] <- 0
  X_cf[, "I(distance_mine_log * distance_mine_km25)"] <- 0
  
  coefs <- coef(out_lm1)
  coefs[is.na(coefs)] <- 0
  
  y_cf <- cbind(1, X_cf) %*% coefs
  adj <- sd(y - y_cf)
  y_cf <- 0.5 * adj ^ 2 + y_cf
  # points(y, y_cf, col=2)
  p <- p +
    ggplot2::geom_point(data = data.frame(y, y_cf), aes(x = y, y = y_cf), shape = 1, color = "green", fill = NA, alpha = 0.5) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
    ggplot2::labs(title = paste(get_iso(file), names(formulas[i]))) 
  store2[[file]] <- p
  
  # X_cf <- X
  # X_cf[, "distance_mine_log"] <- max(X[, "distance_mine_log"])
  # X_cf[, "I(distance_mine_log * distance_mine_km5)"] <- 0
  # X_cf[, "I(distance_mine_log * distance_mine_km25)"] <- 0
  # 
  # coefs <- coef(out_lm1)
  # coefs[is.na(coefs)] <- 0
  # 
  # y_cf <- cbind(1, X_cf) %*% coefs
  # adj <- sd(y - y_cf)
  # y_cf <- 0.5 * adj ^ 2 + y_cf
  
  # # points(y, y_cf, col=3)
  # p <- p +
  #   ggplot2::geom_point(data = data.frame(y, y_cf), aes(x = y, y = y_cf), shape = 1, color = "blue", fill = NA, alpha = 0.5) +
  #   ggplot2::geom_abline(intercept = 0, slope = 1, color = "red")
  
  
}
# 
# p_1 <- do.call(gridExtra::grid.arrange, c(store1, nrow=3))
# ggplot2::ggsave("y_yhat_1_subs.png", 
#                 plot = p_1, device = "png", 
#                 path = paste0("./output/plots/y_yhat_scatter"),
#                 scale = 1, width = 300, height = 300, units = "mm")

p_2 <- do.call(gridExtra::grid.arrange, c(store2, nrow=3))
ggplot2::ggsave("y_yhat_2_subs.png", 
                plot = p_2, device = "png", 
                path = paste0("./output/plots/y_yhat_scatter"),
                scale = 1, width = 300, height = 300, units = "mm")

}



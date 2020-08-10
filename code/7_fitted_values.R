

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



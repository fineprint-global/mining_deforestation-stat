
library("ggplot2")
library("sf")

# BRA = -0.62; IDN = -0.38
mine_coef <- -0.38 # Look this up instead at some point
tbl_raw$predicted <- log(pmax(tbl_raw$distance_mine, 1)) * mine_coef

x <- filter(tbl_raw, area_accumulated_forest_loss > 0)

ggplot(x) +
  geom_sf(aes(fill = log(area_accumulated_forest_loss)), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Log loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))
ggsave(paste0("output/plots/map_loss_", get_iso(file), ".png"), width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = area_accumulated_forest_loss), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))
ggsave(paste0("output/plots/map_loss_level_", get_iso(file), ".png"), width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = predicted), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Coefficient") +
  ggtitle(paste0("Forest loss ~ mine distance (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))
ggsave(paste0("output/plots/map_coefs_", get_iso(file), ".png"), width = 10, height = 10)


# Pick up from 3_models.R

# LM
# out_lm1 <- lm(y ~ X, weights = w)

y_hat <- predict(out_lm1)
adj <- sd(y - y_hat)
y_hat <- 0.5 * adj ^ 2 + y_hat

X_cf <- X
X_cf[, "distance_mine_log"] <- mean(X[, "distance_mine_log"])
X_cf[, "I(distance_mine_log * distance_mine_km5)"] <- 0
X_cf[, "I(distance_mine_log * distance_mine_km25)"] <- 0

coefs <- coef(out_lm1)
coefs[is.na(coefs)] <- 0

y_cf <- cbind(1, X_cf) %*% coefs
adj <- sd(y - y_cf)
y_cf <- 0.5 * adj ^ 2 + y_cf

sum(exp(y))
sum(y_hat_e)
sum(y_cf_e)

# Tobit
yy_hat <- predict(out_tob)
adj <- sd(yy - yy_hat)
yy_hat <- 0.5 * adj ^ 2 + yy_hat

XX_cf <- XX
XX_cf[, "distance_mine_log"] <- max(XX[, "distance_mine_log"])
XX_cf[, "I(distance_mine_log * distance_mine_km5)"] <- 0
XX_cf[, "I(distance_mine_log * distance_mine_km25)"] <- 0

coefs <- coef(out_tob)
coefs[is.na(coefs)] <- 0

yy_cf <- cbind(1, XX_cf) %*% coefs
adj <- sd(yy - yy_cf)
yy_cf <- 0.5 * adj ^ 2 + yy_cf

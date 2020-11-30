
# Check with simulated data
X <- rnorm(1000, 1000, 100)
y <- exp(10 + log(X) * 0.5 + rnorm(1000))

mdl <- lm(log(y) ~ log(X))

distance_mine <- X
distance_mine_alt <- X - 100
relative_change <- (distance_mine - distance_mine_alt) / distance_mine * -1
deforestation <- y

# extract dist_mine coeffs
beta <- coef(mdl)[2]
beta_ci <- confint(mdl)[2, ]

relative <- cbind(
  relative_change * beta_ci[1],
  relative_change * beta,
  relative_change * beta_ci[2])

pred_manual <- cbind(
  relative_change * beta_ci[1] * deforestation,
  relative_change * beta * deforestation,
  relative_change * beta_ci[2] * deforestation)

exp(predict(mdl, interval = "conf"))
exp(predict(mdl, interval = "pred"))
pred_scenario <- exp(predict(mdl, newdata = data.frame(X = X - 100)))

exp(predict(mdl)) - pred_scenario


# Try and predict

source("code/0_prelim.R")

path <- "" # "/home/luckeneder/mining_deforestation-stat/"

list.files(paste0(path, "output/reg_out_rda/"))

load(paste0(path, "output/reg_out_rda/lm_COD_f_base_log_minesize.RData"))
load(paste0(path, "output/reg_out_rda/logit_COD_f_base_log_minesize.RData"))

X <- out_lm1$model$X
y <- out_lm1$model$y
y_glm <- out_glm$model$y_glm

forest_2000 <- sum(exp(X[, "area_forest_2000_log"]))
lost <- sum(exp(y) - 1)


X_new <- X
# Move a 100m closer, minimum is on-site
X_new[, "distance_mine_log"] <- log(pmax(exp(X[, "distance_mine_log"]) - 100, 1))

out_lm2 <- out_lm1
out_lm2$model$X <- X_new

y_0 <- predict(out_lm1)
y_1 <- predict(out_lm2)
y_1_int <- predict(out_lm2, interval = "conf")

summary(y_0 - y_1)
summary(y - y_1)

act_lm <- sum(exp(y) - 1)
# Truncate manually
pred_lm_0 <- sum(exp(pmin(13.65615, pmax(0, y_0))))
pred_lm_1 <- sum(exp(pmin(13.65615, pmax(0, y_1))))
pred_low <- sum(exp(pmin(13.65615, pmax(0, y_1_int[, "lwr"]))))
pred_upp <- sum(exp(pmin(13.65615, pmax(0, y_1_int[, "upr"]))))

cat(paste0("Difference: ", round((-pred_lm_0 + pred_lm_1) / 1e6), "km2\n",
  "Bounds: ", round((-pred_lm_0 + pred_low) / 1e6), "km2 - ",
  round((-pred_lm_0 + pred_upp) / 1e6), "km2"), "\n")

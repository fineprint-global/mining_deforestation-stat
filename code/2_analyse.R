
# Dependencies -----

library("glmnet")
library("cem")
library("BMS")

stopifnot(exists("tbl"))

# CEM -----

imb1 <- imbalance(tbl$treated, as.data.frame(tbl),
  drop = c("countries", "id_grid"))

out1 <- cem(treatment = "treated", data = tbl,
  drop = drop_but(tbl,
    c("elevation", "slope", "area_forest_2000", "dist_road", "pop_2000",
    "dist_waterway"),
  keep.all = TRUE)

lm1 <- att(out1, area_accumulated_forest_loss ~
  poly(distance_mine, 2) + slope + elevation + area_forest_2000 + pop_2000,
  data = as.data.frame(tbl))
summary(lm1)

lm2 <- lm(area_accumulated_forest_loss ~
  poly(distance_mine, 3) + slope + elevation + area_forest_2000 + pop_2000,
  data = as.data.frame(tbl), weights = out1$w)
summary(lm2)

# Models -----

form1 <- area_accumulated_forest_loss ~
  poly(distance_mine, 5) + poly(elevation, 5) + poly(slope, 5) +
  soilgrid + esa_cci_2000 + pop_2000 +
  poly(area_forest_2000, 5) + poly(dist_road, 5) + dist_waterway +
  I(elevation * slope) + I(pop_2000 * distance_mine) +
  I(pop_2000 * area_forest_2000) + I(elevation * slope) +
  I(elevation * area_forest_2000) + I(slope * area_forest_2000)

y <- tbl[["area_accumulated_forest_loss"]]
X <- model.matrix(form1, data = tbl)[, -1] # Screw the constant
X <- scale(X)

cv_ridge <- cv.glmnet(x = X, y = y, weights = NULL, alpha = 0)
out_ridge <- glmnet(x = X, y = y, weights = NULL,
  alpha = 0, lambda = cv_ridge[["lambda.1se"]])
coef(out_ridge)

cv_lasso <- cv.glmnet(x = X, y = y, weights = NULL, alpha = 0)
out_lasso <- glmnet(x = X, y = y, weights = NULL,
  alpha = 0, lambda = cv_lasso[["lambda.1se"]])
coef(out_lasso)

out_lm <- lm(y ~ X)
sm(out_lm)

out_bma <- BMS::bms(cbind(y, X), user.int = FALSE)
sm(out_bma)

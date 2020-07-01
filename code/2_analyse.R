
# Dependencies -----

library("glmnet")
library("cem")
library("BMS")
library("grf")

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
  poly(distance_mine, 3) + poly(elevation, 3) + poly(slope, 3) +
  poly(pop_2000, 3) + poly(area_forest_2000, 3) +
  poly(dist_road, 3) + poly(dist_waterway, 3) +
  soilgrid_grouped + esa_cci_2000_grouped +
  I(elevation * slope) +
  I(pop_2000 * distance_mine) + I(pop_2000 * area_forest_2000) +
  I(area_forest_2000 * elevation) + I(area_forest_2000 * slope) +
  I(dist_road * pop_2000) + I(dist_road * area_forest_2000)

form2 <- area_accumulated_forest_loss ~
  distance_mine + elevation + slope +
  pop_2000 + area_forest_2000 +
  dist_road + dist_waterway +
  soilgrid_grouped + esa_cci_2000_grouped

y <- tbl[["area_accumulated_forest_loss"]]
X <- model.matrix(form1, data = tbl)[, -1] # Screw the constant
X <- scale(X)

cv_ridge <- cv.glmnet(x = X, y = y, weights = NULL, alpha = 0)
out_ridge <- glmnet(x = X, y = y, weights = NULL, alpha = 0)

coef(out_ridge, exact = TRUE, s = cv_ridge[["lambda.min"]])
plot(out_ridge)

cv_lasso <- cv.glmnet(x = X, y = y, weights = NULL, alpha = 1)
out_lasso <- glmnet(x = X, y = y, weights = NULL, alpha = 1)

coef(out_lasso, exact = TRUE, s = cv_lasso[["lambda.min"]])
plot(out_lasso)

out_lm <- lm(y ~ X)
sm(out_lm)

out_bma <- BMS::bms(cbind(y, X), user.int = FALSE)
print(out_bma)

out_rf <- causal_forest(X = model.matrix(form2, data = tbl),
  Y = y, W = tbl[["distance_mine"]], sample.weights = NULL)

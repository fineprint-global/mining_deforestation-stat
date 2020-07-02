
library("glmnet") # Penalized regression
library("BMS") # BMA 1
library("BMA") # BMA 2
library("grf") # Causal forest

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

form1 <- area_accumulated_forest_loss ~
  distance_mine + elevation + slope +
  pop_2000 + area_forest_2000 +
  dist_road + dist_waterway +
  soilgrid_grouped + esa_cci_2000_grouped

y <- tbl[["area_accumulated_forest_loss"]]
X <- model.matrix(form1, data = tbl)[, -1] # Screw the constant
X <- scale(X)

cv_lasso <- cv.glmnet(x = X, y = y, weights = NULL, alpha = 1)
out_lasso <- glmnet(x = X, y = y, weights = NULL, alpha = 1)

coef(out_lasso, exact = TRUE, s = cv_lasso[["lambda.min"]])
plot(out_lasso)

out_lm <- lm(y ~ X)
summary(out_lm)

out_bma <- BMS::bms(cbind(y, X), user.int = FALSE)
print(out_bma)

out_bma <- bicreg(X, y, wt = rep(1, nrow(X)))
summary(out_bma)

out_rf <- causal_forest(X = model.matrix(form2, data = tbl),
  Y = y, W = tbl[["distance_mine"]], sample.weights = NULL)
average_partial_effect(out_rf)

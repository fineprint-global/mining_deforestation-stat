
library("grf")

i <- 1

y <- tbl[[as.character(formulas[[i]][[2]])]]
X <- model.matrix(formulas[[i]], data = tbl)[, -1] # no need for constant here
if(SCALE_CENTER) {X <- scale(X)}
X <- X[, !apply(X, 2, function(x) all(is.na(x)))]
w <- out_cem[["w"]]

out_grf <- causal_forest(X = X[, -1], Y = y, W = X[, 1], sample.weights = w)
saveRDS(out_grf, "grf.rds")

y_glm <- tbl$area_accumulated_forest_loss / tbl$area
y_glm <- pmin(y_glm, 1)
out_glm <- glm(y_glm ~ X, weights = w, family = "binomial")



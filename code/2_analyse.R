
# Dependencies -----

library("cem") # Matching

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}

# CEM -----

# out_imb <- imbalance(tbl$treated, as.data.frame(tbl),
#   drop = c("countries", "id_grid"))

out_cem <- cem(treatment = "treated", data = tbl,
  drop = drop_but(tbl,
    c("elevation", "slope", "area_forest_2000", "dist_road", "pop_2000",
    "dist_waterway", "soilgrid_grouped", "esa_cci_2000_grouped")),
  keep.all = TRUE)

# Models -----

form1 <- area_accumulated_forest_loss ~
  distance_mine + I(distance_mine * treated) + elevation + slope +
  pop_2000 + area_forest_2000 +
  dist_road + dist_waterway + distance_protected_area +
  soilgrid_grouped + esa_cci_2000_grouped

y <- tbl[["area_accumulated_forest_loss"]]
X <- model.matrix(form1, data = tbl)[, -1] # Screw the constant
X <- scale(X)

# LM -----

out_lm1 <- lm(y ~ X, weights = out_cem[["w"]])
summary(out_lm1)
out_lm2 <- lm(y ~ X, weights = NULL)
summary(out_lm2)

# Penalized regression -----

library("glmnet")

cv_lasso <- cv.glmnet(x = X, y = y, weights = out_cem[["w"]], alpha = 1)
out_lasso <- glmnet(x = X, y = y, weights = out_cem[["w"]], alpha = 1)

# Outputs -----

png(paste0("output/plots/lasso_simple_", 
  sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
plot(out_lasso, label = TRUE)
dev.off()

readr::write_csv(tibble(
  "vars" = c("constant", colnames(X)),
  "lm_coef" = coef(out_lm1),
  "lm_unweighted_coef" = coef(out_lm2),
  "lasso_cv_coef" = (coef(out_lasso, exact = TRUE, s = cv_lasso[["lambda.min"]]))[, 1],
  "lasso_n_coef" = apply(coef(out_lasso), 1, function(x) sum(x < 0.01))),
  path = paste0("output/txt/coef_", sub(".*([A-Z]{3}).rds", "\\1", file), ".csv"))

readr::write_csv(tibble(
  "N" = sum(out_cem[["w"]]),
  "N_full" = nrow(tbl),
  "N_treated" = out_cem[["tab"]][1, 2],
  "N_untreated" = out_cem[["tab"]][1, 1],
  "N_tr_matched" = out_cem[["tab"]][2, 2],
  "N_untr_unmatched" = out_cem[["tab"]][2, 1],
  "BIC" = BIC(out_lm1),
  "BIC_unweighted" = BIC(out_lm2)),
  path = paste0("output/txt/info_", sub(".*([A-Z]{3}).rds", "\\1", file), ".csv"))

rm(X, y, out_lm1, out_lm2, out_lasso); gc()

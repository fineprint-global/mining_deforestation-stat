
# Fit specified models with linear and penalised regression. Save coefficients
# and summary statistics for later use.

# Dependencies -----

library("glmnet") # LASSO

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}
if(!exists("out_cem")) {stop("Please provide matched data in `out_cem`.")}


for(i in seq(formulas)) {

  # Models -----
  cat("Running model ", names(formulas)[i], ".\n", sep = "")

  y <- tbl[[as.character(formulas[[i]][[2]])]]
  X <- model.matrix(formulas[[i]], data = tbl)[, -1] # no need for constant here
  X <- scale(X)
  X <- X[, !apply(X, 2, function(x) all(is.na(x)))]


  cat("Fitting models with least squares.\n")


  out_lm1 <- lm(y ~ X, weights = out_cem[["w"]])
  summary(out_lm1)
  # out_lm2 <- lm(y ~ X, weights = NULL)
  # summary(out_lm2)

  # Penalized regression -----
  cat("Fitting penalised models.\n")

  cv_lasso <- cv.glmnet(x = X, y = y, weights = out_cem[["w"]], alpha = 1)
  out_lasso <- glmnet(x = X, y = y, weights = out_cem[["w"]], alpha = 1)

  # Outputs -----

  # png(paste0("output/plots/lasso_simple",
  #   get_iso(file), ".png"), width = 960, height = 720)
  # plot(out_lasso, label = TRUE)
  # dev.off()

readr::write_csv(tibble(
  "vars" = c("constant", colnames(X)),
  "lm_coef" = coef(out_lm1),
  "lm_se" = sqrt(diag(vcov(out_lm1))),
  # "lm_unweighted_coef" = coef(out_lm2),
  # "lm_unweighted_se" = sqrt(diag(vcov(out_lm2))),
  "lasso_cv_coef" = (coef(out_lasso, exact = TRUE,
    s = cv_lasso[["lambda.min"]]))[, 1],
  "lasso_n_0" = apply(coef(out_lasso), 1, function(x) sum(x < 0.01 & x > -0.01)),
  "country" = rep(get_iso(file), length(coef(out_lm1))),
  "model" = rep(names(formulas)[i], length(coef(out_lm1)))),
  path = paste0("output/txt/coef_", get_iso(file), "_",
    names(formulas)[i], ".csv"))

readr::write_csv(tibble(
  "N" = sum(out_cem[["w"]]),
  "N_full" = nrow(tbl),
  "N_treated" = out_cem[["tab"]][1, 2],
  "N_untreated" = out_cem[["tab"]][1, 1],
  "N_tr_matched" = out_cem[["tab"]][2, 2],
  "N_untr_unmatched" = out_cem[["tab"]][2, 1],
  "BIC" = BIC(out_lm1),
  # "BIC_unweighted" = BIC(out_lm2),
  "R2" = summary(out_lm1)$r.squared),
  path = paste0("output/txt/info_", get_iso(file), "_",
    names(formulas)[i], ".csv"))

# rm(X, y, out_lm1, out_lm2, out_lasso, cv_lasso); gc()
rm(X, y, out_lm1, out_lasso, cv_lasso); gc()

}

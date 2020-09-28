
# Fit specified models with linear and penalised regression. Save coefficients
# and summary statistics for later use.

# Dependencies -----

if(CALC_LASSO) {library("glmnet")}
if(CALC_TOBIT) {library("AER")}

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}
if(!exists("out_cem")) {stop("Please provide matched data in `out_cem`.")}


i <- 1
for(i in seq(formulas)) {

  # Models -----
  cat("Running model ", names(formulas)[i], ".\n", sep = "")

  y <- tbl[[as.character(formulas[[i]][[2]])]]
  X <- model.matrix(formulas[[i]], data = tbl)[, -1] # no need for constant here
  if(SCALE_CENTER) {X <- scale(X)}
  X <- X[, !apply(X, 2, function(x) all(is.na(x)))]
  w <- out_cem[["w"]]

  cat("Fitting models with least squares.\n")

  out_lm1 <- lm(y ~ X, weights = w)
  # summary(out_lm1)
  
  # Trucated tobit regression -----
  if(CALC_TOBIT) {
    cat("Fitting tobit model.\n")
    
    yy <- y[w > 0]
    XX <- X[w > 0, ]
    out_tob <- AER::tobit(yy ~ XX, weights = w[w > 0])
    # Tobit is needy about 0s in the data
    vc <- vcov(out_tob)
    XX <- XX[, (diag(vc) != 0)[c(-1, -ncol(vc))]]
    
    out_tob <- AER::tobit(yy ~ XX, weights = w[w > 0], robust = TRUE)
    summary(out_tob)
    
    tmp <- coef(out_tob)
    pos <- match(substr(names(tmp[-1]), 3, nchar(names(tmp[-1]))), colnames(X))
    tob_coef <- rep(NA_real_, 1 + ncol(X))
    tob_coef[c(1, pos + 1)] <- tmp
    
    tmp <- diag(vcov(out_tob))
    tob_se <- rep(NA_real_, 1 + ncol(X))
    tob_se[c(1, pos + 1)] <- sqrt(tmp[-length(tmp)])
  }

  # Penalized regression -----
  if(CALC_LASSO) {
    cat("Fitting penalised models.\n")

    cv_lasso <- cv.glmnet(x = X, y = y, weights = w, alpha = 1)
    out_lasso <- glmnet(x = X, y = y, weights = w, alpha = 1)
  }

  # Outputs -----

  # if(CALC_LASSO) {
  #   png(paste0("output/plots/lasso_simple",
  #     get_iso(file), ".png"), width = 960, height = 720)
  #   plot(out_lasso, label = TRUE)
  #   dev.off()
  # }

  readr::write_csv(tibble(
    "vars" = c("constant", colnames(X)),
    "lm_coef" = coef(out_lm1),
    "lm_se" = sqrt(diag(vcov(out_lm1))),
    "tob_coef" = if(CALC_TOBIT) tob_coef else NA,
    "tob_se" = if(CALC_TOBIT) tob_se else NA,
    "lasso_cv_coef" = if(CALC_LASSO) (coef(out_lasso, exact = TRUE,
      s = cv_lasso[["lambda.min"]]))[, 1] else NA,
    "lasso_n_0" = if(CALC_LASSO) apply(coef(out_lasso), 1, function(x) 
      sum(abs(x) > 0.01)) else NA,
    "country" = rep(get_iso(file), length(coef(out_lm1))),
    "model" = rep(names(formulas)[i], length(coef(out_lm1)))),
    path = paste0("output/txt/coef_", get_iso(file), "_",
      names(formulas)[i], ".csv"))

  readr::write_csv(tibble(
    "country" = get_iso(file),
    "model" = names(formulas)[i],
    "N" = sum(w),
    "N_full" = nrow(tbl),
    "N_treated" = out_cem[["tab"]][1, 2],
    "N_untreated" = out_cem[["tab"]][1, 1],
    "N_tr_matched" = out_cem[["tab"]][2, 2],
    "N_untr_unmatched" = out_cem[["tab"]][2, 1],
    "BIC" = BIC(out_lm1),
    "BIC_tobit" = if(CALC_TOBIT) BIC(out_tob) else NA,
    "R2" = summary(out_lm1)$r.squared),
    path = paste0("output/txt/info_", get_iso(file), "_",
      names(formulas)[i], ".csv"))
}


# Fit specified models with linear and penalised regression. Save coefficients
# and summary statistics for later use.
# Produces:
#   - out_lm1, (out_glm), (out_tob), (out_lasso, cv_lasso)

# Dependencies ---

if(CALC_LASSO) {library("glmnet")}
if(CALC_TOBIT) {library("AER")}

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}
if(!exists("out_cem")) {stop("Please provide matched data in `out_cem`.")}
if(!exists("formulas")) {stop("Please provide specifications in `formulas`.")}


# Run ---

# i <- 1
for(i in seq(formulas)) {

  # Models -----

  cat("Running model ", names(formulas)[i], ".\n", sep = "")

  y <- tbl[[as.character(formulas[[i]][[2]])]]
  X <- model.matrix(formulas[[i]], data = tbl)[, -1] # no need for constant

  if(SCALE_CENTER) {X <- scale(X)}

  NA_cols <- apply(X, 2, function(x) all(is.na(x)))
  if(any(NA_cols)) {warning("Found NA values in the design matrix.")}
  X <- X[, !NA_cols] # kick NAs
  w <- out_cem[["w"]]

  cat("Fitting model with least squares.\n")

  out_lm1 <- lm(y ~ X, weights = w)
  # summary(out_lm1)

  # Store outputs and data - To-do: Should really be RDS
  if(SAVE_MDL) {
    save(out_lm1, file = paste0("output/reg_out_rda/lm_",
      get_iso(file), "_", names(formulas)[i], ".RData"))
  }
  if(SAVE_MAT) {
    save(y, file = paste0("output/reg_out_rda/y_",
      get_iso(file), "_", names(formulas)[i], ".RData"))
    save(X, file = paste0("output/reg_out_rda/X_",
      get_iso(file), "_", names(formulas)[i], ".RData"))
  }


  # Logistic regression ---

  if(CALC_LOGIT) {
    cat("Fitting logit model.\n")

    if(stringr::str_detect(as.character(formulas[[i]][[2]]), "log")) {
      y_nominator <- exp(tbl[[as.character(formulas[[i]][[2]])]])-1} else {
      y_nominator <- tbl[[as.character(formulas[[i]][[2]])]]}
    y_glm <- as.numeric(y_nominator / tbl$area); rm(y_nominator)
    y_glm <- pmin(y_glm, 1)

    # To-do: Add robust standard errors
    out_glm <- glm(y_glm ~ X, weights = w, family = "binomial")
    # summary(out_glm)

    # Store outputs and data - To-do: Should really be RDS
    if(SAVE_MDL) {
      save(out_glm, file = paste0("output/reg_out_rda/logit_",
        get_iso(file), "_", names(formulas)[i], ".RData"))
    }
    if(SAVE_MAT) {
      save(y_glm, file = paste0("output/reg_out_rda/y_logit_",
        get_iso(file), "_", names(formulas)[i], ".RData"))
    }
  }


  # Trucated tobit regression ---

  if(CALC_TOBIT) {
    cat("Fitting tobit model.\n")

    yy <- y[w > 0]
    XX <- X[w > 0, ]
    out_tob <- AER::tobit(yy ~ XX, weights = w[w > 0])
    # Tobit is needy about 0s in the data
    vc <- vcov(out_tob)
    XX <- XX[, (diag(vc) != 0)[c(-1, -ncol(vc))]]

    out_tob <- AER::tobit(yy ~ XX, weights = w[w > 0], robust = TRUE)
    # summary(out_tob)

    tmp <- coef(out_tob)
    pos <- match(substr(names(tmp[-1]), 3, nchar(names(tmp[-1]))), colnames(X))
    tob_coef <- rep(NA_real_, 1 + ncol(X))
    tob_coef[c(1, pos + 1)] <- tmp

    tmp <- diag(vcov(out_tob))
    tob_se <- rep(NA_real_, 1 + ncol(X))
    tob_se[c(1, pos + 1)] <- sqrt(tmp[-length(tmp)])
  }


  # Penalized regression ---

  if(CALC_LASSO) {
    cat("Fitting penalised models.\n")

    cv_lasso <- cv.glmnet(x = X, y = y, weights = w, alpha = 1)
    out_lasso <- glmnet(x = X, y = y, weights = w, alpha = 1)
  }


  # Outputs ---

  readr::write_csv(tibble(
    "vars" = c("constant", colnames(X)),
    "lm_coef" = coef(out_lm1),
    "lm_se" = sqrt(diag(vcov(out_lm1))),
    "tob_coef" = if(CALC_TOBIT) tob_coef else NA,
    "tob_se" = if(CALC_TOBIT) tob_se else NA,
    "logit_coef" = if(CALC_LOGIT) coef(out_glm) else NA,
    "logit_se" = if(CALC_LOGIT) sqrt(diag(vcov(out_glm))) else NA,
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
    "BIC_logit"= if(CALC_LOGIT) BIC(out_glm) else NA,
    "R2" = summary(out_lm1)$r.squared),
    path = paste0("output/txt/info_", get_iso(file), "_",
      names(formulas)[i], ".csv"))
}

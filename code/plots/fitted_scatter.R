
# Depends on 1_prep.R

library("ggplot2")

for(i in seq(formulas)) {

  cat("Running for ", names(formulas[i]), ".\n", sep = "")
  store_FS <- list()

  for(file in files){

  cat("Plotting ", get_iso(file), ".\n", sep = "")


# lm ----------------------------------------------------------------------

  load(paste0("output/reg_out_rda/lm_", get_iso(file), "_", names(formulas)[i], ".RData"))
  load(paste0("output/reg_out_rda/y_", get_iso(file), "_", names(formulas)[i], ".RData"))
  load(paste0("output/reg_out_rda/X_", get_iso(file), "_", names(formulas)[i], ".RData"))

  # get fitted values # #
  y_hat <- predict(out_lm1)

  # correct bias from log (see https://stats.stackexchange.com/questions/140713/making-predictions-with-log-log-regression-model)
  adj <- sd(y - y_hat)
  y_hat <- 0.5 * adj ^ 2 + y_hat

  # plot scatter fitted vs actual
  p <- ggplot2::ggplot(data.frame(y, y_hat), aes(x = y, y = y_hat)) +
    ggplot2::geom_point(alpha = 0.007, colour="purple")
  # store_FS[[file]] <- p
  #
  # }
  # cat("Exporting figure... \n")
  # p_fitted <- do.call(gridExtra::grid.arrange, c(store_FS, nrow=3))
  # ggplot2::ggsave(paste0("y_yhat_subs_OLS_", names(formulas[i]) ,".png"),
  #                 plot = p_fitted, device = "png",
  #                 path = paste0("./output/plots/y_yhat_scatter"),
  #                 scale = 1, width = 300, height = 300, units = "mm")


# construct counterfactual (here: all observations have mean distance to mines)
X_cf <- X
X_cf[, "distance_mine_log"] <- mean(X[, "distance_mine_log"])
# X_cf[, "I(distance_mine_log * distance_mine_km5)"] <- 0
# X_cf[, "I(distance_mine_log * distance_mine_km25)"] <- 0

coefs <- coef(out_lm1)
coefs[is.na(coefs)] <- 0

y_cf <- cbind(1, X_cf) %*% coefs
adj <- sd(y - y_cf)
y_cf <- 0.5 * adj ^ 2 + y_cf

# add new fitted to scatterplot
p <- p +
  ggplot2::geom_point(data = data.frame(y, y_cf), aes(x = y, y = y_cf), shape = 1, color = "green", alpha = 0.007, fill = NA) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = paste(get_iso(file), names(formulas[i])))
store_FS[[file]] <- p


}
    cat("Exporting figure... \n")
    p_fitted <- do.call(gridExtra::grid.arrange, c(store_FS, nrow=3))
    ggplot2::ggsave(paste0("y_yhat_subs_OLS_counterfact_", names(formulas[i]) ,".png"),
                    plot = p_fitted, device = "png",
                    path = paste0("./output/plots/y_yhat_scatter"),
                    scale = 1, width = 300, height = 300, units = "mm")

    gc()

# logit -------------------------------------------------------------------

#   load(paste0("output/reg_out_rda/logit_", get_iso(file), "_", names(formulas)[i], ".RData"))
#   load(paste0("output/reg_out_rda/y_logit_", get_iso(file), "_", names(formulas)[i], ".RData"))
#   load(paste0("output/reg_out_rda/X_", get_iso(file), "_", names(formulas)[i], ".RData"))
#
#   # # get fitted values # #
#   y_hat <- predict.glm(out_glm, type = "response")
#
#   # plot scatter fitted vs actual
#   p <- ggplot2::ggplot(data = data.frame(y_glm, y_hat), aes(x = y_glm, y = y_hat)) +
#     ggplot2::geom_point(alpha = 0.007, colour="purple") +
#     ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
#     ggplot2::labs(title = paste(get_iso(file), names(formulas[i])))
#   store_FS[[file]] <- p
#
#
# }
#
# cat("Exporting figure... \n")
# p_fitted <- do.call(gridExtra::grid.arrange, c(store_FS, nrow=3))
# ggplot2::ggsave(paste0("y_yhat_subs_logit_", names(formulas[i]) ,".png"),
#                 plot = p_fitted, device = "png",
#                 path = paste0("./output/plots/y_yhat_scatter"),
#                 scale = 1, width = 400, height = 400, units = "mm")
#
# gc()


}

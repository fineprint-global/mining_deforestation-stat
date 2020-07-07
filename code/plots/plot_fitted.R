
xs <- seq(0, max(tbl[["distance_mine"]]), length.out = 1000)
smpl <- sample(nrow(tbl), min(10000, nrow(tbl) / 4))

png(paste0("output/plots/fitted_",get_iso(file), "_",
  names(formulas)[i], ".png"), width = 960, height = 720)

ys = xs * coef(out_lm1)[2] + xs ^ 2 * coef(out_lm1)[3] + xs ^ 0.5 * coef(out_lm1)[4]

op <- par(mfrow = c(2, 1))
plot(x = tbl[["distance_mine"]][smpl],
  y = tbl[["area_accumulated_forest_loss"]][smpl],
  col = "grey", xlab = "Distance to mine", ylab = "Forest loss",
  main = paste0("Fitted - ", get_iso(file)),
  ylim = c(0, min(max(ys) * 5, max(tbl[["area_accumulated_forest_loss"]]))))
plot(xs, ys, lwd = 2, type = "l", main = "Fitted coefficients",
  xlab = "Distance to mine", ylab = "Forest loss",)
par(op)

dev.off()

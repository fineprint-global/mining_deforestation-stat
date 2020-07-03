
library("splines")

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}

cat("Fitting naive splines.\n")

# Plot y ~ X

# Forest > 0
data <- tbl[tbl$area_accumulated_forest_loss > 0, ]
xs <- seq(0, max(data[["distance_mine"]]), length.out = 1000)

out_spl <- lm(area_accumulated_forest_loss ~ 
    bs(distance_mine), data = data)
smpl <- sample(nrow(data), min(10000, nrow(data) / 4))

png(paste0("output/plots/spline-forested_", 
  sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
pred <- predict(out_spl, newdata = list(distance_mine = xs))
plot(x = data[["distance_mine"]][smpl], 
  y = data[["area_accumulated_forest_loss"]][smpl],
  col = "grey", xlab = "Distance to mine", ylab = "Forest loss", 
  main = paste0("Spline base - ", gsub(".*([A-Z]{3}).rds", "\\1", file)),
  ylim = c(0, min(max(pred) * 5, max(data[["area_accumulated_forest_loss"]]))))
points(xs, pred, col = "darkgreen", lwd = 2, type = "l")
grid()
dev.off()

rm(data, out_spl); gc()

# Matched
data <- tbl
xs <- seq(0, max(data[["distance_mine"]]), length.out = 1000)

out_spl <- lm(area_accumulated_forest_loss ~ 
    bs(distance_mine), data = data, weights = out_cem[["w"]])
smpl <- sample(nrow(data), min(10000, nrow(data) / 4))

png(paste0("output/plots/spline-matched_", 
  sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
pred <- predict(out_spl, newdata = list(distance_mine = xs))
plot(x = data[["distance_mine"]][smpl], 
  y = data[["area_accumulated_forest_loss"]][smpl],
  col = "grey", xlab = "Distance to mine", ylab = "Forest loss", 
  main = paste0("Spline matched - ", gsub(".*([A-Z]{3}).rds", "\\1", file)),
  ylim = c(0, min(max(pred) * 5, max(data[["area_accumulated_forest_loss"]]))))
points(xs, pred, col = "darkgreen", lwd = 2, type = "l")
grid()
dev.off()

rm(data, out_spl); gc()

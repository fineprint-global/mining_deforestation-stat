
# Fit causal forests on the data

# Dependencies ---

source("code/0_prelim.R")

library("grf")
dir.create("output/grf", FALSE)

# Settings ---

STORED_CEM <- TRUE # if TRUE load existing matches
SCALE_CENTER <- FALSE # if TRUE scale and center explanatories

f <- area_accumulated_forest_loss_2019 ~
  distance_mine + elevation + slope + I(elevation * slope) +
  pop_2000 + area_forest_2000 +
  dist_road_log + dist_waterway_log +
  distance_protected_area_log + distance_cropland_2000_log +
  soilgrid_grouped + esa_cci_2000 + biomes_2017


# Run ---

file <- "IDN"
for(file in files) {

  cat("Running for ", get_iso(file), ".\n", sep = "")

  source("code/2_data.R")
  source("code/3_cem.R")

  y <- tbl[[as.character(formulas[[i]][[2]])]]
  X <- model.matrix(f, data = tbl)[, -1] # no need for constant

  if(SCALE_CENTER) {X <- scale(X)}

  NA_cols <- apply(X, 2, function(x) all(is.na(x)))
  if(any(NA_cols)) {warning("Found NA values in the design matrix.")}
  X <- X[, !NA_cols] # kick NAs
  w <- out_cem[["w"]]

  out_grf <- causal_forest(X = X[, -1], Y = y, W = X[, 1],
    sample.weights = w)
  saveRDS(out_grf, paste0("output/grf/GRF-", get_iso(file), ".rds"))

  var_imp <- out_grf %>%
    variable_importance() %>%
    as.data.frame() %>%
    mutate(variable = colnames(out_grf$X.orig)) %>%
    arrange(desc(V1))
  write.csv(var_imp, file = paste0("output/grf/VARIMP-", get_iso(file), ".csv"))

  APE <- average_partial_effect(out_grf)
  write.csv(APE, file = paste0("output/grf/APE-", get_iso(file), ".csv"))
}


# Mess about ---

preds <- predict(out_grf, estimate.variance = TRUE)

test <- as.data.frame(X)
test$preds <- preds$predictions

library("ggplot2")

p1 <- ggplot(test, aes(x = dist_road_log, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p2 <- ggplot(test, aes(x = pop_2000, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p3 <- ggplot(test, aes(x = area_forest_2000, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p4 <- ggplot(test, aes(x = distance_protected_area_log, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

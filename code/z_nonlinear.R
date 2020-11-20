
library("grf")

files <- files[grep("AGO|BRA|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MOZ|MYS|NIC|PNG|SLE|TZA|VEN|VNM|ZMB", files)]
files <- files[grep("AGO|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|HND|KEN|LBR|MOZ|NIC|PNG|SLE|TZA|VEN|VNM|ZMB|IND|BRA", files)]
file <- files[grep("IDN", files)]
for(file in files) {

cat("Running for ", get_iso(file), ".\n", sep = "")

tbl_raw <- readRDS(paste0(path_in, file))
tbl <- prep_data(tbl_raw, has_forest = FALSE,
  sub_eco = "Tropical", geom = FALSE)

# Add variables
tbl <- add_vars(tbl,
  treated = c(-1, 5e4),
  dist_log = TRUE,
  dist_bool = TRUE, # _bool at 1km, then suffixed
  dist_decay = 0.5)

out_cem <- readRDS(
  paste0("/home/luckeneder/mining_deforestation-stat/input/cem/", 
  get_iso(file), ".rds"))

y <- tbl[["area_accumulated_forest_loss_2010"]]
X <- model.matrix(area_accumulated_forest_loss_2010 ~ 
  distance_mine + elevation + slope + I(elevation * slope) +
  pop_2000 + area_forest_2000 + 
  dist_road_log + dist_waterway_log +
  distance_protected_area_log + distance_cropland_2000_log + 
  soilgrid_grouped + esa_cci_2000 + biomes_2017, 
  data = tbl)[, -1] # no need for constant here
if(SCALE_CENTER) {X <- scale(X)}
X <- X[, !apply(X, 2, function(x) all(is.na(x)))]
w <- out_cem[["w"]]

out_grf <- causal_forest(X = X[, -1], Y = y, W = X[, 1], sample.weights = w)
saveRDS(out_grf, paste0("output/grf/", get_iso(file), ".rds"))

var_imp <- out_grf %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(out_grf$X.orig)) %>% 
  arrange(desc(V1))
write.csv(var_imp, file = paste0("output/grf/VARIMP-", get_iso(file), ".csv"))

APE <- average_partial_effect(out_grf)
write.csv(ATE, file = paste0("output/grf/APE-", get_iso(file), ".csv"))
}

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

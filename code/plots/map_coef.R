
library("ggplot2")
library("sf")

file <- "BRA"

source("code/0_prelim.R")


source("code/2_data.R")

# BRA = -0.306; IDN = -0.227; COD -0.141
mine_coef <- switch(get_iso(file), BRA = -0.306, IDN = -0.227, COD = -0.141)

# Kick zeros, do `log-distance * coefficient`

x <- tbl_raw[tbl_raw$area_accumulated_forest_loss_2019 > 0, ]
x$predicted = log(pmax(x$distance_mine, 1)) * -.25
x$pred_bin <- cut(x$predicted, 
  c(-Inf, -2.5, -2.4, -2.3, -2.2, -2.1, -2, -1.9, -1.8, -1.7, -1.6, -1.5, -1, -0.5, 0))

# Colors
cols <- colorspace::sequential_hcl(n = 14, 
  h = c(-100, 85), c = c(0, 100, 10), l = c(1, 99), power = c(1.1, 0.9), 
  register = "Custom-Palette", rev = TRUE)
names(cols) <- levels(x$pred_bin)

# Save
x %>% sf:::select.sf(predicted, pred_bin, area_accumulated_forest_loss_2019) %>% 
  saveRDS(paste0("output/plots/map_coefs_data_", get_iso(file), ".rds"))

# ggplot(x) +
#   geom_sf(aes(fill = log(area_accumulated_forest_loss_2019)), lwd = 0) +
#   scale_fill_viridis_c() +
#   labs(fill = "Log loss") +
#   ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
#   theme(
#     panel.grid.major = element_line(color = gray(0.5),
#       linetype = "dashed", size = 0.2),
#     panel.background = element_rect(fill = "transparent"))
# 
# ggsave(paste0("output/plots/map_loss_", get_iso(file), ".png"),
#   width = 10, height = 10)

# ggplot(x) +
#   geom_sf(aes(fill = area_accumulated_forest_loss_2019), lwd = 0) +
#   scale_fill_viridis_c() +
#   labs(fill = "Loss") +
#   ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
#   theme(
#     panel.grid.major = element_line(color = gray(0.5),
#       linetype = "dashed", size = 0.2),
#     panel.background = element_rect(fill = "transparent"))
# 
# ggsave(paste0("output/plots/map_loss_level_", get_iso(file), ".png"),
#   width = 10, height = 10)


p <- x %>%
  ggplot() +
  geom_sf(aes(fill = pred_bin), lwd = 0) +
  switch(get_iso(file), 
    BRA = coord_sf(xlim = c(-60, -20), ylim = c(-40, -10), expand = FALSE),
    IDN = coord_sf(xlim = c(90, 130), ylim = c(-20, 10), expand = FALSE),
    COD = coord_sf(xlim = c(0, 40), ylim = c(-20, 10), expand = FALSE)) +
  scale_fill_manual(values = cols) +
  labs(fill = "Effect") +
  ggtitle(paste0("Forest loss ~ mine distance (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

saveRDS(p, file = paste0("output/plots/map_coefs_", get_iso(file), ".rds"))

ggsave(paste0("output/plots/map_coefs_", get_iso(file), ".png"),
  width = 20, height = 20)


y <- st_crop(x, xmin = 25, xmax = 27.5, ymin = -12.5, ymax = -10)
z <- st_crop(y, xmin = 25, xmax = 26, ymin = -11, ymax = -10)

ggplot(y) +
  geom_sf(aes(fill = pred_bin), lwd = 0) +
  scale_fill_manual(values = cols) +
  theme(panel.grid.major = 
      element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggplot(z) +
  geom_sf(aes(fill = pred_bin), lwd = 0) +
  scale_fill_viridis_d(option = "magma", na.value = "#FFFFFF", direction = -1) +
  # scale_fill_manual(values = cols) +
  theme(panel.grid.major = 
      element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggplot(y) +
  geom_sf(aes(fill = -.1 * log(pmax(1, distance_mine))), lwd = 0) +
  colorspace::scale_fill_continuous_sequential("Custom-Palette",
    na.value = "#FFFFFF00") +
  theme(panel.grid.major = 
      element_line(color = gray(0.5), linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

  
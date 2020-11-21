
library("ggplot2")
library("sf")

# BRA = -0.62; IDN = -0.38
mine_coef <- -0.38 # Look this up instead at some point

# Kick zeros, do `log-distance * coefficient`
x <- tbl_raw %>%
  filter(tbl_raw, area_accumulated_forest_loss > 0)
  mutate(predicted = log(pmax(distance_mine, 1)) * mine_coef)

ggplot(x) +
  geom_sf(aes(fill = log(area_accumulated_forest_loss)), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Log loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_loss_", get_iso(file), ".png"),
  width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = area_accumulated_forest_loss), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_loss_level_", get_iso(file), ".png"),
  width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = predicted), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Coefficient") +
  ggtitle(paste0("Forest loss ~ mine distance (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_coefs_", get_iso(file), ".png"),
  width = 10, height = 10)

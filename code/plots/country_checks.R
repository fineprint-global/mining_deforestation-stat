
# Produce plots to visualise country's situations

# Dependencies ---

source("code/0_prelim.R")

library("ggplot2")


# Data ---

file <- "HND"
source("code/2_data.R")

# Use a random subset
x <- tbl_raw[sample(nrow(tbl_raw), min(10000, nrow(tbl_raw))), ]

# Plot ---

ggplot(x) +
  geom_sf(aes(fill = area_accumulated_forest_loss_2019), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_loss_", get_iso(file), ".png"),
  width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = distance_mine), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Mine distance") +
  ggtitle(paste0("Mine distance (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_mine-distance_", get_iso(file), ".png"),
  width = 10, height = 10)

ggplot(x) +
  geom_sf(aes(fill = area_forest_2000), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Log loss") +
  ggtitle(paste0("Initial forest (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_forest_", get_iso(file), ".png"),
  width = 10, height = 10)


# Produce plots to visualise country's situations

# Dependencies ---

source("code/0_prelim.R")

library("ggplot2")


# Data ---

file_subset <- ".*"
file <- "SUR" # "HDN", "THA", "GHA", "PHI", "MEX"
source("code/2_data.R")

# Use a random subset
N <- 1e6
x <- tbl[sample(nrow(tbl), min(N, nrow(tbl))), ]

# Plot ---

p <- ggplot(x) +
  geom_sf(aes(fill = area_accumulated_forest_loss_2019), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Loss") +
  ggtitle(paste0("Forest loss (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_loss_", get_iso(file), ".png"), p,
  width = 10, height = 10)

p <- ggplot(x) +
  geom_sf(aes(fill = distance_mine_log), lwd = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Log-distance") +
  ggtitle(paste0("Mine distance (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_mine-distance_", get_iso(file), ".png"), p,
  width = 10, height = 10)

p <- ggplot(x) +
  geom_sf(aes(fill = area_forest_2000), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "Forest") +
  ggtitle(paste0("Initial forest (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_forest_", get_iso(file), ".png"), p,
  width = 10, height = 10)


p <- ggplot(x) +
  geom_sf(aes(fill = dist_road_log), lwd = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Log-distance") +
  ggtitle(paste0("Roads (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_infra_", get_iso(file), ".png"), p,
  width = 10, height = 10)


p <- ggplot(x) +
  geom_sf(aes(fill = distance_cropland_2000_log), lwd = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Log-distance") +
  ggtitle(paste0("Croplands (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_croplands_", get_iso(file), ".png"), p,
  width = 10, height = 10)


p <- ggplot(x) +
  geom_sf(aes(fill = distance_protected_area_log), lwd = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Log-distance") +
  ggtitle(paste0("Protected areas (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_protected_", get_iso(file), ".png"), p,
  width = 10, height = 10)


p <- ggplot(x) +
  geom_sf(aes(fill = pop_2000), lwd = 0) +
  scale_fill_viridis_c() +
  labs(fill = "head") +
  ggtitle(paste0("Population (", get_iso(file), ")")) +
  theme(
    panel.grid.major = element_line(color = gray(0.5),
      linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "transparent"))

ggsave(paste0("output/plots/map_population_", get_iso(file), ".png"), p,
  width = 10, height = 10)

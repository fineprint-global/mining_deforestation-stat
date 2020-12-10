
library("rnaturalearth")
library("ggplot2")
library("sf")

source("code/0_prelim.R")

world <- ne_countries(scale = "medium", returnclass = "sf")

world$used <- world$iso_a3 %in% get_iso(files)

ggplot(data = world) +
  geom_sf(aes(fill = used)) + 
  scale_fill_manual(values = c("#EEEEEE", "#008000")) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") + 
  theme_void() + 
  theme(legend.position = "none")

ggsave("output/plots/country_subset.png", width = 10, height = 6)

world$coefs <- c()

# Get coef object from 6_focus...

mine_coef <- coefs %>% 
  filter(grepl("mine_log$", vars), grepl("^f_base_log_minesize$", model)) %>% 
  select(country, lm_coef)

world <- left_join(world, mine_coef, by = c("iso_a3" = "country")) %>% 
  mutate(coef = ifelse(used, lm_coef, NA))

ggplot(world) +
  geom_sf(aes(fill = coef)) + 
  scale_fill_viridis_c(na.value = "#e4e4e4") +
  # scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
  #   ifelse(x < 0, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 1.2)), 1)
  #   }) +
  coord_sf(xlim = c(-120, 160), ylim = c(-50, 40), expand = FALSE) +
  theme_void() +
  labs(fill = "Coefficient") +
  theme(legend.position = "bottom")

ggsave("output/plots/country_coefficients.png", width = 10, height = 4)

mine_coef <- coefs %>% 
  filter(grepl("mine_log", vars), grepl("^f_vary_log$", model)) %>% 
  select(country, vars, tob_coef) %>% 
  mutate(vars = ifelse(grepl("log$", vars), "base", 
    ifelse(grepl("km5", vars), "km5", "km25")))

mine_coef %>% 
  pivot_wider(names_from = vars, values_from = tob_coef) %>% 
  mutate(post_km5 = base - km5, post_km25 = base - km5 - km25) %>% 
  filter(!is.na(base)) %>% 
  select(country, base, post_km5, post_km25) %>% 
  mutate_at(2:4, round, 2) %>% 
  knitr::kable()

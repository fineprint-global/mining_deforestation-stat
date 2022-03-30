
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

mine_coef <- read.csv("/home/luckeneder/mining_deforestation-stat/output/results_tables/base_log_minesize.csv")

world <- left_join(world, mine_coef, by = c("iso_a3" = "country")) %>% 
  mutate(coef = ifelse(used, dm, NA))

world <- world %>% mutate(coef_fac = cut(world$coef, c(-Inf, -1.5, -1, -0.5, -0.25, 0, Inf)))

cols <- rev(c("#fde725", "#95d840", "#3cbb75", "#1f968b", "#2d708e", "#482677"))

ggplot(world) +
  geom_sf(aes(fill = coef_fac), col = "#bdbdbd", size = .2) + 
  scale_fill_manual(na.value = "#f0f0f0", values = cols) + 
  # scale_fill_viridis_d(na.value = "#f0f0f0", direction = -1) +
  # scale_fill_viridis_c(na.value = "#f0f0f0") +
  # scale_fill_viridis_c(rescaler = function(x, to = c(0, 1), from = NULL) {
  #   ifelse(x < 0, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), 1.2)), 1)
  #   }) +
  coord_sf(xlim = c(-180, 240), ylim = c(-60, 60), expand = FALSE) +
  theme_void() +
  labs(fill = "Coefficient") +
  theme(legend.position = "bottom")

ggsave("output/plots/country_coefficients.png", width = 10, height = 4)

library("cowplot")
library("ggplot2") 
library("grid")
library("gridExtra") 

df <- data.frame(
  Coefficient = factor(x = c("< -1.5", "(-1.5, -1]", "(-1, -0.5]", "(-0.5, -0.25]", "(-0.25, 0]", "> 0"),
    levels = c("< -1.5", "(-1.5, -1]", "(-1, -0.5]", "(-0.5, -0.25]", "(-0.25, 0]", "> 0")),
  Colour = c("#fde725", "#95d840", "#3cbb75", "#1f968b", "#2d708e", "#482677"),
  Value = 1:6,
  stringsAsFactors = FALSE)

(gg_legend <- ggplot(df, aes(Value, fill = Coefficient)) + 
    geom_bar() +
    scale_fill_manual(values = df$Colour # , 
      # labels = list(
      #   latex2exp::TeX("$x \\leq -1.5$"), 
      #   latex2exp::TeX("$-1.5 < x \\leq -1$"),
      #   latex2exp::TeX("$-1 < x \\leq -0.5$"),
      #   latex2exp::TeX("$-0.5 < x \\leq -0.25$"),
      #   latex2exp::TeX("$-0.25 < x \\leq 0$"),
      #   latex2exp::TeX("$x > 0$"))
      ) +
    theme_bw(base_family = "Arial") +
    theme(rect = element_rect(fill = "transparent")))

# Using the cowplot package
legend <- cowplot::get_legend(gg_legend)

grid.newpage()
grid.draw(legend)
ggsave("output/plots/legend.png", legend, width = 2, height = 2, bg = "white")




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

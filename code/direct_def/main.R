

library("sf")
library("tidyverse")
library("gridExtra")
library("scales")
source("code/9_helpers.R")
countries <- read.csv("input/countries.csv")


# path <- "/mnt/nfs_fineprint/data/geoserver/fineprint_grid_30sec/"
# file <- "global_miningarea_sqkm_30arcsecond_v1.tif"

files <- paste0(countries$continent, "-", countries$iso, ".rds")
# files <- files[grep("AGO|ZMB", files)]
files <- files[grep("AGO|BRA|COL|COG|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MOZ|NIC|PNG|SLE|TZA|VEN|VNM|ZMB", files)]

path <- "/mnt/nfs_fineprint/tmp/mining_def/"

for(file in files) {
  
  cat("Running for ", get_iso(file), ".\n", sep = "")
  
  tbl_raw <- readRDS(paste0(path, file))
  
  tbl_out_temp <- tbl_raw %>% dplyr::filter(area_mine > 0) %>%  sf::st_drop_geometry() 

  
  if (file == files[1]) {tbl_out <- tbl_out_temp} else {tbl_out <- dplyr::bind_rows(tbl_out, tbl_out_temp)}
  
}

# reduce size and mutate
data <- tbl_out %>% 
  dplyr::select(countries, area_forest_2000_mine_lease, area_accumulated_loss_mine_lease) %>%
  dplyr::filter(area_forest_2000_mine_lease > 0) %>%
  dplyr::mutate(share_forest_loss_mine_lease = area_accumulated_loss_mine_lease / area_forest_2000_mine_lease)


# plot total
data_summary <- data %>% dplyr::group_by(countries) %>% 
  dplyr::summarise(area_forest_2000_mine_lease = sum(area_forest_2000_mine_lease),
                   area_accumulated_loss_mine_lease = sum(area_accumulated_loss_mine_lease)) %>%
  dplyr::mutate(share_forest_loss_mine_lease = area_accumulated_loss_mine_lease / area_forest_2000_mine_lease)

cat.levels <- levels(reorder(data_summary$countries, -data_summary$area_accumulated_loss_mine_lease))
data_summary$countries <- factor(data_summary$countries, levels = cat.levels)

p1 <- data_summary %>% 
  dplyr::mutate(area_accumulated_loss_mine_lease = area_accumulated_loss_mine_lease / 1000000) %>%
  ggplot2::ggplot(aes(x = countries, y = area_accumulated_loss_mine_lease)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 panel.grid.major.x =   element_blank())

# plot relative
p2 <- data_summary %>% 
  dplyr::mutate(share_forest_loss_mine_lease = share_forest_loss_mine_lease * 100) %>%
  ggplot2::ggplot(aes(x = countries, y = share_forest_loss_mine_lease)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Relative",  x = NULL, y = "Direct deforestation [%]") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 panel.grid.major.x =   element_blank())

# arrange plots
p <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggplot2::ggsave("direct_deforestation_2017.pdf", 
                plot = p, device = "pdf", 
                path = paste0("./output/plots/direct_def"),
                scale = 1, width = 300, height = 150, units = "mm")

# plot boxplots
tmp <- data %>% dplyr::group_by(countries) %>% dplyr::summarise(n = n()) %>%
  dplyr::mutate(countries2 = paste0(countries, "\n (", n, ")"))

data <- data %>% 
  dplyr::left_join(tmp, by = "countries")

tmp <- data %>% dplyr::group_by(countries2) %>% dplyr::summarise(total = sum(area_accumulated_loss_mine_lease))
cat.levels <- levels(reorder(tmp$countries2, -tmp$total))
data$countries2 <- factor(data$countries2, levels = cat.levels)

p <- data %>%
  dplyr::mutate(share_forest_loss_mine_lease = share_forest_loss_mine_lease * 100) %>%
  ggplot2::ggplot(aes(x = countries2, y = share_forest_loss_mine_lease)) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Distribution",  x = NULL, y = "Direct deforestation [%]") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 panel.grid.major.x =   element_blank())

ggplot2::ggsave("direct_deforestation_2017_boxplots.pdf", 
                plot = p, device = "pdf", 
                path = paste0("./output/plots/direct_def"),
                scale = 1, width = 480, height = 240, units = "mm")





  

  
  
  
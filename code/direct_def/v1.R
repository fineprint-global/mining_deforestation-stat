

library("sf")
library("tidyverse")
library("gridExtra")
library("scales")
library("viridis")
source("code/9_helpers.R")
countries <- read.csv("input/countries.csv")
# countries <- countries$iso[grep("AGO|BRA|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MYS|MOZ|NIC|PNG|SLE|TZA|VEN|VNM|ZMB", countries$iso)]
countries <- countries$iso[grep("AGO|BRA|COL|COD|CIV|ECU|GAB|GHA|GTM|GIN|GUY|HND|IND|IDN|KEN|LBR|MEX|MOZ|MYS|NIC|PHL|PNG|SLE|SUR|THA|TZA|VEN|VNM|ZMB", countries$iso)]

path <- "/mnt/nfs_fineprint/data/geoserver/fineprint_grid_30sec/timeseries_20200926"
file <- "mine_forest_timeseries.csv"

tbl_raw <- readr::read_csv(file.path(path, file))

tbl <- tbl_raw %>% dplyr::filter(ISO3_CODE %in% countries)

forest_2000 <- tbl %>% dplyr::filter(year == 2000) %>% dplyr::select(ISO3_CODE, area_remaining_forest) %>%
  dplyr::rename(area_forest_2000 = area_remaining_forest)

data_summary <- tbl %>% dplyr::group_by(ISO3_CODE) %>% 
  dplyr::summarise(area_accumulated_direct_forest_loss = sum(area_direct_forest_loss)) %>%
  dplyr::left_join(forest_2000, by = "ISO3_CODE") %>%
  dplyr::mutate(relative_direct_forest_loss = area_accumulated_direct_forest_loss / area_forest_2000)

# plot total
p_dat <- tbl %>% 
  mutate(period = ifelse(year %in% c(2000:2004), "2000-2004", NA)) %>%
  mutate(period = ifelse(year %in% c(2005:2009), "2005-2009", period)) %>%
  mutate(period = ifelse(year %in% c(2010:2014), "2010-2014", period)) %>%
  mutate(period = ifelse(year %in% c(2015:2019), "2015-2019", period)) %>%
  dplyr::group_by(period, ISO3_CODE) %>%
  dplyr::summarise(area_direct_forest_loss = sum(area_direct_forest_loss),
                   area_remaining_forest_period = max(area_remaining_forest )) %>%
  dplyr::mutate(relative_direct_forest_loss_period = area_direct_forest_loss / area_remaining_forest_period)

cat.levels <- levels(reorder(data_summary$ISO3_CODE, -data_summary$area_accumulated_direct_forest_loss))
data_summary$ISO3_CODE <- factor(data_summary$ISO3_CODE, levels = cat.levels)
p_dat$ISO3_CODE <- factor(p_dat$ISO3_CODE, levels = cat.levels)
p_dat$period <- factor(p_dat$period, levels = rev(c("2000-2004", "2005-2009", "2010-2014", "2015-2019")))

p11 <- p_dat %>% 
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = NULL, x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())  +
  ggplot2::guides(fill = guide_legend(reverse = TRUE))

p12 <- p_dat %>% 
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())

# log scale

p31 <- p_dat %>% 
  dplyr::mutate(area_direct_forest_loss  = log(area_direct_forest_loss  / 1000000)) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2, log]")  +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())  +
  ggplot2::guides(fill = guide_legend(reverse = TRUE)) 

p32 <- data_summary %>% 
  dplyr::mutate(area_accumulated_direct_forest_loss   = log(area_accumulated_direct_forest_loss / 1000000) )%>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_accumulated_direct_forest_loss )) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2, log]")  +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())

# without IDN

p41 <- p_dat %>% 
  dplyr::filter(ISO3_CODE != "IDN") %>%
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total (excl. IDN)", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())  +
  ggplot2::guides(fill = guide_legend(reverse = TRUE)) 

p42 <- p_dat %>% 
  dplyr::filter(ISO3_CODE != "IDN") %>%
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Total (excl. IDN)", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())

# log axis

p51 <- p_dat %>% 
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_y_continuous(expand = c(0, 0), trans = 'log10') +
  ggplot2::annotation_logticks(sides="l") +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())  +
  ggplot2::guides(fill = guide_legend(reverse = TRUE)) 

p52 <- data_summary %>% 
  dplyr::mutate(area_accumulated_direct_forest_loss   = area_accumulated_direct_forest_loss / 1000000)%>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_accumulated_direct_forest_loss )) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), trans = 'log10') +
  ggplot2::annotation_logticks(sides="l") +
  ggplot2::labs(title = "Total", x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x = element_blank())

# plot relative
p_dat$period <- factor(p_dat$period, levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019"))

p21 <- p_dat %>% 
  dplyr::mutate(relative_direct_forest_loss_period = relative_direct_forest_loss_period * 100) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = relative_direct_forest_loss_period, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = NULL,  x = NULL, y = "Direct deforestation [%]") +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85, direction = -1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 90, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x =   element_blank()) 


p22 <- data_summary %>% 
  dplyr::mutate(relative_direct_forest_loss = relative_direct_forest_loss * 100) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = relative_direct_forest_loss)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = "Relative",  x = NULL, y = "Direct deforestation [%]") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(hjust = 1),
                 axis.title.y = element_text(hjust = 1),
                 axis.text.x = element_text(angle = 45, vjust = 0.5),
                 legend.title = element_blank(),
                 legend.position = "bottom",
                 panel.grid.major.x =   element_blank())

# arrange plots
p <- gridExtra::grid.arrange(p11, p41, p31, p51, p21, p12, p42, p32, p52, p22, nrow = 2)
ggplot2::ggsave("direct_deforestation_2019.pdf", 
                plot = p, device = "pdf", 
                path = paste0("./output/plots/direct_def"),
                scale = 1, width = 750, height = 300, units = "mm")


# final plot 

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(p11)

# add corner labels
store <- list(p11 + theme(legend.position="none"), p21 + theme(legend.position="none"))
labls <- c("A", "B")
for (l in seq_along(labls)) {
  
  store[[l]] <- gridExtra::arrangeGrob(store[[l]], top = grid::textGrob(labls[l], x = unit(0.5, "npc"), 
                                                                        y   = unit(1, "npc"), just=c("left","top"),
                                                                        gp=grid::gpar(col="black", fontsize=12)))
  }


p <- do.call(gridExtra::grid.arrange, c(store, nrow=1))
p <- grid.arrange(p, mylegend, nrow=2,heights=c(10, 1))

ggplot2::ggsave("direct_deforestation_2019_final.pdf", 
                plot = p, device = "pdf", 
                path = paste0("./output/plots/direct_def"),
                scale = 1, width = 220, height = 120, units = "mm")

# export data
write.csv(p_dat,"output/plots/direct_def/p_dat.csv", row.names = FALSE)

# plot boxplots

# TBC when needed

p11b <- p_dat %>% 
  dplyr::mutate(ISO3_CODE = as.character(ISO3_CODE)) %>% 
  dplyr::mutate(ISO3_CODE = ifelse(ISO3_CODE %in% c("IDN", "BRA", "GHA", "VEN"), ISO3_CODE, "OTH")) %>% 
  dplyr::mutate(ISO3_CODE = factor(ISO3_CODE, levels = c("IDN", "BRA", "GHA", "VEN", "OTH"))) %>% 
  dplyr::mutate(area_direct_forest_loss  = area_direct_forest_loss  / 1000000) %>%
  ggplot2::ggplot(aes(x = ISO3_CODE, y = area_direct_forest_loss, fill = period)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=pretty_breaks(n=6)) +
  ggplot2::labs(title = NULL, x = NULL, y = "Direct deforestation [km2]")  +
  ggplot2::scale_fill_manual(values = viridis(4, end = 0.85)) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Direct deforestation") +
  ggplot2::theme(axis.line = element_line(colour = "black"),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "bottom",
    panel.grid.major.x = element_blank())  +
  ggplot2::guides(fill = guide_legend(reverse = TRUE))
ggplot2::ggsave("output/plots/direct_deforestation_2019_custom.png", 
  plot = p11b, device = "png", scale = 1, width = 220, height = 120, units = "mm")

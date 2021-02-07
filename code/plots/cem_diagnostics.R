
# Analyse matching: balance diagnostics (Zhang et al 2019)

# Depends on 3_cem.R

library("grid")
library("cobalt")
library("ggplot2")
library("viridis")

source("code/0_prelim.R")

p1_store <- list()
p2_store <- list()

# file <- files[grep("NIC", files)]
for(file in files) {
  
  cat("Running for ", get_iso(file), ".\n", sep = "")
  
  cat("Retrieving matches.\n")
  out_cem <- readRDS(paste0("input/cem/", get_iso(file), ".rds"))
  out_cem$vars <- out_cem$vars[c(1, 2, 4, 6, 8)]
  
  source("code/2_data.R")
  tbl_match <- tbl %>% dplyr::select(out_cem$vars)
  
  p1 <- cobalt::love.plot(bal.tab(out_cem, data = tbl_match, m.threshold=0.1), 
                    stat = "mean.diffs", abs = F, colors = c("darkorchid2", "skyblue2"), shapes = c(17, 15), 
                    size = 5, title = get_iso(file), position = "none") + 
    ggplot2::xlab(NULL)
  p1_store[[file]] <- p1; gc()
  
  p2 <- cobalt::bal.plot(out_cem, data = tbl_match, var.name = 'area_forest_2000', which = 'both', 
                         sample.names = c("Unadjusted", "Adjusted"),
                         #colors = c("red3", "yellow2"),
                         colors = c(viridis(3)[1], viridis(3)[3])) + 
    ggplot2::labs(title = get_iso(file), x = NULL) +
    ggplot2::scale_x_continuous(n.breaks = 3) +
    ggplot2::scale_y_continuous(n.breaks = 3) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = element_text(hjust = 1))
  p2_store[[file]] <- p2; gc()
  
}

save(p1_store, file = "output/plots/love_plots/p1_store.RData")
save(p2_store, file = "output/plots/bal_plots/p2_store.RData")

# load("output/plots/love_plots/p1_store.RData")
# load("output/plots/bal_plots/p2_store.RData")

for(file in seq_along(files)) {if (! file %in% c(1, 6, 11, 16, 21, 26)) {
  p1_store[[file]] <- p1_store[[file]] + ggplot2::theme(axis.text.y = element_blank())
}}

# p1 <- do.call(gridExtra::grid.arrange, c(p1_store, ncol=5))

library(cowplot)
p1 <- cowplot::plot_grid(p1_store[[1]], p1_store[[2]], p1_store[[3]], p1_store[[4]], p1_store[[5]],
                        p1_store[[6]], p1_store[[7]], p1_store[[8]], p1_store[[9]], p1_store[[10]],
                        p1_store[[11]], p1_store[[12]], p1_store[[13]], p1_store[[14]], p1_store[[15]],
                        p1_store[[16]], p1_store[[17]], p1_store[[18]], p1_store[[19]], p1_store[[20]],
                        p1_store[[21]], p1_store[[22]], p1_store[[23]], p1_store[[24]], p1_store[[25]],
                        p1_store[[26]], p1_store[[27]], p1_store[[28]], p1_store[[29]],
                        align = "h", ncol = 5,
                        rel_widths = c(0.26, 0.185, 0.185, 0.185, 0.185))

ggplot2::ggsave("output/plots/love_plots/love_plots.pdf", 
                plot = p1, device = "pdf", 
                scale = 1, width = 300, height = 400, units = "mm")


for(file in seq_along(files)) {if (! file %in% c(1, 6, 11, 16, 21, 26)) {
  p2_store[[file]] <- p2_store[[file]] + ggplot2::theme(axis.title.y = element_blank())
}}
for(file in seq_along(files)) {if (! file %in% c(1:5)) {
  p2_store[[file]] <- p2_store[[file]] + ggplot2::theme(strip.background = element_blank(),
                                                        strip.text.x = element_blank())
}}
for(file in seq_along(files)) {if (! file %in% c(26:29)) {
  p2_store[[file]] <- p2_store[[file]] + ggplot2::theme(axis.text.x = element_blank())
}}

# p2 <- do.call(gridExtra::grid.arrange, c(p2_store, ncol=6))

p2 <- cowplot::plot_grid(p2_store[[1]], p2_store[[2]], p2_store[[3]], p2_store[[4]], p2_store[[5]],
                         p2_store[[6]], p2_store[[7]], p2_store[[8]], p2_store[[9]], p2_store[[10]],
                         p2_store[[11]], p2_store[[12]], p2_store[[13]], p2_store[[14]], p2_store[[15]],
                         p2_store[[16]], p2_store[[17]], p2_store[[18]], p2_store[[19]], p2_store[[20]],
                         p2_store[[21]], p2_store[[22]], p2_store[[23]], p2_store[[24]], p2_store[[25]],
                         p2_store[[26]], p2_store[[27]], p2_store[[28]], p2_store[[29]],
                         ncol = 5,
                         rel_widths = c(1/5, 1/5, 1/5, 1/5, 1/5),
                         rel_heights = c(1.1, 0.95, 0.95, 0.95, 0.95, 1.1))

ggplot2::ggsave("output/plots/bal_plots/bal_plots.pdf", 
                plot = p2, device = "pdf", 
                scale = 1, width = 420, height = 270, units = "mm")

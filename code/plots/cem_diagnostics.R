
# Depends on 3_cem.R

library("grid")
library("cobalt") # Check CEM outputs

# Analyse matching: balance diagnostics (Zhang et al 2019)
p <- cobalt::bal.plot(out_cem, data = tbl, var.name = 'area_forest_2000', which = 'both')
png(paste0("output/plots/bal_plots/forest_2000_",
           sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
grid::grid.draw(p)
dev.off()

p <- cobalt::love.plot(bal.tab(out_cem, data = tbl, m.threshold=0.1), stat = "mean.diffs", abs = F)
png(paste0("output/plots/love_plots/",
           sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
grid::grid.draw(p)
dev.off()

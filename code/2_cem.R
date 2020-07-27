
# Perform CEM, optionally store summary statistics.

# Dependencies -----

library("cem") # Matching
library("grid")
# library("cobalt") # Check CEM outputs


if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}

# CEM -----

# out_imb <- imbalance(tbl$treated, as.data.frame(tbl),
#   drop = c("countries", "id_grid"))

cat("Retrieving matches.\n")

if(exists("STORE_CEM") && STORE_CEM && 
    file.exists(paste0("input/cem/", get_iso(file), ".rds"))) {
  out_cem <- readRDS(paste0("input/cem/", get_iso(file), ".rds"))
} else {
  out_cem <- cem(treatment = "treated", data = tbl,
   drop = drop_but(tbl, match_on),
    keep.all = TRUE)
  saveRDS(out_cem, paste0("input/cem/", get_iso(file), ".rds"))
}

# # Analyse matching: balance diagnostics (Zhang et al 2019)
# p <- cobalt::bal.plot(out_cem, data = tbl, var.name = 'area_forest_2000', which = 'both')
# png(paste0("output/plots/bal_plots/forest_2000_",
#            sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
# grid::grid.draw(p)
# dev.off()
# 
# p <- cobalt::love.plot(bal.tab(out_cem, data = tbl, m.threshold=0.1), stat = "mean.diffs", abs = F)
# png(paste0("output/plots/love_plots/",
#            sub(".*([A-Z]{3}).rds", "\\1", file), ".png"), width = 960, height = 720)
# grid::grid.draw(p)
# dev.off()

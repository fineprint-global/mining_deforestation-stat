
# Perform CEM, optionally store summary statistics.

# Dependencies -----

library("cem") # Matching
library("grid")
# library("cobalt") # Check CEM outputs


if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}

# Imbalance ---
if(CALC_IMBALANCE) {
  out_imb <- imbalance(tbl$treated, as.data.frame(tbl),
    drop = drop_but(tbl, match_on))
}


# CEM ---

if(exists("STORED_CEM") && STORE_CEM &&
    file.exists(paste0("input/cem/", get_iso(file), ".rds"))) {
  cat("Retrieving matches.\n")
  out_cem <- readRDS(paste0("input/cem/", get_iso(file), ".rds"))
} else {
  cat("Matching.\n")
  out_cem <- cem(treatment = "treated", data = tbl,
   drop = drop_but(tbl, match_on),
    keep.all = TRUE)
  saveRDS(out_cem, paste0("input/cem/", get_iso(file), ".rds"))
}

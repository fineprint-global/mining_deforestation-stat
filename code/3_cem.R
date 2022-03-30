
# Perform CEM, optionally store summary statistics.
# Produces:
#   - out_cem, (out_imb)

# Dependencies ---

library("cem") # Matching

dir.create("input/cem", FALSE)

#' @title Helper for CEM's drop
#'
#' @param x Object with names.
#' @param but Character vector with names to exclude.
#'
#' @return Returns a character vector with all names but but.
drop_but <- function(x, but) {

  names(x)[!grepl(paste0(but, collapse = "|"), names(x))]
}

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}


# Imbalance ---

if(CALC_IMBALANCE) {
  out_imb <- imbalance(tbl$treated, as.data.frame(tbl),
    drop = drop_but(tbl, match_on))
}


# CEM ---

cached <- "/home/luckeneder/mining_deforestation-stat/"

if(exists("STORED_CEM") && STORED_CEM && # Local
    file.exists(paste0(cached, "input/cem/", get_iso(file), ".rds"))) {
  cat("Retrieving matches.\n")
  out_cem <- readRDS(paste0(cached, "input/cem/", get_iso(file), ".rds"))
} else if(exists("STORED_CEM") && STORED_CEM && # Cloud
  file.exists(paste0("/home/luckeneder/mining_deforestation-stat/",
    "input/cem/", get_iso(file), ".rds"))) {
  cat("Retrieving matches.\n")
  out_cem <- readRDS(paste0("/home/luckeneder/mining_deforestation-stat/",
    "input/cem/", get_iso(file), ".rds"))
} else {
  cat("Matching.\n")
  out_cem <- cem(treatment = "treated", data = tbl,
   drop = drop_but(tbl, match_on),
    keep.all = TRUE)
  saveRDS(out_cem, paste0("input/cem/", get_iso(file), ".rds"))
}

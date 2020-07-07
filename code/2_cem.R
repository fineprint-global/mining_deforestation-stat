
# Dependencies -----

library("cem") # Matching

if(!exists("tbl")) {stop("Please prepare the data in `tbl`.")}

# CEM -----

# out_imb <- imbalance(tbl$treated, as.data.frame(tbl),
#   drop = c("countries", "id_grid"))

cat("Retrieving matches.\n")

out_cem <- cem(treatment = "treated", data = tbl,
  drop = drop_but(tbl, match_on),
  keep.all = TRUE)


cem_data <- function(x) {

  if(!inherits(x, "cem.match")) stop("Provide a 'cem.match' object.")

  out <- data.frame(cem_treated = x[["groups"]], cem_weight = x[["w"]])
  return(out)
}

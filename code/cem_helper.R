
cem_data <- function(x) {

  if(!inherits(x, "cem.match")) stop("Provide a 'cem.match' object.")

  out <- data.frame(treated = x[["groups"]], weight = x[["w"]])
  return(out)
}

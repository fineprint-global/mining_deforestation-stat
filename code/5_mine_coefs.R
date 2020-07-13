
# Evaluate coefficients from fitted models on data summaries.

library("dplyr")

country_sm <- read.csv("output/country_data-summary.csv")
files <- list.files("output/txt/", pattern = "coef")
files <- list.files("output/txt/", pattern = "coef.*_f_no-road")
files <- list.files("output/txt/", pattern = "coef.*_form1")

lapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  xs <- seq(info[1, 3], 1e5, length.out = 10000)
  f <- function() {
    xs * y[2, 2] + (xs >= 50000) * xs * y[3, 2]
  }
  png(paste0("output/ex-post/coefs-", sub("coef_([A-Z]+).*", "\\1", x)),
    width = 960, height = 720)
  plot(x = xs, y = f(), type = "l", main = sub("coef_([A-Z]+).*", "\\1", x),
    xlab = "distance", ylab = "forest loss")
  dev.off()
})

(sign <- t(sapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  c(sign(y[2, 4]), sign(y[3, 2]))
})))
rownames(sign) <- sub(".*([A-Z]{3}).*.csv", "\\1", files)
(sign[, 1] == -1) | (sign[, 1] == 1 & sign[, 2] == -1)

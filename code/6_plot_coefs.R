
library("dplyr")

country_sm <- read.csv("output/country_data-summary.csv")
files <- list.files("output/txt/", pattern = "coef")

lapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  xs <- seq(info[1, 3], info[4, 3,], length.out = 10000)
  f <- function() {
    cbind(xs, xs ^ 0.5) %*% y[2:3, 2] +
      (xs >= 50000) * cbind(xs, xs ^ 0.5) %*% y[4:5, 2]

  }

  png(paste0("output/ex-post/coefs-", sub("coef_([A-Z]+).*", "\\1", x)),
    width = 960, height = 720)
  plot(x = xs, y = f(), type = "l", main = sub("coef_([A-Z]+).*", "\\1", x),
    xlab = "distance", ylab = "forest loss")
  dev.off()
})

(sign <- sapply(files, function(x) {
  info <- filter(country_sm, iso == sub("coef_([A-Z]+).*", "\\1", x))
  y <- read.csv(paste0("output/txt/", x))
  round(y[9, 4], 2)
}))

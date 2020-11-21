
library("grf")
library("cem")

x <- readRDS("IDN.rds")
y <- readRDS("oceania-IDN.rds")

data <- y[x$w > 0, ]
w <- x$w[x$w > 0]

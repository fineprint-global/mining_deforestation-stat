
y <- tbl[[as.character(formulas[[i]][[2]])]]
X <- model.matrix(formulas[[i]], data = tbl)[, -1] # no need for constant here
if(SCALE_CENTER) {X <- scale(X)}
X <- X[, !apply(X, 2, function(x) all(is.na(x)))]

r <- sample(seq(length(y)), 100000)

y2 <- y[r]
X2 <- X[r, ]
X2 <- X2[, colSums(X2) > 0]
w <- out_cem[["w"]][r]


out_lm1 <- lm(y2 ~ X2, weights = w)
coef(out_lm1)[1:10]

library("censReg")
out_cens <- censReg(formulas[[i]], data = tbl[r, ])

library("AER")
y3 <- y2[w > 0]
X3 <- X2[w > 0, ]
X3 <- X3[, colSums(X3) > 0]
out_tob <- AER::tobit(y3 ~ X3, weights = w[w > 0])
vc <- vcov(out_tob)
X3 <- X3[, (diag(vc) != 0)[c(-1, -ncol(vc))]]
out_tob <- AER::tobit(y3 ~ X3, weights = w[w > 0])
coef(out_tob)[1:10]
summary(out_tob)
  
library("pscl")
y4 <- as.integer(round(tbl[["distance_mine"]][r] / 1000))
out_hurdle <- hurdle(y4 ~ X2, weights = w, dist = "negbin")

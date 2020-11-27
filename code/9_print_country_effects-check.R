
X <- rnorm(1000, 1000, 100)
y <- exp(10 + log(X) * 0.5 + rnorm(1000))

mdl <- lm(log(y) ~ log(X))

distance_mine <- X
distance_mine_alt <- X - 100
relative_change <- (distance_mine - distance_mine_alt) / distance_mine * -1
deforestation <- y

# extract dist_mine coeffs
beta <- coef(mdl)[2]
beta_ci <- confint(mdl)[2, ]

relative <- cbind(
  relative_change * beta_ci[1],
  relative_change * beta,
  relative_change * beta_ci[2])

pred_manual <- cbind(
  relative_change * beta_ci[1] * deforestation,
  relative_change * beta * deforestation,
  relative_change * beta_ci[2] * deforestation)

exp(predict(mdl, interval = "conf"))
exp(predict(mdl, interval = "pred"))
pred_scenario <- exp(predict(mdl, newdata = data.frame(X = X - 100)))

exp(predict(mdl)) - pred_scenario

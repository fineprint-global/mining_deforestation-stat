
cem_reg <- function(obj, formula, data, model = "linear", extrapolate = FALSE, ntree = 2000) {

  mod.type <- NULL
  mod <- NULL

  if ((class(obj)[1] != "cem.match") && (class(obj)[1] != "cem.match.list"))
    stop("Argument `obj' must be a `cem.match' or `cem.match.list' object")
  mods <- c("linear", "lm", "lme", "linear-RE", "logit", "logistic", "rf", "forest")
  if (!(model %in% mods))
    stop(sprintf("model `%s' not yet implemented", model))
  if (model %in% c("lm", "linear")) {
    mod <- "lm"
    mod.type <- "Linear regression model"
  }
  if (model %in% c("lme", "linear-RE")) {
    mod <- "lme"
    mod.type <- "Linear random effect model"
  }
  if (model %in% c("forest", "rf")) {
    mod <- "randomForest"
    mod.type <- "Random forest model"
  }
  if (model %in% c("logit", "logistic")) {
    mod <- "glm"
    mod.type <- "Logistic model"
  }
  random.cem <- NULL
  random.all <- NULL
  ranef.cem <- NULL
  ranef.all <- NULL
  reg.cem <- NULL
  reg.all <- NULL
  rf.cem <- NULL
  TE <- NULL
  control = lmeControl(maxIter = 1000, opt = "optim")
  if (class(obj)[1] == "cem.match") {
      if (mod == "randomForest") {return(mod_rf())}
      if (mod == "lme") {return(mod_lme)}
      if (mod == "lm") {return(mod_lm())}
      if (mod == "glm") {return(mod_glm())}
  }
  if (class(data) != "list")
      stop("Argument `data' must be a list of `data.frame's")
  n.cems <- length(obj) - 1
  if (length(data) != n.cems)
      stop("lengths of `cem.match.list' object and `data' do not match")
  est <- vector(n.cems, mode = "list")
  for (i in 1:n.cems) {
      out <- att(obj[[i]], formula = formula, data = data[[i]],
          model = model, extrapolate = extrapolate)
      est[[i]] <- out
  }
  ncoef <- NCOL(est[[1]])
  qoi <- numeric(ncoef)
  seq <- numeric(ncoef)
  for (i in 1:n.cems) {
      att.m <- est[[i]]$att.model
      qoi <- qoi + att.m["Estimate", ]
      seq <- seq + att.m["Std. Error", ]^2
  }
  qoi <- qoi/n.cems
  seq <- seq/n.cems
  s2 <- numeric(ncoef)
  for (i in 1:n.cems) {
      att.m <- est[[i]]$att.model
      s2 <- s2 + (att.m["Estimate", ] - qoi)^2
  }
  s2 <- s2/(n.cems - 1)
  S <- sqrt(seq + s2 * (1 + 1/n.cems))
  TE <- vector(n.cems, mode = "list")
  for (i in 1:n.cems) TE[[i]] <- est[[i]]$TE
  att.model <- rbind(qoi, S)
  att.model <- rbind(att.model, att.model[1, ]/att.model[2,
      ])
  att.model <- rbind(att.model, 2 * (1 - pnorm(att.model[3,
      ])))
  if (colnames(att.model)[1] == "qoi")
      colnames(att.model)[1] <- obj[[1]]$treatment
  rownames(att.model) <- c("Estimate", "Std. Error", "t value",
      "p-value")
  out <- list(mult = est, att.model = att.model, treatment = obj[[1]]$treatment,
      extrapolate = extrapolate, mod.type = mod.type, TE = TE)
  class(out) <- "cem.att"
  out
}



mod_lm <- function() {
  out <- do.call(mod, list(formula = formula, data = data, weights = obj$w))
  fit <- fitted(out)
  response <- all.vars(formula)[attr(terms(formula), "response")]
  tmp.data <- data
  tmp.data[, obj$treatment] <- 0
  prd <- predict(out, tmp.data)
  TEi <- data[, response] - prd
  g <- function(i) {
    idt <- which(obj$mstrata == i & obj$groups == obj$g.names[2])
    mean(TEi[idt])
  }
  mstrata <- na.omit(unique(obj$mstrata))
  TE <- sapply(mstrata, g)
  names(TE) <- mstrata
  idx <- which(is.na(TE))
  if (length(idx) > 0) TE <- TE[-idx]
  reg.cem <- t(summary(out)$coefficients)
  rownames(reg.cem) <- c("Estimate", "Std. Error", "t value", "p-value")
  att.model <- reg.cem
  if (extrapolate) {
    idx2 <- which(!obj$matched & obj$groups == obj$g.names[2])
    f3 <- formula(sprintf(" ~  . -%s ", obj$treatment))
    f4 <- update(formula, f3)
    tmp.data <- data[idx2, ]
    tmp.data[obj$treatment] <- 0
    y1 <- predict(out, newdata = tmp.data)
    response <- all.vars(f4)[attr(terms(formula), "response")]
    tmp <- t.test(y1, data[idx2, response])
    att4 <- diff(tmp$estimate)
    att4.serr <- abs(diff(tmp$estimate)/tmp$statistic)
    p <- obj$tab[2, 2]/obj$tab[1, 2]
    att4 <- (1 - p) * att4 + p * reg.cem["Estimate", obj$treatment]
    reg.all <- matrix(NA, 4, 1)
    rownames(reg.all) <- rownames(reg.cem)
    colnames(reg.all) <- obj$treatment
    reg.all["Estimate", 1] <- att4
    reg.all["Std. Error", 1] <- sqrt(p^2 *
      reg.cem["Std. Error", obj$treatment]^2 + (1 - p)^2 * att4.serr^2)
    reg.all["t value", 1] <- att4/att4.serr
    reg.all["p-value", 1] <- 2 * (1 - pnorm(att4/att4.serr))
    reg.cem <- NULL
    att.model <- reg.all
    TE <- NULL
    fit <- fitted(out)
    tmp.data <- data
    tmp.data[obj$treatment] <- 0
    prd <- predict(out, tmp.data)
    TEi <- data[, response] - prd
    g <- function(i) {
      idt <- which(obj$strata == i & obj$groups == obj$g.names[2])
      if (length(idt) > 0) return(mean(TEi[idt]))
      return(NA)
    }
    strata <- na.omit(unique(obj$strata))
    TE <- sapply(strata, g)
    names(TE) <- strata
    idx <- which(is.na(TE))
    TE <- TE[-idx]
  }
  out <- list(att.model = att.model, tab = obj$tab, treatment = obj$treatment,
    extrapolate = extrapolate, mod.type = mod.type, TE = TE)
  class(out) <- "cem.att"
  return(out)
}

mod_glm <- function() {
  out <- do.call(mod, list(formula = formula, data = data,
    weights = obj$w, family = "binomial"))
  fit <- fitted(out)
  response <- all.vars(formula)[attr(terms(formula), "response")]
  tmp.data <- data
  tmp.data[, obj$treatment] <- 0
  prd <- predict(out, tmp.data, type = "response")
  TEi <- data[, response] - prd
  g <- function(i) {
    idt <- which(obj$mstrata == i & obj$groups == obj$g.names[2])
    mean(TEi[idt])
  }
  mstrata <- na.omit(unique(obj$mstrata))
  TE <- sapply(mstrata, g)
  names(TE) <- mstrata
  idx <- which(is.na(TE))
  if (length(idx) > 0)
    TE <- TE[-idx]
  reg.cem <- t(summary(out)$coefficients)
  rownames(reg.cem) <- c("Estimate", "Std. Error", "t value", "p-value")
  att.model <- reg.cem
  if (extrapolate) {
    idx2 <- which(!obj$matched & obj$groups == obj$g.names[2])
    f3 <- formula(sprintf(" ~  . -%s ", obj$treatment))
    f4 <- update(formula, f3)
    tmp.data <- data[idx2, ]
    tmp.data[obj$treatment] <- 0
    y1 <- predict(out, newdata = tmp.data, type = "response")
    response <- all.vars(f4)[attr(terms(formula),
      "response")]
    tmp <- t.test(y1, data[idx2, response])
    att4 <- diff(tmp$estimate)
    att4.serr <- abs(diff(tmp$estimate)/tmp$statistic)
    p <- obj$tab[2, 2]/obj$tab[1, 2]
    att4 <- (1 - p) * att4 + p * reg.cem["Estimate",
      obj$treatment]
    reg.all <- matrix(NA, 4, 1)
    rownames(reg.all) <- rownames(reg.cem)
    colnames(reg.all) <- obj$treatment
    reg.all["Estimate", 1] <- att4
    reg.all["Std. Error", 1] <- sqrt(p^2 * reg.cem["Std. Error",
      obj$treatment]^2 + (1 - p)^2 * att4.serr^2)
    reg.all["t value", 1] <- att4/att4.serr
    reg.all["p-value", 1] <- 2 * (1 - pnorm(att4/att4.serr))
    reg.cem <- NULL
    att.model <- reg.all
    TE <- NULL
    tmp.data <- data
    tmp.data[obj$treatment] <- 0
    prd <- predict(out, tmp.data, type = "response")
    TEi <- data[, response] - prd
    g <- function(i) {
      idt <- which(obj$strata == i & obj$groups ==
        obj$g.names[2])
      if (length(idt) > 0)
        return(mean(TEi[idt]))
      return(NA)
    }
    strata <- na.omit(unique(obj$strata))
    TE <- sapply(strata, g)
    names(TE) <- strata
    idx <- which(is.na(TE))
    TE <- TE[-idx]
  }
  out <- list(att.model = att.model, tab = obj$tab,
      treatment = obj$treatment, extrapolate = extrapolate,
      mod.type = mod.type, TE = TE)
  class(out) <- "cem.att"
  return(out)
}

mod_lme <- function() {
  data1 <- data
  data1$allID <- factor(obj$strata)
  data1$cemID <- factor(obj$mstrata)
  data1$matched <- obj$matched
  matched <- obj$matched
  f1 <- formula(sprintf(" ~ %s | allID", obj$treatment))
  f2 <- formula(sprintf(" ~ %s | cemID", obj$treatment))
  if (extrapolate) {
    rand.try <- try(lme(update(formula, ~. - cemID - allID - matched),
      data = data1, random = f1, keep.data = FALSE, control = control),
      silent = TRUE)
    if (class(rand.try) == "try-error") {
      cat("\nCannot estimate the random effects model for the complete data set\n")
    }
    else {
      rand.all <- rand.try
      random.all <- t(summary(rand.all)$tTable)
      rand.cf <- coef(rand.all)
      ww <- table(obj$strata, obj$groups)[, 2]
      cf.idx <- match(rownames(rand.cf), names(ww))
      rownames(random.all) <- c("Estimate", "Std. Error", "DF", "t value", "p-value")
      w.coef <- apply(rand.cf, 2, function(x) weighted.mean(x, w = ww[cf.idx]))
      random.all["Estimate", ] <- w.coef
      random.all["Std. Error", ] <- random.all["Std. Error",] * sqrt(sum(ww^2)/sum(ww)^2)
      random.all["t value", ] <- random.all["Estimate", ] / random.all["Std. Error", ]
      random.all["p-value", ] <- 2 * (1 - pnorm(random.all["t value", ]))
      att.model <- random.all
      TE <- rand.cf[, obj$treatment]
      names(TE) <- rownames(rand.cf)
    }
  }
  else {
    rand.try <- try(lme(update(formula, ~. - cemID - allID - matched),
      data = data1, random = f2, subset = matched == TRUE, keep.data = FALSE,
      control = control), silent = TRUE)
    if (class(rand.try) == "try-error") {
      cat("\nCannot estimate the random effects model for the CEM matched subsample\n")
    }
    else {
      rand.cem <- rand.try
      random.cem <- t(summary(rand.cem)$tTable)
      rand.cf <- coef(rand.cem)
      ww <- table(obj$mstrata, obj$groups)[, 2]
      cf.idx <- match(rownames(rand.cf), names(ww))
      rownames(random.cem) <- c("Estimate", "Std. Error", "DF", "t value", "p-value")
      w.coef <- apply(rand.cf, 2, function(x) weighted.mean(x, w = ww[cf.idx]))
      random.cem["Estimate", ] <- w.coef
      random.cem["Std. Error", ] <- random.cem["Std. Error", ] * sqrt(sum(ww^2)/sum(ww)^2)
      random.cem["t value", ] <- random.cem["Estimate", ] / random.cem["Std. Error", ]
      random.cem["p-value", ] <- 2 * (1 - pnorm(random.cem["t value", ]))
      ranef.cem <- random.effects(rand.cem)
      att.model <- random.cem
      TE <- rand.cf[, obj$treatment]
      names(TE) <- rownames(rand.cf)
    }
  }
  out <- list(att.model = att.model, tab = obj$tab, treatment = obj$treatment,
    extrapolate = extrapolate, mod.type = mod.type, TE = TE)
  class(out) <- "cem.att"
  return(out)
}

mod_rf <- function() {
  out <- do.call(mod, list(formula = formula, data = data,
    subset = obj$matched == TRUE & obj$groups == obj$g.names[1], ntree = ntree))
  response <- all.vars(formula)[attr(terms(formula), "response")]
  tmp.data <- data
  tmp.data[, obj$treatment] <- 0
  prd <- predict(out, tmp.data)
  TEi <- data[, response] - prd
  if (extrapolate) {
    g <- function(i) {
      idt <- which(obj$strata == i & obj$groups == obj$g.names[2])
      mean(TEi[idt])
    }
    strata <- na.omit(unique(obj$strata))
    TE <- sapply(strata, g)
    names(TE) <- strata
    ww <- table(obj$strata, obj$groups)[, 2]
    w.coef <- weighted.mean(TE, ww, na.rm = TRUE)
    idx <- which(is.na(TE))
    if (length(idx) > 0) TE <- TE[-idx]
    rf.cem <- matrix(NA, 4, 1)
    v0 <- var(data[obj$groups == obj$g.names[2], response])
    v1 <- var(prd[obj$groups == obj$g.names[2]])
  }
  else {
    g <- function(i) {
      idt <- which(obj$mstrata == i & obj$groups == obj$g.names[2])
      mean(TEi[idt])
    }
    mstrata <- na.omit(unique(obj$mstrata))
    TE <- sapply(mstrata, g)
    names(TE) <- mstrata
    ww <- table(obj$mstrata, obj$groups)[, 2]
    w.coef <- weighted.mean(TE, ww, na.rm = TRUE)
    idx <- which(is.na(TE))
    if (length(idx) > 0) TE <- TE[-idx]
    rf.cem <- matrix(NA, 4, 1)
    v0 <- var(data[obj$matched == TRUE & obj$groups == obj$g.names[2], response])
    v1 <- var(prd[obj$matched == TRUE & obj$groups == obj$g.names[2]])
  }
  dimnames(rf.cem) <- list(c("Estimate", "Std. Error", "t value", "p-value"), obj$treatment)
  rf.cem["Estimate", ] <- w.coef
  rf.cem["Std. Error", ] <- sqrt((v1 + v0) * sum(ww^2)/sum(ww)^2)
  rf.cem["t value", ] <- rf.cem["Estimate", ] / rf.cem["Std. Error", ]
  rf.cem["p-value", ] <- 2 * (1 - pnorm(rf.cem["t value", ]))
  att.model <- rf.cem
  out <- list(att.model = att.model, tab = obj$tab, treatment = obj$treatment,
    extrapolate = extrapolate, mod.type = mod.type, TE = TE)
  class(out) <- "cem.att"
  return(out)
}
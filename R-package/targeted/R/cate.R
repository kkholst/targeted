ate_if_fold <- function(fold, data,
                     propensity_model, response_model,
                     treatment, level, stratify=FALSE) {
  if (length(fold) == NROW(data)) { ## No cross-fitting
    dtrain <- data
    deval <- data
  } else {
    dtrain <- data[-fold, ]
    deval <- data[fold, ]
  }

  pmod <- propensity_model$estimate(dtrain)
  X <- deval
  if (stratify) {
    idx <- which(dtrain[, treatment]==level)
    tmp <- response_model$estimate(dtrain[idx, , drop=FALSE])
  } else {
    tmp <- response_model$estimate(dtrain)
    X[, treatment] <- level
  }

  A <- propensity_model$response(deval)
  Y <- response_model$response(deval, na.action=lava::na.pass0)
  pr <- propensity_model$predict(newdata = deval)
  if (NCOL(pr)>1)
    pr <- pr[, 2]
  eY <- response_model$predict(newdata = X)
  part1 <- A / pr * (Y - eY)
  IC <- part1 + eY
  adj <- NULL
  if (inherits(pmod, "glm")) {
    pX <- propensity_model$design(deval, intercept = TRUE)
    dlinkinv <- pmod$family$mu.eta
    adj <- -part1 / pr * dlinkinv(pmod$family$linkfun(pr))
    for (i in seq_len(ncol(pX))) {
      pX[, i] <- pX[, i] * adj
    }
    adj <- pX
  }
  return(list(IC = IC, pmod = pr, qmod = eY, adj = adj))
}

cate_fold1 <- function(fold, data, score, treatment_des) {
  y <- score[fold]
  x <- update(treatment_des, data[fold, , drop = FALSE])$x
  lm.fit(y = y, x = x)$coef
}

##' Conditional Average Treatment Effect estimation via Double Machine Learning
##'
##' @title Conditional Average Treatment Effect estimation
##' @param treatment formula specifying treatment and variables to condition on
##' @param response_model formula or ml_model object (formula => glm)
##' @param propensity_model formula or ml_model object (formula => glm)
##' @param contrast treatment contrast (default 1 vs 0)
##' @param data data.frame
##' @param nfolds Number of folds
##' @param type 'dml1' or 'dml2'
##' @param silent supress all messages and progressbars
##' @param mc.cores mc.cores Optional number of cores. parallel::mcmapply used
##'   instead of future
##' @param stratify If TRUE the response_model will be stratified by treatment
##' @param ... additional arguments to future.apply::future_mapply
##' @return cate.targeted object
##' @author Klaus Kähler Holst, Andreas Nordland
##' @examples
##' sim1 <- function(n=1e4,
##'                  seed=NULL,
##'                  return_model=FALSE, ...) {
##' suppressPackageStartupMessages(require("lava"))
##' if (!is.null(seed)) set.seed(seed)
##' m <- lava::lvm()
##' regression(m, ~a) <- function(z1,z2,z3,z4,z5)
##'          cos(z1)+sin(z1*z2)+z3+z4+z5^2
##' regression(m, ~u) <- function(a,z1,z2,z3,z4,z5)
##'         (z1+z2+z3)*a + z1+z2+z3 + a
##' distribution(m, ~a) <- binomial.lvm()
##' if (return_model) return(m)
##' lava::sim(m, n, p=par)
##' }
##'
##' d <- sim1(200)
##' e <- cate(a ~ z1+z2+z3, response=u~., data=d)
##' e
##' @export
cate <- function(treatment,
                 response_model,
                 propensity_model,
                 contrast = c(1, 0),
                 data,
                 nfolds = 5,
                 type = "dml2",
                 silent = FALSE,
                 stratify = FALSE,
                 mc.cores,
                 ...) {

  cl <- match.call()
  if (is.character(treatment)) {
    treatment <- as.formula(paste0(treatment, "~", 1))
  }
  desA <- design(treatment, data, intercept=TRUE, rm_envir=FALSE)
  if (inherits(response_model, "formula")) {
    response_model <- ML(response_model)
  }
  response_var <- lava::getoutcome(response_model$formula, data=data)
  treatment_var <- lava::getoutcome(treatment)
  if (length(contrast) > 2) {
    stop("Expected contrast vector of length 1 or 2.")
  }
  propensity_outcome <- function(treatment_level)
    paste0("I(", treatment_var, "==", treatment_level, ")")
  if (missing(propensity_model)) {
    newf <- reformulate(
      paste0(" . - ", response_var),
      response=propensity_outcome(contrast[1])
    )
    propensity_model <- ML(newf, family=binomial)
  }
  if (inherits(propensity_model, "formula")) {
    propensity_model <- ML(propensity_model, family=binomial)
  }

  n <- nrow(data)
  if (nfolds<1) nfolds <- 1
  folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
  folds <- lapply(folds, sort)
  ff <- Reduce(c, folds)
  idx <- order(ff)
  scores <- adj <- list()
  if (!silent) {
    pb <- progressr::progressor(steps = length(contrast) * nfolds)
  }
  fargs <- rbind(expand.grid(seq_len(nfolds), contrast))

  procfold <- function(a, fold,
                       data,
                       propensity_model,
                       response_model,
                       treatment_var,
                       stratify,
                       folds,
                       ...) {
    rmod <- response_model$clone(deep = TRUE)
    pmod <- propensity_model$clone(deep = TRUE)
    newf <- reformulate(as.character(pmod$formula)[[3]], propensity_outcome(a))
    pmod$update(newf)
    if (!silent) pb()
    val <- list(ate_if_fold(folds[[fold]], data,
      propensity_model = pmod,
      response_model = rmod,
      treatment = treatment_var,
      level = a, stratify = stratify
      ))
    return(val)
  }

  if (!missing(mc.cores)) {
    val <- parallel::mcmapply(procfold,
      a = as.list(fargs[, 2]), fold = as.list(fargs[, 1]),
      mc.cores = 1,
      MoreArgs = list(
        propensity_model = propensity_model,
        response_model = response_model,
        treatment_var = treatment_var,
        data = data, folds = folds, stratify = stratify
      ),
      ...
     )
  } else {
    val <- future.apply::future_mapply(procfold,
      a = as.list(fargs[, 2]), fold = as.list(fargs[, 1]),
      future.seed = TRUE,
      ## future.packages = c("lava", "targeted", "R6"),
      MoreArgs = list(
        propensity_model = propensity_model,
        response_model = response_model,
        treatment_var = treatment_var,
        data = data, folds = folds, stratify = stratify
      ),
      ...
    )
  }

  for (i in contrast) {
    ii <- which(fargs[, 2] == i)
    scores <- c(
      scores,
      list(unlist(lapply(ii, function(x) val[[x]]$IC))[idx])
    )
    if (!is.null(val[[1]]$adj)) {
      A <- lapply(ii, function(x) {
        val[[x]]$adj
      })
      adj <- c(adj, list(Reduce(rbind, A)[idx, , drop = FALSE]))
    }
  }
  names(scores) <- contrast
  if (length(adj)>0) names(adj) <- contrast

  Y <- scores[[1]]
  if (length(contrast)>1)
    Y <- Y-scores[[2]]
  if (type=="dml1") {
    est1 <- lapply(folds, function(x) cate_fold1(x, data, Y, desA))
    est <- colMeans(Reduce(rbind, est1))
  } else {
    est <- coef(lm(Y ~ -1+desA$x))
  }
  names(est) <- colnames(desA$x)

  V <- desA$x
  h0 <- V%*%est
  h1 <- V
  r <- (Y-h0)
  IF <- apply(h1, 2, function(x) x*r)
  A <- solve(crossprod(V))*n
  IF <- IF %*% A
  rownames(IF) <- rownames(data)

  ## Expectation of potential outcomes
  resp <- lava::getoutcome(response_model$formula, data = data)
  nam <- paste0("E[", resp, "(", names(scores), ")]")
  est0 <- unlist(lapply(scores, mean))
  IF0 <- c()
  for (i in seq_along(est0)) {
    newIF <- scores[[i]] - est0[i]
    if (length(adj) > 0) {
      pmod <- propensity_model$clone(deep = TRUE)
      newf <- reformulate(
        as.character(pmod$formula)[[3]],
        propensity_outcome(contrast[i])
      )
      pmod$update(newf)
      icprop <- IC(pmod$estimate(data))
      newIF <- newIF + icprop %*% colMeans(adj[[i]])
    }
    IF0 <- cbind(IF0,  newIF)
  }

  names(est0) <- nam
  est <- c(est0, est)
  IF <- cbind(IF0, IF)
  estimate <- lava::estimate(coef=est, IC=IF)
  estimate$model.index <- list(
    seq_along(est0),
    seq_along(est) + length(est0)
  )
  potential.outcomes <- as.list(nam)
  names(potential.outcomes) <- names(scores)

  res <- list(folds=folds, scores=scores, treatment_des=desA,
              est=est,
              potential.outcomes=potential.outcomes,
              call=cl,
              estimate=estimate)
  class(res) <- c("cate.targeted", "targeted")
  return(res)
}

score_fold <- function(fold,
                       data,
                       propensity_model,
                       response_model,
                       importance_model,
                        treatment, level) {
  dtrain <- data[-fold, ]
  deval <- data[fold, ]

  # training
  tmp <- propensity_model$estimate(dtrain)
  tmp <- response_model$estimate(dtrain)
  A <- propensity_model$response(dtrain)
  Y <- response_model$response(dtrain)
  X <- dtrain
  X[, treatment] <- level
  pr <- propensity_model$predict(newdata = dtrain)
  if (NCOL(pr) > 1) {
    pr <- pr[, 2]
  }
  eY <- response_model$predict(newdata = X)
  D <- A / pr * (Y - eY) + eY
  X[["D_"]] <- D
  tmp <- importance_model$estimate(data = X)

  # evaluation
  A <- propensity_model$response(deval)
  Y <- response_model$response(deval)
  X <- deval
  X[, treatment] <- level
  pr <- propensity_model$predict(newdata = deval)
  if (NCOL(pr) > 1) {
    pr <- pr[, 2]
  }
  eY <- response_model$predict(newdata = X)
  D <- A / pr * (Y - eY) + eY
  II <- importance_model$predict(newdata = X)

  return(list(II = II, D = D))
}

#' Conditional Relative Risk estimation via Double Machine Learning
#'
#' @title Conditional Relative Risk estimation
#' @param treatment formula specifying treatment and variables to condition on
#' @param response_model SL object
#' @param propensity_model SL object
#' @param importance_model  SL object
#' @param contrast treatment contrast (default 1 vs 0)
#' @param data data.frame
#' @param nfolds Number of folds
#' @param type 'dml1' or 'dml2'
#' @param ... additional arguments to SuperLearner
#' @return cate.targeted object
#' @author Klaus Kähler Holst & Andreas Nordland
#' @examples
#' sim1 <- function(n=1e4,
#'                  seed=NULL,
#'                  return_model=FALSE, ...){
#' suppressPackageStartupMessages(require("lava"))
#' if (!is.null(seed)) set.seed(seed)
#' m <- lava::lvm()
#' distribution(m, ~x) <- gaussian.lvm()
#' distribution(m, ~v) <- gaussian.lvm(mean = 10)
#' distribution(m, ~a) <- binomial.lvm("logit")
#' regression(m, "a") <- function(v, x){.1*v + x}
#' distribution(m, "y") <- gaussian.lvm()
#' regression(m, "y") <- function(a, v, x){v+x+a*x+a*v*v}
#' if (return_model) return(m)
#' lava::sim(m, n = n)
#' }
#'
#' d <- sim1(n = 2e3, seed = 1)
#' if (require("SuperLearner",quietly=TRUE)) {
#'   e <- crr(data=d,
#'            type = "dml2",
#'            treatment = a ~ v,
#'            response_model = ML(y~ a*(x + v + I(v^2))),
#'            importance_model = ML(D_ ~ v + I(v^2)),
#'            propensity_model = ML(a ~ x + v + I(v^2), family=binomial),
#'            nfolds = 2)
#'   summary(e) # the true parameters are c(1,1)
#' }
#'
#' @export
crr <- function(treatment,
                response_model,
                propensity_model,
                importance_model,
                contrast=c(1, 0),
                data,
                nfolds=5,
                type="dml1",
                ...) {
  cl <- match.call()
  if (is.character(treatment)) {
    treatment <- as.formula(paste0(treatment, "~", 1))
  }
  desA <- design(treatment, data, intercept=TRUE, rm_envir=FALSE)
  if (inherits(response_model, "formula")) {
    response_model <- SL(response_model, ...)
  }
  if (length(contrast)!=2)
    stop("Expected contrast vector of length 2.")

  response_var <- lava::getoutcome(response_model$formula, data=data)
  treatment_var <- lava::getoutcome(treatment)
  treatment_f <- function(treatment_level, x=paste0(".-", response_var))
    as.formula(paste0("I(", treatment_var, "==", treatment_level, ") ~ ", x))
  if (missing(propensity_model)) {
    propensity_model <- SL(treatment_f(contrast[1]), ..., binomial=TRUE)
  }
  if (missing(importance_model)) {
    importance_formula <- update(treatment, D_~.)
    importance_model <- SL(importance_formula, ...)
  }

  n <- nrow(data)
  folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
  folds <- lapply(folds, sort)

  ff <- Reduce(c, folds)
  idx <- order(ff)

  # D_a = I(A=a)/P(A=a|W)[Y - E[Y|A=a, W]] + E[Y|A=a, W], a = {1,0}
  D <- list()
  # II = E[E[Y|A=a, W]|V] = E[D_a|V], a = {1,0}
  II <- list()
  pb <- progressr::progressor(steps = length(contrast)*nfolds)
  for (i in seq_along(contrast)) {
    a <- contrast[i]
    propensity_model$update(treatment_f(a))
    val <- c()
    for (f in folds) {
      pb()
      val <- c(val, list(score_fold(f,
                                 data = data,
                                 propensity_model = propensity_model,
                                 response_model = response_model,
                                 importance_model = importance_model,
                                 treatment=treatment_var, level=a)))
    }
    d <- lapply(val, function(x) x[["D"]])
    d <- unlist(d)[idx]
    l <- lapply(val, function(x) x[["II"]])
    l <- unlist(l)[idx]
    D <- c(D, list(d))
    II <- c(II, list(l))
  }
  names(D) <- contrast
  names(II) <- contrast

  score <- D[[1]]*II[[2]] - D[[2]]*II[[1]]
  score <- score + II[[1]] * II[[2]]
  score <- score * II[[2]]^(-2)

  if (type=="dml1") {
    est1 <- lapply(folds, function(x) cate_fold1(x,
                                                 data = data,
                                                 score = score,
                                                 treatment_des = desA))
    est <- colMeans(Reduce(rbind, est1))
  } else {
    est <- coef(lm(score ~ -1+desA$x))
  }
  names(est) <- names(desA$x)

  M1 <- desA$x
  C <-  -n^(-1) * crossprod(M1)
  IF <- -solve(C) %*% t(M1 * as.vector(score - M1 %*% est))
  IF <- t(IF)

  estimate <- estimate(coef=est, IC=IF)
  res <- list(folds=folds,
              score=score,
              treatment_des=desA,
              IF=IF,
              est=est,
              call=cl,
              estimate=estimate)
  class(res) <- c("crr.targeted", "targeted")
  return(res)
}

#' Conditional average treatment effect estimation via Double Machine Learning
#'
#' @title Conditional Relative Risk estimation
#' @param treatment formula specifying treatment and variables to condition on
#' @param link Link function
#' @param response_model SL object
#' @param propensity_model SL object
#' @param importance_model SL object
#' @param contrast treatment contrast (default 1 vs 0)
#' @param data data.frame
#' @param nfolds Number of folds
#' @param type 'dml1' or 'dml2'
#' @param ... additional arguments to SuperLearner
#' @return cate.targeted object
#' @author Klaus Kähler Holst & Andreas Nordland
#' @examples
#' # Example 1:
#' sim1 <- function(n=1e4,
#'                  seed=NULL,
#'                  return_model=FALSE, ...){
#' suppressPackageStartupMessages(require("lava"))
#' if (!is.null(seed)) set.seed(seed)
#' m <- lava::lvm()
#' distribution(m, ~x) <- gaussian.lvm()
#' distribution(m, ~v) <- gaussian.lvm(mean = 10)
#' distribution(m, ~a) <- binomial.lvm("logit")
#' regression(m, "a") <- function(v, x){.1*v + x}
#' distribution(m, "y") <- gaussian.lvm()
#' regression(m, "y") <- function(a, v, x){v+x+a*x+a*v*v}
#' if (return_model) return(m)
#' lava::sim(m, n = n)
#' }
#'
#' if (require("SuperLearner",quietly=TRUE)) {
#'   d <- sim1(n = 1e3, seed = 1)
#'   e <- cate_link(data=d,
#'            type = "dml2",
#'            treatment = a ~ v,
#'            response_model = y~ a*(x + v + I(v^2)),
#'            importance_model = SL(D_ ~ v + I(v^2)),
#'            nfolds = 10)
#'   summary(e) # the true parameters are c(1,1)
#' }
#' @export
cate_link <- function(treatment,
                link = "identity",
                response_model,
                propensity_model,
                importance_model,
                contrast=c(1, 0),
                data,
                nfolds=5,
                type="dml1",
                ...) {
  cl <- match.call()
  if (is.character(treatment)) {
    treatment <- as.formula(paste0(treatment, "~", 1))
  }
  desA <- design(treatment, data, intercept=TRUE, rm_envir=FALSE)
  if (inherits(response_model, "formula")) {
    response_model <- SL(response_model, ...)
  }
  if (length(contrast)>2)
    stop("Expected contrast vector of length 1 or 2.")

  response_var <- lava::getoutcome(response_model$formula, data=data)
  treatment_var <- lava::getoutcome(treatment)
  treatment_f <- function(treatment_level, x=paste0(".-", response_var))
    as.formula(paste0("I(", treatment_var, "==", treatment_level, ") ~ ", x))

  if (missing(propensity_model)) {
    propensity_model <- SL(treatment_f(contrast[1]), ..., binomial=TRUE)
  }

  if (missing(importance_model)) {
    importance_formula <- update(treatment, D_~.)
    importance_model <- SL(importance_formula, ...)
  }

  if (link == "identity") {
    g <- identity
    gd <- function(x) rep(1, length(x))
  } else if (link == "log") {
    g <- log
    gd <- function(x) 1/x
  } else if (link == "logit") {
    g <- lava::logit
    gd <- function(x) 1/x + 1/(1-x)
  }

  n <- nrow(data)
  folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
  folds <- lapply(folds, sort)
  ff <- Reduce(c, folds)
  idx <- order(ff)
  # D_a = I(A=a)/P(A=a|W)[Y - E[Y|A=a, W]] + E[Y|A=a, W], a = {1,0}
  D <- list()
  # II = E[E[Y|A=a, W]|V] = E[D_a|V], a = {1,0}
  II <- list()
  pb <- progressr::progressor(steps = length(contrast)*nfolds)
  for (i in seq_along(contrast)) {
    a <- contrast[i]
    propensity_model$update(treatment_f(a))
    val <- c()
    for (f in folds) {
      pb()
      val <- c(val, list(score_fold(f,
                                    data = data,
                                    propensity_model = propensity_model,
                                    response_model = response_model,
                                    importance_model = importance_model,
                                    treatment=treatment_var, level=a)))
    }
    d <- lapply(val, function(x) x[["D"]])
    d <- unlist(d)[idx]
    l <- lapply(val, function(x) x[["II"]])
    l <- unlist(l)[idx]
    D <- c(D, list(d))
    II <- c(II, list(l))
  }
  names(D) <- contrast
  names(II) <- contrast

  score <- gd(II[[1]])*(D[[1]] - II[[1]]) + g(II[[1]])
  if (length(contrast)>1) {
    score <- score - (gd(II[[2]])*(D[[2]] - II[[2]]) + g(II[[2]]))
  }

  if (type=="dml1") {
    est1 <- lapply(folds, function(x) cate_fold1(x,
                                                 data = data,
                                                 score = score,
                                                 treatment_des = desA))
    est <- colMeans(Reduce(rbind, est1))
  } else {
    est <- coef(lm(score ~ -1+desA$x))
  }
  names(est) <- colnames(desA$x)

  M1 <- desA$x
  C <-  -n^(-1) * crossprod(M1)
  IF <- -solve(C) %*% t(M1 * as.vector(score - M1 %*% est))
  IF <- t(IF)

  estimate <- estimate(coef=est, IC=IF)

  res <- list(folds=folds,
              score=score,
              treatment_des=desA,
              IF=IF,
              est=est,
              call=cl,
              estimate=estimate)
  class(res) <- c("cate.targeted", "targeted")
  return(res)
}

score_fold <- function(fold,
                       data,
                       propensity_model,
                       response_model,
                       importance_model,
                        treatment, level) {
  dtrain <- data[-fold, ]
  deval <- data[fold, ]

  # training
  tmp <- propensity_model$estimate(dtrain) # nolint
  tmp <- response_model$estimate(dtrain) # nolint
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
  treatment_f <- function(treatment_level, x = paste0(".-", response_var)) {
    as.formula(paste0("I(", treatment_var, "==", treatment_level, ") ~ ", x))
  }
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
    est1 <- lapply(folds, function(x) {
      cate_fold1(x,
        data = data,
        score = score,
        cate_des = desA
      )
    })
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
              cate_des=desA,
              IF=IF,
              coef=est,
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
  treatment_f <- function(treatment_level, x = paste0(".-", response_var)) {
    as.formula(paste0("I(", treatment_var, "==", treatment_level, ") ~ ", x))
  }
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
    est1 <- lapply(folds, function(x) {
      cate_fold1(x,
        data = data,
        score = score,
        cate_des = desA
      )
    })
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
              cate_des=desA,
              IF=IF,
              est=est,
              call=cl,
              estimate=estimate)
  class(res) <- c("cate.targeted", "targeted")
  return(res)
}

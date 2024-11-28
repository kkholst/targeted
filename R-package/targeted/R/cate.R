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
    ## Term from estimating propensity model / treatment probabilities that does
    ## not go to zero in probability unless Q-model is correct
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

cate_fold1 <- function(fold, data, score, cate_des) {
  y <- score[fold]
  x <- update(cate_des, data[fold, , drop = FALSE])$x
  lm.fit(y = y, x = x)$coef
}

##' Conditional Average Treatment Effect estimation with cross-fitting.
##'
##' We have observed data \eqn{(Y,A,W)} where \eqn{Y} is the response variable,
##' \eqn{A} the binary treatment, and \eqn{W} covariates. We further let \eqn{V}
##' be a subset of the covariates. Define the conditional potential mean outcome
##' \deqn{\psi_{a}(P)(V) = E_{P}[E_{P}(Y\mid A=a, W)|V]} and let \eqn{m(V;
##' \beta)} denote a parametric working model, then the target parameter is the
##' mean-squared error \deqn{\beta(P) = \operatorname{argmin}_{\beta}
##' E_{P}[\{\Psi_{1}(P)(V)-\Psi_{0}(P)(V)\} - m(V; \beta)]^{2}}
##' @title Conditional Average Treatment Effect estimation
##' @param response_model formula or ml_model object (formula => glm)
##' @param propensity_model formula or ml_model object (formula => glm)
##' @param cate_model formula specifying regression design for conditional
##'   average treatment effects
##' @param contrast treatment contrast (default 1 vs 0)
##' @param data data.frame
##' @param nfolds Number of folds
##' @param rep Number of replications of cross-fitting procedure
##' @param silent supress all messages and progressbars
##' @param stratify If TRUE the response_model will be stratified by treatment
##' @param mc.cores mc.cores Optional number of cores. parallel::mcmapply used
##'   instead of future
##' @param ... additional arguments to future.apply::future_mapply
##' @return cate.targeted object
##' @author Klaus Kähler Holst, Andreas Nordland
##' @references Mark J. van der Laan (2006) Statistical Inference for Variable
##'   Importance, The International Journal of Biostatistics.
##' @examples
##' sim1 <- function(n=1000, ...) {
##'   w1 <- rnorm(n)
##'   w2 <- rnorm(n)
##'   a <- rbinom(n, 1, expit(-1 + w1))
##'   y <- cos(w1) + w2*a + 0.2*w2^2 + a + rnorm(n)
##'   data.frame(y, a, w1, w2)
##' }
##'
##' d <- sim1(5000)
##' ## ATE
##' cate(cate_model=~1,
##'      response_model=y~a*(w1+w2),
##'      propensity_model=a~w1+w2,
##'      data=d)
##' ## CATE
##' cate(cate_model=~1+w2,
##'      response_model=y~a*(w1+w2),
##'      propensity_model=a~w1+w2,
##'      data=d)
##'
##' \dontrun{ ## superlearner example
##' mod1 <- list(
##'    glm=predictor_glm(y~w1+w2),
##'    gam=predictor_gam(y~s(w1) + s(w2))
##' )
##' s1 <- predictor_sl(mod1, nfolds=5)
##' cate(cate_model=~1,
##'      response_model=s1,
##'      propensity_model=predictor_glm(a~w1+w2, family=binomial),
##'      data=d,
##'      stratify=TRUE)
##' }
##'
##' @export
cate <- function(response_model,
                 propensity_model,
                 cate_model = ~1,
                 contrast = c(1, 0),
                 data,
                 nfolds = 1,
                 rep = 1,
                 ## Average cross-fitted IFs
                 ## or average parameter estimates:
                 silent = FALSE,
                 stratify = FALSE,
                 mc.cores,
                 ...) {

  cl <- match.call()
  dots <- list(...)
  n <- nrow(data)

  if ("treatment" %in% names(dots)) { ## Backward compatibility
    if (!is.null(cate_model)) {
      stop(
        "Calling `cate` with both the obsolete 'treatment'",
        " and the new 'cate_model' argument"
      )
    }
    cate_model <- dots$treatment
    if (missing(propensity_model)) {
      propensity_model <- lava::getoutcome(cate_model)
    }
  }

  if (is.character(propensity_model)) {
    propensity_model <- stats::reformulate("1", propensity_model)
  }

  desA <- design(cate_model, data, intercept=TRUE, rm_envir=FALSE)
  if (inherits(response_model, "formula")) {
    response_model <- ML(response_model)
  }

  if (length(contrast) > 2) {
    stop("Expected contrast vector of length 1 or 2.")
  }
  propensity_outcome <- function(treatment_level)
    paste0("I(", treatment_var, "==", treatment_level, ")")
  if (missing(propensity_model)) {
    response_var <- lava::getoutcome(response_model$formula, data=data)
    newf <- reformulate(
      paste0(" . - ", response_var),
      response=propensity_outcome(contrast[1])
    )
    propensity_model <- ML(newf, family=binomial)
  }
  if (inherits(propensity_model, "formula")) {
    propensity_model <- ML(propensity_model, family = binomial)
  }
  treatment_var <- lava::getoutcome(propensity_model$formula)

  procfold <- function(a, fold,
                       data,
                       propensity_model,
                       response_model,
                       treatment_var,
                       stratify,
                       folds,
                       pb,
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


  calculate_scores <- function(args) {
    if (!silent & (rep == 1)) {
      pb <- progressr::progressor(steps = length(contrast) * nfolds)
    } else {
      pb <- function(...) NULL
    }
    ## Create random folds
    if (nfolds<1) nfolds <- 1
    folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
    folds <- lapply(folds, sort)
    ff <- Reduce(c, folds)
    idx <- order(ff)

    fargs <- rbind(expand.grid(seq_len(nfolds), contrast))
    if (mc) {
      val <- parallel::mcmapply(procfold,
        a = as.list(fargs[, 2]), fold = as.list(fargs[, 1]),
        mc.cores = 1,
        MoreArgs = list(
          propensity_model = propensity_model,
          response_model = response_model,
          treatment_var = treatment_var,
          data = data, folds = folds,
          stratify = stratify,
          pb=pb
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
          data = data, folds = folds,
          stratify = stratify,
          pb=pb
        ),
        ...
      )
    }

    qval <- pval <- scores <- adj <- list()
    for (i in contrast) {
      ii <- which(fargs[, 2] == i)
      scores <- c(
        scores,
        list(unlist(lapply(ii, function(x) val[[x]]$IC))[idx])
      )
      qval <- c(
        qval,
        list(unlist(lapply(ii, function(x) val[[x]]$qmod))[idx])
      )
      pval <- c(
        pval,
        list(unlist(lapply(ii, function(x) val[[x]]$pmod))[idx])
      )
      if (!is.null(val[[1]]$adj)) {
        A <- lapply(ii, function(x) {
          val[[x]]$adj
        })
        adj <- c(adj, list(Reduce(rbind, A)[idx, , drop = FALSE]))
      }
    }
    names(scores) <- contrast
    names(qval) <- contrast
    names(pval) <- contrast
    if (length(adj) > 0) names(adj) <- contrast
    list(scores = scores, adj = adj, qval = qval, pval = pval)
  }

  mc <- !missing(mc.cores)
  if (rep > 1) {
    pb <- progressr::progressor(steps = rep)
    f <- function(...) {
      pb()
      return(calculate_scores())
    }
    if (mc) {
      val <- parallel::mclapply(1:rep, f,
        mc.cores = mc.cores, pb = pb, ...
      )
    } else {
      val <- future.apply::future_lapply(
        1:rep, f,
        pb = pb, future.seed = TRUE, ...
      )
    }
  } else {
    val <- list(calculate_scores())
  }

  pval <- val[[1]]$pval
  qval <- val[[1]]$qval
  scores <- val[[1]]$scores
  adj <- val[[1]]$adj
  if (rep > 1) {
    for (i in 2:rep) {
      for (j in seq_len(length(scores))) {
        scores[[j]] <- scores[[j]] + val[[i]]$scores[[j]]
        qval[[j]] <- qval[[j]] + val[[i]]$qval[[j]]
        pval[[j]] <- pval[[j]] + val[[i]]$pval[[j]]
        if (length(adj) > 0) {
          adj[[j]] <- adj[[j]] + val[[j]]$adj[[j]]
        }
      }
    }
    for (j in seq_len(length(scores))) {
      scores[[j]] <- scores[[j]] / rep
      qval[[j]] <- qval[[j]] / rep
      pval[[j]] <- pval[[j]] / rep
      if (length(adj) > 0) {
        adj[[j]] <- adj[[j]] / rep
      }
    }
  }

  Y <- scores[[1]]
  if (length(contrast) > 1) {
    Y <- Y - scores[[2]]
  }
  # if (type=="dml1") {
  #   est1 <- lapply(folds, function(x) cate_fold1(x, data, Y, desA))
  #   est <- colMeans(Reduce(rbind, est1))
  # } else {
  est <- coef(lm(Y ~ -1+desA$x))
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

  res <- list(scores=scores, cate_des=desA,
              coef=est,
              response_model = response_model,
              propensity_model = propensity_model,
              pval = pval, qval = qval,
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
                                                 cate_des = desA))
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
                                                 cate_des = desA))
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

ate_if_fold <- function(fold, data,
                     propensity_model, response_model,
                     treatment, level) {
  dtrain <- data[-fold,]
  deval <- data[fold,]
  tmp <- propensity_model$estimate(dtrain)
  tmp <- response_model$estimate(dtrain)

  A <- propensity_model$response(deval)
  Y <- response_model$response(deval)
  X <- deval
  X[, treatment] <- level
  pr <- propensity_model$predict(newdata=deval)
  if (NCOL(pr)>1)
    pr <- pr[,2]
  eY <- response_model$predict(newdata=X)
  IF <- A/pr*(Y-eY) + eY
  return(IF)
}

cate_fold1 <- function(fold, data, score, treatment_des) {
  y <- score[fold]
  x <- update(treatment_des, data[fold,])$x
  lm.fit(y=y, x=x)$coef
}

##' Conditional Average Treatment Effect estimation via Double Machine Learning
##'
##' @title Conditional Average Treatment Effect estimation
##' @param treatment formula specifying treatment and variables to condition on
##' @param response_model SL object
##' @param propensity_model SL object
##' @param contrast treatment contrast (default 1 vs 0)
##' @param data data.frame
##' @param nfolds Number of folds
##' @param type 'dml1' or 'dml2'
##' @param ... additional arguments to SuperLearner
##' @return cate.targeted object
##' @author Klaus Kähler Holst
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
##' if (require("SuperLearner",quietly=TRUE)) {
##'   e <- cate(a ~ z1+z2+z3, response=u~., data=d)
##'   e
##' }
##'
##' @export
cate <- function(treatment,
                 response_model,
                 propensity_model,
                 contrast=c(1,0),
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
  n <- nrow(data)
  folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
  folds <- lapply(folds, sort)
  ff <- Reduce(c, folds)
  idx <- order(ff)

  scores <- list()
  pb <- progressr::progressor(steps = length(contrast)*nfolds)
  for (i in seq_along(contrast)) {
    a <- contrast[i]
    propensity_model$update(treatment_f(a))
    val <- c()
    for (f in folds) {
      pb()
      val <- c(val, list(ate_if_fold(f, data,
                                     propensity_model, response_model,
                                     treatment=treatment_var, level=a)))
    }
    val <- unlist(val)[idx]
    scores <- c(scores, list(val))
  }
  names(scores) <- contrast

  Y <- scores[[1]]
  if (length(contrast)>1)
    Y <- Y-scores[[2]]
  if (type=="dml1") {
    est1 <- lapply(folds, function(x) cate_fold1(x, data, Y, desA))
    est <- colMeans(Reduce(rbind, est1))
  } else {
    est <- coef(lm(Y ~ -1+desA$x))
  }
  names(est) <- names(desA$x)

  V <- desA$x
  h0 <- V%*%est
  h1 <- V
  r <- (Y-h0)
  IF <- apply(h1, 2, function(x) x*r)
  A <- solve(crossprod(V))*n
  IF <- IF%*%A
  estimate <- estimate(coef=est, IC=IF)

  res <- list(folds=folds, scores=scores, treatment_des=desA,
              IF=IF, est=est,
              call=cl,
              estimate=estimate)
  class(res) <- c("cate.targeted", "targeted")
  return(res)
}

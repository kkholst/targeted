##' Assumption lean inference via cross-fitting (Double ML). See
##' <doi:10.1111/rssb.12504
##'
##' Let \eqn{Y} be the response variable, \eqn{A} the exposure and \eqn{W}
##' covariates. The target parameter is: \deqn{\Psi(P) = \frac{E(Cov[A,
##' g\{E(Y|A,W)\}\mid W])} {E\{Var(A\mid W)\}} }
##'
##' The \code{response_model} is the model for \eqn{E(Y|A,W)}, and
##' \code{exposure_model} is the model for \eqn{E(A|W)}.
##' \code{link} specifies \eqn{g}.
##'
##' @title Assumption Lean inference for generalized linear model parameters
##' @param response_model formula or ml_model object (formula => glm)
##' @param exposure_model  model for the exposure
##' @param data data.frame
##' @param link Link function (g)
##' @param g_model Model for \eqn{E[g(Y|A,W)|W]}
##' @param nfolds Number of folds
##' @param silent supress all messages and progressbars
##' @param mc.cores mc.cores Optional number of cores. parallel::mcmapply used instead of future
##' @param ... additional arguments to future.apply::future_mapply
##' @return alean.targeted object
##' @author Klaus Kähler Holst
##' @examples
##'
##' sim1 <- function(n, family=gaussian(), ...) {
##'    m <- lvm() |>
##'      distribution(~ y, binomial.lvm()) |>
##'      regression('a', value=function(l) l) |>
##'      regression('y', value=function(a,l) a + l)
##'      if (family$family=="binomial")
##'         distribution(m, ~a) <- binomial.lvm()
##'    sim(m, n)
##' }
##'
##' library(splines)
##' f <- binomial()
##' d <- sim1(1e4, family=f)
##' e <- alean(response_model=ML(y ~ a + bs(l, df=3), family=binomial),
##'            exposure_model=ML(a ~ bs(l, df=3), family=f),
##'            data=d,
##'            link = "logit", mc.cores=1, nfolds=1)
##' e
##'
##' e <- alean(response_model=ML(y ~ a + l, family=binomial),
##'            exposure_model=ML(a ~ l),
##'            data=d,
##'            link = "logit", mc.cores=1, nfolds=1)
##' e
##' @export
alean <- function(response_model,
                  exposure_model,
                  data,
                  link = "identity",
                  g_model,
                  nfolds=1,
                  silent=FALSE,
                  mc.cores,
                  ...) {

  cl <- match.call()
  if (inherits(response_model, "formula")) {
    response_model <- ML(response_model)
  }
  if (inherits(exposure_model, "formula")) {
    A_var <- lava::getoutcome(exposure_model)
    family <- stats::gaussian()
    if (!is.numeric(data[, A_var])) {
      family <- stats::binomial()
    }
    exposure_model <- ML(exposure_model, family = family)
  }
  A_var <- lava::getoutcome(exposure_model$formula)
  A <- exposure_model$response(data)
  A_levels <- sort(unique(A))
  g_model_response <- "gEY_0"
  if (missing(g_model)) {
    tf <- terms(response_model$formula)
    tf <- drop.terms(tf, which(attr(tf, "factors")[A_var, ] == 1))
    g_model <- ML(formula(tf))
  }
  g_model$update(update(
    g_model$formula,
    reformulate(".", response = g_model_response)
    ))

  glink <- stats::quasi(link)
  g <- glink$linkfun
  ginv <- glink$linkinv
  dginv <- glink$mu.eta
  dg <- function(x) 1/dginv(g(x)) ## Dh^-1 = 1/(h'(h^-1(x)))

  n <- NROW(data)
  if (nfolds<1) nfolds <- 1
  folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
  folds <- lapply(folds, sort)
  ff <- Reduce(c, folds)
  idx <- order(ff)
  scores <- list()
  fargs <- seq_len(nfolds)
  if (!silent) {
    pb <- progressr::progressor(steps = NROW(fargs))
  }

  procfold <- function(fold, ...) {
    Ymod <- response_model$clone(deep = TRUE)
    Amod <- exposure_model$clone(deep = TRUE)
    gmod <- g_model$clone(deep = TRUE)

    fold <- folds[[fold]]
    if (length(fold) == NROW(data)) { ## No cross-fitting
      dtrain <- data
      deval <- data
    } else {
      dtrain <- data[-fold, ]
      deval <- data[fold, ]
    }

    tmp <- Ymod$estimate(dtrain) ## E(Y|A,L)
    tmp <- Amod$estimate(dtrain) ## E(A|L)
    EA <- Amod$predict(newdata = deval)
    EY <- Ymod$predict(newdata = deval)
    dtrain[, g_model_response] <- g(EY)

    if (length(A_levels) == 2) {
      X <- deval
      X[, A_var] <- A_levels[2]
      Eg <- EA * Ymod$predict(newdata = X)
      X[, A_var] <- A_levels[1]
      Eg <- Eg + (1 - EA) * Ymod$predict(newdata = X)
    } else {
      tmp <- gmod$estimate(dtrain)
      Eg <- gmod$predict(newdata = deval)
    }
    Y <- Ymod$response(deval, na.action = na.pass)
    A <- Amod$response(deval, na.action = na.pass)
    mu <- dg(EY) * (Y - EY) + g(EY) - Eg
    A. <- (A - EA)
    if (!silent) pb()
    cbind(mu, A.)
  }

  if (!missing(mc.cores)) {
    val <- parallel::mclapply(as.list(fargs), procfold,
      mc.cores = 1,
      MoreArgs = list(),
      ...
    )
  } else {
    val <- future.apply::future_lapply(as.list(fargs), procfold,
      future.seed = TRUE,
      ## future.packages = c("lava", "targeted", "R6"),
      MoreArgs = list(),
      ...
    )
  }

  scores <- Reduce(rbind, val)
  A. <- scores[, 2]
  mu <- scores[, 1]
  est <- coef(lm(mu ~ -1 + A.))
  names(est) <- A_var
  IF <- A.*(mu - est * A.) / mean(A.^2)
  estimate <- estimate(coef=est, IC=IF)
  res <- list(folds=folds, scores=scores,
              IF=IF, est=est,
              call=cl,
              estimate=estimate)
  class(res) <- c("alean.targeted", "targeted")
  return(res)
}


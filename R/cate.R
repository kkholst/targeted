ate_if_fold <- function(fold, data,
                     propensity.model, response.model,
                     treatment, level, stratify=FALSE) {
  if (length(fold) == NROW(data)) { ## No cross-fitting
    dtrain <- data
    deval <- data
  } else {
    dtrain <- data[-fold, ]
    deval <- data[fold, ]
 }

  pmod <- propensity.model$estimate(dtrain)
  X <- deval
  if (stratify) {
    idx <- which(dtrain[, treatment]==level)
    tmp <- response.model$estimate(dtrain[idx, , drop=FALSE]) # nolint
  } else {
    tmp <- response.model$estimate(dtrain)
    X[, treatment] <- level
  }
  A <- propensity.model$response(deval)
  Y <- response.model$response(deval, na.action=lava::na.pass0)
  pr <- propensity.model$predict(newdata = deval)
  if (NCOL(pr)>1)
    pr <- pr[, 2]
  eY <- response.model$predict(newdata = X)
  part1 <- A / pr * (Y - eY)
  IC <- part1 + eY
  adj <- NULL
  if (inherits(pmod, "glm")) {
    ## Term from estimating propensity model / treatment probabilities that does
    ## not go to zero in probability unless Q-model is correct
    pX <- propensity.model$design(deval, intercept = TRUE)
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
  return(lm.fit(y = y, x = x)$coef)
}

#' Conditional Average Treatment Effect estimation with cross-fitting.
#'
#' We have observed data \eqn{(Y,A,W)} where \eqn{Y} is the response variable,
#' \eqn{A} the binary treatment, and \eqn{W} covariates. We further let \eqn{V}
#' be a subset of the covariates. Define the conditional potential mean outcome
#' \deqn{\psi_{a}(P)(V) = E_{P}[E_{P}(Y\mid A=a, W)|V]} and let \eqn{m(V;
#' \beta)} denote a parametric working model, then the target parameter is the
#' mean-squared error \deqn{\beta(P) = \operatorname{argmin}_{\beta}
#' E_{P}[\{\Psi_{1}(P)(V)-\Psi_{0}(P)(V)\} - m(V; \beta)]^{2}}
#' @inheritParams deprecated_argument_names
#' @title Conditional Average Treatment Effect estimation
#' @param response.model formula or ml_model object (formula => glm)
#' @param ... additional arguments to future.apply::future_mapply
#' @param propensity.model formula or ml_model object (formula => glm)
#' @param cate.model formula specifying regression design for conditional
#'   average treatment effects
#' @param contrast treatment contrast (default 1 vs 0)
#' @param data data.frame
#' @param nfolds Number of folds
#' @param rep Number of replications of cross-fitting procedure
#' @param silent suppress all messages and progressbars
#' @param stratify If TRUE the response.model will be stratified by treatment
#' @param mc.cores mc.cores Optional number of cores. parallel::mcmapply used
#'   instead of future
#' @param second.order Add seconder order term to IF to handle misspecification
#'   of outcome models
#' @return cate.targeted object
#' @author Klaus Kähler Holst, Andreas Nordland
#' @references Mark J. van der Laan (2006) Statistical Inference for Variable
#'   Importance, The International Journal of Biostatistics.
#' @examples
#' sim1 <- function(n=1000, ...) {
#'   w1 <- rnorm(n)
#'   w2 <- rnorm(n)
#'   a <- rbinom(n, 1, expit(-1 + w1))
#'   y <- cos(w1) + w2*a + 0.2*w2^2 + a + rnorm(n)
#'   data.frame(y, a, w1, w2)
#' }
#'
#' d <- sim1(5000)
#' ## ATE
#' cate(cate.model=~1,
#'      response.model=y~a*(w1+w2),
#'      propensity.model=a~w1+w2,
#'      data=d)
#' ## CATE
#' cate(cate.model=~1+w2,
#'      response.model=y~a*(w1+w2),
#'      propensity.model=a~w1+w2,
#'      data=d)
#'
#' \dontrun{ ## superlearner example
#' mod1 <- list(
#'    glm = learner_glm(y~w1+w2),
#'    gam = learner_gam(y~s(w1) + s(w2))
#' )
#' s1 <- predictor_sl(mod1, nfolds=5)
#' cate(cate.model=~1,
#'      response.model=s1,
#'      propensity.model=learner_glm(a~w1+w2, family=binomial),
#'      data=d,
#'      stratify=TRUE)
#' }
#'
#' @export
cate <- function(response.model, # nolint
                 propensity.model,
                 cate.model = ~1,
                 contrast = c(1, 0),
                 data,
                 nfolds = 1,
                 rep = 1,
                 silent = FALSE,
                 stratify = FALSE,
                 mc.cores = NULL,
                 second.order = TRUE,
                 response_model = deprecated,
                 cate_model = deprecated,
                 propensity_model = deprecated,
                 treatment = deprecated,
                 ...) {

  cl <- match.call()
  n <- nrow(data)

  dvers <- "1.0.0"
  if (!missing(response_model)) {
    deprecate_arg_warn("response_model", "response.model", "cate", dvers)
    response.model <- response_model
  }

  if (!missing(propensity_model)) {
    deprecate_arg_warn("propensity_model", "propensity.model", "cate", dvers)
    propensity.model <- propensity_model
  }

  if (!missing(cate_model)) {
    deprecate_arg_warn("cate_model", "cate.model", "cate", dvers)
    cate.model <- cate_model
  }

  if (!missing(treatment)) { ## Backward compatibility
    # ~1 is current default value of cate.model
    if (!isTRUE(all.equal(cate.model, ~1))) {
      stop(
        "Calling `cate` with both the obsolete 'treatment'",
        " and the new 'cate.model' argument"
      )
    }
    # only used to inform user that treatment argument is deprecated
    deprecate_arg_warn("treatment", "cate.model", "cate", dvers)
    cate.model <- treatment
  }

  if (missing(propensity.model)) {
    propensity.model <- lava::getoutcome(cate.model)
  }
  if (length(propensity.model) == 0) {
    stop("Empty `propensity.model`")
  }

  if (is.character(propensity.model)) {
    propensity.model <- stats::reformulate("1", propensity.model)
  }

  desA <- design(cate.model, data, intercept=TRUE, rm_envir=FALSE)
  if (inherits(response.model, "formula")) {
    response.model <- learner_glm(response.model)
  }

  if (length(contrast) > 2) {
    stop("Expected contrast vector of length 1 or 2.")
  }
  propensity_outcome <- function(treatment_level) {
    return(paste0("I(", treatment_var, "==", treatment_level, ")"))
  }
  if (missing(propensity.model)) {
    response_var <- lava::getoutcome(response.model$formula, data=data)
    newf <- reformulate(
      paste0(" . - ", response_var),
      response=propensity_outcome(contrast[1])
    )
    propensity.model <- learner_glm(newf, family=binomial)
  }
  if (inherits(propensity.model, "formula")) {
    propensity.model <- learner_glm(propensity.model, family = binomial)
  }
  treatment_var <- lava::getoutcome(propensity.model$formula)

  procfold <- function(a, fold,
                        data,
                        propensity.model,
                        response.model,
                        treatment_var,
                        stratify,
                        folds,
                        ...) {
    rmod <- response.model$clone(deep = TRUE)
    pmod <- propensity.model$clone(deep = TRUE)
    newf <- reformulate(as.character(pmod$formula)[[3]], propensity_outcome(a))
    pmod$update(newf)
    val <- list(ate_if_fold(folds[[fold]], data,
      propensity.model = pmod,
      response.model = rmod,
      treatment = treatment_var,
      level = a, stratify = stratify
    ))
    return(val)
  }


  calculate_scores <- function(args) {
    ## Create random folds
    if (nfolds<1) nfolds <- 1
    folds <- split(sample(1:n, n), rep(1:nfolds, length.out = n))
    folds <- lapply(folds, sort)
    ff <- Reduce(c, folds)
    idx <- order(ff)
    fargs <- rbind(expand.grid(fold = seq_len(nfolds), a = contrast))

    if (!silent && (rep == 1) && (nfolds>1)) {
      pb <- progressr::progressor(message="cross-fitting",
                                    steps = nrow(fargs))
    } else {
      pb <- function(...) invisible(NULL)
    }

    myargs <- list(procfold,
      a = as.list(fargs[, "a"]),
      fold = as.list(fargs[, "fold"]),
      MoreArgs = list(
        propensity.model = propensity.model,
        response.model = response.model,
        treatment_var = treatment_var,
        data = data, folds = folds,
        stratify = stratify
      ), ...
    )
    if (!is.null(mc.cores)) {
      myargs$mc.cores <- ifelse(rep == 1, mc.cores, 1)
      val <- do.call(parallel::mcmapply, myargs)
    } else {
      myargs[[1]] <- function(a, fold, ...) {
        res <- procfold(a = a, fold = fold, ...)
        pb()
        return(res)
      }
      if (!"future.seed" %in% names(myargs)) {
        myargs["future.seed"] <- list(NULL)
      }
      val <- do.call(
        future.apply::future_mapply,
        myargs
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
        A <- lapply(ii, function(x) val[[x]]$adj)
        adj <- c(adj, list(Reduce(rbind, A)[idx, , drop = FALSE]))
      }
    }
    names(scores) <- contrast
    names(qval) <- contrast
    names(pval) <- contrast
    if (length(adj) > 0) names(adj) <- contrast
    return(list(scores = scores, adj = adj, qval = qval, pval = pval))
  }

  if (rep > 1) {
    pb <- progressr::progressor(steps = rep, message="repetition")
    f <- function(...) {
      res <- calculate_scores()
      pb()
      return(res)
    }
    if (!is.null(mc.cores)) {
      val <- parallel::mclapply(1:rep, f,
        mc.cores = mc.cores
      )
    } else {
      myargs <- list(X=1:rep, FUN=f, ...)
      if (!"future.seed" %in% names(myargs)) {
        myargs["future.seed"] <- list(NULL)
      }
      val <- do.call(future.apply::future_lapply, myargs)
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
  resp <- lava::getoutcome(response.model$formula, data = data)
  nam <- paste0("E[", resp, "(", names(scores), ")]")
  est0 <- unlist(lapply(scores, mean))
  IF0 <- c()
  for (i in seq_along(est0)) {
    newIF <- scores[[i]] - est0[i]
    if (length(adj) > 0 && second.order) {
      pmod <- propensity.model$clone(deep = TRUE)
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
              response.model = response.model,
              propensity.model = propensity.model,
              pval = pval, qval = qval,
              potential.outcomes=potential.outcomes,
              call=cl,
              estimate=estimate)
  class(res) <- c("cate.targeted", "targeted")
  return(res)
}

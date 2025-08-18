procfold <- function(a, fold,
                     data,
                     propensity.model,
                     response.model,
                     treatment_var,
                     stratify,
                     folds,
                     ...) {
  qmod <- response.model$clone(deep = TRUE)
  pmod <- propensity.model$clone(deep = TRUE)
  newf <- reformulate(as.character(pmod$formula)[[3]],
                      outcome_level(treatment_var, a))
  pmod$update(newf)
  val <- list(est_nuisance_fold(
    folds[[fold]],
    data,
    propensity.model = pmod,
    response.model = qmod,
    treatment = treatment_var,
    level = a,
    stratify = stratify
  ))
  return(val)
}

est_nuisance_fold <- function(fold,
                              data,
                              propensity.model,
                              response.model,
                              treatment, level,
                              stratify=FALSE) {
  if (length(fold) == NROW(data)) { ## No cross-fitting
    dtrain <- data
    deval <- data
  } else {
    dtrain <- data[-fold, ]
    deval <- data[fold, ]
  }
  propensity.model$estimate(dtrain)
  X <- deval
  if (stratify) {
    idx <- which(dtrain[, treatment]==level)
    tmp <- response.model$estimate(dtrain[idx, , drop=FALSE]) # nolint
  } else {
    tmp <- response.model$estimate(dtrain)
    X[, treatment] <- level
  }
  pr <- propensity.model$predict(newdata = deval)
  if (NCOL(pr)>1)
    pr <- pr[, 2]
  eY <- response.model$predict(newdata = X)
  return(list(pmod = pr, qmod = eY))
}

outcome_level <- function(variable, level) {
    return(paste0("I(", variable, "=='", level, "')"))
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
#' @param response.model formula or learner object (formula => learner_glm)
#' @param ... additional arguments to future.apply::future_mapply
#' @param propensity.model formula or learner object (formula => learner_glm)
#' @param cate.model formula specifying regression design for conditional
#'   average treatment effects
#' @param calibration.model linear calibration model. Specify covariates in
#'   addition to predicted potential outcomes to include in the calibration.
#' @param contrast treatment contrast (default 1 vs 0)
#' @param data data.frame
#' @param nfolds number of folds
#' @param rep number of replications of cross-fitting procedure
#' @param silent suppress all messages and progressbars
#' @param stratify if TRUE the response.model will be stratified by treatment
#' @param mc.cores (optional) number of cores. parallel::mcmapply used instead
#'   of future
#' @param rep.type repeated cross-fitting applied by averaging nuisance models
#'   (`rep.type="nuisance"`) or by average estimates from each replication
#'   (`rep.type="average"`).
#' @param var.type when equal to "IC" the asymptotic variance is derived from
#'   the influence function. Otherwise, based on expressions in Bannick et al.
#'   (2025) valid under different covariate-adaptive randomization schemes (only
#'   available for ATE and when `calibration.model` is also specified)
#' @param second.order add seconder order term to IF to handle misspecification
#'   of outcome models
#' @return cate.targeted object
#' @author Klaus Kähler Holst, Andreas Nordland
#' @references Mark J. van der Laan (2006) Statistical Inference for Variable
#'   Importance, The International Journal of Biostatistics.
#' @examples
#' sim1 <- function(n=1000, ...) {
#'   w1 <- rnorm(n)
#'   w2 <- rnorm(n)
#'   a <- rbinom(n, 1, plogis(-1 + w1))
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
#' s1 <- learner_sl(mod1, nfolds=5)
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
                 calibration.model = NULL,
                 data,
                 contrast,
                 nfolds = 1,
                 rep = 1,
                 silent = FALSE,
                 stratify = FALSE,
                 mc.cores = NULL,
                 rep.type = c("nuisance", "average"),
                 var.type = "IC",
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

  if (inherits(response.model, "formula")) {
    response.model <- learner_glm(response.model)
  }

  if (inherits(propensity.model, "formula")) {
    propensity.model <- learner_glm(propensity.model, family = binomial)
  }
  # treatment reponse variable
  # treatment_response <- all.vars(update(propensity.model$formula, ~1))
  treatment_var <- lava::getoutcome(propensity.model$formula)
  # variable in data.frame, in case propensity-model is of the form `I(a>0) ~ 1`
  # check that treatment variable is part of the response model
  preds <- union(rownames(attr(
    terms(response.model$formula,
      data = data,
      specials = c("strata", "stratify")
    ),
    "factors"
  )), all.vars(response.model$formula))
  if (!stratify && !(treatment_var %in% preds)) {
    warning("treatment variable not present in `response.model`",
            " and stratify=FALSE")
  }

  if (missing(contrast)) {
    contrast <- rev(sort(unique(data[, treatment_var])))
  }

  estimate_nuisance_models <- function(args) {
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

    qval <- pval <- list()
    for (i in contrast) {
      ii <- which(fargs[, 2] == i)
      qval <- c(
        qval,
        list(unlist(lapply(ii, function(x) val[[x]]$qmod))[idx])
      )
      pval <- c(
        pval,
        list(unlist(lapply(ii, function(x) val[[x]]$pmod))[idx])
      )
    }
    names(qval) <- contrast
    names(pval) <- contrast
    return(list(qval = qval, pval = pval))
  }

  if (rep > 1) {
    pb <- progressr::progressor(steps = rep, message="repetition")
    f <- function(...) {
      res <- estimate_nuisance_models()
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
    val <- list(estimate_nuisance_models())
  }
  val <- list(nuisance = val)
  a <- c()
  pmod <- propensity.model$clone(deep = TRUE)
  for (i in seq_along(contrast)) {
    newf <-reformulate(
      as.character(pmod$formula)[[3]],
      outcome_level(treatment_var, contrast[i])
    )
    pmod$update(newf)
    a <- cbind(a, pmod$response(data))
  }
  colnames(a) <- contrast
  val$a <- a
  rm(a)
  val$y <- cbind(response.model$response(data, na.action=lava::na.pass0))
  colnames(val$y) <- lava::getoutcome(response.model$formula, data = data)

  if (rep.type[1] == "nuisance") { # average nuisance model pred. over rep.
    pval <- val$nuisance[[1]]$pval # list with treatment probabilities P(A=a|W)
    qval <- val$nuisance[[1]]$qval # list with outcome models E(Y|A=a,W)
    if (rep > 1) {
      for (i in 2:rep) {
        for (j in seq_along(contrast)) {
          qval[[j]] <- qval[[j]] + val$nuisance[[i]]$qval[[j]]
          pval[[j]] <- pval[[j]] + val$nuisance[[i]]$pval[[j]]
        }
      }
      for (j in seq_along(contrast)) {
        qval[[j]] <- qval[[j]] / rep
        pval[[j]] <- pval[[j]] / rep
      }
    }
    val$p <- list(Reduce(cbind, pval))
    val$q <- list(Reduce(cbind, qval))
    rm(qval, pval)
  } else {
    # rep.type[1] == "average" && rep > 1
    val$p <- lapply(val$nuisance, \(x) Reduce(cbind, x$pval))
    val$q <- lapply(val$nuisance, \(x) Reduce(cbind, x$qval))
  }
  val$nuisance <- NULL

  res <- list(
    call = cl,
    propensity.model = propensity.model,
    # (outcome, trt, propensity-pred, outcome-pred)
    data = val # (y, a, p, q)
  )
  class(res) <- c("cate.targeted", "targeted")

  res <- update(res,
                cate.model = cate.model,
                data = data,
                calibration.model = calibration.model,
                var.type = var.type,
                second.order = second.order
  )
  return(res)
}

cate_est <- function(y, # response vector
                     a, # matrix with treatment indicators a=1, a=0
                     p, # matrix with treatment probabilities a=1, a=0
                     q, # matrix with outcome predictions E(Y|A=1,X), E(Y|A=0,X)
                     data, # data.frame
                     propensity.model = NULL, # propensity model
                     X.cate
                     ) {

  K <- a / p * (y %x% rbind(rep(1, NCOL(a))) - q)
  scores <- K + q

   ## Expected potential outcomes
  est0 <- apply(scores, 2, mean)
  IF0 <- c()
  contrast <- colnames(a)
  if (!is.null(propensity.model)) {
    treatment_var <- lava::getoutcome(propensity.model$formula)
  }
  for (i in seq_along(est0)) {
    newIF <- scores[, i] - est0[i]
    if (!is.null(propensity.model) &&
        inherits(propensity.model, "learner_glm")) {
      pmod <- propensity.model$clone(deep = TRUE)
      newf <- reformulate(
        as.character(pmod$formula)[[3]],
        outcome_level(treatment_var, contrast[i])
      )
      pmod$update(newf)
      fit <- pmod$estimate(data)
      dlinkinv <- fit$family$mu.eta
      adj <- - K[, i] / p[, i] * dlinkinv(fit$family$linkfun(p[, i]))
      X.prop <- pmod$design(data, intercept = TRUE)$x
      for (i in seq_len(ncol(X.prop))) {
        X.prop[, i] <- X.prop[, i] * adj
      }
      adj <- X.prop
      icprop <- IC(pmod$estimate(data))
      newIF <- newIF + icprop %*% colMeans(adj)
    }
    IF0 <- cbind(IF0,  newIF)
  }
  nam <- paste0("E[", colnames(y), "(", colnames(a), ")]")
  names(est0) <- nam

  if (length(contrast) > 1) {
    pairs <- utils::combn(seq_along(contrast), 2) ## all pairs
  } else {
    pairs <- cbind(1)
  }
  res <- c()
  for (i in seq_len(ncol(pairs))) {
    cc <- pairs[, i]

    Yhat <- scores[, cc[1]]
    if (NCOL(scores) > 1) {
      Yhat <- Yhat - scores[, cc[2]]
    }

    est <- coef(lm(Yhat ~ -1 + X.cate))
    names(est) <- colnames(X.cate)
    V <- X.cate
    h0 <- V %*% est
    h1 <- V
    r <- (Yhat - h0)
    IF <- apply(h1, 2, function(x) x * r)
    n <- nrow(data)
    B <- solve(crossprod(V)) * n
    IF <- IF %*% B
    rownames(IF) <- rownames(X.cate)

    if (NCOL(X.cate) == 1 && all(X.cate == 1)) {
      # construct IF directly from potential outcomes rather than the
      # least squares projection above. Thereby we can use the
      # second order remainder term correction derived above.
      est <- mean(Yhat)
      names(est) <- colnames(X.cate)
      IF <- IF0[, cc[1]]
      if (NCOL(scores) > 1) {
        IF <- IF - IF0[, cc[2]]
      }
    }
    res <- c(res, list(list(est = est, IF = IF)))
  }
  if (length(res) > 1) { # more than one contrast
    for (i in seq_len(ncol(pairs))) {
      names(res[[i]]$est) <- paste0(
        names(res[[i]]$est),
        "[",
        contrast[pairs[1, i]], "-",
        contrast[pairs[2, i]],
        "]"
      )
    }
  }
  est0 <- c(est0, unlist(lapply(res, \(x) x$est)))
  IF0 <- cbind(IF0, Reduce(cbind, lapply(res, \(x) x$IF)))

  return(list(coef = est0, IC = IF0, scores = scores))
}

#' @export
update.cate.targeted <- function(object,
                                 cate.model = ~1,
                                 data,
                                 calibration.model = NULL,
                                 var.type = "IC",
                                 second.order = TRUE, ...) {

  desA <- design(cate.model, data, intercept = TRUE, rm_envir = FALSE)
  if (length(object$data$y) != nrow(desA$x)) {
    stop("Not same data as the `cate` object")
  }
  vcov <- NULL
  if (!is.null(calibration.model)) {
    des_cal <- design(calibration.model, data, intercept = TRUE)
    object$data$q0 <- object$data$q
    q <- object$data$q[[1]]
    a <- object$data$a
    y <- object$data$y
    Z <- cbind(des_cal$x, q)
    rs <- c() # residuals
    ps <- c() # treatment assignment prob.
    bs <- c() # linear regr. coef.
    for (i in seq_len(ncol(a))) {
      idx <- which(a[, i])
      b <- lm.fit(Z[idx, , drop = FALSE], y[idx])$coefficients
      b[is.na(b)] <- 0
      bs <- cbind(bs, cbind(b))
      q[, i] <- Z %*% b
      rs <- c(rs, list(y[idx] - Z[idx, , drop = FALSE] %*% b))
      ps <- c(ps, mean(a[, i]))
    }
    var <- function(x) stats::var(x) * (NROW(x) - 1) / NROW(x)
    v1 <- diag(unlist(lapply(rs, var)) / ps)
    v2 <- t(bs) %*% var(Z) %*% bs
    vcov <- (v1 + v2)/n
    object$data$q[[1]] <- q
  }


  pmod <- object$propensity.model # nolint
  if (!second.order) pmod <- NULL
  ests <- lapply(
    seq_along(object$data$p),
    \(x) {
      with(
        object$data,
        cate_est(
          y = y,
          a = cbind(a),
          p = cbind(p[[x]]),
          q = cbind(q[[x]]),
          propensity.model = pmod,
          data = data,
          X.cat = desA$x
        )
      )
    }
  )
  est <- ests[[1]]$coef
  IC <- ests[[1]]$IC
  scores <- ests[[1]]$scores
  if (tolower(var.type) == "ic" || is.null(vcov) || ncol(desA$x)>1) {
    estimate <- lava::estimate(coef = est, IC = IC)
  } else {
    e <- lava::estimate(coef = est[seq_len(ncol(vcov))], vcov = vcov)
    pairs <- utils::combn(seq_along(coef(e)), 2)
    B <- matrix(0, ncol(pairs), length(coef(e)))
    for (i in seq_len(ncol(pairs))) {
      B[i, pairs[, i]] <- c(1, -1)
    }
    estimate <- estimate(e, rbind(diag(nrow = length(coef(e))), B)) |>
      labels(names(est))
  }
  n <- ncol(object$data$p[[1]])
  nc <- length(est) - n
  estimate$model.index <- list(
    seq(n),
    seq(nc) + n
  )
  object$vcov.cal <- vcov
  object$scores <- scores
  object$estimate <- estimate
  object$cate.model <- cate.model
  object$levels <- colnames(object$data$a)
  return(object)
}

#' @export
summary.cate.targeted <- function(object, ...) {
  B <- rbind(rep(0, length(coef(object))))
  B[1:2] <- c(1, -1)
  obj <- structure(list(
    estimate = object$estimate,
    call = object$call,
    ate = lava::estimate(object$estimate, B)
  ), class = "summary.cate.targeted")
  return(obj)
}

#' @export
print.summary.cate.targeted <- function(x, ...) {
  print(x$call)
  cat("\n")
  print(x$estimate, ...)
  cat("\nAverage Treatment Effect:\n")
  print(x$ate)


}

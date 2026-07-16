# utility function to ensure that Dmat in quadprog::solve.QP is positive
# definite by projecting the original matrix to the nearest positive definite
# matrix
make_dmat_pos_definite <- function(pred) {
  Dmat <- t(pred) %*% pred
  if (all(dim(Dmat) == c(1, 1))) return(Dmat)
  .eigen <- eigen(Dmat)
  tau <- .eigen$values
  tau[tau < sqrt(.Machine$double.eps)] <- sqrt(.Machine$double.eps)
  Dmat <- .eigen$vectors %*% diag(tau) %*% t(.eigen$vectors)
  return(Dmat)
}


#' @title Non-negative least squares meta learner
#' @description Estimates the ensemble weights of a [superlearner] by minimizing
#'   the cross-validated MSE via non-negative least squares regression. The
#'   estimated weights are non-negative and normalized to sum to one.
#' @param method (character) Quadratic-programming solver used to compute the
#'   non-negative least squares weights. Either `"quadprog"` (default, using
#'   [quadprog::solve.QP]) or `"nnls"` (using [nnls::nnls]).
#' @param y (numeric) Response vector.
#' @param pred (matrix) Matrix of cross-validated predictions with one column
#'   per candidate learner.
#' @param ... Additional arguments (currently ignored).
#' @return (numeric) Vector of ensemble weights, one element per column of
#'   `pred`.
#' @details `targeted:::metalearner_nnls2` is an internal wrapper for using the
#'  `"nnls"` package instead of `"quadprog"`.
#' @seealso [superlearner] [learner_sl]
#' @name metalearner_nnls
#' @export
metalearner_nnls <- function(y, pred, method = "quadprog", ...) {
  if (NCOL(pred) == 1) {
    return(1.0)
  }
  idx <- which(apply(pred, 2, \(x) !any(is.na(x))))
  coefs <- rep(0, ncol(pred))
  pred <- pred[, idx, drop = FALSE]
  if (!(method %in% c("quadprog", "nnls"))) rlang::abort(
    "Provided method is not supported. Choose either quadprog or nnls"
  )
  if (method == "nnls") {
    res <- nnls::nnls(A = pred, b = y)
    coefs[idx] <- res$x
  } else {
    opt <- tryCatch(
      quadprog::solve.QP(
        Dmat = make_dmat_pos_definite(pred),
        Amat = diag(nrow = ncol(pred)),
        dvec = t(pred) %*% y
      ),
      error = function(...) list(solution = rep(NA, NCOL(pred)))
    )
    coefs[idx] <- opt$solution
  }
  if (any(is.na(coefs))) coefs[is.na(coefs)] <- 0
  if (all(coefs == 0)) coefs[1] <- 1
  return(coefs / sum(coefs))
}

metalearner_nnls2 <- function(y, pred, ...) {
  metalearner_nnls(y, pred, method = "nnls", ...)
}

#' @title Convex combination meta learner
#' @description Estimates the ensemble weights of a [superlearner] by minimizing
#'   the cross-validated MSE as a convex combination of the candidate
#'  predictions, i.e. by least squares regression of the response on the
#'   candidate predictions subject to the constraint that the weights are
#'   non-negative and sum to one.
#' @inherit metalearner_nnls return seealso
#' @inheritParams metalearner_nnls
#' @export
metalearner_convexcomb <- function(y, pred, ...) {
  if (NCOL(pred)==1) return(1.0)
  idx  <- which(apply(pred, 2, \(x) !any(is.na(x))))
  coefs <- rep(0, ncol(pred))
  pred <- pred[, idx, drop = FALSE]
  A <- diag(nrow = ncol(pred))
  A <- cbind(1, A)
  b <- c(1, rep(0, ncol(pred)))
  opt <- quadprog::solve.QP(
    Dmat = make_dmat_pos_definite(pred),
    Amat = A,
    dvec = t(pred) %*% y,
    bvec = b,
    meq = 1
  )
  coefs[idx] <- opt$solution
  if (any(is.na(coefs))) coefs[is.na(coefs)] <- 0
  if (all(coefs == 0)) coefs[1] <- 1
  return(coefs)
}

## metalearner_glmnet <- function(y, pred, ...) {
##   if (NCOL(pred)==1) return(1.0)
##   idx  <- which(apply(pred, 2, \(x) !any(is.na(x))))
##   coefs <- rep(0, ncol(pred))
##   pred <- pred[, idx, drop = FALSE]
##   res <- glmnet::glmnet(
##      y = y, x = pred,
##      intercept = FALSE,
##      lambda = 0,
##      lower.limits = rep(0, ncol(pred))
##      )
##   coefs[idx] <- opt$solution
##   if (any(is.na(coefs))) coefs[is.na(coefs)] <- 0
##   if (all(coefs == 0)) coefs[1] <- 1
##   return(coefs / sum(coefs))
## }


#' @title Discrete meta learner
#' @description Implements the discrete super learner: the candidate learner
#'   with the lowest risk (computed via the `model.score` argument of
#'   [superlearner]) is given weight one and all other learners weight zero.
#' @param model.score (function) Method for scoring the predictions of each base
#' learner.
#' @inherit metalearner_nnls return seealso
#' @inheritParams metalearner_nnls
#' @export
metalearner_discrete <- function(y, pred, model.score, ...) {
  risk <- apply(pred, 2, \(x) model.score(y, x))
  weights <- rep(0, NCOL(pred))
  risk[is.na(weights)] <- Inf
  weights[which.min(risk)[1]] <- 1
  return(weights)
}

get_learner_names <- function(model.list, name.prefix) {
  .names <- names(model.list)
  if (is.null(.names)) .names <- rep("", length(model.list))

  if (is.null(name.prefix)) {
    # NULL check because learner$new has info = NULL by default
    new_names <- lapply(
      model.list,
      \(lr) ifelse(is.null(lr$info), "", lr$info)
    ) |> unlist()
  } else {
    new_names <- paste0(name.prefix, seq_along(model.list))
  }
  .names[.names == ""] <- new_names[.names == ""]
  return(.names)
}

#' @export
#' @title Superlearner (stacked/ensemble learner)
#' @description This function creates a predictor object (class [learner]) from
#'   a list of existing [learner] objects. When estimating this model a stacked
#'   prediction will be created by weighting together the predictions of each of
#'   the initial learners The weights are learned using cross-validation.
#' @param data (data.frame) Data containing the response variable and
#'   covariates.
#' @param learners (list) List of [learner] objects (i.e. [learner_glm])
#' @param nfolds (integer) Number of folds to use in cross-validation to
#'   estimate the ensemble weights.
#' @param meta.learner (function) Algorithm to learn the ensemble weights
#'   (default non-negative least squares). Must be a function of the response
#'   (nx1 vector), `y`, and the base learner predictions (nxp matrix), `pred`,
#'   with p being the number of learners. The function can optionally accept a
#'   `model.score` argument for scoring the base learners. See
#'   [metalearner_nnls], [metalearner_convexcomb] and [metalearner_discrete] for
#'   the available meta learners.
#' @param model.score (function) Method for scoring the predictions of each base
#' learner. Expects two arguments; vector of response variable and prediction
#' from a base learner (see `targeted:::mse` for additional details).
#' @param name.prefix (character) Prefix used to name learner objects in
#'   `learners` without names. If NULL, then obtain the name from the info field
#'   of a learner.
#' @param mc.cores (integer) If not NULL, then [parallel::mcmapply] is used with
#'   `mc.cores` number of cores for parallelization instead of the
#'   [future.apply::future_lapply] package. Parallelization is disabled with
#'   `mc.cores = 1`.
#' @param silent (logical) Suppress all messages and progressbars
#' @param future.seed (logical or integer) Argument passed on to
#'   [future.apply::future_lapply]. If TRUE, then [.Random.seed] is used if it
#'   holds a L'Ecuyer-CMRG RNG seed, otherwise one is created randomly.
#' @param ... Additional arguments to [parallel::mclapply] or
#'   [future.apply::future_lapply].
#' @references Luedtke & van der Laan (2016) Super-Learning of an Optimal
#'   Dynamic Treatment Rule, The International Journal of Biostatistics.
#' @aliases superlearner
#' @seealso [predict.superlearner] [weights.superlearner] [score.superlearner]
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' m <- list(
#'   "mean" = learner_glm(y ~ 1),
#'   "glm" = learner_glm(y ~ x1 + x2)
#' )
#' sl <- superlearner(m, data = sim1(), nfolds = 2)
#' predict(sl, newdata = sim1(n = 5))
#' predict(sl, newdata = sim1(n = 5), all.learners = TRUE)
superlearner <- function(learners,
                         data,
                         nfolds = 10,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         mc.cores = NULL,
                         future.seed = TRUE,
                         silent = TRUE,
                         name.prefix = NULL,
                         ...) {
  pred_mod <- function(models, data) {
    n <- nrow(data)
    res <- matrix(NA, nrow=n, ncol=length(models))
    for (i in seq_along(models)) {
      if (!is.null(models[[i]]$fit)) {
        res[, i] <- tryCatch(
          models[[i]]$predict(data), error=function(x) rep(NA, n)
        )
      }
    }
    return(res)
  }
  est_mod <- function(models, data) {
    for (i in seq_along(models)) {
      v <- tryCatch(
        models[[i]]$estimate(data),
        error=function(x) NULL
      )
      # not strictly needed because model$fit == NULL upon learner instantiation
      if (is.null(v)) models[[i]]$clear
    }
    return(models)
  }

  if (is.character(model.score)) {
    model.score <- get(model.score)
  }

  if (any(!unlist(lapply(learners, \(lr) inherits(lr, "learner"))))) stop(
    "All provided learners must be of class targeted::learner."
  )

  responses <- unlist(lapply(learners, \(m) deparse(m$formula[[2]])))
  if (length(unique(responses)) > 1) {
    r <- paste0(unique(responses), collapse = ", ")
    warning("Different response variables found among learners: ", r)
  }

  if (!is.function(meta.learner)) rlang::abort(
    "meta.learner needs to be a function."
  )

  model.names <- get_learner_names(learners, name.prefix)
  n <- nrow(data)
  folds <- lava::csplit(n, nfolds)
  pred <- matrix(NA, n, length(learners))
  if (!silent) pb <- progressr::progressor(along = seq_len(nfolds))
  onefold <- function(fold, data, learners, pb) {
    n <- nrow(data)
    test <- data[fold, , drop = FALSE]
    train <- data[setdiff(1:n, fold), , drop = FALSE]
    mod <- lapply(learners, \(x) x$clone(deep = TRUE))

    est_mod(mod, train)
    pred.test <- pred_mod(mod, test)
    if (!silent) pb()
    return(list(pred = pred.test, fold = fold))
  }
  if (!is.null(mc.cores)) {
    if (mc.cores == 1L) {
      ## disable parallelization
      pred.folds <- lapply(folds, function(fold) {
        return(onefold(fold, data, learners, pb))
      })
    } else {
      ## mclapply
      pred.folds <- parallel::mclapply(
        folds,
        function(fold) onefold(fold, data, learners, pb),
        mc.cores = mc.cores, ...
        )
    }
  } else {
    ## future
    pred.folds <- do.call(
      future.apply::future_lapply,
      list(
        X = folds,
        FUN = function(fold) onefold(fold, data, learners, pb),
        future.seed = future.seed,
        ...
      )
    )
  }
  for (i in seq_along(pred.folds)) {
    pred[pred.folds[[i]]$fold, ] <- pred.folds[[i]]$pred
  }
  mod <- lapply(learners, \(x) x$clone())
  names(mod) <- model.names

  ## Full predictions
  est_mod(mod, data)
  if (all(sapply(mod, \(x) is.null(x$fit)))) stop(
    "All learners failed to be estimated."
  )

  # Meta-learner
  y <- learners[[1]]$response(data)
  risk <- apply(pred, 2, \(x) model.score(y, x))
  # Learners with failed predictions
  idx <- which(apply(pred, 2, \(x) any(is.na(x) | is.nan(x))))

  if (length(idx) == length(mod)) stop(
    "Terminating the estimation of the superlearner because the hold-out set ",
    "predictions of all learners contain NAs. Therefore, the ensemble ",
    "weights cannot be estimated."
  )

  if (length(risk) > 0) risk[idx] <- Inf

  names(risk) <- model.names

  w <- meta.learner(y = y, pred = pred, model.score = model.score)
  names(w) <- model.names


  res <- list(
    model.score = risk,
    weights = w,
    names = model.names,
    fit = mod,
    folds = folds
  )
  return(structure(res, class = "superlearner"))
}

#' @export
print.superlearner <- function(x, ...) {
  res <- cbind("score" = x$model.score, "weight" = x$weights)
  if (!is.null(x$fit)) {
      rownames(res) <- x$names
  } else {
    rownames(res) <- paste("model", seq_along(x$fit))
  }
  return(print(res))
}

#' @title Extract ensemble weights
#' @param object (superlearner) Fitted model.
#' @param ... Not used.
#' @export
weights.superlearner <- function(object, ...) {
  return(object$weights)
}

#' @title Extract average cross-validated score of individual learners
#' @param x (superlearner) Fitted model.
#' @param ... Not used.
#' @export
score.superlearner <- function(x, ...) {
  return(x$model.score)
}

#' @title Predict Method for superlearner Fits
#' @description Obtains predictions for ensemble model or individual learners.
#' @export
#' @param object (superlearner) Fitted [superlearner] object.
#' @param newdata (data.frame) Data in which to look for variables with which to
#' predict.
#' @param all.learners (logical) If FALSE (default), then return the predictions
#' from the ensemble model. Otherwise, return predictions of from all individual
#' learners.
#' @param ... Not used.
#' @return numeric (`all.learners = FALSE`) or matrix (`all.learners = TRUE`)
predict.superlearner <- function(object, newdata, all.learners = FALSE, ...) {
  # learners that fail to be estimated on the full data have x$fit == NULL
  pr <- lapply(
    object$fit,
    \(x) if(is.null(x$fit)) rep(0, NROW(newdata)) else x$predict(newdata)
  )
  if (length(object$weights) == 1) return(unname(pr[[1]]))
  res <- Reduce(cbind, pr)
  colnames(res) <- names(object$fit)

  # learners which produced predictions with some NAs during any fold will have
  # their ensemble weight set to 0
  if (!all.learners) {
    res <- as.vector(res %*% object$weights)
  }
  return(res)
}

#' @title SuperLearner wrapper for learner (defunct)
#' @description `SL()` has been removed. Use [learner_sl] instead.
#' @param ... Ignored.
#' @author Klaus Kähler Holst
#' @export
SL <- function(...) {
  .Defunct("learner_sl", package = "targeted",
    msg = paste(
      "'SL' is defunct. Use learner_sl instead."
    )
  )
}

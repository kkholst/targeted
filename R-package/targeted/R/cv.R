##' Generic cross-validation function
##'
##' @title Cross-validation
##' @param models List of fitting functions
<<<<<<< HEAD
##' @param data data.frame
##' @param response Response variable (vector or name of column in `data`)
##' @param K Number of folds (default 5, 0 splits in 1:n/2, n/2:n with last part
##'   used for testing)
##' @param rep Number of repetitions (default 1)
##' @param weights Optional frequency weights
##' @param modelscore Model scoring metric (default: RMSE / Brier score). Must
##'   be a function with arguments: response, prediction, weights, ...
=======
##' @param data data.frame or matrix
##' @param response Response variable (vector or name of column in `data`).##'
##' @param K Number of folds (default 5. K=0 splits in 1:n/2, n/2:n with last part
##'   used for testing)
##' @param rep Number of repetitions (default 1)
##' @param weights Optional frequency weights
##' @param modelscore Model scoring metric (default: RMSE / Brier score).
##'   Must be a function with arguments: response, prediction, weights, ...
>>>>>>> develop
##' @param seed Random seed (argument parsed to future_Apply::future_lapply)
##' @param shared Function applied to each fold with results send to each model
##' @param args.pred Optional arguments to prediction function (see details
##'   below)
<<<<<<< HEAD
##' @param ... Additional arguments parsed to models in `models`
##' @author Klaus K. Holst
##' @details ...
##' @return An object of class '\code{cross_validated}' is returned. See
##'   \code{\link{cross_validated-class}} for more details about this class and
##'   its generic functions.
=======
##' @param ... Additional arguments parsed to models in models
##' @author Klaus K. Holst
##' @return An object of class '\code{cross_validated}' is returned. See
##'   \code{\link{cross_validated-class}} for more details about this class and
##'   its generic functions.
##' @details models should be list of objects of class ml_model.
##' Alternatively, each element of models should be a list with a fitting function
##' and a prediction function.
##'
##' The `reponse` argument can optionally be a named list where the name is then
##' used as the name of the response argument in models. Similarly, if data
##' is a named list with a single data.frame/matrix then this name will be used
##' as the name of the data/design matrix argument in models.
>>>>>>> develop
##' @examples
##' f0 <- function(data,...) lm(...,data=data)
##' f1 <- function(data,...) lm(Sepal.Length~Species,data=data)
##' f2 <- function(data,...) lm(Sepal.Length~Species+Petal.Length,data=data)
##' x <- cv(list(m0=f0,m1=f1,m2=f2),rep=10, data=iris, formula=Sepal.Length~.)
##' x
##' @export
cv <- function(models, data, response = NULL, K = 5, rep = 1,
<<<<<<< HEAD
               weights = NULL, modelscore, seed=TRUE,
               shared = NULL, args.pred = NULL, ...) {
  if (is.vector(data)) data <- cbind(data)
=======
               weights = NULL, modelscore,
               seed = NULL, shared = NULL, args.pred = NULL, ...) {
>>>>>>> develop
  if (missing(modelscore)) modelscore <- scoring
  if (!is.list(models)) stop("Expected a list of models")
  nam <- names(models)
  if (is.null(nam)) nam <- paste0("model", seq_along(models))
  args0 <- list(...)
  args <- args0
  data.arg <- NULL
  response.arg <- "response"
  if (is.list(response)) {
    if (!is.null(names(response))) response.arg <- names(response)[1]
    response <- response[[1]]
  }
  if (inherits(data, "list")) {
    data.arg <- names(data)[1]
    data <- data[[1]]
  }
  if (is.vector(data)) data <- cbind(data)

  if (is.character(response) && length(response) == 1) {
    response <- data[, response, drop = TRUE]
  }

  for (i in seq_along(models)) {
    f <- models[[i]]
<<<<<<< HEAD
    if (!is.list(f) || length(f) == 1) {
=======
    if ((!is.list(f) || length(f) == 1)
        && !inherits(f, "ml_model")) {
>>>>>>> develop
      ## No predict function provided. Assume 'predict' works on fitted object
      if (is.list(f)) f <- f[[1]]
      models[[i]] <- list(
        fit = f,
        predict = function(fit, newdata, ...) {
          predict(fit, newdata = newdata, ...)
        }
      )
    }
  }

  ## Model (1) run on full data:
  if (!is.null(shared)) {
    sharedres <- shared(data, ...)
    args <- c(args, sharedres)
  }
  arglist <- c(list(data), args)
  if (!is.null(weights)) arglist <- c(arglist, list(weights = weights))

<<<<<<< HEAD
  f <- models[[1]][[1]]
  if (!is.null(response) && "response" %in% formalArgs(f)) {
    arglist <- c(arglist, list(response = response))
=======
  arg_response <- rep(FALSE, length(models))
  for (i in seq_along(models)) {
    if (inherits(models[[i]], "ml_model")) {
      if (!is.null(response) && response.arg %in% names(models[[i]]$formals$estimate)) {
        arg_response[i] <- TRUE
      }
    } else {
      if (!is.null(response) && response.arg %in% formalArgs(models[[i]][[1]])) {
        arg_response[i] <- TRUE
      }
    }
  }
  f <- models[[1]]
  if (arg_response[1]) {
    arglist[response.arg] <- list(response)
  }

  if (inherits(f, "ml_model")) {
    fit0 <- do.call(f$estimate, arglist)
    if (is.null(response)) {
      response <- f$response(data)
    } else if (length(response)==1)
      response <- data[, response, drop=TRUE]
  } else {
    fit0 <- do.call(f[[1]], arglist)
>>>>>>> develop
  }
  if (is.null(response)) {
    if (inherits(f, "ml_model")) {
      response <- f$response(data)
    } else {
      response <- tryCatch(data[, lava::endogenous(fit0), drop = TRUE],
                           error = function(...) NULL)
    }
    if (is.null(response)) stop("Provide 'response'")
  }
  ## In-sample predictive performance:
<<<<<<< HEAD
  pred0 <- do.call(
    models[[1]][[2]],
       c(list(fit0, data = data), args.pred))
=======
  if (inherits(f, "ml_model")) {
    pred0 <- do.call(f$predict,
                     c(list(newdata = data), args.pred))
  } else {
    pred0 <- do.call(
      f[[2]],
      c(list(fit0, newdata = data), args.pred))
  }
>>>>>>> develop
  perf0 <- modelscore(prediction=pred0, response=response, weights=weights)
  nam_perf <- if (is.vector(perf0))
               names(perf0) else colnames(perf0)
  n <- NROW(data)
  M <- length(models) # Number of models
  P <- length(perf0) # Number of performance measures
  rm(fit0, pred0, perf0)

  if (K == 0) {
    rep <- 1
    K <- 1
    folds <- list(lava::csplit(seq(n)))
  } else {
    folds <- lava::foldr(n, K, rep)
  }
  arg <- expand.grid(R = seq(rep), K = seq(K)) # ,M=seq_along(models))
  dim <- c(rep, K, M, P)
  perf_arr <- array(0, dim)
  dimnames(perf_arr) <- list(NULL, NULL, nam, nam_perf)
<<<<<<< HEAD

=======
>>>>>>> develop
  pb <- progressr::progressor(along = seq(nrow(arg)))
  ff <- function(i) {
    R <- arg[i, 1]
    k <- arg[i, 2]
    fold <- folds[[R]]
    dtest <- data[fold[[k]], , drop = FALSE]
    ytest <- response[fold[[k]]]
    wtest <- weights[fold[[k]]]
    dtrain <- data[unlist(fold[-k]), , drop = FALSE]
    ytrain <- response[unlist(fold[-k])]
    wtrain <- weights[unlist(fold[-k])]
    args <- args0
    if (!is.null(shared)) {
      sharedres <- shared(dtrain, ...)
      args <- c(args, sharedres)
    }
    arglist <- c(list(dtrain), args)
    if (!is.null(data.arg))
      names(arglist)[1] <- data.arg
    if (!is.null(weights)) arglist <- c(arglist, list(weights = wtrain))
    fits <- list()
<<<<<<< HEAD
    for (i in seq_along(models)) {
      f <- models[[i]][[1]]
      if ("response" %in% formalArgs(f)) {
        arglist <- c(arglist, list(response = ytrain))
=======
    for (j in seq_along(models)) {
      if (arg_response[j]) {
        arglist[response.arg] <- list(ytrain)
      } else {
        arglist[response.arg] <- NULL
      }
      f <- models[[j]]
      if (inherits(f, "ml_model")) {
        fits <- c(fits, list(do.call(f$estimate, arglist)))
      } else {
        fits <- c(fits, list(do.call(f[[1]], arglist)))
>>>>>>> develop
      }
    }
    perfs <- list()
<<<<<<< HEAD
    for (i in seq_along(fits)) {
      pred <- do.call(
        models[[i]][[2]],
        c(list(fits[[i]], data = dtest), args.pred)
      )
=======
    for (j in seq_along(fits)) {
      if (inherits(models[[j]], "ml_model")) {
        pred <- do.call(
          models[[j]]$predict,
          c(list(newdata = dtest), args.pred))
      } else {
        pred <- do.call(
          models[[j]][[2]],
          c(list(fits[[j]], newdata = dtest), args.pred))
      }
>>>>>>> develop
      perfs <- c(perfs, list(
        do.call(
          modelscore,
          c(list(
            prediction = pred,
            response = ytest,
            weights = wtest
          ))
        )
      ))
    }
    pb()
    do.call(rbind, perfs)
  }
  val <- future.apply::future_mapply(ff, seq(nrow(arg)),
<<<<<<< HEAD
                                     SIMPLIFY = FALSE, USE.NAMES = TRUE,
=======
                                     SIMPLIFY = FALSE,
                                     USE.NAMES = TRUE,
>>>>>>> develop
                                     future.seed = seed)
  for (i in seq(nrow(arg))) {
    R <- arg[i, 1]
    k <- arg[i, 2]
    perf_arr[R, k, , ] <- val[[i]]
  }

  structure(list(
    cv = perf_arr,
    call = match.call(),
    names = nam,
    rep = rep, folds = K
  ),
  class = "cross_validated"
  )
}

##' @export
summary.cross_validated <- function(object, ...) {
  return(coef(object))
}

##' @export
print.cross_validated <- function(x, ...) {
  cat("Call: ")
  print(x$call)
  cat("\n", x$fold, "-fold cross-validation", sep="")
  cat(" with ", x$rep, " repetitions\n\n", sep="")
  res <- coef(x)
  print(res, quote=FALSE)
}

##' @export
coef.cross_validated <- function(object, min=FALSE, ...) {
  res <- apply(object$cv, 3:4, function(x) mean(x))
  if (length(object$names)==nrow(res))
    rownames(res) <- object$names
  if (min) {
    res <- apply(res, 2, which.min)
  }
  res
}

#' @title cross_validated class object
#'
#' @description The functions \code{\link{cv}} returns an object of the type
#'   \code{cross_validated}.
#'
#' An object of class '\code{cross_validated}' is a list with at least the following components:
#'
#' \describe{
#'   \item{cv}{An array with the model score(s) evaluated for each fold, repetition, and model.
#'   estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{names}{Names (character vector) of the models}
#'   \item{rep}{number of repetitions of the CV}
#'   \item{folds}{Number of folds of the CV}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class \code{cross_validated}:
#' \itemize{
#'   \item{\code{coef}}{Extract average model scores from the cross-validation procedure.}
#'   \item{\code{print}}{Basic print method.}
#'   \item{\code{summary}}{Summary of the cross-validation procedure.}'
#'  }
#'
#' @aliases cross_validated-class cross_validated
#' @seealso \code{\link{cv}}
#' @return objects of the S3 class '\code{cross_validated}'
#' @examples ## See example(cv) for examples
#' @docType class
#' @name cross_validated-class
NULL

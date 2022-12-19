##' Generic cross-validation function
##'
##' @title Cross-validation
##' @param modelList List of fitting functions
##' @param data data.frame
##' @param response Response variable (vector or name of column in `data`)
##' @param K Number of folds (default 5, 0 splits in 1:n/2, n/2:n with last part
##'   used for testing)
##' @param rep Number of repetitions (default 1)
##' @param weights Optional frequency weights
##' @param modelscore Model scoring metric (default: RMSE / Brier score).
##'   Must be a function with arguments: response, prediction, weights, ...
##' @param seed Optional random seed
##' @param shared Function applied to each fold with results send to each model
##' @param args.pred Optional arguments to prediction function (see details
##'   below)
##' @param ... Additional arguments parsed to models in modelList
##' @author Klaus K. Holst
##' @details ...
##' @examples
##' f0 <- function(data,...) lm(...,data=data)
##' f1 <- function(data,...) lm(Sepal.Length~Species,data=data)
##' f2 <- function(data,...) lm(Sepal.Length~Species+Petal.Length,data=data)
##' x <- cv(list(m0=f0,m1=f1,m2=f2),rep=10, data=iris, formula=Sepal.Length~.)
##' @export
cv <- function(modelList, data, response = NULL, K = 5, rep = 1,
               weights = NULL, modelscore,
               seed = NULL, shared = NULL, args.pred = NULL, ...) {
  if (is.vector(data)) data <- cbind(data)
  if (missing(modelscore)) modelscore <- scoring
  if (!is.list(modelList)) stop("Expected a list of models")
  nam <- names(modelList)
  if (is.null(nam)) nam <- paste0("model", seq_along(modelList))
  args0 <- list(...)
  args <- args0
  if (is.character(response) && length(response) == 1) {
    response <- data[, response, drop = TRUE]
  }

  for (i in seq_along(modelList)) {
    f <- modelList[[i]]
    if (!is.list(f) || length(f) == 1) {
      ## No predict function provided. Assume 'predict' works on fitted object
      if (is.list(f)) f <- f[[1]]
      modelList[[i]] <- list(
        fit = f,
        predict = function(fit, data, ...) {
          predict(fit, newdata = data, ...)
        }
      )
    }
  }

  ## Model (1) run on full data:
  if (!is.null(shared)) {
    sharedres <- shared(data, ...)
    args <- c(args, sharedres)
  }
  arglist <- c(list(data = data), args)
  if (!is.null(weights)) arglist <- c(arglist, list(weights = weights))

  f <- modelList[[1]][[1]]
  if (!is.null(response) && "response" %in% formalArgs(f)) {
    arglist <- c(arglist, list(response = response))
  }
  fit0 <-do.call(f, arglist)
  if (is.null(response)) {
    response <- tryCatch(data[, lava::endogenous(fit0), drop = TRUE],
      error = function(...) NULL
    )
    if (is.null(response)) stop("Provide 'response'")
  }
  ## In-sample predictive performance:
  pred0 <- do.call(
    modelList[[1]][[2]],
       c(list(fit0, data = data), args.pred))
  perf0 <- modelscore(prediction=pred0, response=response, weights=weights)
  namPerf <- if (is.vector(perf0))
               names(perf0) else colnames(perf0)
  n <- NROW(data)
  M <- length(modelList) # Number of models
  P <- length(perf0) # Number of performance measures
  rm(fit0, pred0, perf0)

  if (!is.null(seed)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      runif(1)
    }
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  if (K == 0) {
    rep <- 1
    K <- 1
    folds <- list(lava::csplit(seq(n)))
  } else {
    folds <- lava::foldr(n, K, rep)
  }
  arg <- expand.grid(R = seq(rep), K = seq(K)) # ,M=seq_along(modelList))
  dim <- c(rep, K, M, P)
  PerfArr <- array(0, dim)
  dimnames(PerfArr) <- list(NULL, NULL, nam, namPerf)

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
    arglist <- c(list(data = dtrain), args)
    if (!is.null(weights)) arglist <- c(arglist, list(weights = wtrain))
    fits <- list()
    for (i in seq_along(modelList)) {
      f <- modelList[[i]][[1]]
      if ("response" %in% formalArgs(f)) {
        arglist <- c(arglist, list(response = ytrain))
      }
      fits <- c(fits, list(do.call(f, arglist)))
    }
    perfs <- list()
    for (i in seq_along(fits)) {
      pred <- do.call(
        modelList[[i]][[2]],
        c(list(fits[[i]], data = dtest), args.pred)
      )
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
    SIMPLIFY = FALSE, future.seed = TRUE
  )
  for (i in seq(nrow(arg))) {
    R <- arg[i, 1]
    k <- arg[i, 2]
    PerfArr[R, k, , ] <- val[[i]]
  }

  structure(list(
    cv = PerfArr,
    call = match.call(),
    names = nam,
    rep = rep, folds = K
    ##fit = fit0
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

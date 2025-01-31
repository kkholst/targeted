metalearner_nnls <- function(y, pred, method = "nnls") {
  if (NCOL(pred)==1) return(1.0)
  if (method == "nnls") {
    res <- nnls::nnls(A = pred, b = y)
    coefs <- res$x
  } else {
    res <- glmnet::glmnet(
      y = y, x = pred,
      intercept = FALSE,
      lambda = 0,
      lower.limits = rep(0, ncol(pred))
    )
    coefs <- as.vector(coef(res))[-1]
  }
  if (any(is.na(coefs))) coefs[is.na(coefs)] <- 0
  if (all(coefs == 0)) coefs[1] <- 1
  return(coefs / sum(coefs))
}

superlearner <- function(model.list,
                         data,
                         nfolds = 10,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         mc.cores = NULL,
                         future.seed = TRUE,
                         ...) {
  pred_mod <- function(models, data) {
    res <- lapply(models, \(x) x$predict(data))
    Reduce(cbind, res)
  }
  if (is.character(model.score)) {
    model.score <- get(model.score)
  }
  model.names <- names(model.list)
  n <- nrow(data)
  folds <- lava::csplit(n, nfolds)
  pred <- matrix(NA, n, length(model.list))
  pb <- progressr::progressor(along = seq_len(nfolds))
  onefold <- function(fold, data, model.list, pb) {
    n <- nrow(data)
    test <- data[fold, , drop = FALSE]
    train <- data[setdiff(1:n, fold), , drop = FALSE]
    mod <- lapply(model.list, \(x) x$clone(deep = TRUE))
    lapply(mod, \(x) x$estimate(train))
    pred.test <- pred_mod(mod, test)
    pb()
    list(pred = pred.test, fold = fold)
  }
  if (!is.null(mc.cores)) {
    if (mc.cores == 1L) {
      ## disable parallelization
      pred.folds <- lapply(folds, function(fold) {
        onefold(fold, data, model.list, pb)
      })
    } else {
      ## mclapply
      pred.folds <- parallel::mclapply(
        folds,
        function(fold) {
          onefold(fold, data, model.list, pb)
        },
        mc.cores = mc.cores, ...
        )
    }
  } else {
    ## future
    pred.folds <- do.call(
      future.apply::future_lapply,
      list(
        X = folds,
        FUN = function(fold) {
          onefold(fold, data, model.list, pb)
        },
        future.seed = future.seed,
        ...
      )
    )
  }
  for (i in seq_along(pred.folds)) {
    pred[pred.folds[[i]]$fold, ] <- pred.folds[[i]]$pred
  }
  mod <- lapply(model.list, \(x) x$clone())
  ## Meta-learner
  y <- model.list[[1]]$response(data)
  risk <- apply(pred, 2, \(x) model.score(y, x))
  names(risk) <- model.names
  w <- meta.learner(y = y, pred = pred)
  names(w) <- model.names
  ## Full predictions
  lapply(mod, \(x) x$estimate(data))
  res <- list(
    model.score = risk,
    weights = w,
    names = model.names,
    fit = mod,
    folds = folds
  )
  structure(res, class = "superlearner")
}

##' @export
print.superlearner <- function(x, ...) {
  res <- cbind("score" = x$model.score, "weight" = x$weights)
  if (!is.null(x$fit)) {
      rownames(res) <- x$names
  } else {
    rownames(res) <- paste("model", seq_along(x$fit))
  }
  print(res)
}


##' SuperLearner wrapper for ml_model
##'
##' @title SuperLearner wrapper for ml_model
##' @aliases SL
##' @param formula Model design
##' @param ... Additional arguments for SuperLearner::SuperLearner
##' @param SL.library character vector of prediction algorithms
##' @param binomial boolean specifying binomial or gaussian family (default
##'   FALSE)
##' @param data Optional data.frame
##' @param info model information (optional)
##' @return ml_model object
##' @author Klaus Kähler Holst
##' @export
SL <- function(formula=~., ...,
               SL.library=c("SL.mean", "SL.glm"),
               binomial=FALSE,
               data=NULL,
               info = "SuperLearner") {
  dots <- list(...)
  if (!requireNamespace("SuperLearner")) {
      stop("Package 'SuperLearner' required.")
  }
  pred <- as.character(formula)
  pred <- ifelse (length(pred)==2, pred[2], pred[3])
  if (pred=="1") {
    SL.library <- "SL.mean"
  }
  m <- ml_model$new(formula,
    info = info,
    estimate = function(x, y) {
      Y <- as.numeric(y)
      X <- as.data.frame(x)
      args <- c(list(
        Y = Y, X = X,
        SL.library = SL.library
      ), dots)
      if (binomial) {
        args <- c(args, list(family = binomial()))
      }
      res <- do.call(SuperLearner::SuperLearner, args)
      res$call <- quote(SuperLearner(...))
      if (binomial) {
        res$call <- quote(
          SuperLearner::SuperLearner(
            ...,
            family = binomial()
          )
        )
      }
      res
    },
    predict = function(object, newdata) {
      pr <- predict(object, newdata = newdata)$pred
      if (binomial) {
        pr <- cbind((1 - pr), pr)
      }
      return(pr)
    }
    )
  if (!is.null(data))
    m$estimate(data)
  return(m)
}

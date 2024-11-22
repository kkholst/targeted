
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

metalearner <- function(y, pred, method = "nnls") {
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
                         meta.learner = NULL,
                         model.score = mse) {
  pred_mod <- function(models, data) {
    res <- lapply(models, \(x) x$predict(data))
    Reduce(cbind, res)
  }
  if (is.null(meta.learner)) {
    meta.learner <- metalearner
  }
  model.names <- names(model.list)
  n <- nrow(data)
  folds <- lava::csplit(n, nfolds)
  pred <- matrix(NA, n, length(model.list))
  for (i in 1:nfolds) {
    test <- data[folds[[i]], , drop = FALSE]
    train <- data[setdiff(1:n, folds[[i]]), , drop = FALSE]
    lapply(model.list, \(x) x$estimate(train))
    pri <- pred_mod(model.list, test)
    pred[folds[[i]], ] <- pri
  }
  y <- model.list[[1]]$response(data)
  risk <- apply(pred, 2, \(x) model.score(y, x))
  names(risk) <- model.names
  w <- metalearner(y = y, pred = pred)
  names(w) <- model.names
  ## Full predictions
  lapply(model.list, \(x) x$estimate(data))
  res <- list(
    model.score = risk,
    weights = w,
    names = model.names,
    fit = model.list,
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

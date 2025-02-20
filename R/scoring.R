mse <- function(response, prediction, weights=NULL, ...) {
  res <- (response - prediction)^2
  if (!is.null(weights)) {
    res <- stats::weighted.mean(res, w = weights, na.rm = TRUE)
  } else {
    res <- mean(res, na.rm = TRUE)
  }
  names(res) <- "mse"
  return(res)
}

mae <- function(response, prediction, weights=NULL, ...) {
  res <- abs(response - prediction)
  if (!is.null(weights)) {
    res <- stats::weighted.mean(res, w = weights, na.rm = TRUE)
  } else {
    res <- mean(res, na.rm = TRUE)
  }
  names(res) <- "mae"
  return(res)
}

quantitative_scoring1 <- function(response, prediction, weights=NULL, ...) {
  res <- c(
    mse(response, prediction, weights=weights, ...),
    mae(response, prediction, weights=weights, ...)
  )
  return(res)
}

multiclass_scoring1 <- # nolint
  function(response, prediction, levels = NULL, weights = NULL, messages = 1) {
    if (is.null(levels) && NCOL(prediction)>1) {
      levels <- colnames(prediction)
    }
    if (is.factor(response) && is.null(levels)) {
      levels <- levels(response)
    }
    if (is.null(levels)) {
      warning("missing definition of levels, using observed")
      levels <- sort(unique(response))
    }
    cl <- levels
    if (is.factor(response)) {
      cl.response <- levels(response)
    } else {
      cl.response <- sort(unique(response))
    }
    if ((length(cl.response)==2) && (NCOL(prediction)==1)) {
      prediction <- cbind(1 - prediction, prediction)
      colnames(prediction) <- cl.response
    }
    newcl <- which(!cl.response %in% cl) # response classes for which no
    ## predictions are available assigning pred 0 probabilities to unresponse
    ## classes
    if (length(newcl)) {
      if (messages > 0) warning("new response classes detected")
      temp <- array(0, dim = c(nrow(prediction), length(newcl)))
      colnames(temp) <- cl.response[newcl]
      prediction <- cbind(prediction, temp)
    }
    y <- outer(response, colnames(prediction), "==")
    ## Brier score
    Bi <- apply((prediction - y)^2, 1, sum)
    if (ncol(prediction) == 2) {
      Bi <- Bi / 2
    }
    ## logscore
    Li <- apply(
      log(prediction) * y, 1,
      function(x) sum(x[is.finite(x)], na.rm = TRUE)
    )
    ##
    if (!is.null(weights)) {
      B <- stats::weighted.mean(Bi, w = weights, na.rm = TRUE)
      L <- stats::weighted.mean(Li, w = weights, na.rm = TRUE)
    } else {
      B <- mean(Bi, na.rm = TRUE)
      L <- mean(Li, na.rm = TRUE)
    }
    return(list("brier" = B, "-logscore" = -L))
  }

#' Predictive model scoring
#'
#' @title Predictive model scoring
#' @param response Observed response
#' @param ... model predictions (continuous predictions or class probabilities
#'   (matrices))
#' @param type continuous or categorical response (the latter is automatically
#'   chosen if response is a factor, otherwise a continuous response is
#'   assumed)
#' @param metrics which metrics to report
#' @param weights optional frequency weights
#' @param names optional names of models coments (given as ..., alternatively
#'   these can be named arguments)
#' @param object optional model object
#' @param newdata optional new data.frame
#' @param levels (optional) unique levels in response variable
#' @param messages controls amount of messages/warnings (0: none)
#' @examples
#' data(iris)
#' set.seed(1)
#' dat <- csplit(iris,2)
#' g1 <- NB(Species ~ Sepal.Width + Petal.Length, data=dat[[1]])
#' g2 <- NB(Species ~ Sepal.Width, data=dat[[1]])
#' pr1 <- predict(g1, newdata=dat[[2]], wide=TRUE)
#' pr2 <- predict(g2, newdata=dat[[2]], wide=TRUE)
#' table(colnames(pr1)[apply(pr1,1,which.max)], dat[[2]]$Species)
#' table(colnames(pr2)[apply(pr2,1,which.max)], dat[[2]]$Species)
#' scoring(dat[[2]]$Species, pr1=pr1, pr2=pr2)
#' ## quantitative response:
#' scoring(response=1:10, prediction=rnorm(1:10))
#' @return Numeric matrix of dimension m x p, where m is the number of
#'   different models and p is the number of model metrics
#' @export
scoring <- function(response, ...,
                    type="quantitative",
                    levels=NULL, metrics=NULL,
                    weights=NULL, names=NULL, object=NULL, newdata=NULL,
                    messages=1) {
  if (is.factor(response) || is.character(response)) {
    type <- "multiclass"
  }
  if (tolower(type) %in% c("quantitative", "cont", "continuous")) {
    S <- suppressWarnings(lapply(list(...),
      quantitative_scoring1,
      response = response,
      weights = weights, messages = messages
    ))
  } else {
    S <- suppressWarnings(lapply(list(...),
      multiclass_scoring1,
      response = response,
      levels = levels, weights = weights, messages = messages
    ))
  }
  if (is.null(names)) {
    names <- base::names(list(...))
  }
  res <- matrix(unlist(S), byrow=TRUE, ncol=length(S[[1]]))
  rownames(res) <- names
  colnames(res) <- names(S[[1]])
  if (!is.null(metrics))
    res <- res[, metrics, drop=FALSE]
  return(res)
}

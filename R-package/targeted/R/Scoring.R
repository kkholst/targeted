rmse <- function(response, prediction, weights=NULL, ...) {
  res <- (response - prediction)^2
  if (!is.null(weights)) {
    res <- stats::weighted.mean(res, w = weights, na.rm = TRUE)
  } else {
    res <- mean(res, na.rm = TRUE)
  }
  names(res) <- "rmse"
  return(res^.5)
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
  c(rmse(response, prediction, weights, ...),
    mae(response, prediction, weights, ...))
}

multiclass_scoring1 <-
  function(prob, response, levels=NULL, weights = NULL, messages=1) {
    if (is.null(levels) && length(colnames(prob))==length(levels))
      levels <- colnames(prob)
    if (is.factor(response) && is.null(levels)) {
      levels <- levels(response)
    }
    if (is.null(levels)) stop("missing definition of levels")
    cl <- levels
    if (is.factor(response)) {
      cl.response <- levels(response)
    } else {
      cl.response <- unique(response)
    }
    newcl <- which(!cl.response%in%cl) # responseerved classes for which no predictions are available
    ## assigning pred 0 probabilities to unresponseerved classes
    if (length(newcl)) {
        if (messages>0) warning("new classes among responseervations")
        temp <- array(0, dim = c(nrow(prob), length(newcl)))
        colnames(temp) <- cl.response[newcl]
        prob <- cbind(prob, temp)
    }
    y <- outer(response, colnames(prob), "==")
    ## Brier score
    Bi <- apply((prob - y)^2, 1, sum)
    ## logscore
    Li <- apply(log(prob) * y, 1, function(x) sum(x[is.finite(x)], na.rm=TRUE))

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

##' Predictive model scoring
##'
##' @title Predictive model scoring
##' @param response Observed response
##' @param ... model predictions (continuous predictions or class probabilities
##'   (matrices))
##' @param type continuous or categorical response (the latter is automatically
##'   chosen if response is a factor, otherwise a continuous response is
##'   assumed)
##' @param metrics which metrics to report
##' @param weights optional frequency weights
##' @param names optional names of models coments (given as ..., alternatively
##'   these can be named arguments)
##' @param messages controls amount of messages/warnings (0: none)
##' @export
##' @examples
##' data(iris)
##' set.seed(1)
##' dat <- csplit(iris,2)
##' g1 <- NB(Species ~ Sepal.Width + Petal.Length, data=dat[[1]])
##' g2 <- NB(Species ~ Sepal.Width, data=dat[[1]])
##' pr1 <- predict(g1, newdata=dat[[2]], wide=TRUE)
##' pr2 <- predict(g2, newdata=dat[[2]], wide=TRUE)
##' table(colnames(pr1)[apply(pr1,1,which.max)], dat[[2]]$Species)
##' table(colnames(pr2)[apply(pr2,1,which.max)], dat[[2]]$Species)
##' scoring(dat[[2]]$Species, pr1=pr1, pr2=pr2)
##' ## quantitative response:
##' scoring(response=1:10, prediction=rnorm(1:10))
##' @export
scoring <- function(response, ..., type="quantitative", metrics=NULL,
                    weights=NULL, names=NULL, messages=1) {
  if (is.factor(response) || is.character(response))
    type <- "multiclass"
  if (tolower(type)%in%c("quantitative","cont","continuous")) {
    S <- suppressWarnings(lapply(list(...),
                                 quantitative_scoring1, response=response,
                                 weights=weights, messages=messages))
  } else {
    S <- suppressWarnings(lapply(list(...),
                                 multiclass_scoring1, response=response,
                                 weights=weights, messages=messages))
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

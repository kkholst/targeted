rmse1 <- function(fit, data, response=NULL, ...) {
    yhat <- predict(fit, newdata = data, ...)
    if (is.null(response)) response <- endogenous(fit)
    y <- data[, response]
    c(RMSE = mean(as.matrix(y - yhat)^2)^.5)
}

continuous_scoring <- function(fit, data, response=NULL, ...) {
    yhat <- predict(fit, newdata = data, ...)
    if (is.null(response)) response <- endogenous(fit)
    y <- data[, response]
    c(RMSE = mean(as.matrix(y - yhat)^2)^.5)
}

multiclass_scoring <-
  function(prob, obs, levels=NULL, weights = NULL,
           metrics = c("brier"), messages=0) {
    if (is.null(levels) && length(colnames(prob))==length(levels))
      levels <- colnames(prob)
    if (is.factor(obs) && is.null(levels)) {
      levels <- levels(obs)
    }
    if (is.null(levels)) stop("missing definition of levels")
    cl <- levels
    if (is.factor(obs)) {
      cl.obs <- levels(obs)
    } else {
      cl.obs <- unique(obs)
    }
    newcl <- which(!cl.obs%in%cl) # observed classes for which no predictions are available
    ## assigning pred 0 probabilities to unobserved classes
    if (length(newcl)) {
        if (messages>0) warning("new classes among observations")
        temp <- array(0, dim = c(nrow(prob), length(newcl)))
        colnames(temp) <- cl.obs[newcl]
        prob <- cbind(prob, temp)
    }
    y <- outer(obs, colnames(prob), "==")
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
    if (tolower(metrics[1])%in%"brier") return(B)
    return(list(brier = B, logscore = L))
}


##' Predictive model scoring
##'
##' @title Multiclass scoring
##' @param prob Model class probabilities (matrix)
##' @param obs Observed class
##' @param weights optional frequency weights
##' @param metrics metrics
##' @param messages Adjust amount of warnings/messages (0 default: none)
##' @aliases Scoring
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
##' @export
scoring <- function(class, ..., weights=NULL, names=NULL) {
    S <- suppressWarnings(lapply(list(...), multiclass_scoring, obs=class, weights=weights))
    if (is.null(names)) {
        names <- base::names(list(...))
    }
    res <- matrix(unlist(S), byrow=TRUE, ncol=length(S[[1]]))
    rownames(res) <- names
    colnames(res) <- names(S[[1]])
    return(res)
}

##' Kernel density estimator predictions
##'
##' @title Prediction for kernel density estimates
##' @param object density object
##' @param xnew New data on which to make predictions for
##' @param ... additional arguments to lower level functions
##' @return Numeric vector with predictions.
##' @export
##' @author Klaus K. Holst
predict.density <-
function(object, xnew, ...) {
    neval <- length(object$x)
    nnew  <- length(xnew)
    ynew  <- rep(NA, nnew)
    for (i in seq_len(nnew)) {
        j <- findInterval(xnew[i], object$x)
        if (j == 0 || j == neval) {
            ynew[i] <- 0  ## don't extrapolate beyond range,set to 0
        } else {
            ynew[i] <- object$y[j] + (object$y[j + 1] - object$y[j]) /
                (object$x[j + 1] - object$x[j]) *
                (xnew[i] - object$x[j])
        }
    }
    return(ynew)
}

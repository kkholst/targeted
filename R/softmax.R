
##' Softmax transformation
##' @export
##' @param x Input matrix (e.g., linear predictors of multinomial logistic model)
##' @param log Return on log-scale (default FALSE)
##' @param ref Add reference level (add 0 column to x)
##' @param ... Additional arguments to lower level functions
##' @return Numeric matrix of dimension n x p, where \code{n= nrow(x)} and
##'   \code{p = ncol(x) + (ref==TRUE)}
softmax <- function(x, log=FALSE, ref=TRUE, ...) {
    if (is.vector(x) || NCOL(x)==1) {
        return(.softmax(cbind(x), log=log, ref=TRUE)[, -1, drop=is.vector(x)])
    }
    .softmax(cbind(x), log=log, ref=ref)
}

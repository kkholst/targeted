#' @description Pooled Adjacent Violators Algorithm
#' @title Pooled Adjacent Violators Algorithm
#' @param y response variable
#' @param x (optional) predictor vector (otherwise y is assumed
#' to be a priori sorted according to relevant predictor)
#' @param weights weights (optional) weights
#' @return List with index (idx) of jump points and values (value)
#' at each jump point.
#' @export
#' @author Klaus K. Holst
#' @aliases pava isoreg isoregw
#' @examples
#' x <- runif(5e3, -5, 5)
#' pr <- lava::expit(-1 + x)
#' y <- rbinom(length(pr), 1, pr)
#' pv <- pava(y, x)
#' plot(pr ~ x, cex=0.3)
#' with(pv, lines(sort(x)[index], value, col="red", type="s"))
pava <- function(y, x=numeric(0), weights=numeric(0)) {
    if (length(x)) {
        ord <- order(x)
        y <- y[ord]
        if (length(weights)==length(y)) weights <- weights[ord]
    }
    if (length(weights)!=length(y)) weights <- numeric(0)
    val <- .pava(y, x, weights)
    return(val)
}

#' @export
isoregw <- function(x, y, weights=NULL, ...) {
    ord <- order(x)
    if (is.null(weights)) {
        pv <- pava(y[ord])
    } else {
        ## replication weights
        pv <- try(pava(y[ord], x=numeric(0), weights[ord]), silent=TRUE)
        if (inherits(pv, "try-error")) return(base::identity)
    }
    suppressWarnings(f <- tryCatch(approxfun(x[ord[pv$index]], pv$value,
                                             rule=2, method="constant"),
                                   error=function(...) base::identity))
    return(f)
}

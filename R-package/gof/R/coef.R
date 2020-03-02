##' @export
coef.cumres <- function(object,...) {
    res <- with(object, data.frame(KS, CvM))
    colnames(res) <- c("p-value (KS)", "p-value (CvM)")
    rownames(res) <- object$variable
    res
}

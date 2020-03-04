##' @export
coef.cumres <- function(object,...) {
    res <- with(object, data.frame(KS, CvM))
    colnames(res) <- c("p-value(Sup)", "p-value(L2)")
    rownames(res) <- object$variable
    res
}

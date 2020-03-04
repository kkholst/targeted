##' @export
print.cumres <- function(x,...) {
    cat("\n")
    print(coef(x), ...)
    cat("\nBased on ", x$R, " realizations.\n",sep="")
    invisible(x)
}

##' @export
summary.cumres <- function(object,...) {
    res <- with(object, list(KS=KS, CvM=CvM, R=R, n=n, type=type, model=model, variable=variable))
    return(structure(res, class="summary.cumres"))
}

##' @export
print.summary.cumres <- function(x,...) {
    cat("\n");
    for (i in 1:length(x$variable)) {
        if (!is.null(x$KS)) cat("Sup-test: p-value=", x$KS[i], "\n", sep="")
        if (!is.null(x$CvM)) cat("L2-test: p-value=", x$CvM[i], "\n", sep="")
        cat("Based on ", x$R, " realizations. Cumulated residuals ordered by '", x$variable[i], "'.\n", sep="")
        cat("---\n");
    }
    invisible(x)
}



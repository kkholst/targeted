##' @export
print.targeted <- function(x, ...) {
    print(x$call)
    cat("\n")
    print(x$estimate, ...)
}


print.summary.targeted <- function(x, ...) {
    print(x$call)
    cat("\n")    
    print(x$estimate, ...)
}

##' @export
summary.targeted <- function(object, ...) {
    structure(list(estimate=object$estimate, call=object$call), class="summary.targeted")
}


##' @export
iid.targeted <- function(x, ...) {
    iid(x$estimate,...)
}

##' @export
logLik.targeted <- function(object, ...) {
    val <- object$logLik
    if (is.null(val)) return(NULL)
    structure(val, nobs=object$nobs, nall=object$nobs, df=length(coef(object)), class="logLik")
}

##' @export
vcov.targeted <- function(object, ...) {
    vcov(object$estimate,...)
}

##' @export
coef.targeted <- function(object, ...) {
    coef(object$estimate,...)
}

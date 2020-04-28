#' @title targeted class object
#'
#' @description The functions \code{\link{riskreg}} and \code{\link{ace}} returns an object of the type \code{targeted}.
#'
#' An object of class '\code{targeted}' is a list with at least the following components:
#' \describe{
#'   \item{estimate}{An \code{estimate} object with the target parameter estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{opt}{Object returned from the applied optimization routine}
#'   \item{npar}{number of parameters of the model (target and nuisance)}
#'   \item{type}{String describing the model}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object of class \code{targeted}:
#' \itemize{
#'   \item{\code{coef}}{Extract target coefficients of the estimated model.}
#'   \item{\code{vcov}}{Extract the variance-covariance matrix of the target parameters.}
#'   \item{\code{iid}}{Extract the estimated influence function.}
#'   \item{\code{print}}{Print estimates of the target parameters.}
#'   \item{\code{summary}}{Extract information on both target parameters and estimated nuisance model.}'
#'  }
#'
#' @aliases targeted-class riskreg.targeted ace.targeted
#' @seealso \code{\link{riskreg}}, \code{\link{ace}}
#' @return objects of the S3 class '\code{targeted}'
#' @examples ## See example(riskreg) for examples
#' @docType class
#' @name targeted-class
NULL


##' @export
print.targeted <- function(x, ...) {
    print(x$estimate, ...)
}

##' @export
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

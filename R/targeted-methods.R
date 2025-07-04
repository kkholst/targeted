#' @title targeted class object
#'
#' @description The functions \code{\link{riskreg}} and \code{\link{ate}}
#'   returns an object of the type \code{targeted}.
#'
#' An object of class '\code{targeted}' is a list with at least the
#' following components:
#' \describe{
#'   \item{estimate}{An \code{estimate} object with the target parameter
#'   estimates (see \code{\link[lava]{estimate.default}})}
#'   \item{opt}{Object returned from the applied optimization routine}
#'   \item{npar}{number of parameters of the model (target and nuisance)}
#'   \item{type}{String describing the model}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object of
#' class \code{targeted}:
#' \describe{
#'   \item{\code{coef}}{Extract target coefficients of the estimated model.}
#'   \item{\code{vcov}}{Extract the variance-covariance matrix of
#' the target parameters.}
#'   \item{\code{IC}}{Extract the estimated influence function.}
#'   \item{\code{print}}{Print estimates of the target parameters.}
#'   \item{\code{summary}}{Extract information on both target parameeters and
#' estimated nuisance model.}'
#'  }
#'
#' @aliases targeted-class riskreg.targeted ate.targeted
#' @seealso \code{\link{riskreg}}, \code{\link{ate}}
#' @return objects of the S3 class '\code{targeted}'
#' @examples ## See example(riskreg) for examples
#' @docType class
#' @name targeted-class
NULL


#' @export
print.targeted <- function(x, ...) {
  print(x$estimate, ...)
}

#' @export
print.summary.targeted <- function(x, ...) {
  print(x$call)
  cat("\n")
  print(x$estimate, ...)
}

#' @export
summary.targeted <- function(object, ...) {
  obj <- structure(list(estimate = object$estimate, call = object$call),
    class = "summary.targeted"
  )
  return(obj)
}

#' @export
IC.targeted <- function(x, ...) {
  return(lava::IC(x$estimate, ...))
}

#' @export
transform.targeted <- function(`_data`, ...) {
  transform(`_data`$estimate, ...)
}

##' @export
labels.targeted <- function(object, str, ...) {
  labels(object$estimate, labels=str, ...)
}

##' @export
parameter.targeted <- function(x, ...) {
  parameter(x$estimate, ...)
}

##' @export
subset.targeted <- function(x, keep, ...) {
  subset(x$estimate, keep = keep, ...)
}

#' @export
logLik.targeted <- function(object, ...) {
  val <- object$logLik
  if (is.null(val)) {
    return(NULL)
  }
  obj <- structure(val,
    nobs = object$nobs, nall = object$nobs, df = length(coef(object)),
    class = "logLik"
  )
  return(obj)
}

#' @export
vcov.targeted <- function(object, ...) {
  return(vcov(object$estimate, ...))
}

#' @export
coef.targeted <- function(object, ...) {
  return(coef(object$estimate, ...))
}

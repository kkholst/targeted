#' @keywords internal
"_PACKAGE"

##' Targeted inference
##'
##' Methods for targeted and semiparametric inference.
##'
##' @name targeted-package
##' @import Rcpp methods
##' @importFrom graphics plot points abline lines
##' @importFrom grDevices nclass.Sturges
##' @importFrom lava IC getoutcome estimate Inverse na.pass0
##' @importFrom stats approxfun as.formula update binomial deriv density
##'  	glm.fit lm.wfit lm.fit glm lm coef vcov
##'     model.frame model.matrix na.pass nlminb predict
##'     dnorm quantile terms weighted.mean runif
##'     .getXlevels delete.response model.response gaussian formula
##'     model.offset reformulate drop.terms weights model.extract
##' @importFrom digest sha1
##' @importFrom optimx optimx
##' @importFrom data.table data.table is.data.table
##' @importFrom futile.logger flog.warn flog.debug flog.info
##' @importFrom R6 R6Class
##' @importFrom survival survfit
##' @importFrom utils tail head
##' @useDynLib targeted, .registration=TRUE
##' @aliases targeted-package targeted
##' @author Klaus K. Holst (Maintainer) <klaus@@holst.it>
##' @keywords package
##' @examples
##' \dontrun{
##' example(riskreg)
##' example(cate)
##' example(ate)
##' example(calibration)
##' }
loadModule("riskregmodel", TRUE)
NULL

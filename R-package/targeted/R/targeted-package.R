
##' Targeted inference 
##'
##' Semi-parametric regression models with nuisance parameters.
##'
##' @name targeted-package
##' @import Rcpp
##' @importFrom graphics plot points
##' @importFrom lava iid getoutcome estimate Inverse
##' @importFrom stats as.formula update binomial deriv
##'  	glm.fit lm.fit glm lm coef vcov model.frame model.matrix nlminb predict 
##' @importFrom optimx optimx
##' @importFrom futile.logger flog.warn flog.debug flog.info
##' @importFrom utils tail head
##' @useDynLib targeted, .registration=TRUE
##' @aliases targeted-package targeted
##' @docType package
##' @author Klaus K. Holst Maintainer: <klaus@@holst.it>
##' @keywords package
##' @examples
##'
##' example(riskreg)
##'
##' example(ace)
##'
loadModule("riskregmodel", TRUE)
loadModule("dcmodel", TRUE)
NULL

##' For internal use
##'
##' @title For internal use
##' @name Us
##' @rdname internal
##' @author Klaus K. Holst
##' @keywords utilities
##' @export
##' @aliases
##' Us
NULL

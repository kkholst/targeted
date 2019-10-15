
##' Targeted inference 
##'
##' Semi-parametric regression models with nuisance parameters.
##'
##' @name target-package
##' @import Rcpp
##' @importFrom graphics plot points
##' @importFrom lava iid getoutcome estimate Inverse
##' @importFrom stats as.formula update binomial deriv
##'  	glm.fit lm.fit glm lm coef vcov model.frame model.matrix nlminb predict 
##' @importFrom DEoptim DEoptim DEoptim.control
##' @importFrom utils tail head
##' @useDynLib target, .registration=TRUE
##' @aliases target-package target
##' @docType package
##' @author Klaus K. Holst Maintainer: <klaus@@holst.it>
##' @keywords package
##' @examples
##'
##' example(riskreg)
##'
##' example(ace)
##'
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

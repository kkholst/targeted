
##' Targeted inference
##'
##' Methods for targeted and semiparametric inference including
##' augmented inverse probability weighted estimators for missing data and
##' causal inference.
##'
##' @name targeted-package
##' @import Rcpp methods
##' @importFrom graphics plot points abline lines
##' @importFrom grDevices nclass.Sturges
##' @importFrom lava iid getoutcome estimate Inverse na.pass0
##' @importFrom stats approxfun as.formula update binomial deriv density
##'  	glm.fit lm.wfit lm.fit glm lm coef vcov
##'     model.frame model.matrix na.pass nlminb predict
##'     dnorm quantile terms weighted.mean runif
##' @importFrom digest sha1
##' @importFrom optimx optimx
##' @importFrom data.table data.table is.data.table
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
##' example(ate)
##'
##' example(calibration)
##'
loadModule("riskregmodel", TRUE)
NULL

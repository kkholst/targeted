
##' State Space Models
##'
##' Filtering and smoothing for State Space Models
##'
##' @name ssm-package
##' @import Rcpp methods
##' @importFrom graphics plot points abline lines
##' @importFrom grDevices nclass.Sturges
##' @importFrom stats approxfun as.formula update binomial deriv density
##'  	glm.fit lm.wfit lm.fit glm lm coef vcov
##'     model.frame model.matrix na.pass nlminb predict
##'     dnorm quantile terms weighted.mean runif
##' @importFrom optimx optimx
##' @importFrom data.table data.table is.data.table
##' @importFrom futile.logger flog.warn flog.debug flog.info
##' @importFrom utils tail head
##' @useDynLib ssm, .registration=TRUE
##' @aliases ssm-package ssm
##' @docType package
##' @author Klaus K. Holst Maintainer: <klaus@@holst.it>
##' @keywords package
loadModule("ssmodel", TRUE)
NULL

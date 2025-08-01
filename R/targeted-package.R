#' @keywords internal
"_PACKAGE"

#' @name targeted-package
#' @import Rcpp methods
#' @importFrom graphics plot points abline lines
#' @importFrom grDevices nclass.Sturges
#' @importFrom lava IC getoutcome estimate Inverse na.pass0 score logit expit
#'   parameter sim
#' @importFrom stats approxfun as.formula update binomial deriv density glm.fit
#'   lm.wfit lm.fit glm lm coef vcov model.frame model.matrix na.pass nlminb
#'   predict dnorm quantile terms weighted.mean runif .getXlevels
#'   delete.response model.response gaussian formula model.offset reformulate
#'   drop.terms weights model.extract cov2cor pchisq uniroot sd na.omit offset
#' @importFrom rlang hash call_match
#' @importFrom optimx optimx
#' @importFrom data.table data.table is.data.table dcast :=
#' @importFrom R6 R6Class
#' @importFrom survival survfit Surv strata
#' @importFrom utils tail head capture.output getFromNamespace
#' @references
#' Bang & Robins (2005) Doubly Robust Estimation in Missing Data and
#' Causal Inference Models, Biometrics.
#'
#' Vansteelandt & Dukes (2022) Assumption-lean inference for
#' generalised linear model parameters, Journal of the Royal Statistical
#' Society: Series B (Statistical Methodology).
#'
#' Thomas S. Richardson, James M. Robins & Linbo Wang (2017) On Modeling and
#' Estimation for the Relative Risk and Risk Difference, Journal of the American
#' Statistical Association.
#'
#' Mark J. van der Laan (2006) Statistical Inference for Variable Importance,
#' The International Journal of Biostatistics.
#'
#' @useDynLib targeted, .registration=TRUE
#' @aliases targeted-package targeted
#' @author Klaus K. Holst (Maintainer) <klaus@@holst.it>
#' @keywords package
#' @examples
#' \dontrun{
#' example(riskreg)
#' example(cate)
#' example(ate)
#' example(calibration)
#' }
loadModule("riskregmodel", TRUE)
NULL

#' @title Scores truncated by death
#' @description
#' Simulated data inspired by the FLOW study (Perkovic 2024)...elt()
#' The following variables are considered in this simulated data
#' set
#'
#' - `time`: time of first event in years (first major irreversible kidney
#'   event or non-related death)
#' - `status`: event type at first major irreversible kidney event (=1),
#'   non-related death (=2), or right censoring (=0)
#' - `y`: clinical outcome measurement (eGFR) at landmark time (:=2)
#' - `r`: missing indicator for `y` (1 if observed, 0 if either t<2 or if the
#'   outcome was not measured for other reasons)
#' - `a`: binary treatment (1 := active, 0 := placebo)
#' - `x1`: covariate, clinical outcome at baseline (eGFR)
#' - `x2`: covariate, binary treatment usage indicator (1: SGLT2 treatment,
#'   0: none).
#'
#' The actual failure times and censoring times are also included
#' (`failure.time`, `cens.time`), and the full-data outcome (`y0`) given t>2.
#'
#' @references
#' Perkovic, V., Tuttle, K. R., Rossing, P.,
#' Mahaffey, K. W., Mann, J. F., Bakris, G., Baeres, F. M., Idorn, T.,
#' Bosch-Traberg, H., Lausvig, N. L., and Pratley, R. (2024). Effects of
#' semaglutide on chronic kidney disease in patients with type 2 diabetes. New
#' England Journal of Medicine, 391(2):109–121.
#' @name truncatedscore
#' @docType data
#' @keywords data
#' @source Simulated data
#' @examples
#' data(truncatedscore)
NULL


##' @export
lava::IC

##' @export
lava::sim

##' @export
lava::score

##' @export
lava::parameter

##' @export
lava::estimate

##' @export
survival::strata

##' @export
survival::Surv

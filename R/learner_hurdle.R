##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param lrn
##' @param lrn.mix
##' @param value
##' @param info
##' @param ...
##' @return
##' @author Klaus Kähler Holst
learner_hurdle <- function(lrn, lrn.mix, value=0L,
                           info="hurdle model", ...) {
  est <- function(formula, data, ...) {
    idx <- which(!lrn.mix$response(data))
    lrn$estimate(data[idx,,drop=FALSE])
    lrn.mix$estimate(data)
    return(list("learner" = lrn, "learner.mixing" = lrn.mix))
  }
  pred <- function(object, newdata, ...) {
    pr <- object[[2]]$predict(newdata)
    value * pr + (1-pr) * object[[1]]$predict(newdata)
  }
  formula <- reformulate(".", deparse(lrn$formula[[2]]))
  learner$new(
            formula = formula,
            estimate = est,
            predict = pred,
            info = info, ...)
}

#' @description Constructs a [learner] class object for fitting Cox proportional
#'   hazards models.
#' @inherit constructor_shared
#' @inheritParams mets::phreg
#' @author Klaus Kähler Holst
#' @export
#' @examples
#' data(sTRACE, package="mets")
#' mod <- learner_surv_cox(Surv(time, status>0) ~ sex + strata(age))
#' mod$estimate(sTRACE)
#' mod$predict(head(sTRACE), time=5) # P(T>t|X)
learner_surv_cox <- function(formula, info="mets::phreg",
                        learner.args = NULL, ...) {
  args <- c(learner.args,
            list(formula = formula,
                 predict.args = c(),
                 info = info))
  args$estimate.args <- c(list(...))
  args$estimate <- function(formula, data, ...) {
    mets::phreg(formula, data, ...)
  }
  args$predict <- function(object, newdata,
                           time=NULL,
                           individual.time=FALSE,
                           se=FALSE, ...) {
    if (is.null(time)) {
      time <- object$time
    }
    ord <- order(time)
    time <- time[ord]
    ## suppressMessages(browser())
    if (individual.time && length(time) == nrow(newdata)) {
      newdata <- newdata[ord, , drop=FALSE]
    }
    pr <- predict(object, newdata=newdata, se=se, time=time,
                  individual.time = individual.time,
                  ...)$surv[, , drop=TRUE]
    if (length(time) > 1L) {
      if (individual.time) return(pr[order(ord)])
      pr <- pr[, order(ord), drop=FALSE]
    }
    return(pr)
  }
  mod <- do.call(learner$new, args)
  class(mod) <- c("learner_surv_cox", class(mod))
  return(mod)
}


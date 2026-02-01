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
#' mod$predict(head(sTRACE), times=5) # P(T>t|X)
learner_surv_cox <- function(formula, info="mets::phreg",
                        learner.args = NULL, ...) {
  args <- c(learner.args,
            list(formula = formula,
                 predict.args = c(),
                 info = info))
  args$estimate.args <- c(list(...))
  args$predict.args <- list(
    times = NULL,
    individual.time = FALSE,
    se = FALSE)
  args$estimate <- function(formula, data, ...) {
    mets::phreg(formula, data, ...)
  }
  args$predict <- function(object, newdata,
                           times, individual.time, se,
                           ...) {
    if (is.null(times)) {
      times <- object$time
    }
    ord <- order(times)
    times <- times[ord]
    ## suppressMessages(browser())
    if (individual.time && length(times) == nrow(newdata)) {
      newdata <- newdata[ord, , drop=FALSE]
    }
    pr <- predict(object, newdata=newdata, se=se, times=times,
                  individual.time = individual.time,
                  ...)$surv[, , drop=TRUE]
    if (length(times) > 1L) {
      if (individual.time) return(pr[order(ord)])
      pr <- pr[, order(ord), drop=FALSE]
    }
    return(pr)
  }
  mod <- do.call(learner$new, args)
  class(mod) <- c("learner_surv_cox", class(mod))
  return(mod)
}

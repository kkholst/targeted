#' @description Constructs a [learner] class object for random survival forests
#' @inherit constructor_shared
#' @inheritParams ranger::ranger
#' @author Klaus Kähler Holst
#' @export
#' @examples
#' data(sTRACE, package="mets")
#' mod <- learner_surv_rf(Surv(time, status>0) ~ sex + age)
#' mod$estimate(sTRACE)
#' mod$predict(head(sTRACE), time=5) # P(T>t|X)
learner_surv_rf <- function(formula,
                            info="survival forest (ranger)",
                            num.threads = 1L,
                            learner.args = NULL, ...) {
  args <- c(learner.args,
            list(formula = formula,
                 predict.args = c(),
                 info = info))
  args$estimate.args <- c(
    list(num.threads = num.threads),
    list(...))
  args$predict.args <- list(times = NULL)
  args$estimate <- function(formula, data, ...) {
    ranger::ranger(formula, data = data, ...)
  }
  args$predict <- function(object, newdata, times, ...) {
    res <- cumhaz(object, newdata, times=times, ...)$surv
    if (NCOL(res)==1L) res <- as.vector(res)
    return(res)
  }
  mod <- do.call(learner$new, args)
  class(mod) <- c("learner_surv_rf", class(mod))
  return(mod)
}

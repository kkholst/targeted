#' @description [learner] generator function for [superlearner]
#' @export
#' @inherit learner_shared
#' @inheritParams superlearner
#' @seealso [cv.learner_sl]
#' @param ... Additional arguments to [superlearner]
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' d <- sim1()
#'
#' m <- list(
#'   "mean" = learner_glm(y ~ 1),
#'   "glm" = learner_glm(y ~ x1 + x2),
#'   "iso" = predictor_isoreg(y ~ x1)
#' )
#'
#' s <- learner_sl(m, nfolds = 10)
#' s$estimate(d)
#' pr <- s$predict(d)
#' if (interactive()) {
#'     plot(y ~ x1, data = d)
#'     points(d$x1, pr, col = 2, cex = 0.5)
#'     lines(cos(x1) + x1 ~ x1, data = d[order(d$x1), ],
#'           lwd = 4, col = lava::Col("darkblue", 0.3))
#' }
#' print(s)
#' # weights(s$fit)
#' # score(s$fit)
#'
#' cvres <- cv(s, data = d, nfolds = 3, rep = 2)
#' cvres
#' # coef(cvres)
#' # score(cvres)
learner_sl <- function(learners,
                         info = NULL,
                         nfolds = 5L,
                         meta.learner = metalearner_nnls,
                         model.score = mse,
                         learner.args = NULL,
                         ...) {

  if (is.null(info)) {
    info <- "superlearner\n"
    nn <- names(learners)
    for (i in seq_along(nn)) {
      info <- paste0(info, "\t", nn[i])
      if (i < length(nn)) info <- paste0(info, "\n")
    }
  }
  args <- c(learner.args, list(info = info))
  estimate.args <- list(learners = learners, nfolds = nfolds,
    meta.learner = meta.learner, model.score = model.score
  )
  args$estimate.args <- c(estimate.args, list(...))
  args$estimate <- function(data, ...) superlearner(data = data, ...)
  args$predict <- function(object, newdata, ...) predict(object, newdata, ...)

  mod <- do.call(learner$new, args)

  # duplicate check from superlearner to catch error during instantiation
  # of a learner instead in the estimate method call
  if (length(unique(lapply(learners, \(m) all.vars(m$formula)[[1]]))) > 1) {
    stop("All learners must have the same response variable.")
  }
  mod$update(learners[[1]]$formula) # TODO: not clean but will be fixed later
  # because it requires changes to the learner R6 class and the cv function

  attr(mod, "model.score") <- model.score
  class(mod) <- c("learner_sl", class(mod))
  return(mod)
}

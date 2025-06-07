#' @export
print.learner.list <- function(x, ...) {
  for (i in seq_len(length(x))) {
    fit <- x[[i]]$fit
    attr(fit, c("design")) <- NULL
    if (!is.atomic(fit) && !is.null(fit$call)) fit$call <- substitute()
    cat("\u2500\u2500\u2500 ", names(x)[[i]], "\n", sep="")
    print(fit)
  }
  invisible(x)
}

#' @export
#' @inherit survival::strata
#' @seealso [survival::strata]
stratify <- survival::strata

#' @title Construct stratified learner
#' @description This function creates a stratified learner from an existing
#'   [learner] wrapper function such as [learner_glm] or [learner_xgboost]. The
#'   stratification variable can be specified either using the `stratify`
#'   argument (which can be given as a string "a" or a formula , for example ~
#'   I(a==0)), or it can be defined as a special term directly in the formula, y
#'   ~ ... + stratify(a). The formula will subsequently be passed to the
#'   `learner_` wrapper without the stratify special term.
#' @param formula formula specifying outcome and design matrix
#' @param learner (learner) [learner] object
#' @param learner.args (list) optional arguments to the learner constructor
#' @param stratify (character,formula) variables to stratify by
#' @param info optional description of the model
#' @param ... additional arguments passed to the learner constructor
#' @return learner object
#' @export
#' @examples
#' simdata <- function(n=1000) {
#'   a <- rbinom(n, 1, 0.5)
#'   x <- rnorm(n)
#'   y <- rbinom(n, 1, plogis(-1 + a + a * x))
#'   data.frame(y, a, x)
#' }
#' d <- simdata()
#'
#' lr <- learner_stratify(
#'   y ~ x + stratify(a),
#'   learner_glm,
#'   family=binomial()
#' )
#' lr$estimate(d)
#' lr$predict(head(d))
learner_stratify <- function(formula,
                             learner,
                             stratify=NULL,
                             info=NULL,
                             learner.args=list(),
                             ...) {
  if (!is.null(stratify)) {
    if (inherits(stratify, "formula")) stratify <- all.vars(stratify)
    if (!length(stratify) == 1L && is.character(stratify)) {
      stop("expected string `stratify``")
    }
    ff <- as.character(formula)
    formula <- reformulate(c(ff[3],
                             paste0("stratify(", stratify, ")")), ff[2])
  }
  dots <- list(...)
  if (length(dots)>0) learner.args[names(dots)] <- dots
  est <- function(formula, data, stratify, ...) {
    dots <- list(...)
    if (length(dots)>0) earner.args[names(dots)] <- dots
    lr <- do.call(learner, c(list(formula), learner.args))
    if (is.null(stratify)) stratify <- rep(1, nrow(data))
    res <- c()
    stratify <- factor(stratify)
    for (i in levels(stratify)) {
      idx <- which(stratify == i)
      mylr <- lr$clone(deep=TRUE)
      mylr$estimate(data[idx, , drop=FALSE])
      res <- c(res, list(mylr))
    }
    names(res) <- levels(stratify)
    class(res) <- c("learner.list", "list")
    return(res)
  }
  pred <- function(object, newdata, stratify, ....) {
    res <- rep(NA, nrow(newdata))
    for (i in seq_along(levels(stratify))) {
      s <- levels(stratify)[i]
      if (s %in% names(object)) {
        idx <- which(stratify == s)
        if (length(idx) > 0) {
          res[idx] <- object[[s]]$predict(
            newdata[idx, , drop=FALSE], ...
          )
        }
      } # else the strata was not in the data used for estimation
    }
    return(res)
  }
  lr <- targeted::learner$new(
    info = info,
    formula = formula,
    estimate = est,
    predict = pred,
    specials = "stratify",
  )
  return(lr)
}

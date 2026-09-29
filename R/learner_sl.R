# Formula of a super learner: the common response variable of the base
# learners and the union of the covariates of all base learners. Special terms
# (e.g., weights, offset) are not yet handled and appear as covariates.
sl_formula <- function(learners, env = environment(learners[[1]]$formula)) {
  response <- paste(deparse(learners[[1]]$formula[[2]]), collapse = " ")
  covariates <- unique(unlist(
    lapply(learners, \(lr) all.vars(lr$formula[[3]]))
  ))
  if (length(covariates) == 0) covariates <- "1"
  return(reformulate(covariates, response = response, env = env))
}

#' @title R6 class for super learners
#' @description Super learner (stacked ensemble) implementation of the
#' [learner] R6 class. Objects are usually created with [learner_sl()]. A
#' `learnerSL` object owns a list of base learners, which are estimated and
#' combined by [superlearner()].
#'
#' The formula of the super learner is derived from its base learners. It is
#' defined by their common response variable and the union of the covariates of
#' all base learners. Special terms (e.g., weights or offsets) of the base
#' learners are currently not handled and appear as covariates.
#'
#' The `update()` method changes the response variable of the super learner and
#' of all base learners. The covariates of the base learners are never modified
#' by `update()`.
#'
#' The base learners are deep cloned when a `learnerSL` object is created and
#' when it is cloned with `clone(deep = TRUE)`. Updating a super learner
#' therefore neither modifies the learner objects used to create it nor the
#' base learners of any other super learner object.
#' @seealso [learner_sl], [superlearner], [cv.learner_sl]
#' @examples
#' n <- 200
#' d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
#' d$y <- d$x1 + rnorm(n)
#' d$z <- d$x1 - d$x2 + rnorm(n)
#'
#' sl <- learnerSL$new(list(
#'   "mean" = learner_glm(y ~ 1),
#'   "glm" = learner_glm(y ~ x1 + x2)
#' ), nfolds = 2)
#' sl$formula # common response and union of covariates
#'
#' # update the response variable of the super learner and all base learners
#' sl$update("z")
#' sl$formula
#' lapply(sl$learners, \(lr) lr$formula)
#'
#' sl$estimate(d)
#' sl$predict(head(d))
#' @export
learnerSL <- R6::R6Class("learner_sl", # nolint
  inherit = learner,
  public = list(
    #' @description
    #' Create a new super learner object
    #' @param learners (list) List of [learner] objects that define the
    #' ensemble. All learners must be defined with a formula and share the same
    #' response variable.
    #' @param info (character) Optional information to describe the
    #' instantiated object. Defaults to a listing of the names of `learners`.
    #' @param nfolds (integer) Number of folds to use in cross-validation to
    #' estimate the ensemble weights.
    #' @param meta.learner (function) Algorithm to learn the ensemble weights
    #' (see [superlearner]).
    #' @param model.score (function) Model scoring method (see [superlearner]).
    #' @param learner.args (list) Additional arguments to
    #' [learner$new()][learner].
    #' @param ... Additional arguments to [superlearner].
    initialize = function(learners,
                          info = NULL,
                          nfolds = 5L,
                          meta.learner = metalearner_nnls,
                          model.score = mse,
                          learner.args = NULL,
                          ...) {
      if (!is.list(learners) || length(learners) == 0 ||
          !all(vapply(learners, \(lr) inherits(lr, "learner"), logical(1)))) {
        stop("'learners' must be a non-empty list of learner objects.")
      }
      has_response <- vapply(
        learners,
        \(lr) inherits(lr$formula, "formula") && length(lr$formula) == 3L,
        logical(1)
      )
      if (!all(has_response)) {
        stop("All learners must be defined with a formula with a response.")
      }
      # duplicate check from superlearner to catch error during instantiation
      # instead of in the estimate method call
      # TODO: how do we want to handle different response variables in the base learners?
      if (length(unique(lapply(learners, \(m) all.vars(m$formula)[[1]]))) > 1) {
        stop("All learners must have the same response variable.")
      }

      if (is.null(info)) {
        info <- "superlearner\n"
        nn <- names(learners)
        for (i in seq_along(nn)) {
          info <- paste0(info, "\t", nn[i])
          if (i < length(nn)) info <- paste0(info, "\n")
        }
      }

      # base learners are owned by the super learner and are therefore cloned
      # to avoid modifying the learner objects provided by the user
      private$.learners <- lapply(learners, \(lr) lr$clone(deep = TRUE))

      estimate.args <- list(
        nfolds = nfolds,
        meta.learner = meta.learner,
        model.score = model.score
      )
      args <- c(learner.args, list(
        info = info,
        estimate = function(data, ...) superlearner(data = data, ...),
        predict = function(object, newdata, ...) {
          predict(object, newdata, ...)
        },
        estimate.args = c(estimate.args, list(...))
      ))
      do.call(super$initialize, args)
      private$.formula <- sl_formula(private$.learners)
    },

    #' @description
    #' Estimation method. Estimates the super learner with [superlearner].
    #' @param data (data.frame) Data used to estimate the super learner.
    #' @param ... Additional arguments to [superlearner] and the prediction
    #' filter generator function.
    #' @param learners (list) Base learners. Defaults to the base learners of
    #' the object.
    #' @param store (logical) If TRUE, the estimated model is stored inside the
    #' object.
    estimate = function(data, ..., learners = private$.learners,
                        store = TRUE) {
      # base learners are passed explicitly (instead of via estimate.args)
      # because the estimation function of cloned objects is bound to the
      # private environment of the original object
      return(super$estimate(data, learners = learners, ..., store = store))
    },

    #' @description
    #' Update the response variable of the super learner and all its base
    #' learners. Each base learner keeps its covariates. A warning is raised
    #' when `formula` specifies covariates which differ from the covariates of
    #' the super learner, because the covariates are not updated.
    #' @param formula (formula or character) Formula or name of the new
    #' response variable (e.g., `"z"`, `"I(a == 1)"` or `z ~ .`).
    update = function(formula) {
      if (is.character(formula) && !grepl("~", formula)) {
        response <- formula
      } else {
        formula <- stats::as.formula(formula)
        if (length(formula) != 3L) {
          stop("'formula' must specify a response variable.")
        }
        response <- paste(deparse(formula[[2]]), collapse = " ")
        covariates <- all.vars(formula[[3]])
        if (!identical(covariates, ".") &&
            !setequal(covariates, all.vars(private$.formula[[3]]))) {
          warning(
            "learnerSL$update() only updates the response variable. ",
            "The covariates of the base learners are not modified."
          )
        }
      }
      for (lr in private$.learners) lr$update(response)
      private$.formula <- sl_formula(
        private$.learners,
        env = environment(private$.formula)
      )
      return(invisible(private$.formula))
    },

    #' @description
    #' Summary method. See [learner$summary()][learner].
    summary = function() {
      obj <- super$summary()
      obj$estimate.args <- c(
        list(learners = private$.learners), obj$estimate.args
      )
      return(obj)
    },

    #' @description
    #' Get options
    #' @param arg (character) Name of option to get value of. Use `"learners"`
    #' to get the base learners.
    opt = function(arg) {
      if (identical(arg, "learners")) return(private$.learners)
      return(super$opt(arg))
    }
  ),
  active = list(
    #' @field learners Return the list of base learners (read-only). Use
    #' [learnerSL$update()][learnerSL] to update the response variable of the
    #' base learners.
    learners = function(value) {
      if (!missing(value)) stop("'learners' is read-only.")
      return(private$.learners)
    }
  ),
  private = list(
    # @field .learners List of base learners
    .learners = NULL,
    deep_clone = function(name, value) {
      if (name == ".learners") {
        return(lapply(value, \(lr) lr$clone(deep = TRUE)))
      }
      return(super$deep_clone(name, value))
    }
  )
)

#' @description Constructs a [learnerSL] object for fitting a
#' [superlearner].
#' @export
#' @inherit learner_glm
#' @inheritParams superlearner
#' @seealso [learnerSL], [cv.learner_sl]
#' @param ... Additional arguments to [superlearner]
#' @return [learnerSL] object.
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
#'   "iso" = learner_isoreg(y ~ x1)
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
  if (inherits(learners, "learner")) return(learners)
  return(learnerSL$new(
    learners = learners,
    info = info,
    nfolds = nfolds,
    meta.learner = meta.learner,
    model.score = model.score,
    learner.args = learner.args,
    ...
  ))
}

score_sl <- function(response,
                     newdata,
                     object,
                     model.score,
                     ...) {
  pr.all <- object$predict(newdata, all.learners = TRUE)
  pr <- object$predict(newdata)
  risk.all <- apply(pr.all, 2, function(x) model.score(x, response))
  risk <- cbind(rbind(model.score(response, pr))[1, ])
  nam <- names(risk)
  if (is.null(nam)) nam <- "score"
  nam <- paste0(nam, ".")
  risk <- cbind(risk, rbind(risk.all))
  colnames(risk)[1] <- "sl"
  nn <- colnames(risk)
  names(risk) <- paste0(nam, nn)
  w <- rbind(c(NA, weights(object$fit)))
  rownames(w) <- "weight"
  risk <- rbind(risk, w)
  res <- c()
  for (i in seq_len(nrow(risk))) {
    x <- risk[i, ]
    names(x) <- paste0(rownames(risk)[i], ".", colnames(risk), sep="")
    res <- c(res, x)
  }
  return(res)
}

#' Cross-validation for [learner_sl]
#' @description Cross-validation estimation of the generalization error of the
#'   super learner and each of the separate models in the ensemble. Both the
#'   chosen model scoring metrics as well as the model weights of the stacked
#'   ensemble.
#' @param object (learner_sl) Instantiated [learner_sl] object.
#' @export
#' @inheritParams cv.default
#' @examples
#' sim1 <- function(n = 5e2) {
#'    x1 <- rnorm(n, sd = 2)
#'    x2 <- rnorm(n)
#'    y <- x1 + cos(x1) + rnorm(n, sd = 0.5**.5)
#'    data.frame(y, x1, x2)
#' }
#' sl <- learner_sl(list(
#'                    "mean" = learner_glm(y ~ 1),
#'                    "glm" = learner_glm(y ~ x1),
#'                    "glm2" = learner_glm(y ~ x1 + x2)
#'                   ))
#' cv(sl, data = sim1(), rep = 2)
cv.learner_sl <- function(object,
                            data,
                            nfolds = 5,
                            rep = 1,
                            model.score = scoring,
                            ...) {
  res <- cv(list("performance"=object),
            data = data,
            nfolds = nfolds, rep = rep,
            model.score = function(...) score_sl(..., model.score = model.score)
            )
  nam <- dimnames(res$cv)
  nam <- nam[[length(nam)]]
  st <- strsplit(nam, "\\.")
  type <- unlist(lapply(st, \(x) x[1])) |> unique() # metrics
  n <- length(nam)/length(type) # number of models
  nam <- gsub(paste0(type[1], "\\."), "", nam[seq_len(n)])

  idx <- 1:n
  cvs <- c()
  for (i in seq_along(type)) {
    score <- res$cv[, , , idx + (i-1)*n, drop=FALSE]
    cvs <- abind::abind(cvs, score, along=3)
  }
  dimnames(cvs)[[4]] <- nam
  dimnames(cvs)[[3]] <- type
  cvs <- aperm(cvs, c(1, 2, 4, 3))
  res$names <- nam
  res$cv <- cvs
  res$call <- NULL
  class(res) <- c("cross_validated.learner_sl", "cross_validated")
  return(res)
}

#' @export
print.cross_validated.learner_sl <- function(x, digits=5, ...) {
  res <- round(summary.cross_validated(x)*1e5, digits=0) / 1e5
  cat("\n", x$fold, "-fold cross-validation", sep="")
  if (x$rep > 1) cat(" with ", x$rep, " repetitions", sep="")
  cat("\n")
  p <- dim(res)[3]
  for (i in seq_len(p)) {
    cli::cli_h3(dimnames(res)[[3]][i])
    print(res[, , i], na.print="-")
  }
}

#' @title R6 class for prediction models
#' @description Interface for statistical and machine learning models to be used
#' for nuisance model estimation in targeted learning.
#'
#' The following list provides an overview of constructors for many commonly
#' used models.
#'
#' Regression and classification: [learner_glm], [learner_gam], [learner_grf],
#' [learner_hal], [learner_glmnet_cv], [learner_svm], [learner_xgboost],
#' [learner_mars] \cr
#' Regression: [learner_isoreg] \cr
#' Classification: [learner_naivebayes] \cr
#' Ensemble (super learner): [learner_sl]
#' @param data data.frame
#' @author Klaus Kähler Holst, Benedikt Sommer
#' @examples
#' data(iris)
#' rf <- function(formula, ...) {
#'   learner$new(formula,
#'     info = "grf::probability_forest",
#'     estimate = function(x, y, ...) {
#'       grf::probability_forest(X = x, Y = y, ...)
#'     },
#'     predict = function(object, newdata) {
#'       predict(object, newdata)$predictions
#'     },
#'     estimate.args = list(...)
#'   )
#' }
#'
#' args <- expand.list(
#'   num.trees = c(100, 200), mtry = 1:3,
#'   formula = c(Species ~ ., Species ~ Sepal.Length + Sepal.Width)
#' )
#' models <- lapply(args, function(par) do.call(rf, par))
#'
#' x <- models[[1]]$clone()
#' x$estimate(iris)
#' predict(x, newdata = head(iris))
#'
#' \donttest{
#' # Reduce Ex. timing
#' a <- targeted::cv(models, data = iris)
#' cbind(coef(a), attr(args, "table"))
#' }
#'
#' # defining learner via function with arguments y (response)
#' # and x (design matrix)
#' f1 <- learner$new(
#'   estimate = function(y, x) lm.fit(x = x, y = y),
#'   predict = function(object, newdata) newdata %*% object$coefficients
#' )
#' # defining the learner via arguments formula and data
#' f2 <- learner$new(
#'   estimate = function(formula, data, ...) glm(formula, data, ...)
#' )
#' # generic learner defined from function (predict method derived per default
#' # from stats::predict
#' f3 <- learner$new(
#'   estimate = function(dt, ...) {
#'     lm(y ~ x, data = dt)
#'   }
#' )
#' @export
learner <- R6::R6Class("learner", # nolint
  public = list(
    #' @field info Optional information/name of the model
    info = NULL,
    #' @field formula Formula specifying response and design matrix
    formula = NULL,

    #' @description
    #' Create a new prediction model object
    #' @param formula formula specifying outcome and design matrix
    #' @param estimate function for fitting the model. This must be a function
    #'  with response, 'y', and design matrix, 'x'. Alternatively, a function
    #'  with a formula and data argument. See the examples section.
    #' @param predict prediction function (must be a function of model
    #' object, 'object', and new design matrix, 'newdata')
    #' @param info optional description of the model
    #' @param predict.args optional arguments to prediction function
    #' @param estimate.args optional arguments to estimate function
    #' @param specials optional specials terms (weights, offset,
    #'  id, subset, ...) passed on to [targeted::design]
    #' @param intercept (logical) include intercept in design matrix
    initialize = function(formula = NULL,
                          estimate,
                          predict = stats::predict,
                          predict.args = NULL,
                          estimate.args = NULL,
                          info = NULL,
                          specials = c(),
                          intercept = FALSE
                         ) {
      estimate <- add_dots(estimate)

      private$des.args <- list(specials = specials, intercept = intercept)
      fit_formula <- "formula" %in% formalArgs(estimate)
      fit_data_arg <- "data" %in% formalArgs(estimate)
      private$init.estimate <- estimate
      private$init.predict <- predict

      private$estimate.args <- estimate.args
      no_formula <- is.null(formula)
      if (!no_formula && is.character(formula) || is.function(formula)) {
        no_formula <- TRUE
      }
      if (no_formula) {
        private$fitfun <- function(...) {
          args <- private$update_args(private$estimate.args, ...)
          return(do.call(private$init.estimate, args))
        }
        private$predfun <- function(...) {
          args <- private$update_args(predict.args, ...)
          return(do.call(private$init.predict, args))
        }
      } else {
        if (fit_formula) { # Formula in arguments of estimation procedure
          private$fitfun <- function(data, ...) {
            args <- private$update_args(private$estimate.args, ...)
            args <- c(
              args, list(formula = self$formula, data = data)
            )
            return(do.call(private$init.estimate, args))
          }
        } else {
          #  Formula automatically processed into design matrix & response
          private$fitfun <- function(data, ...) {
            xx <- do.call(
              targeted::design,
              c(list(formula = self$formula, data = data), private$des.args)
            )
            args <- private$update_args(private$estimate.args, ...)
            args <- c(list(x = xx$x, y = xx$y), args)

            if (length(xx$specials) > 0) {
              args <- c(args, xx[xx$specials])
            }
            return(structure(do.call(private$init.estimate, args),
              design = summary(xx)
            ))
          }
        }
        private$predfun <- function(object, data, ...) {
          if (fit_formula || no_formula) {
            predict_args_call <- private$update_args(predict.args, ...)
            args <- c(list(object, newdata = data), predict_args_call)
          } else {
            args <- list(...)
            des <- update(attr(object, "design"), data)
            for (s in des$specials) {
              if (is.null(args[[s]])) args[[s]] <- des[[s]]
            }
            predict_args_call <- predict.args
            predict_args_call[names(args)] <- args

            args <- c(list(object,
              newdata = model.matrix(des)
            ), predict_args_call)
          }
          return(do.call(private$init.predict, args))
        }
      }
      self$formula <- formula
      self$info <- info
      private$init <- list(
        estimate.args = estimate.args,
        predict.args = predict.args,
        estimate = estimate,
        predict = predict,
        specials = specials,
        intercept = intercept
      )
    },

    #' @description
    #' Estimation method
    #' @param ... Additional arguments to estimation method
    #' @param store Logical determining if estimated model should be
    #'   stored inside the class.
    estimate = function(data, ..., store = TRUE) {
      res <- private$fitfun(data, ...)
      if (store) private$fitted <- res
      return(invisible(res))
    },

    #' @description
    #' Prediction method
    #' @param newdata data.frame
    #' @param ... Additional arguments to prediction method
    #' @param object Optional model fit object
    predict = function(newdata, ..., object = NULL) {
      if (is.null(object)) object <- private$fitted
      if (is.null(object)) stop("Provide estimated model object")
      return(private$predfun(object, newdata, ...))
    },

    #' @description
    #' Update formula
    #' @param formula formula or character which defines the new response
    update = function(formula) {
      if (is.character(formula)) {
        if (grepl("~", formula)) {
          formula <- as.formula(formula)
        } else {
          formula <- reformulate(as.character(self$formula)[3], formula)
        }
      }
      self$formula <- formula
      environment(private$fitfun)$formula <- formula
      environment(private$fitfun)$self <- self
      return(invisible(formula))
    },

    #' @description
    #' Print method
    print = function() {
      learner_print(self, private)
      return(invisible())
    },

    #' @description
    #' Summary method to provide more extensive information than
    #' [learner$print()][learner].
    #' @return summarized_learner object, which is a list with the following
    #' elements:
    #' \describe{
    #'  \item{info}{description of the learner}
    #'  \item{formula}{formula specifying outcome and design matrix}
    #'  \item{estimate}{function for fitting the model}
    #'  \item{estimate.args}{arguments to estimate function}
    #'  \item{predict}{function for making predictions from fitted model}
    #'  \item{predict.args}{arguments to predict function}
    #'  \item{specials}{provided special terms}
    #'  \item{intercept}{include intercept in design matrix}
    #' }
    #' @examples
    #' lr <- learner_glm(y ~ x, family = "nb")
    #' lr$summary()
    #'
    #' lr_sum <- lr$summary() # store returned summary in new object
    #' names(lr_sum)
    #' print(lr_sum)
    summary = function() {
      obj <- structure(
        c(list(formula = self$formula, info = self$info), private$init),
        class = "summarized_learner"
      )
      return(obj)
    },


    #' @description
    #' Extract response from data
    #' @param eval when FALSE return the untransformed outcome
    #' (i.e., return 'a' if formula defined as I(a==1) ~ ...)
    #' @param ... additional arguments to [targeted::design]
    response = function(data, eval = TRUE, ...) {
      if (eval) {
        return(self$design(data = data, ...)$y)
      }
      if (is.null(self$formula)) return(NULL)
      newf <- update(self$formula, ~1)
      return(data[, all.vars(newf), drop = TRUE])
    },

    #' @description
    #' Generate [targeted::design] object (design matrix and response) from data
    #' @param ... additional arguments to [targeted::design]
    design = function(data, ...) {
      args <- c(private$des.args, list(data = data))
      args[...names()] <- list(...)
      return(do.call(design, c(list(self$formula), args)))
    },

    #' @description
    #' Get options
    #' @param arg name of option to get value of
    opt = function(arg) {
      return(private$estimate.args[[arg]])
    }
  ),
  active = list(
    #' @field fit Active binding returning estimated model object
    fit = function() private$fitted
  ),
  private = list(
    # @field des.args Arguments for targeted::design
    des.args = NULL,
    # @field estimate.args Arguments for estimate method
    estimate.args = NULL,
    # @field init.estimate Original estimate method supplied at initialization
    init.estimate = NULL,
    # @field init.predict Original predict method supplied at initialization
    init.predict = NULL,
    # @field predfun Prediction method
    predfun = NULL,
    # @field fitfun Estimation method
    fitfun = NULL,
    # @field fitted Fitted model object
    fitted = NULL,
    # @field init Information on the initialized model
    init = NULL,
    # When x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
    # each field, with the name and value.
    deep_clone = function(name, value) {
      if (name == "fitfun") {
        env <- list2env(
          as.list.environment(environment(value),
            all.names = TRUE
          ),
          parent = globalenv()
        )
        environment(value) <- env
        return(value)
      } else {
        # For everything else, just return it. This results in a shallow
        # copy of s3.
        return(value)
      }
    },
    # #' Utility to update list of arguments with ellipsis
    # #' @param args list or NULL
    update_args = function(args, ...) {
      if (is.null(args)) args <- list() # because predict.args = NULL by default
      dots <- list(...)

      # update args for unnamed list of arguments
      if (length(dots) > 0 && is.null(names(dots))) {
        args <- c(args, dots)
      } else {
        args[names(dots)] <- dots
      }
      return(args)
    }
   )
)

#' @export
estimate.learner <- function(x, ...) {
  return(x$estimate(...))
}

#' @export
predict.learner <- function(object, ...) {
  return(object$predict(...))
}

format_fit_predict_args <- function(args) {
  if (length(args) == 0) return(" ")
  funs <- c(is.numeric, is.character, is.integer, is.logical, is.null)

  # print family attribute of family objects instead of printing only that the
  # argument is of class <family>
  mask <- unlist(lapply(args, \(x) inherits(x, "family")))
  vals <- lapply(args[mask], \(x) x$family)
  args[mask] <- vals

  mask <- unlist(lapply(args, \(x) any(sapply(funs, \(f) f(x)))))
  args_class <- paste0("<", lapply(args, \(x) class(x)[[1]]), ">")
  args[!mask] <- args_class[!mask]

  return(paste0(names(args), "=", args, collapse =", "))
}


learner_print <- function(self, private) {
  cat_ruler(" learner object ", 10)

  if (!is.null(self$info)) {
    cat(self$info, "\n\n")
  }

  cat(
    "Estimate arguments:",
    format_fit_predict_args(private$init$estimate.args),
    "\nPredict arguments:",
    format_fit_predict_args(private$init$predict.args),
    "\nFormula:",
    capture.output(print(self$formula)),
    "\n"
  )

  if (!is.null(private$fitted)) {
    cat_ruler("\u2500", 18)
    fit <- self$fit
    if (!is.null(fit$call)) fit$call <- substitute()
    cat(capture.output(print(fit)), sep ="\n")
  }

  return(invisible())
}

#' @export
print.summarized_learner <- function(x, ...) {
  cat_ruler(" learner object ", 10)

  if (!is.null(x$info)) {
    cat(x$info, "\n\n")
  }

  cat(
    "formula:",
    capture.output(print(x$formula)),
    "\nestimate:", paste0(names(formals(x$estimate)), collapse = ", "),
    "\nestimate.args:",
    format_fit_predict_args(x$estimate.args),
    "\npredict:", paste0(names(formals(x$predict)), collapse = ", "),
    "\npredict.args:",
    format_fit_predict_args(x$predict.args),
    "\nspecials:", paste(x$specials, collapse = ", ")
  )
}

#' @title R6 class for prediction models
#' @description Replaced by [learner]
#' @export
ml_model <- R6Class("ml_model",
  inherit = learner,
  public = list(
    #' @description Create a new prediction model object
    #' @param ... deprecated
    initialize = function(...) {
      rlang::warn(paste0(
        "targeted::ml_model is deprecated and will ",
        "be removed in targeted v0.7.0. Use targeted::learner instead.")
      )
      super$initialize(...)
    }
  )
)

#' @export
estimate.ml_model <- function(x, ...) {
  rlang::warn(paste0(
        "targeted::ml_model is deprecated and will ",
        "be removed in targeted v0.7.0. Use targeted::learner instead.")
  )
  return(x$estimate(...))
}

#' @export
predict.ml_model <- function(object, ...) {
  rlang::warn(paste0(
        "targeted::ml_model is deprecated and will ",
        "be removed in targeted v0.7.0. Use targeted::learner instead.")
  )
  return(object$predict(...))
}

#' ML model
#'
#' Wrapper for ml_model
#' @export
#' @param formula formula
#' @param model model (sl, rf, pf, glm, ...)
#' @param ... additional arguments to model object
ML <- function(formula, model="glm", ...) {
  stop(
    "targeted::ML has been removed in targeted 0.6. ",
    "Please use the targeted::learner_ functions instead."
  )
}

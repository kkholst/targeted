#' @title R6 class for prediction models
#' @description Provides standardized estimation and prediction methods
#' @param data data.frame
#' @author Klaus Kähler Holst
#' @aliases ml_model predictor
#' predictor_glm predictor_gam predictor_glmnet
#' predictor_grf predictor_grf_binary
#' predictor_xgboost predictor_xgboost_multiclass
#' predictor_xgboost_count predictor_xgboost_cox
#' predictor_xgboost_binary
#' predictor_hal predictor_isoreg
#' @seealso predictor_sl
#' @examples
#' data(iris)
#' rf <- function(formula, ...) {
#'   ml_model$new(formula,
#'     info = "grf::probability_forest",
#'     estimate = function(x, y, ...) {
#'       grf::probability_forest(X = x, Y = y, ...)
#'     },
#'     predict = function(object, newdata) {
#'       predict(object, newdata)$predictions
#'     },
#'     ...
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
#' ff <- ml_model$new(
#'   estimate = function(y, x) lm.fit(x = x, y = y),
#'   predict = function(object, newdata) newdata %*% object$coefficients
#' )
#' ## tmp <- ff$estimate(y, x=x)
#' ## ff$predict(x)
#' @export
ml_model <- R6::R6Class("ml_model", # nolint
  public = list(
    #' @field info Optional information/name of the model
    info = NULL,
    #' @field formals List with formal arguments of estimation and
    #'  prediction functions
    formals = NULL,
    #' @field formula Formula specifying response and design matrix
    formula = NULL,
    #' @field args optional arguments to fitting function specified during
    #' initialization
    args = NULL,
    #' @field description optional description field
    description = NULL,

    #' @description
    #' Create a new prediction model object
    #' @param formula formula specifying outcome and design matrix
    #' @param estimate function for fitting the model (must be a function
    #'  with response, 'y', and design matrix, 'x'. Alternatively, a function
    #'  with a single 'formula' argument)
    #' @param predict prediction function (must be a function of model
    #' object, 'object', and new design matrix, 'newdata')
    #' @param info optional description of the model
    #' @param predict.args optional arguments to prediction function
    #' @param specials optional specials terms (weights, offset,
    #'  id, subset, ...) passed on to [targeted::design]
    #' @param response.arg name of response argument
    #' @param intercept (logical) include intercept in design matrix
    #' @param x.arg name of design matrix argument
    #' @param ... optional arguments to fitting function
    initialize = function(formula = NULL,
                          estimate,
                          predict = stats::predict,
                          predict.args = NULL,
                          info = NULL,
                          specials = c(),
                          response.arg = "y",
                          intercept = FALSE,
                          x.arg = "x",
                          ...) {
      dots <- list(...)
      if (!is.null(dots$fit)) { ## Backward compatibility
        .Deprecated("Use argument 'estimate' instead of 'fit'")
        if (missing(estimate)) {
          estimate <- dots$fit
        }
        dots$fit <- NULL
      }
      estimate <- add_dots(estimate)

      des.args <- list(specials = specials, intercept = intercept)
      fit_formula <- "formula" %in% formalArgs(estimate)
      fit_response_arg <- response.arg %in% formalArgs(estimate)
      fit_x_arg <- x.arg %in% formalArgs(estimate)
      fit_data_arg <- "data" %in% formalArgs(estimate)
      private$init.estimate <- estimate
      private$init.predict <- predict

      self$args <- dots
      no_formula <- is.null(formula)
      if (!no_formula && is.character(formula) || is.function(formula)) {
        no_formula <- TRUE
      }
      if (no_formula) {
        private$fitfun <- function(...) {
          args <- c(list(...), dots)
          return(do.call(private$init.estimate, args))
        }
        private$predfun <- function(...) {
          args <- c(list(...), predict.args)
          return(do.call(private$init.predict, args))
        }
      } else {
        if (fit_formula) { # Formula in arguments of estimation procedure
          private$fitfun <- function(data, ...) {
            args <- c(
              self$args, list(formula = self$formula, data = data),
              list(...)
            )
            return(do.call(private$init.estimate, args))
          }
        } else {
          #  Formula automatically processed into design matrix & response
          private$fitfun <- function(data, ...) {
            xx <- do.call(
              targeted::design,
              c(list(formula = self$formula, data = data), des.args)
            )
            args <- c(list(xx$x), list(...), self$args)
            if (fit_x_arg) {
              names(args)[1] <- x.arg
            } else {
              if (fit_data_arg) names(args)[1] <- "data"
            }
            if (fit_response_arg) {
              args[response.arg] <- list(xx$y)
            }
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
            args <- c(list(object, newdata = data), predict.args, list(...))
          } else {
            args <- list(...)
            des <- update(attr(object, "design"), data)
            for (s in des$specials) {
              if (is.null(args[[s]])) args[[s]] <- des[[s]]
            }
            args <- c(list(object,
              newdata = model.matrix(des)
            ), predict.args, args)
          }
          return(do.call(private$init.predict, args))
        }
      }
      self$formula <- formula
      self$info <- info
      self$formals <- list(
        estimate = formals(estimate),
        predict = formals(predict)
      )
      private$call <- list( # nolint
        estimate = substitute(estimate),
        predict = substitute(predict),
        argslist = substitute(dots),
        predict.args = substitute(predict.args)
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
      cat(
        "Prediction Model (class ml_model)",
        "\n_________________________________\n\n"
      )
      if (!is.null(self$info)) {
        cat(self$info, "\n\n")
      }

      if (!is.null(self$description)) {
        cat(self$description)
      } else {
        cat("Arguments:\n")
        argslist <- gsub("^list\\(", "",
          deparse(private$call$argslist),
          perl = TRUE
        )
        argslist <- gsub("\\)$", "", argslist)
        argslist <- strsplit(
          paste(argslist, collapse = ""),
          ","
        ) |>
          _[[1]] |>
          lapply(lava::trim) |>
          unlist()
        cat(
          paste0("\t", paste(argslist, collapse = "\n\t")),
          "\n",
          sep = ""
        )
      }

      ## if (!is.null(self$formula)) {
      ##   cat("Model:\n",
      ##     "\t", deparse1(self$formula), "\n",
      ##     sep = ""
      ##   )
      ## }
      ## cat("\`estimate` method:`\n",
      ##     "\tfunction(", paste(names(self$formals[[1]]),
      ##                          collapse=", "), ")\n", sep="")
      ## cat("`predict` method:\n",
      ##   "\tfunction(", paste(names(self$formals[[2]]),
      ##     collapse = ", "
      ##   ), ")\n",
      ## sep = ""
      ## )
      if (!is.null(self$fit)) {
        cat("\n_________________________________\n\n")
        print(self$fit)
      }
      return(invisible())
    },

    #' @description
    #' Extract response from data
    #' @param eval when FALSE return the untransformed outcome
    #' (i.e., return 'a' if formula defined as I(a==1) ~ ...)
    #' @param ... additional arguments to [targeted::design]
    response = function(data, eval = TRUE, ...) {
      if (is.null(self$formula)) {
        if (!is.null(self$responsevar)) {
          return(data[, self$responsevar, drop = TRUE])
        }
        return(NULL)
      }
      newf <- update(self$formula, ~1)
      if (!eval) {
        return(data[, all.vars(newf), drop = TRUE])
      }
      return(design(newf, data = data, ...)$y)
    },

    #' @description
    #' Extract design matrix (features) from data
    #' @param ... additional arguments to [targeted::design]
    design = function(data, ...) {
      return(design(self$formula, data = data, ...)$x)
    },

    #' @description
    #' Get options
    #' @param arg name of option to get value of
    opt = function(arg) {
      return(self$args[[arg]])
    }
  ),
  active = list(
    #' @field fit Active binding returning estimated model object
    fit = function() private$fitted
  ),
  private = list(
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
    # @field call Information on the initialized model
    call = NULL,
    # @field optional field containing name of response variable
    responsevar = NULL,
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
    }
  )
)

#' @export
estimate.ml_model <- function(x, ...) {
  return(x$estimate(...))
}

#' @export
predict.ml_model <- function(object, ...) {
  return(object$predict(...))
}

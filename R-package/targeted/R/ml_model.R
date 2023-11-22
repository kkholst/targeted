##' R6 class for prediction models
##'
##' Provides standardized estimation and prediction methods
##' @author Klaus Kähler Holst
##' @examples
##' data(iris)
##' rf <- function(formula, ...)
##' ml_model$new(formula, info="grf::probability_forest",
##'              estimate=function(x,y, ...) grf::probability_forest(X=x, Y=y, ...),
##'              predict=function(object, newdata) predict(object, newdata)$predictions, ...)
##'
##' args <- expand.list(num.trees=c(100,200), mtry=1:3,
##'                    formula=c(Species ~ ., Species ~ Sepal.Length + Sepal.Width))
##' models <- lapply(args, function(par) do.call(rf, par))
##'
##' x <- models[[1]]$clone()
##' x$estimate(iris)
##' predict(x, newdata=head(iris))
##'
##' \donttest{ # Reduce Ex. timing
##' a <- targeted::cv(models, data=iris)
##' cbind(coef(a), attr(args, "table"))
##' }
##'
##' ff <- ml_model$new(estimate=function(y,x) lm.fit(x=x, y=y),
##'                    predict=function(object, newdata) newdata%*%object$coefficients)
##' ## tmp <- ff$estimate(y, x=x)
##' ## ff$predict(x)
##' @export
ml_model <- R6::R6Class("ml_model",
    public = list(
      ##' @field info Optional information/name of the model
      info = NULL,
      ##' @field formals List with formal arguments of estimation and prediction functions
      formals = NULL,
      ##' @field formula Formula specifying response and design matrix
      formula = NULL,
      ##' @field args additional arguments specified during initialization
      args = NULL,

     ##' @description
     ##' Create a new prediction model object
     ##' @param formula formula specifying outcome and design matrix
     ##' @param estimate function for fitting the model (must be a function response, 'y',
     ##'   and design matrix, 'x'. Alternatively, a function with a single 'formula' argument)
     ##' @param predict prediction function (must be a function of model object, 'object',
     ##' and new design matrix, 'newdata')
     ##' @param info optional description of the model
     ##' @param predict.args optional arguments to prediction function
     ##' @param specials optional additional terms (weights, offset, id, subset, ...)
     ##'   passed to 'estimate'
     ##' @param response.arg name of response argument
     ##' @param x.arg name of design matrix argument
     ##' @param ... optional arguments to fitting function
      initialize = function(formula=NULL,
                           estimate,
                           predict=predict,
                           predict.args=NULL,
                           info=NULL, specials,
                           response.arg="y",
                           x.arg="x",
                           ...) {
      if (!("..."%in%formalArgs(estimate))) {
        formals(estimate) <- c(formals(estimate), alist(...=))
      }
      des.args <- lapply(substitute(specials), function(x) x)[-1]
      fit_formula <- "formula"%in%formalArgs(estimate)
      fit_response_arg <- response.arg %in% formalArgs(estimate)
      fit_x_arg <- x.arg%in%formalArgs(estimate)
      fit_data_arg <- "data" %in% formalArgs(estimate)
      private$init.estimate <- estimate
      private$init.predict <- predict
      ## if (!fit_x_arg && !("data"%in%formalArgs(estimate)))
      ##   stop("Estimation method must have an argument 'x' or 'data'")

      dots <- list(...)
      self$args <- dots
      no_formula <- is.null(formula)
      if (no_formula) {
        private$fitfun <- function(...) {
          args <-  c(list(...), dots)
          do.call(private$init.estimate, args)
        }
        private$predfun <- function(...) {
          args <-  c(list(...), predict.args)
          do.call(private$init.predict, args)
        }
      } else {
        if (fit_formula) {  ## Formula in arguments of estimation procedure
          private$fitfun <- function(data, ...) {
            args <- c(self$args, list(formula=self$formula, data=data), list(...))
            return(do.call(private$init.estimate, args))
          }
        } else {  ##  Formula automatically processed into design matrix & response
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
            if (length(xx$specials)>0)
              args <- c(args, xx[xx$specials])
            return(structure(do.call(private$init.estimate, args), design=summary(xx)))
          }
        }
        private$predfun <- function(object, data, ...) {
          if (fit_formula || no_formula) {
            args <-  c(list(object, newdata=data), predict.args, list(...))
          } else {
            x <- model.matrix(update(attr(object, "design"), data))
            args <-  c(list(object, newdata=x), predict.args, list(...))
          }
          return(do.call(private$init.predict, args))
        }
      }
      self$formula <- formula
      self$info <- info
      self$formals <- list(estimate=formals(estimate), predict=formals(predict))
      private$call <- list(estimate=substitute(estimate),
                           predict=substitute(predict),
                           dots=substitute(list(...)),
                           predict.args=substitute(predict.args))
     },

     ##' @description
     ##' Estimation method
     ##' @param data data.frame
     ##' @param ... Additional arguments to estimation method
     ##' @param store Logical determining if estimated model should be stored inside the class.
     estimate = function(data, ..., store=TRUE) {
       res <- private$fitfun(data, ...)
       if (store) private$fitted <- res
       invisible(res)
     },

     ##' @description
     ##' Prediction method
     ##' @param newdata data.frame
     ##' @param ... Additional arguments to prediction method
     ##' @param object Optional model fit object
     predict = function(newdata, ..., object=NULL) {
       if (is.null(object)) object <- private$fitted
       if (is.null(object)) stop("Provide estimated model object")
       private$predfun(object, newdata, ...)
     },

     ##' @description
     ##' Update formula
     ##' @param formula formula or character which defines the new response
     ##' @param ... Additional arguments to lower level functions
     update = function(formula, ...) {
       if (is.character(formula)) {
         if (grepl("~", formula))
           formula <- as.formula(formula)
         else
           formula <- reformulate(as.character(self$formula)[3], formula)
       }
       self$formula <- formula
       environment(private$fitfun)$formula <- formula
     },

     ##' @description
     ##' Print method
     ##' @param ... Additional arguments to lower level functions
     print = function(...) {
       cat("Prediction Model (class ml_model)",
           "\n_________________________________\n\n")
       if (!is.null(self$info))
         cat(self$info, "\n\n")
       cat("Arguments:\n")
       print(unlist(private$call$dots))
       if (!is.null(self$formula))
         cat("Model:\n",
             "\t", deparse1(self$formula), "\n", sep="")
       cat("Estimate function:\n",
           "\tfunction(",paste(names(self$formals[[1]]),
                               collapse=", "), ")\n", sep="")
       cat("Prediction:\n",
           "\tfunction(",paste(names(self$formals[[2]]),
                               collapse=", "), ")\n", sep="")
       #cat("\n\nMethods: estimate, predict, fit, update, response, design, clone\n")
     },

     ##' @description
     ##' Extract response from data
     ##' @param data data.frame
     ##' @param ... additional arguments to 'design'
     response = function(data, ...) {
       if (is.null(self$formula)) return(NULL)
       design(update(self$formula, ~ 1), data=data, ...)$y
     },

     ##' @description
     ##' Extract design matrix (features) from data
     ##' @param data data.frame
     ##' @param ... additional arguments to 'design'
     design = function(data, ...) {
       design(self$formula, data=data, ...)$x
     },

     ##' @description
     ##' Get options
     ##' @param arg name of option to get value of
     ##' @param ... additional arguments to lower level functions
     opt = function(arg, ...) {
       return(self$args[[arg]])
     }

   ),

   active = list(
     ##' @field fit Active binding returning estimated model object
     fit = function() private$fitted
   ),

   private = list(
     ## @field init.estimate Original estimate method supplied at initialization
     init.estimate = NULL,
     ## @field init.predict Original predict method supplied at initialization
     init.predict = NULL,
     ## @field predfun Prediction method
     predfun = NULL,
     ## @field fitfun Estimation method
     fitfun = NULL,
     ## @field fitted Fitted model object
     fitted = NULL,
     ## @field call Information on the initialized model
     call = NULL,
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
         ## For everything else, just return it. This results in a shallow
         ## copy of s3.
         return(value)
       }
     }
   )
)
##
##' @export
estimate.ml_model <- function(x, ...) {
  val <- x$estimate(...)
  val
}

##' @export
predict.ml_model <- function(object, ...) {
  object$predict(...)
}


##' ML model
##'
##' Wrapper for ml_model
##' @export
##' @param formula formula
##' @param ... additional arguments to model object
##' @param model model (sl, rf, pf, glm, ...)
##' @details
##' model 'sl' (SuperLearner::SuperLearner)
##' args: SL.library, cvControl, f<aamily, method
##' example:
##'
##' model 'grf' (grf::regression_forest)
##' args: num.trees, mtry, sample.weights, sample.fraction, min.node.size, ...
##' example:
##'
##' model 'grf.binary' (grf::probability_forest)
##' args: num.trees, mtry, sample.weights, ...
##' example:
##'
##' model 'glm'
##' args: family, weights, offset, ...
##'
##'
ML <- function(formula, model="glm", ...) {
  model <- tolower(model)
  dots <- list(...)
  addargs <- function(..., dots, args = list()) {
      for (p in names(args)) {
          if (!(p %in% names(dots))) dots[p] <- args[[p]]
      }
      c(list(...), dots)
  }

  ## SL / SuperLearner
  if (model == "sl") {
      return(SL(formula, ...))
  }

  ## grf
  grf.bin <- c("grf.binary", "pf", "probability_forest")
  if (model%in%c("grf", "rf", "regression_forest",
                 grf.bin)) {
    args <- list(
      num.trees = 2000,
      min.node.size = 5,
      alpha = 0.05,
      sample.fraction = 0.5,
      num.threads=1
    )
    obj <- "grf::regression_forest"
    est <- function(x, y)
        grf::regression_forest(X = x, Y = y, ...)
    if (model %in% grf.bin) {
        obj <- "grf::probability_forest"
        est <- function(x, y)
            grf::probability_forest(X = x, Y = y, ...)
    }
    ml_args <- addargs(formula,
        info = obj,
        estimate = est,
        predict = function(object, newdata, ...) {
          predict(object, newdata, ...)$predictions
        },
        dots = dots,
        args = args
        )
    return(do.call(ml_model$new, ml_args))
  }

  ## xgboost
  if (model %in% c("xgboost", "xgb", "xgboost.multiclass", "xgboost.binary", "xgboost.count", "xgboost.survival")) {
    obj <- switch(model,
        xgboost.multiclass = "multi:softprob",
        xgboost.binary = "reg:logistic",
        xgboost.survival = "survival:cox",
        xgboost.count = "count:poisson",
        "reg:squarederror"
        )
    args <- list(
        max_depth = 2,
        eta = 1,
        nrounds = 2,
        subsample = 1,
        lambda = 1,
        objective = obj,
        verbose = 0
    )
    pred <- function(object, newdata, ...) {
        d <- xgboost::xgb.DMatrix(newdata)
        predict(object, d, ...)
    }
    if (obj == "multi:softprob") {
        pred <- function(object, newdata, ...) {
            d <- xgboost::xgb.DMatrix(newdata)
            val <- predict(object, d, ...)
            matrix(val, nrow = NROW(d), byrow=TRUE)
        }
    }
    ml_args <- addargs(formula,
                       info = paste0("xgboost (", obj, ")"),
                       estimate = function(x, y) {
                         d <- xgboost::xgb.DMatrix(x, label = y)
                         xgboost::xgboost(d, ...)
                       },
                       predict = pred,
                       dots = dots,
                       args = args
                       )
    return(do.call(ml_model$new, ml_args))
  }

  ## GAM
  if (model %in% c("mgcv", "gam")) {
    args <- list(
        family = gaussian(),
        select = FALSE,
        gamma = 1
    )
    ml_args <- addargs(formula,
        info = paste0("mgcv::gam"),
        estimate = function(formula, data, ...) {
            mgcv::gam(formula, data = data, ...)
        },
        predict = function(object, newdata) {
            stats::predict(object, newdata = newdata, type = "response")
        },
        dots = dots,
        args = args
        )
    return(do.call(ml_model$new, ml_args))
  }

  ## glm, default
  m <- ml_model$new(formula, info = "glm", ...,
        estimate = function(formula, data, ...)
          stats::glm(formula, data=data, ...),
        predict = function(object, newdata)
          stats::predict(object, newdata=newdata, type="response")
        )
  return(m)

}

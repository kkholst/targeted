##' R6 class for prediction models
##'
##' Provides standardized estimation and prediction methods
##' @author Klaus Kähler Holst
##' @export
ml_model <- R6::R6Class("ml_model",
    public = list(
     ##' @field info Optional information/name of the model
     info = NULL,

     ##' @description
     ##' Create a new prediction model object
     ##' @param formula formula specifying outcome and design matrix
     ##' @param fit function for fitting the model (must be a function response, 'y',
     ##'   and design matrix, 'x'. Alternatively, a function with a single 'formula' argument)
     ##' @param pred prediction function (must be a function of model object, 'fit',
     ##' and new design matrix, 'newdata')
     ##' @param info optional description of the model
     ##' @param pred.args optional arguments to prediction function
     ##' @param specials optional additional terms (weights, offset, id, subset, ...)
     ##'   passed to 'fit'
     ##' @param ... optional arguments to fitting function
     initialize = function(formula=NULL, fit,
                           pred=predict, pred.args=NULL,
                           info=NULL, specials, ...) {
      if (!("..."%in%formalArgs(fit))) {
        formals(fit) <- c(formals(fit), alist(...=))
      }
      des.args <- lapply(substitute(specials), function(x) x)[-1]

      fit_formula <- "formula"%in%formalArgs(fit)
      dots <- list(...)
      no_formula <- is.null(formula)
      if (no_formula) {
        fit_formula <- "formula"%in%formalArgs(fit)
        private$fitfun <- function(...) {
          args <-  c(list(...), dots)
          do.call(fit, args)
        }
        private$predfun <- function(...) {
          args <-  c(list(...), pred.args)
          do.call(pred, args)
        }
      } else {
        private$fitfun <- function(data, ...) {
          if (fit_formula || no_formula) {
            if (no_formula) {
              args <- c(dots, list(data=data), list(...))
            } else {
              args <- c(dots, list(formula=formula, data=data), list(...))
            }
            return(do.call(fit, args))
          } else {
            xx <- do.call(design, c(list(formula=formula, data=data), des.args))
            ##xx <- design(formula, data)
            args <- c(dots, list(y=xx$y, x=xx$x), list(...))
            if (length(xx$specials)>0)
              args <- c(args, xx[xx$specials])
            return(structure(do.call(fit, args), design=summary(xx)))
          }
        }
        private$predfun <- function(fit, data, ...) {
          if (fit_formula || no_formula) {
            args <-  c(list(fit, newdata=data), pred.args, list(...))
          } else {
            x <- model.matrix(update(attr(fit, "design"), data))
            args <-  c(list(fit, newdata=x), pred.args, list(...))
          }
          return(do.call(pred, args))
        }
      }
      private$formula <- formula
      self$info <- info
      private$formals <- list(formals(fit), formals(pred))
      private$call <- list(fit=substitute(fit),
                           pred=substitute(pred),
                           dots=substitute(dots),
                           pred.args=substitute(pred.args))
     },

     ##' @description
     ##' Estimation method
     ##' @param data data.frame
     ##' @param ... Additional arguments to estimation method
     ##' @param store Logical determining if estimated model should be stored inside the class.
     estimate = function(data, ..., store=TRUE) {
       res <- private$fitfun(data, ...)
       if (store) private$fitted <- res
       return(res)
     },

     ##' @description
     ##' Prediction method
     ##' @param newdata data.frame
     ##' @param ... Additional arguments to prediction method
     ##' @param fit Optional model fit object
     predict = function(newdata, ..., fit=NULL) {
       if (is.null(fit)) fit <- private$fitted
       if (is.null(fit)) stop("Provide estimated model object")
       private$predfun(fit, newdata, ...)
     },

     ##' @description
     ##' Update formula
     ##' @param formula formula
     ##' @param ... Additional arguments to lower level functions
     update = function(formula, ...) {
       ## environment(formula) <- baseenv()
       private$formula <- formula
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
       if (!is.null(private$formula))
         cat("Data:\n",
             "\t", deparse1(private$formula), "\n", sep="")
       cat("Model:\n",
           "\tfunction(",paste(names(private$formals[[1]]),
                               collapse=", "), ")\n", sep="")
       cat("Prediction:\n",
           "\tfunction(",paste(names(private$formals[[2]]),
                               collapse=", "), ")\n", sep="")
       #cat("\n\nMethods: estimate, predict, fit, update, response, design, clone\n")
     },

     ##' @description
     ##' Extract response from data
     ##' @param data data.frame
     response = function(data) {
       design(update(private$formula, ~ 1), data)$y
     },

     ##' @description
     ##' Extract design matrix (features) from data
     ##' @param data data.frame
     design = function(data) {
       design(private$formula, data)$x
     }

   ),

   active = list(
     ##' @field fit Active binding returning estimated model object
     fit = function() private$fitted
   ),

   private = list(
     ## @field formula Formula specifying response and design matrix
     formula = NULL,
     ## @field predfun Prediction method
     predfun = NULL,
     ## @field fitfun Estimation method
     fitfun = NULL,
     ## @field fitted Fitted model object
     fitted = NULL,
     ## @field call Information on the initialized model
     call = NULL,
     ## @field formals Formal arguments of estimation and prediction methods
     formals = NULL
   )
)

##' @export
estimate.ml_model <- function(x, ...) {
  x$estimate(...)
}

##' @export
predict.ml_model <- function(object, ...) {
  object$predict(...)
}

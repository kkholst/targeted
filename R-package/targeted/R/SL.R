
##' SuperLearner wrapper for ml_model
##'
##' @title SuperLearner wrapper for ml_model
##' @param formula Model design
##' @param ... Additional arguments for SuperLearner::SuperLearner
##' @param SL.library character vector of prediction algorithms
##' @param binomial boolean specifying binomial or gaussian family (default FALSE)
##' @param data Optional data.frame
##' @return ml_model object
##' @author Klaus Kähler Holst
##' @export
SL <- function(formula=~., ...,
               SL.library=c("SL.mean", "SL.glm"),
               binomial=FALSE,
               data=NULL) {
  dots <- list(...)
  if (!requireNamespace("SuperLearner")) {
      stop("Package 'SuperLearner' required.")
  }
  if (length(attr(getoutcome(formula), "x")) == 0) {
    SL.library <- "SL.mean"
  }
  m <- ml_model$new(formula, info="SuperLearner",
               fit=function(x,y) {
                 Y <- as.numeric(y)
                 X <- as.data.frame(x)
                 args <- c(list(Y=Y, X=X, SL.library=SL.library), dots)
                 if (binomial)
                   args <- c(args, list(family=binomial()))
                 res <- do.call(SuperLearner::SuperLearner, args)
                 res$call <- quote(SuperLearner(...))
                 if (binomial)
                   res$call <- quote(SuperLearner::SuperLearner(..., family=binomial()))
                 res
               },
               pred=function(object,newdata) {
                 pr <- predict(object,newdata=newdata)$pred
                 if (binomial)
                   pr <- cbind((1-pr), pr)
                 return(pr)
               })
  if (!is.null(data))
    m$estimate(data)
  return(m)
}


##' @export
GLM <- function(formula, family=gaussian, ..., data=NULL) {
  m <- ml_model$new(formula, info="GLM",
                    fit=function(formula, data)
                      stats::glm(formula, data=data, family=family, ...),
                    pred = function(object, newdata)
                      stats::predict(object, newdata=newdata, type="response")
                    )
  if (!is.null(data))
    m$estimate(data)
  return(m)
}

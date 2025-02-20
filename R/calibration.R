#' Calibration for multiclassication methods
#'
#' @title Calibration (training)
#' @param pr matrix with probabilities for each class
#' @param cl class variable
#' @param weights counts
#' @param threshold do not calibrate if less then 'threshold' events
#' @param method either 'isotonic' (pava), 'logistic',
#' 'mspline' (monotone spline), 'bin' (local constant)
#' @param breaks optional number of bins (only for method 'bin')
#' @param df degrees of freedom (only for spline methods)
#' @param ... additional arguments to lower level functions
#' @author Klaus K. Holst
#' @details ...
#' @aliases calibration calibrate
#' @return An object of class '\code{calibration}' is returned.
#' See \code{\link{calibration-class}}
#' for more details about this class and its generic functions.
#' @export
#' @examples
#' sim1 <- function(n, beta=c(-3, rep(.5,10)), rho=.5) {
#'  p <- length(beta)-1
#'  xx <- lava::rmvn0(n,sigma=diag(nrow=p)*(1-rho)+rho)
#'  y <- rbinom(n, 1, lava::expit(cbind(1,xx)%*%beta))
#'  d <- data.frame(y=y, xx)
#'  names(d) <- c("y",paste0("x",1:p))
#'  return(d)
#' }
#'
#' set.seed(1)
#' beta <- c(-2,rep(1,10))
#' d <- sim1(1e4, beta=beta)
#' a1 <- NB(y ~ ., data=d)
#' a2 <- glm(y ~ ., data=d, family=binomial)
#' ## a3 <- randomForest(factor(y) ~ ., data=d, family=binomial)
#'
#' d0 <- sim1(1e4, beta=beta)
#' p1 <- predict(a1, newdata=d0)
#' p2 <- predict(a2, newdata=d0, type="response")
#' ## p3 <- predict(a3, newdata=d0, type="prob")
#'
#' c2 <- calibration(p2, d0$y, method="isotonic")
#' c1 <- calibration(p1, d0$y, breaks=100)
#' if (interactive()) {
#'   plot(c1)
#'   plot(c2,col="red",add=TRUE)
#'   abline(a=0,b=1)#'
#'   with(c1$xy[[1]], points(pred,freq,type="b", col="red"))
#' }
#'
#' set.seed(1)
#' beta <- c(-2,rep(1,10))
#' dd <- lava::csplit(sim1(1e4, beta=beta), k=3)
#' mod <- NB(y ~ ., data=dd[[1]])
#' p1 <- predict(mod, newdata=dd[[2]])
#' cal <- calibration(p1, dd[[2]]$y)
#' p2 <- predict(mod, newdata=dd[[3]])
#' pp <- predict(c1, p2)
#' cc <- calibration(pp, dd[[3]]$y)
#' if (interactive()) {#'
#'   plot(cal)
#'   plot(cc, add=TRUE, col="blue")
#' }
calibration <- function(pr, cl, #nolint
                        weights=NULL,
                        threshold=10,
                        method="bin",
                        breaks=nclass.Sturges,
                        df=3, ...) {
    if (!is.matrix(pr) && !is.data.frame(pr) && !is.numeric(pr)) {
        pr <- predict(pr, ...)
    }
    unique_cl <- sort(unique(cl))
    if (NCOL(pr)==1) {
        lastcl <- firstcl <- c()
        if (is.factor(cl)) {
            lastcl <- tail(levels(cl), 1)
            firstcl <- levels(cl)[1]
        } else {
            lastcl <- tail(unique_cl, 1)
            firstcl <- unique_cl[1]
        }
        pr <- cbind(pr)
        colnames(pr) <- lastcl
        if (length(unique_cl)==2) {
            pr <- cbind(1-pr, pr)
            colnames(pr) <- c(firstcl, lastcl)
        }
    }
    classes <- colnames(pr)
    clmis <- !(unique_cl %in% classes) ## Classes not in probability matrix
    if (any(clmis)) { ## Assign 0 probability
        pr0 <- matrix(0, nrow=nrow(pr), ncol=sum(clmis))
        colnames(pr0) <- unique_cl[clmis]
        pr <- cbind(pr, pr0)
        classes <- c(classes, colnames(pr0))
    }
    pr[is.na(pr)] <- 0
    stepfuns <- list()
    xy <- list()
    method <- tolower(method)
    qtl <- NULL
    if (!is.function(breaks)) {
        method <- "bin"
        if (is.numeric(breaks) && length(breaks)==1)
            qtl <- seq(0, 1, length.out=breaks)
    }
    if (method=="bin") {
        if (is.function(breaks)) {
            ncuts <- breaks(pr[, 1])
            qtl <- seq(0, 1, length.out=ncuts)
        }
    }
    for (i in seq_len(ncol(pr))) {
        y <- (cl==classes[i])
        sy <- if (!is.null(weights)) sum(y*weights) else sum(y)
        ## Check if enough observations falls in class 'i'
        if (any(!is.na(y)) && sy>threshold) {
            if (method=="isotonic") {
                m <- isoregw(pr[, i], y, weights=weights)
                stepfuns <- c(stepfuns, m)
            }
            if (method%in%c("logistic", "platt", "ns", "mspline")) {
                if (method%in%c("logistic", "platt")) {
                    m <- glm(y~pr[, i], weights=weights, family=binomial)
                } else {
                    if (method%in%c("mspline")) {
                        if (requireNamespace("mgcv", quietly=TRUE)) {
                          m <- glm(y ~ mgcv::mono.con(pr[, i]),
                                   weights = weights,
                                   family = binomial
                            )
                        } else {
                            method <- "ns"
                        }
                    }
                    if (method%in%c("ns")) { ## Natural cubic spline
                      m <- glm(y ~ splines::ns(pr[, i], df = df),
                               weights = weights,
                               family = binomial
                        )
                    }
                }
                phat <- predict(m, type="response")
                f <- function(x) {
                  a <- approxfun(c(0, pr[, i], 1),
                                 c(0, phat, 1),
                      method = "constant"
                    )
                    res <- a(x)
                    res[res<0] <- 0
                    res[res>1] <- 1
                    res
                }
                stepfuns <- c(stepfuns, f)
            }
            if (method%in%"bin") {
                if (!is.null(qtl)) {
                    cpt <- quantile(pr[, i], qtl)
                } else {
                    cpt <- breaks
                }
                cpt <- cpt[which(!duplicated(cpt))]
                val <- cut(pr[, i], breaks=cpt, include.lowest=TRUE)
                if (!is.null(weights)) {
                    phat <- as.vector(by(cbind(y, weights), val, function(z) {
                        return(weighted.mean(z[, 1], z[, 2]))
                    }))
                } else {
                    phat <- as.vector(by(y, val, mean))
                }
                mpt <- diff(cpt)/2+cpt[-length(cpt)] # mid-point
                xy <- c(xy, list(data.frame(
                              cpt = cpt,
                              pred = c(0, mpt),
                              freq = c(0, phat)
                )))
                f <- suppressWarnings(approxfun(c(0, cpt, 1),
                                                c(0, phat, 1, 1),
                  method = "constant", rule = 2
                ))
                stepfuns <- c(stepfuns, f)
            }
        } else {
            ## With to few observations we will not do any further calibration
            stepfuns <- c(stepfuns, base::identity)
        }
    }
    names(stepfuns) <- classes
    mycall <- match.call()
    return(structure(list(
        call=mycall,
        stepfun=stepfuns,
        classes=classes,
        model=method,
        xy=xy),
        class="calibration"))
}



#' @title calibration class object
#'
#' @description The functions \code{\link{calibration}} returns an object
#' of the class \code{calibration}.
#'
#' An object of class '\code{calibration}' is a list with at least the
#' following components:
#' \describe{
#'   \item{stepfun}{estimated step-functions
#' (see \code{stepfun}) for each class}
#'   \item{classes}{the unique classes}
#'   \item{model}{model/method type (string)}
#'   \item{xy}{list of data.frame's with predictions (pr) and estimated
#' probabilities of success (only for 'bin' method)}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object
#' of class \code{targeted}:
#' \describe{
#'   \item{\code{predict}}{Apply calibration to new data.}
#'   \item{\code{plot}}{Plot the calibration curves (reliability plot).}
#'   \item{\code{print}}{Basic print method.}
#'  }
#'
#' @aliases calibration-class
#' @seealso \code{\link{calibration}}, \code{\link{calibrate}}
#' @return objects of the S3 class '\code{calibration}'
#' @examples ## See example(calibration) for examples
#' @docType class
#' @name calibration-class
NULL


#' @export
calibrate <- function(object, pr, normalize = TRUE, ...) {
  classes <- object$classes
  namesprovided <- FALSE
  class <- colnames(pr)
  if (is.null(class)) {
    class <- seq_len(NCOL(pr))
  }
  for (cl in class) {
    if (!(cl %in% classes) && !namesprovided) {
      ## Do not calibrate,
      ## p0 <- rep(epsilon,NROW(pr))
    } else {
      p0 <- pr[, cl]
      f <- object$stepfun[[cl]]
      pr[, cl] <- f(p0)
    }
  }
  if (normalize) {
    pr <- t(apply(pr, 1, function(x) x / sum(x)))
  }
  return(pr)
}



#' @export
plot.calibration <- function(x, cl = 2,
                             add = FALSE,
                             xlab = "Prediction",
                             ylab = "Fraction of positives",
                             main = "Calibration plot",
                             type = "s",
                             ...) {
  if (!add) {
    plot(0, 0,
      type = "n",
      xlim = c(0, 1),
      ylim = c(0, 1),
      xlab = xlab,
      ylab = ylab,
      main = main
    )
    abline(a = 0, b = 1, col = "lightgray")
  }
  if (length(x$xy) > 0) {
    xx <- x$xy[[cl]]$cpt
    xx[1] <- xx[1] + 1e-12
    xx[length(xx)] <- xx[length(xx)] - 1e-12
    yy <- x$stepfun[[cl]](xx)
    return(lines(xx, yy, type = type, ...))
  } else {
    return(plot(x$stepfun[[cl]], add = TRUE, type = type, ...))
  }
}

#' @export
print.calibration <- function(x, ...) {
  cat("Call: ")
  print(x$call)
  cat("\nCalibration model:", x$model, "\n")
  cat("Number of classes:", length(x$classes), "\n")
  return(invisible())
}

#' @export
predict.calibration <- function(object, newdata, ...) {
  if (data.table::is.data.table(newdata)) newdata <- as.data.frame(newdata)
  if (NCOL(newdata) == 1) {
    pr <- cbind(1 - newdata, newdata)
    res <- calibrate(object, pr, ...)[, 2]
    return(res)
  }
  return(calibrate(object, newdata, ...))
}

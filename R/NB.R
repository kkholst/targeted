##' Naive Bayes Classifier
##'
##' @title Naive Bayes
##' @param formula Formula with syntax: response ~ predictors | weights
##' @param data data.frame
##' @param weights optional frequency weights
##' @param kernel If TRUE a kernel estimator is used for numeric predictors (otherwise a gaussian model is used)
##' @param laplace.smooth Laplace smoothing
##' @param prior optional prior probabilities (default estimated from data)
##' @param ... additional arguments to lower level functions
##' @aliases NB NB2
##' @export
##' @author Klaus K. Holst
##' @examples
##' data(iris)
##' m2 <- NB(Species ~ Sepal.Width + Petal.Length, data=iris)
##' pr2 <- predict(m2, newdata=iris)
NB <- function(formula, data, weights=NULL,
       kernel=FALSE, laplace.smooth=0, prior=NULL, ...) {
    if (missing(data)) stop("Need data as data.frame or data.table")
    if (!data.table::is.data.table(data)) data <- data.table::data.table(data)
    ff <- procform(formula, data=as.data.frame(data))
    y <- as.factor(as.matrix(with(ff, data[, response, with=FALSE])))
    if (length(ff$filter)>0 && is.null(weights)) {
        weights <- as.matrix(data[, ff$filter[[1]], with=FALSE])[, 1]
    } else if (is.null(weights)) weights <- rep(1, length(y))
    X <-  data[, ff$predictor, with=FALSE, drop=FALSE]
    charvar <- names(Filter(is.character, X))
    ## Convert character vectors to factors to avoid loosing levels
    ## when calculating conditional probabilities
    if (length(charvar)>0)
        for (col in charvar) data.table::set(X, j=col, value=factor(X[[col]]))
    xtabs0 <- function(counts, x, prop=FALSE, ...) {
        res <- stats::xtabs(counts~x)
        if (prop) res <- res/sum(res)
        return(structure(as.numeric(res), names=names(res)))
    }
    cls <- levels(y)
    prior0 <- xtabs0(weights, y, prop=TRUE)
    if (is.null(prior)) {
        ## user-defined priors
        ## TODO: Assign new values and renormalize
    }
    estcond <- function(x, weights, ...) {
        if (data.table::is.data.table(x)) x <- as.matrix(x[, 1])
        w <- weights/sum(weights)
        if (is.numeric(x)) {
            if (!kernel) {
                ## TODO: "smoothing" in sparse cases
                est <- c(mean=sum(x*w), sd=(sum(x^2*w)-sum(x*w)^2)^.5)
                return(list(model="gaussian", estimate=est))
            } else {
                ## Kernel density estimate
                ## TODO: add tuning parameters?
                return(list(model="density", estimate=stats::density(x, weights=w)))
            }
        } else {
            ## Laplace smoothing, (x+laplace.smooth)/(N+k*alpha),
            ## x: counts in different categories; N: total counts; k: number of categories
            ##idx <- rep(seq_along(x),weights);
            M <- xtabs0(weights, x)+laplace.smooth
            return(list(model="multinomial", estimate=M/sum(M)))
        }
    }
    pcond <- lapply(cls, function(i) {
        idx <- which(y==i)
        m0 <- with(ff, as.data.frame(X[idx, predictor, with=FALSE, drop=FALSE]))
        lapply(m0, estcond, weights=weights[idx])
    })
    structure(list(prior=prior0,       ## Pr(class)
                   conditional=pcond,  ## Pr(x|class)
                   classes=cls,
                   xvar=names(pcond[[1]]),
                   xmodel=unlist(lapply(pcond[[1]], function(x) x$model)),
                   model=ff,
                   call=match.call()),
              class="NB")
}

##' @export
print.NB <-
function(x, ...) {
    print(x$call)
    cat("\n")
    val <- x$prior
    names(val) <- paste0(seq(length(val)), ": ", names(val))
    print(data.table::data.table(Prior=val))
    cat("\n")
}

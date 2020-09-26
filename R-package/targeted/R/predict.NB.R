##' Naive Bayes Classifier predictions
##' 
##' @title Predictions for Naive Bayes Classifier
##' @param object density object
##' @param newdata new data on which to make predictions
##' @param expectation Variable to calcualte conditional expectation wrt probabilities from NB classifier
##' @param threshold Threshold parameters. First element defines the threshold on the probabilities and the second element the value to set those truncated probabilities to.
##' @param ... Additional arguments to lower level functions
##' @export 
##' @author Klaus K. Holst
predict.NB <- function(object,newdata,expectation=NULL,threshold=c(1e-3,1e-3), ...) {
    if (missing(newdata)) stop("Need new data to make predictions")
    if (!is.data.table(newdata)) newdata <- data.table::data.table(newdata)
    ## Likelihood P(class|x) = P(class)P(x,class)
    if (!is.null(expectation)) {
        if (inherits(expectation,"formula")) expectation <- all.vars(expectation)
        z <- newdata[,expectation]
        ## TODO: Not used for now
    }
    if (!all(c(object$model$predictor,expectation)%in%names(newdata))) stop("Variables missing in data")

    if (is.null(expectation)) {
        lposterior <- matrix(nrow=nrow(newdata),ncol=length(object$classes))
    }
    xx <- object$model$predictor
    X <-  newdata[,xx,with=FALSE,drop=FALSE]
    charvar <- names(Filter(is.character,X))
    if (length(charvar)>0)
        for (col in charvar) data.table::set(X, j=col, value=factor(X[[col]]))
    px <- rep(0,nrow(newdata))
    for (i in seq_along(object$classes)) {
        lpcond <- rep(0,nrow(newdata)) ## P(x|c) = prod P(xi|c) pr independence assumption
        for (j in seq_along(xx)) {
            x0 <- object$conditional[[i]]
            nam <- object$xvar[j]
            x <- as.matrix(X[,nam,with=FALSE,drop=FALSE])[,1]
            estx <- x0[[j]]
            if (is.list(estx)) {
                estx <- estx$estimate
            }
            curmodel <- object$xmodel[j]
            if (curmodel=="multinomial") {
                xs <- unique(x)
                misx <- which(!(xs%in%names(estx)))
                if (length(misx)>0) {
                    nn <- c(names(estx),xs[misx])
                    estx <- c(estx,rep(NA,length(misx)))
                    names(estx) <- nn
                }
                estx[estx<threshold[1] | is.na(estx)] <- threshold[2]
                estx <- estx/sum(estx)
                lpcond <- lpcond+log(estx[x])
            }
            if (curmodel=="gaussian") {
                ## TODO: treshold
                if (is.na(estx[1])) estx[1] <- 0
                if (is.na(estx[2]) || estx[2]<1e-16) estx[2] <- 1
                lpcond <- lpcond+dnorm(x,mean=estx[1],sd=estx[2],log=TRUE)
            }
            if (curmodel%in%c("kernel","density")) {
                estx <- predict(estx,x)
                ## TODO: treshold
                lpcond <- lpcond+log(estx)
            }
        } 
        logjoint <- lpcond + log(object$prior[i]) ## log P(x,c)
        if (is.null(expectation)) {
            lposterior[,i] <- logjoint
        }
        px <- px + exp(logjoint) ## P(x)
        lposterior[,i] <- logjoint
    }
    
    if (is.null(expectation)) {
        for (i in seq(ncol(lposterior))) {          
            lposterior[,i] <- lposterior[,i]-log(px) ## log P(c|x)
        }
    }
    colnames(lposterior) <- object$classes
    exp(lposterior)
}

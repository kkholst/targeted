##' @export
aipw <- function(formula, data, propensity=NULL, ...) {
    yf <- getoutcome(formula,sep="|")
    if (length(attr(yf,"x"))==1) {
        yf <- getoutcome(formula,sep="")
        xx <- list(as.formula(paste("~",as.character(formula)[3])))
        if (!is.null(propensity))
            attr(yf,"x") <- c(xx, list(propensity))
        else
            attr(yf,"x") <- c(xx, xx)
    }
    r <- !is.na(model.frame(as.formula(paste0(yf,"~1")), data=data, na.action=na.pass))*1
    data[,"_R"] <- r[,1]
    formula <- c(list(as.formula(paste(yf[1],"~ `_R`"))), attr(yf,"x"))
    res <- ace(formula, data=data, ..., missing=TRUE,labels=yf[1])
    res
}

##' Augmented Inverse Probability Weighting estimator for the Average Causal Treatment Effect.
##'
##' @title AIPW estimator for Average Causal Effect
##' @param formula Formula (see details below)
##' @param data data.frame
##' @param weights optional frequency weights
##' @param binary Binary response (default TRUE)
##' @param nuisance outcome regression formula
##' @param propensity propensity model formula
##' @param all If TRUE all standard errors are calculated (default TRUE when exposure
##' only has two levels)
##' @param missing If TRUE a missing data (AIPW) estimator is returned
##' @param labels Optional treatment labels
##' @param ... Additional arguments to lower level functions
##' @return An object of class '\code{ace.targeted}' is returned. See \code{\link{targeted-class}}
##' for more details about this class and its generic functions.
##' @details
##' The formula may either be specified as:
##' response ~ treatment | nuisance-formula | propensity-formula
##'
##' For example: \code{ace(y~a | x+z | x*z, data=...)}
##'
##' Alternatively, as a list: \code{ace(list(y~a, ~x+z, ~x*z), data=...)}
##'
##' Or using the nuisance (and propensity argument): \code{ace(y~a, nuisance=~x+z, ...)}
##' @export
##' @author Klaus K. Holst
##' @aliases ace aipw
##' @examples
##' m <- lvm(y ~ a+x, a~x)
##' distribution(m,~ a+y) <- binomial.lvm()
##' d <- sim(m,1e3,seed=1)
##'
##' a <- ace(y ~ a, nuisance=~x, data=d)
##' summary(a)
##'
##' # Multiple treatments
##' m <- lvm(y ~ a+x, a~x)
##' distribution(m,~ y) <- binomial.lvm()
##' m <- ordinal(m, K=4, ~a)
##' transform(m, ~a) <- factor
##' d <- sim(m,1e4)
##' (a <- ace(y~a|a*x|x, data=d))
##'
##' # Comparison with randomized experiment
##' m0 <- cancel(m, a~x)
##' d0 <- sim(m0,2e5)
##' lm(y~a-1,d0)
##'
##' # Choosing a different contrast for the association measures
##' summary(a, contrast=c(2,4))
ace <- function(formula,
         data, weights, binary=TRUE,
         nuisance=NULL,
         propensity=nuisance,
         all, missing=FALSE, labels=NULL, ...) {
    if (!is.null(nuisance) && inherits(nuisance, "formula")) {
        formula <- list(formula, nuisance, propensity)
    }
    if (is.list(formula)) {
        yf <- getoutcome(formula[[1]])
        exposure <- gsub("`","",attr(yf,"x"))
        xf <- formula
        xf[[1]] <- update(xf[[1]], .~.-1)
    } else {
        if (inherits(formula,"formula")) {
            yf <- getoutcome(formula, sep="|")
            xf <- attr(yf,"x")
            exposure <- attr(getoutcome(xf[[1]]),"x")
            xf[[1]] <- update(as.formula(paste0(yf,deparse(xf[[1]])) ), .~.-1)
            ##if (!missing) ## Add exposure indicator to outcome regression model unless we are doing a missing data analysis
            ##    xf[[2]] <- update(xf[[2]], as.formula(paste0("~ ",exposure," + .")))
        }
    }
    if (is.list(formula) || inherits(formula,"formula")) {
        ## xf <- lapply(xf, function(x) { environment(x) <- baseenv(); return(x) })
        xx <- lapply(xf, function(x) model.matrix(x, data=data))
        if (missing) {
            yx <- model.frame(xf[[1]], data=data, na.action=na.pass0)
        } else {
            yx <- model.frame(xf[[1]],data=data)
        }
        a <- yx[,exposure]
        y <- yx[,yf[1]]
        x1 <- xx[[2]]
        x2 <- xx[[3]]
        nn <- list(yf[1], exposure, colnames(x1), colnames(x2))
        rm(yx,xx,yf)
    } else {
        stop("Expected a formula or a matrix (response,exposure)")
    }

    if (base::missing(weights)) weights <- rep(1,length(y))
    if (inherits(weights,"formula")) weights <- all.vars(weights)
    if (is.character(weights)) weights <- as.vector(data[,weights])
    weights2 <- weights
    if (missing) {
        binary <- FALSE
        a0 <- (a!=1)
        weights2[a0] <- 0
    }
    if (binary) {
        l1 <- glm.fit(y=y, x=x1, weights=weights, family=binomial())
    } else {
        l1 <- lm.wfit(y=y, x=x1, w=weights2)
        rm(weights2)
    }
    beta <- l1$coef
    iid.beta <- fast_iid(y,l1$fitted,x1,weights,binary)
    treatments <- if (is.factor(a)) levels(a) else sort(unique(a))
    if (length(treatments)>20) stop("Unexpected large amount of treatments")
    if (base::missing(all)) all <- length(treatments)==2
    if (length(treatments)>2) {
        if (!is.factor(a)) warning("'",exposure,"' should probably be converted into a factor. An additive model is now assumed in the outcome regression model.")
    }
    est <- c()
    if (missing) treatments <- 1
    iids <- matrix(nrow=length(y),ncol=length(treatments))
    coefs <- numeric(length(treatments))
    count <- 0
    data0 <- data
    bprop <- NULL
    Vprop <- NULL
    nlabels <- labels
    for (trt in treatments) {
        count <- count+1
        a0 <- (a==trt)
        ## For simplicity we here fit a logistic regression for each treatment.
        ## TODO: we do not need to recalculate logistic regression for the last treatment.
        l2 <- glm.fit(y=a0, x=x2, weights=weights, family=binomial("logit"))
        gamma <- l2$coef
        data0[,exposure] <- factor(trt,levels=treatments)
        x1 <- model.matrix(xf[[2]],data=data0)
        val <- ace_est(y=cbind(y),a=cbind(a0),x1=cbind(x1),x2=cbind(x2),theta=c(beta,gamma),weights=weights,binary=binary)
        alpha.index <- 1
        beta.index <- seq_along(beta) + length(alpha.index)
        gamma.index <- seq_along(gamma) + length(alpha.index) + length(beta.index)
        U0 <- val$u
        DU <- t(val$du)
        iid.gamma <- fast_iid(a,l2$fitted,x2,weights)
        if (count==1) {
            gidx <- seq_along(gamma)
            bprop <- numeric(length(gamma)*(length(treatments)-1))
            names(bprop) <- rep(names(gamma),length(treatments)-1)
            Vprop <- matrix(NA, ncol=length(bprop), nrow=length(bprop))
        }
        if (count < length(treatments) && all) {
            gpos <- gidx+length(gamma)*(count-1)
            bprop[gpos] <- gamma
            Vprop[gpos,gpos] <- crossprod(iid.gamma)
        }
        iid.alpha <- ( U0 + iid.beta %*% t(DU[,beta.index,drop=FALSE]) +
                       iid.gamma %*% t(DU[,gamma.index,drop=FALSE]) ) %*%
            t(-Inverse(DU[,alpha.index,drop=FALSE]))
        iids[,count] <- iid.alpha
        coefs[count] <- val$alpha
        if (is.null(labels)) nlabels <- c(nlabels, paste0(exposure,"=",trt))
    }
    Vout  <- NULL
    if (all) Vout <- crossprod(iid.beta)
    outreg <- estimate(coef=coef(l1),vcov=Vout)
    if (missing) {
        bprop <- gamma
        Vprop <- crossprod(iid.gamma)
    }
    propmod <- estimate(coef=bprop,vcov=Vprop)
    V <- crossprod(iids)
    est <- estimate(coef=coefs, vcov=V, labels=nlabels)
    est$iid <- iids
    structure(list(estimate=est,
                   outcome.reg=outreg, propensity.model=propmod, names=unlist(nn)[1:2],
                   formula=xf,
                   npar=c(length(treatments),ncol(x1),ncol(x2)), nobs=length(y), opt=NULL,
                   all=all,
                   bread=V, type=ifelse(binary,"binary","linear")), class=c("ace.targeted","targeted"))
}


##' @export
print.summary.ace.targeted <- function(x, ...) {
    nam <- x$names
    cat("\nAugmented Inverse Probability Weighting estimator\n")
    outreg <- ifelse(x$type=="binary", "logistic regression", "linear regression")
    cat("  Response ", nam[[1]], " (Outcome model: ", outreg,"):\n",sep="")
    cat("\t",paste(nam[[1]],x$formula[[2]]),"\n")
    cat("  Exposure ", nam[[2]], " (Propensity model: logistic regression):\n", sep="")
    cat("\t",paste(nam[[2]],x$formula[[3]]),"\n")
    cat("\n")
    if (x$all) {
        print(x$estimate, unique.names=FALSE,
              sep.which=c(x$npar[[1]],x$npar[[1]]+x$npar[[2]]),
              sep.labels=paste0(c(nam[[4]],nam[[5]])), ...)
    } else {
        print(x$estimate, unique.names=FALSE, ...)
    }
    if (length(x$contrast)>1) {
        cc <- rownames(x$estimate$coefmat)
        with(x, cat("\nAverage Causal Effect (constrast: '",
                    cc[contrast[1]],"' vs. '",cc[contrast[2]],"'):\n\n",sep=""))
        if (!is.null(x$asso)) print(x$asso)
    }
    cat("\n")
}


##' @export
summary.ace.targeted <- function(object, contrast=c(1:2), ...) {
    nn <- lapply(object[c("estimate","outcome.reg","propensity.model")],function(x) length(coef(x)))
    if (object$all) {
        cc <- rbind(object$estimate$coefmat,
                    object$outcome.reg$coefmat,
                    object$propensity.model$coefmat)
    } else {
        cc <- object$estimate$coefmat
    }
    cc <- estimate(coef=cc[,1], vcov=diag(cc[,2]^2,ncol=nrow(cc)),labels=rownames(cc))
    if (length(contrast)>2)
        warning("Only the first two elements of 'contrast' are used")
    if (object$npar[1]<2) contrast <- 1
    asso <- NULL
    if (length(contrast)>=2)
        if (object$type=="binary") {
            asso <- estimate(object$estimate,function(x) c(x[contrast[1]]/x[contrast[2]], lava::OR(x[contrast]), x[contrast[1]]-x[contrast[2]]), labels=c("RR","OR","RD"))
        } else {
            asso <- estimate(object$estimate,function(x) x[contrast[1]]-x[contrast[2]], labels=c("ACE"))
        }
    structure(list(estimate=cc, npar=nn, type=object$type, asso=asso, names=c(object$names,"","Outcome model:","Propensity model:"),
                   all=object$all, formula=gsub("~","~ ",unlist(lapply(object$formula,deparse))),
                   contrast=contrast),
                   class="summary.ace.targeted")
}

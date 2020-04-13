##' Risk regression with binary exposure
##'
##' Risk regression with binary exposure and nuisance model for the odds-product
##' @title Risk regression
##' @param formula formula (see details below)
##' @param exposure formula or character
##' @param data data.frame
##' @param weights optional weights
##' @param type type of association measure (rd og rr)
##' @param optimal If TRUE optimal weights are calculated
##' @param std.err If TRUE standard errors are calculated
##' @param start optional starting values
##' @param ... additional arguments to unconstrained optimization routine (nlminb)
##' @details
##' The formula should be given as
##' response ~ exposure | target | nuisance
##'
##' @export
##' @aliases riskreg riskreg_semi riskreg_mle
##' @author Klaus K. Holst
##' @examples
##' m <- lvm(a[-2] ~ 1*x,
##'         linpred.target[1] ~ 1,
##'         linpred.nuisance[-1] ~ 2*x)
##' distribution(m,~a) <- binomial.lvm("logit")
##' m <- binomial.rr(m, "y","a","linpred.target","linpred.nuisance")
##' d <- sim(m,5e2,seed=1)
##' a <- riskreg(y ~ a | 1 | x, a~x, data=d, type="rr")
##' a
riskreg <- function(formula, exposure=NULL,
             data, weights, type="rr",
             optimal=TRUE, std.err=TRUE, start=NULL, ...) {
    if (is.list(formula)) {
        yf <- getoutcome(formula[[1]],sep="|")
        xf <- formula
        xf[[1]] <- update(xf[[1]], .~.-1)
    } else {
        if (!inherits(formula,"formula")) stop("Formula needed")
        yf <- getoutcome(formula, sep="|")
        xf <- attr(yf,"x")
        xf[[1]] <- update(as.formula(paste0(yf,deparse(xf[[1]]))), .~.-1)
    }
    xx <- lapply(xf, function(x) model.matrix(x, data))
    y <- model.frame(xf[[1]],data=data)[,yf]
    if (NCOL(xx[[1]])>1) stop("First part of formula should specify exposure variable only")
    a <- xx[[1]]
    x1 <- xx[[2]]
    x2 <- xx[[3]]
    if (length(xx)>3) {
        x3 <- xx[[4]]
    }
    nn <- list(yf[1], colnames(a), colnames(x1), colnames(x2))
    semi  <- !is.null(exposure) || length(xx)>3
    rm(xx)
    if (semi) {
        if (inherits(exposure,"formula")) {
            x3 <- model.matrix(exposure,data)
        }
        nn <- c(nn, list(colnames(x3)))

    }
    if (missing(weights)) weights <- rep(1,length(y))
    if (inherits(weights,"formula")) weights <- all.vars(weights)
    if (is.character(weights)) weights <- as.vector(data[,weights])
    val <- riskreg_mle(y=y,a=a,x1=x1,x2=x2,weights=weights,std.err=std.err,type=type,start=start,labels=c(nn[[3]],nn[[4]]),...)
    val$labels <- nn[1:4]
    if (semi) {
        val <- riskreg_semi(y=y,a=a,x1=x1,x2=x2,x3=x3,
                            weights=weights,std.err=std.err,type=type,
                            optimal=optimal,start=start,labels=nn[[3]],
                            mle=val, ...)
        val$prop <- estimate(val$prop, labels=nn[[5]])
    }

    val$labels <- nn
    val$call <- match.call()
    return(val)
}


##' @export
riskreg_mle <- function(y,a,x1,x2=x1,weights=rep(1,length(y)), std.err=TRUE, type="rd", start=NULL, control=list(), ...) {
    x1 <- cbind(x1)
    x2 <- cbind(x2)
    type <- substr(type,1,2)
    f <- function(p) -as.vector(bin_logl(y=y, a=cbind(a),
                                      x1=cbind(x1), x2=cbind(x2),
                                      par=p, weights=weights, type=type))
    df <- function(p,...) -bin_dlogl(y=y, a=cbind(a),
                              x1=cbind(x1), x2=cbind(x2),
                              par=p, weights=weights, type=type,...)
    d2f <- function(theta) deriv(function(p) -bin_dlogl_c(y=y, a=cbind(a),
                                        x1=cbind(x1), x2=cbind(x2),
                                        par=p, weights=weights, type=type), theta)
    # Starting values
    if (is.null(start))
        start <- rep(0.0,ncol(x1)+ncol(x2))
    op <- nlminb(start, f, df, control=control)
    if (std.err) {
        U <- -df(op$par,indiv=TRUE)
        V <- Inverse(d2f(op$par))
        ii <- U%*%V
    } else {
        V <- ii  <- NULL
    }
    loglik  <- -f(op$par)
    est <- estimate(coef=op$par,vcov=V,...)
    est$iid  <- ii
    structure(list(estimate=est, npar=c(ncol(x1),ncol(x2)), logLik=loglik, nobs=length(y), opt=op, bread=V, type=type, estimator="mle"),
              class=c("riskreg.targeted","targeted"))
}


##' @export
riskreg_semi <- function(y, a,
                  x1, x2=x1, x3=x2,
                  weights=rep(1,length(y)), optimal=TRUE,
                  std.err=TRUE, type="rd",start=NULL, control=list(),
                  mle, ...) {
    x1 <- cbind(x1)
    x2 <- cbind(x2)
    x3 <- cbind(x3)
    if (missing(mle))
        mle <- riskreg_mle(y,a,x1,x2,weights=weights,std.err=std.err,type=type)
    theta0 <- coef(mle$estimate)
    alpha0 <- start
    if (is.null(alpha0)) {
        alpha0 <- theta0[seq(NCOL(x1))]
    }
    ## propmod <- glm(a ~ -1 + x3, family=binomial, weights=weights)
    ## gamma <- coef(propmod)
    ## pr <- predict(propmod, type="response")
    propmod <- glm.fit(y=a, x=x3, weights=weights, family=binomial("logit"))
    gamma <- propmod$coefficients
    pr <- propmod$fitted
    omega <- 1
    Alt <- length(grep("2",type))>0
    type <- gsub("[0-9]","",type)
    if (optimal) {
        pp <- bin_pa(y=y,a=a,x1=x1,x2=x2,par=theta0,type=type)
        p0 <- pp[,2]; p1 <- pp[,3]; target <- pp[,4]
        if (type=="rd" || type=="rd2") {
            ## E(A drho/(Pa(1-Pa))|V) = pr*drho/[p1(1-p1)]
            nom <- pr*(1-target^2)/(p1*(1-p1))
            ## E(1/(Pa(1-Pa))|V) =  (1-pr)/[p0(1-p0)] + pr/[p1(1-p1)]
            denom <- (1-pr)/(p0*(1-p0)) + pr/(p1*(1-p1))
            omega <- nom/denom / ( pr*p0*(1-p0) )
        } else {
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            nom <- pr*p1/(1-p1)
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            denom <- (1-pr)*p0/(1-p0) + pr*p1/(1-p1)
            omega <- nom/denom / ( pr*(1-p0) )
        }
    }
    weights1  <- weights*omega
    u <- function(p,indiv=FALSE) {
        pp <- theta0
        if (Alt) pp[seq_along(p)] <- p
        val <- bin_esteq(y=y, a=a,
                         x1=x1, x2=x2,
                         pr=pr,
                         alpha=p, par=pp,
                         weights=weights1, type=type)
        if (!indiv) val <- as.vector(colSums(val))
        return(val)
    }

    f <- function(x) sqrt(sum(u(x))^2)
    opt <- nlminb(alpha0, f, control=control)
    if (opt$objective>1e-3) {
        suppressWarnings(
            op <- optimx::optimx(alpha0, f,
                                 method=c("Nelder-Mead", "BFGS", "nlminb", "Rcgmin"),
                                 control=control))
        op1 <- head(summary(op, order="value"),1)
        opt <- list(objective=op1[,"value",drop=TRUE],
                    par=unlist(op1[,seq_along(alpha0),drop=TRUE]))
    }
    if (opt$objective>1e-3) {
        futile.logger::flog.warn("riskreg optimization: convergence issues")
    }
    Vprop <- NULL
    if (std.err) {
        thetahat <- c(theta0,gamma)
        alphahat <- opt$par
        alpha.index <- seq_along(opt$par)
        theta.index <- seq_along(theta0) + length(alpha.index)
        gamma.index <- seq_along(gamma) + length(alpha.index) + length(theta.index)
        pp <- c(alphahat, thetahat)
        U <- function(p) bin_esteq_c(y=y, a=a,
                              x1=x1, x2=x2, x3=x3,
                              alpha=p[seq_along(alphahat)],
                              par=p[seq_along(thetahat)+length(alphahat)],
                              weights=weights1, type=type)
        DU  <- deriv(U, pp)
        U0 <- u(alphahat, indiv=TRUE)
        ## iid.gamma <- iid(propmod)
        iid.gamma <- fast_iid(y,pr,x3,weights)
        Vprop <- crossprod(iid.gamma)
        iid.theta <- iid(mle$estimate)
        ii <- ( U0 + iid.theta %*% t(DU[,theta.index,drop=FALSE]) +
               iid.gamma %*% t(DU[,gamma.index,drop=FALSE]) ) %*% t(-Inverse(DU[,alpha.index,drop=FALSE]))
        V <- crossprod(ii)
    } else {
        V <- ii <- NULL
    }
    est <- estimate(coef=opt$par,vcov=V,...)
    est$iid  <- ii
    propmod <- estimate(coef=coef(propmod), vcov=Vprop)
    structure(list(estimate=est,opt=opt, npar=c(ncol(x1),ncol(x2),ncol(x3)), nobs=length(y),
                   mle=mle, prop=propmod, type=type, estimator="dre"),
              class=c("riskreg.targeted","targeted"))
}



##' @export
print.summary.riskreg.targeted <- function(x, ...) {
    print(x$call)
    nam <- x$names
    if (x$type=="rr") {
        cat("\nRelative risk model\n")
        model <- "log(RR)"
    }
    if (x$type=="rd") {
        cat("\nRisk difference model\n")
        model <- "atanh(RD)"
    }
    cat("  Response: ", nam[[1]], "\n")
    cat("  Exposure: ", nam[[2]], "\n")
    cat("\n")
    if (x$estimator=="mle") {
        print(x$estimate,unique.names=FALSE,
              sep.which=c(0,length(nam[[3]])),
              sep.labels=paste0(c(model, "log(OP)"),":"), ...)
    } else {
        print(x$estimate, unique.names=FALSE,
              sep.which=c(0,length(nam[[3]]),length(nam[[3]])+length(nam[[4]])),
              sep.labels=paste0(c(model, "log(OP)","logit(Pr)"),":"), ...)
    }
    cat("\n")
}


##' @export
summary.riskreg.targeted <- function(object, ...) {
    nam <- object$labels
    if (length(grep(object$type,"rr"))>0) {
        type <- "rr"
        model <- "log(RR)"
    } else if (length(grep(object$type,"rd"))) {
        type <- "rd"
        model <- "atanh(RD)"
    }
    if (object$estimator=="mle") {
        cc <- object$estimate
        cc$iid <- NULL
    } else {
        cc <- rbind(object$mle$estimate$coefmat, object$prop$coefmat)
        cc[seq_len(object$npar[1]),] <- object$estimate$coefmat
        cc <- estimate(coef=cc[,1], vcov=diag(cc[,2]^2,ncol=nrow(cc)),labels=rownames(cc))
    }
    structure(list(estimate=cc, object$npar, estimator=object$estimator, call=object$call,
                   model=model, type=type, names=nam), class="summary.riskreg.targeted")
}

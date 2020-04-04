targetMLE_proto <- function(y,x,x1,x2=x1,weights=rep(1,length(y)),type="rd") {
    x1 <- cbind(x1)
    x2 <- cbind(x2)
    alpha <- rep(0,ncol(x1))
    beta <- rep(0,ncol(x2))
    theta <- c(alpha,beta)
    pa <- function(theta) {
        alpha <- theta[seq(ncol(x1))]
        beta <- theta[seq(ncol(x2))+ncol(x1)]
        op <- exp(x2%*%beta)
        if (type=="rd") {
            target <- tanh(x1%*%alpha)
            pp <- rd2pr(target,op)
        } else {
            target <- exp(x1%*%alpha)
            pp <- rr2pr(target,op)
        }
        pA <- pp[,1]*(1-x) + x*pp[,2]
        cbind(p=pA,p0=pp[,1],p1=pp[,2],target,op)
    }
    logl <- function(theta) {
        pA <- pa(theta)[,1]
        sum((y*log(pA) + (1-y)*log(1-pA))*weights)
    }
    dlogl <- function(theta) {
        pp <- pa(theta)
        p0 <- pp[,2]
        p1 <- pp[,3]
        target <- pp[,4]
        op <- pp[,5]
        p <- pp[,1]
        if (type=="rd") {        
            s0 <- p0 * (1 - p0)
            s1 <- p1 * (1 - p1)
            dp0.rd <- -s0/(s0 + s1)
            dp0.rd <- (dp0.rd + x)*(1 - target^2)
            d.alpha = apply(x1,2, function(x) x*dp0.rd )
            dp0.op = s0 * s1/(s0 + s1)
            d.beta <- apply(x2,2, function(x) x*dp0.op )
        } else {
            s <- (1-p0) + (1-p1)
            tmp  <-  1 - x + x*target;
            dp.rr <- -p0/p1 * p0*(1-p0) / s * tmp + x*p0
            dp.rr  <- dp.rr*target
            d.alpha <- apply(x1,2, function(x) x*dp.rr)
            s <- (1-p1)*p1 + (1-p0)*p1            
            dp.op  <- (1-p0)^2*(1-p1)^2 / s * tmp * op;
            d.beta <- apply(x2,2, function(x) x*dp.op)            
        }
        S <- (y-p)/(p*(1-p))
        val <- apply(cbind(d.alpha,d.beta),2,function(x) weights*x*S)
        return(colSums(val))
    }
    d2logl <- function(theta) deriv(dlogl,theta)
    op <- nlminb(theta, function(p) -logl(p), function(p) -dlogl(p))
    V <- -Inverse(d2logl(op$par))
    est <- estimate(coef=op$par,vcov=V)
    list(estimate=est,opt=op, coef=op$par)
}


Us <- function(alpha, theta,
        y, x,
        x1, x2=x1, x3=x2,
        weights=rep(1, length(y)),
        optimal=TRUE, type="rd", indiv=TRUE, ...) {    
    x1 <- cbind(x1)
    x2 <- cbind(x2)
    x3 <- cbind(x3)
    alpha1 <- theta[seq(NCOL(x1))]
    beta1 <- theta[seq(NCOL(x2))+NCOL(x1)]
    gamma1 <- theta[seq(NCOL(x3))+NCOL(x2)+NCOL(x1)]
    pr <- lava::expit(x3%*%gamma1)
    op <- exp(x2%*%beta1)
    if (type=="rd") {
        target <- tanh(x1%*%alpha1)
        pp <- rd2pr(target,op)
    } else {
        target <- exp(x1%*%alpha1)
        pp <- rr2pr(target,op)
    }
    p0 <- pp[,1]
    p1 <- pp[,2]
    omega <- 1
    if (optimal) {
        if (type=="rd") {
            nom <- pr*(1-target^2)/(p1*(1-p1))
            denom <- (1-pr)/(p0*(1-p0)) + pr/(p1*(1-p1))
            omega <- nom/denom / ( pr*p0*(1-p0) )
        } else {
            nom <- pr*p1/(1-p1)         
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            denom <- (1-pr)*p0/(1-p0) + pr*p1/(1-p1)
            omega <- nom/denom / ( pr*(1-p0) )            
        }
    }
    if (type=="rd") {
        target <- tanh(x1%*%alpha)
        pp <- rd2pr(target,op)
        H <- y-x*target # Y-X*tanh(a'X1)
    } else {
        lp <- x1%*%alpha
        target <- exp(lp)
        pp <- rr2pr(target,op)
        H <- y*exp(-x*lp) # y*exp(-X*log(RR))
    }
    S <- (x-pr)*(H-p0)*weights*omega 
    res <- apply(x1,2,function(x) S*x)
    if (indiv) return(res)
    colSums(res)
}

target_proto <- function(y,x,x1,x2=x1,x3=x2,weights=rep(1,length(y)),optimal=TRUE,std.err=FALSE,type="rd",...) {
    x1 <- cbind(x1)
    x2 <- cbind(x2)
    x3 <- cbind(x3)
    mle <- riskreg_mle(y,x,x1,x2,weights=weights,std.err=std.err,type=type)
    theta0 <- coef(mle$estimate)
    alpha0 <- theta0[seq(NCOL(x1))]
    propmod <- glm(x ~-1+x3, family=binomial, weights=weights)
    gamma <- coef(propmod)
    pr <- predict(propmod, type="response")
    pa <- function(theta) {
        alpha <- theta[seq(ncol(x1))]
        beta <- theta[seq(ncol(x2))+ncol(x1)]
        op <- exp(x2%*%beta)
        if (type=="rd") {
            target <- tanh(x1%*%alpha)
            pp <- rd2pr(target,op)
        } else {
            target <- exp(x1%*%alpha)
            pp <- rr2pr(target,op)
        }
        pA <- pp[,1]*(1-x) + x*pp[,2]
        cbind(p=pA,p0=pp[,1],p1=pp[,2],target,op)
    }
    omega <- 1
    if (optimal) { ## Not implemented for RR
        pp <- pa(theta0)
        print(theta0)
        print(head(pp))
        p0 <- pp[,2]; p1 <- pp[,3]; target <- pp[,4]
        if (type=="rd") {
            ## omega.opt = (1 - rd) * (1 + rd)/(p0 * (1 - p0) + rd * (1 - pr) * 
            ##                                  (1 - 2 * p0 - rd))
            ## E(A drho/(Pa(1-Pa))|V) = pr*drho/[p1(1-p1)]
            nom <- pr*(1-target^2)/(p1*(1-p1))
            ## E(1/(Pa(1-Pa))|V) =  (1-pr)/[p0(1-p0)] + pr/[p1(1-p1)] =
            denom <- (1-pr)/(p0*(1-p0)) + pr/(p1*(1-p1))
            omega.opt <- nom/denom / ( pr*p0*(1-p0) )
            omega <- omega.opt
        } else {
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            nom <- pr*p1/(1-p1)         
            # E(A pa/(1-Pa) |V) = pr*drho/[p1(1-p1)]
            denom <- (1-pr)*p0/(1-p0) + pr*p1/(1-p1)
            omega <- nom/denom / ( pr*(1-p0) )
        }
    }
    print(tail(omega))
    U <- function(alpha) {
        theta <- theta0
        p0 <- pa(theta)[,2]
        theta[seq_along(alpha)] <- alpha
        pp <- pa(theta)
        ##p0 <- pp[,2]
        if (type=="rd") {
            H  <-  y-x*pp[,4] # Y-X*tanh(a'W)
        } else {
            H  <-  y*exp(-x*log(pp[,4]))
        }
        p0 <- pp[,2]
        S <- (x-pr)*(H-p0)*weights*omega
        apply(x1,2,function(x) S*x)
    }
    weights1  <- weights*omega
    u <- function(p) as.vector(colSums(U(p)))
    opt <- nlminb(alpha0,function(x) sum(u(x)^2),...)
    if (std.err) {
        thetahat <- c(theta0,gamma);
        alphahat <- opt$par
        alpha.index <- seq_along(opt$par)
        theta.index <- seq_along(theta0) + length(alpha.index)
        gamma.index <- seq_along(gamma) + length(alpha.index) + length(theta.index)    
        pp <- c(alphahat, thetahat)
        UU <- function(p,indiv=FALSE) Us(alpha=p[seq_along(alphahat)], theta=p[seq_along(thetahat)+length(alphahat)], y=y, x=x, x1=x1,x2=x2,x3=x3,weights=weights,optimal=optimal, indiv=indiv)
        DU  <- deriv(UU,pp)
        U0 <- UU(pp,indiv=TRUE)
        iid.gamma <- iid(propmod)
        iid.theta <- iid(mle$estimate)
        ii <- (U0 - iid.theta%*%t(DU[,theta.index]) - iid.gamma%*%t(DU[,gamma.index])) %*% t(solve(DU[,alpha.index]))
        V <- crossprod(ii)
    } else {
        V <- ii <- NULL
    }
    est <- estimate(coef=opt$par,vcov=V)
    est$iid  <- ii
    structure(list(estimate=est,opt=opt, mle=mle), class="targeted")    
}

##' @export
cumres.ordreg <- function(model,variable,R=100,plot=FALSE,aggregate=sum,...) {
    env <- model$up
    n <- length(env$y)
    nc <- length(env$alpha)+1
    ## Cumulative Probabilities
    Y <- with(env, matrix(0,nrow=length(y),ncol=length(alpha)))
    for (i in seq(nrow(Y))) if (env$y[i]<nc) Y[i,seq(env$y[i],nc-1)] <- 1
    cpr <- predict(model,type="cumulative")
    r <- Y-cpr
    ## Actual classes (
    Y0 <- matrix(0,nrow=n,ncol=nc)
    Y0[cbind(seq(n),env$y)] <- 1
    pr <- predict(model,type="prob")
    r0 <- (Y0-pr)[,-nc,drop=FALSE]

    Res <- r ## For now we only use residuals defined from the cum.prob.
    if (missing(variable) || variable=="predicted") {
        variable <- "predicted"
        x <- env$lp[,1]
    } else {
        x <- env$X[,variable]
    }
    xord <- order(x)
    x <- x[xord]
    Res <- Res[xord,,drop=FALSE]
    x0 <- unique(x)
    cr <- apply(Res,2,cumsum)
    
    W <- matrix(0,nrow=nrow(Res),ncol=ncol(Res))
    for (i in rev(x0)) {
        idx0 <- which(x<=i)
        W[idx0,] <- cr[tail(idx0,1),]
    }
    W <- W/sqrt(n)
    
    dr0 <- -numDeriv::jacobian(function(p) predict(model,p=p,type="cumulative"),coef(model))
    dr <- dr0[seq(n),,drop=FALSE]
    for (i in seq_len(nc-2)) dr <- cbind(dr,dr0[seq(n)+i*n,])
    dr <- dr[xord,,drop=FALSE]
    eta <- apply(dr,2,cumsum)
    Eta <- eta
    for (i in rev(x0)) {
        idx0 <- which(x<=i)
        eta0 <- eta[tail(idx0,1),,drop=FALSE]%x%cbind(rep(1,length(idx0)))
        Eta[idx0,] <- eta0
    }
    IID <- lava::iid(model)[xord,,drop=FALSE]
    
    ##agr <- function(x) sum(x) ## Functional to combine processes 
    Wagr <- t(rbind(apply(W,1,aggregate)))    
    What0 <- matrix(ncol=R,nrow=nrow(W))
    What <- c()
    for (i in seq_len(ncol(Wagr))) {
        What <- c(What,list(What0))
    }
    if (plot) {
        ylim <- range(Wagr)
        ylim <- ylim + c(-1,1)*diff(ylim)*0.4
        plot(0,type="n",xlab="x",ylab="Cumulative residual",xlim=range(x0),ylim=ylim)
        ## plot(0,type="n",xlab="x",ylab="cumulative residual",xlim=range(x0),ylim=ylim)
        ## legend("topright",legend=paste("Category",seq(nc-1)),lty=seq(n-1))     
        ## for (i in 1:ncol(W)) lines(x,W[,i],lty=i,type="s")
    }

    KS <- c()
    KSagr <- c()
    for (k in seq_len(ncol(Wagr))) KSagr <- c(KSagr,list(NULL))
    for (j in seq(R)) {
        G <- rnorm(n)
        Res_ <- apply(Res,2,function(x) x*G)
        cr_<- cumsum(Res_)
        W_ <- matrix(0,nrow=n,ncol=nc-1)
        IID_ <- apply(IID,2,function(x) x*G)
        for (i in rev(x0)) {
            idx0 <- which(x<=i)
            idx1 <- tail(idx0,1)
            I <- (x<=i)*1
            Eta_ <- matrix(Eta[idx1,],byrow=TRUE,ncol=ncol(IID))
            ir_ <- apply(Res_,2,function(x) I*x)
            W_[idx0,] <- colSums(ir_+ IID_%*%t(Eta_))
        }
        W_ <- W_/sqrt(n)
        W_agr <- t(rbind(apply(W_,1,aggregate)))
        for (k in seq_len(ncol(W_agr))) {
            What[[k]][,j] <- W_agr[,k]
            KSagr[[k]] <- c(KSagr[[k]],max(abs(W_agr[,k])))
        }
        if (plot && j<50) {
            ## for (k in 1:ncol(W_)) 
            ##     lines(x,W_[,k],col="orange",type="s",lty=k)
            lines(x,W_agr,col=lava::Col("orange",0.25),type="s")
        }
    }
    
    if (plot) {
        lines(x,Wagr,type="s",lwd=2)
    }
    xx <- KSagr0 <- c()
    for (k in seq_len(ncol(Wagr))) {
        KSagr0 <- c(KSagr0,max(abs(Wagr[,k])))
        xx <- cbind(xx,x)
    }
    varnames <- paste(variable,seq_len(ncol(Wagr)),sep=".")
    type <- rep("normal",ncol(Wagr))
    pvalKSagr <- c()
    for (k in seq_len(ncol(Wagr))) {
        pvalKSagr <- c(pvalKSagr,mean(KSagr[[k]]>KSagr0[k]))
    }
    res <- list(W=Wagr,What=What,R=R,x=xx,
                KS=pvalKSagr, CvM=NULL, n=length(x),
                type=type,
                sd=NULL, cvalues=NULL, variable=varnames,
                model="ordreg")
    class(res) <- "cumres"
    return(res)
}

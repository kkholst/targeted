##' @export
`cumres` <-
function(model,...) UseMethod("cumres")

##' @export
`cumres.default` <-
    function(model, variable, r=stats::residuals(model), dr,  
      R=1000, b=0, plots=min(R,50),
      coef=stats::coef(model), ic=lava::iid(model), breakties=1e-12,
      ...) {

        if(any(is.na(coef))) stop("Over-parametrized model")
        if (missing(dr)) {            
            dr <- attributes(r)$grad    
            if (is.null(dr)) {
                if (!requireNamespace("numDeriv")) stop("Supply gradient")
                dr <- numDeriv::jacobian(r, coef, ...)
            }
        } else {
            if (is.function(dr))
                dr <- dr(coef)
        }
        if (is.function(r)) {
            r <- r(coef,...)
        }
                
        n <- length(r)
        untie <- runif(n,0,breakties)
        cr <- new(CumRes, r, dr, ic)         
        W <- c()
        What <- c()
        KS <- c()
        CvM <- c()
        mytype <- c()
        UsedData <- c()
        variable <- as.data.frame(variable)
        for (x in variable) {
            cr$reorder(x+untie)
            idx <- seq(n)-1
            teststat <- gof_samplestat(cr, R=R, idx=idx)
            KS <- c(KS, teststat$pval.ks)
            CvM <- c(CvM, teststat$pval.cvm)
            UsedData <- cbind(UsedData, teststat$t);
            W <- cbind(W, teststat$w)
            What0 <- matrix(0, length(idx), plots)
            for (i in seq(plots)) What0[,i] <- cr$sample1(idx)
            What <- c(What, list(What0))
            mytype <- c(mytype,"residual")
        }     
    
        res <- list(W=W, What=What,
                    x=UsedData,
                    KS=KS, CvM=CvM,
                    R=R, n=n,
                    sd=NULL, cvalues=NULL,
                    variable=names(variable),
                    type=mytype, untie=untie,
                    model=class(model)[1])
        class(res) <- "cumres"        
        return(res)    
    }

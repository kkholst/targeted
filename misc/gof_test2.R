library(lava)
devtools::load_all("../R-package")

m <- lvm(list(c(y1,y2,y3)~eta,eta~x)); latent(m) <- ~eta
## simulate some data with non-linear covariate effect
functional(m,eta~x) <- function(x) 0.3*x^2
d <- sim(m,500)

e <- estimate(m,d)
## Checking the functional form of eta on x
g <- cumres(e,eta~x,R=1000)

x <- function(p) predict(e,x=~y2+y3,p=p)[,"eta"]
## Checking the functional form of y1 on eta
cumres(e,y1~eta,R=1000)
g <- cumres(e,"y1",x=x,R=1000)
plot(g)


library("futile.logger")
flog.layout(layout.colored)

set.seed(1)
n <- 10000
flog.info("Simulating (n=%s)", n)
x <- rnorm(n)
z <- seq(0,1,length.out=n)
y <- 0*x*x + x + z + rnorm(n)
d <- data.frame(y,x,z)
write.table(d, file="d.csv", row.names=FALSE, col.names=FALSE, sep=",")

l <- lm(y~x+z, data=d)

ii <- lava::iid(l)
r <- residuals(l)
t <- x ## Variable to order residuals after
d2 <- data.frame(r,t,ii)
write.table(d2, file="r.csv", row.names=FALSE, col.names=FALSE, sep=",")



ord <- order(t)
t0 <- t[ord]
x0 <- model.matrix(l)[ord,]
r0 <- r[ord]
ii0 <- ii[ord,,drop=FALSE]
eta <- -apply(x0,2,cumsum)
KS <- function(x) max(abs(x))

R <- 50
Ws <- matrix(0,n,max(100,R))
stat <- numeric(R)
for (i in seq(R)) {
    G0 <- rnorm(n, sd=1)
    W1 <- cumsum(G0*r0)
    iiG <- colSums(apply(ii0,2,function(x) x*G0))
    W2 <- apply(eta, 1, function(x) crossprod(x, iiG))
    (Wi <- (W1+W2)/sqrt(n))
    if (i<=ncol(Ws))
        Ws[,i] <- Wi
    stat[i] <- KS(Wi)
    if (i%%50==0) message(i)
}

W0 <- cumsum(r0)/sqrt(n)
matplot(t0, Ws, type="s", col=lava::Col(1,0.3), ylim=range(c(W0,Ws)))
lines(t0, W0, type="s", lwd=3, col=lava::Col("darkred",.5))

##################################################

t1a <- t0[fast.approx(t0, seq(t0[1], t0[n], length.out=50))]
t1b <- quantile(t0, seq(0,1,length.out=50))
t1 <- sort(union(t1a,t1b))
pos <- 1
idx <- numeric(n) # grouping
for (i in seq_along(idx)) {
    while (t0[i]>t1[pos]) {
        pos <- pos+1        
    }
    idx[i] <- pos
}
idx1 <- dby(data.frame(id=idx, pos=seq_along(idx)), pos~id, pos=max, REDUCE=T)$pos
n1 <- length(t1)

R <- 500
Ws <- matrix(0, n1, R)
for (k in seq(R)) {
    message(k)

    G <- rnorm(n)
    W1 <- cumsum(G*r0)[idx1]
    iiG <- apply(ii0, 2, function(x) G*x)
    W2 <- c()
    for (i in seq_along(t1)) {
        ww <- sum(iiG%*%eta[idx1[i],])
        #epsilon0 <- colSums(apply(ii0, 1, function(x) x*eta[idx1[i],]))
        W2 <- c(W2, ww)
    }
    Ws[,k] <- (W1+W2)/sqrt(n)
}

plot(t0, W0, type="s", col=lava::Col("darkred",0.5), lwd=3, ylim=range(c(W0,Ws)))
matplot(t1, Ws, type="s", col=lava::Col(1,0.2), add=TRUE)


##################################################


R <- 10
Ws <- matrix(0, n1, R)
for (k in seq(R)) {
    message(k)

    W1 <- c()
    for (i in seq_along(t1)) {
        epsilon0 <- apply(ii0, 1, function(x) sum(x*eta[idx1[i],]))
        oner <- (t0<=t1[i])*r0 + epsilon0    
        G1 <- rnorm(n, sd=sqrt(oner^2))
        W1 <- c(W1, sum(G1))
    }
    Ws[,k] <- W1/sqrt(n)
}

plot(t0, W0, type="s", col=lava::Col("darkred",0.5), lwd=3, ylim=range(c(W0,Ws)))
matplot(t1, Ws, type="s", col=lava::Col(1,0.2), add=TRUE)



library(gof)
n <- 2e2
m <- lvm(y~1)
#distribution(m,~y) <- uniform.lvm()
d <- sim(m,n)
y <- d$y

##goftest::cvm.test(y,null=pnorm)
##nortest::cvm.test(y)
##nortest::lillie.test(y)
ord <- order(y)
y <- y[ord]
F <- seq(n)/n
F0 <- pnorm((y-mean(y))/sd(y))
plot(y, F, type="s")
lines(y, F0, type="s", col="blue", lty=2)
D <- F-F0
K <- max(D, 1/n-D)
ks.test((y-mean(y))/sd(y),pnorm)

mc.test <- function(n,mu,sigma) {
    ys <- sort(rnorm(n,mu,sigma))
    Fs <- seq(n)/n
    Fs0 <- pnorm((ys-mean(ys))/sd(ys))
    Dsim <- Fs-Fs0
    max(Dsim, 1/n-Dsim)
}
Ksim <- replicate(1e4, mc.test(n,mean(y),sd(y)))
mean(Ksim>K)
nortest::lillie.test(y)

y <- sort(y)
l <- lm(y~1)
ic <- iid(l)
beta <- coef(l)
X <- model.matrix(l)

fd <- function(beta) {
    r <- y-X%*%beta
    r0 <- r/sqrt(sum(r^2)/(length(y)-length(beta)))
    return(r0)
}
fds <- function(beta)
    pnorm(fd(beta))/n
#apply(fd(beta),2,cumsum)
eta <- numDeriv::jacobian(fds,beta)
r0 <- fd(beta)
F <- seq(n)/n
F0 <- pnorm(r0)
D <- F-F0

sim <- function() {
    W <- numeric(n)
    G <- rnorm(n)
    icG <- colSums(apply(ic, 2, function(x) x*G))
    for (i in seq(n)) {
        W[i] <- (F-F0)[i] + icG%*%eta[i,]
    }
    max(abs(W))
}
Ks <- replicate(1e3, sim())
head(Ks)
mean(max(abs(D))>Ks)

nortest::lillie.test(y)

l <- lm(y~1)
cumres(l)

plot(cumres(l,seq(n)))



n <- 500
t <- seq(0,10,length.out=n)
x <- dnorm(t)
dx <- t*dnorm(t)
dx2 <- t^2*dnorm(t)
y <- dnorm(t-3)

l <- lm(y~x+dx+dx2)
yhat <- predict(l)
plot(y~t)
points(yhat ~ t, col="red", cex=0.5)

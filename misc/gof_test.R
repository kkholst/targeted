Rcpp::compileAttributes("../R-package/gof")
devtools::load_all("../R-package/gof")

surgunit <- read.csv("surgunit.csv")
l <- lm(log(survival) ~ bloodclot + enzyme + prognostic, surgunit)
(g <- cumres(l,R=1e4))
if (interactive()) par(mfrow=c(2,2)); plot(g)

library("futile.logger")
set.seed(1)
n <- 1000
flog.info("Simulating (n=%s)", n)
x <- rnorm(n)
z <- seq(0,1,length.out=n)
y <- 0.1*x*x + x + z + rnorm(n)
d <- data.frame(y,x,z)
write.table(d, file="d.csv", row.names=FALSE, col.names=FALSE, sep=",")
l <- lm(y~x+z, data=d)
ii <- lava::iid(l)
r <- residuals(l)
t <- x ## Variable to order residuals after
d2 <- data.frame(r,t,ii)
write.table(d2, file="r.csv", row.names=FALSE, col.names=FALSE, sep=",")
X <- model.matrix(l)

gg <- cumres(l,R=1e4,subset.max=1e4)
if (interactive()) par(mfrow=c(2,2)); plot(gg)
coef(gg)

g <- new(gof::CumRes, r, -X, ii);
g$order(X[,-1])
W <- g$obs()
head(g$sample1(matrix(nrow=0,ncol=0)))
idx <- cbind(c(1,2,3),c(1,2,3))
g$sample1(idx)
ks0 <- SupTest(W[,1])
mean(g$samplestat(1e3, matrix(nrow=0,ncol=0), FALSE)[,1]>ks0)


library(lava)
m <- lvm(list(c(y1,y2,y3)~eta,eta~x)); latent(m) <- ~eta
functional(m,eta~x) <- function(x) 0.3*x^2
d <- sim(m,100)

e <- estimate(m,d)
(g <- cumres(e,eta~x,R=1000))
if (interactive()) plot(g)
cumres(e, eta~x, full=TRUE)

x <- function(p) predict(e,x=~y2+y3,p=p)[,"eta"]
cumres(e,y1~eta,R=1000)
g <- cumres(e,"y1",x=x,R=1000)
if (interactive()) plot(g)



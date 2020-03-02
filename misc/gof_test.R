Rcpp::compileAttributes("../R-package/gof")
devtools::load_all("../R-package/gof")

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

gg <- cumres(l,R=1e3)
par(mfrow=c(2,2)); plot(gg)
coef(gg)

g <- new(gof::CumRes, r, -X, ii);
g$reorder(predict(l))
ks0 <- SupTest(g$obs())
mean(g$samplestat(1e4, seq(n)-1, FALSE)[,1]>ks0)

g$reorder(x)
ks0 <- SupTest(g$obs())
mean(g$samplestat(1e4, seq(n)-1, FALSE)[,1]>ks0)

g$reorder(z)
ks0 <- SupTest(g$obs())
mean(g$samplestat(1e4, seq(n)-1, FALSE)[,1]>ks0)

g$rnorm()[1:10]

g$reorder(t)
t0 <- g$t
W0 <- g$obs()
t1a <- t0[mets::fast.approx(t0, seq(t0[1], t0[n], length.out=50))]
t1b <- quantile(t0, seq(0,1,length.out=50))
t1 <- sort(union(t1a,t1b))
pos <- 1
idx <- numeric(n) # grouping
for (i in seq_along(idx)) {
    if (t0[i]>t1[pos]) {
        pos <- pos+1        
    }
    idx[i] <- pos
}
idx1 <- mets::dby(data.frame(id=idx, pos=seq_along(idx)), pos~id, pos=max, REDUCE=T)$pos

Rplot <- 200
Ws <- matrix(0,length(idx1),Rplot)
for (i in seq(Rplot))
    Ws[,i] <- g$sample1(idx1-1)
plot(t0, W0, type="n", ylim=range(W0,Ws))
matplot(t0[idx1], Ws, type="s", col=Col(1,0.2), lty=1, lwd=2)
lines(t0, W0, type="s", col=lava::Col("darkred", 0.7), lwd=5, ylim=range(W0,Ws))

gof:::gof_samplestat(g)

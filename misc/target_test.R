library("target")
library("futile.logger")

flog.info("Simulating")
set.seed(1)
m <- lvm(a[-2] ~ 1*x+z,
         linpred.target[1] ~ 1,
         linpred.nuisance[-1] ~ 2*x+z)
distribution(m,~a) <- binomial.lvm("logit")
m <- binomial.rr(m, "y","a","linpred.target","linpred.nuisance")
d <- sim(m,1e3)
write.csv(subset(d, select=c(y,a,x,z)), "d.csv", row.names=FALSE)

d <- read.csv(file="d.csv")
summary(fit <- target::riskreg(y ~ a | 1 | x+z | 1, data=d, type="rr"))
flog.info("target::riskreg (no interaction): %s", coef(fit))
summary(fit <- target::riskreg(y ~ a | x | x+z | 1, data=d, type="rr"))
flog.info("target::riskreg (with interaction): %s", coef(fit))

x2 <- model.matrix(~x+z,d)
y <- with(d, cbind(y))
a <- with(d, cbind(a))
w <- x1 <- cbind(rep(1, nrow(d)))
theta <- rbind(1, 1,1,1)
res <- target:::bin_logl(y,a,x1,x2, theta, w, type="rr", indiv=TRUE)
flog.info("likelihood: %s", sum(res))
res <- target:::bin_logl(y,a,x1,x2, theta*0, w, type="rr", indiv=TRUE)
flog.info("likelihood: %s", sum(res))

m <- new(target:::RiskReg, y,a,x1,x2,x2,w, "rr")
m$update(theta)

flog.info("likelihood: %s (class method)", m$logl())
head(m$dlogl(TRUE))

op <- nlminb(theta, function(p) {m$update(p); -m$logl()}, function(p) {m$update(p); -m$dlogl(FALSE)})
f <- function(p) {
    target:::bin_logl(y,a,x1,x2, rbind(p), w, type="rr", indiv=FALSE)[1]
}
op
numDeriv::jacobian(f,op$par)

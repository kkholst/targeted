devtools::load_all("~/Software/target/R-package/targeted")


m <- lvm(a[-2] ~ 1*x,
        linpred.target[1] ~ 1,
        linpred.nuisance[-1] ~ 2*x)
distribution(m,~a) <- binomial.lvm("logit")
m <- binomial.rr(m, "y","a","linpred.target","linpred.nuisance")

d <- sim(m,1e4)
a <- riskreg(y ~ a | 1 | x | x, data=d, type="rr")
a

data(iris)
g <- condlogit(Species ~ Sepal.Width*Petal.Length, data=iris)
pr <- predict(g, newdata=iris, wide=TRUE)
cl <- colnames(pr)[apply(pr, 1, which.max)]
table(iris$Species, cl)
a



library("tmle")
W = cbind(X=d$x)
system.time(a1 <- with(d, tmle(y, a, W, Qform="Y~A+X", gform="A~X", family="binomial")))

## Data adaptive (SuperLearner)
system.time(a2 <- with(d, tmle(y, a, cbind(1,x), family="binomial")))
summary(a2)


aa <- riskreg(y ~ a | x | x | x, data=d, type="rr")


onerun <- function(...) {
    d <- sim(m,5e3)
    t1 <- system.time(a1 <- riskreg(y ~ a | 1 | x | x, data=d, type="rr"))
    W = cbind(X=d$x)
    t2 <- system.time(a2 <- with(d, tmle(y, a, W, Qform="Y~A+X", gform="A~X", family="binomial")))
    t3 <- system.time(a3 <- with(d, tmle(y, a, W, family="binomial")))
    c(estimate.target=coef(a1), se.target=vcov(a1)^.5,
      with(a2$estimates$RR, c(estimate.tmle=log.psi, se.tmle=var.log.psi^.5)),
      with(a3$estimates$RR, c(estimate.tmle2=log.psi, se.tmle2=var.log.psi^.5)),      
      time.target=t1[1], time.tmle=t2[1], time.tmle2=t3[1])
}

val <- sim(onerun, 500, mc.cores=8, messages=1)

summary(val, estimate=c(1,3,5), se=c(2,4,6), true=1)


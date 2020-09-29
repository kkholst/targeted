devtools::load_all("~/Software/target/R-package/targeted")
library("mets")

data(iris)
set.seed(1)
dat <- csplit(iris,2) ## Random split

g1 <- condlogit(Species ~ Sepal.Width + Petal.Length, data=dat[[1]])
m1 <- mets::mlogit(Species ~ Sepal.Width*Petal.Length, data=dat[[1]])

pr1 <- predict(g1, newdata=dat[[2]], wide=TRUE)



alt <- dat[[1]]$Species
X <- model.matrix(~Sepal.Width*Petal.Length, dat[[1]])
X0 <- model.matrix(~1, dat[[1]])
dd <- .mlogit_expand(alt,X,rep(1,length(alt)),unique(alt))

M <- new(dcmodel, dd$choice, dd$alt, dd$id_idx, list(X,X,X), dd$weights)
theta <- 1:10
M$update(theta)

##g2 <- condlogit(Species ~ Sepal.Width*Sepal.Length, data=dat[[1]])##'
pr2 <- predict(g2, newdata=dat[[2]], wide=TRUE)


library("targeted")

set.seed(1)
x <- runif(1e2, -5, 5)
pr <- lava::expit(-1 + x)
y <- rbinom(length(pr), 1, pr)
pv <- pava(y, x)
plot(pr ~ x, cex=0.3)
with(pv, lines(sort(x)[index], value, col="red", type="s"))

bf <- targeted::isoreg(x=x, y=y)

af <- with(pv, approxfun(sort(x)[index], value, rule=2))

f <- stats::isoreg(x,y)
o

plot(f)
lines(pv, lines(sort(x)[index], value, col="orange", type="s"))

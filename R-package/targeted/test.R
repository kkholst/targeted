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

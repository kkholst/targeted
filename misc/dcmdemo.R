devtools::load_all("~/Software/target/R-package/targeted")
n <- 100
K <- 20
x <- cbind(1,matrix(rnorm(K*n, mean=2), ncol=K))
beta <- numeric(K+1)
beta[1:min(K,5)] <- c(-1,1,0,0,0)[1:min(K,5)]
lp <- x%*%beta
pr <- lava::expit(lp)
y <- rbinom(n,1,pr)
d <- data.frame(y=y, x=x[,-1], y0=letters[y+1])
colnames(d) <- c("y",paste0("x",1:K), "y0")

kk <- K
f <- as.formula(paste0("y0~",paste0(paste0("x",1:kk),collapse="+")))
dd <- targeted::.mlogit_expand(y, x, weights=rep(1,length(y)), alts=c(0,1))

m <- new(dcmodel, dd$choice, dd$alt, dd$id_idx,
         list(dd$x, dd$x[,NULL],dd$x[,NULL]), dd$weights)
m$update(beta,1)
m$logl()

system.time(g0 <- condlogit(f, data=d, basealt=1))
system.time(g1 <- condlogit(alt=d$y0, x=x))
system.time(g <- glm(y ~ -1+x, family=binomial))

##     testthat::expect_equivalent(logLik(g), logLik(g0))
##     testthat::expect_equivalent(logLik(g1), logLik(g0))

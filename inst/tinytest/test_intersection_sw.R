library("tinytest")

# simulate RCT data with treatment trt, terminal event R, and outcome Y only
# observed when R=1
simdata <- function(n, mu,sigma,lambda,tau,gamma,trteff1,trteff2){
  trt<-rbinom(n,1,0.5)
  Times<-rexp(n,rate=lambda+trteff1*trt)
  R<-ifelse(Times>tau,0,1)
  Y<-(1-R)*rnorm(n,mean=mu+trteff2*trt,sd=sigma)
  Ytilde<-(1-R)*Y+gamma*R
  data.frame(trt,R,Y,Ytilde)
}

# estimation of parameters of interest
est <- function(data){
  thetahat1 <- with(data,mean(trt*(1-R))/mean(trt)-mean((1-trt)*(1-R))/mean(1-trt))
  thetahat2 <- with(data,mean(Y*trt*(1-R))/mean(trt*(1-R))-mean(Y*(1-trt)*(1-R))/mean((1-trt)*(1-R)))
  thetahat3 <- with(data,mean(trt*Ytilde)/mean(trt)-mean((1-trt)*Ytilde)/mean(1-trt))
  thetahat <- c(thetahat1,thetahat2,thetahat3)
  psi10 <- with(data, ((1-trt)*(1-R)-mean((1-trt)*(1-R)))/mean(1-trt)-(mean((1-trt)*(1-R))/(mean(1-trt)*mean(1-trt)))*(1-trt-mean(1-trt)))
  psi11 <- with(data, (trt*(1-R)-mean(trt*(1-R)))/mean(trt)-(mean(trt*(1-R))/(mean(trt)*mean(trt)))*(trt-mean(trt)))
  psi20 <- with(data, ((1-trt)*(1-R)*Y-mean((1-trt)*(1-R)*Y))/mean((1-R)*(1-trt))-(mean((1-trt)*(1-R)*Y)/(mean((1-trt)*(1-R))*mean((1-trt)*(1-R))))*((1-R)*(1-trt)-mean((1-R)*(1-trt))))
  psi21 <- with(data, (trt*(1-R)*Y-mean(trt*(1-R)*Y))/mean((1-R)*trt)-(mean(trt*(1-R)*Y)/(mean(trt*(1-R))*mean(trt*(1-R))))*((1-R)*trt-mean((1-R)*trt)))
  psi30 <- with(data,((1-trt)*Ytilde-mean((1-trt)*Ytilde))/(mean(1-trt))-(mean((1-trt)*Ytilde)/(mean(1-trt)*mean(1-trt)))*(1-trt-mean(1-trt)))
  psi31 <- with(data,(trt*Ytilde-mean(trt*Ytilde))/(mean(trt))-(mean(trt*Ytilde)/(mean(trt)*mean(trt)))*(trt-mean(trt)))
  infl1 <- psi11 - psi10
  infl2 <- psi21 - psi20
  infl3 <- psi31 - psi30
  lava::estimate(coef = thetahat, IC  = cbind(infl1, infl2, infl3))
}

## signed wald intersection test
test_intersection_sw2 <- function(thetahat,
                                  sigmahat,
                                  weights, Nsim.
                                  null = 10000,
                                  noninf = rep(0, length(thetahat))) {
  W <- diag(weights)
  p <- length(thetahat)
  ## sqrtS <- pracma::sqrtm(sigmahat)
  sqrt.sigma <- expm::sqrtm(sigmahat)
  sqrt.sigma.inv <- expm::sqrtm(solve(sigmahat))
  uhat <- (sqrt.sigma.inv %*% W %*% matrix(thetahat - noninf, length(thetahat), 1))
  ## ||uhat-u||^2 = (uhat-u)^T(uhat -h) =>xo
  ## u^T I u - 2 * uhat^T u + K  s.t. Au =< b
  ## QP: min_u -d' u + 1/2 u' D u, s.t. Au >= b
  lscon <- function(uhat) {
    opt0 <- quadprog::solve.QP(
      Dmat = diag(nrow = p),
      dvec = uhat, Amat = -sqrt.sigma,
      factorized = TRUE # :=R, D = R'R
    )
    sum((uhat - opt0$sol)^2)
  }
  genSWobs = lscon(uhat)
  genSW.sim <- rep(0, Nsim.null)
  tmpmat <- sqrt.sigma.inv %*% W %*% sqrt.sigma
  sigma.usim <- tmpmat %*% t(tmpmat)
  uu <- mets::rmvn(Nsim.null, mu = rep(0, p), sigma = sigma.usim)
  for (i in 1:Nsim.null) {
    genSW.sim[i] <- lscon(uu[i, ])
  }
  pval <- length(genSW.sim[genSW.sim >= genSWobs]) / Nsim.null
  return(list(statistic = genSWobs, p.value = pval))
}



# simulate data and estimate parameters
sim_args <- list(
  n = 500,
  lambda = 0.07,
  tau = 2,
  mu = 40,
  trteff1 = -0.018,
  trteff2 = 3.2,
  sigma = 15,
  gamma = 15
)
set.seed(1)
dat <- do.call(simdata, sim_args)
e <- est(dat)


test_intersection_test_sw <- function() {
  # check random seed works
  set.seed(1)
  e1 <- test_intersection_sw(coef(e), vcov(e))
  set.seed(1)
  e2 <- test_intersection_sw(coef(e), vcov(e))
  expect_equal(e1$p.value, e2$p.value)
  expect_equivalent(e1, e2)

  # equal weights
  e1 <- test_intersection_sw(coef(e), vcov(e))
  e2 <- test_intersection_sw2(coef(e), vcov(e), weights = c(1,1,1))
  expect_true(abs(e2$statistic - e1$statistic) < 1e-3)
  expect_true(abs(e2$p.value - e1$p.value) < 0.01)

  # unequal weights
  w <- c(1 / 2, 1 / 4, 1 / 4)
  e1 <- test_intersection_sw(coef(e), vcov(e), weights = w)
  e2 <- test_intersection_sw2(coef(e), vcov(e), weights = w)
  expect_true(abs(e2$statistic - e1$statistic) < 1e-3)
  expect_true(abs(e2$p.value - e1$p.value) < 0.01)

  # non-inferiority margins
  noninf <- c(-0.05, 0, -0.05)
  e1 <- test_intersection_sw(coef(e), vcov(e), weights = w, noninf=noninf)
  e2 <- test_intersection_sw2(coef(e), vcov(e), weights = w, noninf=noninf)
  expect_true(abs(e2$statistic - e1$statistic) < 1e-3)
  expect_true(abs(e2$p.value - e1$p.value) < 0.01)

  # 2d par.
  w <- c(1 / 4, 3 / 4)
  e1 <- test_intersection_sw(coef(e)[1:2], vcov(e)[1:2, 1:2], weights = w)
  e2 <- test_intersection_sw2(coef(e)[1:2], vcov(e)[1:2,1:2], weights = w)
  expect_true(abs(e2$statistic - e1$statistic) < 1e-3)
  expect_true(abs(e2$p.value - e1$p.value) < 0.01)

  # 1d par.
  b <- abs(coef(e)[1])
  v <- vcov(e)[1]
  e0 <- test_intersection_sw(b, v)
  z <- b / v**.5
  expect_equivalent(pnorm(z, lower.tail = FALSE), e0$p.value)

  e0 <- test_intersection_sw(-0.2, v)
  expect_equivalent(1, e0$p.value)
}

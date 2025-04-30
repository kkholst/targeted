# superlearner
library("tinytest")
library("SuperLearner")

sim1 <- function(n = 5e3) {
   x1 <- rnorm(n, sd = 2)
   x2 <- rnorm(n)
   lp <- x2*x1 + cos(x1)
   yb <- rbinom(n, 1, lava::expit(lp))
   y <-  lp + rnorm(n, sd = 0.5**.5)
   data.frame(y, yb, x1, x2)
}
set.seed(1)
d <- sim1(1e3)

test_sl <- function() {
  m <- list(
    "mean" = predictor_glm(y ~ 1),
    "glm"  = predictor_glm(y ~ x1 + x2),
    "xgb"  = predictor_xgboost(y ~ x1 + x2, eta=.5, nrounds=100),
    "mars" = predictor_mars(y ~ x1+x2, degree=2)
  )
  c1 <- cv(m, data=d, rep=2)

  s <- predictor_sl(m, nfolds=10)
  s$estimate(d)
  b <- summary(s, nfolds=10, rep=2, data=d)
  # sm <- cv(list(s), d, rep=1, model.score=mse) |> summary()
  sm <- summary(b)["sl",,"mse"]

  # comparison with SuperLearner library
  slib <- c("SL.mean",
            "SL.glm",
            "SL.xgboost",
            "SL.earth"
            )
  s1 <- with(d, SuperLearner(cbind(y),
                             data.frame(x1, x2),
                             SL.library = slib))

  s2 <- with(d, CV.SuperLearner(cbind(y),
                                data.frame(x1, x2),
                                SL.library = slib
                                ))
  sm2 <- as.numeric(summary(s2)$Table[1,-1])

  ci2 <- c(sm2[1]-3*sm2[2], sm2[1]+3*sm2[2])
  expect_true(sm[1]>ci2[1] & sm[1]<ci2[2])
}
test_sl()

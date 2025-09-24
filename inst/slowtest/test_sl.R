# superlearner
library("tinytest")
suppressPackageStartupMessages(
  library("SuperLearner")
)

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

# comparison with the SuperLearner pacakge
test_sl <- function() {
  m <- list(
    "mean" = learner_glm(y ~ 1),
    "glm"  = learner_glm(y ~ x1 + x2),
    "xgb"  = learner_xgboost(y ~ x1 + x2, eta = .5, nrounds = 100),
    "mars" = learner_mars(y ~ x1 + x2, degree = 2)
  )
  c1 <- cv(m, data = d, rep = 2)
  learners_mse <- summary(c1)[,,"mse"][,"mean"]

  s <- learner_sl(m, nfolds = 10)
  b <- cv(s, nfolds = 10, rep = 2, data = d)
  # sm <- cv(list(s), d, rep=1, model.score=mse) |> summary()
  sm <- summary(b)["sl",,"mse"]
  learners_mse_sl <- summary(b)[-1,,"mse"][,"mean"]
  # rough comparison of mse calculated from sl with cv of individual learners
  expect_true(mean((learners_mse - learners_mse_sl)**2)<0.25)

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


test_metalearners <- function() {
  m <- list(
    "mean" = learner_glm(y ~ 1),
    "glm" = learner_glm(y ~ x1 + x2),
    "glm2" = learner_glm(y ~ x2)
  )

  s1 <- learner_sl(m, nfolds = 10)
  s2 <- learner_sl(m, nfolds = 10, meta.learner = metalearner_nnls2)
  s3 <- learner_sl(m, nfolds = 10, meta.learner = metalearner_convexcomb)

  b1 <- cv(s1, nfolds = 10, rep = 2, data = d)
  b2 <- cv(s2, nfolds = 10, rep = 2, data = d)
  b3 <- cv(s3, nfolds = 10, rep = 2, data = d)

  expect_equivalent(summary(b1)[, , "weight"], summary(b2)[, , "weight"], tolerance = 0.05)
  expect_equivalent(summary(b2)[, , "weight"], summary(b3)[, , "weight"], tolerance = 0.05)
  expect_equivalent(summary(b1)[, , "weight"], summary(b3)[, , "weight"], tolerance = 0.05)
}
test_metalearners()

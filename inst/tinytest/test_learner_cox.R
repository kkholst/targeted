library("tinytest")
library("survival")

data(sTRACE, package="mets")

test_learner_cox <- function() {
  mod <- learner_cox(Surv(time, status>0) ~ age + strata(sex))
  mod$estimate(sTRACE)

  m <- coxph(Surv(time, status>0) ~ age + strata(sex), data=sTRACE)
  expect_true(abs(coef(mod$fit)- coef(m)) < 1e-5)

  pr1 <- mod$predict(data.frame(age=50, sex=0:1), time=10)
  pr <- predict(
    m,
    newdata=data.frame(time=10,status=0, age=50, sex=0:1),
    type="survival"
  )
  expect_true(mean((pr- pr1)^2) < 1e-6)
}
test_learner_cox()

library("tinytest")
suppressPackageStartupMessages(
  library("mets")
)

data("truncatedscore")
a <- estimate_truncatedscore(
  data = truncatedscore,
  mod.y = y ~ a,
  mod.r = r ~ a,
  mod.a = a ~ 1,
  mod.event = mets::Event(time, status>0) ~ 1,
  time = 2
)

# testing the survival probability estimates
test_truncatedscore_survival <- function() {
  s <- phreg(Event(time, status > 0) ~ strata(a), data = truncatedscore)
  pr <- predict(s, times = 2, data.frame(a = 0:1), se = 0)$surv[, 1]
  expect_equivalent(coef(a)[4:5], pr, tolerance=1e-3)
}
test_truncatedscore_survival()


library("mmrm")
library("lava")

data(fev_data, package="mmrm")

fevd <- subset(fev_data, select=-c(VISITN2, AVISIT)) |>
  mets::drename(FEV ~ FEV1)
dw <- mets::fast.reshape(fevd, id="USUBJID",
                         varying="FEV", num="VISITN") |>
  subset(select=-c(FEV4))
dw <- na.omit(dw) # Data with complete-case in wide-format

dl <- mets::fast.reshape(
              dw,
              varying = list(FEV=c("FEV1", "FEV2", "FEV3")),
              ) |>
  transform(AVISIT=factor(num),
            ARMCD=factor(ARMCD),
            USUBJID=factor(USUBJID)) # Data in long format


a0 <- mmrm(
  FEV ~ -1 + AVISIT + ARMCD : AVISIT + us(AVISIT | USUBJID),
  data = dl, reml = FALSE
)
##colSums(score(a0))

a <- mmrm(
  FEV ~ -1 + AVISIT + ARMCD : AVISIT + us(AVISIT | USUBJID),
  data = dl, reml = FALSE,
  optimizer = "nlminb",
  optimizer_control = list(
    eval.max = 1000,
    iter.max = 1000,
    rel.tol = 1e-9
  )
)

test_mmrm_subject <- function(fit, data_long) {
  ref <- targeted:::.mmrm_subjects(fit = fit, theta = NULL)

  new_data_long <- data_long
  new_data_long$AVISIT <- factor(new_data_long$AVISIT, levels = c("FEV3", "FEV1", "FEV2"))
  new_data_long$USUBJID <- factor(new_data_long$USUBJID,
                                  levels = levels(new_data_long$USUBJID)[c(2, 1, 3:65)])

  new_fit <- mmrm(
    FEV ~ -1 + AVISIT + ARMCD : AVISIT + us(AVISIT | USUBJID),
    data = new_data_long, reml = FALSE,
    optimizer = "nlminb",
    optimizer_control = list(
      eval.max = 1000,
      iter.max = 1000,
      rel.tol = 1e-9
    )
  )

  ms <- targeted:::.mmrm_subjects(fit = new_fit, theta = NULL)

  ## full frame in the model fit is ordered by id and visit levels
  new_ord <- c(2, 3, 1)
  tinytest::expect_true(
    all(ref[[1]]$visits == ms[[2]]$visits[new_ord])
  )
  tinytest::expect_true(
    all(ref[[1]]$Sigma - ms[[2]]$Sigma[new_ord, new_ord] < 10-8)
  )

}
test_mmrm_subject(fit = a, data_long = dl)

test_mmrm_varcor <- function(fit) {

  ref <- mmrm::VarCorr(fit)
  tmp <- targeted:::.mmrm_varcor(
                      fit = fit,
                      theta = mmrm::component(fit, "theta_est")
                    )

  tinytest::expect_equal(
              ref,
              tmp
            )

  tmp <- targeted:::.mmrm_varcor(
                      fit = fit,
                      theta = mmrm::component(fit, "theta_est") + 0.1
                    )

  tinytest::expect_true(
              all(abs(ref - tmp) > 1e-2)
            )

}

test_mmrm_score <- function(fit) {

  expect_true(mean(colMeans(score(fit)))<1e-9)
  ## f <- mmrm2sigma(a)
  ## transform(estimate(a), f)
  ll <- c(targeted:::.mmrm_loglik(fit, beta=coef(fit)), logLik(fit))
  S0 <- numDeriv::jacobian(\(p) targeted:::.mmrm_loglik(fit, beta=p),
                           coef(fit)+1)
  S <- targeted:::.mmrm_score_beta(fit, beta=coef(fit)+1)
  U0 <- numDeriv::jacobian(\(p) targeted:::.mmrm_loglik(fit, theta=p), fit$theta_est+1)
  U <- targeted:::.mmrm_score_theta(fit, theta=fit$theta_est+1)

  expect_equivalent(ll[1], ll[2])
  expect_equivalent(as.numeric(S0), colSums(S))
  expect_equivalent(as.numeric(U0), colSums(U))
}

test_mmrm_score(a)


# Transformation to real variance/covariance scale
test_mmrm_sigma <- function() {
  f <- mmrm2sigma(a)
  ea <- estimate(a)
  avar <- transform(ea, f)
  # check id is there
  expect_equivalent(
              index(avar), sort(levels(dl$USUBJID))
  )
  covarest <- f(vec=FALSE)
  expect_true(all(dim(covarest) == c(3L, 3L)))

  ## Comparison with lava
  m <- lvm(c(FEV1,FEV2,FEV3) ~ ARMCD) |>
    covariance(~FEV1+FEV2+FEV3, pairwise=TRUE)
  es <- estimate(m, dw)
  score(es)
  e <- estimate(es)

  meanpar_mmrm <- subset(ea, 1:6)
  meanpar_lava <- subset(e, 1:6)
  varpar_mmrm <- subset(avar)
  varpar_lava <- subset(e, c(7,10,8,11,12,9))

  tinytest::expect_true(
              mean((vcov(meanpar_mmrm)-vcov(meanpar_lava))^2)<1e-3
            )
  tinytest::expect_true(
              mean((coef(meanpar_mmrm)-coef(meanpar_lava))^2)<1e-9
          )
  tinytest::expect_true(
            mean((vcov(varpar_mmrm)-vcov(varpar_lava))^2)<1e-3
            )
  tinytest::expect_true(
              mean((coef(varpar_mmrm)-coef(varpar_lava))^2)<1e-9
            )
}
test_mmrm_sigma()

## Grouped covariance
b <- mmrm(
  FEV ~ -1 + AVISIT + ARMCD:AVISIT + us(AVISIT | ARMCD / USUBJID),
  data = dl, reml = FALSE
)
test_mmrm_score(b)

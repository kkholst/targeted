
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
colSums(score(a0))

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
colSums(score(a))

f <- mmrm2sigma(a)
transform(estimate(a), f)

fit <- a
ll <- c(targeted:::.mmrm_loglik(fit, beta=coef(fit)), logLik(fit))
S0 <- numDeriv::jacobian(\(p) .mmrm_loglik(fit, beta=p),
                         coef(fit)+1)
S <- targeted:::.mmrm_score_beta(fit, beta=coef(fit)+1)

tinytest::expect_equivalent(ll[1], ll[2])
tinytest::expect_equivalent(as.numeric(S0), colSums(S))


# Transformation to real variance/covariance scale
f <- mmrm2sigma(a)
ea <- estimate(a)
avar <- transform(ea, f)
tinytest::expect_equivalent(
            index(avar), sort(levels(dl$USUBJID))
)

covarest <- f(vec=FALSE)
tinytest::expect_true(all(dim(covarest) == c(3L, 3L)))

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

## Grouped covariance

b <- mmrm(
  FEV ~ -1 + AVISIT + ARMCD:AVISIT + us(AVISIT | ARMCD / USUBJID),
  data = dl, reml = FALSE
)

fit <- b
ll <- c(targeted:::.mmrm_loglik(fit, beta=coef(fit)), logLik(fit))
tinytest::expect_equivalent(ll[1], ll[2])

S0 <- numDeriv::jacobian(\(p) targeted:::.mmrm_loglik(fit, beta=p),
                         coef(fit)+1)
S <- targeted:::.mmrm_score_beta(fit, beta=coef(fit)+1)
tinytest::expect_equivalent(as.numeric(S0), colSums(S))

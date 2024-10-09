
##' AIPW estimator
##'
##' AIPW for the mean (and linear projections of the EIF) with missing
##' observations
##' @export
##' @param response_model Model for the response given covariates (ml_model or
##'   formula)
##' @param propensity_model Optional missing data mechanism model (propensity
##'   model) (ml_model or formula)
##' @param data data.frame
##' @param ... additional arguments (see [cate()])
##' @param formula design specifying the OLS estimator with outcome given by the
##'   EIF
##' @examples
##' m <- lvm(y ~ x+z, r ~ x)
##' distribution(m,~ r) <- binomial.lvm()
##' transform(m, y0~r+y) <- function(x) { x[x[,1]==0,2] <- NA; x[,2] }
##' d <- sim(m,1e3,seed=1)
##'
##' aipw(y0 ~ x, data=d)
aipw <- function(response_model,
                 propensity_model,
                 formula = ~1,
                 data,
                 ...) {
  if (inherits(response_model, "formula")) {
    response_model <- ML(response_model)
  }
  resp <- lava::getoutcome(response_model$formula)
  r <- !is.na(model.frame(
          as.formula(paste0(resp, "~1")),
    data = data, na.action = na.pass
  )) * 1
  data[, "R_"] <- r[, 1]
  if (base::missing(propensity_model)) {
      propensity_model <- update(response_model$formula, as.formula("R_ ~ ."))
  }
  if (inherits(propensity_model, "formula")) {
    propensity_model <- ML(propensity_model, family=binomial)
  }
  cate(response_model=response_model,
       propensity_model=propensity_model,
       cate_model = formula,
      data = data, contrast = 1, stratify = TRUE,
      ...
  )
}

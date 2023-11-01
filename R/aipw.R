
##' AIPW estimator
##'
##' AIPW for the mean (and linear projections of the EIF) with missing
##' observations
##' @export
##' @param response_model Model for the response given covariates (ml_model or
##'   formula)
##' @param data data.frame
##' @param formula design specifying the OLS estimator with outcome given by the
##'   EIF
##' @param missing_model Optional missing_model (ml_model or formula). By
##'   default will use the same design as the response_model.
##' @param ... arguments to cate
##' @examples
##' m <- lvm(y ~ x+z, r ~ x)
##' distribution(m,~ r) <- binomial.lvm()
##' transform(m, y0~r+y) <- function(x) { x[x[,1]==0,2] <- NA; x[,2] }
##' d <- sim(m,1e3,seed=1)
##'
##' aipw(y0 ~ x, data=d)
aipw <- function(response_model, data,
                 formula = ~1,
                 missing_model,
                 ...) {
  if (inherits(response_model, "formula")) {
    response_model <- ML(response_model)
  }
  resp <- lava::getoutcome(response_model$formula)
  r <- !is.na(model.frame(as.formula(paste0(resp, "~1")), data = data, na.action = na.pass)) * 1
  data[, "R_"] <- r[, 1]
  if (base::missing(missing_model)) {
      missing_model <- update(response_model$formula, as.formula("`_R` ~ ."))
  }
  if (inherits(missing_model, "formula")) {
    missing_model <- ML(missing_model, family=binomial)
  }
  formula <- update(formula, as.formula("R_ ~ ."))
  cate(formula, response_model, missing_model,
      data = data, contrast = 1, stratify = TRUE,
      ...
  )
}

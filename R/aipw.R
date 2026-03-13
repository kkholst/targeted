#' AIPW estimator
#'
#' AIPW for the mean (and linear projections of the EIF) with missing
#' observations
#' @export
#' @param response.model Model for the response given covariates (learner or
#'   formula)
#' @param propensity.model Optional missing data mechanism model (propensity
#'   model) (learner or formula)
#' @param data data.frame
#' @param ... additional arguments (see [cate()])
#' @param formula design specifying the OLS estimator with outcome given by the
#'   EIF (see `cate`)
#' @inheritParams deprecated_argument_names
#' @examples
#' m <- lava::lvm(y ~ x+z, r ~ x) |>
#'      lava::distribution(~ r, value = lava::binomial.lvm()) |>
#'      transform(y0~r+y, value = \(x) { x[x[,1]==0,2] <- NA; x[,2] })
#' d <- lava::sim(m,1e3,seed=1)
#'
#' aipw(y0 ~ x, data=d)
aipw <- function(response.model,
                 propensity.model,
                 formula = ~1,
                 data,
                 response_model = deprecated,
                 propensity_model = deprecated,
                 ...) {

  dvers <- "1.0.0"
  if (!missing(response_model)) {
    deprecate_arg_warn("response_model", "response.model", "aipw", dvers)
    response.model <- response_model
  }
  if (!missing(propensity_model)) {
    deprecate_arg_warn("propensity_model", "propensity.model", "aipw", dvers)
    propensity.model <- propensity_model
  }

  if (inherits(response.model, "formula")) {
    response.model <- learner_glm(response.model)
  }
  resp <- lava::getoutcome(response.model$formula)
  r <- !is.na(model.frame(
          as.formula(paste0(resp, "~1")),
    data = data, na.action = na.pass
  )) * 1
  data[, "R_"] <- r[, 1]
  if (base::missing(propensity.model)) {
      propensity.model <- update(response.model$formula, as.formula("R_ ~ ."))
  }
  if (inherits(propensity.model, "formula")) {
    propensity.model <- learner_glm(propensity.model, family = binomial)
  }
  res <- cate(
    response.model = response.model,
    treatment.model = propensity.model,
    cate.model = formula,
    data = data, contrast = TRUE, stratify = TRUE,
    ...
  )

  est <- estimate(coef = coef(res)[-1], IC = IC(res)[, -1, drop = FALSE])
  res$estimate <- est
  return(res)
}

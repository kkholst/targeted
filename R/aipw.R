#' AIPW estimator
#'
#' AIPW for the mean (and linear projections of the EIF) with missing
#' observations
#' @export
#' @param response.model (learner or formula)Model for the response given
#'   covariates
#' @param (learner or formula) propensity.model missing data mechanism model and
#'   if omitted a logistic regression model with the same covariates as
#'   `response.model` is used
#' @param data data.frame
#' @param ... additional arguments (see [cate()])
#' @param formula design specifying the OLS estimator with outcome given by the
#'   EIF (see `cate`)
#' @examples
#' m <- lava::lvm(y ~ x+z, r ~ x) |>
#'      lava::distribution(~ r, value = lava::binomial.lvm()) |>
#'      transform(y0~r+y, value = \(x) { x[x[,1]==0,2] <- NA; x[,2] })
#' d <- lava::sim(m,5e3,seed=1)
#'
#' aipw(y0 ~ x, ~ x + z, data=d)
aipw <- function(response.model,
                 propensity.model,
                 formula = ~1,
                 data,
                 ...) {

  if (inherits(response.model, "formula")) {
    response.model <- learner_glm(response.model)
  }
  resp <- lava::getoutcome(response.model$formula)
  for (nm in c("R_", "AIPW_Y_")) {
    if (nm %in% colnames(data)) {
      stop("`", nm, "` is used internally and not permitted in `data`")
    }
  }
  yval <- response.model$response(data, na.action = na.pass)
  data[, "R_"] <- !is.na(yval)
  ## Hand `cate()` a complete outcome. Rows with a missing outcome have
  ## R_ = 0 and hence zero weight in the AIPW score, so the value put in
  ## their place is irrelevant
  data[, "AIPW_Y_"] <- lava::na.pass0(yval)
  response.model <- response.model$clone(deep = TRUE)
  response.model$update("AIPW_Y_")
  if (base::missing(propensity.model)) {
    propensity.model <- update(response.model$formula, as.formula("R_ ~ ."))
  }
  if (inherits(propensity.model, "formula")) {
    propensity.model <- learner_glm(propensity.model, family = binomial)
  }
  propensity.model$update("R_")
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

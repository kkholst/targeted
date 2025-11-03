qprob <- function(corr) {
  return(0.5 - mets::pmvn(upper = cbind(0, 0), sigma = corr, cor = TRUE))
}

prob_fct <- function(x, alpha, corr) {
  q <- qprob(corr)
  res <- 0.5 * pchisq(x, 1, lower.tail = FALSE) +
    q * pchisq(x, 2, lower.tail = FALSE) - alpha
  return(res)
}

q_fct <- function(alpha, corr) {
  root <- uniroot(prob_fct,
    alpha = alpha,
    corr = corr,
    interval = c(0, 10)
  )$root
  return(root)
}

pval.marg <- function(thetahat, sigmahat, noninf = 0) {
  se <- sqrt(diag(sigmahat))
  z <- (thetahat - noninf) / se
  1 - pnorm(z, sd = 1)
}

test_proc_arg <- function(par, vcov, index = NULL, ...) {
  if (inherits(par, "estimate")) {
    vcov <- stats::vcov(par)
    par <- stats::coef(par)
  }
  args <- list(...) |>
    lapply(function(x) { rep(x, length.out=length(par))})
  argnames <- names(args)
  if (!is.null(index)) {}
  res <- c(list(par = par, vcov = vcov), args)
  if (!is.null(index)) {
    if (length(par) < length(index)) stop("wrong `index`")
    res$par <- res$par[index]
    res$vcov <- res$vcov[index, index, drop = FALSE]
    for (arg in argnames) {
      res[[arg]] <- res[[arg]][index]
    }
  }
  return(res)
}

#' @title Signed Wald intersection test
#' @description Calculating test statistics and p-values for the signed Wald
#'   intersection test given by \deqn{SW = \inf_{\theta \in \cap_{i=1}^n H_i}
#'   \{(\widehat{\theta}-\theta)^\top W\widehat{\Sigma}W
#'   (\widehat{\theta}-\theta)\} } with individual hypotheses for each
#'   coordinate of \eqn{\theta} given by \eqn{H_i: \theta_j < \delta_j} for some
#'   non-inferiority margin \eqn{\delta_j}, \eqn{j=1,\ldots,n}.
#
#' @param par (numeric) parameter estimates or `estimate` object
#' @param vcov (matrix) asymptotic variance estimate
#' @param noninf (numeric) non-inferiority margins
#' @param weights (numeric) optional weights
#' @param nsim.null number of sample used in Monte-Carlo simulation
#' @param index subset of parameters to test
#' @export
#' @author Klaus Kähler Holst, Christian Bressen Pipper
#' @return `htest` object
#' @examples
#' S <- matrix(c(1, 0.5, 0.5, 2), 2, 2)
#' thetahat <- c(0.5, -0.2)
#' test_intersection_sw(thetahat, S, nsim.null = 1e5)
#' test_intersection_sw(thetahat, S, weights = NULL)
#'
#' \dontrun{
#' # only on 'lava' >= 1.8.2
#' e <- estimate(coef = thetahat, vcov = S, labels = c("p1", "p2"))
#' lava::closed_testing(e, test_intersection_sw, noninf = c(-0.1, -0.1)) |>
#'   summary()
#' }
test_intersection_sw <- function(par,
                                 vcov,
                                 noninf = 0,
                                 weights = 1,
                                 nsim.null = 1e4,
                                 index = NULL) {
  obj <- test_proc_arg(
    par = par, vcov = vcov,
    noninf = noninf,
    weights = weights,
    index = index
  )
  par <- obj$par
  vcov <- obj$vcov
  noninf <- obj$noninf
  weights <- with(obj, weights / sum(weights))
  z <- (par - noninf) / diag(matrix(vcov))**.5
  if (length(z) == 1) {
    signwald <- (z**2) * (z >= 0)
    # 1-pnorm(z)
    pval <- ifelse(z >= 0,
      0.5 * pchisq(signwald, 1, lower.tail = FALSE), 1
    )
    return(
      structure(list(
        data.name = sprintf("H0: b <= %g", noninf[1]),
        statistic = c("Q" = unname(signwald)),
        estimate = c("b" = unname(par)),
        parameter = NULL,
        method = "Signed Wald Test",
        alternative = sprintf("HA: b > %g", noninf[1]),
        p.value = pval
      ), class = "htest")
    )
  }
  if (length(z) == 2L && is.null(weights)) { # exact calculations
    corr <- cov2cor(vcov)[1, 2]
    zmin <- min(z[1], z[2])
    zmax <- max(z[1], z[2])
    signwald.intersect <- ifelse(zmax >= 0 & zmin <= (corr * zmax), 1, 0) *
      zmax * zmax + ifelse(zmax >= 0 & zmin > (corr * zmax),
        (zmax * zmax + zmin * zmin - 2 * corr * zmax * zmin) /
          (1 - corr * corr),
        0
      )
    # critval.intersect <- q_fct(alpha, corr)
    pval.intersect <- ifelse(signwald.intersect > 0,
      prob_fct(signwald.intersect, 0, corr), 1
    )
  } else { # simulation-based inference
    sw <- .signedwald(par, vcov,
      noninf = noninf,
      weights = weights, nsim_null = nsim.null
    )
    signwald.intersect <- sw$test.statistic
    pval.intersect <- sw$pval
  }
  w <- paste0(format(weights, digits = 2), collapse = ", ")
  test.int <- structure(list(
    data.name = sprintf(
      "\n%s: theta =< [%s]\nw = [%s]",
      "Intersection null hypothesis",
      paste(noninf, collapse = ", "), w
    ),
    statistic = c("Q" = unname(signwald.intersect)),
    parameter = NULL,
    method = "Signed Wald Intersection Test",
    p.value = pval.intersect
  ), class = "htest")
  return(test.int)
}

#' @title One-sided Zmax test
#' @description Calculating test statistics and p-values for the
#'   onesided Zmax / minP test.z
#'
#'  Given parameter estimates \eqn{(\widehat{\theta}_1, \ldots,
#'  \widehat{\theta}_p)^\top} with approximate assymptotic covariance matrix
#'  \eqn{\widehat{S}}, let \eqn{ Z_i = \frac{\widehat{\theta}_i -
#'  \delta_i}{\operatorname{SE}(\widehat{\theta}_i)}} , where
#'  \eqn{\operatorname{SE}(\widehat{\theta}_i) = \widehat{S}_{ii}}. The Zmax
#'  test statistic is then \eqn{Z_{max} = \max \{Z_1,\ldots,Z_p\}}, and the
#'  null-hypothesis is \eqn{H_0: \theta_i \leq \delta_i, i=1,\ldots,p} with
#'  non-inferiority margin \eqn{\delta_i, i=1,\ldots,p}, for which the p-value
#'  is calculated as \eqn{ 1 - \Phi_R(Z_{max}) } with correlation matrix \eqn{R
#'  = \operatorname{diag}(S_{11}^{-0.5}, \ldots,
#'  S_{pp}^{-0.5})S\operatorname{diag}(S_{11}^{-0.5}, \ldots, S_{pp}^{-0.5})}.
#' @param par (numeric) parameter estimates or `estimate` object
#' @param vcov (matrix) asymptotic variance estimate
#' @param noninf (numeric) non-inferiority margins
#' @param index subset of parameters to test
#' @return `htest` object
#' @export
#' @author Christian Bressen Pipper, Klaus Kähler Holst
test_zmax_onesided <- function(par,
                               vcov,
                               noninf = 0,
                               index = NULL) {
  obj <- test_proc_arg(
    par = par,
    vcov = vcov,
    noninf = noninf,
    index = index
  )
  par <- obj$par
  vcov <- obj$vcov
  noninf <- obj$noninf
  z <- (par - noninf) / diag(vcov)^0.5
  zmax <- max(z)
  pval.zmax <- 1 -
    mets::pmvn(upper = rep(zmax, length(z)),
               sigma = cov2cor(vcov))[1]
  test.int <- structure(
    list(
      data.name = sprintf(
        "\n%s: theta =< [%s]",
        "Intersection null hypothesis",
        paste(noninf, collapse = ", ")
      ), statistic = c(Q = unname(zmax)), parameter = NULL,
      method = "Zmax/minP test", p.value = pval.zmax
    ),
    class = "htest"
  )
  return(test.int)
}

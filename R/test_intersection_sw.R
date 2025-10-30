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
#' @return list with Wald
#' @examples
#' S <- matrix(c(1, 0.5, 0.5, 2), 2, 2)
#' thetahat <- c(0.5, -0.2)
#' test_intersection_sw(thetahat, S, nsim.null = 1e5)
#' test_intersection_sw(thetahat, S, weights = NULL)
#'
#' \dontrun{ # only on 'lava' >= 1.8.2
#' e <- estimate(coef = thetahat, vcov = S, labels = c("p1", "p2"))
#' lava::closed_testing(e, test_intersection_sw, noninf = c(-0.1, -0.1)) |>
#'   summary()
#' }
test_intersection_sw <- function(par,
                                 vcov,
                                 noninf = NULL,
                                 weights = NULL,
                                 nsim.null = 1e4,
                                 index = NULL) {
  if (inherits(par, "estimate")) {
    vcov <- stats::vcov(par)
    par <- stats::coef(par)
  }
  if (is.null(noninf)) {
    noninf <- rep(0, length(par))
  }
  if (is.null(weights)) {
    weights <- rep(1, length(par))
  }
  if (!is.null(index)) {
    if (length(par) < length(index)) stop("wrong `index`")
    par <- par[index]
    vcov <- vcov[index, index, drop = FALSE]
    noninf <- noninf[index]
    weights <- weights[index]
  }
  weights <- weights / sum(weights)
  z <- (par - noninf) / diag(matrix(vcov))**.5
  if (length(z) == 1) {
    signwald <- (z**2) * (z >= 0)
    ## 1-pnorm(z)
    pval <- ifelse(z >= 0,
      0.5 * pchisq(signwald, 1, lower.tail = FALSE), 1
      )
    return(
      structure(list(
        data.name = sprintf("H0: b <= %g", noninf[1]),
        statistic = c("Q" = unname(signwald)),
        estimate = c("b"=unname(par)),
        parameter = NULL,
        method = "Signed Wald Test",
        # null.value =,
        # alternative = "one.sided",
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
      paste(noninf, collapse = ", "),  w
    ),
    statistic = c("Q" = unname(signwald.intersect)),
    parameter = NULL,
    method = "Signed Wald Intersection Test",
    p.value = pval.intersect
  ), class = "htest")
  return(test.int)
}

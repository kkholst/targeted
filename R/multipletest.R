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

#' @title Signed intersection Wald test
#' @description Calculating test statistics and p-values for the signed Wald
#'   test given by \deqn{SW = \inf_{\theta \in \cap_{i=1}^n H_i}
#'   \{(\widehat{\theta}-\theta)^\top W\widehat{\Sigma}W
#'   (\widehat{\theta}-\theta)\} } with individual hypotheses for each
#'   coordinate of \eqn{\theta} given by \eqn{H_i: \theta_j < \delta_j}
#' for some non-inferiority margin \eqn{\delta_j}, \eqn{j=1,\ldots,n}.
#'
#' @param par (numeric) parameter estimates
#' @param vcov (matrix) asymptotic variance estimate
#' @param noninf (numeric) non-inferiority margins
#' @param weights (numeric) optional weights
#' @param alpha (numeric) nominal level
#' @export
#' @author Klaus Kähler Holst Christian Bressen Pipper,
#' @return list with Wald
#' @examples
#' S <- matrix(c(1, 0.5, 0.5, 2), 2, 2)
#' thetahat <- c(0.5, -0.2)
#' test_sw(thetahat, S, nsim.null = 1e5)
#' test_sw(thetahat, S, weights=NULL)
test_sw <- function(par,
                    vcov,
                    noninf = rep(0, length(par)),
                    weights = rep(1, length(par)),
                    alpha = 0.05,
                    nsim.null = 1e4) {
  z <- (par - noninf) / diag(matrix(vcov))**.5
  if (length(z) == 1) {
    signwald <- (z**2) * (z >= 0)
    pval <- ifelse(signwald > 0,
      0.5 * pchisq(signwald, 1, lower.tail = FALSE), 1
      )
    return(
      structure(list(
        data.name = sprintf("H0: b <= %g", noninf[1]),
        statistic = c("Q" = unname(signwald)),
        estimate = c("b"=unname(par)),
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
    SignWald.intersect <- ifelse(zmax >= 0 & zmin <= (corr * zmax), 1, 0) *
      zmax * zmax + ifelse(zmax >= 0 & zmin > (corr * zmax),
        (zmax * zmax + zmin * zmin - 2 * corr * zmax * zmin) /
          (1 - corr * corr),
        0
      )
    critval.intersect <- q_fct(alpha, corr)
    pval.intersect <- ifelse(SignWald.intersect > 0,
      ## prob_fct(SignWald.intersect, alpha, corr) + alpha, 1
      prob_fct(SignWald.intersect, 0, corr), 1
    )
  } else { # simulation-based inference
    sw <- .signedwald(par, vcov,
      noninf = noninf,
      weights = weights, nsim_null = nsim.null
      )
    critval.intersect <- NULL
    SignWald.intersect <- sw$test.statistic
    pval.intersect <- sw$pval
  }
  test.int <- structure(list(
    data.name = sprintf("Non-inferiority hypotheses, b <= [%s]",
                        paste(noninf, collapse=", ")),
    statistic = c("Q" = unname(SignWald.intersect)),
    parameter = NULL,
    method = "Signed Wald Intersection Test",
    # null.value =,
    # alternative = "one.sided",
    p.value = pval.intersect
  ), class = "htest")
  return(structure(test.int, critval.intersect=critval.intersect))
}



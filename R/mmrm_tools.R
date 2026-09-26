#' @export
score.mmrm <- function(x,
                       p = NULL,
                       indiv = TRUE,
                       which = c("beta", "theta"),
                       ...) {
  nbeta <- length(coef(x))
  if (is.null(p)) {
    p <- pars(x)
    beta <- p[seq_len(nbeta)]
    theta <- p[-seq_len(nbeta)]
  } else {
    if ("beta" %in% which) {
      beta <- p[seq_len(nbeta)]
      p <- p[-seq_along(beta)]
    }
    if ("theta" %in% which) {
      theta <- p
    }
  }
  U1 <- U2 <- NULL
  if ("beta" %in% which)  U1 <- .mmrm_score_beta(x, beta=beta)
  if ("theta" %in% which) U2 <- .mmrm_score_theta(x, theta=theta)
  res <- cbind(U1, U2)
  ## Attach subject IDs as row names
  rownames(res) <- vapply(.mmrm_subjects(x), `[[`, character(1), "id")
  if (!indiv) return(colSums(res))
  return(res)
}

#' @export
pars.mmrm <- function(x, which = c("beta", "theta"), ...) {
  theta <- x$theta_est
  if (is.null(names(theta))) names(theta) <- paste0("theta", seq_along(theta))
  res <- c()
  if ("beta" %in% which) res <- c(res, x$beta_est)
  if ("theta" %in% which) res <- c(res, theta)
  return(res)
}

#' @export
IC.mmrm <- function(x, ...) {
  pp <- pars(x, ...)
  I <- -numDeriv::jacobian(function(p) {
    score(x, p = p, indiv = FALSE, ...)
  }, pp, method = lava::lava.options()$Dmethod)
  U <- score(x, indiv=TRUE, ...)
  bread <- Inverse(I)*NROW(U)
  res <- U%*%bread
  colnames(res) <- names(pp)
  return(res)
}

#' @export
estimate.mmrm <- function(x, which = c("beta", "theta"), ...) {
  res <- lava::estimate(coef = pars(x, which = which),
                        IC = IC(x, which = which))
  lava::estimate(res, ...)
}

## Reconstruct the VarCorr-style covariance (matrix or list-per-group) for a
## given theta.  When theta is NULL (default) the result equals VarCorr(fit)
## exactly.
## Scope: discrete-visit covariances (us, cs, toep, ad, ar1)
.mmrm_varcor <- function(fit, theta = NULL) {
  stopifnot(inherits(fit, "mmrm"))
  if (is.null(theta)) theta <- mmrm::component(fit, "theta_est")

  cs <- mmrm::as.cov_struct(
                as.formula(mmrm::component(fit, "formula"))
              )
  if (identical(mmrm::component(fit, "cov_type"), "sp_exp")) {
    stop("varcor_at(): spatial covariance (sp_exp) is not supported.")
  }

  rep <- fit$tmb_object$report(theta) # TMB API
  L   <- rep$covariance_lower_chol
  ntimes  <- mmrm::component(fit, "n_timepoints")
  ngroups   <- mmrm::component(fit, "n_groups")
  visit_names <- levels(fit$tmb_data$full_frame[[cs$visits]])

  cov_list <- lapply(seq_len(ngroups), function(g) {
    Lg <- L[seq((g - 1L) * ntimes + 1L, g * ntimes), , drop = FALSE]
    Sg <- tcrossprod(Lg)
    dimnames(Sg) <- list(visit_names, visit_names)
    Sg
  })

  if (ngroups==1L) {
    return(cov_list[[1]])
  }
  names(cov_list) <- levels(fit$tmb_data$subject_groups)
  cov_list
}

## Return a list per subject with:
##   id      : subject label
##   rows    : integer row indices into full_frame / x_matrix / y_vector
##   visits  : character vector of observed visit-factor levels (or numeric
##             coordinates for spatial structures)
##   X, y, w : per-subject design, response, weights
##   Sigma   : n_i x n_i observed-visit covariance matrix
##   group   : character group label (or NA_character_ if no grouping)
##
## This implementation focuses on discrete-time covariances:
## us, cs, toep, ad, ar1
.mmrm_subjects <- function(fit, theta=NULL) {
  stopifnot(inherits(fit, "mmrm"))
  cs        <- mmrm::as.cov_struct(as.formula(
    mmrm::component(fit, "formula")))
  subj_var  <- mmrm::component(fit, "subject_var")
  visit_var <- cs$visits
  group_var <- if (length(cs$group) == 0L) NA_character_ else cs$group

  ff <- mmrm::component(fit, "full_frame")
  X  <- mmrm::component(fit, "x_matrix")
  y  <- mmrm::component(fit, "y_vector")
  w  <- ff[["(weights)"]]
  if (is.null(w)) w <- rep(1, length(y))

  subj_full <- ff[[subj_var]]
  visit_full <- ff[[visit_var]]
  visit_char <- as.character(visit_full)

  if (!is.null(theta)) {
    Sig <- .mmrm_varcor(fit, theta)
  } else {
    Sig <- mmrm::VarCorr(fit)
  }
  is_grouped <- is.list(Sig) && !is.matrix(Sig)
  if (is_grouped) {
    group_full <- as.character(ff[[group_var]])
  } else {
    group_full <- rep(NA_character_, nrow(ff))
  }

  ## Split rows by subject preserving encounter order
  subj_char <- as.character(subj_full)
  first_seen <- !duplicated(subj_char)
  subj_order <- subj_char[first_seen] # subject IDs in order
  rows_by <- split(seq_along(subj_char), subj_char)[subj_order]

  lapply(subj_order, function(sid) {
    rr <- rows_by[[sid]]
    vv <- visit_char[rr]
    gg <- group_full[rr[1]]
    Sfull <- if (is_grouped) Sig[[gg]] else Sig
    Si <- Sfull[vv, vv, drop = FALSE]
    list(id     = sid,
         rows   = rr,
         visits = vv,
         group  = gg,
         X      = X[rr, , drop = FALSE],
         y      = y[rr],
         w      = w[rr],
         Sigma  = Si)
  })
}

## log-likelihood as a function of parameters (mean (beta) and covariance
## (theta))
.mmrm_loglik <- function(fit, beta=NULL, theta=NULL) {
  subj <- .mmrm_subjects(fit, theta=theta)
  if (is.null(beta)) beta <- mmrm::component(fit, "beta_est")
  Sinv <- lapply(subj, function(s) lava::Inverse(s$Sigma))
  Sdet <- lapply(Sinv, function(s) attributes(s)$det)
  r    <- lapply(subj, function(s) s$y - as.numeric(s$X %*% beta))
  loglik  <- unlist(Map(function(Si, Di, ri) {
    -ncol(Si)/2 * log(2*pi)
    -0.5*log(Di) - 0.5 * as.numeric(t(ri) %*% Si %*% ri)
  }, Sinv, Sdet, r))
  sum(loglik)
}

## Per-subject beta score: n_subj x p_beta.
.mmrm_score_beta <- function(fit, beta=NULL, theta=NULL) {
  subj <- .mmrm_subjects(fit, theta = theta)
  n <- length(subj)
  ## if (is.null(beta)) { #
  ## TODO: this only works if model fitted with vcov="Empirical"
  ##   U <- mmrm::component(fit, "score_per_subject")
  ##   browser()
  ##   colnames(U) <- names(mmrm::component(fit, "beta_est"))
  ##   rownames(U) <- vapply(subj, `[[`, character(1), "id")
  ##   return(U)
  ## }
  ## Analytical expression
  if (is.null(beta)) beta <- mmrm::component(fit, "beta_est")
  p <- length(beta)
  Sinv <- lapply(subj, function(s) solve(s$Sigma))
  r    <- lapply(subj, function(s) s$y - as.numeric(s$X %*% beta))
  Sir  <- Map(function(Si, ri) Si %*% ri, Sinv, r)
  Ub <- matrix(0, n, p,
               dimnames = list(NULL, names(beta)))
  for (i in seq_len(n)) {
    wi <- subj[[i]]$w # subject weights
    if (all(wi == 1)) {
      Ub[i, ] <- as.numeric(crossprod(subj[[i]]$X, Sir[[i]]))
    } else {
      W_half <- sqrt(wi)
      SinvW  <- (W_half * Sinv[[i]]) * rep(W_half, each = length(wi))
      Ub[i, ] <- as.numeric(crossprod(subj[[i]]$X, SinvW %*% r[[i]]))
    }
  }
  Ub
}

## Derivative of the covariance matrix with respect to theta.
##
## mmrm uses TMB automatic differentiation internally but does not expose the
## raw dSigma/dtheta. Differentiate the reported covariance instead. The
## result has one element per theta parameter; each element is either a
## covariance matrix or a named list of covariance matrices, one per group.
dSigma_dtheta <- function(fit,
                          theta = NULL,
                          method = c("central", "richardson"),
                          eps = 1e-5) {
  stopifnot(inherits(fit, "mmrm"))
  method <- match.arg(method)

  if (is.null(theta)) {
    theta <- mmrm::component(fit, "theta_est")
  }

  n_time <- mmrm::component(fit, "n_timepoints")
  n_group <- mmrm::component(fit, "n_groups")
  cov_struct <- mmrm::as.cov_struct(as.formula(
    mmrm::component(fit, "formula")))
  visit_names <- levels(fit$tmb_data$full_frame[[cov_struct$visits]])
  group_names <- if (n_group > 1L) {
    levels(fit$tmb_data$subject_groups)
  } else {
    NULL
  }

  ## Flatten Sigma(theta) so both differentiation methods share the same
  ## output handling.
  sigma_vector <- function(par) {
    sigma <- .mmrm_varcor(fit, par)
    if (is.list(sigma) && !is.matrix(sigma)) {
      unlist(lapply(sigma, as.numeric), use.names = FALSE)
    } else {
      as.numeric(sigma)
    }
  }

  as_covariance <- function(x) {
    expected_length <- n_group * n_time^2
    stopifnot(length(x) == expected_length)

    covariances <- lapply(seq_len(n_group), function(g) {
      first <- (g - 1L) * n_time^2 + 1L
      last <- g * n_time^2
      sigma <- matrix(x[first:last], nrow = n_time, ncol = n_time)
      dimnames(sigma) <- list(visit_names, visit_names)
      sigma
    })

    if (n_group == 1L) {
      return(covariances[[1]])
    }

    names(covariances) <- group_names
    return(covariances)
  }

  if (method == "central") {
    derivative <- lapply(seq_along(theta), function(k) {
      theta_plus <- theta_minus <- theta
      theta_plus[k] <- theta[k] + eps
      theta_minus[k] <- theta[k] - eps
      (sigma_vector(theta_plus) - sigma_vector(theta_minus)) / (2 * eps)
    })
  } else {
    if (!requireNamespace("numDeriv", quietly = TRUE)) {
      stop(
        "dSigma_dtheta(method = 'richardson') requires the 'numDeriv' package."
      )
    }

    jacobian <- numDeriv::jacobian(
      sigma_vector,
      theta,
      method = "Richardson"
    )
    derivative <- lapply(seq_along(theta), function(k) jacobian[, k])
  }

  derivative <- lapply(derivative, as_covariance)
  return(derivative)
}


## Restrict a covariance derivative to one subject's observed visits.
.subject_block <- function(dSigma, subject) {
  if (is.list(dSigma) && !is.matrix(dSigma)) {
    dSigma <- dSigma[[subject$group]]
  }

  return(dSigma[subject$visits, subject$visits, drop = FALSE])
}

## Per-subject score for theta.
##
##   U_ik = -1/2 { tr(Sigma_i^-1 dSigma_i/dtheta_k)
##                 - r_i' Sigma_i^-1 dSigma_i/dtheta_k Sigma_i^-1 r_i }
.mmrm_score_theta <- function(fit,
                              beta = NULL,
                              theta = NULL,
                              method = c("central", "richardson"),
                              eps = 1e-5) {
  method <- match.arg(method)
  subj <- .mmrm_subjects(fit, theta = theta)
  n <- length(subj)

  if (is.null(beta)) beta <- mmrm::component(fit, "beta_est")
  if (is.null(theta)) theta <- mmrm::component(fit, "theta_est")
  q <- length(theta)

  Sinv <- lapply(subj, function(s) solve(s$Sigma))
  r <- lapply(subj, function(s) s$y - as.numeric(s$X %*% beta))
  Sir <- Map(function(Si, ri) as.numeric(Si %*% ri), Sinv, r)
  dSig <- dSigma_dtheta(fit, theta = theta, method = method, eps = eps)

  Ut <- matrix(0, n, q, dimnames = list(NULL, paste0("theta", seq_len(q))))
  for (i in seq_len(n)) {
    Si <- Sinv[[i]]
    Siri <- Sir[[i]]
    for (k in seq_len(q)) {
      dS_ik <- .subject_block(dSig[[k]], subj[[i]])
      tr_term <- sum(Si * dS_ik)
      quad <- as.numeric(crossprod(Siri, dS_ik %*% Siri))
      Ut[i, k] <- -0.5 * (tr_term - quad)
    }
  }

  return(Ut)
}

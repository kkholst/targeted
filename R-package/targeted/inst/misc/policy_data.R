library(data.table)

smpd <- function(d, tau, lambda, alpha, sigma, beta, gamma, psi, rho, ...){
  stage_vec <- vector("numeric")
  entry_vec <- vector("numeric")
  exit_vec <- vector("numeric")
  event_vec <- vector("numeric")
  a_vec <- vector("numeric")
  
  # baseline covariates
  z <- rbinom(n = 1, size = 1, prob = 0.3)
  z <- ifelse(z == 1, "a", "b")
  
  # stage specific covariates
  x_vec <- vector("numeric")
  x_lead_vec <- vector("numeric")
  
  stage <- 1
  entry_vec <- c(entry_vec, 0)
  # rate <- lambda
  # t_increment <- rexp(n = 1, rate = rate)
  # t <- t_increment
  t <- 0
  x_lead <- 0

  while (t<tau){
    x <- rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead) + (alpha[5] * (z == "a")), sd = sigma)
    a <- d(stage = stage, t = t, x = x, z = z, beta = beta)
    
    exit_vec <- c(exit_vec, t)
    stage_vec <- c(stage_vec, stage)
    event_vec <- c(event_vec, 0)
    x_vec <- c(x_vec, x)
    a_vec <- c(a_vec, a)
    x_lead_vec <- c(x_lead_vec, x_lead)
    
    if (a == 1){
      entry_vec <- c(entry_vec, t)
    }
    t_increment <- if(a == 1) rexp(n = 1, 1) / exp(lambda + rho * x)  else Inf # remember that mean(t_increment  = 1 / rate)
    t <- t + t_increment + psi # minimum increment psi
    stage <- stage + 1
    x_lead <- x
  }
  
  if (a == 0){
    stage_vec <- c(stage_vec, stage)
    entry_vec <- c(entry_vec, last(exit_vec))
    exit_vec <- c(exit_vec, last(exit_vec))
    event_vec <- c(event_vec, 1)
    a_vec <- c(a_vec, NA)
    x_vec <- c(x_vec, NA)
    x_lead_vec <- c(x_lead_vec, NA)
    
  }
  
  if (a == 1){
    stage_vec <- c(stage_vec, stage)
    exit_vec <- c(exit_vec, tau)
    event_vec <- c(event_vec, 2)
    a_vec <- c(a_vec, NA)
    x_vec <- c(x_vec, NA)
    x_lead_vec <- c(x_lead_vec, NA)
    
  }
  
  mp <- matrix(c(stage_vec, entry_vec, exit_vec, event_vec, a_vec, x_vec, x_lead_vec), ncol = 7, byrow = FALSE)
  colnames(mp) <- c("stage", "entry", "exit", "event", "A", "X", "X_lead")
  
  os <- matrix(c(rnorm(n = 1, mean = gamma * last(exit_vec)), z), ncol = 2)
  colnames(os) <- c("U_os", "Z")

  return(list(mp = mp, os = os))
}

d_obs <- function(stage, t, x, z, beta){
  prob <- lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x) + (beta[4] * (z == "a")))
  rbinom(n = 1, size = 1, prob = prob)
}

d_1 <- function(stage, t, x, z, beta){
  return(1)
}

d_0 <- function(stage, t, x, z, beta){
  return(1)
}

simulate_policy_data <- function(n, args){
  l <- sapply(
    1:n,
    function(id){
      d <- do.call(what = "smpd", args)
      mp <- d$mp
      os <- d$os
      
      mp <- cbind(id = id, mp)
      os <- cbind(id = id, os)
      
      return(list(mp = mp, os = os))
    },
    simplify = "array"
  )
  
  mp <- do.call(what  = "rbind", l["mp",])
  mp <- as.data.table(mp)
  mp[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
  mp[event %in% c(0), U_0 := 0]
  mp[event %in% c(0), U_1 := -X]
  mp[, A := as.character(A)]
  
  os <- as.data.table(do.call(what  = "rbind", l["os",]))
  os[, id := as.numeric(id)]
  os[, U_os := as.numeric(U_os)]
  
  return(list(mp = mp, os = os))
}

args0 <- list(
  d = d_obs,
  tau = 10,
  lambda = 0, # exp(-lambda) = mean, rate = 1 / mean
  alpha =  c( # distribution of x
    0, # intercept
    0.5, # t,
    0.1, # t^2, x (the cost) will increase with t
    -0.5, # x_lead
    0.4 # z
  ),
  # beta = c( # distribution of a
  #   1.5, # intercept
  #   -0.1, # t
  #   -0.2, # x
  #   0.3 # z == "a"
  # ),
  beta = c( # distribution of a
    0.3, # intercept
    0, # t
    0, # x
    0.3 # z == "a"
  ),
  sigma = 1,
  gamma = -0.1,
  psi = 1, # minimum time increment
  rho = -0.5 # Cox parameter for X_lead (the cost), if negative, the rate will decrease
)

new_policy_data <- function(mp, os){
  
  stopifnot(
    is.data.table(mp),
    is.data.table(os)
  )
  mp <- copy(mp)
  os <- copy(os)
  setkey(mp, id, stage)
  setkey(os, id)
  
  stopifnot(
    is.character(mp$A), # A must be a character
    all(sapply(mp, function(col) !is.factor(col))), # factors in mp are not allowed
    all(sapply(os, function(col) !is.factor(col))) # factors in os are not allowed
  )
  
  # maximal set of actions:
  action_set <- sort(unique(mp$A))
  
  # names of columns for the deterministic utility contributions for every action in the action set:
  U_A_colnames <- paste("U", action_set, sep = "_")
  # required column names in mp:
  mp_names <- c("id", "stage", "entry", "exit", "event", "A", "U", U_A_colnames)
  # required column names in os:
  os_names <- c("id", "U_os")
  stopifnot(
    all(mp_names %in% names(mp)), # required column names in mp
    all(os_names %in% names(os)) # required column names in os
  )
  
  # names of columns for the stage specific state data (X_k)
  state_names <- names(mp)[!(names(mp) %in% mp_names)]
  # names of columns for the baseline data
  baseline_names <- names(os)[!(names(os) %in% os_names)]
  
  # checks
  stopifnot(
    anyDuplicated(mp, by = key(mp)) == 0, # no duplicated keys
    anyDuplicated(os, by = key(os)) == 0, 
    all(unique(mp$id) == os$id), # id match in mp and os
    all(mp[ , .(check = all(stage == 1:.N)), by = id]$check), # stages must be on the form 1, 2, ..., k
    all(mp[, .(check = all(event == c(rep(0, times = (.N-1)), 1) | event == c(rep(0, times = (.N-1)), 2))), id]$check), # events must be on the form 0,0,...,0,j (j in {1,2})
    all(is.numeric(mp$U) & !is.na(mp$U)), # the utility must be numeric
    all(sapply(mp[, ..U_A_colnames], function(col) is.numeric(col))),
    all(is.numeric(os$U_os) & !is.na(os$U_os))
  )
  
  # getting dimensions
  n <- length(unique(mp$id))
  K <- mp[event == 0, .(max(stage))][[1]]
  
  object <- list(
    mp = mp,
    os = os,
    colnames = list(
      mp_names = mp_names,
      state_names = state_names,
      os_names = os_names,
      baseline_names = baseline_names
    ),
    action_set = action_set,
    dim = list(
      n = n,
      K = K
    )
  )
  
  class(object) <- "policy_data"
  
  return(object)
}


# data sample -------------------------------------------------------------

set.seed(1)
policy_data <- simulate_policy_data(2e3, args0)
policy_data <- new_policy_data(mp = policy_data$mp, os = policy_data$os)
save(policy_data, file = "policy_data.rda")


# checks ------------------------------------------------------------------

# pd <- simulate_policy_data(1e5, args0)
# pd <- new_policy_data(mp = pd$mp, os = pd$os)
# tmp <- pd$mp
# tmp <- tmp[, t := exit - entry - args0$psi][entry != exit, ][event == 0, ]
# library(survival)
# coxph(Surv(tmp$t) ~ tmp$X_lead)
# args0$rho

# set.seed(2)
# tmp <- do.call(what = "smpd", args0)
# tmp2 <- tmp$mp
# tmp2 <- as.data.table(tmp2)
# tmp2[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
# tmp2[event %in% c(0), U_0 := 0]
# tmp2[event %in% c(0), U_1 := -X]

# set.seed(1)
# policy_data <- simulate_policy_data(2e3, args0)
# policy_data <- new_policy_data(mp = policy_data$mp, os = policy_data$os)
# tmp <- policy_data$mp
# tmp2 <- policy_data$os
# tmp[,
#     .(
#       u_total = max(exit) - min(entry)  - sum(as.numeric(A) * X, na.rm = TRUE),
#       u_total_2 = sum(U)
#     ),
#     id][,
#         .(check = abs(u_total - u_total_2) < 1e-10)
#         ][,
#           all(check)
#           ]





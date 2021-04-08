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
    # the time increment comes from an exponential distribution with mean exp(lambda + rho * x)
    # remember that mean(t_increment  = 1 / rate)
    t_increment <- if(a == 1) rexp(n = 1, 1) / exp(lambda + rho * x)  else Inf
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
  
  stage_data <- matrix(c(stage_vec, entry_vec, exit_vec, event_vec, a_vec, x_vec, x_lead_vec), ncol = 7, byrow = FALSE)
  colnames(stage_data) <- c("stage", "entry", "exit", "event", "A", "X", "X_lead")
  
  baseline_data <- matrix(c(rnorm(n = 1, mean = gamma * last(exit_vec)), z), ncol = 2)
  colnames(baseline_data) <- c("U_os", "Z")

  return(list(stage_data = stage_data, baseline_data = baseline_data))
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
      stage_data <- d$stage_data
      baseline_data <- d$baseline_data
      
      stage_data <- cbind(id = id, stage_data)
      baseline_data <- cbind(id = id, baseline_data)
      
      return(list(stage_data = stage_data, baseline_data = baseline_data))
    },
    simplify = "array"
  )
  
  stage_data <- do.call(what  = "rbind", l["stage_data",])
  stage_data <- as.data.table(stage_data)
  stage_data[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
  stage_data[event %in% c(0), U_0 := 0]
  stage_data[event %in% c(0), U_1 := -X]
  stage_data[, A := as.character(A)]
  
  baseline_data <- as.data.table(do.call(what  = "rbind", l["baseline_data",]))
  baseline_data[, id := as.numeric(id)]
  baseline_data[, U_os := as.numeric(U_os)]
  
  return(list(stage_data = stage_data, baseline_data = baseline_data))
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
    -0.1, # t
    -0.1, # x
    0.3 # z == "a"
  ),
  sigma = 1,
  gamma = -0.1,
  psi = 1, # minimum time increment
  rho = -0.5 # Cox parameter for X_lead (the cost), if negative, the rate will decrease
)

new_policy_data <- function(stage_data, baseline_data, id = "id", stage = "stage", action = "A", utility = "U", entry ="entry", exit = "exit"){
  
  # check if data.table
  stopifnot(
    is.data.table(stage_data),
    is.data.table(baseline_data)
  )
  
  # copy data.table's
  stage_data <- copy(stage_data)
  baseline_data <- copy(baseline_data)
  
  # setting keys 
  setkey(stage_data, id, stage)
  setkey(baseline_data, id)
  
  stopifnot(
    is.character(stage_data$A), # A must be a character
    all(sapply(stage_data, function(col) !is.factor(col))), # factors in stage_data are not allowed
    all(sapply(baseline_data, function(col) !is.factor(col))) # factors in baseline_data are not allowed
  )
  
  # maximal set of actions:
  action_set <- sort(unique(stage_data$A))
  
  # names of columns for the deterministic utility contributions for every action in the action set:
  U_A_colnames <- paste("U", action_set, sep = "_")
  # required column names in stage_data:
  stage_data_names <- c("id", "stage", "entry", "exit", "event", "A", "U", U_A_colnames)
  # required column names in baseline_data:
  os_names <- c("id", "U_os")
  stopifnot(
    all(stage_data_names %in% names(stage_data)), # required column names in stage_data
    all(os_names %in% names(baseline_data)) # required column names in baseline_data
  )
  
  # names of columns for the stage specific state data (X_k)
  state_names <- names(stage_data)[!(names(stage_data) %in% stage_data_names)]
  # names of columns for the baseline data
  baseline_names <- names(baseline_data)[!(names(baseline_data) %in% os_names)]
  
  # checks
  stopifnot(
    anyDuplicated(stage_data, by = key(stage_data)) == 0, # no duplicated keys
    anyDuplicated(baseline_data, by = key(baseline_data)) == 0, 
    all(unique(stage_data$id) == baseline_data$id), # id match in stage_data and baseline_data
    all(stage_data[ , .(check = all(stage == 1:.N)), by = id]$check), # stages must be on the form 1, 2, ..., k
    all(stage_data[, .(check = all(event == c(rep(0, times = (.N-1)), 1) | event == c(rep(0, times = (.N-1)), 2))), id]$check), # events must be on the form 0,0,...,0,j (j in {1,2})
    all(is.numeric(stage_data$U) & !is.na(stage_data$U)), # the utility must be numeric
    all(sapply(stage_data[, ..U_A_colnames], function(col) is.numeric(col))),
    all(is.numeric(baseline_data$U_os) & !is.na(baseline_data$U_os))
  )
  
  # getting dimensions
  n <- length(unique(stage_data$id))
  K <- stage_data[event == 0, .(max(stage))][[1]]
  
  object <- list(
    stage_data = stage_data,
    baseline_data = baseline_data,
    colnames = list(
      stage_data_names = stage_data_names,
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
policy_data <- new_policy_data(stage_data = policy_data$stage_data, baseline_data = policy_data$baseline_data)
save(policy_data, file = "policy_data.rda")

# checks ------------------------------------------------------------------

# pd <- simulate_policy_data(1e5, args0)
# pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
# tmp <- pd$stage_data
# tmp <- tmp[, t := exit - entry - args0$psi][entry != exit, ][event == 0, ]
# library(survival)
# coxph(Surv(tmp$t) ~ tmp$X_lead)
# args0$rho

# set.seed(2)
# tmp <- do.call(what = "smpd", args0)
# tmp2 <- tmp$stage_data
# tmp2 <- as.data.table(tmp2)
# tmp2[, U := (exit - entry) + shift(ifelse(!is.na(A), -X * A, 0), fill = 0)]
# tmp2[event %in% c(0), U_0 := 0]
# tmp2[event %in% c(0), U_1 := -X]

# set.seed(1)
# policy_data <- simulate_policy_data(2e3, args0)
# policy_data <- new_policy_data(stage_data = policy_data$stage_data, baseline_data = policy_data$baseline_data)
# tmp <- policy_data$stage_data
# tmp2 <- policy_data$baseline_data
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





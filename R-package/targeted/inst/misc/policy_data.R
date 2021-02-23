# lifetable METS
# Survsplit
library(data.table)

smpd <- function(d, tau, lambda, alpha, sigma, beta, gamma){
  stage_vec <- vector("numeric")
  entry_vec <- vector("numeric")
  exit_vec <- vector("numeric")
  event_vec <- vector("numeric")
  a_vec <- vector("numeric")
  
  # Z
  x_vec <- vector("numeric")
  x_lead_vec <- vector("numeric")
  
  stage <- 1
  entry_vec <- c(entry_vec, 0)
  rate <- lambda
  t_increment <- rexp(n = 1, rate = rate)
  t <- t_increment
  x_lead <- 0

  while (t<tau){
    x <- rnorm(n = 1, mean = alpha[1] + (alpha[2] * t) + (alpha[3]*t^2) + (alpha[4] * x_lead), sd = sigma)
    a <- d(stage = stage, t = t, x = x, beta = beta)
    
    exit_vec <- c(exit_vec, t)
    stage_vec <- c(stage_vec, stage)
    event_vec <- c(event_vec, a)
    x_vec <- c(x_vec, x)
    a_vec <- c(a_vec, a)
    x_lead_vec <- c(x_lead_vec, x_lead)
    
    if (a == 1){
      entry_vec <- c(entry_vec, t)
    }
    rate <- lambda * abs(x)
    t_increment <- if(a == 1) rexp(n = 1, rate = rate) else Inf
    t <- t + t_increment
    stage <- stage + 1
    x_lead <- x
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
  
  os <- matrix(rnorm(n = 1, mean = gamma * last(exit_vec)), ncol = 1)
  colnames(os) <- c("U_os")

  return(list(mp = mp, os = os))
}

d_obs <- function(stage, t, x, beta){
  rbinom(n = 1, size = 1, prob = lava::expit(beta[1] + (beta[2] * t) + (beta[3] * x)))
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
  mp[event %in% c(0,1), U_0 := 0]
  mp[event %in% c(0,1), U_1 := -X]

  os <- do.call(what  = "rbind", l["os",])
  
  return(list(mp = mp, os = os))
}

args0 <- list(
  d = d_obs,
  tau = 10,
  lambda = 1.2,
  alpha =  c( # distribution of x
    0, # intercept
    0.5, # t,
    0.1, # t^2,
    -0.5 # x_lead
  ),
  beta = c( # distribution of a
    1.5, # intercept
    -0.1, # t
    -0.2 # x
  ),
  sigma = 1,
  gamma = -0.1
)
#do.call(what = "smpd", args0)
set.seed(1)
policy_data <- simulate_policy_data(2e3, args0)
#save(policy_data, file = "policy_data.rda")



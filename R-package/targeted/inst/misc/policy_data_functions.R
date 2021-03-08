copy_policy_data <- function(object){
  
  object$mp <- copy(object$mp)
  object$os <- copy(object$os)
  
  return(object)
}

partial <- function(object, K)
  UseMethod("partial")

partial.policy_data <- function(object, K){
  object <- copy_policy_data(object)
  
  mp_colnames <- object$colnames$mp_colnames
  
  mp_K <- object$mp[stage <= K, ]
  mp_res <- object$mp[stage > K, ..mp_colnames]
  
  mp_res <- mp_res[,
    .(
      stage = min(stage),
      entry = min(entry),
      exit = max(exit),
      event = max(event),
      U = sum(U),
      U_0 = NA,
      U_1 = NA
    ),
    id
  ]
  mp <- rbindlist(list(mp_K, mp_res), fill = TRUE, use.names = TRUE)
  setkey(mp, id, stage)
  
  object$mp <- mp
  object$K <- K
  
  return(object)
}

full_stage_history <- function(object, stage)
  UseMethod("full_stage_history")

# add age
# t_1, t_2, ...
full_stage_history <- function(object, stage){
  stage_ <- stage
  
  sh_names <- c("id", "stage", "A", object$colnames$mp_X_colnames)
  sh <- object$mp[event == 0, ]
  sh <- sh[stage <= stage_, ..sh_names]
  sh <- sh[, if(any(stage == stage_)) .SD, id]
  
  sh <- dcast(sh, id ~ stage, value.var = sh_names[-c(1,2)])
  #as <- paste("A", stage_, sep = "_")
  #sh[, (as) := NULL]
  
  return(sh)
}

markov_stage_history <- function(object, stage)
  UseMethod("markov_stage_history")

# add age
markov_stage_history <- function(object, stage){
  stage_ <- stage
  
  sh_names <- c("id", "stage", "A", object$colnames$mp_X_colnames)
  sh <- object$mp[event == 0, ]
  sh <- sh[stage == stage_, ..sh_names]
  nn <- paste(c("A", object$colnames$mp_X_colnames), stage_, sep = "_")
  setnames(sh, old = c("A", object$colnames$mp_X_colnames), new = nn)
  #sh[, stage := NULL]
  
  return(sh)
}

markov_history <- function(object)
  UseMethod("markov_history")

markov_history.policy_data <- function(object){
  mp_X_colnames <- object$colnames$mp_X_colnames
  
  sh_names <- c("id", "stage", "A", mp_X_colnames)
  sh <- object$mp[event == 0, ]
  sh <- sh[, ..sh_names]
  
  # alternative form
  # sh <- list(
  #   ref = sh[, c("id", "stage")],
  #   A = sh[, c("A")],
  #   X = sh[, ..mp_X_colnames]
  # )
  
  return(sh)
}

utility <- function(object)
  UseMethod("utility")

utility.policy_data <- function(object){
  mp <- object$mp
  os <- object$os
  
  u <- mp[, .(U_mp = sum(U)), id]
  u <- os[u, ]
  u <- u[, .(id = id, U = U_mp + U_os)]
  
  return(u)
}

# Checks
# data("policy_data")
# tmp <- utility(policy_data)
# tmp <- policy_data$mp
# tmp2 <- markov_stage_history(policy_data, stage = 3)
# tmp <- partial(policy_data, K = 5)
# all(abs(tmp[, . (U = sum(U)), id]$U - policy_data$mp[, .(U = sum(U)), id]$U) < 1e-10)


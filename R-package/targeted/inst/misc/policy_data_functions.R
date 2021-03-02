data("policy_data")

partial <- function(object, K)
  UseMethod("partial")

partial.policy_data <- function(object, K){
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
  
  return(mp)
}

# Checks
# tmp <- partial(policy_data, K = 5)
# all(abs(tmp[, . (U = sum(U)), id]$U - policy_data$mp[, .(U = sum(U)), id]$U) < 1e-10)


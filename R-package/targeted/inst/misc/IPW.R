g_function <- function(object, g_model)
  UseMethod("g_function")

# Returns a g_function resulting from fitting the given g_model to the data given by the history object.
g_function.history <- function(object, g_model){
  action_set <- object$action_set
  
  # getting the action (A) and the model matrix (X):
  A <- get_A(object)
  X <- get_X(object)
  
  # fitting the model:
  gm <- g_model(A = A, X = X)
  
  g_function <- list(
    gm = gm,
    X_names = colnames(X),
    action_set = action_set
  )
  class(g_function) <- "g_function"
  
  return(g_function)
}

# Returns the predictions of the given g_function evaluated on the (new) history object.
# The predictions will be a matrix with columns g_(action_1), g_(action_2).. .
predict.g_function <- function(object, new_history){
  X_names <- object$X_names
  g_model <- object$gm
  action_set <- object$action_set
  
  id <- get_id(new_history)
  new_X <- get_X(new_history)
  
  # checking that the model matrix has the correct form (could be an issue if factor levels are missing)
  stopifnot(
    names(new_X) == X_names
  )
  
  g_predictions <- predict(g_model, new_X = new_X, type = "probs", action_set = action_set)
  # including the id's
  g_predictions <- data.table(id, g_predictions)
  setkey(g_predictions, id, stage)
  
  return(g_predictions)
}

Q_function <- function(object, V, Q_model)
  UseMethod("Q_function")

# IPW ---------------------------------------------------------------------

# d_index <- Vectorize(
#   function(d, action_set){
#     which(action_set %in% d)
#   },
#   vectorize.args = "d"
# )

# action_matrix: Constructs a boolean matrix. Each column represents an action in the action set.
# Each row represents an observed action. Each row contains one true value.
am <- Vectorize(
  function(A, action_set){
    am <- action_set %in% A
    return(am)
  },
  vectorize.args = "A"
)
action_matrix <- function(A, action_set){
  t(am(A = A, action_set = action_set))
}
# check
action_matrix(c("B","B","A","C","D"), c("A","B", "C", "D"))

IPW <- function(object, ...)
  UseMethod("IPW")

IPW.policy_data <- function(policy_data, g_model_list, policy, g_full_history = FALSE){
  K <- policy_data$dim$K
  action_set <- policy_data$action_set
  
  # checking g_model input
  stopifnot(
    if(class(g_model_list)[[1]] == "list")
      length(g_model_list) == K
    else TRUE
  )
  # checking policy input
  stopifnot(
    if(class(policy)[[1]] == "list")
      length(policy) == K
    else TRUE
  )
  
  
  if(g_full_history == FALSE){
    if (class(g_model_list)[[1]] == "list"){
      
    }
    else {
      gh <- markov_history(policy_data)
      gf <- g_function(gh, g_model = g_model_list)
      gp <- predict(gf, new_history = gh)
    }
  }
  
  if (class(policy)[[1]] == "list"){
  } else{
    dh <- markov_history(policy_data)
    d <- policy(dh)
  }
  
  # collecting the model data
  md <- policy_data$stage_data[event == 0, c("id", "stage", "A"), with = FALSE]
  # merging the g-function probabilities
  md <- merge(md, gp, all.x = TRUE)
  # merging the policy actions
  md <- merge(md, d, all.x = TRUE)
  stopifnot(
    all(complete.cases(md))
  )
  
  # calculating observed action probabilities
  G_A <- action_matrix(A = md$A, action_set = action_set) * md[, (regexpr("\\g_.",names(md)) == 1), with = FALSE]
  G_A <- apply(G_A, MARGIN = 1, sum)
  md[, G_A := G_A]
  
  # calculating the inverse probability weights
  ed <- md[ , .(ipw = prod(A == d) / prod(G_A)), id]
  # merging the utility
  ed <- merge(ed, utility(policy_data), all.x = TRUE)
  stopifnot(
    all(complete.cases(ed))
  )
  md[, G_A := NULL]
  
  out <- list(
    ed = ed, # estimation data
    md = md # model data
  )
    
  return(out)
}

# Test --------------------------------------------------------------------

binom_model <- function(A, X){
  # binary outcome
  stopifnot(
    all(A %in% c("0","1"))
  )
  A <- as.numeric(as.character(A))
  
  # model matrix as data.frame
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  
  glm_model <- glm(A ~ ., data = X, family = binomial(), model = FALSE)
  
  bm <- list(
    glm_model = glm_model
  )

  class(bm) <- "binom_model"
  return(bm)
}

predict.binom_model <- function(object, new_X, type = "probs", action_set){
  glm_model <- object$glm_model
  
  stopifnot(
    all(action_set == c("0","1"))
  )
  
  # model matrix as data.frame
  if (is.matrix(new_X)) {
    new_X = as.data.frame(new_X)
  }
  fit <- predict.glm(object = glm_model, newdata = new_X, type = "response")
  
  probs <- sapply(as.numeric(action_set), function(a) a * fit + (1-a) * (1-fit))
  colnames(probs) <- paste("g", action_set, sep = "_")
  
  return(probs)
}

policy_1 <- function(history){
  pol <- history$dt[, c("id", "stage")]
  pol[, d := "1"]
  return(pol)
}

policy_0 <- function(history){
  pol <- history$dt[, c("id", "stage")]
  pol[, d := "0"]
  return(pol)
}

# history functions
# data(policy_data)
# # policy_data <- partial(policy_data, K = 3)
# # history0 <- markov_stage_history(policy_data, stage = 1)
# # history0 <- full_stage_history(policy_data, stage = 1)
# # history0 <- markov_stage_history(policy_data, stage = 2)
# # history0 <- full_stage_history(policy_data, stage = 2)
# history0 <- markov_history(policy_data)
# # propensity model parameters
# get_A(history0)
# policy_1(history0)
# gf <- g_function(object = history0, g_model = binom_model)
# gf$gm
# gf$X_names
# gf$action_set
# # args0$beta
# pred <- predict(gf, new_history = history0)

# # inverse probability weight check
source("~/Projects/target/R-package/targeted/inst/misc/policy_data.R")
source("~/Projects/target/R-package/targeted/inst/misc/policy_data_functions.R")
pd <- simulate_policy_data(1e5, args0)
pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)
h <- markov_history(pd)
gf <- g_function(object = h, g_model = binom_model)
gf$gm
args0$beta
ipw_1 <- IPW(pd, g_model_list = binom_model, policy = policy_1)
mean(ipw_1$ed$ipw) # should be close to 1
ipw_0 <- IPW(pd, g_model_list = binom_model, policy = policy_0)
mean(ipw_0$ed$ipw) # should be close to 1
rm(pd)

# always treat policy:
set.seed(1)
m <- 1e3
ipw_1_est <- vector(mode = "numeric", length = m)
for (i in seq_along(ipw_1_est)){
  pd <- simulate_policy_data(2e3, args0)
  pd <- new_policy_data(stage_data = pd$stage_data, baseline_data = pd$baseline_data)

  ipw <- IPW(pd, g_model_list = binom_model, policy = policy_1)
  est <- ipw$ed[,.(mean(U * ipw))][[1]]
  rm(ipw)

  ipw_1_est[i] <- est
}
mean(ipw_1_est)
# approximate true value
args1 <- args0
args1$d <- d_1
pd1 <- simulate_policy_data(1e5, args1)
pd1 <- new_policy_data(stage_data = pd1$stage_data, baseline_data = pd1$baseline_data)
mean(utility(pd1)$U)


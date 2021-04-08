### Notes
# A is a factor. This can become an issue if not all levels of A are observed in cross-fitting the g_model 

library(data.table)

# Returns a copy of the policy data object.
copy_policy_data <- function(object){
  
  object$stage_data <- copy(object$stage_data)
  object$baseline_data <- copy(object$baseline_data)
  
  return(object)
}

partial <- function(object, K)
  UseMethod("partial")

###
### BUG: If K is higher then the observed maximum number if stages it gives a warning
###

# Returns a policy data object with the same 1-K stages in the marked point data.table as the original object.
# An additional row is added to the marked point data.table that summarize the information of the remaining stages.
partial.policy_data <- function(object, K){
  # copy object to avoid references in data.table
  object <- copy_policy_data(object)
  
  # getting the required column names in stage_data:
  stage_data_names <- object$colnames$stage_data_names
  
  # filtering stage_data rows up till stage K:
  stage_data_K <- object$stage_data[stage <= K, ]
  # filtering stage_data rows for stages above K and the required columns:
  stage_data_res <- object$stage_data[stage > K, ..stage_data_names]
  # summarizing stage_data_res as a single row:
  stage_data_res_sum <- stage_data_res[,
    .(
      stage = min(stage), # min(stage) is K + 1
      entry = min(entry), # min(entry) is the time of stage K-1
      exit = max(exit), # max(exit) is the time of stage K*, where K* is the stochastic number of stages
      event = max(event), # max(event) is the event at stage K*, which is either 1 or 2
      U = sum(U), # sum(U) is the sum of the utility contributions from stage K+1 to K*
      U_0 = NA,
      U_1 = NA
    ),
    id
  ]
  # binding stage_data_K with stage_data_res_sum
  stage_data <- rbindlist(list(stage_data_K, stage_data_res_sum), fill = TRUE, use.names = TRUE)
  # setting keys
  setkey(stage_data, id, stage)
  
  object$stage_data <- stage_data
  object$dim$K <- K
  
  return(object)
}
# # checks
# data("policy_data")
# tmp <- partial(policy_data, K = 3)
# tmp2 <- tmp$stage_data


# History functions -------------------------------------------------------

# A history function returns a history object containing
# - dt: a data.table containing the data
# - id_names: names of the columns in dt that identify the observation and stage
# - action_name: name of the column in dt for the action at the given stage (A_k)
# - history_names: names of the columns in dt for the history at the given stage (H_k)
# - action_set: the set of possible actions

full_stage_history <- function(object, stage)
  UseMethod("full_stage_history")

# Each row in dt represents the action and full history for an observation at the given stage (including the baseline information).
full_stage_history <- function(object, stage){
  stage_data <- object$stage_data
  state_names <- object$colnames$state_names
  baseline_data <- object$baseline_data
  baseline_names <- object$colnames$baseline_names
  action_set <- object$action_set
  stage_ <- stage; rm(stage)
  
  # getting stage specific history names:
  history_names <- c("id", "stage", "exit", "A", state_names)
  # filtering rows which have an action (event = 0):
  history <- stage_data[event == 0, ]
  # filtering rows up till the given stage number:
  history <- history[stage <= stage_, ..history_names]
  # filtering observations with an action at the given stage:
  history <- history[, if(any(stage == stage_)) .SD, id]
  # transforming the data from long to wide format:
  history <- dcast(history, id ~ stage, value.var = history_names[-c(1,2)])
  # getting the baseline data:
  baseline_names <- c("id",baseline_names)
  baseline <- baseline_data[, ..baseline_names]
  # merging the stage specific histories and the the baseline data:
  history <- merge(history, baseline, all.x = TRUE)
  # inserting stage column
  history[, stage := stage_]
  setcolorder(history, neworder = c(1, ncol(history), 2:(ncol(history) -1)))
  
  id_names <- c("id", "stage")
  action_name <- paste("A", stage_, sep = "_")
  history_names <- names(history)[!(names(history) %in% c(id_names, action_name))]
  
  history <- list(
    dt = history,
    id_names = id_names,
    action_name = action_name,
    history_names = history_names,
    action_set = action_set
  )
  class(history) <- "history"
  
  return(history)
}
# checks:
# data("policy_data")
# tmp <- full_stage_history(policy_data, 2)
# tmp2 <- tmp$dt
# tmp$action_name
# tmp$history_names
# tmp$id_names

markov_stage_history <- function(object, stage)
  UseMethod("markov_stage_history")

# Each row in dt represents the action and state for an observation at the given stage (including the baseline information).
markov_stage_history <- function(object, stage){
  stage_data <- object$stage_data
  state_names <- object$colnames$state_names
  baseline_data <- object$baseline_data
  baseline_names <- object$colnames$baseline_names
  action_set <- object$action_set
  stage_ <- stage
  
  # getting stage specific history names:
  history_names <- c("id", "stage", "exit", "A", state_names)
  # filtering rows which have an action (event = 0):
  history <- stage_data[event == 0, ]
  # filtering observations with an action at the given stage:
  history <- history[stage == stage_, ..history_names]
  # setting new names:
  new_names <- paste(c("exit", "A", state_names), stage_, sep = "_")
  setnames(history, old = c("exit", "A", state_names), new = new_names)
  # getting the baseline data:
  baseline_names <- c("id",baseline_names)
  baseline <- baseline_data[, ..baseline_names]
  # merging the stage specific histories and the the baseline data:
  history <- merge(history, baseline, all.x = TRUE)
  
  id_names <- c("id", "stage")
  action_name <- paste("A", stage_, sep = "_")
  history_names <- names(history)[!(names(history) %in% c(id_names, action_name))]
  
  history <- list(
    dt = history,
    id_names = id_names,
    action_name = action_name,
    history_names = history_names,
    action_set = action_set
  )
  class(history) <- "history"
  
  return(history)
}
# checks
data("policy_data")
tmp <- markov_stage_history(policy_data, stage = 2)
tmp2 <- tmp$dt
tmp$action_name
tmp$history_names
tmp$action_set

markov_history <- function(object)
  UseMethod("markov_history")

# Each row in dt represents the action and state for an observation (including the baseline information).
# Each row is identified by the id and stage number.
markov_history.policy_data <- function(object){
  stage_data <- object$stage_data
  state_names <- object$colnames$state_names
  baseline_data <- object$baseline_data
  baseline_names <- object$colnames$baseline_names
  action_set <- object$action_set
  
  # getting stage specific history names:
  history_names <- c("id", "stage", "exit", "A", state_names)
  # filtering rows which have an action (event = 0):
  history <- stage_data[event == 0, ]
  history <- history[, ..history_names]
  
  # getting the baseline data:
  baseline_names <- c("id",baseline_names)
  baseline <- baseline_data[, ..baseline_names]
  # merging the stage specific histories and the the baseline data:
  history <- merge(history, baseline, all.x = TRUE)
  # setting keys
  setkey(history, id, stage)
  
  id_names <- c("id", "stage")
  action_name <- "A"
  history_names <- names(history)[!(names(history) %in% c(id_names, action_name))]
  
  history <- list(
    dt = history,
    id_names = id_names,
    action_name = action_name,
    history_names = history_names,
    action_set = action_set
  )
  class(history) <- "history"
  
  return(history)
}
# # checks
data("policy_data")
tmp <- markov_history(policy_data)
tmp2 <- tmp$dt
tmp$history_names
tmp$action_name

get_X <- function(object)
  UseMethod("get_X")

# Returns the model matrix X associated with the history object
get_X.history <- function(object){
  dt <- object$dt
  history_names <- object$history_names
  
  # collecting the data:
  X <- dt[, ..history_names]
  
  # dropping character columns with 1 unique element (1 level)
  character_cols <- names(X)[sapply(X, is.character)]
  dn <- character_cols[(sapply(X[, ..character_cols], function(x) length(unique(x))) == 1)]
  if (length(dn) > 0)
    X[, (dn) := NULL]
  
  # constructing the model matrix (without an intercept)
  X <- model.matrix(~., X)[, -1]
  
  return(X)
}
# checks
# data("policy_data")
# # tmp <- markov_history(policy_data)
# tmp <- markov_stage_history(policy_data, stage = 1)
# tmp <- full_stage_history(policy_data, stage = 2)
# colnames(get_X(tmp))

get_A <- function(object)
  UseMethod("get_A")

# Returns a vector with the action of the given history object
get_A.history <- function(object){
  dt <- object$dt
  action_name <- object$action_name
  
  A <- dt[[action_name]]
  
  return(A)
}
# checks
# data("policy_data")
# #tmp <- markov_history(policy_data)
# tmp <- full_stage_history(policy_data, stage = 2)
# get_A(tmp)

get_id <- function(object)
  UseMethod("get_id")

get_id <- function(object){
  dt <- object$dt
  id_names <- object$id_names
  
  id <- dt[, ..id_names]
  
  return(id)
}
# data("policy_data")
# tmp <- full_stage_history(policy_data, stage = 2)
# get_id(tmp)

# Utility functions -------------------------------------------------------

utility <- function(object)
  UseMethod("utility")

# Returns a data.table with the id and the (total) utility
utility.policy_data <- function(object){
  stage_data <- object$stage_data
  baseline_data <- object$baseline_data
  
  u <- stage_data[, .(U_stage_data = sum(U)), id]
  u <- baseline_data[u, ]
  u <- u[, .(id = id, U = U_stage_data + U_os)]
  
  return(u)
}
# checks:
# data("policy_data")
# tmp <- utility(policy_data)
# tmp <- policy_data$stage_data
# tmp2 <- markov_stage_history(policy_data, stage = 3)
# tmp <- partial(policy_data, K = 5)
# all(abs(tmp[, . (U = sum(U)), id]$U - policy_data$stage_data[, .(U = sum(U)), id]$U) < 1e-10)


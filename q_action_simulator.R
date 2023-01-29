source("action_simulator.R")

QActionSimulator <- setRefClass("QActionSimulator",
                                fields = c("action_gen",
                                           "theta",
                                           "phi",
                                           "q"),
                                contains = "ActionSimulator")

QActionSimulator$methods(
  initialize = function(action_gen, theta, ...) {
    action_gen <<- action_gen
    theta <<- theta
    callSuper(...)
  },
  get_action = function(t, state, seed = NA) {
    q <<- data.frame()
    get_q_all_eligible_actions(t, state)
    best_action_name <- q[which.max(q$q), "action_name"]
    action_gen$get_action_by_name(t, state, best_action_name)
  },
  get_q_all_eligible_actions = function(t, state) {
    eligible_action_names <- action_gen$get_eligible_action_names(t, state)
    for (action_name in eligible_action_names) {
      get_action_q_and_store(t, state, action_name)
    }
  },
  get_action_q_and_store = function(t, state, action_name) {
    action <- action_gen$get_action_by_name(t, state, action_name)
    phi <<- get_phi(t, state, action)
    q_val <- get_current_q()
    
    next_ind <- nrow(q) + 1
    q[next_ind, "action_name"] <<- action_name
    q[next_ind, "q"] <<- q_val
  },
  get_current_q = function() {
    q_val <- 0
    for (col in colnames(theta)) q_val <- q_val + theta[1, col] * phi[1, col]
    q_val
  },
  get_phi = function(t, state, action) {
    stop("Function get_phi(t, state, action) must be defined for ",
         "QActionSimulator subclass.")
  }
)
source("action_simulator.R")

FixedActionSimulator <- setRefClass("FixedActionSimulator",
                                    fields = c("action_gen",
                                               "t_action_names"),
                                    contains = "ActionSimulator")

FixedActionSimulator$methods(
  initialize = function(action_gen, t_action_names, ...) {
    callSuper(global_markov_parameters = action_gen$global_markov_parameters, 
              ...)
    action_gen <<- action_gen
    initialize_t_action_names(t_action_names)
  },
  initialize_t_action_names = function(t_action_names) {
    t_action_names <<- t_action_names
    for (t in 0:(action_gen$global_markov_parameters$num_epochs-1)) {
      t_str <- as.character(t)
      if (!(t_str %in% names(t_action_names))) t_action_names[[t_str]] <<- "none"
    }
  },
  get_action = function(t, state, seed = NA) {
    t_str <- as.character(t)
    name <- t_action_names[[t_str]]
    action_gen$get_action_by_name(t, state, name)
  }
)
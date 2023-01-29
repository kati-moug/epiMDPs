source("markov_utility.R")

ActionGenerator <- setRefClass("ActionGenerator", 
                               contains = "MarkovUtility")
ActionGenerator$methods(
  initialize = function(global_markov_parameters = NA, 
                        test_parameters = NA, ...) {
    callSuper(global_markov_parameters = global_markov_parameters, 
              test_parameters = test_parameters, ...)
  },
  get_eligible_action_names = function(t, state, seed = NA) {
    stop("Function get_eligible_action_names(t, state, seed = NA) ",
         "must be defined for ActionGenerator subclass.")
  },
  get_action_by_name = function(t, state, name) {
    stop("Function get_action_by_name(t, state, name) ",
         "must be defined for ActionGenerator subclass.")
  }
)
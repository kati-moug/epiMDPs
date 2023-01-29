source("markov_utility.R")
ActionSimulator <- setRefClass("ActionSimulator", 
                               contains = "MarkovUtility")
ActionSimulator$methods(
  initialize = function(global_markov_parameters = NA,
                        test_parameters = list(), ...) {
    callSuper(global_markov_parameters = global_markov_parameters,
              test_parameters = test_parameters, ...)
  },
  get_action = function(t, state, seed = NA) {
    stop("Function get_action(t, state, seed = NA) must be defined ",
         "for ActionSimulator subclass.")
  }
)
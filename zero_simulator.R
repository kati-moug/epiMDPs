source("action_simulator.R")
ZeroActionSimulator <- setRefClass("ZeroActionSimulator", 
                               contains = "ActionSimulator")
ZeroActionSimulator$methods(
  initialize = function(...) {
    callSuper(...)
  },
  get_action = function(t, state, seed = NA) {
    get_zero_action()
  },
  get_zero_action = function() {
    stop("Function get_zero_action() must be defined for ",
         "ZeroActionSimulator subclass.")
  }
)
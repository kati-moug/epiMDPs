source("init_field_reader.R")
MarkovUtility <- setRefClass("MarkovUtility", 
                                   fields = c("global_markov_parameters",
                                              "test_parameters"),
                                   contains = "InitFieldReader")
MarkovUtility$methods(
  initialize = function(global_markov_parameters, test_parameters=list()) {
    global_markov_parameters <<- global_markov_parameters
    test_parameters <<- test_parameters
    callSuper() # change June 13
  },
  get_components = function(type) {
    if (type == "state") get_state_components()
    else if (type == "action") get_action_components()
    else if (type == "reward") get_reward_components()
  },
  get_state_components = function() {
    global_markov_parameters$state_components
  },
  get_action_components = function() {
    global_markov_parameters$action_components
  },
  get_reward_components = function() {
    global_markov_parameters$reward_components
  }
)
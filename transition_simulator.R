source("markov_utility.R")
TransitionSimulator <- setRefClass("TransitionSimulator", 
                               contains = "MarkovUtility")
TransitionSimulator$methods(
  initialize = function(global_markov_parameters = NA, 
                        test_parameters = NA, ...) {
    callSuper(global_markov_parameters = global_markov_parameters, 
              test_parameters = test_parameters, ...)
  },
  get_reward_and_next_state = function(t, state, action, seed) {
    get_null_reward_and_next_state(state)
  },
  get_null_reward_and_next_state = function(state) {
    reward <- get_zero_reward()
    next_state <- get_zero_transition(state)
    list("reward" = reward, "next_state" = next_state)
  },
  get_zero_reward = function() {
    get_zero_data_frame_row(cols = global_markov_parameters$reward_components)
  },
  get_zero_transition = function(state) {
    state
  }
)
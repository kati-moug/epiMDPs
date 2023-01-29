source("zero_simulator.R")

EpiRelZeroActionSimulator = setRefClass("EpiRelZeroActionSimulator",
                                        contains = "ZeroActionSimulator")

EpiRelZeroActionSimulator$methods(
  initialize = function(...) {
    callSuper(...)
  },
  get_zero_action = function() {
    get_zero_data_frame_row(cols = global_markov_parameters$action_components)
  }
)
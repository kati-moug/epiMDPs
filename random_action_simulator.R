source("action_simulator.R")

RandomActionSimulator <- setRefClass("RandomActionSimulator",
                                     fields = 
                                       list("action_gen" = "ActionGenerator",
                                            "zero_sim" = "ZeroActionSimulator",
                                            "prob_act" = "numeric",
                                            "num_epochs" = "numeric",
                                            "action_epochs" = "numeric"),
                                     contains = "ActionSimulator")

RandomActionSimulator$methods(
  initialize = function(action_gen, zero_sim, prob_act = 1, 
                        ...) {
    action_gen <<- action_gen
    zero_sim <<- zero_sim
    prob_act <<- prob_act
    num_epochs <<- action_gen$global_markov_parameters$num_epochs
    action_epochs <<- 0:(num_epochs-1)
    callSuper(global_markov_parameters = action_gen$global_markov_parameters,
              test_parameters = action_gen$test_parameters,
              ...)
  },
  get_action = function(t, state, seed = NA) {
    if (!(is.na(seed))) set.seed(seed)
    act <- sample(1:2, 1, prob = c(prob_act, 1-prob_act))
    if ((act==1) && (t %in% action_epochs)) {
      eligible_action_names <- action_gen$get_eligible_action_names(t, state)
      action_name <- sample(eligible_action_names, 1)
      action_gen$get_action_by_name(t, state, action_name)
    }
    else {
      zero_sim$get_zero_action()
    }
  },
  set_action_epochs = function(seed, num_action_epochs) {
    set.seed(seed)
    action_epochs <<- sample(0:(num_epochs-1), 
                             num_action_epochs)
  }
)
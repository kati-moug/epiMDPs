source("init_field_reader.R")
library("dplyr")
MarkovTrajSimulator <- setRefClass("MarkovTrajSimulator",
                                fields = list("traj" = "data.frame",
                                              "stochastic_traj" = "data.frame",
                                              "markov_parameters" = 
                                                "GlobalMarkovParameters",
                                              "test_parameters" = "list",
                                              "action_simulator" = 
                                                "ActionSimulator",
                                              "transition_simulator" =
                                                "TransitionSimulator",
                                              "t" = "numeric", 
                                              "state" = "data.frame", 
                                              "action" = "data.frame", 
                                              "reward" = "data.frame",
                                              "next_state" = "data.frame",
                                              "seed" = "numeric",
                                              "save_eligible_actions" = "logical",
                                              "eligible_actions" = "data.frame",
                                              "current_eligible_actions" = "character"),
                                contains = "InitFieldReader")

MarkovTrajSimulator$methods(
  initialize = function(test_params, action_simulator, transition_simulator,
                        initial_seed = 0, save_eligible_actions = FALSE,
                        ...) {
    if (!("initial_state" %in% names(test_params))) stop()
    test_parameters <<- test_params
    traj <<- data.frame()
    stochastic_traj <<- data.frame()
    action_simulator <<- action_simulator
    transition_simulator <<- transition_simulator
    seed <<- initial_seed
    get_markov_parameters()
    state <<- test_parameters$initial_state
    t <<- 0
    save_eligible_actions <<- save_eligible_actions
    eligible_actions <<- data.frame()
  },
  get_markov_parameters = function() {
    a_param <- action_simulator$global_markov_parameters$get_list_fields()
    tr_param <- transition_simulator$global_markov_parameters$get_list_fields()
    if (!(identical(a_param, tr_param))) stop()
    markov_parameters <<- action_simulator$global_markov_parameters
  },
  run = function() {
    while (t <= markov_parameters$num_epochs - 1) simulate_epoch()
    record_final_state()
  },
  run_until_absorb = function() {
    while ((t <= markov_parameters$num_epochs - 1) & 
           !(markov_parameters$is_absorbing(state))) simulate_epoch()
    record_final_state()
  },
  run_fixed_epochs = function(fixed_epochs) {
    while (t <= fixed_epochs - 1) simulate_epoch()
    record_final_state()
  },
  simulate_epoch = function() {
    perform_action()
    get_reward_and_next_state()
    record_epoch()
    transition_state()
  },
  perform_action = function() {
    action <<- action_simulator$get_action(t, state, seed = seed)
    if (save_eligible_actions) record_eligible_actions()
  },
  record_eligible_actions = function() {
    if (nrow(action_simulator$eligible_action_df)== 0) {
      action_simulator$get_eligible_action_df(t, state)
    }
    df <- action_simulator$eligible_action_df
    colnames(df) <- paste_vec(colnames(df), "action", "pre")
    df$epoch <- t
    state_df <- get_with_prefix("state")
    for (i in 1:nrow(df)) {
      for (j in colnames(state_df)) {
        df[i, j] = state_df[1, j]
      }
    }
    eligible_actions <<- rbind(eligible_actions, df)
  },
  get_reward_and_next_state = function() {
    transition <- transition_simulator$get_reward_and_next_state(t, state, action,
                                                                 seed)
    reward <<- transition$reward
    next_state <<- transition$next_state
    if ("stochastic_traj" %in% names(transition)) {
      stochastic_traj <<- rbind(stochastic_traj, transition$stochastic_traj)
    }
  },
  record_epoch = function() {
    with_prefix <- list("state" = NA, "action" = NA, "reward" = NA)
    for (type in names(with_prefix)) with_prefix[[type]] <- get_with_prefix(type)
    epoch <- cbind(data.frame("epoch"=t), with_prefix$state, with_prefix$action, 
                   with_prefix$reward, data.frame("seed"=seed))
    traj <<- rbind(traj, epoch) 
  },
  get_with_prefix = function(type) {
    df <- field(type) 
    colnames(df) <- paste_vec(colnames(df), type, "pre")
    df
  },
  record_final_state = function() {
    epoch <- cbind(data.frame("epoch"=t), get_with_prefix("state"))
    traj <<- bind_rows(traj, epoch)
  },
  transition_state = function() {
    state <<- next_state
    t <<- t + 1
    seed <<- seed + 1
  },
  get_traj = function(index = NA) {
    trajectory <- traj
    if (!(is.na(index))) trajectory$traj <- index
    trajectory
  },
  get_eligible_actions = function(index = NA) {
    ea <- eligible_actions
    if (!(is.na(index))) ea$traj <- index
    ea
  }
)
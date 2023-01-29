source("action_generator.R")
EpiRelSixActionGenerator <- setRefClass("EpiRelSixActionGenerator",
                                        fields = c("actions"),
                               contains = "ActionGenerator")
EpiRelSixActionGenerator$methods(
  initialize = function(...) {
    callSuper(...)
    actions <<- c("high health low rec only",
                  "high health only",
                  "low rec only",
                  "only exclude low health high rec",
                  "all",
                  "none")
  },
  get_action_by_name = function(t, state, action_name) {
    action <- get_zero_data_frame_row(cols = 
                                  global_markov_parameters$action_components)
    if (action_name == "none") return(action)
    if (action_name == "high health low rec only") {
      action[1, "s h1 r0"] = state[1, "s h1 r0"]
    }
    else if (action_name == "high health only") {
      action[1, "s h1 r0"] = state[1, "s h1 r0"]
      action[1, "s h1 r1"] = state[1, "s h1 r1"]
    }
    else if (action_name == "low rec only") {
      action[1, "s h0 r0"] = state[1, "s h0 r0"]
      action[1, "s h1 r0"] = state[1, "s h1 r0"]
    }
    else if (action_name == "only exclude low health high rec") {
      action[1, "s h0 r0"] = state[1, "s h0 r0"]
      action[1, "s h1 r0"] = state[1, "s h1 r0"]
      action[1, "s h1 r1"] = state[1, "s h1 r1"]
    }
    else if (action_name == "all") {
      action[1, "s h0 r0"] = state[1, "s h0 r0"]
      action[1, "s h0 r1"] = state[1, "s h0 r1"]
      action[1, "s h1 r0"] = state[1, "s h1 r0"]
      action[1, "s h1 r1"] = state[1, "s h1 r1"]
    }
    else stop()
    action
  },
  get_eligible_action_names = function(t, state) {
    eligible = c("none")
    if (global_markov_parameters$is_absorbing(state)) return(eligible) # updated June 16
    if (state[1, "s h0 r1"] > 0) eligible <- append(eligible, "all")
    if (state[1, "s h0 r0"] > 0) eligible <- append(eligible, "low rec only")
    if (state[1, "s h1 r1"] > 0) eligible <- append(eligible, 
                                                    "high health only")
    if (state[1, "s h1 r0"] > 0) eligible <- append(eligible, 
                                                    "high health low rec only")
    if ((state[1, "s h0 r0"] > 0)& (state[1,"s h1 r1"]>0)){
      eligible <- append(eligible, "only exclude low health high rec")
    }
    eligible
  },
  # use binary index - is action included or not? - then convert to 10 digit
  get_eligible_actions_ind = function(t, state) {
    eligible_actions = get_eligible_action_names(t, state)
    if (!("none" %in% eligible_actions)) stop()
    ind = 0
    if ("all" %in% eligible_actions) ind = ind + 1
    if ("low rec only" %in% eligible_actions) ind = ind + 2
    if ("high health only" %in% eligible_actions) ind = ind + 4
    if ("high health low rec only" %in% eligible_actions) ind = ind + 8
    if ("only exclude low health high rec" %in% eligible_actions) ind = ind + 16
    ind
  }
)
source("transition_simulator.R")
source("utility_functions.R")
library("EpiModel")
if (packageVersion("EpiModel")!="2.2.1") stop()

epi_folder <- "epi_files/"
source(paste(epi_folder, "param_icm_hr.R", sep = ""))
source(paste(epi_folder, "initialize_icm_hr.R", sep = ""))
source(paste(epi_folder, "infection_icm_hr.R", sep = ""))
source(paste(epi_folder, "prevalence_hr.R", sep = ""))

StochSirTransitionSimulator <- setRefClass("StochSirTransitionSimulator",
                                           fields = c("infn_prob",
                                                      "contact_quad",
                                                      "contact_slope",
                                                      "contact_int",
                                                      "recovery_rate",
                                                      "seed",
                                                      "state",
                                                      "reward",
                                                      "next_state",
                                                      "sim_df",
                                                      "N",
                                                      "len_epoch",
                                                      "p_rec_list",
                                                      "ep_state",
                                                      "s_r_percent",
                                                      "s_r_percent2"),
                                           contains = "TransitionSimulator")

StochSirTransitionSimulator$methods(
  initialize = function(global_markov_parameters = NA, 
                        test_parameters = NA, ...) {
    callSuper(global_markov_parameters = global_markov_parameters, 
              test_parameters = test_parameters, ...)
    initialize_gmp()
    initialize_test_param()
  },
  initialize_gmp = function() {
    gmp <- c("infn_prob", "recovery_rate", "len_epoch", "p_rec_list")
    for (field_name in gmp) {
      field(field_name, global_markov_parameters$field(field_name))
    }
  },
  initialize_test_param = function() {
    test_param <- c("contact_slope", "contact_int")
    for (field_name in test_param) {
      field(field_name, test_parameters[[field_name]])
    }
    initialize_contact_quad()
  },
  initialize_contact_quad = function() {
    if ("contact_quad" %in% names(test_parameters)) {
      contact_quad <<- test_parameters[["contact_quad"]]
    }
    else {
      contact_quad <<- 0
    }
  },
  # take release action, record release cost, transition, record infn cost
  get_reward_and_next_state = function(t, pre_action_state, action, seed) {
    seed <<- seed
    # save release cost to reward data frame
    get_rel_cost(action)
    # save state and next_state to reflect releases
    get_state_after_rel(pre_action_state, action)
    # check that state is not absorbing and then simulate transition
    if (!(global_markov_parameters$is_absorbing(state))) {
      N <<- sum(state[1,]) # get total number in facility
      get_ep_state()
      get_s_r_percent()
      simulate_icm() # save simulation as sim_df
      add_descriptive_cols_sim_df(t)
      get_next_state()
      get_infn_cost() # add infn cost to reward
    }
    else {
      next_state <<- get_zero_transition(state) 
      reward[1, "infn h0"] <<- 0
      reward[1, "infn h1"] <<- 0
      sim_df <<- data.frame()
    }
    list("reward" = reward, "next_state" = next_state, "stochastic_traj" = 
           sim_df)
  },
  get_rel_cost = function(action) {
    reward <<- data.frame()
    for (r in global_markov_parameters$R_rel) {
      all_H_r <- get_str_cartesian_prod(list("s"="s",
                                             "H"=global_markov_parameters$H, 
                                             "r"=r))
      releases <- action[1, all_H_r]
      reward[1, paste("rel", r)] <<- -p_rec_list[[r]]*sum(releases)
    }
  },
  get_state_after_rel = function(pre_action_state, action) {
    state <<- subtract_df_row(pre_action_state, action, 1)
  },
  get_ep_state = function() {
    ep_state <<- data.frame()
    ep_state[1, "s h0"] <<- state[1, "s h0 r0"] +
      state[1, "s h0 r1"] + state[1, "s h0 r2"]
    ep_state[1, "s h1"] <<- state[1, "s h1 r0"] +
      state[1, "s h1 r1"] + state[1, "s h1 r2"]
    ep_state[1, "i h0"] <<- state[1, "i h0"]
    ep_state[1, "i h1"] <<- state[1, "i h1"]
    ep_state[1, "r h0"] <<- state[1, "r h0"]
    ep_state[1, "r h1"] <<- state[1, "r h1"]
  },
  get_s_r_percent = function() {
    if (ep_state[1, "s h0"] != 0) {
      s_r_percent <<- c(state[1, "s h0 r0"] / ep_state[1, "s h0"],
                        state[1, "s h0 r1"] / ep_state[1, "s h0"],
                        state[1, "s h0 r2"] / ep_state[1, "s h0"])
    }
    else {
      s_r_percent <<- c(0, 0, 0)
    }
    if (ep_state[1, "s h1"] != 0) {
      s_r_percent2 <<- c(state[1, "s h1 r0"] / ep_state[1, "s h1"],
                         state[1, "s h1 r1"] / ep_state[1, "s h1"],
                         state[1, "s h1 r2"] / ep_state[1, "s h1"])
    }
    else {
      s_r_percent2 <<- c(0, 0, 0)
    }
  },
  simulate_icm = function() {
    param <- param.icm.hr(inf.prob = infn_prob[1],
                          act.rate = contact_int + contact_slope*N 
                           + contact_quad*N**2, 
                          rec.rate = recovery_rate[1],
                          inf.prob.g2 = infn_prob[2], 
                          rec.rate.g2 = recovery_rate[2],
                          s.r.percent = s_r_percent,
                          s.r.percent.g2 = s_r_percent2)
    
    init <- init.icm(s.num = as.numeric(ep_state[1, "s h0"]), 
                     i.num = as.numeric(ep_state[1, "i h0"]), 
                     r.num = as.numeric(ep_state[1, "r h0"]),
                     s.num.g2 = as.numeric(ep_state[1, "s h1"]), 
                     i.num.g2 = as.numeric(ep_state[1, "i h1"]), 
                     r.num.g2 = as.numeric(ep_state[1, "r h1"]))
    
    control <- control.icm(type = "SIR", nsteps = len_epoch+1, nsims=1, 
                           initialize.FUN = initialize.icm.hr,
                           infection.FUN = infection.icm.hr, 
                           prevalence.FUN = prevalence.hr)
    set.seed(seed)
    sim <- icm(param, init, control)
    sim_df <<- as.data.frame(sim)
    check_sim_df_initial_state()
  },
  get_next_state = function() {
    next_state <<- sim_df[len_epoch+1, global_markov_parameters$sus_components]
    next_state[1, "i h0"] <<- sim_df[len_epoch+1, "i.num"]
    next_state[1, "i h1"] <<- sim_df[len_epoch+1, "i.num.g2"]
    next_state[1, "r h0"] <<- sim_df[len_epoch+1, "r.num"]
    next_state[1, "r h1"] <<- sim_df[len_epoch+1, "r.num.g2"]
  },
  get_infn_cost = function() {
    for (h in global_markov_parameters$H) {
      all_R_h <- get_str_cartesian_prod(list("s"="s", "h"=h,
                                             "R"=global_markov_parameters$R))
      reward[1, paste("infn", h)] <<- (sum(next_state[1, all_R_h]) 
                                     - sum(state[1, all_R_h]))
    }
  },
  add_descriptive_cols_sim_df = function(t) {
    t1 <- t*len_epoch
    t2 <- (t+1)*len_epoch
    when_df <- data.frame('day'=t1:t2)
    when_df['when'] <- NA
    when_df[1,'when']<- 'after action'
    when_df[len_epoch+1,'when']<- 'before action'
    sim_df <<- cbind(when_df,sim_df)
  },
  check_sim_df_initial_state = function() {
    for (comp in global_markov_parameters$sus_components) {
      if (sim_df[1, comp] != state[1, comp]) {
        stop("Sim df is ", sim_df[1,],". Initial state is ", state)
      }
    }
    for (comp in c("i", "r")) {
      if (sim_df[1, paste(comp, "num", sep = ".")] != 
          state[1, paste(comp, "h0")]) {
        stop("Sim df is ", sim_df[1,],". Initial state is ", state)
      }
      if (sim_df[1, paste(comp, "num.g2", sep = ".")] != 
          state[1, paste(comp, "h1")]) {
        stop("Sim df is ", sim_df[1,],". Initial state is ", state)
      }
    }
  }
)
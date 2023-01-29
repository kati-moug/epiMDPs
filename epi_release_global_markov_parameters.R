source("global_markov_parameters.R")
EpiRelGlobalParameters <- setRefClass("EpiRelGlobalParameters",
                                      fields = list("H" = "character", 
                                                    "R" = "character",
                                                    "R_rel" = "character",
                                                    "I" = "character",
                                                    "H_R" = "character",
                                                    "H_R_rel" = "character",
                                                    "len_epoch" = "numeric",
                                                    "p_rec" = "numeric",
                                                    "p_rec_list" = "list"),
                                      contains = "GlobalMarkovParameters")
EpiRelGlobalParameters$methods(
  initialize = function(init_fields_csv = NA, ...) {
    callSuper(init_fields_csv, ...)
    get_H_R_cartesian_sets()
    get_global_parameter_components()
  },
  get_H_R_cartesian_sets = function() {
    H_R <<- get_str_cartesian_prod(list('H' = H, 'R' = R))
    H_R_rel <<- get_str_cartesian_prod(list('H' = H, 'R_rel' = R_rel))
  },
  get_global_parameter_components = function() {
    state_components <<- get_state_components()
    action_components <<- get_action_components()
    reward_components <<- get_reward_components()
    p_rec_list <<- get_p_rec_list()
  },
  get_state_components = function() {
    sus_components <- get_str_cartesian_prod(list('s' = 's', 'H_R' = H_R))
    infn_components <- get_str_cartesian_prod(list('I' = I, 'H' = H))
    append(sus_components, infn_components)
  },
  get_action_components = function() {
    get_str_cartesian_prod(list('s' = 's', 'H_R_rel' = H_R_rel))
  },
  get_reward_components = function() {
    hosp_components <- get_str_cartesian_prod(list('hosp' = 'hosp', 'H' = H))
    rel_components <- get_str_cartesian_prod(list('r' = 'rel', 'R_rel' = R_rel))
    append(hosp_components, rel_components)
  },
  get_p_rec_list = function() {
    lst <- list()
    for (r in 1:length(R_rel)) lst[[R_rel[r]]] <- p_rec[r]
    lst
  }
)

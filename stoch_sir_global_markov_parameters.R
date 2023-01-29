source("epi_release_global_markov_parameters.R")
StochSirGlobalParameters <- setRefClass("StochSirGlobalParameters",
                                      fields = c("infn_prob",
                                                 "recovery_rate",
                                                 "sus_components"),
                                      contains = "EpiRelGlobalParameters")
StochSirGlobalParameters$methods(
  initialize = function(init_fields_csv = NA, ...) {
    callSuper(init_fields_csv, ...)
    
    if (length(H) != 2) stop("Code assumes two health categories.")
    if (length(infn_prob) != length(H)) stop()
    if (length(recovery_rate) != length(H)) stop()
    
  },
  get_state_components = function() {
    sus_components <<- c("s h0 r0", "s h0 r1", "s h0 r2",
                         "s h1 r0", "s h1 r1", "s h1 r2")
    append(sus_components, c('i h0', 'i h1', 'r h0', 'r h1'))
  },
  get_reward_components = function() {
    inf_components <- get_str_cartesian_prod(list('infn' = 'infn', 'H' = H))
    rel_components <- get_str_cartesian_prod(list('r' = 'rel', 'R_rel' = R_rel))
    append(inf_components, rel_components)
  },
  is_absorbing = function(state) {
    ((state[1, "i h0"]+state[1, "i h1"] == 0) || (sum(state[1, sus_components])==0))
  }
)
source("action_generator.R")

EpiRelHealthActionGenerator <- setRefClass("EpiRelHealthActionGenerator",
                                        fields = c("mult",
                                                   "H",
                                                   "R_rel",
                                                   "eligible_H",
                                                   "mult_H"),
                                        contains = "ActionGenerator")

EpiRelHealthActionGenerator$methods(
  initialize = function(mult, ...) {
    mult <<- mult
    callSuper(...)
    initialize_gmp()
  },
  initialize_gmp = function() {
    gmp <- c("H", "R_rel")
    for (field_name in gmp) {
      field(field_name, global_markov_parameters$field(field_name))
    }
  },
  get_eligible_action_names = function(t, state, seed = NA) {
    eligible_actions <- c()
    initialize_eligible_actions(state)
    for (i in 1:nrow(mult_H)) {
      name <- get_name_from_h_total(state, mult_H[i,])
      eligible_actions <- append(eligible_actions, name)
    }
    eligible_H <<- list()
    mult_H <<- list()
    eligible_actions
  },
  initialize_eligible_actions = function(state) {
    initialize_eligible_h(state)
    initialize_multiples()
  },
  initialize_eligible_h = function(state) {
    eligible_h <- list()
    for (h in H) {
      eligible_h[[h]] <- 0
      for (r in R_rel) {
        eligible_h[[h]] <- eligible_h[[h]] + state[1, paste("s",h,r)]
      }
    }
    eligible_H <<- eligible_h
  },
  initialize_multiples = function() {
    mult_h <- list()
    for (h in H) {
      mult_h[[h]] <- seq(0, eligible_H[[h]], mult)
    }
    mult_H <<- expand.grid(mult_h)
  },
  get_name_from_h_total = function(state, h_total) {
    names <- numeric(length(H)*length(R_rel))
    ind <- 1
    for (h in H) {
      h_current <- 0
      for (r in R_rel) {
        required <- h_total[, h] - h_current
        val <- min(required, state[1, paste("s",h,r)])
        h_current <- h_current + val
        names[ind] <- as.character(val)
        ind <- ind + 1
      }
    }
    name <- paste(names, collapse = "_")
  },
  get_action_by_name = function(t, state, name) {
    action <- get_zero_data_frame_row(cols = 
                                    global_markov_parameters$action_components)
    if (name == "none") return(action)
    action_vals <- unlist(strsplit(name, "_"))
    ind <- 1
    for (h in H) {
      for (r in R_rel) {
        val <- as.numeric(action_vals[ind])
        ind <- ind + 1
        action[1, paste("s", h, r)] <- val
      }
    }
    action
  }
)
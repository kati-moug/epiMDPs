source("markov_utility.R")
MarkovResults <- setRefClass("MarkovResults", 
                             fields = list("all_traj" = "data.frame",
                                           "all_stochastic_event_traj" = 
                                             "data.frame",
                                           "num_traj" = "numeric",
                                           "cols" = "list",
                                           "traj_sums" = "data.frame",
                                           "weight_options" = "list",
                                           "disc_options" = "list",
                                           "traj_stats" = "data.frame",
                                           "epoch_stats" = "data.frame"),
                             contains = "MarkovUtility")
MarkovResults$methods(
  initialize = function(num_traj, weight_options, disc_options, ...) {
    callSuper(...)
    num_traj <<- num_traj
    all_traj <<- data.frame()
    all_stochastic_event_traj <<- data.frame()
    traj_sums <<- data.frame()
    weight_options <<- weight_options
    disc_options <<- disc_options
    initialize_weight_options()
    initialize_all_cols()
  },
  initialize_weight_options = function() {
    for (option in names(weight_options)) {
      weight_option <- weight_options[[option]]
      names(weight_option) <- paste_vec(names(weight_option), "reward", "pre")
      update_field_list("weight_options", option, weight_option)
    }
  },
  initialize_all_cols = function() {
    cols <<- list("total_epoch" = c(),
                  "weighted_reward" = c(),
                  "disc_weighted_reward" = c())
    for (type in c("reward", "action")) initialize_element_cols(type)
  },
  initialize_element_cols = function(type) {
    element_cols <- get_components(type)
    element_lst <- list() 
    element_lst[[type]] <- paste_vec(element_cols, type, "pre")
    cols <<- append(cols, element_lst)
  },
  append_traj = function(new_traj, index = NA) {
    if (!(is.na(index))) new_traj$traj <- index
    all_traj <<- rbind(all_traj, new_traj)
  },
  append_stochastic_event_traj = function(new_traj, index = NA) {
    if (!(is.na(index))) new_traj$traj <- index
    all_stochastic_event_traj <<- rbind(all_stochastic_event_traj, new_traj)
  },
  analyze = function() {
    add_epoch_cols_all_traj()
    calculate_traj_totals()
    describe_traj_statistics()
    describe_epoch_statistics()
  },
  add_epoch_cols_all_traj = function() {
    add_all_weighted_reward_cols() # checked for accuracy 11-1-21
    add_all_disc_reward_cols()
    sum_over_epoch_all_traj_all_cols() 
  },
  add_all_weighted_reward_cols = function() {
    for (weight_name in names(weight_options)){
      add_weighted_reward_cols(weight_name) 
    } 
  },
  add_weighted_reward_cols = function(weight_name) {
    weighted_reward_cols = paste_vec(cols$reward, weight_name, "pre", sep = "-")
    for (reward_col in cols$reward) add_weighted_reward_col(weight_name, reward_col)
    add_total_weighted_reward_col(weighted_reward_cols, weight_name)
  },
  add_weighted_reward_col = function(weight_name, reward_col) {
    weighted_reward_col <- paste(weight_name, reward_col, sep = "-")
    weight <- weight_options[[weight_name]]
    if (!(weighted_reward_col %in% colnames(all_traj))) {
      weighted_reward <- weight[[reward_col]]*all_traj[reward_col]
      colnames(weighted_reward)[1] <- paste(weighted_reward_col)
      all_traj <<- cbind(all_traj, weighted_reward)
      append_vec_in_field_list("cols", "weighted_reward", weighted_reward_col)
    }
  },
  add_total_weighted_reward_col = function(weighted_reward_cols, weight_name) {
    colname <- paste("total epoch", weight_name, "reward")
    total_col <- as.data.frame(rowSums(all_traj[, weighted_reward_cols]))
    colnames(total_col)[1] <- colname
    all_traj <<- cbind(all_traj, total_col)
    append_vec_in_field_list("cols", "total_epoch", colname)
  },
  
  add_all_disc_reward_cols = function() {
    for (disc_name in names(disc_options)) add_disc_reward_cols(disc_name)
  },
  sum_disc_reward_over_epoch = function() {
    wd_options <- get_str_cartesian_prod(list("disc" = names(disc_options),
                                              "weight" = names(weight_options)), 
                                         sep = "-")
    for (wd in wd_options) {
      wd_cols <- paste_vec(cols$reward, wd, "pre", sep = "-")
      sum_over_epoch_all_traj(paste(wd, "reward"), element_cols = wd_cols)
    }
  },
  add_disc_reward_cols = function(disc_name) {
    add_disc_col(disc_name)
    for (col in cols$weighted_reward) add_disc_reward_col(col, disc_name)
  },
  add_disc_col = function(disc_name) {
    discount <- disc_options[[disc_name]]
    disc_function <- function(x) discount**x
    epoch_discounts <- as.data.frame(apply(all_traj["epoch"], 1, disc_function))
    names(epoch_discounts)[1] <- disc_name
    all_traj <<- cbind(all_traj, epoch_discounts)
  },
  add_disc_reward_col = function(col, disc_name) {
    discount <- disc_options[[disc_name]]
    disc_reward_colname <- paste(disc_name, col, sep = "-")
    disc_reward <- as.data.frame(all_traj[disc_name]*all_traj[col])
    colnames(disc_reward)[1] <- disc_reward_colname
    all_traj <<- cbind(all_traj, disc_reward)
    append_vec_in_field_list("cols", "disc_weighted_reward", disc_reward_colname)
  },
  sum_over_epoch_all_traj_all_cols = function() {
    for (type in c("reward", "action")) sum_over_epoch_all_traj(type)
    sum_disc_reward_over_epoch()
  },
  sum_over_epoch_all_traj = function(element_type, element_cols = "n") {
    sum_colname <- paste("total epoch", element_type)
    if (length(element_cols)==1) {
      if (element_cols=="n") element_cols <- cols[[element_type]]
    }
    sum_components <- as.data.frame(rowSums(all_traj[, element_cols]))
    colnames(sum_components)[1] <- sum_colname
    all_traj <<- cbind(all_traj, sum_components)
    append_vec_in_field_list("cols", "total_epoch", sum_colname)
  },
  calculate_traj_totals = function() {
    traj_sum_cols <- get_traj_sum_cols()
    for (traj in 1:num_traj) {
      one_traj_sum <- sum_over_traj_cols(traj_sum_cols, traj)
      rownames(one_traj_sum)[1] <- traj
      colnames(one_traj_sum) <- paste_vec(colnames(one_traj_sum), "total traj",
                                          "pre")
      traj_sums <<- rbind(traj_sums, one_traj_sum)
    }
    
  },
  get_traj_sum_cols = function() {
    traj_sum_cols <- c()
    for (name in names(cols)) {
      for (item in cols[[name]]) {
        traj_sum_cols <- append(traj_sum_cols, item)
      }
    } 
    traj_sum_cols
  },
  sum_over_traj_cols = function(traj_cols, traj) {
    one_traj <- all_traj[all_traj$traj == traj, ]
    t(as.data.frame(colSums(one_traj[, traj_cols], na.rm = TRUE)))
  },
  describe_traj_statistics = function() {
    traj_stats <<- as.data.frame(summary(traj_sums))
  },
  describe_epoch_statistics = function() {
    all_traj_no_index <- all_traj[, !(colnames(all_traj) %in% c("traj"))]
    epoch_stats <<- data.frame()
    for (t in 0:max(all_traj_no_index$epoch)) {
      epoch_summary <- as.data.frame(
        summary(all_traj_no_index[(all_traj_no_index$epoch==t),]))
      epoch_summary$epoch <- t
      epoch_stats <<- rbind(epoch_stats, epoch_summary)
    }
  }
)
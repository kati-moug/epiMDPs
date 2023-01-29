source("sparsity_algorithm_with_clusters.R")
source("discretize.R")
library("dplyr")
SparsitySimulator <- setRefClass("SparsitySimulator",
                                 fields = list("alg_iterations" = "numeric",
                                   "epoch_states" = "data.frame",
                                   "epoch_state_clusters" = "vector",
                                   "epoch_state_centers" = "data.frame",
                                   "action_horizon" = "numeric",
                                   "action_epoch" = "numeric",
                                   "num_epochs" = "numeric",
                                   "all_traj" = "data.frame",
                                   "num_traj" = "numeric",
                                   "next_epoch_states" = "data.frame",
                                   "center_actions" = "character",
                                   "all_epoch_state_clusters" = "data.frame",
                                   "all_epoch_state_centers" = "data.frame",
                                   "frac_epoch_state_centers" = "data.frame",
                                   "all_fractional_epoch_state_centers_with_action" = "data.frame",
                                   "folder" = "character",
                                   "sep_clusters_by_action" = "logical",
                                   "read_traj_seeds" = "logical",
                                   "traj_seeds" = "data.frame",
                                   "runtime" = "data.frame"),
                                 contains = "SparsityAlg")

SparsitySimulator$methods(
  initialize = function(num_traj, alg_iterations, folder, sep_clusters_by_action,
                        read_traj_seeds, traj_seeds, ...) {
    callSuper(...)
    print("Check that trans_sim and action_sim have the same global Markov.")
    num_traj <<- num_traj
    alg_iterations <<- alg_iterations
    folder <<- folder
    action_label_multiplier <<- action_label_multiplier
    sep_clusters_by_action <<- sep_clusters_by_action
    read_traj_seeds <<- read_traj_seeds
    traj_seeds <<- traj_seeds
    action_epoch <<- 0
    action_horizon <<- horizon # action_horizon will be fixed, horizon will change
    all_epoch_state_clusters <<- data.frame()
    all_epoch_state_centers <<- data.frame()
    frac_epoch_state_centers <<- data.frame()
    all_traj <<- data.frame()
    runtime <<- data.frame()
    num_epochs <<- trans_sim$global_markov_parameters$num_epochs
    if (action_gen$global_markov_parameters$num_epochs != num_epochs) stop()
    if (num_final_epochs+action_horizon != num_epochs) stop()
    save_fields_to_file()
    # initialize epoch_states as num_traj row copies of initial_state
    initialize_epoch_states()
    # initialize epoch_state_centers as initial state
    # initialize epoch_state_clusters as 1 for all trajectories
    find_epoch_states_clusters(k = 1, sep_by_action = FALSE)
    all_fractional_epoch_state_centers_with_action <<- data.frame()
  },
  initialize_epoch_states = function() {
    epoch_states <<- data.frame()
    for (i in 1:num_traj) {
      for (col in colnames(initial_state)) {
        epoch_states[i, col] <<- initial_state[1, col]
      }
    }
  },
  save_fields_to_file = function() {
    field_names <- names(getRefClass()$fields())
    dump(field_names, file = paste(folder,"sparsity_simulator_fields.R", sep=''))
  },
  run = function() {
    print("Action Epoch")
    print(0)
    for (t in 1:action_horizon) {
      get_actions_and_simulate_epoch()
      print("Action Epoch")
      print(t)
      action_epoch <<- t
      update_epoch_states_and_get_new_centers()
      save_results_to_file()
    }
    finish_all_traj()
    save_results_to_file()
    create_markov_results_analyze_and_store()
  },
  get_actions_and_simulate_epoch = function() {
    # set sparsity algorithm horizon to total number of actions taken starting at
    # action_epoch
    horizon <<- action_horizon - action_epoch
    next_epoch_states <<- data.frame()
    center_actions <<- character(nrow(epoch_state_centers))
    print("Action Horizon")
    print(action_horizon)
    print("Horizon")
    print(horizon)
    get_center_actions()
    print("All center actions")
    print(center_actions)
    simulate_all_traj_with_actions()
  },
  get_center_actions = function() {
    for (row in 1:nrow(epoch_state_centers)) {
      str1 <- paste("center", paste(row, "sparsity alg tree.csv"))
      str2 <- paste("epoch", paste(action_epoch, str1))
      tree_storage <<- paste(folder, str2, sep="")
      cluster_storage <<- paste(folder, "epoch", action_epoch, "center", row,
                                "sparsity alg clusters.csv")
      initial_state <<- epoch_state_centers[row, ]
      tree <<- initial_state
      print("Center actions, initial state")
      print(initial_state)
      alg_tic <- Sys.time()
      Q <- run_algorithm()
      alg_toc <- Sys.time()
      r_ind <- nrow(runtime)+1
      runtime[r_ind, "Action Epoch"] <<- action_epoch
      runtime[r_ind, "Test"] <<- paste("Algorithm, cluster",row)
      runtime[r_ind, "Runtime (secs)"] <<- difftime(alg_toc, alg_tic, 
                                                    units="secs")
      print("Q")
      print(Q)
      action_ind <- which.max(Q)
      center_actions[row] <<- names(Q)[action_ind]
    }
  },
  simulate_all_traj_with_actions = function() {
    print("Simulate all traj with actions")
    traj_tic <- Sys.time()
    for (traj in 1:num_traj) {
      center <- epoch_state_clusters[traj]
      action_name <- center_actions[center]
      simulate_epoch(traj, action_name)
    }
    traj_toc <- Sys.time()
    r_ind <- nrow(runtime)+1
    runtime[r_ind, "Action Epoch"] <<- action_epoch
    runtime[r_ind, "Test"] <<- "Simulate epoch, all traj"
    runtime[r_ind, "Runtime (secs)"] <<- difftime(traj_toc, traj_tic, 
                                                  units="secs")
  },
  simulate_epoch = function(traj, action_name) {
    if (read_traj_seeds) {
      traj_seed <- traj_seeds[((traj_seeds$Traj==traj)
                              &(traj_seeds$Epoch==action_epoch)),"Seed"]
      if (length(traj_seed)>1) stop()
      if (traj_seed >= seed) stop()
    }
    else {
      record_seed_tracking("Transition", traj_data = traj, 
                           epoch_data = action_epoch)
      traj_seed <- seed
    }
    
    state <- epoch_states[traj, ]
    action <- action_gen$get_action_by_name(action_epoch, state, action_name)
    trans <- trans_sim$get_reward_and_next_state(action_epoch, state, action, 
                                                 traj_seed)
    ind <- nrow(all_traj) + 1
    all_traj[ind, "traj"] <<- traj
    all_traj[ind, "epoch"] <<- action_epoch
    all_traj[ind, "seed"] <<- traj_seed
    for (name in colnames(state)) {
      all_traj[ind, paste("state", name)] <<- state[, name]
    }
    for (name in colnames(action)) {
      all_traj[ind, paste("action", name)] <<- action[, name]
    }
    for (name in names(trans$reward)) {
      all_traj[ind, paste("reward", name)] <<- trans$reward[[name]]
    }
    for (col in colnames(trans$next_state)) {
      next_epoch_states[traj, col] <<- trans$next_state[1, col]
    }
    if (!(read_traj_seeds)) seed <<- seed + 1
  },
  update_epoch_states_and_get_new_centers = function() {
    epoch_states <<- next_epoch_states
    cluster_tic <- Sys.time()
    find_epoch_states_clusters(sep_by_action = sep_clusters_by_action)
    cluster_toc <- Sys.time()
    r_ind <- nrow(runtime)+1
    runtime[r_ind, "Action Epoch"] <<- action_epoch
    runtime[r_ind, "Test"] <<- paste("Finding clusters")
    runtime[r_ind, "Runtime (secs)"] <<- difftime(cluster_toc, cluster_tic, 
                                                  units="secs")
  },
  find_epoch_states_clusters = function(k = NA, sep_by_action = FALSE) {
    if (sep_by_action) add_action_label_to_epoch_states()
    if (is.na(k)) k <- alg_iterations
    if (sep_by_action) {
      # get number of action indices
      action_df <- epoch_states$action
      uniq_actions <- unique(action_df)
      uniq <- length(uniq_actions)
      if (uniq > k) k <- uniq
    }
    cl <- kmeans(epoch_states,k)
    epoch_state_clusters <<- cl$cluster
    store_epoch_state_clusters()
    centers_df <- as.data.frame(cl$centers)
    colnames(centers_df) <- colnames(epoch_states)
    epoch_state_centers <<- centers_df
  
    if (sep_by_action) {
      all_fractional_epoch_state_centers_with_action <<- rbind(
        all_fractional_epoch_state_centers_with_action, epoch_state_centers
      )
      epoch_state_centers <<- subset(epoch_state_centers, select=-c(action))
      epoch_states <<- subset(epoch_states, select=-c(action))
    }
    for (row in 1:k) {
      fractional_center <- epoch_state_centers[row,]
      epoch_state_centers[row,] <<- discretize(fractional_center)
      
      # record fractional_center and epoch_state_center
      fractional_center[, "Epoch"] <- action_epoch
      fractional_center[, "Cluster"] <- row
      frac_epoch_state_centers <<- rbind(frac_epoch_state_centers,
                                         fractional_center)
      
      epoch_state_center <- epoch_state_centers[row,]
      epoch_state_center[, "Epoch"] <- action_epoch
      epoch_state_center[, "Cluster"] <- row
      all_epoch_state_centers <<- rbind(all_epoch_state_centers,
                                         epoch_state_center)
    }
  },

  add_action_label_to_epoch_states = function() {
    unique_epoch_states <- unique(epoch_states)
    unique_epoch_state_ind <- unique_epoch_states
    # for each state, get available actions
    for (i in 1:nrow(unique_epoch_states)) {
      ind <- action_gen$get_eligible_actions_ind(action_epoch, 
                                                 unique_epoch_states[i,])
      unique_epoch_state_ind[i, "action"] <- ind*action_label_multiplier
    }
    # preserve order of epoch_states
    merge_cols <- colnames(epoch_states)
    epoch_states$traj <<- 1:num_traj
    epoch_states <<- merge(epoch_states, unique_epoch_state_ind, 
                          by=merge_cols) 
    epoch_states <<- epoch_states[order(epoch_states$traj),]
    epoch_states <<- subset(epoch_states, select=-c(traj))
  },
  store_epoch_state_clusters = function() {
    ind <- nrow(all_epoch_state_clusters) + 1
    for (traj in 1:num_traj) {
      all_epoch_state_clusters[ind, "Traj"] <<- traj
      all_epoch_state_clusters[ind, "Epoch"] <<- action_epoch
      all_epoch_state_clusters[ind, "Cluster"] <<- epoch_state_clusters[traj]
      ind <- ind + 1
    }
  },
  save_results_to_file = function() {
    write.csv(all_epoch_state_clusters,
              paste(folder, "all_epoch_state_clusters.csv", sep = ""))
    write.csv(all_epoch_state_centers,
              paste(folder, "all_epoch_state_centers.csv", sep = ""))
    write.csv(all_traj,
              paste(folder, "all_traj.csv", sep = ""))
    write.csv(frac_epoch_state_centers,
              paste(folder, "frac_epoch_state_centers.csv", sep = ""))
    write.csv(all_fractional_epoch_state_centers_with_action, 
              paste(folder, "all_fractional_epoch_state_centers_with_action.csv",
                    sep = ""))
    if (!(read_traj_seeds)) write.csv(seed_tracking[!(is.na(seed_tracking$Traj)),], paste(folder,'seeds.csv', sep=''))
    write.csv(runtime, paste(folder,"runtime.csv", sep=""))
  },
  finish_all_traj = function() {
    all_traj_tic <- Sys.time()
    for (traj in 1:num_traj) {
      if (read_traj_seeds) {
        traj_seed <- traj_seeds[((traj_seeds$Traj==traj)
                                &(traj_seeds$Epoch==action_horizon)),"Seed"]
        if (length(traj_seed)>1) stop()
        if (traj_seed >= seed) stop()
      }
      else {
        record_seed_tracking("Final Traj", traj_data = traj)
        traj_seed <- seed
      }
      markov_traj <- MarkovTrajSimulator$new(test_params = list(
                                          "initial_state"=epoch_states[traj,]),
                                             action_simulator = zero_sim,
                                             transition_simulator = trans_sim,
                                             initial_seed = traj_seed)
      markov_traj$run_fixed_epochs(num_epochs-action_horizon)
      traj_df = markov_traj$get_traj(index = traj) 
      traj_df$epoch <- traj_df$epoch + action_horizon
      all_traj <<- bind_rows(all_traj, traj_df) 
      if (!(read_traj_seeds)) {seed <<- markov_traj$seed}
    }
    all_traj_toc <- Sys.time()
    r_ind <- nrow(runtime)+1
    runtime[r_ind, "Action Epoch"] <<- action_epoch
    runtime[r_ind, "Test"] <<- "Final trajectories"
    runtime[r_ind, "Runtime (secs)"] <<- difftime(all_traj_toc, all_traj_tic, 
                                                  units="secs")
  },
  create_markov_results_analyze_and_store = function() {
    markov_results <- MarkovResults$new(num_traj=num_traj,
                                        weight_options=list("weight 1"=weight),
                                        disc_options=list("disc 1"=discount),
                  global_markov_parameters=trans_sim$global_markov_parameters)
    markov_results$all_traj <- all_traj
    analyze_tic <- Sys.time()
    markov_results$analyze()
    analyze_toc <- Sys.time()
    r_ind <- nrow(runtime)+1
    runtime[r_ind, "Test"] <<- paste("Analyze Markov results")
    runtime[r_ind, "Runtime (secs)"] <<- difftime(analyze_toc, analyze_tic, 
                                                  units="secs")
    write.csv(markov_results$all_traj, paste(folder,"all_traj_with_reward.csv",
                                             sep=""))
    write.csv(markov_results$traj_sums, paste(folder,"traj_sums.csv",sep=""))
    write.csv(runtime, paste(folder,"runtime.csv",sep=""))
  }
)
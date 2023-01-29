source("run_markov_traj_record_and_analyze.R")

SparsityAlg <- setRefClass("SparsityAlg", 
                            fields = list("discount" = "numeric",
                                          "action_gen" = "ActionGenerator",
                                          "zero_sim" = "ZeroActionSimulator",
                                          "trans_sim" = "TransitionSimulator",
                                          "weight" = "list",
                                          "children" = "numeric",
                                          "horizon" = "numeric",
                                          "num_final_epochs" = "numeric",
                                          "num_final_replicates" = "numeric",
                                          "seed" = "numeric",
                                          "tree" = "data.frame",
                                          "sample_r" = "data.frame",
                                          "mean_sample_r" = "data.frame",
                                          "sample_s" = "data.frame",
                                          "seed_tracking" = "data.frame",
                                          "total_mean_weighted_reward" = "numeric",
                                          "initial_state" = "data.frame",
                                          "tree_storage" = "character",
                                          "absorbing_checker" = "GlobalMarkovParameters",
                                          "sample_number" = "numeric",
                                          "action_label_multiplier" = "numeric",
                                          "cluster_df" = "data.frame",
                                          "cluster_storage" = "character"))
SparsityAlg$methods(
  initialize = function(horizon, children, discount, action_gen, zero_sim,
                        trans_sim, initial_state, weight,
                        num_final_epochs, num_final_replicates, initial_seed,
                        absorbing_checker, sample_number, action_label_multiplier,
                        cluster_storage = "cluster_df.csv",
                        tree_storage = "alg_tree.csv") {
    discount <<- discount
    action_gen <<- action_gen
    zero_sim <<- zero_sim
    trans_sim <<- trans_sim
    weight <<- weight
    children <<- children
    horizon <<- horizon
    num_final_epochs <<- num_final_epochs
    num_final_replicates <<- num_final_replicates
    seed <<- initial_seed
    tree <<- initial_state
    seed_tracking <<- data.frame()
    sample_r <<- data.frame()
    mean_sample_r <<- data.frame()
    sample_s <<- data.frame()
    cluster_df <<- data.frame()
    total_mean_weighted_reward <<- 0
    initial_state <<- initial_state
    tree_storage <<- tree_storage
    absorbing_checker <<- absorbing_checker
    sample_number <<- sample_number
    action_label_multiplier <<- action_label_multiplier
    cluster_storage <<- cluster_storage
  },
  run_algorithm = function() {
    Q <- estimate_Q(horizon, initial_state, 1) 
    if (tree_storage != "skip") write.csv(tree, tree_storage)
    Q
  },
  estimate_Q = function(depth, state, state_ind) {
    if (absorbing_checker$is_absorbing(state)) return(list("none"=0))
    else if (depth==0) return(estimate_final_Q(horizon, state))
    else {
      if (tree_storage != "skip") write.csv(tree, tree_storage)
      write.csv(cluster_df, cluster_storage)
    }
    Q <- list()
    actions <- action_gen$get_eligible_action_names(horizon-depth, state)
    for (a in actions) {
      Q[[a]] <- estimate_Q_a(depth, state, state_ind, a)
      tree[state_ind, paste("Qsa",a)] <<- Q[[a]]
    }
    Q
  },
  estimate_final_Q = function(t, state) {
    record_seed_tracking("final Q")
    results <- run_markov_traj_record_and_analyze(num_final_replicates, 
              list("weight 1"=weight), list("disc 1"=discount), zero_sim, 
              trans_sim, list("initial_state"=state), num_final_epochs, seed)
    seed <<- results$seed
    list("none"=mean(results$markov_results$traj_sums[['total traj total epoch disc 1-weight 1 reward']]))
  },
  estimate_Q_a = function(depth, state, state_ind, action_name) {
      action <- action_gen$get_action_by_name(horizon-depth, state, action_name)
      samples <- sample_transition(horizon-depth, state, action)
      cluster_states <- get_cluster_states(samples$states, horizon-depth+1,
                                           state_ind, action_name)
      
      # if all are absorbing, only current reward matters
      if (nrow(cluster_states$states)==0) return(samples$total_mean_weighted_reward)
      
      v_total <- 0
      
      for (i in 1:nrow(cluster_states$states)) {
        prob <- cluster_states$prob[i]
        next_state_ind <- nrow(tree) + 1
        next_state <- cluster_states$states[i,]
        store_state(next_state_ind, next_state, state, action, depth-1, state_ind,
                    prob)
        v_total <- v_total + prob*estimate_V(depth-1, next_state, next_state_ind)
      }
      v_total*discount+samples$total_mean_weighted_reward
  },
  sample_transition = function(t, state, action) {
    initialize_sample_transition()
    for (c in 1:sample_number) sample_child(t, state, action) 
    get_sample_total_mean_weighted_reward()
    list("states" = sample_s, 
         "total_mean_weighted_reward" = total_mean_weighted_reward)
  },
  initialize_sample_transition = function() {
    sample_r <<- data.frame()
    sample_s <<- data.frame()
  },
  sample_child = function(t, state, action) {
    record_seed_tracking("Sample Child")
    sample <- trans_sim$get_reward_and_next_state(t, state, action, seed)
    sample_r <<- rbind(sample_r, sample$reward)
    sample_s <<- rbind(sample_s, sample$next_state)
    seed <<- seed + 1
  },
  record_seed_tracking = function(str, traj_data = NA, epoch_data = NA) {
    ind <- nrow(seed_tracking) + 1
    seed_tracking[ind, "Seed"] <<- seed
    seed_tracking[ind, "Sample"] <<- str
    if (!(is.na(traj_data))) seed_tracking[ind, "Traj"] <<- traj_data
    if (!(is.na(epoch_data))) seed_tracking[ind, "Epoch"] <<- epoch_data
  },
  get_sample_total_mean_weighted_reward = function() {
    mean_sample_r <<- as.data.frame(colMeans(sample_r))
    total_mean_weighted_reward <<- 0
    for (name in names(weight)) add_mean_weighted_reward_component(name)
  },
  add_mean_weighted_reward_component = function(name) {
    total_mean_weighted_reward <<- (total_mean_weighted_reward 
                                    + weight[[name]]*mean_sample_r[name, 1]) 
  },
  estimate_V = function(depth, state, state_ind) {
    Q <- estimate_Q(depth, state, state_ind) 
    V <- max(unlist(Q))
    tree[state_ind, "V"] <<- V
    V
  },
  store_state = function(state_ind, state, parent_state, parent_action, depth,
                         parent_ind, prob) {
    tree[state_ind, "Depth"] <<- depth
    tree[state_ind, "Parent index"] <<- parent_ind
    # store state and parent state
    for (col in colnames(state)) {
      if (col != "action"){
      tree[state_ind, col] <<- state[1, col]
      tree[state_ind, paste("Parent state", col)] <<- parent_state[1, col]
      }
    }
    # store parent action
    for (col in colnames(parent_action)) {
      tree[state_ind, paste("Parent action", col)] <<- parent_action[1, col]
    }
    # store prob
    tree[state_ind, "Prob"] <<- prob
  },
  get_cluster_states = function(states, t, parent_index, parent_action) {
    current_states <- states
    current_states$parent_index <- parent_index
    current_states$parent_action <- parent_action
    current_states$category <- "All States"
    current_states$cluster <- "N/A"
    current_states$action <- "N/A"
    cluster_df <<- rbind(cluster_df, current_states)
    
    # get all absorbing states & exclude from analysis
    absorbing <- c()
    for (i in 1:sample_number) {
      if (absorbing_checker$is_absorbing(states[i,])) {
        absorbing <- append(absorbing, i)
      }
    }
    if (length(absorbing)==sample_number) return(list("states"=data.frame()))
    row.names(states) <- 1:nrow(states)
    nonabsorbing_states <- states[! row.names(states) %in% absorbing,]
    # get actions
    unique_states <- unique(nonabsorbing_states)
    unique_state_ind <- unique_states
    # for each state, get available actions
    for (i in 1:nrow(unique_states)) {
      ind <- action_gen$get_eligible_actions_ind(t, unique_states[i,])
      unique_state_ind[i, "action"] <- ind*action_label_multiplier
    }
    # preserve order of nonabsorbing_states
    merge_cols <- colnames(nonabsorbing_states)
    nonabsorbing_states$index <- row.names(nonabsorbing_states)
    nonabsorbing_states <- merge(nonabsorbing_states, unique_state_ind, 
                           by=merge_cols) 
    nonabsorbing_states <- nonabsorbing_states[order(nonabsorbing_states$index),]
    nonabsorbing_states <- subset(nonabsorbing_states, select=-c(index))
    
    # make sure you have as many states as unique actions
    k <- children
    action_df <- nonabsorbing_states$action
    uniq_actions <- unique(action_df)
    uniq <- length(uniq_actions)
    if (uniq > k) k <- uniq
    if (k >= nrow(nonabsorbing_states)) {
      unique_k <- nrow(unique(nonabsorbing_states))
      if (unique_k < nrow(nonabsorbing_states)) {
        k <- unique_k
      }
      else {
        return(list("states"=nonabsorbing_states,
                    "prob"=1/sample_number * rep(1, nrow(nonabsorbing_states))))
      }
    }
  
    cl <- kmeans(nonabsorbing_states,k)
    
    with_cluster <- nonabsorbing_states
    with_cluster$parent_index <- parent_index
    with_cluster$parent_action <- parent_action
    with_cluster$cluster <- cl$cluster
    with_cluster$category <- "After Cluster Analysis"
    cluster_df <<- rbind(cluster_df, with_cluster)
    state_clusters <- table(cl$cluster)
    prob <- numeric(k)
    for (i in 1:k) prob[i] <- state_clusters[names(state_clusters)==i] / sample_number
    
    centers_df <- as.data.frame(cl$centers)
    colnames(centers_df) <- colnames(nonabsorbing_states)
    save_center <- centers_df
    save_center$parent_index <- parent_index
    save_center$parent_action <- parent_action
    save_center$cluster <- 1:k
    save_center$category <- "Fractional Center"
    cluster_df <<- rbind(cluster_df, save_center)
    
    state_centers <- subset(centers_df, select=-c(action))
    
    for (row in 1:k) {
      fractional_center <- unlist(state_centers[row,])
      state_centers[row,] <- discretize(fractional_center)
    }
    save_final_center <- state_centers
    save_final_center$parent_index <- parent_index
    save_final_center$parent_action <- parent_action
    save_final_center$cluster <- 1:k
    save_final_center$category <- "Final Center"
    save_final_center$action <- "N/A"
    cluster_df <<- rbind(cluster_df, save_final_center)
    list("states" = state_centers, "prob" = prob)
  }
)
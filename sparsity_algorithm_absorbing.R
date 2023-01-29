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
                                          "absorbing_checker" = "GlobalMarkovParameters"))
SparsityAlg$methods(
  initialize = function(horizon, children, discount, action_gen, zero_sim,
                        trans_sim, initial_state, weight,
                        num_final_epochs, num_final_replicates, initial_seed,
                        absorbing_checker,
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
    total_mean_weighted_reward <<- 0
    initial_state <<- initial_state
    tree_storage <<- tree_storage
    absorbing_checker <<- absorbing_checker
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
      v_total <- 0
      
      for (i in 1:nrow(samples$states)) {
        next_state_ind <- nrow(tree) + 1
        next_state <- samples$states[i,]
        store_state(next_state_ind, next_state, state, action, depth-1, state_ind)
        v_total <- v_total + estimate_V(depth-1, next_state, next_state_ind)
      }
      v_total/children*discount+samples$total_mean_weighted_reward
  },
  sample_transition = function(t, state, action) {
    initialize_sample_transition()
    for (c in 1:children) sample_child(t, state, action) 
    get_sample_total_mean_weighted_reward()
    list("states" = sample_s, 
         "total_mean_weighted_reward" = total_mean_weighted_reward)
  },
  initialize_sample_transition = function() {
    sample_r <<- data.frame()
    sample_s <<- data.frame()
  },
  sample_child = function(t, state, action) {
    record_seed_tracking("Child")
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
                         parent_ind) {
    tree[state_ind, "Depth"] <<- depth
    tree[state_ind, "Parent index"] <<- parent_ind
    # store state and parent state
    for (col in colnames(state)) {
      tree[state_ind, col] <<- state[1, col]
      tree[state_ind, paste("Parent state", col)] <<- parent_state[1, col]
    }
    # store parent action
    for (col in colnames(parent_action)) {
      tree[state_ind, paste("Parent action", col)] <<- parent_action[1, col]
    }
  }
)
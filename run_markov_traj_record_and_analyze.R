source("markov_traj_simulator.R")
source("markov_results.R")
run_markov_traj_record_and_analyze <- function(num_traj, weight_options,
                                               disc_options, action_sim,
                                               trans_sim, test_param, 
                                               fixed_epochs, initial_seed, 
                                               print = FALSE) {
  markov_traj <- MarkovTrajSimulator$new(test_params = test_param,
                                         action_simulator = action_sim,
                                         transition_simulator = trans_sim)
  
  markov_results <- MarkovResults$new(num_traj=num_traj,
                                      weight_options=weight_options,
                                      disc_options=disc_options,
                        global_markov_parameters=markov_traj$markov_parameters)
  seed <- initial_seed
  for (i in 1:num_traj){
    if (print) print(paste("Final Traj",i))
    markov_traj <- MarkovTrajSimulator$new(test_params = test_param,
                                           action_simulator = action_sim,
                                           transition_simulator = trans_sim,
                                           initial_seed = seed)
    markov_traj$run_fixed_epochs(fixed_epochs)

    traj = markov_traj$get_traj(index = i)
    markov_results$append_traj(traj)
    seed <- markov_traj$seed
  }
  markov_results$analyze()
  list("markov_results"=markov_results, "seed"=seed)
}
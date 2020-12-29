# read in create pomp model wrapper
# load("../../../data/result_neutral.Rdata")

# parameters of interest
est_these <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
               "rho_A", "rho_B")


# natural constraints on the parameters 

list(lower = c(R0_B = 0.5, R0_A = 0.5, amplitude_A = 1e-10, 
               amplitude_B = 1e-10, tpeak_A = 1/365.25, tpeak_B = 1/365.25,  
               rho_A = 1e-10, rho_B = 1e-10), 
     upper = c(R0_B = 25, R0_A = 25, amplitude_A = 1, amplitude_B = 1, 
               tpeak_A = 1, tpeak_B = 1, rho_A = 0.01, rho_B = 0.01)) -> param_bounds

# using the mle simulate the oberavtion process 

neutral_mle <- params_all
neutral_mle[c("phi_A", "phi_B")] <- 0  
neutral_mle[est_these] <- result_neutral$GAobj@solution[1,]

# collect the time vector 
inc_data %>% 
  filter(flu_type == "total_b") %>% 
  select(time) %>% 
  unlist() %>% unname() -> time_vec

# this set of functions will be moved to a util functions script for general use after the workflow is set up


# simulate 1000 realizations from the observation process
set.seed(12345)
sim_obs_model(po_obj = pomp_sirs, 
              params = neutral_mle, times = time_vec, 
              nsim = 1000, method = "ode45", long_form = TRUE) %>% 
  transmute(time = time, 
            `.id` = `.id`,
            flu_type = FluType, 
            obs_cases = SimObs) -> mle_sim_data
  
  
# of(par = result_neutral$GAobj@solution[1,], est = est_these, 
#    params_all_int = neutral_mle)  
  
est_on_sim_data <- function (counter, target_data = mle_sim_data) {
  # browser()
  mle_sim_data %>% 
    filter(`.id` == counter) %>% 
    select(-`.id`) -> tmp_target_data
  
  
  of_int <- function(par, est = est_these, 
                     target_data = tmp_target_data) {
    -of(par = par, est = est, target_data = target_data, 
        params_all_int = neutral_mle)
  }
  
  optim(par = result_neutral$GAobj@solution[1,], fn = of_int, method = "L-BFGS-B",
        upper = param_bounds$upper, lower = param_bounds$lower) -> result
  
  return(list(simulation = counter, result = result))
  
}


bootstrapped_params_neutral_list <- mclapply(1:1000, est_on_sim_data, mc.set.seed = TRUE, 
                                             mc.cores = detectCores())


# save(bootstrapped_params_neutral_list, file = "bootstrapped_neutral.Rdata")

  







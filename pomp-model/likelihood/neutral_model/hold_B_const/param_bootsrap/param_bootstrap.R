# parameters of interest
# parameters of interest
est_these <- c("R0_A",  "amplitude_A",  "rho_A")

# natural constraints on the parameters 

list(lower = c(R0_A = 0.5, amplitude_A = 1e-10, rho_A = 1e-10), 
     upper = c(R0_A = 6, amplitude_A = 0.5, rho_A = 0.01)) -> param_bounds


# modify the default parameters to have few epi-parameters identical to the previous model 

these_params_to_fix <- c("tpeak_A", "tpeak_B", "R0_B", "amplitude_B", "rho_B")

params_all_2019_neut[these_params_to_fix] <- result_neutral$GAobj@solution[1,these_params_to_fix]


# collect the time vector 
inc_data_2 %>% 
  filter(flu_type == "total_a") %>% 
  select(time) %>% 
  unlist() %>% unname() -> time_vec

# using the mle simulate from the obseravtion process 
# set up the mle with other fixed parmeters 
neutral_A_mle <- params_all_2019_neut
neutral_A_mle[est_these] <- result_neutral_2019_3p_A$GAobj@solution[1,est_these]



# simulate 1000 realizations from the observation process
set.seed(12345)
sim_obs_model(po_obj = po_2019, 
              params = neutral_A_mle, times = time_vec, 
              nsim = 1000, method = "ode45", long_form = TRUE) %>% 
  filter(FluType == "total_a") %>% 
  transmute(time = time, 
            `.id` = `.id`,
            flu_type = FluType, 
            obs_cases = SimObs) -> mle_sim_data
  
  
est_on_sim_data <- function (counter, target_data = mle_sim_data) {
  # browser()
  mle_sim_data %>% 
    filter(`.id` == counter) %>% 
    select(-`.id`) -> tmp_target_data
  
  of_int <- function(par, est = est_these, 
                     target_data = tmp_target_data) {
    -of_2019(par = par, est = est, target_data = target_data,
             pomp_object = po_2019, params_all_int = params_all_2019_neut, fit = "A")
  }
  
  optim(par = result_neutral_2019_3p_A$GAobj@solution[1,est_these], 
        fn = of_int, method = "L-BFGS-B",
        upper = param_bounds$upper, lower = param_bounds$lower) -> result
  
  return(list(simulation = counter, result = result))
  
}

# est_on_sim_data(counter = 1)

bootstrapped_params_neutral_A_2019_list <- mclapply(1:1000, est_on_sim_data, mc.set.seed = TRUE, 
                                                    mc.cores = detectCores()-1)


save(bootstrapped_params_neutral_A_2019_list, file = "bootstrapped_neutral_A_2019.Rdata")

  







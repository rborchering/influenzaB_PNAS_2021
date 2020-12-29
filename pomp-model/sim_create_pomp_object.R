## See ./0load.R for data loads
## See ../src/libraries.R for library loads
## See ./0make.R for dependencies and build order

# Load relevant files and packages
set.seed(594709947L)

stopifnot(packageVersion("pomp")>="2.0.9.1")

# specify the kind of seed for parallelizingwhen using mclapply 
RNGkind("L'Ecuyer-CMRG")

# Two strain model model wrapper
# Note: dt presently passed in units of days, maybe fix this?
create_SIRS2_pomp_model <- function(df,
                                    time_start_sim, dt=1,
                                    use_stochastic = TRUE, 
                                    use_gaussian_rep = FALSE) {
  
  
  # Model parameter names
  model_params = c("R0_A","R0_B", "gamma_A", "gamma_B",
                   "w_A", "w_B","eta_A", "eta_B", "phi_A", "phi_B",
                   "chi_BA", "chi_AB", "rho_A", "rho_B", "sigmaSE",
                   "amplitude_A", "tpeak_A", "amplitude_B", "tpeak_B", "pop")

  # Model variable names
  model_variables = c("S", "I_A", "I_B", "R_A", "R_B", "C_A", "C_B", 
                      "I_BA", "I_AB", "R_AB")
  
  # Model initial conditions parameter names
  model_ic_params = paste(model_variables, "_0", sep="")
  
  
  
  #Todo: note the timestep chosen
  if(use_stochastic==TRUE){
    print("Using Euler-multinomial model")
    process_model <- euler(rproc_euler_multinomial, delta.t = dt/365.25)
  }
  else{
    print("Using Deterministic model")
    process_model <- euler(rproc_gauss, delta.t = dt/365.25)
  }
  
  
  if(use_gaussian_rep == TRUE) {
    print("Using Gaussian measurement model")
    dmeas <- dmeas_gauss
    rmeas <- rmeas_gauss
    
  } else {
    print("Using Poisson measurement model")
    dmeas <- dmeas_poisson
    rmeas <- rmeas_poisson
  }   
  
  
  po <- pomp(
    data = df,
    times = "time",
    t0 = time_start_sim,
    obsnames = c("total_a","total_b"),
    rprocess = process_model,
    skeleton = vectorfield(det_skel),
    dmeasure = dmeas,
    rmeasure = rmeas, 
    rinit = rinit_ee,
    accumvars =c ("K_A", "K_B", "W"),
    statenames = c(model_variables, c("K_A", "K_B", "W")),
    #cfile = "2strain_flu_model",
    #cdir = getwd(),
    paramnames = c(model_params, model_ic_params)
  )
  
  return(po)
}


##############################################################################################################
########################################### Preliminaries ####################################################
#################################### for the objective function ##############################################
##############################################################################################################
# For the three seasons: 2016-2018
# Define the date in a long format : To be used in the objective function
real_data_3s %>%
  gather(key = flu_type, value = obs_cases, -time) -> inc_data

# specify length of burn-in for simulations (in years)
t_start <- -100 

# create pomp model - 1 
real_data_3s %>% 
  create_SIRS2_pomp_model(time_start_sim=t_start) -> pomp_sirs


if(FALSE) {


# create pomp model - 2 
df_ma_2019 %>%
  bind_rows(data.frame(Year = 2019.769 - 1/52, 
                       total_a = NA, total_b = NA)) %>% 
  arrange(Year) %>%
  mutate(time = Year) %>% 
  select(-Year) %>% 
  create_SIRS2_pomp_model(time_start_sim=(2019.769 - 1/52)) -> pomp_sirs_2019
}


# set a default vector of parameters: requirement of the objective function ::: Can this be improved(?)
# NOTE: Keep the defaults to be at R0s of 1 and neutral model 
# NOTE: phi value shave been fixed to 365/30 for defaults, mostly for convenience
rp_vals <- c(R0_A = 1, gamma_A=365./2.5, w_A=1./4.,
             R0_B = 1, gamma_B=365./3.5, w_B=1./4.,
             phi_A=365/30., phi_B=365/30.,
             chi_BA=0, chi_AB=0, eta_A=365., eta_B=365.,
             rho_A=0, rho_B=0, 
             sigmaSE=0.0000, 
             amplitude_A=0, tpeak_A=0, amplitude_B=0, tpeak_B=0,
             pop=6.7e6)

ic_vals <- SIRS2_independent_endemic_equilibrium(rp_vals)
params_all <- c(rp_vals,ic_vals)



##############################################################################################################
################################ Objective function definitions ##############################################
##############################################################################################################
# objective function for 3 seasons
# This function calculates the posssion log-liklihood | condtional  log-liklihood 
# for a target-data:model pair 

of <- function(par, pomp_object = pomp_sirs, est, params_all_int = params_all, 
               give_conditional_log_lik = FALSE, target_data = inc_data, 
               fail_value = -1e9) {
  
  # replace values for the parameters of interest
  if(class(par) %in% c("numeric")) {
    params_all_int[est] <- par[est]
  } else{
      params_all_int[est] <- unlist(par[,est])  
  }
  # browser()
  # use globally defined pomp object and replace param vector
  pomp_object %>% 
    pomp(params  = params_all_int) %>% 
    # generate trajectories using the replaced parameters
    trajectory(include.data = FALSE, format = "d", method = "ode45") %>%
    # scale the "true incidence" by the reporting probabilities situated in params_all_int
    mutate(K_A = ifelse(time < 2.75, NA, K_A), 
           total_a = params_all_int["rho_A"]*K_A,
           K_B = ifelse(time < 2.75, NA, K_B), 
           total_b = params_all_int["rho_B"]*K_B) %>% 
    # select relevant state veariables: type specific new scaled cases of flu
    select(time, total_a, total_b) %>% 
    # store this in a long-format tibble 
    gather(key = flu_type, value = st_cases, -time) %>% 
    # join this with the target data: to calculate poisson log-likelihood
    right_join(target_data, by = c("time", "flu_type")) %>% 
    # calculate poisson log-density: P(data_t|model_t)
    mutate(poisson_log_density = dpois(x = obs_cases, lambda = st_cases, log = TRUE)) -> log_density_data 
  
  # This feature is for potential post-hoc analysis: if conditional log-liklihoods are called for
  if(give_conditional_log_lik == TRUE) {
    
    log_density_data %>% 
      # NA's are given a conditional log-density of 0
      replace_na(list(poisson_log_density = 0)) %>% 
      # Following two steps are used to generate type-specific conditional log-liklihood
      group_by(flu_type) %>% 
      mutate(conditional_loglikelihood = cumsum(poisson_log_density)) -> result
    
  } else {
    
    log_density_data %>% 
      # calculate the poisson log-likelihood : FOR THE OPTIMIZER
      select(poisson_log_density) %>% 
      summarise_all(sum, na.rm = TRUE) %>% 
      mutate(poisson_log_density = ifelse(is.finite(poisson_log_density) == TRUE & 
                                          poisson_log_density < 0, poisson_log_density, 
                                          fail_value)) %>% 
      unlist() -> result
  }
  
  print(result)
  print(par)
  # browser()
  return(result)  
  
}

#objective function for 2019
# This function calculates the posssion log-liklihood | condtional  log-liklihood 
# for a target-data:model pair 
of_2019 <- function(par, pomp_object, est, params_all_int, 
                    give_conditional_log_lik = FALSE, target_data = inc_data_2, 
                    fail_value = -1e9, fit = "both") {
  
  # replace values for the parameters of interest
  if(class(par) %in% c("numeric")) {
    params_all_int[est] <- par[est]
  } else{
    params_all_int[est] <- unlist(par[,est])  
  }
  # browser()
  # use globally defined pomp object and replace param vector
  
  
  if(fit == "B") {
    # browser()
    # select only type b data from the incidence data 
    target_data %>% 
      filter(flu_type == "total_b") -> target_data_b
    
    pomp_object %>% 
      pomp(params  = params_all_int) %>% 
      # generate trajectories using the replaced parameters
      trajectory(include.data = FALSE, format = "d", method = "ode45") %>%
      # scale the "true incidence" by the reporting probabilities situated in params_all_int
      mutate(K_B = ifelse(time < 2019.76, NA, K_B), 
             total_b = params_all_int["rho_B"]*K_B) %>% 
      # select relevant state veariables: type specific new scaled cases of flu
      select(time, total_b) %>% 
      # store this in a long-format tibble 
      gather(key = flu_type, value = st_cases, -time) %>% 
      # join this with the target data: to calculate poisson log-likelihood
      right_join(target_data_b, by = c("time", "flu_type")) %>% 
      # calculate poisson log-density: P(data_t|model_t)
      mutate(poisson_log_density = dpois(x = obs_cases, lambda = st_cases, log = TRUE)) -> log_density_data 
  
  
  } else if(fit == "A") {
    # browser()
    # select only type a data from the incidence data  
    target_data %>% 
      filter(flu_type == "total_a") -> target_data_a
    
    pomp_object %>% 
      pomp(params  = params_all_int) %>% 
      # generate trajectories using the replaced parameters
      trajectory(include.data = FALSE, format = "d", method = "ode45") %>%
      # scale the "true incidence" by the reporting probabilities situated in params_all_int
      mutate(K_A = ifelse(time < 2019.76, NA, K_A), 
             total_a = params_all_int["rho_A"]*K_A) %>% 
      # select relevant state veariables: type specific new scaled cases of flu
      select(time, total_a) %>% 
      # store this in a long-format tibble 
      gather(key = flu_type, value = st_cases, -time) %>% 
      # join this with the target data: to calculate poisson log-likelihood
      right_join(target_data_a, by = c("time", "flu_type")) %>% 
      # calculate poisson log-density: P(data_t|model_t)
      mutate(poisson_log_density = dpois(x = obs_cases, lambda = st_cases, log = TRUE)) -> log_density_data
    
  } else if(fit == "both"){
  
    pomp_object %>% 
      pomp(params  = params_all_int) %>% 
      # generate trajectories using the replaced parameters
      trajectory(include.data = FALSE, format = "d", method = "ode45") %>%
      # scale the "true incidence" by the reporting probabilities situated in params_all_int
      mutate(K_A = ifelse(time < 2019.76, NA, K_A), 
             total_a = params_all_int["rho_A"]*K_A,
             K_B = ifelse(time < 2019.76, NA, K_B), 
             total_b = params_all_int["rho_B"]*K_B) %>% 
      # select relevant state veariables: type specific new scaled cases of flu
      select(time, total_a, total_b) %>% 
      # store this in a long-format tibble 
      gather(key = flu_type, value = st_cases, -time) %>% 
      # join this with the target data: to calculate poisson log-likelihood
      right_join(target_data, by = c("time", "flu_type")) %>% 
      # calculate poisson log-density: P(data_t|model_t)
      mutate(poisson_log_density = dpois(x = obs_cases, lambda = st_cases, log = TRUE)) -> log_density_data 
  
  }
  
  # This feature is for potential post-hoc analysis: if conditional log-liklihoods are called for
  if(give_conditional_log_lik == TRUE) {
    
    log_density_data %>% 
      # NA's are given a conditional log-density of 0
      replace_na(list(poisson_log_density = 0)) %>% 
      # Following two steps are used to generate type-specific conditional log-liklihood
      group_by(flu_type) %>% 
      mutate(conditional_loglikelihood = cumsum(poisson_log_density)) %>% ungroup () -> result
    
  } else {
    # browser()
    log_density_data %>% 
      # calculate the poisson log-likelihood : FOR THE OPTIMIZER
      select(poisson_log_density) %>% 
      summarise_all(sum, na.rm = TRUE) %>% 
      mutate(poisson_log_density = ifelse(is.finite(poisson_log_density) == TRUE & poisson_log_density < 0, 
                                          poisson_log_density, fail_value)) %>% 
      unlist() -> result
  }
  
  
  # browser()
  return(result)  
  
}








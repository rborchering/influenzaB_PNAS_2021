set.seed(594709947L)

# treat the 2019 season data here, this step ready's it for the pomp object here 
df_ma_2019 %>%
  bind_rows(data.frame(Year = 2019.769 - 1/52, 
                       total_a = NA, total_b = NA)) %>% 
  mutate(time = Year) %>% 
  select(-Year) %>% 
  arrange(time) -> real_data_2019s 

# Define the data in a long format : To be used in the objective function
real_data_2019s %>% 
  gather(key = flu_type, value = obs_cases, -time) -> inc_data_2

# generate a fake pomp object for simulating ic for 2019 data - This is based on the parameterization from 
# previous three seasons 

data.frame(time = seq(2019.769-3, 2019.750, by = 1/52),
           total_a = NA, 
           total_b = NA) %>% 
  create_SIRS2_pomp_model(time_start_sim=(2019.769-103)) -> po_ic_2019


# use this 
real_data_2019s %>% 
  pomp(t0 =  real_data_2019s$time[1], 
       times = "time", 
       obsnames = c("total_a","total_b"), 
       skeleton = vectorfield(det_skel),
       rprocess =  euler(rproc_euler_multinomial, delta.t = 1/365.25), 
       dmeasure = dmeas_poisson, 
       rmeasure = rmeas_poisson, 
       rinit = rinit_2019, 
       accumvars =c ("K_A", "K_B", "W"),
       statenames = c(model_variables, c("K_A", "K_B", "W")), 
       paramnames = names(params_all)) -> po_2019

# function to calculate the time changing R0s through time 

cal_R0_t <- function(t, R0, amplitude, t_peak) {
  
  seas = 1 + amplitude*cos(2.*3.1415*(t-t_peak)/1.)
  
  R0*seas    
  
}

##############################################################################################################
########################### initial conditions for 2019 with neutral model ###################################
##############################################################################################################

two_era_traj <- function(R0_A_1, R0_A_2, r_1, r_2, Inc = FALSE) {
  #browser()
  # Assign the default parameter values to a new vector to avoid over writing
  # 2016-18 era
  sim_params_2016_18 <- params_all
  #sim_params_2016_18[c("phi_A", "phi_B")] <- 0
  # 2019 era
  sim_params_2019 <- params_all
  #sim_params_2019[c("phi_A", "phi_B")] <- 0
  
  # Following parameters will be fixed in this hypothesis 
  fp_2016_18 <- c("amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
                  "rho_A", "rho_B")
  
  fp_A_2019 <- c("amplitude_A", "rho_A")
  
  fp_B_2019 <- c("amplitude_B", "rho_B")
  
  # Replace the default parameter values with fixed and dynamic parameter estimates 
  sim_params_2016_18[fp_2016_18] <- result_neutral$GAobj@solution[,fp_2016_18]
  sim_params_2016_18[c("R0_A", "R0_B")] <- c(R0_A_1, R0_A_1/r_1) 
  sim_params_2016_18[c("amplitude_A", "amplitude_B")] <- 0.35
  
  # Simulate the initial conditions for neutral model in 2019 
  traj_2016_18 <- po_ic_2019 %>% 
    trajectory(params = sim_params_2016_18, 
               format = "d", method = "ode45", verbose = TRUE)
  
  # Lines 80-89: set up a vector of parameters for simulation of 2019 season
  # 1: Initial conditions are inferred from the 2016-18 seasons 
  sim_params_2019[names(ic_vals)] <- traj_2016_18 %>% 
    select(-c("K_A", "K_B", "W", "time", ".id")) %>% 
    tail(1) %>% 
    unlist() 
  
  # 2: Fixing amplitude and reporting to parameter estimates 
  sim_params_2019[c(fp_A_2019, fp_B_2019)] <- c(result_neutral_2019_3p_A$GAobj@solution[1,fp_A_2019], 
                                                result_neutral_2019_3p$GAobj@solution[1,fp_B_2019])  
  
  # 3: Fixing time of peak to the values of 2016-18 since these were not estimated.
  sim_params_2019[c("tpeak_A", "tpeak_B")] <- sim_params_2016_18[c("tpeak_A", "tpeak_B")]
  
  # 4: Changing the R0_A values as variables of this function 
  sim_params_2019[c("R0_A", "R0_B")] <- c(R0_A_2, R0_A_2/r_2) 
  sim_params_2019[c("amplitude_A", "amplitude_B")] <- 0.35
  sim_params_2019[c("tpeak_A", "tpeak_B")] <- 0.13
  
  # Simulate the 2019 season and remember to get rid of the first row (this is the last row from 2016-18 era) 
  traj_2019 <- po_2019 %>% 
    trajectory(params = sim_params_2019, method  = "ode45", verbose = TRUE, format = "d") %>% 
    slice(-1)
  
  
  # calculate the time varying R0s for the types
   
  traj_2016_18 %<>% 
    mutate(R0_A_t = cal_R0_t(t = time, 
                             R0 = sim_params_2016_18["R0_A"], 
                             amplitude = sim_params_2016_18["amplitude_A"],
                             t_peak = sim_params_2016_18["tpeak_A"]), 
           R0_B_t = cal_R0_t(t = time, 
                             R0 = sim_params_2016_18["R0_B"], 
                             amplitude = sim_params_2016_18["amplitude_B"],
                             t_peak = sim_params_2016_18["tpeak_B"])) 
  
  traj_2019 %<>% 
    mutate(R0_A_t = cal_R0_t(t = time, 
                             R0 = sim_params_2019["R0_A"], 
                             amplitude = sim_params_2019["amplitude_A"],
                             t_peak = sim_params_2019["tpeak_A"]), 
           R0_B_t = cal_R0_t(t = time, 
                             R0 = sim_params_2019["R0_B"], 
                             amplitude = sim_params_2019["amplitude_B"],
                             t_peak = sim_params_2019["tpeak_B"]))
  
  # put the two era's together
  
  traj_2016_19 <- traj_2016_18 %>% 
    bind_rows(traj_2019)
  
  inc_2016_2019 <- traj_2016_19 %>% 
    select(time, K_A, K_B)
    
  
  # Process the combined simulation and make ready for plots 
  
  susc_inf_2016_19 <- traj_2016_19  %>%  
    # Select variables to calculate type-specific susceptible and infectious sub-populations
    select(time, S, C_A, C_B, R_A, R_B, I_A, I_B, I_AB, I_BA, R0_A_t, R0_B_t) %>% 
    # Calculate type-specific susceptible and infectious sub-populations 
    transmute(time = time, 
              S_A = S + C_B + R_B, 
              S_B = S + C_A + R_A, 
              K_A = (I_A+I_AB), 
              K_B = (I_B+I_BA), 
              R0_A_t = R0_A_t, 
              R0_B_t = R0_B_t) %>% 
    filter(time > min(time))
    # Store result in a long format suitable for plotting 
          
    #gather(key = "Comp", value = "Count", -time) %>% 
    #mutate(FluType = ifelse(Comp %in% c("S_A", "K_A"), "A", "B"),
    #       Comp = rep(c("Susceptible", "Infectious"), each = 2*nrow(traj_2016_19))) %>% 
    #filter(time > min(.$time))

  if(Inc == TRUE) {
    return(inc_2016_2019)    
  } else{
    return(susc_inf_2016_19)
  }
  
  
}


# Preparing the plot for illustration 

p_2016_18 <- result_neutral$GAobj@solution[,c("R0_A", "R0_B")] 
p_2019 <- c(result_neutral_2019_3p_A$GAobj@solution[,"R0_A"], 
            result_neutral_2019_3p$GAobj@solution[,"R0_B"])



# simulate with both the estimates 
est_sim_both <- two_era_traj(R0_A_1 = unname(p_2016_18["R0_A"]), 
                             R0_A_2 = unname(p_2016_18["R0_A"]), 
                             r_1 = unname(p_2016_18["R0_A"]/p_2016_18["R0_B"]), 
                             r_2 = unname(p_2016_18["R0_A"]/p_2019["R0_B"]))


# Collect values for type A 
replacement_data <- est_sim_both %>% 
  select(time, S_A, K_A, R0_A_t) %>%
  slice(rep(1:n(), times = 3)) %>% 
  mutate(which = "A", 
         Susceptible = S_A, 
         Infectious = K_A, 
         R0_t = R0_A_t) %>% 
  select(-c(S_A, K_A, R0_A_t))

# modify the data set 
est_sim_both %<>% 
  select(-c(K_A, S_A, R0_A_t)) %>% 
  mutate(R0_t = R0_B_t, which = "est_both") %>% 
  select(-R0_B_t)

# simulate holding R0 value for B to the estimate of 2019
est_sim_2019 <- two_era_traj(R0_A_1 = unname(p_2016_18["R0_A"]), 
                             R0_A_2 = unname(p_2016_18["R0_A"]), 
                             r_1 = unname(p_2016_18["R0_A"]/p_2019["R0_B"]), 
                             r_2 = unname(p_2016_18["R0_A"]/p_2019["R0_B"]))

# take out A related variables 
est_sim_2019 %<>% 
  select(-c(K_A, S_A, R0_A_t)) %>% 
  mutate(R0_t = R0_B_t, which = "est_2019") %>% 
  select(-R0_B_t)


# simulate holding R0 value for B to the estimate of 2016-18
est_sim_2016_18 <- two_era_traj(R0_A_1 = unname(p_2016_18["R0_A"]), 
                                R0_A_2 = unname(p_2016_18["R0_A"]), 
                                r_1 = unname(p_2016_18["R0_A"]/p_2016_18["R0_B"]), 
                                r_2 = unname(p_2016_18["R0_A"]/p_2016_18["R0_B"]))


# take out A related variables 
est_sim_2016_18 %<>% 
  select(-c(K_A, S_A, R0_A_t)) %>% 
  mutate(R0_t = R0_B_t, which = "est_2016_18") %>% 
  select(-R0_B_t)



# bind the three together and right join with the replacement
plt_data <- est_sim_both %>% 
  bind_rows(est_sim_2019) %>% 
  bind_rows(est_sim_2016_18) %>% 
  mutate(Susceptible = S_B , 
         Infectious = K_B) %>% 
  select(-c(S_B, K_B)) %>% 
  bind_rows(replacement_data) %>% 
  mutate(Susceptible = Susceptible/6.7e6, 
         Infectious = Infectious/6.7e6, 
         Reff = R0_t*Susceptible)  
  
  
##############################################################################################################
############################# generating the susceptible backlog by changing the R0s of B - ##################
##############################################################################################################


# Use estimates for the first season

fp_2016_18_1 <- c("amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
                  "rho_A", "rho_B", "R0_A", "R0_B")

params_2016_18_1 <- params_all
params_2016_18_1[fp_2016_18_1] <- result_neutral$GAobj@solution[,fp_2016_18_1]
params_2016_18_1[c("amplitude_A", "amplitude_B")] <- 0.35 
params_2016_18_1[c("tpeak_A", "tpeak_B")] <- 0.13

# reconfigure the pomp object 
data.frame(time = seq(2018.769-2, 2018.769, by = 1/52),
           total_a = NA, 
           total_b = NA) %>% 
  create_SIRS2_pomp_model(time_start_sim=(2018.769-102)) -> po_ic_2016_17


traj_2016_17 <- po_ic_2016_17 %>% 
  trajectory(params = params_2016_18_1, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  mutate(R0_A_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_1["R0_A"], 
                           amplitude = params_2016_18_1["amplitude_A"],
                           t_peak = params_2016_18_1["tpeak_A"]), 
         R0_B_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_1["R0_B"], 
                           amplitude = params_2016_18_1["amplitude_B"],
                           t_peak = params_2016_18_1["tpeak_B"])
         )



# collect the initial conditions and re-define the param 


params_2016_18_2 <- params_2016_18_1

traj_2016_17 %>% 
  select(-c("K_A", "K_B", "W", "time", ".id")) %>% 
  tail(1) %>% 
  unlist() -> replace_these

names(replace_these) <- paste0(names(replace_these), "_0")


params_2016_18_2[names(ic_vals)] <- replace_these[names(ic_vals)]
  
  
# reset the R0 for B to a lower than 1 value
params_2016_18_2[c("R0_B", "amplitude_B", "eta_B")] <- c(0.5, 0.0, 0.0) 


# define a pomp object to integrate this part of the time-series
data.frame(time = seq(2019.769-1, 2019.750, by = 1/52),
           total_a = NA, 
           total_b = NA) %>% 
  pomp(t0 =  2018.750, 
       times = "time", 
       obsnames = c("total_a","total_b"), 
       skeleton = vectorfield(det_skel),
       rprocess =  euler(rproc_euler_multinomial, delta.t = 1/365.25), 
       dmeasure = dmeas_poisson, 
       rmeasure = rmeas_poisson, 
       rinit = rinit_2019, 
       accumvars =c ("K_A", "K_B", "W"),
       statenames = c(model_variables, c("K_A", "K_B", "W")), 
       paramnames = names(params_all)) -> po_2019_sim



traj_2017_18 <- po_2019_sim %>% 
  trajectory(params = params_2016_18_2, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  mutate(R0_A_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_2["R0_A"], 
                           amplitude = params_2016_18_2["amplitude_A"],
                           t_peak = params_2016_18_2["tpeak_A"]), 
         R0_B_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_2["R0_B"], 
                           amplitude = params_2016_18_2["amplitude_B"],
                           t_peak = params_2016_18_2["tpeak_B"])
  )
  


# collect the initial conditions and re-define the param 


params_2016_18_3 <- params_2016_18_1

traj_2017_18 %>% 
  select(-c("K_A", "K_B", "W", "time", ".id")) %>% 
  tail(1) %>% 
  unlist() -> replace_these_2

names(replace_these_2) <- paste0(names(replace_these_2), "_0")


params_2016_18_3[names(ic_vals)] <- replace_these_2[names(ic_vals)]


# reset the R0 for B to a lower than 1 value
#params_2016_18_2[c("R0_B", "amplitude_B", "eta_B")] <- c(0.5, 0.0) 


# define a pomp object to integrate this part of the time-series
data.frame(time = seq(2019.769, 2020.750, by = 1/52),
           total_a = NA, 
           total_b = NA) %>% 
  pomp(t0 =  2019.750, 
       times = "time", 
       obsnames = c("total_a","total_b"), 
       skeleton = vectorfield(det_skel),
       rprocess =  euler(rproc_euler_multinomial, delta.t = 1/365.25), 
       dmeasure = dmeas_poisson, 
       rmeasure = rmeas_poisson, 
       rinit = rinit_2019, 
       accumvars =c("K_A", "K_B", "W"),
       statenames = c(model_variables, c("K_A", "K_B", "W")), 
       paramnames = names(params_all)) -> po_2020_sim



traj_2018_19 <- po_2020_sim %>% 
  trajectory(params = params_2016_18_3, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  mutate(R0_A_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_3["R0_A"], 
                           amplitude = params_2016_18_3["amplitude_A"],
                           t_peak = params_2016_18_3["tpeak_A"]), 
         R0_B_t = cal_R0_t(t = time, 
                           R0 = params_2016_18_3["R0_B"], 
                           amplitude = params_2016_18_3["amplitude_B"],
                           t_peak = params_2016_18_3["tpeak_B"])
  )



traj_2016_17 %>% 
  bind_rows(traj_2017_18) %>% 
  bind_rows(traj_2018_19) -> traj_2016_19



# convert the data frame into long form to generate stattictics for the plot

plt_data_2 <- traj_2016_19 %>% 
  mutate(S_A = S + C_B + R_B, 
         S_B = S + C_A + R_A,
         In_A = (I_A + I_AB), 
         In_B = (I_B + I_BA)) %>% 
  select(time, starts_with("S_"), starts_with("In_"), starts_with("R0_")) %>% 
  gather(key = "Comp", value = "Count", -time) %>% 
  # define extra identifier variables used to define aesthetics in the plots
  mutate(FluType = factor(ifelse(Comp %in% c("S_A", "In_A", "R0_A_t"), "A", "B"), 
                          levels = c("A", "B")), 
         Comp = case_when(Comp %in% c("S_A", "S_B") ~ "Susceptible", 
                          Comp %in% c("In_A", "In_B") ~ "Infectious",
                          Comp %in% c("R0_A_t", "R0_B_t") ~ "R0_t")) %>% 
  # Manually remove the rows with duplicate identifiers
  slice(-c(105,314,523,732, 941, 1150)) %>% 
  # Spread on the Comp variable to get two columns to be represented on two axes in the plot
  spread(key = Comp, value = Count) %>% 
  # get rid of the first row
  filter(time > 2016.769) %>% 
  mutate(Infectious = Infectious/6.7e6, 
         Susceptible = Susceptible/6.7e6, 
         Reff = R0_t*Susceptible)
  

peak_info <- plt_data_2 %>% 
  filter(time > 2019.75) %>% 
  select(-Susceptible) %>% 
  spread(key = FluType, value = Infectious) %>% 
  mutate(pw_A = time[A == max(A)], 
         pw_B = time[B == max(B)], 
         pw_diff = pw_A - pw_B, 
         pm_A = max(A), 
         pm_B = max(B)) %>% 
  slice(n()) %>% 
  unlist()



if(FALSE) {

# 4 season fit

load("./../extra/data-pomp/result_neutral_4s.Rdata")
result_neutral_4s$GAobj@fitnessValue
result_neutral_4s$GAobj@solution

 
AIC_new <- 2*8 +2*4535.358

AIC_old <- 7269.21+1287.08

AIC_new - AIC_old



}
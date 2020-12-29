## See ./0load.R for data loads
## See ../src/libraries.R for library loads
## See ./0make.R for dependencies and build order
## see also ./sim_create_pomp_object.R"

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

data.frame(time = seq(2019.769-30/52, 2019.750, by = 1/52),
           total_a = NA, 
           total_b = NA) %>% 
  create_SIRS2_pomp_model(time_start_sim=(2019.769-103)) -> po_ic_2019



##############################################################################################################
####################### initial conditions for 2019 with cross-protection model ##############################
##############################################################################################################
# Assign the default paramter values to a new vector to avoid over writing
params_all_cp <- params_all

# default parameters for cp model in 2019, again, to avoid overwriting
params_all_2019_cp <- params_all

# Following parameters were estimated in this hypothesis 
estd_cp <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
              "rho_A", "rho_B", "chi_AB", "chi_BA")

# Replace the default parameter values with parameter estimates 
params_all_cp[estd_cp] <- result_fix_w_cp$GAobj@solution[1,estd_cp]

# Simulate the initial conditions for cp model in 2019 
po_ic_2019 %>% 
  trajectory(params = params_all_cp, format = "d", method = "ode45", verbose = TRUE) %>% 
  select(-c("K_A", "K_B", "W", "time", ".id")) %>% 
  tail(1) %>% 
  unlist() -> params_all_2019_cp[names(ic_vals)]

##############################################################################################################
########################### initial conditions for 2019 with neutral model ###################################
##############################################################################################################
# Assign the default paramter values to a new vector to avoid over writing
params_all_neut <- params_all

# default parameters for neutral model in 2019, again, to avoid overwriting
params_all_2019_neut <- params_all

# Following parameters were estimated in this hypothesis 
estd_neut <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
               "rho_A", "rho_B")

# Replace the default parameter values with parameter estimates 
params_all_neut[estd_neut] <- result_neutral$GAobj@solution[1,estd_neut]

# Simulate the initial conditions for neutral model in 2019 
po_ic_2019 %>% 
  trajectory(params = params_all_neut, format = "d", method = "ode45", verbose = TRUE) %>% 
  select(-c("K_A", "K_B", "W", "time", ".id")) %>% 
  tail(1) %>% 
  unlist() -> params_all_2019_neut[names(ic_vals)]

# generating the proportion of suscetipbles 

# For A replace, R0 for B by 0
params_all_neut_for_A <- params_all_neut

# Initial number of susceptibles 

po_ic_2019 %>% 
  trajectory(params = params_all_neut, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  select(c("S", "C_B", "R_B")) %>% 
  tail(1) %>% 
  unlist() %>% 
  sum()/6.7e+06 -> S_inf_A

# theoretical S_inf for an SIR model 

S_inf_A_th <- 1/2.590831e+00

po_ic_2019 %>% 
  trajectory(params = params_all_neut, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  select(c("S", "C_A", "R_A")) %>% 
  tail(1) %>% 
  unlist() %>% 
  sum()/6.7e+06 -> S_inf_B

# theoretical S_inf for an SIR model 

S_inf_B_th <- 1/1.562135


## flu specific infected number of individuals 
po_ic_2019 %>% 
  trajectory(params = params_all_neut, 
             format = "d", method = "ode45", verbose = TRUE) %>% 
  tail(1) %>% 
  mutate(I_A_all = (I_A+I_AB)/6.7e+06, 
         I_B_all = (I_B+I_BA)/6.7e+06, 
         S_A_all = (S + C_B + R_B)/6.7e+06, 
         S_B_all = (S + C_A + R_A)/6.7e+06, 
         dI_A = (I_A+I_AB)*(params_all_neut["gamma_A"])*(3.074*(S + C_B + R_B)-1), 
         dI_B = (I_B+I_BA)*(params_all_neut["gamma_B"])*(2.409*(S + C_A + R_A)-1)) %>% 
  select(I_A_all, I_B_all, S_A_all, S_B_all, dI_A, dI_B) ->  ic_2020
  
  
  

##############################################################################################################
############################# Prepare a pomp object for this analysis ########################################
##############################################################################################################
# Generate the pomp object with these values ready for cp_2019 model  
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








# inc_data_2 %>% 
#   ggplot(aes(x = time, y = obs_cases, colour = flu_type))+
#   geom_line()+
#   scale_colour_manual(values = c("red", "blue"))
 
# inc_data %>% 
#   ggplot(aes(x = time, y = obs_cases, colour = flu_type))+
#   geom_line()+
#   scale_colour_manual(values = c("red", "blue"))

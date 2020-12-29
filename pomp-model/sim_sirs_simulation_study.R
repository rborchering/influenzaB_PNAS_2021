set.seed(594709947L)
source("../src/libraries.R", chdir = TRUE)
source("../src/functions.R", chdir = TRUE)
# theme_set(theme_bw(base_size = 15))
stopifnot(packageVersion("pomp")>="2.0.9.1")

# Set up parallel
cores = detectCores(all.tests = FALSE, logical = TRUE)
print(cores)
# there is probably a way to pass this from the command line
registerDoParallel(cores = cores-8)
print(getDoParWorkers())
registerDoRNG(998468235L)

#Todo: Read data. Temporary hack because of different relative path definitions :S
if(FALSE){
# Read in model Csnippets
source("./sim_sirs_new_ps.R")
#source("./sim_model_sirs_two_type.R")

# Read in create pomp model wrapper
source("./sim_create_pomp_object.R")

# Load data and specify model parameters
source("./sim_data_and_params.R")


load("./data/pmla_fix_w.Rdata")

list_to_tibble(pmla_fix_w) %>% 
  mutate(loglik = -nll) %>% 
  arrange(desc(loglik)) %>% 
  slice(1) -> use_these_values
}
# specify length of burn in for simulations (in years)
t_start <- 0 

# Extract data columns needed for simulation
df_ma_fitting <- df_ma[,c("time","total_a", "total_b")]

# Create pomp object
df_ma_fitting %>% 
  create_SIRS2_pomp_model(time_start_sim=t_start) -> pomp_sirs

if(FALSE) {
df_ma_fitting %>%
  gather(key = "flu_type", value = "count", -time) %>% 
  ggplot(aes(x = time, y = count)) + 
  geom_line(aes(colour = flu_type), size = 0.8) +
  theme(aspect.ratio = 0.5)
}    

##############################################################################################################
############### Functions and plots for collecting statistics from time series ###############################
##############################################################################################################

# FUNCTION:: 


simulate_tss <- function(params, give_everything = FALSE, show_progress = FALSE,...) {
  # browser()
  if(show_progress == TRUE) {
    pb$tick()$print()  
  } else {
    print("Progress of the job will not be displayed!")
  }
  
  # browser()
  guess1_params <- c(R0_A=unname(params[,"R0"]), gamma_A=365./3.4, w_A=1./4.,
                     R0_B=unname(params[,"rel"])*unname(params[,"R0"]), 
                     gamma_B=365./2.5, w_B=1./4.,
                     phi_A=unname(params[,"phi"]), phi_B=unname(params[,"phi"]),
                     chi_BA=unname(params[,"chi"]), chi_AB=unname(params[,"chi"]), 
                     eta_A=365., eta_B=365., rho_A = 0.015, rho_B = 0.007, 
                     sigmaSE=0.0001, psi=0.00001, 
                     amplitude_A=0.3354482, amplitude_B=0.4348537, 
                     tpeak_A=0.1118114, tpeak_B = 0.1374526,
                     pop=6.7e6)
  
  guess1_params <- unlist(guess1_params) 
  
  guess1_ic <- SIRS2_independent_endemic_equilibrium(guess1_params)
  guess1_all <- c(guess1_params,guess1_ic)
  
  # browser()
  
  pomp_sirs %>%
    trajectory(params=guess1_all, t0=-750, format="d", method = "ode45") %>% 
    slice(2:n()) %>% 
    mutate(mp_A = max(K_A), 
           mp_B = max(K_B), 
           mp_AB_ratio = mp_A/mp_B, 
           pw_A = time[which(K_A == max(K_A))], 
           pw_B = time[which(K_B == max(K_B))], 
           p_AB_diff = ((pw_A-floor(pw_A)) - (pw_B-floor(pw_B)))*52, 
           pop = guess1_all["pop"], 
           I = I_A + I_B + I_AB + I_BA, 
           I_A_tot = I_A + I_AB, 
           I_B_tot = I_B + I_BA,   
           C = C_A + C_B, 
           R = R_A + R_B + R_AB, 
           I_A_prop = (I_A + I_AB)/I, 
           I_B_prop = (I_B + I_BA)/I, 
           C_A_prop = C_A/C, 
           C_B_prop = C_B/C) -> everything 
    
    # browser()
  
    everything %>% 
      slice(n()) %>% 
      select(mp_AB_ratio, p_AB_diff) -> test_sim
  
  if(give_everything == TRUE) {
    print("All the Compartments are produced")
    return(everything)
  } else {
    return(test_sim)  
  }
  
} 



#function to loop over values 
multi_simulate_tss <- function(counter, params_mat, ...) {
  #browser()
  simulate_tss(params = params_mat[counter,], ...)
}

# 1 month cp
params_design_30 <- expand.grid(chi = seq(1e-10, 1, length = 500), 
                                rel = seq(0.5, 2, length = 500), 
                                R0 = 2, 
                                phi = 365/30)
                              

# multi_simulate_tss(counter = 12, .f = multi_simulate_tss, params_mat = params_design_30,
#                    show_progress = FALSE)

#Print a progress bar for the functional
# pb <- progress_estimated(nrow(params_design_30))
# 
# res_df <- map_df(.x = 1:nrow(params_design_30), .f = multi_simulate_tss,
#                 params_mat = params_design_30)

tic()
plan(multisession)
res_df <- future_map_dfr(.x = 1:nrow(params_design_30), .f = multi_simulate_tss,
                           params_mat = params_design_30, .progress = TRUE)
toc()


save(res_df, file = "./data/res_df_750.Rdata")

# combine output for plotting
# Load the results from simulation run
# load("./res_df.Rdata")
# 
# params_design_30 %<>%
#   bind_cols(res_df)

##############################################################################################################
############### Functions and plots for collecting statistics from time series ###############################
################################## Supplementary figure ######################################################
# NOTE :: Changing the duration of natural immunity for flu B to 8 10 years
# FUNCTION:: 


simulate_tss_2 <- function(params, give_everything = FALSE, show_progress = FALSE,...) {
  # browser()
  if(show_progress == TRUE) {
    pb$tick()$print()  
  } else {
    print("Progress of the job will not be displayed!")
  }
  
  guess1_params <- c(R0_A=unname(params[,"R0"]), gamma_A=365./3.4, w_A=1./4.,
                     R0_B=unname(params[,"rel"])*unname(params[,"R0"]), gamma_B=365./2.5, w_B=1./10.,
                     phi_A=unname(params[,"phi"]), phi_B=unname(params[,"phi"]),
                     chi_BA=unname(params[,"chi"]), chi_AB=unname(params[,"chi"]), 
                     eta_A=365., eta_B=365., rho_A = 0.015, rho_B = 0.007, 
                     sigmaSE=0.0001, psi=0.00001, 
                     amplitude_A=0.3354482, amplitude_B=0.4348537, 
                     tpeak_A=0.1118114, tpeak_B = 0.1374526,
                     pop=6.7e6)
  
  guess1_params <- unlist(guess1_params) 
  
  guess1_ic <- SIRS2_independent_endemic_equilibrium(guess1_params)
  guess1_all <- c(guess1_params,guess1_ic)
  
  # browser()
  
  pomp_sirs %>%
    trajectory(params=guess1_all, t0=-750, format="d", method = "ode45") %>% 
    slice(2:n()) %>% 
    mutate(mp_A = max(K_A), 
           mp_B = max(K_B), 
           mp_AB_ratio = mp_A/mp_B, 
           pw_A = time[which(K_A == max(K_A))], 
           pw_B = time[which(K_B == max(K_B))], 
           p_AB_diff = ((pw_A-floor(pw_A)) - (pw_B-floor(pw_B)))*52, 
           pop = guess1_all["pop"], 
           I = I_A + I_B + I_AB + I_BA, 
           I_A_tot = I_A + I_AB, 
           I_B_tot = I_B + I_BA,   
           C = C_A + C_B, 
           R = R_A + R_B + R_AB, 
           I_A_prop = (I_A + I_AB)/I, 
           I_B_prop = (I_B + I_BA)/I, 
           C_A_prop = C_A/C, 
           C_B_prop = C_B/C) -> everything 
  
  everything %>% 
    slice(n()) %>% 
    select(mp_AB_ratio, p_AB_diff) -> test_sim
  
  if(give_everything == TRUE) {
    print("All the Compartments are produced")
    return(everything)
  } else {
    return(test_sim)  
  }
  
} 



#function to loop over values 
multi_simulate_tss_2 <- function(counter, params_mat, ...) {
  simulate_tss_2(params = params_mat[counter,], ...)
}

# 1 month cp
params_design_30 <- expand.grid(chi = seq(1e-10, 1, length = 500), 
                                rel = seq(0.5, 2, length = 500), 
                                R0 = 2, 
                                phi = 365/30)


#multi_simulate_tss(1, params_mat = params_design_30)

#Print a progress bar for the functional
# pb <- progress_estimated(nrow(params_design_30))
# 
# res_df_2 <- map_df(.x = 1:nrow(params_design_30), .f = multi_simulate_tss_2,
#                    params_mat = params_design_30)


tic()
plan(multisession)
res_df_2 <- future_map_dfr(.x = 1:nrow(params_design_30), .f = multi_simulate_tss_2,
                           params_mat = params_design_30, .progress = TRUE)
toc()


# these results are from the 
save(res_df_2, file = "./data/res_df_2_750.Rdata")

# combine output for plotting
# Load the results from simulation run
# load("./res_df.Rdata")
# 
# params_design_30 %<>%
#   bind_cols(res_df)

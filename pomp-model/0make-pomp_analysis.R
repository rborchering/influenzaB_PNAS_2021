#Load all the functions and packages 
source("../src/functions.R", chdir = TRUE)

# generate the data objects that are going to be used in all the trajectory matching problem
df_ma <- read_csv(file = "../00-RawData/Massachusetts2seas/flu_inc_2016_18.csv") %>% 
  mutate(time = Year - 2014) %>% 
  select(time, starts_with("total_"))

real_data_3s <- df_ma

df_ma_2019 <- read_csv(file = "../00-RawData/Massachusetts2seas/flu_inc_2019.csv")

# generate the 
# read in model Csnippets
source("sim_sirs_new_ps.R")

# read in sobol sampling function
source("sim_sobol_sampling.R")

# specify the vector of model paramters model parameters - 
source("sim_data_and_params.R")
source("./sim_create_pomp_object.R", chdir = TRUE)

# Era: 2016-18 flu season
# for the neutral model 
# point estimates 
source("./likelihood/neutral_model/global_search_neutral.R", chdir = TRUE)
# confidence intervals 
source("./likelihood/neutral_model/param_bootsrap/param_bootstrap.R", chdir = TRUE)

# for the cross-protection model 
# point estimates
source("./likelihood/fix_w/global_search_fix_w_cp.R", chdir = TRUE)
source("./likelihood/fix_w/global_search_fix_w_phi_tiny.R", chdir = TRUE)
# confidence intervals 
source("./likelihood/fix_w/param_bootsrap/param_bootstrap.R", chdir = TRUE)

# Create the script that uses the estimates from the neutral model fit to 2016-18 and infers 
# the initial conditons for 2019
source("./sim_create_pomp_object_2019.R", chdir = TRUE)

# Era: 2019 flu season - Only the neutral model was fit from here onwards
# point estimates
source("./likelihood/neutral_model/hold_A_const/global_search_neutral_2019_3p.R", chdir = TRUE)
source("./likelihood/neutral_model/hold_B_const/global_search_neutral_2019_3p.R", chdir = TRUE)

# confidence intervals 
source("./likelihood/neutral_model/hold_A_const/param_bootsrap/param_bootstrap.R", chdir = TRUE)
source("./likelihood/neutral_model/hold_B_const/param_bootsrap/param_bootstrap.R", chdir = TRUE)


# Reviewer's response: Parameter fits to all four seasons using the neutral model 
source("./likelihood/neutral_model/global_search_neutral_4s.R", chdir = TRUE)

# simulation study: for figure 4
source("./sim_sirs_simulation_study.R", chdir = TRUE)

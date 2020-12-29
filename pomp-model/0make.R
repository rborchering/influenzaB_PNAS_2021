# source('../src/functions.R', chdir=T)
source("0load.R")
# read in model Csnippets
source("sim_sirs_new_ps.R")

# read in sobol sampling function
source("sim_sobol_sampling.R")

# specify the vector of model paramters model parameters - 
source("sim_data_and_params.R")

# finally, create pomp model wrapper
source("sim_create_pomp_object.R")
source("sim_create_pomp_object_2019.R")

# run part of the simulation study run the simulation
source("sim_sirs_simulation_study_2.R")

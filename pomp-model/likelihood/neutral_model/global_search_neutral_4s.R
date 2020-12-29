# read in create pomp model wrapper
source("../../sim_create_pomp_object.R", chdir = TRUE)

#loading best results from previous model runs

# reformatting the 2019 season to incorporate in the three seasons
# treat the 2019 season data here, this step ready's it for the pomp object here 
df_ma_2019 %>%
  mutate(time = Year-2014-13/52) %>% 
  select(-Year) %>% 
  bind_rows(real_data_3s) %>% 
  arrange(time) -> real_data_4s

if(FALSE) {
real_data_4s %>% 
  gather(key = "type", value = "count", -time) %>% 
  ggplot(aes(x = time, y = count, colour = type)) +
  geom_line(size = 0.8) +
  theme(aspect.ratio = 0.5)
}

# loading from the 
load("../../../extra/data-pomp/result_neutral.Rdata")


# create the pomp object
# create pomp model - 1 
real_data_4s %>% 
  create_SIRS2_pomp_model(time_start_sim=t_start) -> pomp_sirs_4s



# setting the duration of cross protection to zero, implies phi ~ Inf 
# This is set to have an infinitesimal duration of cross protection
neutral_params_all <- params_all


# parameters of interest
est_these <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
               "rho_A", "rho_B")

# create a profile design 
pd <- sobolDesign(lower = c(R0_B = 0.5, R0_A = 0.5, amplitude_A = 1e-10, 
                            amplitude_B = 1e-10, tpeak_A = 1/365.25, tpeak_B = 1/365.25,  
                            rho_A = 1e-10, rho_B = 1e-10), 
                  upper = c(R0_B = 25, R0_A = 25, 
                            amplitude_A = 1, amplitude_B = 1, 
                            tpeak_A = 1, tpeak_B = 1, 
                            rho_A = 0.01, rho_B = 0.01), nseq = 499)

# combine the dataframe of initial guesses with the mle from previous analysis
data.frame(result_neutral$GAobj@solution) %>%
  full_join(pd) -> pd_with_fix_w_neutral_mle


# This function is defined to provide pomp-GA integration
of_ga <- function(x, est = est_these) {
  
  x<- unname(unlist(x))
  # browser()
  of(par = c(R0_B = x[1], R0_A = x[2], 
             amplitude_A = x[3], amplitude_B = x[4], 
             tpeak_A = x[5], tpeak_B = x[6],  
             rho_A = x[7], rho_B = x[8]), est = est, 
     params_all_int = neutral_params_all, 
     pomp_object = pomp_sirs_4s)
}




if(FALSE) {    
  
  # testing the objective functions - WORKS!! 
  of_ga(x = pd_with_fix_w_neutral_mle[1,])   
  
  
  # testing the GA objective functions - WORKS!! 
  tic()
  of_ga(x = pd_with_fix_w_neutral_mle[3,])
  toc()
}



# genetic algorithm set up to run in parallel
tic()
no_cores <- detectCores() - 1 

registerDoParallel(cores=no_cores)  

cl <- makeCluster(no_cores, type="FORK")

GA_neutral <- ga(type = "real-valued", 
                 fitness = of_ga,
                 lower = c(R0_B = 0.5, R0_A = 0.5, amplitude_A = 1e-10, 
                           amplitude_B = 1e-10, tpeak_A = 1/365.25, tpeak_B = 1/365.25,  
                           rho_A = 1e-10, rho_B = 1e-10), 
                 upper = c(R0_B = 25, R0_A = 25, 
                           amplitude_A = 1, amplitude_B = 1, 
                           tpeak_A = 1, tpeak_B = 1, 
                           rho_A = 0.01, rho_B = 0.01),
                 elitism = base::max(1, round(100*.1)), 
                 popSize = 500, maxiter = 10000, run = 500, 
                 suggestions = pd_with_fix_w_neutral_mle,
                 optim = FALSE,
                 optimArgs = list(poptim = 0.1, 
                                  control = list(maxit = 10000)),
                 seed = 12345, parallel = cl)

stopCluster(cl)
toc() -> ga_took


result_neutral_4s <- list(it_took  = c(paste((ga_took$toc - ga_took$tic)/3600, "hrs"), 
                                       paste((ga_took$toc - ga_took$tic)/60, "mins"), 
                                       paste((ga_took$toc - ga_took$tic), "secs")), 
                          GAobj = GA_neutral)
toc()



save(result_neutral_4s, file = "result_neutral_4s.Rdata")
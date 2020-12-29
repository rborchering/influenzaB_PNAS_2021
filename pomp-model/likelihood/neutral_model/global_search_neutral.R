# setting the duration of crossprotection to zero, implies phi ~ Inf 
# This is set to have an infinitesimal duration of crossprotection
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
                            rho_A = 0.01, rho_B = 0.01), nseq = 500)

# combine the dataframe of initial guesses with the mle from previous analysis
pd -> pd_with_fix_w_neutral_mle


# This function is defined to provide pomp-GA integration
of_ga <- function(x, est = est_these) {
  
  x<- unname(unlist(x))
  # browser()
  of(par = c(R0_B = x[1], R0_A = x[2], 
             amplitude_A = x[3], amplitude_B = x[4], 
             tpeak_A = x[5], tpeak_B = x[6],  
             rho_A = x[7], rho_B = x[8]), est = est, 
     params_all_int = neutral_params_all)
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
                 optim = TRUE,
                 optimArgs = list(poptim = 0.1, 
                                  control = list(maxit = 10000)),
                 seed = 12345, parallel = cl)

stopCluster(cl)
toc() -> ga_took


result_neutral <- list(it_took  = c(paste((ga_took$toc - ga_took$tic)/3600, "hrs"), 
                                             paste((ga_took$toc - ga_took$tic)/60, "mins"), 
                                             paste((ga_took$toc - ga_took$tic), "secs")), 
                                GAobj = GA_neutral)
toc()



# save(result_neutral, file = "result_neutral.Rdata")
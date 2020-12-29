# read parameter estimates for convenient reporducibility
load("../../../extra/data-pomp/result_neutral.Rdata")

# parameters of interest
est_these <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
               "rho_A", "rho_B", "chi_AB", "chi_BA")

# create a sampled design of initial guesses: Sobol sampling
pd <- sobolDesign(lower = c(R0_B = 0.5, R0_A = 0.5, amplitude_A = 1e-10, 
                            amplitude_B = 1e-10, tpeak_A = 1/365.25, tpeak_B = 1/365.25,  
                            rho_A = 1e-10, rho_B = 1e-10, chi_AB = 0, chi_BA = 0), 
                  upper = c(R0_B = 25, R0_A = 25, amplitude_A = 1, amplitude_B = 1, 
                            tpeak_A = 1, tpeak_B = 1, rho_A = 0.01, rho_B = 0.01, chi_AB = 1, chi_BA = 1), 
                  nseq = 499)

# combine the dataframe of initial guesses with the mle of the neutral model 
data.frame(result_neutral$GAobj@solution) %>%
  full_join(pd) %>% 
  replace_na(replace = list(chi_AB = 0, chi_BA = 0)) -> pd_with_fix_w_neutral_mle




# This function is defined to provide pomp-GA integration
of_ga <- function(x, est = est_these) {
  
  x<- unname(unlist(x))
  
  of(par = c(R0_B = x[1], R0_A = x[2], 
             amplitude_A = x[3], amplitude_B = x[4], 
             tpeak_A = x[5], tpeak_B = x[6],  
             rho_A = x[7], rho_B = x[8], 
             chi_AB = x[9], chi_BA = x[10]), est = est)
}



# genetic algorithm set up to run in parallel
tic()
no_cores <- detectCores() - 1  

registerDoParallel(cores=no_cores)  

cl <- makeCluster(no_cores, type="FORK")

GA_fix_w <- ga(type = "real-valued", 
               fitness = of_ga,
               lower = c(R0_B = 0.5, R0_A = 0.5, amplitude_A = 1e-10, 
                         amplitude_B = 1e-10, tpeak_A = 1/365.25, tpeak_B = 1/365.25,  
                         rho_A = 1e-10, rho_B = 1e-10, chi_AB = 0, chi_BA = 0), 
               upper = c(R0_B = 25, R0_A = 25, 
                         amplitude_A = 1, amplitude_B = 1, 
                         tpeak_A = 1, tpeak_B = 1, 
                         rho_A = 0.01, rho_B = 0.01, chi_AB = 1, chi_BA = 1),
               elitism = base::max(1, round(100*.1)), 
               popSize = 500, maxiter = 10000, run = 500, 
               suggestions = pd_with_fix_w_neutral_mle,
               optim = TRUE,
               optimArgs = list(poptim = 0.1, 
                                control = list(maxit = 1000)),
               seed = 12345, parallel = cl)

stopCluster(cl)
toc() -> ga_took


result_fix_w_cp <- list(it_took  = c(paste((ga_took$toc - ga_took$tic)/3600, "hrs"), 
                                    paste((ga_took$toc - ga_took$tic)/60, "mins"), 
                                    paste((ga_took$toc - ga_took$tic), "secs")), 
                        GAobj = GA_fix_w)



# save(result_fix_w_cp, file = "result_fix_w_cp.Rdata")






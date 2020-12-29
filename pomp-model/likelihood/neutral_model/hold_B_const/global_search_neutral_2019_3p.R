# For Convenience load this file 
# load("../../../data/result_neutral.Rdata")


# parameters of interest
est_these <- c("R0_A",  "amplitude_A",  "rho_A")


set.seed(12345)
# create a profile design 
pd <- runifDesign(lower = c(R0_A = 0.5, amplitude_A = 1e-10, rho_A = 1e-10), 
                  upper = c(R0_A = 6, amplitude_A = 0.5, rho_A = 0.01), nseq = 1000)


# modify the default parameters to have few epi-parameters identical to the previous model 

these_params_to_fix <- c("tpeak_A", "tpeak_B", "R0_B", "amplitude_B", "rho_B")

params_all_2019_neut[these_params_to_fix] <- result_neutral$GAobj@solution[1,these_params_to_fix]


# This function is defined to provide pomp-GA integration
of_ga <- function(x) {
  
  x <- unname(unlist(x))
  # browser()
  of_2019(par = c(R0_A = x[1], amplitude_A = x[2], rho_A = x[3]),
          est = est_these, pomp_object = po_2019, params_all_int = params_all_2019_neut, 
          target_data = inc_data_2, fit = "A")
}




if(FALSE) {    
# 
#   # testing the objective functions - WORKS!! 
#   of_ga(x = pd_with_fix_w_neutral_mle[2,])   
#   
  
  # testing the GA objective functions - WORKS!! 
  
  
}



# genetic algorithm set up to run in parallel
tic()
no_cores <- detectCores() 

registerDoParallel(cores=no_cores)  

cl <- makeCluster(no_cores, type="FORK")

GA_neutral <- ga(type = "real-valued", 
                 fitness = of_ga,
                 lower = c(R0_A = 0.5, amplitude_A = 1e-10, rho_A = 1e-10), 
                 upper = c(R0_A = 6, amplitude_A = 0.5, rho_A = 0.01), 
                 popSize = 1000, maxiter = 5, run = 500, 
                 suggestions = pd,
                 optim = TRUE, keepBest = TRUE,
                 optimArgs = list(poptim = 0.1, 
                                  control = list(maxit = 10000)),
                 seed = 12345, parallel = cl)

stopCluster(cl)
toc() -> ga_took


result_neutral_2019_3p_A <- list(it_took  = c(paste((ga_took$toc - ga_took$tic)/3600, "hrs"), 
                                              paste((ga_took$toc - ga_took$tic)/60, "mins"), 
                                              paste((ga_took$toc - ga_took$tic), "secs")), 
                                 GAobj = GA_neutral)
toc()



save(result_neutral_2019_3p_A, file = "result_neutral_2019_3p_A.Rdata")


# inc_data_2 %>% 
#   filter(flu_type == "total_a") %>% 
#   filter(time < 2020.22) %>% 
#   ggplot(aes(x = time, y = obs_cases)) +
#   geom_line(size = 0.8)



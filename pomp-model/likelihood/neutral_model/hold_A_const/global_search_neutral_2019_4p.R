source("../../../sim_create_pomp_object_2019.R", chdir = TRUE)

# loading good estimates based on previous optimizations 
load("../../../data/result_neutral.Rdata")



# parameters of interest
est_these <- c("R0_B", "amplitude_B", "tpeak_B", "rho_B")

set.seed(12345)
# create a profile design 
pd <- runifDesign(lower = c(R0_B = 0.5, amplitude_B = 1e-10, tpeak_B = 1e-10, rho_B = 1e-10), 
                  upper = c(R0_B = 6,  amplitude_B = 0.5, tpeak_B = 0.5, rho_B = 0.01), nseq = 1000)


# modify the default parameters to have few epi-parameters identical to the previous model 

these_params_to_fix <- c("tpeak_A", "R0_A", "amplitude_A", "rho_A")

params_all_2019_neut[these_params_to_fix] <- result_neutral$GAobj@solution[1,these_params_to_fix]


# This function is defined to provide pomp-GA integration
of_ga <- function(x) {
  
  x <- unname(unlist(x))
  # browser()
  of_2019(par = c(R0_B = x[1], amplitude_B = x[2], tpeak_B = x[3], rho_B = x[4]),
          est = est_these, pomp_object = po_2019, params_all_int = params_all_2019_neut, 
          target_data = inc_data_2, est = "B")
}


if(FALSE) { 
  df_neut_2019 %>% 
    slice(rep(n(), 5000)) %>% 
    mutate(rho_B = seq(0.0005, 0.0025, length = 5000)) -> pseudo_likelihood_mat_neut

   
# 
#   # testing the objective functions - WORKS!! 
#   of_ga(x = pd_with_fix_w_neutral_mle[2,])   
#   
  
# # testing the GA objective functions - WORKS!! 
tic()
loglik_values  <-  unlist(mclapply(1:5000, function(counter) {of_ga(x = pseudo_likelihood_mat_neut[counter,])}, 
                          mc.cores = 8))
toc()


pseudo_likelihood_mat_neut %>% 
  mutate(logLik = loglik_values) %>% 
  ggplot(aes(x = rho_B, y = logLik)) +
  geom_line(size = 0.8) +
  labs(x = expression(rho[B])) +
  gg.theme


}
# 
# pd_with_fix_w_neutral_mle %>% 
#   mutate(logLik = all_values) %>% arrange(logLik) -> init_logLik_population


# genetic algorithm set up to run in parallel
tic()
no_cores <- detectCores() 

registerDoParallel(cores=no_cores)  

cl <- makeCluster(no_cores, type="FORK")

GA_neutral <- ga(type = "real-valued", 
                 fitness = of_ga,
                 lower = c(R0_B = 0.5, amplitude_B = 1e-10, tpeak_B = 0.0, rho_B = 1e-10), 
                 upper = c(R0_B = 6,  amplitude_B = 0.5, tpeak_B = 0.5, rho_B = 0.01),
                 elitism = base::max(1, round(100*.1)), 
                 popSize = 1000, maxiter = 10000, run = 500, 
                 suggestions = pd,  keepBest = TRUE,
                 optim = TRUE,
                 optimArgs = list(poptim = 0.1, 
                                  control = list(maxit = 10000)),
                 seed = 12345, parallel = cl)

stopCluster(cl)
toc() -> ga_took


result_neutral_2019_4p <- list(it_took  = c(paste((ga_took$toc - ga_took$tic)/3600, "hrs"), 
                                         paste((ga_took$toc - ga_took$tic)/60, "mins"), 
                                         paste((ga_took$toc - ga_took$tic), "secs")), 
                             GAobj = GA_neutral)
toc()

save(result_neutral_2019_4p, file = "result_neutral_2019_4p.Rdata")











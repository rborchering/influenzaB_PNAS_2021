## FITS of B

# loading all dependencies # 
source("../../../../src/install_dependencies.R", chdir = TRUE)
source("../../../../src/libraries.R", chdir = TRUE)
source("../../../../src/functions.R", chdir = TRUE)
source("../../../../plotting/setup.R", chdir = TRUE)


# Read in model Csnippets
source("../../../sim_sirs_new_ps.R", chdir = TRUE)

# Read in create pomp model wrapper
source("../../../sim_create_pomp_object_2019.R", chdir = TRUE)
# Load data and specify model parameters
source("../../../sim_data_and_params.R", chdir = TRUE)

# load 2016-18 estimates of neutral model
load("../../../data/result_neutral.Rdata")


# loading the file requisite 
# data 
load("../../../data/real_data_2019_seas.Rdata")

# estimate data 
# first round of GA - type vanilla - 500 run, maxit 10000
# initialized at the MLE of the neutral model
# first round of GA - type vanilla - 500 run, maxit 10000 


load("result_neutral_2019_3p.Rdata")
load("result_neutral_2019_4p.Rdata")






# collect values from the converged genetic algorithm as global esimates 
# Neutral model 2p
as_tibble(t(c(result_neutral_2019_3p$GAobj@solution[,1:3]))) %>%
  mutate(logLik = result_neutral_2019_3p$GAobj@fitnessValue, 
         AIC = calculate_aic(logLik, npar = 3), 
         model = "3p") -> neutral_estms_3p

# Neutral model 4p - rho
as_tibble(t(c(result_neutral_2019_4p$GAobj@solution[,1:4]))) %>%
  mutate(logLik = result_neutral_2019_4p$GAobj@fitnessValue, 
         AIC = calculate_aic(logLik, npar = 4), 
         model = "4p") -> neutral_estms_4p


column_order <- c("R0_B", "amplitude_B", "tpeak_B", "rho_B", "model","logLik", "AIC")


neutral_estms_3p %>% 
  full_join(neutral_estms_4p) %>% 
  select(column_order) %>% 
  mutate(delta_AIC = (AIC - min(AIC))) %>% 
  arrange(delta_AIC) -> neutral_all_estms 


# formulate a table for this 

neutral_all_estms %>% select(model) %>% unlist() -> model_names
  
neutral_all_estms %>% select(-model) -> only_ests

rownames(only_ests) <- model_names

print(xtable(t(only_ests), display = rep("g",3)))


# plot the values of the with respect to data 

# fit plots and conditional logliklihood

these_params_to_fix <- c("tpeak_A", "tpeak_B", "R0_A", "amplitude_A", "rho_A")

params_all_2019_neut_B <- params_all_2019_neut

params_all_2019_neut_B[these_params_to_fix] <- result_neutral$GAobj@solution[1,these_params_to_fix]


# parameters of interest
est_these <- c("R0_B",  "amplitude_B",  "rho_B")

of_2019(par = result_neutral_2019_3p$GAobj@solution[1,], est = est_these, 
        pomp_object = po_2019, params_all_int = params_all_2019_neut_B, target_data = inc_data_2, 
        only_est_B = TRUE, give_conditional_log_lik = TRUE) -> fit_data




# fit plots
fit_data %>% 
  mutate(cases_lb = qpois(0.025, st_cases), 
         cases_ub = qpois(0.975, st_cases)) %>% 
  ggplot(aes(x = time, y = st_cases)) +
  geom_line(colour = "blue", size = 0.8, linetype = "12345678") +
  geom_ribbon(aes(ymin = cases_lb, ymax = cases_ub), 
              alpha = 0.2, fill = "blue") +
  geom_line(aes(y=obs_cases), size = 0.8, colour = "blue") +
  labs(y = "Cases", 
       x = "Year") +
  gg.theme +
  theme(aspect.ratio = 0.5) +
  guides(linetype = guide_legend(ncol = 1, order = 1), 
         colour = guide_legend(ncol = 1)) +
  theme(strip.text = element_blank()) -> fit_data_plt

# option2

  
  















 








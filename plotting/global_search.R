## see pomp-model/0make.R for dependencies
## and pomp-model/0load.R for data objects
##############################################################################################################

# estimate data 
# first round of GA - type vanilla - 500 run, maxit 10000
# initialized at the MLE of the neutral model
# first round of GA - type vanilla - 500 run, maxit 10000 

# collect values from the converged genetic algorithm as global esimates 
# Neutral model 
as_tibble(t(c(result_neutral$GAobj@solution[,3:8], 
              result_neutral$GAobj@solution[,1:2]))) %>%
  mutate(logLik = result_neutral$GAobj@fitnessValue, 
         AIC = calculate_aic(logLik, npar = 8)) -> neutral_estms


# Cross protection model with fixed duration of cross-protection to 30 days 
as_tibble(t(c(result_fix_w_cp$GAobj@solution[,3:10], 
              result_fix_w_cp$GAobj@solution[,1:2]))) %>% 
  mutate(logLik = result_fix_w_cp$GAobj@fitnessValue, 
         AIC = calculate_aic(logLik, npar = 10)) -> cr_prtct_fix_phi_estms 


# Cross-protection model   
as_tibble(t(c(result_fix_w_phi_tiny_1$GAobj@solution[,5:12], 
              result_fix_w_phi_tiny_1$GAobj@solution[,1:4]))) %>%
  mutate(`1/phi_A` = 1/phi_A,
         `1/phi_B` = 1/phi_B,
         logLik = result_fix_w_phi_tiny_1$GAobj@fitnessValue, 
         AIC = calculate_aic(logLik, npar = 12)) %>% 
  select(-c(phi_A, phi_B)) -> cr_prtct_estms


# collect values from the converged genetic algorithm as global esimates 
# Neutral model : fit to 2019 data  fit to B
as_tibble(t(c(result_neutral_2019_3p$GAobj@solution[,1:3]))) %>%
  mutate(logLik = result_neutral_2019_3p$GAobj@fitnessValue) %>% 
  gather(key = "estd.param", value = "estimate") -> neutral_2019_B_estms


# Neutral model : fit to 2019 data  fit to A
as_tibble(t(c(result_neutral_2019_3p_A_trunc$GAobj@solution[,1:3]))) %>%
  mutate(logLik = result_neutral_2019_3p_A_trunc$GAobj@fitnessValue) %>% 
  gather(key = "estd.param", value = "estimate") -> neutral_2019_A_estms

# Neutral model : fit to 2019 data  fit to A full season
as_tibble(t(c(result_neutral_2019_3p_A$GAobj@solution[,1:3]))) %>%
  mutate(logLik = result_neutral_2019_3p_A$GAobj@fitnessValue) %>% 
  gather(key = "estd.param", value = "estimate") -> neutral_2019_A_fs_estms

# truncated A season
neutral_2019_3p <- cbind(neutral_2019_A_estms, neutral_2019_B_estms)

print(xtable(neutral_2019_3p, display = c("g", "s", "g", "g", "g"), digits = 5),
      include.rownames = FALSE)


# full A season 
neutral_2019_3p_fs <- cbind(neutral_2019_A_fs_estms, neutral_2019_B_estms)

print(xtable(neutral_2019_3p_fs, display = c("g", "s", "g", "g", "g"), digits = 5),
     include.rownames = FALSE)

# combine the results

column_order = c(10:9, 1:4, 7:8, 5:6, 11:14)

cr_prtct_fix_phi_estms %>% 
  full_join(neutral_estms) %>%
  mutate(Delta_AIC = AIC - min(AIC), 
         Model  = c("Cross-protection", "Neutral")) %>% 
  select(column_order) %>% 
  gather(key = "estd.param", value = "estimate", -Model) %>% 
  spread(key = Model, value = estimate) %>% 
  slice(8:9, 4:5, 2:3, 10:11, 12:13, 7, 1, 6) -> table_of_estimates

##############################################################################################################
########################################## condfidence intervals #############################################
##############################################################################################################

# percentile
percentile_2.5 <- function(x) {unname(quantile(x = x, probs = 0.025))}
percentile_97.5 <- function(x) {unname(quantile(x = x, probs = 0.975))}


check_error <- function(counter, res_list) {
  ifelse(class(res_list[[counter]]) == "try-error", TRUE, FALSE)
}

# 2016-2018 --------------------------------------------------------------------------------------------------
# Cross-Protection model -------------------------------------------------------------------------------------
error_cp <- sapply(1:1000, check_error, res_list = bootstrapped_params_cp_list)
# Get rid of all the bad runs -- NO bad runs!!

CI_cp_list <- bootstrapped_params_cp_list[which(error_cp == FALSE)]


cp_vars <- c("R0_B", "R0_A", "amplitude_A", "amplitude_B", "tpeak_A", "tpeak_B", 
             "rho_A", "rho_B", "chi_AB", "chi_BA", "value")


CI_cp <- t(sapply(1:42, function(x) {
  c(CI_cp_list[[x]]$result$par, value = CI_cp_list[[x]]$result$value)})) %>% 
  as.data.frame() %>% 
  summarise_all(.funs = c(percentile_2.5, percentile_97.5)) %>% t()

cbind(CI_cp[1:11], CI_cp[12:22]) %>% 
  as.data.frame() %>% 
  mutate(Parameter = cp_vars) %>% 
  slice(2:1, 9:10, 3:4, 7:8, 5:6) %>% 
  transmute(Parameter = Parameter, 
            `2.5%` = V1,   
            `97.5%` = V2 )-> CI_cp_mod

cr_prtct_fix_phi_estms %>% 
  select(starts_with("R0_"), starts_with("chi_"), 
         starts_with("amplitude_"), starts_with("rho"), starts_with("tpeak_")) %>% 
  gather(key = "Parameter", value = "mle") %>% 
  full_join(CI_cp_mod, by = "Parameter") -> cp_estms

# Neutral Model ----------------------------------------------------------------------------------------------
error_neut <- sapply(1:1000, check_error, res_list = bootstrapped_params_neutral_list)
# Get rid of all the bad runs -- NO bad runs!!

CI_neut_list <- bootstrapped_params_neutral_list[which(error_neut == FALSE)]

neut_vars <- cp_vars[-c(9:11)]

CI_neut <- t(sapply(1:42, function(x) {
  c(CI_neut_list[[x]]$result$par, value = CI_neut_list[[x]]$result$value)})) %>% 
  as.data.frame() %>% 
  summarise_all(.funs = c(percentile_2.5, percentile_97.5)) %>% t()

cbind(CI_neut[1:8,], CI_neut[10:17,]) %>% 
  as.data.frame() %>% 
  mutate(Parameter = neut_vars) %>% 
  slice(2:1, 3:4, 7:8, 5:6) %>% 
  transmute(Parameter = Parameter, 
            `2.5%` = V1,   
            `97.5%` = V2 )-> CI_neu_mod

neutral_estms %>% 
  select(starts_with("R0_"), starts_with("amplitude_"), starts_with("rho"), 
         starts_with("tpeak_")) %>% 
  gather(key = "Parameter", value = "mle") %>% 
  full_join(CI_neu_mod, by = "Parameter") -> neu_estms


# 2019 season ------------------------------------------------------------------------------------------------

# row and colum names 

var_names <- c("R0", "amplitude", "rho", "value")
col_names <- c("", "")

CI_A_2019 <- t(sapply(1:1000,
                      function(x){c(bootstrapped_params_neutral_A_2019_list[[x]]$result$par, 
                                    value = bootstrapped_params_neutral_A_2019_list[[x]]$result$value)})) %>% 
  as.data.frame() %>% 
  summarise_all(.funs = c(percentile_2.5, percentile_97.5)) %>% t()

CIs_A_2019 <- cbind(CI_A_2019[1:4,1], CI_A_2019[5:8,1]) %>% 
  as.data.frame() %>% 
  transmute(Var = var_names, 
            `2.5%` = V1, 
            `97.5%` = V2, 
            flu_type = "A")

CI_B_2019 <- t(sapply(1:1000,
                      function(x){c(bootstrapped_params_neutral_B_2019_list[[x]]$result$par, 
                                    value = bootstrapped_params_neutral_B_2019_list[[x]]$result$value)})) %>% 
  as.data.frame() %>% 
  summarise_all(.funs = c(percentile_2.5, percentile_97.5)) %>% t()

CIs_B_2019 <- cbind(CI_B_2019[1:4,1], CI_B_2019[5:8,1]) %>% 
  as.data.frame() %>% 
  transmute(Var = var_names, 
            `2.5%` = V1, 
            `97.5%` = V2, 
            flu_type = "B")


CIs_A_2019 %>% 
  bind_rows(CIs_B_2019) -> CIs_2019


#print(xtable(table_of_estimates, display = c("s", "g", "g", "g"), digits = 6), 
#      include.rownames = FALSE)


S0 <- tibble(Susceptible_prop_A = S_inf_A*100, 
             Susceptible_prop_B = S_inf_B*100)

#print(xtable(ic_2020, display = c("g", "g", "g", "g", "g", "g", "g"), 
#             digits = 4),include.rownames = FALSE)


##############################################################################################################  
############################################# fit plots ######################################################
##############################################################################################################

# select relevant parameters from the above estimates 

cr_prtct_estms %>% 
  full_join(cr_prtct_fix_phi_estms) %>% 
  full_join(neutral_estms) %>% 
  mutate(Model = c("Cross-protection", "Cross-protection2", "Neutral"), 
         phi_A = 1/`1/phi_A`, 
         phi_B = 1/`1/phi_B`) %>%
  replace_na(replace = list(chi_AB = 0, chi_BA = 0, phi_A = 0, phi_B = 0)) %>% 
  mutate(phi_A = ifelse(Model == "Cross-protection2", 365/30, phi_A),
         phi_B= ifelse(Model == "Cross-protection2", 365/30, phi_B)) %>% 
  select(-c(logLik, AIC,`1/phi_A`, `1/phi_B` )) -> simulate_from


# function to simulate trajectories

give_fit_trajectories <- function(counter = 3, model_params = simulate_from) {
  
  Model <- model_params[counter, "Model"]
  
  model_params[counter,] -> param_vector 
  
  # browser()
  rp_vals <- c(R0_A=param_vector$R0_A, gamma_A=365./2.5, w_A=1/4,
               R0_B=param_vector$R0_B, gamma_B=365./3.5, w_B=1/4,
               amplitude_A = param_vector$amplitude_A,
               amplitude_B = param_vector$amplitude_B,
               tpeak_A = param_vector$tpeak_A,
               tpeak_B = param_vector$tpeak_B,
               phi_A=param_vector$phi_A, phi_B=param_vector$phi_B,
               chi_BA=param_vector$chi_BA, chi_AB=param_vector$chi_AB, 
               eta_A=365., eta_B=365.,
               rho_A=param_vector$rho_A, rho_B=param_vector$rho_B,
               sigmaSE=0.0000, psi=0.00000, 
               pop=6.7e6)
  
  ic_vals <- SIRS2_independent_endemic_equilibrium(rp_vals)
  params_all <- c(rp_vals,ic_vals)
  
  # specify length of burn in for simulations (in years)
  t_start <- -300 
  
  # Create pomp model
  real_data_3s %>% 
    create_SIRS2_pomp_model(time_start_sim=t_start) %>% 
    trajectory(params = params_all, format = "d", method = "ode45") %>% 
    slice(2:n()) %>% 
    select(time, K_A, K_B) %>%
    mutate(scases_A = param_vector$rho_A*K_A, 
           scases_B = param_vector$rho_B*K_B) %>% 
    select(time, scases_A, scases_B) %>% 
    mutate(time = time + 2014) %>% 
    gather(key = "flu_type", value = "cases", -time) %>% 
    mutate(cases_lb = qpois(0.025, cases), 
           cases_ub = qpois(0.975, cases), 
           Model = Model$Model) -> traj_data
  
  return(traj_data)
  
}


# generate trajectories

all_fit <- map_dfr(.x = 1:3, .f = give_fit_trajectories)

# combine the trajectories with data 

real_data_3s %>% 
  slice(2:n()) %>% 
  select(-time) %>% 
  gather(key = "flu_type", value = "obs_cases") %>% 
  select(-flu_type) %>% 
  slice(rep(1:n(), times = 3)) %>% 
  bind_cols(all_fit) %>% 
  select(2, 3, 1, 4:7) -> all_fit_with_data

# NOTE :: Get rid of the model predictions where the duration of cross-protection is estimated 
# NOTE :: Final results iclude model where this dursation was fixed to 30 days.
all_fit_with_data %>% 
  filter(Model != "Cross-protection") -> all_fit_with_data_subset
  
text_data <- tibble(x1 = rep(2016.79,2), x2 = rep(2016.85, 2), x3 = rep(2018.2, 2),  
                    y1 = rep(1100, 2),  y2 = rep(825, 2), y3 = rep(1150, 2),
                    labs1 = c("",""), labs2 = c("A", "B"), labs3 = c("", ""), 
                    flu_type = c("scases_A", "scases_B"))


# make the plot_object 
all_fit_with_data_subset %>% 
  # slice(2:n()) %>% 
  ggplot(aes(x = time, y = cases)) +
  geom_line(aes(colour = flu_type, 
                linetype = Model), size = 0.8) +
  geom_ribbon(aes(ymin = cases_lb, ymax = cases_ub, 
                  fill = flu_type, group = Model), 
              alpha = 0.2) +
  geom_line(aes(y=obs_cases, colour = flu_type), size = 0.8) +
  labs(y = "Cases", 
       x = "Year") +
  geom_text(data = text_data, aes(label = labs1, x = x1, y = y1)) +
  geom_text(data = text_data, aes(label = labs2, x = x2, y = y2)) +
  geom_text(data = text_data, aes(label = labs3, x = x3, y = y3)) +
  scale_colour_manual(name = "", 
                      labels = c("Type A", "Type B"), values = c("red", "blue")) +
  scale_fill_manual(name = "", 
                    labels = c("Type A", "Type B"), values = c("red", "blue")) +  
  scale_linetype_manual(name = "", labels = c("Cross-protection", "Neutral"),
                        values = c("twodash", "dotted")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 900, by = 300), limits = c(0,900)) +
  gg.theme+
  theme(aspect.ratio = 0.5, 
        legend.position = "bottom") +
  guides(linetype = guide_legend(ncol = 1, order = 1), 
         colour = guide_legend(ncol = 1)) -> traj_data_comp_plt_opt1

# option2
traj_data_comp_plt_opt2 <- traj_data_comp_plt_opt1 +
  facet_wrap(.~flu_type, nrow = 2) +
  theme(strip.text = element_blank(), 
        legend.position = "bottom")


##############################################################################################################
################################### fit plots for 2019  ######################################################
##############################################################################################################
# fit to A 

# fit plots and conditional logliklihood

these_params_to_fix_A <- c("tpeak_A", "tpeak_B", "R0_B", "amplitude_B", "rho_B")

params_all_2019_neut_A <- params_all_2019_neut

params_all_2019_neut_A[these_params_to_fix_A] <- result_neutral$GAobj@solution[1,these_params_to_fix_A]

# parameters of interest
est_these_A <- c("R0_A",  "amplitude_A",  "rho_A")


##########################################
######### model fits for season A ########
##########################################


of_2019(par = result_neutral_2019_3p_A$GAobj@solution[1,], est = est_these_A, 
        pomp_object = po_2019, params_all_int = params_all_2019_neut_A, 
        target_data = inc_data_2, give_conditional_log_lik = TRUE, fit = "A") -> fit_data_A



# plot the values of the with respect to data 

# fit plots and conditional logliklihood

these_params_to_fix_B <- c("tpeak_A", "tpeak_B", "R0_A", "amplitude_A", "rho_A")

params_all_2019_neut_B <- params_all_2019_neut

params_all_2019_neut_B[these_params_to_fix_B] <- result_neutral$GAobj@solution[1,these_params_to_fix_B]


# parameters of interest
est_these <- c("R0_B",  "amplitude_B",  "rho_B")

of_2019(par = result_neutral_2019_3p$GAobj@solution[1,], est = est_these, 
        pomp_object = po_2019, params_all_int = params_all_2019_neut_B, target_data = inc_data_2, 
        fit = "B", give_conditional_log_lik = TRUE) -> fit_data_B



.max.week <- with(fit_data_B, 
    which.max(obs_cases)
)
# work with the weeks 
fit_data_B %>% 
  select(1:4) %>% 
  mutate(week = seq(1,n()), 
         ref_max_week = (week - .max.week)
  ) %>% 
  select(flu_type, st_cases, obs_cases, ref_max_week) -> fit_data_B_subset

# Make sure the data for A is centered around the peak week of B
fit_data_A %>% 
  select(1:4) %>% 
  mutate(week = seq(1,n()), 
         ref_max_week = week - .max.week) %>% 
  select(flu_type, st_cases, obs_cases, ref_max_week) -> fit_data_A_subset

# Calculate the poisson error
fit_data_A_subset %>% 
  bind_rows(fit_data_B_subset) %>% 
  mutate(cases_lb = qpois(0.025, st_cases), 
         cases_ub = qpois(0.975, st_cases), 
         flu_type = ifelse(flu_type == "total_a", "A", "B")) -> fit_data_2019


legend_pos = c(0.8, 0.58)



# make a plot for A
fit_data_2019 %>% 
  filter(flu_type == "A") %>% 
  ggplot(aes(x = ref_max_week, y = st_cases)) +
  geom_line(aes(linetype = "dotdash"), colour = "red", size = 0.8) +
  geom_line(aes(y = obs_cases, linetype = "solid"), colour = "red", size = 0.8) +
  geom_ribbon(aes(ymin = cases_lb, ymax = cases_ub, fill = "red"), alpha = 0.2) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", 
             colour = "grey40", size = 0.8) +
  geom_text(aes(label = "A", x = -16.5, y = 590)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
  #geom_text(aes(label = "A", x = -17, y = 590)) +
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
  ylim(0,620) +
  scale_linetype_identity(name = "Type A Incidence", 
                          breaks = c("solid", "dotdash"), 
                          labels = c("Data", "Fit"), 
                          guide = "legend") +
  scale_fill_identity(name = "", 
                      breaks = c("red"), 
                      labels = "Observation\n error", 
                      guide = "legend") +
  labs(x = "Week (Relative to Type B peak)", 
       y = "Cases") +
  gg.theme +
  theme(aspect.ratio = 0.5, 
        legend.position = legend_pos) +
  guides(linetype = guide_legend(ncol = 1, order = 1), 
         colour = guide_legend(ncol = 1)) +
  theme(legend.spacing.y = unit(0.1, "lines"),
        legend.key = element_blank(), 
        legend.background = element_blank()) -> flu_fit_A
  
# make a plot for B
fit_data_2019 %>% 
  filter(flu_type == "B") %>% 
  ggplot(aes(x = ref_max_week, y = st_cases)) +
  geom_line(aes(linetype = "dotdash"), colour = "blue", size = 0.8) +
  geom_line(aes(y = obs_cases, linetype = "solid"), colour = "blue", size = 0.8) +
  geom_ribbon(aes(ymin = cases_lb, ymax = cases_ub, fill = "blue"), alpha = 0.2) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", 
             colour = "grey40", size = 0.8) +
  geom_text(aes(label = "B", x = -16.5, y = 590)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
  #geom_text(aes(label = "B", x = -17, y = 590)) +
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
  ylim(0,620) +
  scale_linetype_identity(name = "Type B Incidence", 
                          breaks = c("solid", "dotdash"), 
                          labels = c("Data", "Fit"), 
                          guide = "legend") +
  scale_fill_identity(name = "", 
                      breaks = c("blue"), 
                      labels = "Observation\n error", 
                      guide = "legend") +
  labs(x = "Week (Relative to Type B peak)", 
       y = "Cases") +
  gg.theme +
  theme(aspect.ratio = 0.5, 
        legend.position = legend_pos) +
  guides(linetype = guide_legend(ncol = 1, order = 1), 
         colour = guide_legend(ncol = 1)) +
  theme(legend.spacing.y = unit(0.1, "lines"), 
        legend.key = element_blank(), 
        legend.background = element_blank()) -> flu_fit_B


plot_grid(flu_fit_A, flu_fit_B, nrow = 2) -> fit_plt_2019




theme(legend.spacing.y = unit(0.1, "lines")) -> flu_fit_B


if(FALSE) {
# Load and extract MA data
#Todo: check and correct data source
df_ma <- (
    read_csv("./ili_clean.csv",)
    %>% filter(state.abb=="MA")
    ## ask toby: edit from commit b6db16ad (5 mar)
    %>% filter(season %in% 2016:2018)
)


# df_ma["time"] <- df_ma["week_mod"]*7/365.25 # best definition?

df_ma["time"] <- df_ma[,"day"]/365.25
df_ma["time"] <- df_ma["time"] - floor(min(df_ma["time"]))

# take care of the first data point
pseudo_data_point <- data.frame(time = df_ma$time[1]-7/365.25,
                                total_a = NA, 
                                total_b = NA)
# bind with the real data
df_ma %>%
  bind_rows(pseudo_data_point) %>%
  arrange(time) -> df_ma
}
if(FALSE) {
# This year's data 
df_ma_2019 <- (
  read_csv("./latest_season_flu.csv") %>% 
    select(YEAR, WEEK, `TOTAL A`, `TOTAL B`) %>% 
    transmute(Year = YEAR + WEEK/52, 
              total_a = `TOTAL A`, 
              total_b = `TOTAL B`)
)

# save(df_ma_2019, file = "./data/real_data_2019_seas.Rdata")

df_ma_2019 %>% 
  gather(key = "FluType", value = "Cases", -Year) %>% 
  ggplot(aes(x = Year, y = Cases, colour = FluType)) +
  geom_line(size = 0.8) +
  scale_colour_manual(values = c("red", "blue")) 
}  



# Model parameter names
model_params = c("R0_A", "gamma_A", "w_A", "eta_A",
                 "R0_B", "gamma_B", "w_B", "eta_B",
                 "phi_A", "phi_B",
                 "chi_AB", "chi_BA",
                 "rho_A", "rho_B", "sigmaSE",
                 "amplitude_A", "tpeak_A", "amplitude_B", "tpeak_B", "pop")

# Model variable names
model_variables = c("S", "I_A",  "C_A",  "R_A",
                    "I_B", "C_B", "R_B", "I_AB", "I_BA", "R_AB")

# Model initial conditions parameter names
model_ic_params = paste(model_variables, "_0", sep="")

# Default model parameters # zero-width for fixed paramters
default_sobol_ranges <- list("R0_A"=c(1.2,7), "gamma_A"=c(365./2.5,365./2.5),
                             "w_A"=c(1./5., 1./5), "eta_A"=c(365./1., 365./1.),
                             "phi_A"=c(365./1., 1./20.),
                             "phi_B"=c(365./1., 1./20.),
                             "R0_B"=c(1.2,7), "gamma_B"=c(365./3.4,365./3.4),
                             "w_B"=c(1./5., 1./5), "eta_B"=c(365./1., 365./1.),
                             "chi_AB"=c(0,1), "chi_BA"=c(0,1), 
                             "rho_A"=c(0.0002, 0.001), "rho_B"=c(0.0002, 0.001), 
                             "sigmaSE"=c(0.0001, 0.001),
                             "amplitude_A"=c(0.01,0.1), "tpeak_A"=c(0.01,0.2), 
                             "amplitude_B"=c(0.01,0.1), "tpeak_B"=c(0.01,0.2), 
                             "pop"=c(6.7e6,6.7e6))

fix_pars=c("pop","gamma_A", "w_A", "eta_A",
           "gamma_B", "w_B", "eta_B", "psi")

# Model endemic equilibrium
# SIRS_endemic_equilibrium <- function(params){
#   S_0<-1/params[["R0_A"]]
#   k <- params[["gamma_A"]]/params[["w_A"]] 
#   return(c(S_0=S_0, I_A_0=(1-S_0)/(1+k),  R_A_0=k*(1-S_0)/(1+k)))
# }

SIRS2_independent_endemic_equilibrium <- function(params){
  S_A <- 1/params[["R0_A"]]
  S_B <- 1/params[["R0_B"]]
  
  k_A <- params[["gamma_A"]]/(params[["w_A"]] + params[["phi_A"]])
  k_B <- params[["gamma_B"]]/(params[["w_B"]] + params[["phi_B"]])
  
  r_A <-  params[["phi_A"]]/params[["w_A"]]
  r_B <-  params[["phi_B"]]/params[["w_B"]]
  
  I_A <- (1-S_A)/(1+k_A + k_A*r_A)
  I_B <- (1-S_B)/(1+k_B + k_B*r_B)
  
  ee_A <- c(S_0=S_A, I_0=I_A,  C_0=k_A*I_A, R_0=r_A*k_A*I_A)
  ee_B <- c(S_0=S_B, I_0=I_B,  C_0=k_B*I_B, R_0=r_B*k_B*I_B)
  
  # Note only  classes: model assumes coinfection (I_A * I_B) is neglible
  return(c(S_0=ee_A[["S_0"]]*ee_B[["S_0"]],
           I_A_0 = ee_A[["I_0"]]*ee_B[["S_0"]],
           I_B_0 = ee_A[["S_0"]]*ee_B[["I_0"]],
           C_A_0 = ee_A[["C_0"]]*ee_B[["S_0"]],
           C_B_0 = ee_A[["S_0"]]*ee_B[["C_0"]],
           R_A_0 = ee_A[["R_0"]]*ee_B[["S_0"]],
           R_B_0 = ee_A[["S_0"]]*ee_B[["R_0"]],
           I_BA_0 = ee_A[["R_0"]]*ee_B[["I_0"]],
           I_AB_0 = ee_A[["I_0"]]*ee_B[["R_0"]],
           R_AB_0 = ee_A[["R_0"]]*ee_B[["R_0"]]))
}


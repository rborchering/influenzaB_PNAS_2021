# load pre-requistie files 

## use default dir
.dir <- function(fn, dir='../extra/data-pomp/') {
    paste0(dir,fn)
}

# data 
load(.dir("real_data_3s.Rdata"))
load(.dir("real_data_2019_seas.Rdata"))

# sets of parameter estimates to generate initial conditions
# estimate parameters  
# season 2016-2018
load(.dir("result_fix_w_phi_tiny_1.Rdata"))
load(.dir("result_fix_w_cp.Rdata"))
load(.dir("result_neutral.Rdata"))

# season 2019: only neutral models 
load(.dir("result_neutral_2019_3p_A_trunc.Rdata"))
load(.dir("result_neutral_2019_3p_A.Rdata"))
load(.dir("result_neutral_2019_3p.Rdata"))


# bootstrapped values for the confidence intervals 
load(.dir("bootstrapped_cp.Rdata"))
load(.dir("bootstrapped_neutral.Rdata"))

load(.dir("bootstrapped_neutral_A_2019.Rdata"))
load(.dir("bootstrapped_neutral_B_2019.Rdata"))

# 
load(.dir("result_neutral_4s.Rdata"))

# simulation results main manuscript and supplementary
#fig.4
load(.dir("res_df_750.Rdata"))
load(.dir("res_df_2_750.Rdata"))
# fig.5
# load(.dir("data/epi_data_R0_scenes.Rdata"))
# load(.dir("data/epi_data_susc_backlog.Rdata"))

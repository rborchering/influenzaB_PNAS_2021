## Brief overview of which files come from which analysis and scripts 

### Following paths are relative to the current directory
path = ../../pomp-model
analysis_path_cp = path/likelihood/fix_w 
analysis_path_neutral = path/likelihood/neutral


### Following files are convenience objects generated from trajectory-matching procedure

Script: path/sim_data_and_params.R
real_data_3s.Rdata
real_data_2019_seas.Rdata

Script: analysis_path_cp/global_search_fix_w_cp.R
result_fix_w_cp.Rdata

Script: analysis_path_cp/global_search_fix_w_cp.R
result_fix_w_phi_tiny_1.Rdata

Script: analysis_path_neutral/global_search_neutral.R
result_neutral.Rdata

Script: analysis_path_neutral/hold_A_const/global_search_neutral_2019_3p.R
result_neutral_2019_3p.Rdata

Script: analysis_path_neutral/hold_B_const/global_search_neutral_2019_3p.R
result_neutral_2019_3p_A.Rdata

Script: analysis_path_neutral/hold_B_const/global_search_neutral_2019_3p_trunc.R
result_neutral_2019_3p_A_trunc.Rdata

Script: analysis_path_neutral/global_search_neutral_4s.R
result_neutral_2019_4s.Rdata


### Following files are convenience objects generated from parameteric bootstrap procedure

Script: analysis_path_cp/param_bootstrap/param_bootstrap.R
bootstrapped_cp.Rdata

Script: analysis_path_neutral/param_bootstrap/param_bootstrap.R
bootstrapped_neutral.Rdata

Script: analysis_path_neutral/hold_B_const/param_bootstrap/param_bootsrap.R
bootstrapped_neutral_A_2019.Rdata

Script: analysis_path_neutral/hold_A_const/param_bootstrap/param_bootsrap.R
bootstrapped_neutral_B_2019.Rdata
  

### Following files are convenience objects generated from simulation procedure

Script: path/sim_sirs_simulation_study.R
res_df_750.Rdata
res_df_2_750.Rdata

Script: path/sim_sirs_simulation_study_2.R
epi_data_R0_scenes.Rdata
epi_data_susc_backlog.Rdata

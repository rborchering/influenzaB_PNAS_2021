## re-used settings for plots
source('plotting/setup.R')
## overview timeseries
source('plotting/preliminaryFigs.R')
source('plotting/seasonality.R')
## model Prop B, compute predict vs observed
source('plotting/glmer.R')
source('plotting/aligned_peaks.R')
## phylodynamics plot
source('plotting/phylodynamic_plot.R')
source('plotting/phase.R')
## per capita samples by season, age-group, lineage 
source('plotting/analyze_lineages_age_groups.R')
## dynamical model plot
## depends on data from cluster:
#source("pomp-model/0load.R", chdir=T)
# source('plotting/pmla_plts_2.R', chdir = TRUE)

source("plotting/dynamic_simulation.R", chdir = TRUE)
source('plotting/R0_scenes_susc_backlog.R', chdir = TRUE)
source('plotting/global_search.R', chdir = TRUE)


## deprecated / historical:
## subsumed into glmer.R
#source('plotting/weekly_prop_pos.R')
#source('plotting/analyze_subtypes_age_groups.R')

source("plotting/dynamic_simulation.R")
source('plotting/global_search.R')


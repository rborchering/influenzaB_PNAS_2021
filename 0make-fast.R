## setup
## includes libraries
source('src/functions.R', chdir=T)
## process data
source('02-Scripts/00-process-demographic.R')
source('02-Scripts/00-process-FluView.R')
source('02-Scripts/prop.by.type.R')
source('02-Scripts/aligned_peaks.R')
source('02-Scripts/lineages_age_groups.R')
source('02-Scripts/phylodynamics_data.R')
source('02-Scripts/phase.R', chdir=T)
source('pomp-model/0make.R', chdir=T)

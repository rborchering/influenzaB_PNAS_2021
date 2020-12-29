# List all the packages that will be used
packages <- c(
    ## plotting
    "ggplot2", "RColorBrewer", "cowplot", 'scales', "egg",
    "viridis", "lattice", "gridExtra", "xtable", "grid",
    ## breaks ggtree plots
    # 'ggpubr',
    ## misc data processing
    "forcats", "lubridate", "ISOweek", 
    ## hadleyverse
    "dplyr", "magrittr", "tidyverse", "tidyr", "dplyr", 
    ## xian
   "parallel",  "wrapr", "data.table", "lme4", "mgcv", "emmeans",
    ## bodie
    "ape", "phytools", "ips", "tidytree",
    ## toby & deven
    "pomp", "doParallel", "doRNG", "LaplacesDemon", "furrr", "nloptr",
    'tictoc', 'doRNG','GA',"egg"
)
# List any BioConductor packages that may be needed
packages.bioconductor <- c("ggtree")
# This will load all the packages listed above...
invisible(lapply(
    c(packages, packages.bioconductor), 
    library, character.only = TRUE
))

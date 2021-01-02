# influenzaB_PNAS_2021
This repository contains the data and code used to analyze the 2019/2020 influenza season in the United States.

[![DOI](https://zenodo.org/badge/325383176.svg)](https://zenodo.org/badge/latestdoi/325383176)



## Data
* FluView Data: most recently accesed Febrary 28, 2020.
* FluView Data: accesed January 31, 2020, February 4, 2020
source: https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
* Sequence data was obtained from GISAID. 
* See also [here](00-RawData/README.md).

## Prerequisites
* R, and the R packages listed [here](./src/libraries.R).
* Note: the tinytex package requires a local install (once per machine). See https://yihui.org/tinytex/ 

## Instructions
* In R: 
    - In the root directory, run `source("0build.R")`.
    - This file calls "0make.R" to run R analyses and generate R figures.
    

## Additional Details
* For details regarding extra files see [here](extra/README.md).
* BEAST was used for the analysis displayed in Figure 3. For details, see [here](00-RawData/phylodynamic-data/README.md).
* Time series analysis (including phase and autoencoder) was conducted with python. Jupiter notebooks are provided in `extra/time-series-analysis`. The results are located in `extra/data-phase` and `extra/figure-si`.
* Parameter estimation was conducted in R on a cluster due to computational time considerations. For conveneience, this step can be by-passed and all the plots can be built from pre-computed R objects (located in `extra/data-pomp`). See [here](pomp-model/README.md) for additional details.

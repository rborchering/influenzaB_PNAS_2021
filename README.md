# InfluenzaB-2020
This repository contains the data and code used to analyze the 2019/2020 flu season. 

## Data
* FluView Data: most recently accesed Febrary 28, 2020.
* FluView Data: accesed January 31, 2020, February 4, 2020
source: https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
* Sequence data was obtained from GISAID. 
* See also [here](00-RawData/README.md).

## Prerequisites
* R, and the R packages listed [here](./src/libraries.R).

## Instructions
* In R: 
    - Download `extra/extra.zip` from gdrive and unzip into `./extra`.
    - Run all analysis from project root dir: `source("0make.R")`
    - Build document `source("0build.R")`

## Additional Details
* For details regarding extra files see [here](extra/README.md).
* BEAST was used for the analysis displayed in Figure 3. For details, see [here](00-RawData/phylodynamic-data/README.md).
* Time series analysis (including phase and autoencoder) was conducted with python. Jupiter notebooks are provided in `extra/time-series-analysis`. The results are located in `extra/data-phase` and `extra/figure-si`.
* Parameter estimation was conducted in R on a cluster due to computational time considerations. For conveneience, this step can be by-passed and all the plots can be built from pre-computed R objects (located in `extra/data-pomp`). See [here](pomp-model/README.md) for additional details.

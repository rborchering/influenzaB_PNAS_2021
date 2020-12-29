* 0make.R
    - prepare figures, called by `../0make.R`
    - uses precomputed results from cluster (see below)
* 0make-pomp_analysis.R
    - Runs trajectory matching on the Influenza weekly incidence data for 2 seasons 2016-2018 and 2019
    - Instrutions in R
        * `source("0make-pomp_analysis.R")`
    - CAUTION: Orginal analysis was run on a 5 node, 24 core cluster computer. Running the jobs sequentially can take a very long time
    - For convenience, results generated from 0make-pomp_analysis.R are stored in the folder `../extra/data-pomp`.
    - These objects are read by `0make.R` (above).

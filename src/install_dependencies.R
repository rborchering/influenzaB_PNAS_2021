install.dependencies <- function (cran.packages = packages, bc.packages = bioconductor.packages) {
  packages.to.install <- cran.packages[!(cran.packages %in% installed.packages()[,'Package'])]
  bioC.to.install <- bc.packages[!(bc.packages %in% installed.packages()[,'Package'])]
  if (length(packages.to.install)) {
    install.packages(packages.to.install, repos = "https://cloud.r-project.org")
  }
  # Check to see if there are any packages to be installed using BioConductor
  if (length(bioC.to.install)) {
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager", repos = "https://cloud.r-project.org")
    BiocManager::install(bioC.to.install)
  }
  # This will load all the packages listed above...
  invisible(lapply(cran.packages, require, character.only = TRUE))
  invisible(lapply(bc.packages, require, character.only = TRUE))
}
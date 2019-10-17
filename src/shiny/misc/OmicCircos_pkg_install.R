####################################
## Install Circos stuff for FATCAT_matplot
## 11/2017 Justin Rajendra
## 10/2019 added devel version for R version >= 3.6

## get R version 
r.ver <- R.Version()

if(r.ver$minor >= 6){

  if (!requireNamespace("BiocManager",quietly=TRUE))
    install.packages("BiocManager",repos="https://cloud.r-project.org")
  
  ## The following initializes usage of Bioc devel
  BiocManager::install(version='devel',ask=FALSE)
  BiocManager::install("OmicCircos")
  
} else {
  source("http://bioconductor.org/biocLite.R")
  biocLite("OmicCircos")
}




####################################
## Install Circos stuff for FATCAT_matplot
## 11/2017 Justin Rajendra
## 10/2019 added devel version for R version >= 3.6

## get R version 
r.ver <- R.Version()

if(r.ver$minor >= 6){

  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  
  ## The following initializes usage of Bioc devel
  BiocManager::install(version='devel')
  BiocManager::install("OmicCircos")
  
} else {
  source("http://bioconductor.org/biocLite.R")
  biocLite("OmicCircos")
}




####################################
## Install Circos stuff for FATCAT_matplot
## 11/2017 Justin Rajendra
## 10/2019 added devel version for R version >= 3.6
## 06/2020 remove devel for R version >= 3.6
## 11/2022 add version 4 specific syntax

## get R version 
r.ver <- R.Version()

if(r.ver$major == 4) {
	if (!require("BiocManager", quietly = TRUE))
		install.packages("BiocManager",repos="https://cloud.r-project.org")
	BiocManager::install("OmicCircos")
	
} else if(r.ver$major == 3 & r.ver$minor >= 6){
  if (!requireNamespace("BiocManager",quietly=TRUE))
    install.packages("BiocManager",repos="https://cloud.r-project.org")
  
  # BiocManager::install(version='devel',ask=FALSE)
  BiocManager::install("OmicCircos")
  
} else {
  source("http://bioconductor.org/biocLite.R")
  biocLite("OmicCircos")
}




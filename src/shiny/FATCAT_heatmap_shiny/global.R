#####################################
## 06/08/2017 Justin Rajendra
## 3dNetCorr heatmap
## global

## check for packages and load them
pkgs <- c('shiny','shinydashboard','plotly',
          'colourpicker','data.table','gplots')
for(pkg in pkgs){
 if(!require(pkg,character.only=TRUE)){
   print(paste0("ERROR: Missing ",pkg,"! please install it."))
   quit(save="no")
 } else {
   require(pkg,character.only=TRUE)
 }
}

## get arguments and check if folder is there
args <- commandArgs(TRUE)
cor.path <- args[1]
if(!dir.exists(cor.path)){
  print(paste0("ERROR: ",cor.path," does not exist!"))
  quit(save="no")
}

## get file list and see if there are files
file.list <- list.files(path=cor.path,pattern='\\.netcc$',full.names=TRUE)
file.list <- c(file.list,list.files(path=cor.path,pattern='\\.grid$',
                                        full.names=TRUE))
if(length(file.list) == 0){
  print(paste0("ERROR: no .netcc or .grid files in ",cor.path))
  quit(save="no")
}
names(file.list) <- basename(file.list)

stat.df <- read.csv('stat_methods.csv',stringsAsFactors=FALSE)

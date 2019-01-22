#####################################
## 11/2017 Justin Rajendra
## FATCAT matrix plot
## global

## check for packages and load them
pkgs <- c('shiny','shinydashboard','plotly','OmicCircos',
          'colourpicker','data.table','gplots','tools')
for(pkg in pkgs){
 if(!require(pkg,character.only=TRUE)){
   print(paste0("ERROR: Missing ",pkg,"! please install it with: ",
                "@afni_R_package_install -shiny -circos"))
   quit(save="no")
 } else {
   require(pkg,character.only=TRUE)
 }
}

#####################################
## get arguments and check if folder is there
args <- commandArgs(TRUE)
cor.path <- args[1]
if(!dir.exists(cor.path)){
  print(paste0("ERROR: ",cor.path," does not exist!"))
  quit(save="no")
}

#####################################
## get file list and see if there are any good files

## FATCAT or Netcorr files
file.list <- list_files_with_exts(cor.path,ext=c('grid','netcc'))

## any non FATCAT or 3dnetcorr files
ext.list <- c('csv','CSV','dat','DAT','tsv','TSV','1d','1D')
csv.files <- list_files_with_exts(cor.path,ext=ext.list)

## see if they are loadable and square
file.list2 <- c()
for(i in csv.files){
  cat(paste0("\nLoading: ",basename(i),"\n"))
  
  ## read in with with fread to take care of seps
  ## and data.frame for the row names
  tryCatch(
    {
      csv.df <- data.frame(fread(i,header=TRUE),row.names=1,check.names=FALSE)
      ## check for square with same names
      if(dim(csv.df)[1] == dim(csv.df)[2] &
         identical(rownames(csv.df),colnames(csv.df))){
        file.list2 <- rbind(file.list2,i)
      } else {
        cat(paste0("\n",basename(i),' is not a loadable matrix',"\n"))
      }
    }, 
    error=function(cond) {
      cat(paste0("\n",basename(i),' is not a loadable matrix',"\n"))
    }
  )
}

## make sure that there is something there and name
if(length(file.list2) > 0) {
  file.list <- c(file.list,file.list2)
}
if(length(file.list) == 0){
  print(paste0("ERROR: no .netcc or .grid files in ",cor.path))
  quit(save="no")
}
names(file.list) <- basename(file.list)

## read in stat descriptions for FATCAT and 3dnetcorr
stat.df <- read.csv('stat_methods.csv',stringsAsFactors=FALSE)



#####################################
## 11/2017 Justin Rajendra
## 3dMVM validator
## global

library(shiny)
library(data.table)
library(plotly)
library(shinydashboard)

#################
## get arguments and check if file is there
args <- commandArgs(TRUE)

## get current folder and make temp folder
cur.dir <- args[2]
out.dir <- paste0(cur.dir,"/__",sample(1000:9999,1),
                  "_3dMVM_validator_temp_delete")
dir.create(out.dir,showWarnings=FALSE)

## mvm table
table.file <- args[1]
if(!file.exists(table.file)){
  print(paste0("ERROR: ",table.file," does not exist!"))
  quit(save="no")
}

#################
## load the stat functions and afni drivers
source('stat_functions.R')
source('afni_funk.R')

## find out where afni is
afni.path <- dirname(system('which afni',intern=TRUE))

####################################################
## subjects and setup

## read in subject table
data.df <- fread(table.file,stringsAsFactors=TRUE,data.table=FALSE)
data.str <- fread(table.file,stringsAsFactors=FALSE)

## fix the quotes for the InputFile
data.df$InputFile <- gsub("'","",data.df$InputFile)
data.df$InputFile <- gsub('"',"",data.df$InputFile)
data.df$InputFile <- gsub('\\[','"[',data.df$InputFile)
data.df$InputFile <- gsub('\\]',']"',data.df$InputFile)
data.df$InputFile <- factor(data.df$InputFile)

data.str$InputFile <- gsub("'","",data.str$InputFile)
data.str$InputFile <- gsub('"',"",data.str$InputFile)
data.str$InputFile <- gsub('\\[','"[',data.str$InputFile)
data.str$InputFile <- gsub('\\]',']"',data.str$InputFile)

## get the number of subjects
n.subj <- length(levels(data.df$Subj))


## get categorical variables (first and last are Subj and InputFile)
catVar <- c(names(data.df)[sapply(data.df,is.factor)])
if(length(catVar) > 2){
  catVar <- catVar[3:length(catVar)-1]
} else {
  catVar <- NA
}

## quantitative variables
qntVar <- c(names(data.df)[sapply(data.df,is.numeric)])
if(length(qntVar) == 0){
  qntVar <- NA
  qntVar_str <- NA
} else {
  qntVar_str <- paste(qntVar,collapse=",")
}

## within subject variables if there
wsVars <- wsVarCatFinder(data.df[1:length(data.df)-1],1)
if(!all(is.na(wsVars))){
  if(identical(wsVars,catVar)){
    bsVars <- NA
  } else {
    bsVars <- setdiff(catVar,wsVars)
  }
} else {
  bsVars <- catVar
}

## get it together
allVars <- c(catVar,qntVar)

## input files as string for extraction
InputFile.str <- paste(data.df$InputFile,collapse=' ')

## copy one dataset to temp folder for the master
master.dset <- 'BassMaster.nii.gz'
system(paste0('cd ',cur.dir,' ; ',
              afni.path,'/3dbucket -prefix ',master.dset,' -session ',
              out.dir,' ',as.character(data.df$InputFile[1])) )

## launch afni
afni_launch(paste0(out.dir,'/',master.dset),out.dir)

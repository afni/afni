#!/usr/bin/env afni_run_R
#Welcome to 3dAOV.R, an AFNI Group Analysis Package!
#-----------------------------------------------------------
#Version 0.0.1,  Sept 28, 2012
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: https://afni.nimh.nih.gov/sscc/gangc/aov.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: 3dLME.R MyOutput &
# (Output is a file in which the running progress including 
# error messages will be stored)

#system("rm -f .RData")
source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))
#source(file.path('~/abin', "AFNIio.R"))
#libLoad("contrast")
libLoad("afex")

comArgs <- commandArgs()
if(length(comArgs)<6) modFile <- "model.txt" else
modFile <- comArgs[6]
#paste(commandArgs())

# Line 1: data type - volume or surface
datatype <- unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=0, strip.white=TRUE, nline=1)), "\\:"))[2]
   
#  Line 2: Output filename
#how to check output filename?
Out <-unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=1, strip.white=TRUE, nline=1)), "\\:"))[2]
#OutFile <- paste(Out, "+orig", sep="")
OutFile <- paste(Out, "+tlrc", sep="")

# check if the filename exists already
if(any((list.files()==paste(OutFile,'.BRIK', sep='')) | 
   (list.files()==paste(OutFile,'.HEAD', sep='')))) {
   print("Output filename already exists! Please rename it...")
   break
}

# Line 3: MASK
mask <- unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=2, strip.white=TRUE, nline=1)), "\\:"))[2]

# Line 4: #jobs
nNodes<- as.integer(unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=3, strip.white=TRUE, nline=1)), "\\:"))[2])

# Line 5: model formula
ModelShape <- unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=4, strip.white=TRUE, nline=1)), "\\:"))[2]
#nfixed <- length(strsplit(ModelShape[[1]], "\\*")[[1]])
#nfixed <- length(unique(unlist(strsplit(unlist(strsplit(ModelShape[[1]], 
#   "\\*")), "\\+"))))

#ModelForm <- as.formula(paste("Beta~", ModelShape))
#ModelForm <- as.formula(paste("Beta~", ModelShape, '+Error(Subj/(Intensity * Condition))'))
#if (length(grep("-1", ModelShape)==1)>0) NoConst <- TRUE else NoConst <- FALSE

# Line 6: covariates Cov is NA if no covariates
err <- unlist(strsplit(unlist(scan(file=modFile, what= list(""), 
   skip=5, strip.white=TRUE, nline=1)), "\\:"))[2]

ModelForm <- as.formula(paste("Beta ~", ModelShape, '+Error(Subj/(', err, '))'))

# Line 7: covariates Cov is NA if no covariates
Cov <- unlist(strsplit(unlist(strsplit(unlist(scan(file=modFile, 
   what= list(""), skip=5, strip.white=TRUE, nline=1)), "\\:"))[2], "\\*"))
if (is.na(Cov)) nCov <- 0 else nCov <- length(Cov)  # number of covariates

# Line 10 and the next few: pair-wise contrasts

# header position (hp) defined by column name InputFile
hp <- grep("InputFile", readLines(modFile)) 
Model <- read.table(modFile, skip=hp[1]-1, header=TRUE)
# More decent way to do this?
Model$Subj <-  as.factor(Model$Subj)
Model$InputFile <-  as.character(Model$InputFile)

# Assume the last column is input files
#FileCol <- length(colnames(Model))
FileCol <- dim(Model)[2]

#Number of input files
NoFile <- dim(Model[1])[1]

# Repeated-measures (use lme) or not (use lm)
#if (length(unique(Model$Subj)) != length(Model$Subj)) RM <- TRUE else RM <- FALSE

# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(Model[1, FileCol])
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
myHist <- inData$header$HISTORY_NOTE; myOrig <- inData$origin; myDelta <- inData$delta

inData <- unlist(lapply(lapply(Model[,FileCol], read.AFNI), '[[', 1))
dim(inData) <- c(dimx, dimy, dimz, NoFile)

if (!is.na(mask)) {
	Mask <- read.AFNI(mask)$brk[,,,1]
	inData <- array(apply(inData, 4, function(x) x*read.AFNI(mask)$brk[,,,1]), dim=c(dimx,dimy,dimz,NoFile))
}

# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

###############################
#Model$Beta<-inData[ii, jj, kk,]

#aa <- aov(Beta ~ Diag * Age * Intensity * Condition + Scanner + Error(Subj/(Intensity * Condition)), data=Model)
#bb <- aov.car(Beta ~ Diag * Age * Intensity * Condition + Scanner + Error(Subj/(Intensity * Condition)), data=Model)
#cc <- aov.car(Beta ~ Diag * Age + Scanner + Error(Subj/(Intensity * Condition)), data=Model)
#dd <- univ(aov.car(Beta ~ Diag * Age * Intensity * Condition + Scanner + Error(Subj/(Intensity * Condition)), data=Model))
#ee <- univ(aov.car(Beta ~ Diag * Age * Intensity * Condition + Scanner -Scanner:Intensity -Scanner:Condition + Error(Subj/(Intensity * Condition)), data=Model))

# number of terms
#nterm <- dim(dd$anova)[1]-1

# terms
#dimnames(dd$anova)[[1]][2:nterm]

#F-value
#unname(dd$anova[2:nterm,'F'])
# numerator DF
#unname(dd$anova[2:nterm,'num Df'])
# denominator DF
#unname(dd$anova[2:nterm,'den Df'])

###############################

for(ii in 1:dimx) for(jj in 1:dimy) for(kk in 1:dimz) {
   if(all(abs(inData[ii,jj,kk,])>10e-5)) {
      xinit <- ii; yinit <- jj; zinit <- kk; break }
}

ii <- xinit; jj <- yinit; kk <- zinit

tag<-1
library(afex)
while (tag == 1) {
   tag<-0
   Model$Beta<-inData[ii, jj, kk,]
        
   try(fm <- univ(aov.car(ModelForm, data=Model)), tag <- 1)
   if (tag == 1) if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      #print("Something is not quite right during testing!")
      break
   }
}

if (tag == 0)  {
   print("Good, test passed...")
   #if (NoConst) NoF <- nrow(anova(fm)) else NoF <- nrow(anova(fm))-1  # Just assume an intercept in the model
        #NoF <- nrow(anova(fm))
   #for (n in 1:ncontr) contrDF[n] <- temp[n]$df
   } else { 
      print(sprintf("Insoluble model! Formula %s on the 4th line in model.txt", ModelShape))
                print("might be inappropriate, or there are incorrect specifications in model.txt")
                print("such as contrasts, factor levels, or other obscure problems.")
      break; next # won't run the whole brain analysis if the test fails
}


# number of terms (or F-stats)
NoBrick <- dim(fm$anova)[1]-1

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

###############################

pars <- vector("list", 1)
pars[[1]] <- NoBrick

runAOV <- function(inData, dataframe, ModelForm, pars, tag) {
   #Stat <- rep(0, NoBrick)
   Stat <- rep(0, pars[[1]])
   #browser()
   if (!all(abs(inData) < 10e-8)) {        
      dataframe$Beta<-inData
      try(fm <- univ(aov.car(ModelForm, data=dataframe)), tag <- 1)
      if (tag == 0) try(Stat <- unname(fm$anova[-1,5]), tag <- 1)
   }
   return(Stat)
}

#runAOV(inData[ii, jj, kk,], dataframe=Model, ModelForm=ModelForm, pars=pars, tag=0)

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time by opening this file from time to time.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################

if (nNodes==1) for (kk in 1:dimz) {
   if(NoBrick > 1)
      Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runAOV, dataframe=Model, 
         ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1)) else
      Stat[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runAOV, dataframe=Model, 
         ModelForm=ModelForm, pars=pars, tag=0), dim=c(dimx, dimy, 1))      
   cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 


if (nNodes>1)    {
   library(snow)
   cl <- makeCluster(nNodes, type = "SOCK")
   clusterEvalQ(cl, library(afex)); #clusterEvalQ(cl, library(contrast))
   for (kk in 1:dimz) {
      if(NoBrick > 1) Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=Model, ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1)) else
         Stat[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=Model, ModelForm=ModelForm, pars=pars, tag=0), dim=c(dimx, dimy, 1))
      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}

# test on Z slice

#if (nNodes>1)    {
#   library(snow)
#   cl <- makeCluster(nNodes, type = "SOCK")
#   clusterEvalQ(cl, library(afex)); #clusterEvalQ(cl, library(contrast))
#   kk<- 32
#   
#      Stat[,,kk,] <-aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, dataframe=Model, ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1))
#      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
#   stopCluster(cl)
#}

###############################

MyLabel <- paste(dimnames(fm$anova)[[1]][-1], " F")
write.AFNI(OutFile, Stat, MyLabel, note=myHist, origin=myOrig, delta=myDelta, idcode=newid.AFNI())

statpar <- "3drefit"
for(ii in 1:NoBrick) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ",
   unname(fm$anova[-1,'num Df'])[ii], " ", unname(fm$anova[-1,'den Df'])[ii])
statpar <- paste(statpar, " -addFDR -newid ", OutFile)
system(statpar)
print(sprintf("Congratulations! You've got output %s+tlrc.*", Out))

# set save defaults using option:
#options(save.defaults=list(ascii=TRUE, safe=FALSE))
#save.image()
#unlink(".RData")

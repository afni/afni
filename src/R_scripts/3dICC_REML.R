#!/usr/bin/env afni_run_R
#Welcome to 3dICC_REML.R, an AFNI IntraClass Correlation Package!
#-----------------------------------------------------------
#Version 0.0.6,  Aug 1, 2013
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: https://afni.nimh.nih.gov/sscc/gangc/icc.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
# If this program is useful for you, consider cite the following:
# Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). 
# Linear Mixed-Effects Modeling Approach to FMRI Group Analysis. 
# NeuroImage, 10.1016/j.neuroimage.2013.01.047.
#-----------------------------------------------------------

# Commannd line to run this script: 3dICC.R MyOutput & (R CMD BATCH 3dICC3.R MyOut &)
# (Output is a file in which the running progress including 
# error messages will be stored)

#varComp <- function(x) {
#   v<-as.numeric(VarCorr(x)[,1])
#   v<-as.numeric(na.omit(v))
#   v/sum(v)
#}


lineNum <- function(key, inFile) grep(key, readLines(inFile), ignore.case = TRUE)

getInfo <- function(key, inFile) {
   LN<-lineNum(key, inFile)
	if (length(LN)>0) unlist(strsplit(unlist(scan(file=inFile, what= list(""), 
      skip=LN-1, strip.white=TRUE, nline=1)), "\\:"))[2] else NA		
}			

band <- function(x, lo, hi) {ifelse((x<=hi)&(x>=lo), x, ifelse(x>hi, hi, lo))}

system("rm -f .RData")

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
#source("~/abin/AFNIio.R")

comArgs <- commandArgs()
if(length(comArgs)<6) parFile <- "model.txt" else
parFile <- comArgs[6]
paste(comArgs)
paste(parFile)

#  Output filename: optional
Out <- getInfo("Output", parFile)
if(is.na(Out)) {
   print("No output file name provided: a suffix of TEST will be used...")
	Out <- "TEST"
}	
OutFile <- paste(Out, "+tlrc", sep="") 

pkgLoad('lme4')

# MASK: optional
mask <- getInfo("Mask", parFile)

fixEff <- getInfo("FixEff", parFile)
if(is.na(fixEff)) fixEff <- 1
ranEff <- unlist(strsplit(getInfo("RanEff", parFile), split="[+]"))

# number of Clusters: optional
nNodes <- as.integer(getInfo("Clusters", parFile))
if(is.na(nNodes)) nNodes<-1

# header position (hp) defined by column name InputFile
if(!is.na(LN<-lineNum("InputFile", parFile))) { 
   Model <- read.table(parFile, skip=LN-1, header=TRUE)
# More decent way to do this?
   Model$Subj <-  as.factor(Model$Subj)
   Model$InputFile <-  as.character(Model$InputFile)
} else {print("ERROR: No column named inputFile found!"); break}

# Number of input files
NoFile <- dim(Model[1])[1]
# number of random factors
# nFact <- dim(Model)[2]-1
nFact <- length(ranEff)
# factor names
#fNames <- colnames(Model)[which(colnames(Model) != "InputFile")]

#ModelForm <- paste("Beta~(1|",fNames[1],")")
ModelForm <- paste("Beta~", fixEff)
#if (nFact == 2 ) ModelForm <- paste(ModelForm,"+","(1|",fNames[2],")")
#if (nFact == 3 ) ModelForm <- paste(ModelForm,"+","(1|",fNames[2],")","+(1|",fNames[3],")")
for(ii in 1:nFact) ModelForm <- paste(ModelForm,"+(1|",ranEff[ii],")")
ModelForm <- as.formula(ModelForm)

# Read in the 1st input file so that we have the dimension information
Data <- read.AFNI(Model[1, "InputFile"])
dimx <- Data$dim[1]
dimy <- Data$dim[2]
dimz <- Data$dim[3]

head <- Data

if(!is.na(mask)) Mask <- read.AFNI(mask)$brk

if (length(grep('tlrc', Model[1, "InputFile"]))==1) outView <- "tlrc"
if (length(grep('orig', Model[1, "InputFile"]))==1) outView <- "orig"


# initialization
IData <- array(data=NA, dim=c(dimx, dimy, dimz, NoFile))

IData[,,,1] <- Data$brk
# Read in the rest input files (beta)
for (m in 2:NoFile) {
   IData[,,,m] <- read.AFNI(Model[m, "InputFile"])$brk
}

# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
ii<-dimx%/%2; jj<-dimy%/%2; kk<-dimz%/%2

tag <-1

while (tag == 1) {
   tag<-0
   Model$Beta<-IData[ii, jj, kk,]
   try(fm <- lmer(ModelForm, Model), tag <- 1)
   if (ii<dimx) ii<-ii+1 else {
      #print("Something is not quite right during testing!")
      break
   }
} 
	
if (tag == 0)  {
   print("Good, test passed...")
	#for (n in 1:ncontr) contrDF[n] <- temp[n]$df
   } else { 
      print("Something is not quite right during testing!")
      break; next # won't run the whole brain analysis if the test fails
}

#time1 <- format(Sys.time(), "%D %H:%M:%OS3")
print(format(Sys.time(), "%D %H:%M:%OS3"))
#print(sprintf("Start to compute %s planes along X axis. You can monitor the progress", dimx))
#print("and estimate the total run time by opening this file from time to time.")

if (!is.na(mask)) {
   IData<-array(apply(IData, 4, function(x) x*Mask[,,,1]), dim=c(dimx,dimy,dimz,NoFile))
   rm(Mask)   # retrieve some memory
}

#runREML <- function(myData, Model, ModelForm, nFact, tag) {
   #browser()
#   myStat<-vector(mode="numeric", length= nFact+1)
#   if (!all(myData == 0)) {	
#	Model$Beta<-myData
#	try(fmAOV<-lmer(ModelForm, data=Model), tag<-1)   
#	if (tag != 1) {	   
#	   for(ii in 1:nFact) myStat[ii] <- VarCorr(fmAOV)[[ii]][1]  # factor variances
#	   myStat[nFact+1] <- attr(VarCorr(fmAOV), "sc")^2  # residual variance
#	   myStat <- myStat/sum(myStat)
#	}
#}
#return(myStat)
#}

runREML <- function(myData, fm, nFact, tag) {
   #browser()
   myStat<-vector(mode="numeric", length= nFact+1)
   if (!all(myData == 0)) {	
	try(fmAOV<-refit(fm, myData), tag<-1)   
	if (tag != 1) {	   
	   for(ii in 1:nFact) myStat[ii] <- VarCorr(fmAOV)[[ii]][1]  # factor variances
	   myStat[nFact+1] <- attr(VarCorr(fmAOV), "sc")^2  # residual variance
	   myStat <- myStat/sum(myStat)
	}
}
return(myStat)
}


print(sprintf("Start to run analysis on %i Z slices: %s", dimz, format(Sys.time(), "%D %H:%M:%OS3")))
print("You can monitor the progress and estimate the total runtime by opening this file from time to time.")

outData <- array(0, dim=c(dimx, dimy, dimz, nFact+1))

if (nNodes==1) for (kk in 1:dimz) {
#   outData[,,kk,] <- aperm(apply(IData[,,kk,], c(1,2), runREML, Model=Model, ModelForm=ModelForm, nFact=nFact, tag=0), c(2,3,1))
   outData[,,kk,] <- aperm(apply(IData[,,kk,], c(1,2), runREML, fm=fm, nFact=nFact, tag=0), c(2,3,1))
   cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
}
	
if (nNodes>1)	 {
   pkgLoad('snow')
   #cl <- makeCluster(rep('locahost', nNodes), type = "SOCK")
   cl <- makeCluster(nNodes, type = "SOCK")
   clusterEvalQ(cl, library(lme4))
   for (kk in 1:dimz) {
#      outData[,,kk,] <- aperm(parApply(cl, IData[,,kk,], c(1,2), runREML, Model=Model, ModelForm=ModelForm, nFact=nFact, tag=0), c(2,3,1))
      outData[,,kk,] <- aperm(parApply(cl, IData[,,kk,], c(1,2), runREML, fm=fm, nFact=nFact, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}

print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
rm(IData)  # retrieve some memory

#outData <- band(outData, 0, 1)
#dd<-aperm(dd, c(2,3,4,1))

#MyLabel <- append(fNames, "Residual")
MyLabel <- append(names(VarCorr(fm)), "Residual")

#write.AFNI(OutFile, outData, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, 
#   delta=Data$delta, idcode="whatever")

write.AFNI(OutFile, outData, MyLabel, defhead=head, idcode=newid.AFNI())

statpar <- "3drefit"
statpar <- paste(statpar, " -view ", outView, OutFile)
system(statpar)
print(sprintf("Congratulations! You've got output %s+%s.*", Out, outView))


# set save defaults using option:
#options(save.defaults=list(ascii=TRUE, safe=FALSE))
#save.image()
#unlink(".RData")

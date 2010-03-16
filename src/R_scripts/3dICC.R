#!/usr/bin/env afni_run_R
#Welcome to 3dICC.R, an AFNI IntraClass Correlation Package!
#-----------------------------------------------------------
#Version 0.0.1,  Dec. 22, 2008
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: http://afni.nimh.nih.gov/sscc/gangc/icc.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
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
source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))

#  Output filename: optional
Out <- getInfo("Output", "model.txt")
if(is.na(Out)) {
   print("No output file name provided: a suffix of TEST will be used...")
	Out <- "TEST"
}	
OutFile <- paste(Out, "+orig", sep="") 

# MASK: optional
mask <- getInfo("Mask", "model.txt")

# Line 4: model formula
#ModelShape <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
#   skip=3, strip.white=TRUE, nline=1)), "\\:"))[2]
#number of factors
#nF <- length(unique(unlist(strsplit(unlist(strsplit(ModelShape[[1]], 
#:q   "\\*")), "\\+"))))
#factors   
#terms <-   unlist(strsplit(strsplit(ModelShape[[1]], "\\+")[[1]], "\\*"))

#ModelForm <- as.formula(paste("Beta~", ModelShape))

# number of Clusters: optional
nNodes <- as.integer(getInfo("Clusters", "model.txt"))
if(is.na(nNodes)) nNodes<-1

# header position (hp) defined by column name InputFile
if(!is.na(LN<-lineNum("InputFile", "model.txt"))) { 
   Model <- read.table("model.txt", skip=LN-1, header=TRUE)
# More decent way to do this?
   Model$Subj <-  as.factor(Model$Subj)
   Model$InputFile <-  as.character(Model$InputFile)
} else {print("ERROR: No column named inputFile found!"); break}

# Assume the last column is input files
#FileCol <- length(colnames(Model))
#FileCol <- dim(Model)[2]

# Number of input files
NoFile <- dim(Model[1])[1]
# number of factors
nFact <- dim(Model)[2]-1
# factor names
fNames <- colnames(Model)[which(colnames(Model) != "InputFile")]

ModelForm <- paste("Beta~", fNames[1])
if (nFact == 2 ) ModelForm <- paste(ModelForm,"+",fNames[2])
if (nFact == 3 ) ModelForm <- paste(ModelForm,"+",fNames[2],"+",fNames[3],
   "+",fNames[1],"*",fNames[2],"+",fNames[1],"*",fNames[3],"+",fNames[2],"*",fNames[3])
ModelForm <- as.formula(ModelForm)

# Read in the 1st input file so that we have the dimension information
Data <- read.AFNI(Model[1, "InputFile"])
dimx <- Data$dim[1]
dimy <- Data$dim[2]
dimz <- Data$dim[3]

if (!is.na(mask)) Mask <- read.AFNI(mask)$brk

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
   try(fm <- lm(ModelForm, Model), tag <- 1)
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

fmAOV<-anova(fm)
#nComp <- length(fmAOV$Df)
DF <- fmAOV$Df+1
# correct for those multipliers
if (nFact == 2) DF[3] <- DF[1]*DF[2]
if (nFact == 3) {
	DF[4] <- DF[1]*DF[2]
	DF[5] <- DF[1]*DF[3]
	DF[6] <- DF[2]*DF[3]
	DF[7] <- DF[1]*DF[2]*DF[3]
	}
	
# Number of sub-bricks
NoBrick <- length(fmAOV$Df)

# Initialization
#Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))
#Stat <- array(0, dim=c(NoBrick, dimx, dimy, dimz))

#time1 <- format(Sys.time(), "%D %H:%M:%OS3")
print(format(Sys.time(), "%D %H:%M:%OS3"))
#print(sprintf("Start to compute %s planes along X axis. You can monitor the progress", dimx))
#print("and estimate the total run time by opening this file from time to time.")

if (!is.na(mask)) {
   IData<-array(apply(IData, 4, function(x) x*Mask[,,,1]), dim=c(dimx,dimy,dimz,NoFile))
   rm(Mask)   # retrieve some memory
}

runAna2 <- function(myData, Model, ModelForm, dof, tag) {
   #browser()
   myStat<-vector(mode="numeric", length=3)
   if (!all(myData == 0)) {	
	Model$Beta<-myData
	try(fmAOV<-anova(lm(ModelForm, data=Model)), tag<-1)   
	if (tag != 1) {	   
	   myStat[1] <- fmAOV$Mean[1] - fmAOV$Mean[3]  #MSA-MSAB
	   myStat[2] <- fmAOV$Mean[2] - fmAOV$Mean[3]  #MSB-MSAB
	   myStat[3] <- fmAOV$Mean[3]
	   
	   myStat <- dof*myStat
	   sumVar <- sum(myStat)
	   myStat <- myStat/sumVar
	}
}
return(myStat)
}


runAna3 <- function(myData, Model, ModelForm, dof, tag) {
#   browser()
   myStat<-vector(mode="numeric", length=7)
   if (!all(myData == 0)) {	
#	tag <- 0
#	browser()
	Model$Beta<-myData
	try(fmAOV<-anova(lm(ModelForm, data=Model)), tag<-1)   
	if (tag != 1) {	   
	   myStat[1] <- fmAOV$Mean[1] - fmAOV$Mean[4] - fmAOV$Mean[5] + fmAOV$Mean[7]  #MSA-MSAB-MSAC+MSABC
	   myStat[2] <- fmAOV$Mean[2] - fmAOV$Mean[4] - fmAOV$Mean[6] + fmAOV$Mean[7]  #MSB-MSAB-MSBC+MSABC
	   myStat[3] <- fmAOV$Mean[3] - fmAOV$Mean[5] - fmAOV$Mean[6] + fmAOV$Mean[7]  #MSC-MSAC-MSBC+MSABC
	   myStat[4] <- fmAOV$Mean[4] - fmAOV$Mean[7] #MSAB-MSABC
	   myStat[5] <- fmAOV$Mean[5] - fmAOV$Mean[7] #MSAC-MSABC
	   myStat[6] <- fmAOV$Mean[6] - fmAOV$Mean[7] #MSBC-MSABC
	   myStat[7] <- fmAOV$Mean[7]
	   
	   myStat <- dof*myStat
	   sumVar <- sum(myStat)
	   myStat <- myStat/sumVar
	}
}
return(myStat)
}

print(sprintf("Start to run analysis on %i Z slices: %s", dimz, format(Sys.time(), "%D %H:%M:%OS3")))
print("You can monitor the progress and estimate the total run time by opening this file from time to time.")

if (nFact ==2) {	
	nBrick <- 3;
    outData <- array(0, dim=c(dimx, dimy, dimz, nBrick))

if (nNodes==1) for (kk in 1:dimz) {
   outData[,,kk,] <-aperm(apply(IData[,,kk,], c(1,2), runAna2, Model=Model, ModelForm=ModelForm, dof=DF, tag=0), c(2,3,1))
   cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
}
	
if (nNodes>1)	 {
   library(snow)
   #cl <- makeCluster(rep('locahost', nNodes), type = "SOCK")
   cl <- makeCluster(nNodes, type = "SOCK")
   for (kk in 1:dimz) {
      outData[,,kk,] <-aperm(parApply(cl, IData[,,kk,], c(1,2), runAna2, Model=Model, ModelForm=ModelForm, dof=DF, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}
}

if (nFact==3) {
	nBrick <- 7
    outData <- array(0, dim=c(dimx, dimy, dimz, nBrick))

if (nNodes==1) for (kk in 1:dimz) {
   outData[,,kk,] <-aperm(apply(IData[,,kk,], c(1,2), runAna3, Model=Model, ModelForm=ModelForm, dof=DF, tag=0), c(2,3,1))
   cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
}
	
if (nNodes>1)	 {
   library(snow)
   #cl <- makeCluster(rep('locahost', nNodes), type = "SOCK")
   cl <- makeCluster(nNodes, type = "SOCK")
   for (kk in 1:dimz) {
      outData[,,kk,] <-aperm(parApply(cl, IData[,,kk,], c(1,2), runAna3, Model=Model, ModelForm=ModelForm, dof=DF, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}
}

print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
rm(IData)  # retrieve some memory

#outData <- band(outData, 0, 1)
#dd<-aperm(dd, c(2,3,4,1))

MyLabel <- paste(dimnames(fmAOV)[1][[1]])

write.AFNI(OutFile, outData, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, 
   delta=Data$delta, idcode="whatever")

statpar <- "3drefit"
statpar <- paste(statpar, " -newid -view ", outView, OutFile)
system(statpar)
print(sprintf("Congratulations! You've got output %s+%s.*", Out, outView))


# set save defaults using option:
#options(save.defaults=list(ascii=TRUE, safe=FALSE))
#save.image()
#unlink(".RData")

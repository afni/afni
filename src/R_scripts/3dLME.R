#!/usr/bin/env R

#Welcome to 3dLME.R, an AFNI Group Analysis Package!
#-----------------------------------------------------------
#Version 0.0.2, Oct. 11, 2007
#Author: Gang Chen
#SSCC/NIMH/ National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: R CMD BATCH $LME/3dLME.R

# Need a mask to reduce runtime!!!!!!

source(file.path(Sys.getenv("LME"), "io.R"));
#install.packages("nlme",dependencies=TRUE);
library(nlme);

# Line 1: data type - volume or surface
datatype <- scan(file="model.txt", what= list(""), skip=0, nline=1);

#  Line 2: Output filename
#how to check output filename?
Out <-scan(file="model.txt", what= list(""), skip=1, nline=1);
OutFile <- paste(Out, "+orig", sep="");

# Line 3: model formula
form <- scan(file="model.txt", what= list(""), skip=2, nline=1);

# Line 5: Number of statistics
NoF <- scan(file="model.txt", what= integer(), skip=4, nline=1);

hp <- grep("InputFile", readLines("model.txt")) # header position defined by column name InputFile
Model <- read.table("model.txt", skip=hp[1]-1, header=TRUE);
# More decent way to do this?
Model$Subj <-  as.factor(Model$Subj);
Model$InputFile <-  as.character(Model$InputFile);

# Assume the last column is input files
#FileCol <- length(colnames(Model));
FileCol <- dim(Model)[2];

#Number of input files
NoFile <- dim(Model[1])[1]

if (datatype == "volume") {

# Read in the 1st input file so that we have the dimension information
Data <- read.AFNI(Model[1, FileCol]);
dimx <- Data$dim[1];
dimy <- Data$dim[2];
dimz <- Data$dim[3];

# initialization
IData <- array(data=NA, dim=c(dimx, dimy, dimz, NoFile));

IData[,,,1] <- Data$ttt;
# Read in the rest input files (beta)
for (m in 2:NoFile) {
   IData[,,,m] <- read.AFNI(Model[m, FileCol])$ttt;
}

Model$Beta<-IData[1, 1, 1,];
#Modeln <- groupedData(Beta ~ session | Subj, data = Model);

# No need for the 1st F, intercept
Stat <- array(data=NA, dim=c(dimx, dimy, dimz, NoF));

#time1 <- format(Sys.time(), "%D %H:%M:%OS3");

format(Sys.time(), "%D %H:%M:%OS3")
print(sprintf("Start to compute %s planes along X axis", dimx));
tag <- 0;
for (i in 1:dimx) {
for (j in 1:dimy) {
for (k in 1:dimz) {
   if (all(IData[i, j, k,]==0)) {
	   Stat[i, j, k,] <- rep(0, NoF);
	} else {	
	Model$Beta<-IData[i, j, k,];
	try(fit.lme <- lme(as.formula(paste("Beta~", form)), random = ~1|Subj, Model), tag <- 1);
   if (tag != 1) {
	   Stat[i, j, k,] <- anova(fit.lme)$F[-1];
	}	else {
	   Stat[i, j, k,] <- rep(0, NoF);
	}
	tag <- 0;
	}
}
}
print(sprintf("Finished dimX = %s", i));
print(format(Sys.time(), "%D %H:%M:%OS3"));
#gc();
}

MyLabel <- paste(rownames(anova(fit.lme))[-1], " F");
write.AFNI(OutFile, Stat, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, delta=Data$delta, idcode="whatever");
statpar <- "3drefit";
for (i in 2:(NoF+1)) {
   statpar <- paste(statpar, " -substatpar ", i-2, " fift ", anova(fit.lme)$numDF[i], " ", anova(fit.lme)$denDF[i]);
} 
statpar <- paste(statpar, " -view tlrc ", OutFile);
system(statpar);
print(sprintf("Congratulations! You've got the result %s+tlrc.*", Out));

} else {

if (datatype == "surface") {

# Read in the 1st input file so that we have the dimension information
# Data <- read.AFNI(Model[1, FileCol]);
Data <- read.table(Model[1, FileCol], skip=0, header=FALSE);
NoNode <- nrow(Data);

# initialization
IData <- array(data=NA, dim=c(NoNode, NoFile));

IData[,1] <- Data$V1;
# Read in the rest input files (beta)
for (m in 2:NoFile) {
   IData[,m] <- read.table(Model[m, FileCol], skip=0, header=FALSE)$V1;
}

Model$Beta<-IData[1,];
#Modeln <- groupedData(Beta ~ session | Subj, data = Model);

# No need for the 1st F, intercept
Stat <- array(data=NA, dim=c(NoNode, NoF));

#time1 <- format(Sys.time(), "%D %H:%M:%OS3");

format(Sys.time(), "%D %H:%M:%OS3")
#print(sprintf("Start to compute %s planes along X axis", dimx));
tag <- 0;
for (i in 1:NoNode) {

   if (all(IData[i,]==0)) {
	   Stat[i,] <- rep(0, NoF);
	} else {	
	Model$Beta<-IData[i,];
	try(fit.lme <- lme(as.formula(paste("Beta~", form)), random = ~1|Subj, Model), tag <- 1);
   if (tag != 1) {
	   Stat[i,] <- anova(fit.lme)$F[-1];
	}	else {
	   Stat[i,] <- rep(0, NoF);
	}
	tag <- 0;
	}
#print(sprintf("Finished dimX = %s", i));
#print(format(Sys.time(), "%D %H:%M:%OS3"));
#gc();
}
print(format(Sys.time(), "%D %H:%M:%OS3"));

dStat <- as.table(Stat);
hdr <- paste(rownames(anova(fit.lme))[-1], " F");
write.table(dStat, file = paste(Out, ".1D", sep=""), row.names = FALSE, col.names = hdr);

print(sprintf("Congratulations! You've got the result %s in text format!", Out));


} else {

print("Error: unrecognizable option on 1st line!");

}
}

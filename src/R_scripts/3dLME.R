#!/usr/bin/env afni_run_R
#Welcome to 3dLME.R, an AFNI Group Analysis Package!
#-----------------------------------------------------------
#Version 0.0.9, Feb. 26, 2007
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: http://afni.nimh.nih.gov/sscc/gangc/lme.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: R CMD BATCH $LME/3dLME.R

Ftype <- "sequential"   # "marginal" or "sequential"

source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#install.packages("nlme",dependencies=TRUE)
library(nlme)
library(contrast)

# Line 1: data type - volume or surface
datatype <- scan(file="model.txt", what= list(""), skip=0, nline=1)

#  Line 2: Output filename
#how to check output filename?
Out <-scan(file="model.txt", what= list(""), skip=1, nline=1)
OutFile <- paste(Out, "+orig", sep="")

# Line 3: MASK
mask <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=2, nline=1)), "\\:"))[2]

# Line 4: model formula
form <- scan(file="model.txt", what= list(""), skip=3, nline=1)
#nfixed <- length(strsplit(form[[1]], "\\*")[[1]])
nfixed <- length(unlist(strsplit(unlist(strsplit(form[[1]], "\\*")), "\\+")))

ModelForm <- as.formula(paste("Beta~", form))

# Line 5: covariates Cov is NA if no covariates
Cov <- unlist(strsplit(unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=4, nline=1)), "\\:"))[2], "\\*"))

# separate interaction terms
xterms <- strsplit(form[[1]], "\\+")[[1]]
covlist <- vector('list', length(Cov))  # list of those fixed factors each covariate interacts with
if (!is.na(Cov[1])) {  # is.na(Cov) doesn't work with more than 1 elements in Cov!
for (n in 1:length(Cov)) {
   # grep(Cov[n], xterms): indices of those interactions that contain each covariate	
	covlist[[n]] <- unique(unlist(strsplit(xterms[grep(Cov[n], xterms)], "\\*"))) 
	covlist[[n]] <- covlist[[n]][!covlist[[n]] %in% Cov]
}
}

# Line 6: SavedForRandomEffects

# Line 7 and the next few: pair-wise contrasts

hp <- grep("InputFile", readLines("model.txt")) # header position (hp) defined by column name InputFile
Model <- read.table("model.txt", skip=hp[1]-1, header=TRUE)
# More decent way to do this?
Model$Subj <-  as.factor(Model$Subj)
Model$InputFile <-  as.character(Model$InputFile)

# number of contrasts (pair-wise only right now)
ncontr <- (hp-7)%/%2

if (ncontr != 0) {
contrLabel <- array(data=NA, dim=ncontr)
contr      <- array(data=NA, dim=ncontr)
cc         <- array(data=NA, dim=c(ncontr, 2, nfixed))
clist      <- vector('list', ncontr)

for (n in 1:ncontr) {
   contrLabel[n] <- paste(unlist(scan(file="model.txt", what= list(""), skip=5+2*n-1, nline=1)), collapse="")
   contr[n]      <- scan(file="model.txt", what= list(""), sep="-", skip=5+2*n, nline=1)
	clist[[n]] <- vector('list', 2)
	for (ii in 1:2) cc[n, ii,] <- strsplit(contr[n][[1]][ii], "\\*")[[1]] # or unlist(strsplit(contr[n][[1]][ii], "\\*"))
	for (ii in 1:2) {  # component in n-th contrast
		# initialize the pair of list in clist
		sublist <- vector('list', nfixed)  # for the parameters
		for (jj in 1:nfixed) {  # jj-th fixed effect in ii-th component of n-th contrast  
         if(cc[n, ii, jj] == "0") { 
			   covp <- match(names(Model)[jj+1], Cov) # covp: covariate position index in Cov; match returns a vector of the positions of (first) matches
				if (is.na(covp)) sublist[[jj]] <- levels(Model[, jj+1])  # categorical factor, not a covariate 
				else {  # jj-th effect is a covariate: this part is so ungainly!
				   if (!identical(covlist[[covp]], character(0))) { # covariate interacts with some categorical factors. xxif (Cov[covp] %in% covlist[[covp]])
					   ave_log <- rep(TRUE, dim(Model)[1]) # initialization: TRUE because of logical AND
					   #ave_idx <- matrix(FALSE, nrow=dim(Model)[1], ncol=nfixed) # initialization: FALSE because of logical OR
						ave_idx <- matrix(FALSE, nrow=dim(Model)[1])
					   for (kk in 1:nfixed) {
							if( cc[n, ii, kk] != "0" ) ave_idx <- ave_idx | (Model[, kk+1]==cc[n, ii, kk])
					      ave_log <- ave_log & ave_idx
					   }
					   sublist[[jj]] <- mean(Model[ave_log, names(Model)[jj+1]])
					} else sublist[[jj]] <- mean(Model[, jj+1], rm.na=TRUE) # covariate doesn't interact with any categorical factors
				}         
			} else sublist[[jj]] <- cc[n, ii, jj]
      }
      names(sublist) <- names(Model)[2:(nfixed+1)]
      clist[[n]][[ii]] <- sublist
   }
}
}

# Assume the last column is input files
#FileCol <- length(colnames(Model))
FileCol <- dim(Model)[2]

#Number of input files
NoFile <- dim(Model[1])[1]

# Repeated-measures (use lme) or not (use lm)
if (length(unique(Model$Subj)) != length(Model$Subj)) RM <- TRUE else RM <- FALSE

if (datatype == "volume" | datatype == "Volume") {

# Read in the 1st input file so that we have the dimension information
Data <- read.AFNI(Model[1, FileCol])
dimx <- Data$dim[1]
dimy <- Data$dim[2]
dimz <- Data$dim[3]

if (is.na(mask)) { Mask <- array(1, c(dimx, dimy, dimz, 1)) 
   } else Mask <- read.AFNI(mask)$ttt

# initialization
IData <- array(data=NA, dim=c(dimx, dimy, dimz, NoFile))

IData[,,,1] <- Data$ttt
# Read in the rest input files (beta)
for (m in 2:NoFile) {
   IData[,,,m] <- read.AFNI(Model[m, FileCol])$ttt
}

# try out a solvable voxel, and find out the number of F tests and DF's for t tests (and catch potential problems as well)
ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3
Model$Beta<-IData[ii, jj, kk,]

contrDF <- array(data=NA, dim=ncontr)
tag<-1
while (tag == 1) {
   tag<-0
   if (RM) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- lm(ModelForm, Model), tag <- 1)
	if (ncontr != 0) try(for (n in 1:ncontr) { contrDF[n] <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average")$df }, tag <- 1)
   if (ii<dimx) ii<-ii+1 else {
      #print("Something is not quite right during testing!")
      break
   }
}

if (tag == 0)  {
   print("Good, test passed...")
   NoF <- nrow(anova(fm))-1  # Just assume an intercept
	#for (n in 1:ncontr) contrDF[n] <- temp[n]$df
   } else { 
      print("Something is not quite right during testing!") 
      break; next # won't run the whole brain analysis if the test fails
}

# F array in anova(fm) contains intercept in lme and residual in lm
if (RM) FArr <- 2:(NoF+1) else FArr <- 1:NoF  

# Number of statistics
NoStat <- NoF + 2*ncontr
#NoStat <- NoF


#Modeln <- groupedData(Beta ~ session | Subj, data = Model)

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoStat))

#time1 <- format(Sys.time(), "%D %H:%M:%OS3")

format(Sys.time(), "%D %H:%M:%OS3")
print(sprintf("Start to compute %s planes along X axis", dimx))
tag <- 0
for (i in 1:dimx) {
for (j in 1:dimy) {
for (k in 1:dimz) {
   if ((Mask[i, j, k,] != 0) & !all(IData[i, j, k,] == 0)) {	
	Model$Beta<-IData[i, j, k,]
	if (RM) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- lm(ModelForm, Model), tag <- 1) 
	if (tag != 1) {
	   Stat[i, j, k, 1:NoF] <- anova(fm, type=Ftype)$F[FArr]
		if (ncontr != 0) {
		   for (n in 1:ncontr) { 
		   con <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average") 
		   Stat[i, j, k, (NoF+2*n-1):(NoF+2*n)] <- c(con$Contrast, con$testStat)
	   }
		}
	}
	tag <- 0
	}
}
}
print(sprintf("Finished dimX = %s", i))
print(format(Sys.time(), "%D %H:%M:%OS3"))
#gc()
}

MyLabel <- paste(rownames(anova(fm))[FArr], " F")
if (ncontr != 0) {
for (n in 1:ncontr) {
   MyLabel <- append(MyLabel, contrLabel[n])
	MyLabel <- append(MyLabel, paste(contrLabel[n], "t"))
}
}
write.AFNI(OutFile, Stat, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, delta=Data$delta, idcode="whatever")
statpar <- "3drefit"
for (i in 2:(NoF+1)) {  # Assume an intercept
   # DFs are acquired from the last solvable voxel 
	if (RM) statpar <- paste(statpar, " -substatpar ", i-2, " fift ", anova(fm)$numDF[i], " ", anova(fm)$denDF[i])
	else statpar <- paste(statpar, " -substatpar ", i-2, " fift ", anova(fm)$Df[i-1], " ", fm$df.residual)
}
if (ncontr != 0) for (n in 1:ncontr) statpar <- paste(statpar, " -substatpar ", NoF+2*n-1, " fitt ", contrDF[n])

statpar <- paste(statpar, " -view tlrc -addFDR -newid ", OutFile)
system(statpar)
print(sprintf("Congratulations! You've got output %s+tlrc.*", Out))

} else {

if (datatype == "surface" | datatype == "Surface") {

# Read in the 1st input file so that we have the dimension information
# Data <- read.AFNI(Model[1, FileCol])
Data <- read.table(Model[1, FileCol], skip=0, header=FALSE)
NoNode <- nrow(Data)

# initialization
IData <- array(data=NA, dim=c(NoNode, NoFile))
IData[,1] <- Data$V1
# Read in the rest input files (beta)
for (m in 2:NoFile) {
   IData[,m] <- read.table(Model[m, FileCol], skip=0, header=FALSE)$V1
}

# try out a solvable voxel, and find out the number of F tests and DF's for t tests (and catch potential problems as well)
ii <- NoNode%/%3
Model$Beta<-IData[ii,]
contrDF <- array(data=NA, dim=ncontr)
tag<-1
while (tag == 1) {
   tag<-0
   if (RM) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- lm(ModelForm, Model), tag <- 1)
	if (ncontr != 0) try(for (n in 1:ncontr) { contrDF[n] <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average")$df }, tag <- 1)
   if (ii<NoNode%/%3+10) ii<-ii+1 else {
      #print("Something is not quite right during testing!")
      break
   }
}

if (tag == 0)  {
   print("Good, test passed... Starting to run node-wise analysis...")
   NoF <- nrow(anova(fm))-1  # Just assume an intercept
	#for (n in 1:ncontr) contrDF[n] <- temp[n]$df
   } else { 
      print("Something is not quite right during testing!") 
      break; next # won't run the whole brain analysis if the test fails
}

# F array in anova(fm) contains intercept in lme and residual in lm
if (RM) FArr <- 2:(NoF+1) else FArr <- 1:NoF  

# Number of statistics
NoStat <- NoF + 2*ncontr

Stat <- array(0, dim=c(NoNode, NoStat))

format(Sys.time(), "%D %H:%M:%OS3")
tag <- 0
for (i in 1:NoNode) {
   if (!all(IData[i,] == 0)) {	
	Model$Beta<-IData[i,]
	if (RM) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- lm(ModelForm, Model), tag <- 1)
	if (tag != 1) {
	   Stat[i,1:NoF] <- anova(fm, type=Ftype)$F[FArr]
		if (ncontr != 0) {
		   for (n in 1:ncontr) { 
		   con <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average") 
		   Stat[i, (NoF+2*n-1):(NoF+2*n)] <- c(con$Contrast, con$testStat)
		}
		}
	}
	tag <- 0
	}
}
print(format(Sys.time(), "%D %H:%M:%OS3"))
dStat <- as.table(Stat)
hdr <- paste(rownames(anova(fm))[FArr], " F")

if (ncontr != 0) for (ii in 1:ncontr) hdr <- c(hdr, contrLabel[ii], paste(contrLabel[ii], "t"))
write.table(dStat, file = paste(Out, ".1D", sep=""), row.names = FALSE, col.names = hdr)

print("")
print("Degrees of freedom for the statistics in the output are:")
print("========================================================")
print("F-statistic  numDF  denumDF")
print("---------------------------")
if (RM) for (ii in FArr) print(sprintf("%s F     %d    %d", rownames(anova(fm))[ii], anova(fm)$numDF[ii], anova(fm)$denDF[ii]))
else for (ii in FArr) print(sprintf("%s F     %d    %d", rownames(anova(fm))[ii], anova(fm)$numDf[ii], fm$df.residual))
print("---------------------------")
print("t-statistic  DF")
print("---------------------------")
if (ncontr != 0) for (ii in 1:ncontr) print(sprintf("%s t     %d", contrLabel[ii], contrDF[ii]))
print("========================================================")
print("")
print(sprintf("Congratulations! You've got the result %s in text format!", Out))

} else {
print("Error: unrecognizable option on 1st line!")
}
}

# set save defaults using option:
options(save.defaults=list(ascii=TRUE, safe=FALSE))
save.image()
unlink(".RData")

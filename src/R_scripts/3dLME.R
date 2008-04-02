#!/usr/bin/env afni_run_R
#Welcome to 3dLME.R, an AFNI Group Analysis Package!
#-----------------------------------------------------------
#Version 1.0.0,  March 5, 2008
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: http://afni.nimh.nih.gov/sscc/gangc/lme.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: 3dLME.R MyOutput &
# (Output is a file in which the running progress including 
# error messages will be stored)

system("rm -f .RData")
source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))
library(nlme)
library(contrast)

# Line 1: data type - volume or surface
datatype <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=0, nline=1)), "\\:"))[2]

#  Line 2: Output filename
#how to check output filename?
Out <-unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=1, nline=1)), "\\:"))[2]
OutFile <- paste(Out, "+orig", sep="")

# Line 3: MASK
mask <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=2, nline=1)), "\\:"))[2]

# Line 4: model formula
ModelShape <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=3, nline=1)), "\\:"))[2]
#nfixed <- length(strsplit(ModelShape[[1]], "\\*")[[1]])
nfixed <- length(unique(unlist(strsplit(unlist(strsplit(ModelShape[[1]], 
   "\\*")), "\\+"))))

ModelForm <- as.formula(paste("Beta~", ModelShape))
if (length(grep("-1", ModelShape)==1)>0) NoConst <- TRUE else NoConst <- FALSE

# Line 5: covariates Cov is NA if no covariates
Cov <- unlist(strsplit(unlist(strsplit(unlist(scan(file="model.txt", 
   what= list(""), skip=4, nline=1)), "\\:"))[2], "\\*"))
if (is.na(Cov)) nCov <- 0 else nCov <- length(Cov)  # number of covariates

# separate interaction terms
xterms <- strsplit(ModelShape[[1]], "\\+")[[1]]
covlist <- vector('list', nCov)  # list of those fixed factors each covariate interacts with
if (!is.na(Cov[1])) {  # is.na(Cov) doesn't work with more than 1 elements in Cov!
for (n in 1:nCov) {
   # grep(Cov[n], xterms): indices of those interactions that contain each covariate	
	covlist[[n]] <- unique(unlist(strsplit(xterms[grep(Cov[n], xterms)], "\\*"))) 
	covlist[[n]] <- covlist[[n]][!covlist[[n]] %in% Cov]
}
}

# Line 6: Random effect - lme or gls?
RanEff <- as.logical(unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=5, nline=1)), "\\:"))[2])

# Line 7: Variance structure for modeling dependence among within-subject errors
VarStr <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=6, nline=1)), "\\:"))[2]

# Line 8: Correlation structure for modeling dependence among within-subject errors
# 0 - nothing; 1 - corAR1; 2 - corARMA(2,0); 3 - corARMA(1,1)
CorTmp <- strsplit(unlist(scan(file="model.txt", what= list(""), skip=7, nline=1)), "\\:|~")
CorStr <- unlist(CorTmp)[2]
CorForm <- as.formula(paste("~", unlist(CorTmp)[3]))

# Line 9: type of sums of squares - "marginal" or "sequential"
Ftype <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=8, nline=1)), "\\:"))[2]

# Line 10 and the next few: pair-wise contrasts

# header position (hp) defined by column name InputFile
hp <- grep("InputFile", readLines("model.txt")) 
Model <- read.table("model.txt", skip=hp[1]-1, header=TRUE)
# More decent way to do this?
Model$Subj <-  as.factor(Model$Subj)
Model$InputFile <-  as.character(Model$InputFile)

# number of contrasts (pair-wise only right now)
ncontr <- (hp-10)%/%2

if (ncontr > 0) {
contrLabel <- array(data=NA, dim=ncontr)
contr      <- array(data=NA, dim=ncontr)
cc         <- array(data=NA, dim=c(ncontr, 2, nfixed))
clist      <- vector('list', ncontr)

# CAREFUL: assume contrasts at the same covariate value for multiple groups, 
# but is this desirable???
for (n in 1:ncontr) {
   contrLabel[n] <- paste(unlist(scan(file="model.txt", what= list(""), skip=8+2*n-1, nline=1)), collapse="")
   contr[n]      <- scan(file="model.txt", what= list(""), sep="-", skip=8+2*n, nline=1)
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
#if (length(unique(Model$Subj)) != length(Model$Subj)) RM <- TRUE else RM <- FALSE

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

# try out a solvable voxel, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3
Model$Beta<-IData[ii, jj, kk,]

if (ncontr > 0) contrDF <- array(data=NA, dim=ncontr)
tag<-1
while (tag == 1) {
   tag<-0
	if (RanEff) {
		if (CorStr == 0) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
		if (CorStr == 1) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corAR1(0.3, form=CorForm), Model), tag <- 1)
		if (CorStr == 2) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corARMA(0.3, p=2, form=CorForm), Model), tag <- 1)
		if (CorStr == 3) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corARMA(0.3, p=1, q=1, form=CorForm), Model), tag <- 1)		
	}	
	else try(fm <- gls(ModelForm, Model), tag <- 1)
	if (ncontr > 0) try(for (n in 1:ncontr) { contrDF[n] <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average")$df }, tag <- 1)
   if (ii<dimx) ii<-ii+1 else {
      #print("Something is not quite right during testing!")
      break
   }
}

if (tag == 0)  {
   print("Good, test passed...")
   if (NoConst) NoF <- nrow(anova(fm)) else NoF <- nrow(anova(fm))-1  # Just assume an intercept in the model
	#for (n in 1:ncontr) contrDF[n] <- temp[n]$df
   } else { 
      print("Something is not quite right during testing!") 
      break; next # won't run the whole brain analysis if the test fails
}

# F array in anova(fm) contains intercept (1st) in lme (and gls) and residual (last) in lm
# if (RM) FArr <- 2:(NoF+1) else FArr <- 1:NoF  
if (NoConst) FArr <- 1:NoF else FArr <- 2:(NoF+1)

# Number of statistics
NoBrick <- NoF + 2*ncontr + 2*nCov

#Modeln <- groupedData(Beta ~ session | Subj, data = Model)

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

#time1 <- format(Sys.time(), "%D %H:%M:%OS3")

print(format(Sys.time(), "%D %H:%M:%OS3"))
print(sprintf("Start to compute %s planes along X axis. You can monitor the progress", dimx))
print("and estimate the total run time by opening this file from time to time.")

tag <- 0
for (i in 1:dimx) {
for (j in 1:dimy) {
for (k in 1:dimz) {
   if ((Mask[i, j, k,] != 0) & !all(IData[i, j, k,] == 0)) {	
	Model$Beta<-IData[i, j, k,]
	if (RanEff) {
		if (CorStr == 0) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
		if (CorStr == 1) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corAR1(0.3, form=CorForm), Model), tag <- 1)
		if (CorStr == 2) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corARMA(0.3, p=2, form=CorForm), Model), tag <- 1)
		if (CorStr == 3) try(fm <- lme(ModelForm, random = ~1|Subj, 
		   correlation=corARMA(0.3, p=1, q=1, form=CorForm), Model), tag <- 1)		
	}	
   else try(fm <- gls(ModelForm, Model), tag <- 1) 
	if (tag != 1) {
	   Stat[i, j, k, 1:NoF] <- anova(fm, type=Ftype)$F[FArr]
		if (ncontr > 0) {
		   for (n in 1:ncontr) { 
		   con <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average") 
		   Stat[i, j, k, (NoF+2*n-1):(NoF+2*n)] <- c(con$Contrast, con$testStat)
	   }
		}
		if (nCov > 0) {
		   for (n in 1:nCov) {
			   Stat[i, j, k, (NoF+2*ncontr+2*n-1):(NoF+2*ncontr+2*n)] <- 
				c(summary(fm)$tTable[Cov[n], "Value"], summary(fm)$tTable[Cov[n], "t-value"])
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
if (ncontr > 0) {
for (n in 1:ncontr) {
   MyLabel <- append(MyLabel, contrLabel[n])
	MyLabel <- append(MyLabel, paste(contrLabel[n], "t"))
}
}
if (nCov > 0) {
for (n in 1:nCov) {
   MyLabel <- append(MyLabel, Cov[n])
	MyLabel <- append(MyLabel, paste(Cov[n], "t"))
}
}

write.AFNI(OutFile, Stat, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, 
   delta=Data$delta, idcode="whatever")
statpar <- "3drefit"
if (!RanEff) glsDF <- as.integer(gsub(" \n", "", gsub("Denom. DF: ", "", attr(anova(fm), "label"))))

#Index adjustment when no intercept 
IdxAdj <- as.integer(NoConst)
for (i in (2-IdxAdj):(NoF+1-IdxAdj)) {  # has an intercept or not
   # DFs are acquired from the last solvable voxel 
	if (RanEff) statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, 
	   " fift ", anova(fm)$numDF[i], " ", anova(fm)$denDF[i])
	else statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, " fift ", 1, " ", glsDF)
}
if (ncontr > 0) for (n in 1:ncontr) statpar <- paste(statpar, " -substatpar ", NoF+2*n-1, " fitt ", contrDF[n])
if (nCov > 0) {
   if (RanEff) for (n in 1:nCov) statpar <- paste(statpar, " -substatpar ", NoF+nCov+2*n-1, " fitt ",
	   summary(fm)$tTable[Cov[n], "DF"]) else
	   for (n in 1:nCov) statpar <- paste(statpar, " -substatpar ", NoF+nCov+2*n-1, " fitt ", glsDF)
}
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
   if (RanEff) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- gls(ModelForm, Model), tag <- 1)
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
FArr <- 2:(NoF+1)  

# Number of statistics
NoBrick <- NoF + 2*ncontr + 2*nCov

Stat <- array(0, dim=c(NoNode, NoBrick))

format(Sys.time(), "%D %H:%M:%OS3")
tag <- 0
for (i in 1:NoNode) {
   if (!all(IData[i,] == 0)) {	
	Model$Beta<-IData[i,]
	if (RanEff) try(fm <- lme(ModelForm, random = ~1|Subj, Model), tag <- 1)
   else try(fm <- gls(ModelForm, Model), tag <- 1)
	if (tag != 1) {
	   Stat[i,1:NoF] <- anova(fm, type=Ftype)$F[FArr]
		if (ncontr > 0) {
		   for (n in 1:ncontr) { 
		   con <- contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average") 
		   Stat[i, (NoF+2*n-1):(NoF+2*n)] <- c(con$Contrast, con$testStat)
		}
		}
		if (nCov > 0) {
		   for (n in 1:nCov) { 
		      Stat[i, j, k, (NoF+2*ncontr+2*n-1):(NoF+2*ncontr+2*n)] <- 
				c(summary(fm)$tTable[Cov[n],][1], summary(fm)$tTable[Cov[n],][3])
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
if (RanEff) for (ii in FArr) print(sprintf("%s F     %d    %d", rownames(anova(fm))[ii], anova(fm)$numDF[ii], anova(fm)$denDF[ii]))
else for (ii in FArr) print(sprintf("%s F     %d    %d", rownames(anova(fm))[ii], anova(fm)$numDf[ii], fm$df.residual))
print("---------------------------")
print("t-statistic  DF")
print("---------------------------")
if (ncontr > 0) for (ii in 1:ncontr) print(sprintf("%s t     %d", contrLabel[ii], contrDF[ii]))
if (nCov > 0) for (ii in 1:nCov) print(sprintf("%s t     %d", contrLabel[ii], contrDF[ii]))
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

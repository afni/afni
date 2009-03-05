#!/usr/bin/env afni_run_R
#Welcome to 3dLME.R, an AFNI Group Analysis Package!
#-----------------------------------------------------------
#Version 1.0.3,  Dec 11, 2008
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: http://afni.nimh.nih.gov/sscc/gangc/lme.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: 3dLME.R MyOutput &
# (Output is a file in which the running progress including 
# error messages will be stored)

libLoad <- function(myLib) {
   sucLoad <- FALSE
   sucCheck <- FALSE
   try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
   if (sucLoad) {print(sprintf("Package %s successfully loaded!", myLib)); sucCheck <- TRUE} else {
	  	try(install.packages(myLib))
      try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
      if (sucLoad) print(sprintf("Package %s successfully loaded...", myLib)) 
   	}
}

system("rm -f .RData")
source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))
libLoad("nlme")
libLoad("contrast")

# Line 1: data type - volume or surface
datatype <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=0, strip.white=TRUE, nline=1)), "\\:"))[2]
   
#  Line 2: Output filename
#how to check output filename?
Out <-unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=1, strip.white=TRUE, nline=1)), "\\:"))[2]
OutFile <- paste(Out, "+orig", sep="")

# Line 3: MASK
mask <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=2, strip.white=TRUE, nline=1)), "\\:"))[2]

# Line 4: model formula
ModelShape <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=3, strip.white=TRUE, nline=1)), "\\:"))[2]
#nfixed <- length(strsplit(ModelShape[[1]], "\\*")[[1]])
nfixed <- length(unique(unlist(strsplit(unlist(strsplit(ModelShape[[1]], 
   "\\*")), "\\+"))))

ModelForm <- as.formula(paste("Beta~", ModelShape))
if (length(grep("-1", ModelShape)==1)>0) NoConst <- TRUE else NoConst <- FALSE

# Line 5: covariates Cov is NA if no covariates
Cov <- unlist(strsplit(unlist(strsplit(unlist(scan(file="model.txt", 
   what= list(""), skip=4, strip.white=TRUE, nline=1)), "\\:"))[2], "\\*"))
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
ranEff <- vector("list", 2)
ranEff[[1]] <- unlist(strsplit(unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=5, strip.white=TRUE, nline=1)), "\\:"))[2], "\\,"))
#if (ranEff[[1]][1]) ranEff[[1]][1] <- 1  # simple for backward compatibility


nRand <- length(ranEff[[1]])
if (nRand==1 & as.numeric(ranEff[[1]][1])==1) ranEff[[2]] <- as.formula(~1|Subj)
if (nRand>1) {
	ranForm <- vector("list", nRand)		
	ranEff[[2]][[1]] <- ~1; 
	for (ii in 2:nRand) ranEff[[2]][[ii]] <- as.formula(sprintf("~%s-1",ranEff[[1]][ii]))
	ranEff[[2]] <- list(Subj=pdBlocked(lapply(ranEff[[2]], pdIdent)))
}	

# Line 7: Variance structure for modeling dependence among within-subject errors
VarStr <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=6, 
   strip.white=TRUE, nline=1)), "\\:"))[2]
# 0 - nothing; 1 - different variances across groups
VarTmp <- strsplit(unlist(scan(file="model.txt", what= list(""), skip=6, 
   strip.white=TRUE, nline=1)), "\\:|~")
VarStr <- unlist(VarTmp)[2]
VarForm <- as.formula(paste("~", unlist(VarTmp)[3]))

# Line 8: Correlation structure for modeling dependence among within-subject errors
# 0 - nothing; 1 - corAR1; 2 - corARMA(2,0); 3 - corARMA(1,1)
CorTmp <- strsplit(unlist(scan(file="model.txt", what= list(""), skip=7, 
   strip.white=TRUE, nline=1)), "\\:|~")
CorStr <- unlist(CorTmp)[2]
CorForm <- as.formula(paste("~", unlist(CorTmp)[3]))

# Line 9: type of sums of squares - "marginal" or "sequential"
Ftype <- unlist(strsplit(unlist(scan(file="model.txt", what= list(""), skip=8, 
   strip.white=TRUE, nline=1)), "\\:"))[2]

clusPos<-grep("Clusters", readLines("model.txt"))
if (length(clusPos)>0) nNodes<- as.integer(unlist(strsplit(unlist(scan(file="model.txt", what= list(""), 
   skip=clusPos-1, strip.white=TRUE, nline=1)), "\\:"))[2]) else nNodes<-1


# Line 10 and the next few: pair-wise contrasts


# header position (hp) defined by column name InputFile
hp <- grep("InputFile", readLines("model.txt")) 
Model <- read.table("model.txt", skip=hp[1]-1, header=TRUE)
# More decent way to do this?
Model$Subj <-  as.factor(Model$Subj)
Model$InputFile <-  as.character(Model$InputFile)

# number of contrasts (pair-wise only right now)
ncontr <- (hp-10)%/%2
clist      <- vector('list', ncontr)

if (ncontr > 0) {
contrLabel <- array(data=NA, dim=ncontr)
contr      <- array(data=NA, dim=ncontr)
cc         <- array(data=NA, dim=c(ncontr, 2, nfixed))

# CAREFUL: assume contrasts at the same covariate value for multiple groups, 
# but is this desirable???
for (n in 1:ncontr) {
   contrLabel[n] <- paste(unlist(scan(file="model.txt", what= list(""), skip=8+2*n-1, 
	   strip.white=TRUE, nline=1)), collapse="")
   contr[n]      <- scan(file="model.txt", what= list(""), sep="-", skip=8+2*n, 
	   strip.white=TRUE, nline=1)
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
inData <- read.AFNI(Model[1, FileCol])
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
myHist <- inData$header$HISTORY_NOTE; myOrig <- inData$origin; myDelta <- inData$delta

inData <- unlist(lapply(lapply(Model[,FileCol], read.AFNI), '[[', 1))
dim(inData) <- c(dimx, dimy, dimz, NoFile)

if (!is.na(mask)) {
	Mask <- read.AFNI(mask)$ttt[,,,1]
	inData <- array(apply(inData, 4, function(x) x*read.AFNI(mask)$ttt[,,,1]), dim=c(dimx,dimy,dimz,NoFile))
}

# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

if (ncontr > 0) contrDF <- array(data=NA, dim=ncontr)
tag<-1
while (tag == 1) {
   tag<-0
	Model$Beta<-inData[ii, jj, kk,]
	if (as.numeric(ranEff[[1]][1])!=0) {
		if (CorStr == 0) { if (VarStr == 0) {
		   try(fm <- lme(ModelForm, random = ranEff[[2]], Model), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = ranEff[[2]], weights=varIdent(VarForm), Model), tag <- 1) } }
			
		if (CorStr == 1) { if (VarStr == 0) {
		   try(fm <- lme(ModelForm, random = ranEff[[2]], 
		   correlation=corAR1(0.3, form=CorForm), Model), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = ranEff[[2]], correlation=corAR1(0.3, form=CorForm),
		   weights=varIdent(VarForm), Model), tag <- 1) } }
			
		if (CorStr == 2) { if (VarStr == 0) {
		   try(fm <- lme(ModelForm, random = ranEff[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=2, form=CorForm), Model), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = ranEff[[2]], 
			correlation=corARMA(c(0.3,0.3), p=2, form=CorForm),
			weights=varIdent(VarForm), Model), tag <- 1) } }
			
		if (CorStr == 3) { if (VarStr == 0) {
		   try(fm <- lme(ModelForm, random = ranEff[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=1, q=1, form=CorForm), Model), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = ranEff[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=1, q=1, form=CorForm),
			weights=varIdent(VarForm), Model), tag <- 1) } }
	}	
	if (nRand==1 & as.numeric(ranEff[[1]][1])==0) try(fm <- gls(ModelForm, Model), tag <- 1)
				
	if (ncontr > 0) try(for (n in 1:ncontr) { contrDF[n] <- 
	   contrast(fm, clist[[n]][[1]], clist[[n]][[2]], type="average")$df }, tag <- 1)
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
      print(sprintf("Insoluble model! Formula %s on the 4th line in model.txt", ModelShape))
		print("might be inappropriate, or there are incorrect specifications in model.txt")
		print("such as contrasts, factor levels, or other obscure problems.")
      break; next # won't run the whole brain analysis if the test fails
}

# F array in anova(fm) contains intercept (1st) in lme (and gls) and residual (last) in lm
# if (RM) FArr <- 2:(NoF+1) else FArr <- 1:NoF  
if (NoConst) FArr <- 1:NoF else FArr <- 2:(NoF+1)

# Number of sub-bricks
NoBrick <- NoF + 2*ncontr + 2*nCov
if (NoConst) {
   if (as.numeric(ranEff[[1]][1])) NoCoef <- length(fm$coefficients$fixed) else {
	   NoCoef <- length(fm$coefficients)
	}
	NoBrick <- NoBrick + 2*NoCoef	
}	else NoCoef <- NA
# to avoid the ambiguity with the case of no intercept	
BrickCnt <- NoBrick - 2*ncontr - 2*nCov  

#Modeln <- groupedData(Beta ~ session | Subj, data = Model)

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

#time1 <- format(Sys.time(), "%D %H:%M:%OS3")

print(format(Sys.time(), "%D %H:%M:%OS3"))
print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time by opening this file from time to time.")

myRand <- vector("list", 6)
myRand[[1]]<-ranEff[[1]]; myRand[[2]]<-ranEff[[2]]; myRand[[3]]<-CorStr; myRand[[4]]<-CorForm; myRand[[5]]<-VarStr; myRand[[6]]<-VarForm
myStuff <- vector("list", 10)
myStuff[[1]]<-NoBrick; myStuff[[2]]<-BrickCnt; myStuff[[3]]<-NoCoef; myStuff[[4]]<-ncontr
myStuff[[5]]<-clist; myStuff[[6]]<-nCov; myStuff[[7]]<-Ftype; myStuff[[8]]<-FArr; myStuff[[9]]<-NoF
myStuff[[10]]<-NoConst; myStuff[[11]]<-Cov

runAna <- function(inData, dataframe, ModelForm, myRand, myStuff, tag) {
	Stat <- vector(mode="numeric", length=myStuff[[1]])
	if (!all(inData == 0)) {	
	   dataframe$Beta<-inData
	if (as.numeric(myRand[[1]][1])!=0) {
		if (myRand[[3]] == 0) { if (myRand[[5]] == 0) {
		   try(fm <- lme(ModelForm, random = myRand[[2]], dataframe), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = myRand[[2]], weights=varIdent(myRand[[6]]), dataframe), tag <- 1) } }			
		if (myRand[[3]] == 1) { if (myRand[[5]] == 0) {
		   try(fm <- lme(ModelForm, random = myRand[[2]], 
		   correlation=corAR1(0.3, form=myRand[[4]]), dataframe), tag <- 1) } else {
			try(fm <- FUNC(ModelForm, random = myRand[[2]], correlation=corAR1(0.3, form=myRand[[4]]),
		   weights=varIdent(myRand[[6]]), dataframe), tag <- 1) } }			
		if (myRand[[3]] == 2) { if (myRand[[5]] == 0) {
		   try(fm <- lme(ModelForm, random = myRand[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=2, form=myRand[[4]]), dataframe), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = myRand[[2]], 
			correlation=corARMA(c(0.3,0.3), p=2, form=myRand[[4]]),
			weights=varIdent(myRand[[6]]), dataframe), tag <- 1) } }			
		if (myRand[[3]] == 3) { if (myRand[[5]] == 0) {
		   try(fm <- lme(ModelForm, random = myRand[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=1, q=1, form=myRand[[4]]), dataframe), tag <- 1) } else {
			try(fm <- lme(ModelForm, random = myRand[[2]], 
		   correlation=corARMA(c(0.3,0.3), p=1, q=1, form=myRand[[4]]),
			weights=varIdent(myRand[[6]]), dataframe), tag <- 1) } }
	}
	if (length(myRand[[1]])==1 & as.numeric(myRand[[1]][1])==0) try(fm <- gls(ModelForm, dataframe), tag <- 1)    
	if (tag != 1) {
	   if (myStuff[[9]] > 0) Stat[1:myStuff[[9]]] <- anova(fm, type=myStuff[[7]])$F[myStuff[[8]]]
		if (myStuff[[10]]) 
		   if (as.numeric(myRand[[1]][1])) {
			   # or unname(summary(fm)$tTable[, "Value"])
				Stat[myStuff[[9]]+2*0.5:myStuff[[3]]] <- unname(fm$coefficients$fixed) 
			   Stat[myStuff[[9]]+2*1:myStuff[[3]]] <- unname(summary(fm)$tTable[,"t-value"])
			} else {
			   Stat[myStuff[[9]]+2*0.5:myStuff[[3]]] <- unname(fm$coefficients)
				Stat[myStuff[[9]]+2*1:myStuff[[3]]] <- unname(summary(fm)$tTable[,"t-value"])
		   }

		if (myStuff[[4]] > 0) {
		   for (n in 1:myStuff[[4]]) { 
		   con <- contrast(fm, myStuff[[5]][[n]][[1]], myStuff[[5]][[n]][[2]], type="average") 
		   Stat[(myStuff[[2]]+2*n-1):(myStuff[[2]]+2*n)] <- c(con$Contrast, con$testStat)
	   }
		}
		if (myStuff[[6]] > 0) {
		   for (n in 1:myStuff[[6]]) {
			   Stat[(myStuff[[2]]+2*myStuff[[4]]+2*n-1):(myStuff[[2]]+2*myStuff[[4]]+2*n)] <- 
				c(summary(fm)$tTable[myStuff[[11]][n], "Value"], summary(fm)$tTable[myStuff[[11]][n], "t-value"])
			}
		}
	}
	}
	return(Stat)	
}

if (nNodes==1) for (kk in 1:dimz) {
#   library(nlme); library(contrast)
	Stat[,,kk,] <-aperm(apply(inData[,,kk,], c(1,2), runAna, dataframe=Model, ModelForm=ModelForm, myRand=myRand, myStuff=myStuff, tag=0), c(2,3,1))
   cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 

if (nNodes>1)	 {
   library(snow)
   cl <- makeCluster(nNodes, type = "SOCK")
	clusterEvalQ(cl, library(nlme)); clusterEvalQ(cl, library(contrast))
   for (kk in 1:dimz) {
      Stat[,,kk,] <-aperm(parApply(cl, inData[,,kk,], c(1,2), runAna, dataframe=Model, ModelForm=ModelForm, myRand=myRand, myStuff=myStuff, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}

MyLabel <- paste(rownames(anova(fm))[FArr], " F")
if (NoConst) {  # RanEff or not
   for (n in 1:dim(summary(fm)$tTable)[1]) {
	   MyLabel <- append(MyLabel, rownames(summary(fm)$tTable)[n])
		MyLabel <- append(MyLabel, paste(rownames(summary(fm)$tTable)[n], "t"))
	}
}		
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

write.AFNI(OutFile, Stat, MyLabel, note=myHist, origin=myOrig, delta=myDelta, idcode="whatever")
statpar <- "3drefit"
if (!as.numeric(ranEff[[1]][1])) glsDF <- as.integer(gsub(" \n", "", gsub("Denom. DF: ", "", attr(anova(fm), "label"))))

#Index adjustment when no intercept 
IdxAdj <- as.integer(NoConst)
for (i in (2-IdxAdj):(NoF+1-IdxAdj)) {  # has an intercept or not
   # DFs are acquired from the last solvable voxel 
	if (as.numeric(ranEff[[1]][1])) statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, 
	   " fift ", anova(fm)$numDF[i], " ", anova(fm)$denDF[i])
	else statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, " fift ", 1, " ", glsDF)
}  # from 0 to NoF-1

if (NoConst) {
   if (as.numeric(ranEff[[1]][1])) { 
		   for (n in 1:dim(summary(fm)$tTable)[1]) {
		      statpar <- paste(statpar, " -substatpar ", NoF+2*n-1, " fitt ", summary(fm)$tTable[n,"DF"])
			}
		} else for (n in 1:dim(summary(fm)$tTable)[1]) {
		   statpar <- paste(statpar, " -substatpar ", NoF+2*n-1, " fitt ", glsDF)
		}
}	# from NoF to BrickCnt-1

if (ncontr > 0) for (n in 1:ncontr) statpar <- paste(statpar, " -substatpar ", BrickCnt+2*n-1, " fitt ", contrDF[n])
if (nCov > 0) {
   if (as.numeric(ranEff[[1]][1])) for (n in 1:nCov) statpar <- paste(statpar, " -substatpar ", BrickCnt+2*ncontr+2*n-1, " fitt ",
	   summary(fm)$tTable[Cov[n], "DF"]) else
	   for (n in 1:nCov) statpar <- paste(statpar, " -substatpar ", BrickCnt+2*ncontr+2*n-2, " fitt ", glsDF)
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
   if (as.numeric(ranEff[[1]][1])) try(fm <- lme(ModelForm, random = ranEff[[2]], Model), tag <- 1)
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
	if (as.numeric(ranEff[[1]][1])) try(fm <- lme(ModelForm, random = ranEff[[2]], Model), tag <- 1)
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
if (as.numeric(ranEff[[1]][1])) for (ii in FArr) print(sprintf("%s F     %d    %d", rownames(anova(fm))[ii], anova(fm)$numDF[ii], anova(fm)$denDF[ii]))
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

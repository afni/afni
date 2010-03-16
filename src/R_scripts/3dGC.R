#!/usr/bin/env AFNI_Batch_R

print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 3dGC.R ==================          ")
print("AFNI Bivariate Auto-Regressive Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.7,  Feb. 1, 2010")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website: http://afni.nimh.nih.gov/sscc/gangc/3dGC.html")
print("SSCC/NIMH, National Institutes of Health, Bethesda MD 20892")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

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

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))

#=============

#libLoad('tcltk')    # for graphics

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("Visit http://afni.nimh.nih.gov/sscc/gangc/3dGC.html and makse sure")
print("you've acquired the data for the analysis in desirable data format.")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("################################################################")
print("Please consider citing the following if this program is useful for you:")
cat("\n\tGang Chen, J. Paul Hamilton, Moriah E. Thomason, Ian H. Gotlib, Ziad S. Saad\n")
cat("\tRobert W. Cox, Granger causality via vector auto-regression (VAR) attuned for\n")
cat("\tFMRI data analysis. ISMRM 17th Scientific Meeting, Hawaii, 2009.\n\n")
print("################################################################")

print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")
anaType <- as.integer(readline("Analysis type (0: quit; 1: individual)? "))

if (anaType==1) {
libLoad("vars")     # VAR modeling 

plotTS <- function(dataFrame, nCurves, msg) {
   if (nCurves <= 5) {
      dev.new(); par(mfrow=c(nCurves, 1))
	   for (ii in 1:nCurves) {
	      plot(dataFrame[,ii], ann=FALSE, axes=TRUE)
		   if (ii==1) title(msg)
		   lines(dataFrame[,ii])
		   mtext(sprintf("%s", names(dataFrame)[ii]), side=2, line=2)
      }
		mtext("time", side=1, line=2.5)
	} else for (ii in 1:nCurves) {
		   dev.new()
		   plot(dataFrame[,ii], ann=FALSE, axes=TRUE)
			title(msg)
			lines(dataFrame[,ii])
			mtext(sprintf("%s", names(dataFrame)[ii]), side=2, line=2)
			mtext("time", side=1, line=2.5)
	}
}

sNum <- 10e-9

anotherAna <- 1
while (anotherAna==1) {

# Set most paratmers here
maxLags <- 8


inFN <- readline("Subject 3d+time file name (suffix unnecessary,e.g., test+tlrc): ")
if (length(grep('tlrc', inFN))==1) outView <- "tlrc"
if (length(grep('orig', inFN))==1) outView <- "orig"

#source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))
masked <- as.integer(readline("Any mask (0: no; 1: yes)? "))
if (masked) {maskFN <- readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); maskData <- read.AFNI(maskFN)$brk}

inDataTS <- read.AFNI(inFN)
myNote=inDataTS$header$HISTORY_NOTE; myOrig=inDataTS$origin; myDelta=inDataTS$delta

dataOrient <- system(sprintf("@GetAfniOrient %s", inFN), intern = TRUE)

inDataTS <- inDataTS$brk
dimx <- dim(inDataTS)[1]; dimy <- dim(inDataTS)[2]; dimz <- dim(inDataTS)[3]; nT <- dim(inDataTS)[4]


print("#++++++++++++++++++++++++++++++++++++++++++++")
print("If there are n consecutive chunks of data, enter n-1 here.")
print("If all the time series are consecutive, enter 0 breaks.")
nChunks <- as.integer(readline("Number of breaks in the time series? "))+1     # number of runs

nPts <- array(data = NA, dim=nChunks)
if (nChunks == 1) nPts[1] <- nT else
for (ii in 1:nChunks) nPts[ii] <- as.integer(readline(paste("Length of number", ii, "run/segment? ")))

print("#++++++++++++++++++++++++++++++++++++++++++++")

sdFN <- readline("Seed time series file name (in one-column format, e.g., seed.1D): ")
sdTS <- read.table(sdFN, header=FALSE)
# plot out the seed time series, and let the user make sure it looks OK
plotSdTS <- as.integer(readline("Plot out the seed time series (0: no; 1: yes)? "))
#print("Check the fitting for each ROI time series")
if (plotSdTS) plotTS(sdTS, 1, "Seed time series")

print("#++++++++++++++++++++++++++++++++++++++++++++")
print("Prior detrending is NOT recommended due to potential complications. ")
print("Trend can be modeled through specifying the order of a polynomial here, ")
print("or can be included as part of covariates later on. If you plan to model the")
print("trend with your own regressors or don't need to model it, choose -1 here.")
print("If trend has already been removed (not recommended), choose 0 here.")
nPoly <- as.integer(readline("Order of polynomials for drifting effects (note: -1 means no trend removal!)? "))  # Legendre

print("#++++++++++++++++++++++++++++++++++++++++++++")
COV <- as.integer(readline("Any other covariates or confounding effects than drift (0: no; 1: yes)? ")) 
if (as.logical(COV)) {
   nCOVs <- as.integer(readline("Number of covariates (e.g., 6)? "))     # number of regions: 6
	print("Header with one line of labels is optional in multi-column files, but NOT allowed in one-column files.")
	covForm <- as.integer(readline("Covariates data type (0: MULTIPLE one-column files; 1: ONE multi-column file)? "))     # covariates format
   if (covForm) {
      fncov <- readline("Covariates file name: ")
		covHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? "))
		if (covHeader == 1) exData <- read.table(fncov, header=TRUE) else {
         exData <- read.table(fncov, header=FALSE)
         for (ii in 1:nCOVs) names(exData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
      }
   } else {
      exData <- data.frame(matrix(data=NA, nrow=nT, ncol=nCOVs, dimnames = NULL))
      exData <- readMultiFiles(nCOVs, 1, exData)
			
      for (ii in 1:nCOVs) names(exData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
   }
} else {exData <- NULL; nCOVs <- 0}

if (as.logical(COV)) {
   plotCov <- as.integer(readline("Plot out covariates time series provided by you (0: no; 1: yes)? "))
   if (plotCov) plotTS(exData, nCOVs, "Covariate time series")
} # if (as.logical(COV))
print("-----------------")

# scaling the data per run
#newData <- array(0, dim = c(dimx, dimy, dimz, nT))
#sdTSs <- vector(, length = nT)
#print(sprintf("If normalization was NOT performed during pre-processing, you can scale the data now."))
#scaleTS <- as.integer(readline("Scale the ROI time series (0: no; 1: yes)? "))
#if (scaleTS) {
#   jumpPts <- 0
# Could the following be vectorized?????	
#	for (mm in 1:nChunks) {
#      for (ii in 1:dimx) for (jj in 1:dimy) for (kk in 1:dimz) {
#			ave <- mean(inDataTS[ii,jj,kk,(jumpPts+1):(jumpPts+nPts[mm])])
#			if (abs(ave)>sNum) newData[ii,jj,kk,(jumpPts+1):(jumpPts+nPts[mm])] <- 
#         inDataTS[ii,jj,kk,(jumpPts+1):(jumpPts+nPts[mm])]/ave 
#	   }
#	   ave <- mean(sdTS[(jumpPts+1):(jumpPts+nPts[mm])])
#	   if (abs(ave)>sNum) sdTSs[(jumpPts+1):(jumpPts+nPts[mm])] <- sdTS[(jumpPts+1):(jumpPts+nPts[mm])]/ave
#	   if (mm < nChunks) jumpPts <- jumpPts+nPts[mm]
#	}	
#}

# create exogenous variables with Legendre polynomials from gsl
if (nPoly > -1) {
   libLoad("gsl")      # Legendre polynomials
   trendMat <- as.data.frame(array(0, dim = c(nT, (nPoly+1)*nChunks)))
   jumpPts <- 0
   for (ii in 1:nChunks) {
	  trendMat[(jumpPts+1):(jumpPts+nPts[ii]),(1+(nPoly+1)*(ii-1)):((nPoly+1)*ii)] <- 
	      t(legendre_Pl_array(nPoly, seq(from=-1,to=1,len=nPts[ii])))    
      names(trendMat)[(1+(nPoly+1)*(ii-1)):((nPoly+1)*ii)] <- sprintf("Run%iTrend%i", ii, seq(nPoly+1)-1)
      if (ii < nChunks) jumpPts <- jumpPts+nPts[ii]
   }
   if (is.null(exData)) exMat <- trendMat else exMat <- cbind(trendMat, exData)
} else exMat <- exData # if no baseline and trend, do nothing
# plot out those polynomials here???

anotherLag <- TRUE
while (anotherLag) {

#print(sprintf("Select the order of VAR model:"))
nLags <- as.integer(readline("Select order of VAR model (e.g., 1)? "))

outFN <- readline("Output file name (no view+suffix needed, e.g., myOutput): ")
outFN <- paste(outFN, "+orig", sep="")

# generate intervention dummy variables for across-run/block breaks: nLags dummies per run
if (nChunks > 1) {
	breakMat <- as.data.frame(array(0, dim = c(nT, (nChunks-1)*nLags)))
	jumpPts <- 0
	for (ii in 1:(nChunks-1)) {
		jumpPts <- jumpPts+nPts[ii]
		for (jj in 1:(nLags)) {
		   breakMat[,(ii-1)*nLags+jj] <- c(rep(0, jumpPts+jj-1), 1, rep(0, nT-jumpPts-jj))
		   names(breakMat)[(ii-1)*nLags+jj] <- sprintf("Run%iLag%i", ii, jj)
		}   
	}
	if (is.null(exMat)) exMatMod <- breakMat else exMatMod <- cbind(breakMat, exMat)
} else exMatMod <- exMat
print("#++++++++++++++++++++++++++++++++++++++++++++")

nBrick <- 6*nLags
#outData <- array(0, dim=c(nBrick, dimx, dimy, dimz))
outData <- array(0, dim=c(dimx, dimy, dimz, nBrick))

# mask and all 0's!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#for (ii in 1:dimx) for (jj in 1:dimy) for (kk in 1:dimz) {
#   if ((maskData[ii, jj, kk,1] != 0) & !all(newData[ii, jj, kk,] == 0)) {
#   tmpData <- cbind(newData[ii, jj, kk,], sdTS)  
#   fm <- VAR(tmpData, p=nLags, type="none", exogen=exMatMod)
#   for (mm in 1:nLags)  {
#	   outData[ii,jj,kk,1+6*(mm-1)] <- coef(fm)[[1]][1,1]   # target to target
#	   outData[ii,jj,kk,2+6*(mm-1)] <- coef(fm)[[1]][1,3]   # t
#	   outData[ii,jj,kk,3+6*(mm-1)] <- coef(fm)[[1]][2,1]   # seed to target
#	   outData[ii,jj,kk,4+6*(mm-1)] <- coef(fm)[[1]][2,3]   # t
#	   outData[ii,jj,kk,5+6*(mm-1)] <- coef(fm)[[2]][1,1]   # target to seed
#	   outData[ii,jj,kk,6+6*(mm-1)] <- coef(fm)[[2]][1,3]   # t
#	}
#	}
#}

ii<-max(dimx%/%3, 1); jj<-max(dimy%/%3, 1); kk<-max(dimz%/%3, 1)

tag <- 1
while (tag == 1) {
#	if ((maskData[ii, jj, kk,1] != 0) & !all(inDataTS[ii, jj, kk,] == 0)) {
   if (ifelse(as.logical(masked), (maskData[ii, jj, kk,1] != 0) & !all(inDataTS[ii, jj, kk,] == 0),
      !all(inDataTS[ii, jj, kk,] == 0))) {
		tag<-0
		tmpData <- cbind(inDataTS[ii, jj, kk,], sdTS)
		try(fm <- VAR(tmpData, p=nLags, type="none", exogen=exMatMod), tag<-1)
      if(tag == 0) try(summary(fm), tag<-1) # in case something goes awry (e.g., running out of DFs)
#		if (ii<dimx) ii<-ii+1 else break
	}
   if(ii<dimx) ii<-ii+1 else break	
}

if (tag == 0)  {
   print("Good, test passed...")
   } else { 
      print("Something is not quite right. Quitting...")
      break; next # won't run the whole brain analysis if the test fails
}

#if (as.logical(masked)) inDataTS <- array(apply(inDataTS, 4, function(x) x*maskData[,,,1]), dim=c(dimx,dimy,dimz,nT))

# do it one slice at a time due to memory issue: 2/26/2009
if (as.logical(masked)) for (kk in 1:dimz) 
   inDataTS[,,kk,] <- array(apply(inDataTS[,,kk,], 3, function(x) x*maskData[,,kk,1]), dim=c(dimx,dimy,nT))


# define the core function
runVAR<-function(inDataTS, sdTS, exMatMod, nLags) {
   outData<-vector(mode="numeric", length=6*nLags) 
   if (!all(inDataTS == 0)) {
   tmpData <- cbind(inDataTS, sdTS)  
   fm <- VAR(tmpData, p=nLags, type="none", exogen=exMatMod)
   for (mm in 1:nLags)  {
	   outData[1+6*(mm-1)] <- coef(fm)[[1]][1+2*(mm-1),1]   # target to target
	   outData[2+6*(mm-1)] <- coef(fm)[[1]][1+2*(mm-1),3]   # t
	   outData[3+6*(mm-1)] <- coef(fm)[[1]][2+2*(mm-1),1]   # seed to target
	   outData[4+6*(mm-1)] <- coef(fm)[[1]][2+2*(mm-1),3]   # t
	   outData[5+6*(mm-1)] <- coef(fm)[[2]][1+2*(mm-1),1]   # target to seed
	   outData[6+6*(mm-1)] <- coef(fm)[[2]][1+2*(mm-1),3]   # t
      # I'm ignoring the seed to seed effect here because it's not practical to store it
	}
	}
   return(outData)
}

nNodes <- as.integer(readline("Number of parallel jobs for the running (e.g., 2)? "))

print(sprintf("Start to run analysis on %i Z slices: %s", dimz, format(Sys.time(), "%D %H:%M:%OS3")))
print(sprintf("You can monitor the progress and estimate the total run time below:"))

if (nNodes==1) for (kk in 1:dimz) {
   outData[,,kk,] <-aperm(apply(inDataTS[,,kk,], c(1,2), runVAR, sdTS=sdTS, exMatMod=exMatMod, nLags=nLags), c(2,3,1))
   cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
}
	
if (nNodes>1)	 {
   library(snow)
   #cl <- makeCluster(rep('locahost', nNodes), type = "SOCK")
   cl <- makeCluster(nNodes, type = "SOCK")
	clusterEvalQ(cl, library(vars))
   for (kk in 1:dimz) {
      outData[,,kk,] <-aperm(parApply(cl, inDataTS[,,kk,], c(1,2), runVAR, sdTS=sdTS, exMatMod=exMatMod, nLags=nLags), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}

print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))

print("#++++++++++++++++++++++++++++++++++++++++++++")

# spill out the original path matrix with direction going from rows to columns

outLabel <- NULL
for (ii in 1:nLags) {
   outLabel <- append(outLabel, sprintf("self-effect lag %i", ii))
   outLabel <- append(outLabel, sprintf("t for self-effect lag %i", ii))
   outLabel <- append(outLabel, sprintf("seed-to-target lag %i", ii))
   outLabel <- append(outLabel, sprintf("t for seed-to-target lag %i", ii))
   outLabel <- append(outLabel, sprintf("target-to-seed lag %i", ii))
   outLabel <- append(outLabel, sprintf("t for target-to-seed lag %i", ii))
}
 
write.AFNI(outFN, outData, outLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")

statpar <- "3drefit"
   for (ii in 1:nLags) for (jj in 1:3) {
	   statpar <- paste(statpar, " -substatpar ", 6*(ii-1)+2*jj-1, 
	   " fitt ", summary(fm)$varresult[[1]]$df[2])
	}
if (outView=="tlrc") statpar <- paste(statpar, " -view tlrc -addFDR -newid -orient ", dataOrient, outFN)
if (outView=="orig") statpar <- paste(statpar, "-addFDR -newid -orient ", dataOrient, outFN)
system(statpar)


print("#++++++++++++++++++++++++++++++++++++++++++++")
anotherLag <- as.integer(readline("Want to try another number of lags for VAR (0: no; 1: yes)? "))
}
print("#++++++++++++++++++++++++++++++++++++++++++++")

#causality(fm, cause=names(myData)[1])

anotherAna <- as.integer(readline("Next (0: quit; 1: another individual analysis; 2: group analysis)? "))
} # while (anotherAna==1)
rm(inDataTS)  # retrieve some memory

} # if (anaType==1)

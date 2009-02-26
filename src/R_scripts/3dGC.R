print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 3dGC.R ==================          ")
print("AFNI Bivariate Auto-Regressive Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.6,  Feb. 26, 2009")
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

#=============

read.AFNI <- function(filename) {
  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- tolower(fileparts[length(fileparts)])

  if (ext == "head") {
    filename.head <- filename
    filename.brik <- paste(c(fileparts[-length(fileparts)],"BRIK"),collapse=".")
  } else if (ext == "brik") {
    filename.head <- paste(c(fileparts[-length(fileparts)],"HEAD"),collapse=".")
    filename.brik <- filename
  } else {
    filename.head <- paste(filename,".HEAD",sep="")
    filename.brik <- paste(filename,".BRIK",sep="")
  }
  
  conhead <- file(filename.head,"r")
  header <- readLines(conhead)
  close(conhead)

  types <- NULL
  args <- NULL
  counts <- NULL
  values <- NULL
  
  for (i in 1:length(header)) {
    if (regexpr("^type *= *", header[i]) != -1) {
      tmptype <- strsplit(header[i]," *= *")[[1]][2]
      types <- c(types,tmptype)
      args <- c(args,strsplit(header[i+1]," *= *")[[1]][2])
      tmpcounts <- as.numeric(strsplit(header[i+2]," *= *")[[1]][2])
      counts <- c(counts,tmpcounts)
      i <- i+3
      tmpvalue <- ""
      while ((regexpr("^$", header[i]) == -1) && (i <= length(header))) {
        tmpvalue <- paste(tmpvalue,header[i])
        i <- i+1
      }
      tmpvalue <- sub("^ +","",tmpvalue)
      if ((tmptype == "integer-attribute") || (tmptype == "float-attribute")) {
        tmpvalue <- as.numeric(strsplit(tmpvalue," +")[[1]])
      }
      values <- c(values,list(value=tmpvalue))
    }        
  }

  names(values) <- args

  dx <- values$DATASET_DIMENSIONS[1]
  dy <- values$DATASET_DIMENSIONS[2]
  dz <- values$DATASET_DIMENSIONS[3]
  dt <- values$DATASET_RANK[2]
  scale <- values$BRICK_FLOAT_FACS
  size <- file.info(filename.brik)$size/(dx*dy*dz*dt)

  if (regexpr("MSB",values$BYTEORDER_STRING[1]) != -1) {
    endian <- "big"
  } else {
    endian <- "little"
  }

  if (min(abs(values$DELTA)) != 0) {
    weights <-
      abs(values$DELTA/min(abs(values$DELTA)))
  } else {
    weights <- NULL
  }
#  browser()
  if (as.integer(size) == size) {
    conbrik <- file(filename.brik,"rb")
  # modified below by GC 12/2/2008
  if (all(values$BRICK_TYPES==0) | all(values$BRICK_TYPES==1)) myttt<- readBin(conbrik, "int", n=dx*dy*dz*dt*size, size=size, signed=TRUE, endian=endian) # unsigned charater or short
  if (all(values$BRICK_TYPES==3)) myttt<- readBin(conbrik, "numeric", n=dx*dy*dz*dt*size, size=size, signed=TRUE, endian=endian) # float        
    close(conbrik)
    dim(myttt) <- c(dx,dy,dz,dt)
#    for (k in 1:dt) {
#      if (scale[k] != 0) {
#        cat("scale",k,"with",scale[k],"\n")
#        cat(range(myttt[,,,k]),"\n")
#        myttt[,,,k] <- scale[k] * myttt[,,,k]
#        cat(range(myttt[,,,k]),"\n")
#      }
#    }
    for (k in 1:dt) if (scale[k] != 0) myttt[,,,k] <- scale[k] * myttt[,,,k]

  mask <- array(TRUE,c(dx,dy,dz))
  mask[myttt[,,,1] < quantile(myttt[,,,1],0.75)] <- FALSE
    z <-
      list(ttt=myttt,format="HEAD/BRIK",delta=values$DELTA,origin=values$ORIGIN,orient=values$ORIENT_SPECIFIC,dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)
#      list(ttt=writeBin(as.numeric(myttt),raw(),4),format="HEAD/BRIK",delta=values$DELTA,origin=values$ORIGIN,orient=values$ORIENT_SPECIFIC,dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)

  } else {
    warning("Error reading file: Could not detect size per voxel\n")
    z <- list(ttt=NULL,format="HEAD/BRIK",delta=NULL,origin=NULL,orient=NULL,dim=NULL,weights=NULL,header=values,mask=NULL)    
  }

  class(z) <- "fmridata"
  attr(z,"file") <- paste(filename,".HEAD/BRIK",sep="")
  invisible(z)
}

write.AFNI <- function(filename, ttt, label, note="", origin=c(0,0,0), delta=c(4,4,4), idcode="WIAS_noid") {
  ## TODO:
  ## 
  ## create object oriented way!!!!
  
  AFNIheaderpart <- function(type, name, value) {
    a <- "\n"
    a <- paste(a, "type = ", type, "\n", sep="")
    a <- paste(a, "name = ", name, "\n", sep="")
    if (regexpr("string",type) == 1) {
      value <- paste("'", value, "~", sep="")
      a <- paste(a, "count = ", nchar(value) - 1, "\n", sep ="")
      a <- paste(a, value, "\n", sep="")
    } else {
      a <- paste(a, "count = ", length(value), "\n", sep ="")
      j <- 0
      while (j<length(value)) {
        left <- length(value) - j
        if (left>4) left <- 5
        a <- paste(a, paste(value[(j+1):(j+left)],collapse="  "), "\n", sep="  ")
        j <- j+5
      }
    }
    a
  }
  
  conhead <- file(paste(filename, ".HEAD", sep=""), "w")
  writeChar(AFNIheaderpart("string-attribute","HISTORY_NOTE",note),conhead,eos=NULL)
  writeChar(AFNIheaderpart("string-attribute","TYPESTRING","3DIM_HEAD_FUNC"),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","IDCODE_STRING",idcode),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","IDCODE_DATE",date()),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","SCENE_DATA",c(0,11,1,-999,-999,-999,-999,-999)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","ORIENT_SPECIFIC",c(0,3,4)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("float-attribute","ORIGIN",origin),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("float-attribute","DELTA",delta),conhead,eos=NULL)  
  minmax <- function(y) {r <- NULL;for (k in 1:dim(y)[4]) {r <- c(r,min(y[,,,k]),max(y[,,,k]))}; r}
  mm <- minmax(ttt)
  writeChar(AFNIheaderpart("float-attribute","BRICK_STATS",mm),conhead,eos=NULL)
  writeChar(AFNIheaderpart("integer-attribute","DATASET_RANK",c(3,dim(ttt)[4],0,0,0,0,0,0)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","DATASET_DIMENSIONS",c(dim(ttt)[1:3],0,0)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","BRICK_TYPES",rep(1,dim(ttt)[4])),conhead,eos=NULL)  

  scale <- rep(0,dim(ttt)[4])
  for (k in 1:dim(ttt)[4]) {
    scale[k] <- max(abs(mm[2*k-1]),abs(mm[2*k]))/32767
    ttt[,,,k] <- ttt[,,,k] / scale[k]
  }

  writeChar(AFNIheaderpart("float-attribute","BRICK_FLOAT_FACS",scale),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BRICK_LABS",paste(label,collapse="~")),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BYTEORDER_STRING","MSB_FIRST"),conhead,eos=NULL)  
  close(conhead)

  conbrik <- file(paste(filename, ".BRIK", sep=""), "wb")
  dim(ttt) <- NULL
  writeBin(as.integer(ttt), conbrik,size=2, endian="big")
  close(conbrik)
}


#libLoad('tcltk')    # for graphics

print("################################################################")
print("Visit http://afni.nimh.nih.gov/sscc/gangc/3dGC.html and makse sure")
print("you've acquired the data for the analysis in desirable data format.")
print("################################################################")
print("~~~~~~~~~~~~~~~~~")
print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")
print("~~~~~~~~~~~~~~~~~")

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
if (masked) {maskFN <- readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); maskData <- read.AFNI(maskFN)$ttt}

inDataTS <- read.AFNI(inFN)
myNote=inDataTS$header$HISTORY_NOTE; myOrig=inDataTS$origin; myDelta=inDataTS$delta
inDataTS <- inDataTS$ttt
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

ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

tag <- 1
while (tag == 1) {
#	if ((maskData[ii, jj, kk,1] != 0) & !all(inDataTS[ii, jj, kk,] == 0)) {
   if (ifelse(as.logical(masked), (maskData[ii, jj, kk,1] != 0) & !all(inDataTS[ii, jj, kk,] == 0),
      !all(inDataTS[ii, jj, kk,] == 0))) {
		tag<-0
		tmpData <- cbind(inDataTS[ii, jj, kk,], sdTS)
		try(fm <- VAR(tmpData, p=nLags, type="none", exogen=exMatMod), tag<-1)
		if (ii<dimx) ii<-ii+1 else break
	}	
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
	   outData[1+6*(mm-1)] <- coef(fm)[[1]][1,1]   # target to target
	   outData[2+6*(mm-1)] <- coef(fm)[[1]][1,3]   # t
	   outData[3+6*(mm-1)] <- coef(fm)[[1]][2,1]   # seed to target
	   outData[4+6*(mm-1)] <- coef(fm)[[1]][2,3]   # t
	   outData[5+6*(mm-1)] <- coef(fm)[[2]][1,1]   # target to seed
	   outData[6+6*(mm-1)] <- coef(fm)[[2]][1,3]   # t
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
rm(inDataTS)  # retrieve some memory

print("#++++++++++++++++++++++++++++++++++++++++++++")

# spill out the original path matrix with direction going from rows to columns

for (ii in 1:nLags) {
   outLabel <- paste(sprintf("self-effect lag %i", ii))
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
if (outView=="tlrc") statpar <- paste(statpar, " -view tlrc -addFDR -newid ", outFN)
if (outView=="orig") statpar <- paste(statpar, "-addFDR -newid ", outFN)
system(statpar)


print("#++++++++++++++++++++++++++++++++++++++++++++")
anotherLag <- as.integer(readline("Want to try another number of lags for VAR (0: no; 1: yes)? "))
}
print("#++++++++++++++++++++++++++++++++++++++++++++")

#causality(fm, cause=names(myData)[1])

anotherAna <- as.integer(readline("Next (0: quit; 1: another individual analysis; 2: group analysis)? "))
} # while (anotherAna==1)

} # if (anaType==1)

print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 1dGC.R ==================          ")
print("AFNI Vector (or Multivariate) Auto-Regressive (VAR or MAR) Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.8,  Oct. 10, 2008")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website: http://afni.nimh.nih.gov/sscc/gangc/VAR.html")
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

# header assumed for multi-column files, but not for one-column ones
readMultiFiles <- function(nFiles, dim, inData) { 
   inFile <- vector('list', nFiles)  # list of file names with path attached
	fn <- vector('list', nFiles)      # ist of file names
   for (ii in 1:nFiles) {
      inFile[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
         "{{Files} {.1D}} {{All files} *}",
         title = paste('Choose number', ii, 'input file')))
      fn[[ii]] <- strsplit(inFile[[ii]], "/")[[1]][length(strsplit(inFile[[ii]], "/")[[1]])]
		print(sprintf("No. %i file just read in: %s", ii, fn[[ii]]))
		if (dim==1) inData[,ii] <- read.table(inFile[[ii]], header=FALSE)
		if (dim==2) inData[[ii]] <- read.table(inFile[[ii]], header=TRUE)    
   }
	return(list(fn, inData))
}

plotTS <- function(dataFrame, nCurves, msg) {
   if (nCurves <= 5) {
      x11(); par(mfrow=c(nCurves, 1))
	   for (ii in 1:nCurves) {
	      plot(dataFrame[,ii], ann=FALSE, axes=TRUE)
		   if (ii==1) title(msg)
		   lines(dataFrame[,ii])
		   mtext(sprintf("%s", names(dataFrame)[ii]), side=2, line=2)
      }
		mtext("time", side=1, line=2.5)
	} else for (ii in 1:nCurves) {
		   x11()
		   plot(dataFrame[,ii], ann=FALSE, axes=TRUE)
			title(msg)
			lines(dataFrame[,ii])
			mtext(sprintf("%s", names(dataFrame)[ii]), side=2, line=2)
			mtext("time", side=1, line=2.5)
	}
}

# plot network
plotNet <- function(net, selfLoop, edgeWd, arrScl, edgeCol, msg) {
	netData <- network(net, loops=selfLoop, directed=TRUE)
	X11()
	plot.network(netData, displaylabels=selfLoop, mode="circle", edge.lwd=edgeWd,
		arrowhead.cex=arrScl, edge.col=edgeCol, loop.cex=5, 
		boxed.label=FALSE, label.pos=0, vertex.col=3)
	title(msg)
}

libLoad("network")  # network drawing
libLoad('tcltk')    # for graphics

print("################################################################")
print("Visit http://afni.nimh.nih.gov/sscc/gangc/VAR.html and makse sure")
print("you've acquired the data for the analysis in desirable data format.")
print("################################################################")
print("~~~~~~~~~~~~~~~~~")
print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")
print("~~~~~~~~~~~~~~~~~")

anaType <- as.integer(readline("Analysis type (0: quit; 1: individual; 2: group)? "))

if (anaType==1) {
libLoad("gsl")      # Legendre polynomials
libLoad("vars")     # VAR modeling 

anotherAna <- 1
while (anotherAna==1) {

# Set most paratmers here
maxLags <- 8

nROIs <- as.integer(readline("Number of regions/nodes (e.g., 6)? "))     # number of ROIs
print("Header with one line of labels is optional in multi-column files, but NOT allowed in one-column files.")
yForm <- as.integer(readline("ROI time series data type (0: MULTIPLE one-column files; 1: ONE multi-column file)? "))     # input format

if (yForm) { # read ROI file (in dataframe), and take label from the header
   fn <- tclvalue( tkgetOpenFile( filetypes = 
      "{{ROI files in multi-column 1D format} {.1D}} {{All files} *}",
      title = paste('Choose ROIs time series file')))
   print(sprintf("File just read in: %s", strsplit(fn, "/")[[1]][length(strsplit(fn, "/")[[1]])]))
   yHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? ")) 
   if (yHeader == 1) myData <- read.table(fn, header=TRUE) else {
      myData <- read.table(fn, header=FALSE)
      for (ii in 1:nROIs) names(myData)[ii] <- readline(sprintf("Name for region/node number %i? ", ii))
   }
#   nROIs <- ncol(myData)
   nTotal <- nrow(myData) 
} else {

   fn <- vector('list', nROIs)
   fn[[1]] <- tclvalue(tkgetOpenFile(filetypes = 
      "{{ROI files in one-column 1D format} {.1D}} {{All files} *}",
      title = paste('Choose number', 1, 'ROI time series file')))
   print(sprintf("No. %i file just read in: %s", 1, strsplit(fn[[1]], "/")[[1]][length(strsplit(fn[[1]], "/")[[1]])]))
   myData <- data.frame(matrix(data=NA, nrow=dim(read.table(fn[[1]], header=FALSE))[1], ncol=nROIs, dimnames = NULL))
   myData[,1] <- read.table(fn[[1]], header=FALSE)
   for (ii in 2:nROIs) { # read ROI 1D file (in 1 column)
   fn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
      "{{ROI files in one-column format} {.1D}} {{All files} *}",
      title = paste('Choose number', ii, 'ROI time series file')))
   print(sprintf("No. %i file just read in: %s", ii, strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])]))
	myData[,ii] <- read.table(fn[[ii]], header=FALSE)    
} #for (ii in 2:nROIs)

# take labels from the filenames
ROIlab <- vector('list', nROIs)
for (ii in 1:nROIs) ROIlab[[ii]] <- strsplit(strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])], ".1D")[[1]]

names(myData) <- ROIlab 
nTotal <- dim(myData)[1]
}
print("#++++++++++++++++++++++++++++++++++++++++++++")
print("If there are n consecutive chunks of data, enter n-1 here.")
print("If all the time series are consecutive, enter 0 breaks.")
nBreaks <- as.integer(readline("Number of breaks in the time series? "))+1     # number of runs

nPts <- array(data = NA, dim=nBreaks)
if (nBreaks == 1) nPts[1] <- nTotal else
for (ii in 1:nBreaks) nPts[ii] <- as.integer(readline(paste("Length of number", ii, "run/segment? ")))

print("#++++++++++++++++++++++++++++++++++++++++++++")

print("Prior detrending is NOT recommended due to potential complications. ")
print("Trend can be modeled through specifying the order of a polynomial here, ")
print("or can be included as part of covariates later on. If you plan to model the")
print("trend with your own regressors or don't need to model it, choose -1 here.")
print("If trend has already been removed (not recommended), choose 0 here.")
nPoly <- as.integer(readline("Order of polynomials for drifting effects (note: -1 means no trend removal!)? "))  # Legendre

print("#++++++++++++++++++++++++++++++++++++++++++++")

COV <- as.integer(readline("Any other covariates or confounding effects than drifting (0: no; 1: yes)? ")) 
if (as.logical(COV)) {
   nCOVs <- as.integer(readline("Number of covariates (e.g., 6)? "))     # number of regions: 6
	print("Header with one line of labels is optional in multi-column files, but NOT allowed in one-column files.")
	covForm <- as.integer(readline("Covariates data type (0: MULTIPLE one-column files; 1: ONE multi-column file)? "))     # covariates format
   if (covForm) {
      fncov <- tclvalue( tkgetOpenFile( filetypes = 
      "{{Covariate File} {.1D}} {{All files} *}",
      title = paste('Choose covariates file in multi-column format')))
      
		covHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? "))
		if (covHeader == 1) exData <- read.table(fncov, header=TRUE) else {
         exData <- read.table(fncov, header=FALSE)
         for (ii in 1:nROIs) names(exData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
      }whYnOT6834
   } else {
#      covn <- vector('list', nCOVs)
      exData <- data.frame(matrix(data=NA, nrow=nTotal, ncol=nCOVs, dimnames = NULL))
      exTmp <- readMultiFiles(nCOVs, 1, exData)
		covFN <- exTmp[[1]]; exData <- exTmp[[2]]
			
#		for (ii in 1:nCOVs) {
#         covn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#            "{{Covariate Files} {.1D}} {{All files} *}",
#            title = paste('Choose number', ii, 'covariate time series file')))
#         print(sprintf("No. %i file just read in: %s", ii, strsplit(covn[[ii]], "/")[[1]][length(strsplit(covn[[ii]], "/")[[1]])]))
#			exData[,ii] <- read.table(covn[[ii]], header=FALSE)    
#      }
      COVlab <- vector('list', nCOVs)
      for (ii in 1:nCOVs) COVlab[[ii]] <- strsplit(covFN[[ii]], ".1D")[[1]]
      names(exData) <- COVlab
   }
} else exData <- NULL
print("#++++++++++++++++++++++++++++++++++++++++++++")

# plot out the time series, and let the user make sure they look OK
plotROIs <- as.integer(readline("Plot out the original ROI time series (0: no; 1: yes)? "))
#print("Check the fitting for each ROI time series")
if (plotROIs) plotTS(myData, nROIs, "Original time series")


if (as.logical(COV)) {
   plotCov <- as.integer(readline("Plot out covariates time series provided by you (0: no; 1: yes)? "))
   if (plotCov) plotTS(exData, nCOVs, "Covariate time series")
} # if (as.logical(COV))
print("-----------------")

# scaling the data per run per ROI
newData <- myData
print(sprintf("If normalization was NOT performed during pre-processing, you can scale the data now."))
scaleTS <- as.integer(readline("Scale the ROI time series (0: no; 1: yes)? "))
if (scaleTS) {
   jumpPts <- 0
   for (ii in 1:nBreaks) {
      for (jj in 1:nROIs) newData[(jumpPts+1):(jumpPts+nPts[ii]),jj] <- 
         myData[(jumpPts+1):(jumpPts+nPts[ii]),jj]/mean(myData[(jumpPts+1):(jumpPts+nPts[ii]),jj])
	  if (ii < nBreaks) jumpPts <- jumpPts+nPts[ii]
	}
}

# create exogenous variables with Legendre polynomials from gsl
if (nPoly > -1) {
   trendMat <- as.data.frame(array(0, dim = c(nTotal, (nPoly+1)*nBreaks)))
   jumpPts <- 0
   for (ii in 1:nBreaks) {
	  trendMat[(jumpPts+1):(jumpPts+nPts[ii]),(1+(nPoly+1)*(ii-1)):((nPoly+1)*ii)] <- 
	      t(legendre_Pl_array(nPoly, seq(from=-1,to=1,len=nPts[ii])))    
      names(trendMat)[(1+(nPoly+1)*(ii-1)):((nPoly+1)*ii)] <- sprintf("Run%iTrend%i", ii, seq(nPoly+1)-1)
      if (ii < nBreaks) jumpPts <- jumpPts+nPts[ii]
   }
   if (is.null(exData)) exMat <- trendMat else exMat <- cbind(trendMat, exData)
} else exMat <- exData # if no baseline and trend, do nothing
# plot out those polynomials here???

critSel <- VARselect(newData, lag.max = maxLags, type = "none", exogen=exMat)
print(sprintf("Suggested orders for VAR:"))
print(critSel$selection)
print("AIC: Akaike Information Criterion;")
print("HQ:  Hannan-Quinn criterion;")
print("SC:  Schwartz Criterion;")
print("FPE: Final Prediction Error criterion.")
print(" ")
print("Usually consistency exists between AIC and FPE, and between")
print("HQ and SC. AIC and FPE tend to overestimate the 'true order'.")
print("Since there is no such a universally best criterion, it may be")
print("better to try different analysis with various orders within")
print("the range covered by the 4 criteria.")

anotherLag <- TRUE
while (anotherLag) {

#print(sprintf("Select the order of VAR model based on above criteria:"))
nLags <- as.integer(readline("Select order of VAR model based on above criteria (e.g., 3)? "))

# generate intervention dummy variables for across-run/block breaks: nLags dummies per run
if (nBreaks > 1) {
	breakMat <- as.data.frame(array(0, dim = c(nTotal, (nBreaks-1)*nLags)))
	jumpPts <- 0
	for (ii in 1:(nBreaks-1)) {
		jumpPts <- jumpPts+nPts[ii]
		for (jj in 1:(nLags)) {
		   breakMat[,(ii-1)*nLags+jj] <- c(rep(0, jumpPts+jj-1), 1, rep(0, nTotal-jumpPts-jj))
		   names(breakMat)[(ii-1)*nLags+jj] <- sprintf("Run%iLag%i", ii, jj)
		}   
	}
	if (is.null(exMat)) exMatMod <- breakMat else exMatMod <- cbind(breakMat, exMat)
	print(sprintf("Suggested orders for VAR updated:"))
	print(VARselect(newData, lag.max = maxLags, type = "none", exogen=exMatMod)$selection)
} else exMatMod <- exMat
print("#++++++++++++++++++++++++++++++++++++++++++++")

fm <- VAR(newData, p=nLags, type="none", exogen=exMatMod)

qualityCheck <- as.integer(readline("Want to run a few quality check tests (0: no; 1: yes)? "))
if (qualityCheck) {
# the modulus of the eigenvalues (presumably less than 1 as stable condition) in the reverse characteristic polynomial; stable process is stationary, but the converse is not true
#print("Quality check of the model:")
if (prod(roots(fm)<1)) print("Eigenvalues of the companion coefficient matrix indciate that the VAR(p) process is stable and thus stationary") else print("The VAR(p) process seems unstable and thus is not stationary")
print("-----------------")
print("Normality testing of the residuals")
print(normality.test(fm))
print("-----------------")
print("Serial correlation test:")
print(serial.test(fm, lags.pt=16, lags.bg=5, type=c("PT.asymptotic")))
print(serial.test(fm, lags.pt=16, lags.bg=5, type=c("PT.adjusted")))
print(serial.test(fm, lags.pt=16, lags.bg=5, type=c("BG")))
print(serial.test(fm, lags.pt=16, lags.bg=5, type=c("ES")))
print("-----------------")
print("Autoregressive conditional heteroskedasticity (ARCH) test")
print(arch.test(fm))
archPlot <- as.integer(readline("Plot out ARCH test result (0: no; 1: yes)? "))
if (archPlot) {X11(); plot(arch.test(fm))}
print("-----------------")
statPlot <- as.integer(readline("Plot out stability test (0: no; 1: yes)? "))
if (statPlot) {	
   print("Available empirical fluctuation process (efp) types are:")
   print("1. OLS-CUSUM:   CUmulative SUMs of Ordinary Least Squares residuals;")
   print("2. Rec-CUSUM:   CUmulative SUMs of Recursive residuals;")
   print("3. OLS-MOSUM:   MOving SUMs of Ordinary Least Squares residuals;")
   print("4. Rec-MOSUM:   MOving SUMs of Recursive residuals;")
   print("5. RE:          Recursive Estimates of the path coefficients;")
   print("6. ME:          Moving Estimates of the path coefficients;")
   print("7. Score-CUSUM: CUmulative SUMs of the ML Scores;")
   print("8. Score-MOSUM: MOving SUMs of the ML scores.")
   anotherType <- TRUE
   while (anotherType) {
   procNo <- as.integer(readline("Select process type (1-8)? "))
   if (procNo==1) procType <- "OLS-CUSUM"
   if (procNo==2) procType <- "Rec-CUSUM"
   if (procNo==3) procType <- "OLS-MOSUM"
   if (procNo==4) procType <- "Rec-MOSUM"
   if (procNo==5) procType <- "RE"
   if (procNo==6) procType <- "ME"
   if (procNo==7) procType <- "Score-CUSUM"
   if (procNo==8) procType <- "Score-CUSUM"
   X11(); plot(stability(fm, type = procType, h = 0.15, dynamic = FALSE, rescale = TRUE))
   anotherType <- as.integer(readline("Want to plot stability with another type (0: no; 1: yes)? "))
   } # while (anotherType)
} # if (statPlot)
	
print("-----------------")	
checkCov <- as.integer(readline("Check significance of covariates (0: no; 1: yes)? "))
if (checkCov) {
   anotherCovPth <- TRUE
   while (anotherCovPth) {
	   pCovThresh <- as.numeric(readline("p-threshold for covariates (e.g., 0.05)? "))
		#Info about all the covariates:
		#lapply(coef(fm), function(x) x[(nROIs*nLags+1):(nROIs*nLags+(nPoly+1)*nBreaks+nCOVs),])
		covPList <- lapply(coef(fm), function(x) x[(nROIs*nLags+1):(nROIs*nLags+(nPoly+1)*nBreaks+nCOVs),4]<=pCovThresh)
		#covSigList <- vector(mode="logical", (nPoly+1)*nBreaks+nCOVs)
		#for (ii in 1:nROIs) covSigList <- covSigList+covPList[[ii]]
		covSigList <- apply(do.call(cbind, covPList), 1, sum)
		# detailed info: apply(do.call(cbind, covPList), c(1,2), sum)
		if (length(covSigList[covSigList==0])) {
			print(sprintf("With a threshold of %f the following covarates don't show significance in the model:", pCovThresh))
			print(names(covSigList[covSigList==0]))
			print("You may consider removing them from the model. However, when ploynomial terms")
			print("show up in the above list, only if the highest order of the polynominals")
			print("indicates insignificant would you try decreasing the order.")
	   } else print(sprintf("All covarates show significance in the model with a threshold of %f.", pCovThresh))
		anotherCovPth <- as.integer(readline("Want to try another p-threshold for covariates (0: no; 1: yes)? "))
	} #
}	
} # model quality check

print("#++++++++++++++++++++++++++++++++++++++++++++")

# spill out the original path matrix with direction going from rows to columns

netMatR <- array(data=NA, dim=c(nLags, nROIs, nROIs))   # original path coefficient matrix
netMatT <- array(data=NA, dim=c(nLags, nROIs, nROIs))   # t values matrix
for (ii in 1:nROIs) for (jj in 1:nROIs) for (kk in 1:nLags)  {
	netMatR[kk,jj,ii] <- coef(fm)[[ii]][jj+nROIs*(kk-1), 1]  # path coefficients
	netMatT[kk,jj,ii] <- coef(fm)[[ii]][jj+nROIs*(kk-1), 3]  # t values
}
				
for (ii in 1:nLags) {
   print(sprintf("Path coefficient matrix with a lag of %i (direction goes from row to column):", ii))
   print(matrix(netMatR[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
   saveMat <- as.integer(readline("Save above path matrix for group analysis (0: no; 1: yes)? "))
   if (saveMat) {
      matName <- as.character(readline("File name prefix (e.g., PathLag1Subj1)? "))
      write.table(netMatR[ii,,], file=sprintf("%s.1D", matName), append=FALSE, row.names=names(myData), col.names=names(myData))
   }   	
   print("-----------------")
	print(sprintf("Matrix of t values with a lag of %i (direction goes from row to column):", ii))
   print(matrix(netMatT[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
	print(sprintf("DFs = %i for Ho: a path coefficient = 0.", summary(fm)$varresult[[1]]$df[2]))
	saveMatT <- as.integer(readline("Save matrix of t values (0: no; 1: yes)? "))
   if (saveMatT) {
      matName <- as.character(readline("File name prefix (e.g., TLag1Subj1)? "))
      write.table(netMatT[ii,,], file=sprintf("%s.1D", matName), append=FALSE, row.names=names(myData), col.names=names(myData))
   }   	
   print("-----------------")
}

if (nLags>1) { # overall network with all lags collapsed
   libLoad("car")  # for linear.hypothesis
	# initialization for overall network across lags
	netCMatF <- matrix(data=NA, nrow=nROIs, ncol=nROIs, dimnames = list(names(myData), names(myData)))
   netCMatP <- matrix(data=NA, nrow=nROIs, ncol=nROIs, dimnames = list(names(myData), names(myData)))
	fTest <- vector("list", nROIs)  # initialization for F test name list
	for (ii in 1:nROIs) for (jj in 1:nLags)
	fTest[[ii]] <- c(fTest[[ii]], sprintf("%s.l%d",names(myData)[ii],jj))
	for (ii in 1:nROIs) for (jj in 1:nROIs) {
	   ltTmp <- linear.hypothesis(fm$varresult[[ii]], fTest[[jj]])
		netCMatF[jj, ii] <- ltTmp$F[2]; netCMatP[jj, ii] <- ltTmp$Pr[2]	
   }
	print(sprintf("Overall F matrix for ALL %i lags (direction goes from row to column):", nLags))
   print(netCMatF)
	print(sprintf("Numerator DFs = %i, and Denominator DFs =%i", -ltTmp$Df[2], ltTmp$Res.Df[1]))
	print(sprintf("Null hypothesis Ho: path coef from region i to j is 0 for all %i lags.", nLags))
   saveCMatF <- as.integer(readline("Save above F matrix (0: no; 1: yes)? "))
   if (saveCMatF) {
      matCFName <- as.character(readline("File name prefix (e.g., FMatSubj1)? "))
      write.table(netCMatF, file=sprintf("%s.1D", matCFName), append=FALSE)
   }
	print("-----------------")
	print(sprintf("Overall p-value matrix for ALL %i lags (direction goes from row to column):", nLags))
   print(netCMatP)
	print(sprintf("Numerator DFs = %i, and Denominator DFs =%i", -ltTmp$Df[2], ltTmp$Res.Df[1]))
	print(sprintf("Null hypothesis Ho: path coef from region i to j is 0 for all %i lags.", nLags))
   saveCMatP <- as.integer(readline("Save above p-value matrix (0: no; 1: yes)? "))
   if (saveCMatP) {
      matCPName <- as.character(readline("File name prefix (e.g., PMatSubj1)? "))
      write.table(netCMatP, file=sprintf("%s.1D", matCPName), append=FALSE)
   }	   	
   print("-----------------")
} # if (nLags>1)	

print("Covariance matrix of residuals:")
print(summary(fm)$covres)
saveCovMat <- as.integer(readline("Save above contemporaneous covariance matrix (0: no; 1: yes)? "))
if (saveCovMat) {
   matCovName <- as.character(readline("File name prefix (e.g., CovSubj1)? "))
   write.table(summary(fm)$covres, file=sprintf("%s.1D", matCovName), append=FALSE, row.names=TRUE, col.names=TRUE)
}   	
print("-----------------")

print("Correlation matrix of residuals:")
print(summary(fm)$corres)
saveCorMat <- as.integer(readline("Save above contemporaneous correlation matrix (0: no; 1: yes)? "))
if (saveCorMat) {
   matCorName <- as.character(readline("File name prefix (e.g., CorSubj1)? "))
   write.table(summary(fm)$corres, file=sprintf("%s.1D", matCorName), append=FALSE, row.names=TRUE, col.names=TRUE)
}   	
print("-----------------")

seeModel <- as.integer(readline("Want to see the model details and statistics (0: no; 1: yes)? "))
if (seeModel) print(summary(fm))

print("#++++++++++++++++++++++++++++++++++++++++++++")
plotFit <- as.integer(readline("Plot out the model fit, residuals, residual ACF and PACF (0: no; 1: yes)? "))
print("-----------------")

if (plotFit) { X11(); plot(fm) }

anotherPth <- TRUE
while (anotherPth) {

print(sprintf("There are totally %i paths in the model. Select a low p value ", nLags*nROIs^2))
print("if you're concerned about multiple comparisons issue:")
pThresh <- as.numeric(readline("p-threshold for causal effects (e.g., 0.05)? "))

# connection goes from row to column, which is the default in network
netMat <- array(data=NA, dim=c(nLags+1, nROIs, nROIs))  # thresholded network
for (ii in 1:nROIs) 
	for (jj in 1:nROIs) { 
		for (kk in 1:nLags) 
			netMat[kk,jj,ii] <- sum(sum(coef(fm)[[ii]][jj+nROIs*(kk-1), 4] 
			   <= pThresh)>0)
		if (nLags>1) {
			ltTmp <- linear.hypothesis(fm$varresult[[ii]], fTest[[jj]])
			netCMatF[jj, ii] <- ltTmp$F[2]; netCMatP[jj, ii] <- ltTmp$Pr[2]
			}
		}			
									
print("#++++++++++++++++++++++++++++++++++++++++++++")
# spill the matrices with those insignificant ones being masked: direction goes from rows to cols
for (ii in 1:nLags) {
   print(sprintf("Path matrix with a lag of %i with insignificant ones masked with 0s:", ii))
   print(matrix(netMatR[ii,,]*netMat[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
   print("-----------------")
	print(sprintf("Matrix of t values with a lag of %i with insignificant ones masked with 0s:", ii))
   print(matrix(netMatT[ii,,]*netMat[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
	print(sprintf("DFs = %i for Ho: a path coefficient = 0.", summary(fm)$varresult[[1]]$df[2]))
   print("-----------------")
}

if (nLags>1) {
   print(sprintf("Overall F matrix for all %i lags with insignificant ones masked with 0s:", nLags))
	print(netCMatF*(netCMatP<=pThresh))
	print("-----------------")	
	print(sprintf("Overall p-value matrix for all %i lags with insignificant ones masked with 0s:", nLags))
	print(netCMatP*(netCMatP<=pThresh))
}
					
plotNetwork <- as.integer(readline("Plot out the identified network (0: no; 1: yes)? "))
if (plotNetwork) {

set.seed(1702)
net <- network.initialize(nROIs)
attr(net, "vertex.names") <- names(myData)
edgeScale <- as.numeric(readline("Scale factor for path thickness (e.g., 20)? "))
arrowScale <- as.numeric(readline("Scale factor for arrows (e.g., 2)? "))
print("A self-loop indicates a region/node has effect on itself from t-1 to t.")
selfLoop <- as.integer(readline("Show self-loops in the network (0: no; 1: yes)? "))

for (ii in 1:nLags) {
	netData <- netMat[ii,,]
	rownames(netData) <- names(myData); colnames(netData) <- names(myData)
	# if path +, col<-2 (red); if path -, col<-4 (blue)
	plotNet(netData, selfLoop, netMatR[ii,,]*edgeScale, arrowScale, 3-sign(netMat[ii,,]*netMatR[ii,,]), sprintf("Network with lag = %s", ii))
} # for (ii in 1:nLags)

if (nLags>1) {   # overall network			
	logF <- log(netCMatF)*(netCMatP<=pThresh)
	# if path +, col<-2 (red); if path -, col<-4 (blue)
	plotNet(logF, selfLoop, logF*edgeScale/10, arrowScale, 3-sign(logF), sprintf("Overall network for all %s lags (scaled by ln(F))", nLags))
}	# if (nLags>1))

} # if (plotNetwork)
	

print("#++++++++++++++++++++++++++++++++++++++++++++")
anotherPth <- as.integer(readline("Want to try another p-threshold/plotting set-up for network (0: no; 1: yes)? "))
}
print("#++++++++++++++++++++++++++++++++++++++++++++")
anotherLag <- as.integer(readline("Want to try another number of lags for VAR (0: no; 1: yes)? "))
}
print("#++++++++++++++++++++++++++++++++++++++++++++")

#causality(fm, cause=names(myData)[1])

anotherAna <- as.integer(readline("Next (0: quit; 1: another individual analysis; 2: group analysis)? "))
} # while (anotherAna==1)

} # if (anaType==1)

# Group analysis below

if (anaType==2 || anotherAna==2) {
libLoad("psych")  # for using fisherz

print("It's suggested to run a separate group analysis for each lag.")

doneGrp <- 1
while (doneGrp) {

nSubjs <- as.integer(readline("Number of subjects (e.g., 12)? "))     # number of subjects
#gfn <- vector('list', nSubjs)
pathList <- vector('list', nSubjs)
pathList <- readMultiFiles(nSubjs, 2, pathList)[[2]]

#for (ii in 1:nSubjs) { # read in path matrices
#   fn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#      "{{Path coef matrix file} {.1D}} {{All files} *}",
#      title = paste('Choose subject No.', ii, 'path coef matrix file')))
#   print(sprintf("No. %i file just read in: %s", ii, strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])]))
#	 pathList[[ii]] <- read.table(fn[[ii]], header=TRUE)
#}

nROIsG <- dim(pathList[[1]])[1]
roiNames <- names(pathList[[1]])

#zArray <- array(data=unlist(lapply(pathList, fisherz)), dim=c(nROIsG, nROIsG, nSubjs))
#grpMat <- array(data=NA, dim=c(nROIsG, nROIsG, 2))
#for (ii in 1:nROIsG) for (jj in 1:nROIsG) {
#   testRes <- t.test(zArray[ii, jj,])
#	grpMat[ii,jj,1] <- as.numeric(testRes$statistic)   # t
#	grpMat[ii,jj,2] <- testRes$p.value   # p
#}
#grpT <- matrix(grpMat[,,1], nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
#grpP <- matrix(grpMat[,,2], nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))

# Instead of looping, we can also use the following aggregation approach
zList <- lapply(pathList, fisherz)  # sapply(pathList, fisherz, simplify = FALSE)
resList <- apply(do.call(rbind, lapply(lapply(zList, as.matrix), c)), 2, t.test)
tList <- lapply(resList, function(x) as.numeric(x$statistic))
pList <- lapply(resList, function(x) as.numeric(x$p.value))
grpT <- matrix(unlist(tList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
grpP <- matrix(unlist(pList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))

   print("Group t matrix (direction goes from row to column):")
   print(grpT)
	print(sprintf("DFs = %i", as.numeric(resList[[1]]$parameter)))
	print("-----------------")	
   saveTMat <- as.integer(readline("Save the above t matrix (0: no; 1: yes)? "))
      if (saveTMat) {
         matTName <- as.character(readline("File name prefix for t matrix? "))
         write.table(grpT, file=sprintf("%sLag.1D", matTName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }
	print("-----------------")
	print("Group p matrix (direction goes from row to column):")
   print(grpP)
	print("-----------------")
   savePMat <- as.integer(readline("Save the above p matrix (0: no; 1: yes)? "))
      if (savePMat) {
         matPName <- as.character(readline("File name prefix for p matrix? "))
         write.table(grpP, file=sprintf("%sLag.1D", matTName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }	
   print("-----------------")
anotherPthG <- TRUE
while (anotherPthG) {
						
pThreshG <- as.numeric(readline("p-threshold for group analysis (e.g., 0.05)? "))
surviveT <- as.numeric(grpP<=pThreshG)*grpT
surviveP <- as.numeric(grpP<=pThreshG)*grpP
print("Group t matrix with insignificant ones masked with 0s:")
print(surviveT)
print(sprintf("DFs = %i", as.numeric(resList[[1]]$parameter)))
print("-----------------")
print("Group p matrix with insignificant ones masked with 0s:")
print(surviveP)
print("-----------------")				
					
plotNetG <- as.integer(readline("Plot out the identified network (0: no; 1: yes)? "))
if (plotNetG) {
print("The thickness of a path indicates its relative strength (or effect).")
print("A path in red means positive strength (or effect) while blue is the opposite.")
set.seed(1702)
net <- network.initialize(nROIsG)
attr(net, "vertex.names") <- names(pathList[[1]])
edgeScaleG <- as.numeric(readline("Scale factor for path thickness (e.g., 1)? "))
arrowScaleG <- as.numeric(readline("Scale factor for arrows (e.g., 2)? "))
selfLoop <- as.integer(readline("Show self-loops in the network (0: no; 1: yes)? "))
	# if path +, col<-2 (red); if path -, col<-4 (blue)
	plotNet(surviveT, selfLoop, surviveT*edgeScaleG, arrowScaleG, 3-sign(surviveT), sprintf("Group network with %s subjects", nSubjs))
} # if (plotNetG)

anotherPthG <- as.integer(readline("Want to try another p-threshold/plotting set-up for group network (0: no; 1: yes)? "))
} # while (anotherPthG)

doneGrp <- as.integer(readline("Next (0: done; 1: another group analysis)? "))
} # while (doneGrp)

} # if (anaType==2 || anotherAna==2)

if (anaType==0 || anotherAna==0 || doneGrp==0) {
   print("Make sure to save all desirable figures before leaving!")
   quitMe <- as.integer(readline("Quit R (0: no; 1: yes)? "))
	print("***********Thanks for using the program!***********")
	print("Any feedback would be welcome - Gang Chen (gangchen@mail.nih.gov)")
   if (quitMe) q()
}
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

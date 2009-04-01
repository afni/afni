print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 1dGC.R ==================          ")
print("AFNI Vector (or Multivariate) Auto-Regressive (VAR or MAR) Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 1.0.8,  April 1, 2009")
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
# readMultiFiles <- function(nFiles, dim, inData) { 
#    inFile <- vector('list', nFiles)  # list of file names with path attached
# 	fn <- vector('list', nFiles)      # ist of file names
#    for (ii in 1:nFiles) {
#       inFile[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#          "{{Files} {.1D}} {{All files} *}",
#          title = paste('Choose number', ii, 'input file')))
#       fn[[ii]] <- strsplit(inFile[[ii]], "/")[[1]][length(strsplit(inFile[[ii]], "/")[[1]])]
# 		print(sprintf("No. %i file just read in: %s", ii, fn[[ii]]))
# 		if (dim==1) inData[,ii] <- read.table(inFile[[ii]], header=FALSE)
# 		if (dim==2) inData[[ii]] <- read.table(inFile[[ii]], header=TRUE)    
#    }
# 	return(inData)
# }

#readMultiFiles <- function(nFiles, dim, inData, type) {
#   inFile <- vector('list', nFiles) # list of file names with path attached
#	fn <- vector('list', nFiles)     # list of file names
#	if (dim==1) inData <-  matrix(data = NA, nrow = dim(read.table(inFile[[ii]], header=FALSE))[1], ncol = nFiles)
#   if (dim==2) inData <- vector('list', nFiles)
#   for (ii in 1:nFiles) {
#     inFile[[ii]] <- readline(sprintf("No. %i %s file name: ", ii, type))  
#     fn[[ii]] <- strsplit(inFile[[ii]], "/")[[1]][length(strsplit(inFile[[ii]], "/")[[1]])]
#     print(sprintf("No. %i file just read in: %s", ii, fn[[ii]]))
#     if (dim==1) inData[,ii] <- read.table(inFile[[ii]], header=FALSE)
#     if (dim==2) inData[[ii]] <- read.table(inFile[[ii]], header=TRUE)
#  }
#   return(inData)
#}

readMultiFiles <- function(nFiles, dim, type) {
   inFile <- vector('list', nFiles) # list of file names with path attached
	fn <- vector('list', nFiles)     # list of file names
	if (dim==1) inData <-  matrix(data = NA, nrow = dim(read.table(inFile[[ii]], header=FALSE))[1], ncol = nFiles)
   if (dim==2) inData <- vector('list', nFiles)
   for (ii in 1:nFiles) {
	   inFile[[ii]] <- readline(sprintf("No. %i %s file name: ", ii, type))  
		fn[[ii]] <- strsplit(inFile[[ii]], "/")[[1]][length(strsplit(inFile[[ii]], "/")[[1]])]
		print(sprintf("No. %i file just read in: %s", ii, fn[[ii]]))
		if (dim==1) inData[,ii] <- read.table(inFile[[ii]], header=FALSE)
		if (dim==2) inData[[ii]] <- read.table(inFile[[ii]], header=TRUE)
	}
	return(inData)
}


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

# plot network
plotNet <- function(net, selfLoop, edgeWd, arrScl, edgeCol, msg) {
	netData <- network(net, loops=selfLoop, directed=TRUE)
	dev.new()
	plot.network(netData, displaylabels=selfLoop, mode="circle", edge.lwd=edgeWd,
		arrowhead.cex=arrScl, edge.col=edgeCol, loop.cex=5, 
		boxed.label=FALSE, label.pos=0, vertex.col=3)
	title(msg)
}


### rma version 0.562 by Wolfgang Viechtbauer, Department of Methodology and Statistics, University of Maastricht

rma <- function(yi, vi, mods=NULL, method="REML", addint=TRUE, ci=95, digits=4, btt=NULL, tau2=NULL, knha=FALSE, subset=NULL, ll=FALSE, control = list()) {

   if (length(yi) != length(vi)) {
      stop("Length of yi and vi is not the same.")
   }
   
   if (is.vector(mods)) {
      mods <- cbind(mods)
   }

   if (is.data.frame(mods)) {
      mods <- as.matrix(mods)
   }

   k     <- length(yi)
   ids   <- 1:k

   if (!is.null(subset)) {
      yi    <- yi[subset]
      vi    <- vi[subset]
      mods  <- mods[subset, , drop=FALSE]
      ids   <- ids[subset]
      k     <- length(yi)
   }

   if (sum(vi <= 0) > 0) {
      null.ids <- which(vi<=0)
      yi    <- yi[-c(null.ids)]
      vi    <- vi[-c(null.ids)]
      mods  <- mods[-c(null.ids), , drop=FALSE]
      ids   <- ids[-c(null.ids)]
      k     <- length(yi)
      warning("Outcomes with non-positive sampling variances have been excluded from the analysis.")
   }
   
   if (k <= 1) {
      stop("Processing terminated since k <= 1.")
   }
   
   if (is.null(mods) && addint == FALSE) {
      warning("Must either include an intercept (addint=TRUE) and/or moderators in model. Coerced intercept into the model.")
      addint <- TRUE
   }
   
	Y <- as.matrix(yi)
	
	if (addint == TRUE) {
      X <- cbind(intrcpt=rep(1,k), mods)
   } else {
      X <- mods
   }
   
   p <- dim(X)[2]

   if (method == "FE" || method == "FU") {
      if (p > k) {
         stop("The number of parameters to be estimated is larger than the number of observed outcomes.")
      }
   } else {
      if (is.null(tau2)) {
         if (p > k-1) {
            stop("The number of parameters to be estimated is larger than the number of observed outcomes.")
         }
      } else {
         if (p > k) {
            stop("The number of parameters to be estimated is larger than the number of observed outcomes.")
         }
      }
   }

   if ( (p == 1) && (sum(X == 1) == k) ) {
      int.only <- TRUE
   } else {
      int.only <- FALSE
   }
   
	tr <- function(X) {
		sum(diag(X))
	}

   con <- list(tau2.init=NULL, threshold=10^-5, maxiter=50, maxtau2=50, verbose=FALSE)
   con[names(control)] <- control

   se.tau2  <- NA
   I2       <- NA
   s2w      <- NA
      
   if (is.null(tau2) == TRUE) {

      if (!is.element(method, c("FE", "FU", "HS", "HE", "DL", "SJ", "ML", "REML", "EB"))) {
         stop("Unknown 'method' specified.")
      }

   	if (method == "HS") {
   		wi     <- 1/vi
   		W      <- diag(wi)
   		P      <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   		RSS    <- t(Y) %*% P %*% Y
   		tau2   <- RSS/sum(wi) - k/sum(wi)
   	}

   	if (method == "HE") {
   		P      <- diag(k) - X %*% solve(t(X) %*% X) %*% t(X)
   		RSS    <- t(Y) %*% P %*% Y
   		tau2   <- ( RSS - tr( P %*% diag(vi) ) ) / (k-p)
   	}
   
   	if (method == "DL") {
   		wi     <- 1/vi
   		W      <- diag(wi)
   		P      <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   		RSS    <- t(Y) %*% P %*% Y
   		tau2   <- ( RSS - (k-p) ) / tr(P)
   	}
  
   	if (method == "SJ") {
   		tau2.0 <- var(yi) * (k-1)/k
   		wi     <- 1/(vi + tau2.0)
   		W      <- diag(wi)
   		P      <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   		tau2   <- (tau2.0/(k-p)) * t(Y) %*% P %*% Y
   	}
  
   	if (is.element(method, c("ML", "REML", "EB"))) {
   
   		conv		<- 1
   		change	<- 1000
   		iter		<- 0
   
         if (is.null(con$tau2.init)) {
            tau2 <- max(0, var(yi) - mean(vi))
         } else {
            tau2 <- con$tau2.init
         }

   		while (change > con$threshold) {
   			if (con$verbose == TRUE) cat("Iteration:", iter, " Estimate of (Residual) Heterogeneity:", tau2, "\n")
   			iter     <- iter + 1
   			tau2.old <- tau2
   			wi       <- 1/(vi + tau2)
   			W		   <- diag(wi)
   			P		   <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   			if (method == "REML") {
   				adj	<- solve( tr(P%*%P) ) %*% ( t(Y)%*%P%*%P%*%Y - tr(P) )
   			}
   			if (method == "ML") {
   				adj	<- solve( tr(W%*%W) ) %*% ( t(Y)%*%P%*%P%*%Y - tr(W) )
   			}
   			if (method == "EB") {
   				adj	<- solve( tr(W) ) %*% ( (k/(k-p)) %*% t(Y)%*%P%*%Y - k )
   			}
   			while (tau2 + adj < 0) {
   				adj <- adj / 2
   			}
   			tau2		<- tau2 + adj
   			change	<- abs(tau2.old - tau2)
   			if (iter > con$maxiter) {
   				conv    <- 0
   				break
   			}
   		}
   
   		if (conv == 0) {
   			stop("Fisher scoring algorithm did not converge. Try increasing the number of iterations (maxiter), lowering the threshold (threshold), or use a different estimation method.")
   		}

         if (method == "ML") {
            se.tau2 <- sqrt( 2/sum(wi^2) )
         }
         if (method == "REML") {
            se.tau2 <- sqrt( 2/tr(P%*%P) )
         }
   	
      }
   
   	tau2 <- max(0, c(tau2))

   }

	if (method == "FE" || method == "FU") {
		tau2 <- 0
	}

   wi    <- 1/vi
   W     <- diag(wi)
   P     <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   QE    <- t(Y) %*% P %*% Y
   QEp   <- 1 - pchisq(QE, df=k-p)

   if (int.only == TRUE) {
      s2  <- (k-1)*sum(wi) / ( sum(wi)^2 - sum(wi^2) )
      I2  <- 100 * tau2 / (tau2 + s2)
   }

   if (method == "FU") {
      b   <- solve(t(X) %*% X) %*% t(X) %*% Y
      vb  <- solve(t(X) %*% X) %*% t(X) %*% diag(vi) %*% X %*% solve(t(X) %*% X)
   } else {
      wi  <- 1/(vi + tau2)
   	W   <- diag(wi)
   	b   <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
   	vb  <- solve(t(X) %*% W %*% X)
   }      

	if (knha == TRUE) {
      P     <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
      s2w   <- c( t(Y) %*% P %*% Y ) / (k-p)
		vb    <- s2w * vb
      if (method == "FE" || method == "FU") {
         warning("The Knapp & Hartung method is not meant to be used in the context of fixed-effects models.")
      }
	}

   if (is.null(btt)) {
      if (p > 1) {
         if (addint == TRUE) {
            btt <- 2:p
         } else {
            btt <- 1:p
         }
      } else {
         btt <- 1
      }
   }

   m <- length(btt)

	QME <- t(b)[btt] %*% solve(vb[btt,btt]) %*% b[btt]
	
	if (knha == FALSE) {
	  QMEp <- 1 - pchisq(QME, df=m)
	} else {
	  QMEp <- 1 - pf( QME/m, df1=m, df2=k-p )
	}
   
   alpha <- (100-ci)/100
   se    <- sqrt(diag(vb))
	z     <- b / se

	if (knha == FALSE) {
   	zp   <- 2*(1-pnorm(abs(z)))
   	crit <- qnorm(1-alpha/2)
	} else {
		zp   <- 2*(1-pt(abs(z), df=k-p))
		crit <- qt(1-alpha/2, df=k-p)
	}

   ci.lb	<- b - crit * se
   ci.ub	<- b + crit * se

   if (method == "FU") {
      P <- W - W %*% X %*% solve(t(X) %*% X) %*% t(X) - X %*% solve(t(X) %*% X) %*% t(X) %*% W + X %*% solve(t(X) %*% X) %*% t(X) %*% W %*% X %*% solve(t(X) %*% X) %*% t(X)
   } else {
      P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   }
	ll.ML      <- -1/2 *(k)   * log(2*pi)                              - 1/2 * sum( log(vi + tau2) )                                  - 1/2 * t(Y) %*% P %*% Y
	ll.REML    <- -1/2 *(k-p) * log(2*pi) + 0/2 * log( det(t(X)%*%X) ) - 1/2 * sum( log(vi + tau2) ) - 1/2 * log( det(t(X)%*%W%*%X) ) - 1/2 * t(Y) %*% P %*% Y
	dev.ML     <- -2 * ll.ML
	dev.REML   <- -2 * ll.REML
	AIC.ML     <- -2 * ll.ML   + 2*(p + (if (method == "FE" || method == "FU") 0 else 1) )
	BIC.ML     <- -2 * ll.ML   +   (p + (if (method == "FE" || method == "FU") 0 else 1) ) * log(k)
	AIC.REML   <- -2 * ll.REML + 2*(p + (if (method == "FE" || method == "FU") 0 else 1) )
	BIC.REML   <- -2 * ll.REML +   (p + (if (method == "FE" || method == "FU") 0 else 1) ) * log(k-p)
   fit.stats  <- c(ll.ML=ll.ML, dev.ML=dev.ML, AIC.ML=AIC.ML, BIC.ML=BIC.ML, ll.REML=ll.REML, dev.REML=dev.REML, AIC.REML=AIC.REML, BIC.REML=BIC.REML)

   res         <- list(b, se, z, zp, ci.lb, ci.ub, vb, tau2, se.tau2, k, p, m, fit.stats, QE, QEp, QME, QMEp, s2w, I2, int.only, yi, vi, X, ids, method, knha, btt, addint, digits, ci, ll, control)
   names(res)  <- c("b", "se", "z", "zp", "ci.lb", "ci.ub", "vb", "tau2", "se.tau2", "k", "p", "m", "fit.stats", "QE", "QEp", "QME", "QMEp", "s2w", "I2", "int.only", "yi", "vi", "X", "ids", "method", "knha", "btt", "addint", "digits", "ci", "ll", "control")
   class(res)  <- c("rma")
   res

}



libLoad("network")  # network drawing
#libLoad('tcltk')    # for graphics

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("Visit http://afni.nimh.nih.gov/sscc/gangc/VAR.html and makse sure")
print("you've acquired the data for the analysis in desirable data format.")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

print("################################################################")
print("Please consider citing the following if this program is useful for you:")
cat("\n\tGang Chen, J. Paul Hamilton, Moriah E. Thomason, Ian H. Gotlib, Ziad S. Saad\n")
cat("\tRobert W. Cox, Granger causality via vector auto-regression (VAR) attuned for\n")
cat("\tFMRI data analysis. ISMRM 17th Scientific Meeting, Hawaii, 2009.\n\n")
print("################################################################")

print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")

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
#   fn <- tclvalue( tkgetOpenFile( filetypes = 
#      "{{ROI files in multi-column 1D format} {.1D}} {{All files} *}",
#      title = paste('Choose ROIs time series file')))
   fn <- readline("ROIs time series file name: ")
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
#   fn[[1]] <- tclvalue(tkgetOpenFile(filetypes = 
#      "{{ROI files in one-column 1D format} {.1D}} {{All files} *}",
#      title = paste('Choose number', 1, 'ROI time series file')))
   fn[[1]] <- readline(sprintf("No. 1 ROI time series file name: "))
	print(sprintf("No. %i file just read in: %s", 1, strsplit(fn[[1]], "/")[[1]][length(strsplit(fn[[1]], "/")[[1]])]))
   myData <- data.frame(matrix(data=NA, nrow=dim(read.table(fn[[1]], header=FALSE))[1], ncol=nROIs, dimnames = NULL))
   myData[,1] <- read.table(fn[[1]], header=FALSE)
   for (ii in 2:nROIs) { # read ROI 1D file (in 1 column)
#   fn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#      "{{ROI files in one-column format} {.1D}} {{All files} *}",
#      title = paste('Choose number', ii, 'ROI time series file')))
   fn[[ii]] <- readline(sprintf("No. %i ROI time series file name: ", ii))
	print(sprintf("No. %i file just read in: %s", ii, strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])]))
	myData[,ii] <- read.table(fn[[ii]], header=FALSE)    
   } #for (ii in 2:nROIs)
	for (ii in 1:nROIs) names(myData)[ii] <- readline(sprintf("Name for region/node number %i? ", ii))
# take labels from the filenames
#   ROIlab <- vector('list', nROIs)
#   for (ii in 1:nROIs) ROIlab[[ii]] <- strsplit(strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])], ".1D")[[1]]
#   names(myData) <- ROIlab
   nTotal <- dim(myData)[1]
}
print("#++++++++++++++++++++++++++++++++++++++++++++")
print("If there are n consecutive chunks of data, enter n-1 here.")
print("If all the time series are consecutive, enter 0 breaks.")
nChunks <- as.integer(readline("Number of breaks in the time series? "))+1     # number of runs

nPts <- array(data = NA, dim=nChunks)
if (nChunks == 1) nPts[1] <- nTotal else
for (ii in 1:nChunks) nPts[ii] <- as.integer(readline(paste("Length of number", ii, "run/segment? ")))

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
#      fncov <- tclvalue( tkgetOpenFile( filetypes = 
#      "{{Covariate File} {.1D}} {{All files} *}",
#      title = paste('Choose covariates file in multi-column format')))
      fncov <- readline("Covariates file name: ")
		covHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? "))
		if (covHeader == 1) exData <- read.table(fncov, header=TRUE) else {
         exData <- read.table(fncov, header=FALSE)
         for (ii in 1:nCOVs) names(exData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
      }
   } else {
#      covn <- vector('list', nCOVs)
      exData <- data.frame(matrix(data=NA, nrow=nTotal, ncol=nCOVs, dimnames = NULL))
      exData <- readMultiFiles(nCOVs, 1, "covariate")  # 1: assuming no header
#		covFN <- exTmp[[1]]; exData <- exTmp[[2]]
			
#		for (ii in 1:nCOVs) {
#         covn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#            "{{Covariate Files} {.1D}} {{All files} *}",
#            title = paste('Choose number', ii, 'covariate time series file')))
#         print(sprintf("No. %i file just read in: %s", ii, strsplit(covn[[ii]], "/")[[1]][length(strsplit(covn[[ii]], "/")[[1]])]))
#			exData[,ii] <- read.table(covn[[ii]], header=FALSE)    
#      }
      for (ii in 1:nCOVs) names(exData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
#		COVlab <- vector('list', nCOVs)
#      for (ii in 1:nCOVs) COVlab[[ii]] <- strsplit(covFN[[ii]], ".1D")[[1]]
#      names(exData) <- COVlab
   }
} else {exData <- NULL; nCOVs <- 0}
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
   for (ii in 1:nChunks) {
      for (jj in 1:nROIs) newData[(jumpPts+1):(jumpPts+nPts[ii]),jj] <- 
         myData[(jumpPts+1):(jumpPts+nPts[ii]),jj]/mean(myData[(jumpPts+1):(jumpPts+nPts[ii]),jj])
	  if (ii < nChunks) jumpPts <- jumpPts+nPts[ii]
	}
}

# create exogenous variables with Legendre polynomials from gsl
if (nPoly > -1) {
   trendMat <- as.data.frame(array(0, dim = c(nTotal, (nPoly+1)*nChunks)))
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
if (nChunks > 1) {
	breakMat <- as.data.frame(array(0, dim = c(nTotal, (nChunks-1)*nLags)))
	jumpPts <- 0
	for (ii in 1:(nChunks-1)) {
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
if (archPlot) {dev.new(); plot(arch.test(fm))}
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
   dev.new(); plot(stability(fm, type = procType, h = 0.15, dynamic = FALSE, rescale = TRUE))
   anotherType <- as.integer(readline("Want to plot stability with another type (0: no; 1: yes)? "))
   } # while (anotherType)
} # if (statPlot)
	
print("-----------------")	
checkCov <- as.integer(readline("Check significance of covariates (0: no; 1: yes)? "))
if (checkCov) {
   anotherCovPth <- TRUE
	totCOVs <- (nPoly+1)*nChunks+nCOVs  # total covariates
   while (anotherCovPth) {
	   pCovThresh <- as.numeric(readline("p-threshold for covariates (e.g., 0.05)? "))
		#Info about all the covariates:
		#lapply(coef(fm), function(x) x[nROIs*nLags+(1:totCOVs),])
		covPList <- lapply(coef(fm), function(x) x[nROIs*nLags+(1:totCOVs),4]<=pCovThresh)
		#covSigList <- vector(mode="logical", totCOVs)
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
	saveMatT <- as.integer(readline("Save matrix of t values for group analysis (0: no; 1: yes)? "))
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
#	totCOVs <- (nPoly+1)*nChunks+nCOVs
#	fCovTest <- vector("list", totCOVs)  # initialization for covariate F test name list
#	for (ii in 1:totCOVs)
#	   fCovTest[[ii]] <- names(myData)[ii],jj)
		
		
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

if (plotFit) { dev.new(); plot(fm) }

anotherPth <- TRUE
while (anotherPth) {

print(sprintf("There are totally %i paths in the model. Select a low p value ", nLags*nROIs^2))
print("if you're concerned about multiple comparisons issue:")
pThresh <- as.numeric(readline("p-threshold for causal effects (e.g., 0.05)? "))

# connection goes from row to column, which is the default in network package
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
   print(sprintf("Path matrix with a lag of %i with insignificant ones masked with NAs:", ii))
#   print(matrix(netMatR[ii,,]*netMat[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
   print(matrix(mapply(function(x,y) ifelse(x, y, NA), netMat[ii,,], netMatR[ii,,]), nrow = nROIs, ncol = nROIs, 
      dimnames = list(names(myData), names(myData))))
   print("-----------------")
	print(sprintf("Matrix of t values with a lag of %i with insignificant ones masked with NAs:", ii))
#   print(matrix(netMatT[ii,,]*netMat[ii,,], nrow = nROIs, ncol = nROIs, dimnames = list(names(myData), names(myData))))
   print(matrix(mapply(function(x,y) ifelse(x, y, NA), netMat[ii,,], netMatT[ii,,]), nrow = nROIs, ncol = nROIs, 
      dimnames = list(names(myData), names(myData))))
	print(sprintf("DFs = %i for Ho: a path coefficient = 0.", summary(fm)$varresult[[1]]$df[2]))
   print("-----------------")
}

if (nLags>1) {
   print(sprintf("Overall F matrix for all %i lags with insignificant ones masked with NAs:", nLags))
#	print(netCMatF*(netCMatP<=pThresh))
   print(matrix(mapply(function(x,y) ifelse(x, y, NA), netCMatP<=pThresh, netCMatF), nrow = nROIs, ncol = nROIs, 
      dimnames = list(names(myData), names(myData))))
	print("-----------------")	
	print(sprintf("Overall p-value matrix for all %i lags with insignificant ones masked with NAs:", nLags))
#	print(netCMatP*(netCMatP<=pThresh))
   print(matrix(mapply(function(x,y) ifelse(x, y, NA), netCMatP<=pThresh, netCMatP), nrow = nROIs, ncol = nROIs, 
      dimnames = list(names(myData), names(myData))))
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

print("It's suggested to run a separate group analysis for each lag.")
print("-----------------")
print("The following from subjects can be taken for group analysis:")
print("(1) Both path coefficients and their t-statistics (highly recommended); or")
print("(2) Path coefficients only (OK but NOT as reliable as (1); or")
print("(3) t-statistics only (NOT recommended); or")
print("(4) F-statistics only (NOT recommended).")
print("-----------------")

grpType <- as.integer(readline("Which type of input files (1: path and t; 2: path only; 3: t only; 4: F only)? ")) 

#fishConv <- TRUE  # Fisher transformation
#if(fishConv) libLoad("psych")

doneGrp <- 1
while (doneGrp) {

nGrp <- as.integer(readline("Number of groups (1 or 2)? "))
if (nGrp==1) {   #one-sample t

nSubjs <- as.integer(readline("Number of subjects (e.g., 12)? "))     # number of subjects
#gfn <- vector('list', nSubjs)
pathList <- vector('list', nSubjs)

if (grpType==1) {  # Fisher tranformation not done since it doesn't seem to matter much in testing
   print("Provide path coef files first:")
   print("-----------------")
   pathList <- readMultiFiles(nSubjs, 2, "subject path coef matrix")  #2: with header
   print("-----------------")
   print("Now provide t-statistic files:")
   pathTList <- readMultiFiles(nSubjs, 2, "subject t-statistic matrix") # get t values
   pathSEList <- mapply("/", pathList, pathTList, SIMPLIFY = FALSE)  # calculate stand. error
   zMat <- do.call(rbind, lapply(lapply(pathList, as.matrix), c))  # convert to matrix
   zSEMat <- do.call(rbind, lapply(lapply(pathSEList, as.matrix), c)) # convert se to matrix
   runMeta <- TRUE
}

if (grpType==2) {  # Fisher tranformation not done since it doesn't seem to matter much in testing
   pathList <- readMultiFiles(nSubjs, 2, "subject path coef matrix")  #2: with header
   zMat <- do.call(rbind, lapply(lapply(pathList, as.matrix), c))  # convert to matrix
   runMeta <- FALSE
}

if (grpType==3) {  # added here simply because other people still practice this approach
   pathList <- readMultiFiles(nSubjs, 2, "subject t-statistic matrix")  #2: with header
   pathList <- lapply(pathList, function(x) log(x^2))  # convert to F, then take log transform
   zMat <- do.call(rbind, lapply(lapply(pathList, as.matrix), c))  # convert to matrix
   runMeta <- FALSE
}

if (grpType==4) {  # added here simply because other people still practice this approach
   pathList <- readMultiFiles(nSubjs, 2, "subject F-statistic matrix")  #2: with header
   pathList <- lapply(pathList, log)  # take log transform
   zMat <- do.call(rbind, lapply(lapply(pathList, as.matrix), c))  # convert to matrix
   runMeta <- FALSE
}

#for (ii in 1:nSubjs) { # read in path matrices
#   fn[[ii]] <- tclvalue( tkgetOpenFile( filetypes = 
#      "{{Path coef matrix file} {.1D}} {{All files} *}",
#      title = paste('Choose subject No.', ii, 'subject path coef matrix file')))
#   print(sprintf("No. %i file just read in: %s", ii, strsplit(fn[[ii]], "/")[[1]][length(strsplit(fn[[ii]], "/")[[1]])]))
#	 pathList[[ii]] <- read.table(fn[[ii]], header=TRUE)
#}

nROIsG <- dim(pathList[[1]])[1]
roiNames <- names(pathList[[1]])

if (runMeta) {
#   libLoad("meta")
#   if(fishConv) {zMat <- fisherz(zMat); zSEMat <- fisherz(zSEMat)}
   resList <- vector('list', nROIsG^2)  # memory allocation
#   for (ii in 1:nROIsG^2) resList[[ii]] <- metagen(zMat[,ii], zSEMat[,ii]) # meta analysis
#   for (ii in 1:nROIsG^2) resList[[ii]] <- rma(zMat[,ii], zSEMat[,ii]^2) # meta analysis
   for (ii in 1:nROIsG^2) resList[[ii]] <- rma(zMat[,ii], zSEMat[,ii]^2, method="REML") # meta analysis
#   zOutList <- lapply(lapply(resList, summary), function(x) x$random$TE) # extract group z-score
   zOutList <- lapply(resList, function(x) as.numeric(x$b))
#   pList <- lapply(lapply(resList, summary), function(x) x$random$p) # extract p
   pList <- lapply(resList, function(x) as.numeric(x$zp)) # extract p
#   qList <- lapply(resList, function(x) x$QE)  # Q for homogeneity test
   qList <- lapply(resList, function(x) x$QEp)  # p-value of Q for homogeneity tes
   grpR <- matrix(unlist(zOutList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
   grpP <- matrix(unlist(pList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
   grpQ <- matrix(unlist(qList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
   
} else {  # no meta analysis: one-sample t-test
#   resList <- apply(do.call(rbind, lapply(lapply(zList, as.matrix), c)), 2, t.test)
#   if(fishConv) zMat <- fisherz(zMat)
#   for (ii in 1:nROIsG^2) resList[[ii]] <- t.test(zMat[,ii])
   resList <- apply(zMat, 2, t.test)
   zOutList <- lapply(resList, function(x) as.numeric(x$estimate))
	pList <- lapply(resList, function(x) as.numeric(x$p.value))
   grpR <- matrix(unlist(zOutList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
#   if(fishConv) grpR <- fisherz2r(grpZ)   # convert z to r
   grpP <- matrix(unlist(pList), nrow=nROIsG, ncol=nROIsG, dimnames = list(roiNames, roiNames))
} # if (runMeta)
   print("-----------------")
	print("Group path matrix (direction goes from row to column):")
   print(grpR)
	print("-----------------")	
	saveRMat <- as.integer(readline("Save the above path matrix (0: no; 1: yes)? "))
      if (saveRMat) {
         matTName <- as.character(readline("File name prefix for path matrix? "))
         write.table(grpR, file=sprintf("%s.1D", matRName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }	
	print("-----------------")
	print("Group p matrix (direction goes from row to column):")
   print(grpP)
	print("-----------------")
   savePMat <- as.integer(readline("Save the above p matrix (0: no; 1: yes)? "))
      if (savePMat) {
         matPName <- as.character(readline("File name prefix for p matrix? "))
         write.table(grpP, file=sprintf("%s.1D", matPName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }	
   print("-----------------")
   if (runMeta) {
      print("p-value matrix for cross-subject hetetrogeneity/variability (direction goes from row to column):")
      print(grpQ)
   }
   print("-----------------")
   anotherPthG <- TRUE
   while (anotherPthG) {
						
   pThreshG <- as.numeric(readline("p-threshold for group analysis (e.g., 0.05)? "))
   surviveR <- as.numeric(grpP<=pThreshG)*grpR  # for network plotting
   showSigR <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpP<=pThreshG, grpR), nrow=nROIsG, ncol=nROIsG, 
      dimnames = list(roiNames, roiNames))
#surviveP <- as.numeric(grpP<=pThreshG)*grpP
   showSigP <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpP<=pThreshG, grpP), nrow=nROIsG, ncol=nROIsG, 
      dimnames = list(roiNames, roiNames))
   if (runMeta) showSigQ <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpQ<=pThreshG, grpQ), nrow=nROIsG,
      ncol=nROIsG, dimnames = list(roiNames, roiNames))   
   print("Group path matrix with insignificant ones masked with NAs:")
   print(showSigR)
   print("-----------------")
   print("Group p matrix with insignificant ones masked with NAs:")
   print(showSigP)
   print("-----------------")	
   if (runMeta) {
      print("p-value matrix for cross-subject hetetrogeneity/variability with insignificant ones masked with NAs:")
      print(showSigQ)
   }
   print("-----------------")				
					
   plotNetG <- as.integer(readline("Plot out the identified network (0: no; 1: yes)? "))
   if (plotNetG) {
      print("The thickness of a path indicates its relative strength (or effect).")
      print("A path in red means positive strength (or effect) while blue is the opposite.")
      set.seed(1702)
      net <- network.initialize(nROIsG)
      attr(net, "vertex.names") <- names(pathList[[1]])
      edgeScaleG <- as.numeric(readline("Scale factor for path thickness (e.g., 1)? "))
      arrowScaleG <- as.numeric(readline("Scale factor for arrow size (e.g., 2)? "))
      selfLoop <- as.integer(readline("Show self-loops in the network (0: no; 1: yes)? "))
      	plotNet(surviveR, selfLoop, surviveR*edgeScaleG, arrowScaleG, 3-sign(surviveR), sprintf("Group network with %s subjects", nSubjs))
   } # if (plotNetG)

   anotherPthG <- as.integer(readline("Want to try another p-threshold/plotting set-up for group network (0: no; 1: yes)? "))
}
} else {  # more than one group

nSubjs <- vector('integer', nGrp)
pathList <- vector('list', nGrp)
pathTList <- vector('list', nGrp)
pathSEList <- vector('list', nGrp)
nROIsG <- vector('integer', nGrp)
roiNames <- vector('list', nGrp)
zList <- vector('list', nGrp)
zMat <- vector('list', nGrp)
zSEMat <- vector('list', nGrp)
DF <- vector('list', nGrp)

for (ii in 1:nGrp) {  # Fisher tranformation not done since it doesn't seem to matter much in testing
	nSubjs[ii] <- as.integer(readline(sprintf("Number of subjects in group %s (e.g., 12)? ", ii)))
   pathList[[ii]] <- vector('list', nSubjs[ii])
	if (grpType==1 ) {   
      print("Provide path coef files first:")
      print("-----------------")
      pathList[[ii]] <- readMultiFiles(nSubjs[ii], 2, "subject path coef matrix")
      print("-----------------")
      print("Now provide t-statistic files in the SAME subject order as path coef's:")
      print("-----------------")   
      pathTList[[ii]] <- readMultiFiles(nSubjs[ii], 2, "subject t-statistic matrix")
      pathSEList[[ii]] <- mapply("/", pathList[[ii]], pathTList[[ii]], SIMPLIFY = FALSE)  # calculate stand. error
      zMat[[ii]] <- do.call(rbind, lapply(lapply(pathList[[ii]], as.matrix), c))  # convert to matrix
      zSEMat[[ii]] <- do.call(rbind, lapply(lapply(pathSEList[[ii]], as.matrix), c)) # convert se to matrix
      runMeta <- TRUE
#      print("-----------------") 
#      DF[[ii]] <- as.integer(readline(sprintf("Degrees of freedom for the t-statistics in group %s? ", ii)))
      print("-----------------")
   }
   if (grpType==2) {  # Fisher tranformation not done here since it doesn't seem to matter much in testing
      pathList[[ii]] <- readMultiFiles(nSubjs[ii], 2, "subject path coef matrix")  #2: with header
      zMat[[ii]] <- do.call(rbind, lapply(lapply(pathList[[ii]], as.matrix), c))  # convert to matrix
      runMeta <- FALSE
   }

   if (grpType==3) {  # added here simply because other people still practice this approach
      pathList[[ii]] <- readMultiFiles(nSubjs, 2, "subject t-statistic matrix")  #2: with header
      pathList[[ii]] <- lapply(pathList[[ii]], function(x) log(x^2))  # convert to F, then take log transform
      zMat[[ii]] <- do.call(rbind, lapply(lapply(pathList[[ii]], as.matrix), c))  # convert to matrix
      runMeta <- FALSE
   }

   if (grpType==4) {  # added here simply because other people still practice this approach
      pathList[[ii]] <- readMultiFiles(nSubjs, 2, "subject F-statistic matrix")  #2: with header
      pathList[[ii]] <- lapply(pathList[[ii]], log)  # take log transform
      zMat[[ii]] <- do.call(rbind, lapply(lapply(pathList[[ii]], as.matrix), c))  # convert to matrix
      runMeta <- FALSE
}
      
	nROIsG[ii] <- dim(pathList[[ii]][[1]])[1]
	if (ii>1) for (jj in 1:ii) if (nROIsG[jj]!=nROIsG[1]) {sprintf("Group %i has %i ROIs while group 1 has %i", jj, nROIsG[jj], nROIsG[1]); break}
	roiNames[[ii]] <- names(pathList[[ii]][[1]])
	if (ii>1) for (jj in 1:ii) if (!identical(roiNames[[ii]], roiNames[[1]])) {sprintf("Group %i has different ROI namess with group 1", jj, roiNames[[ii]]); break}
#	if (pthType==0) zList[[ii]] <- lapply(pathList[[ii]], fisherz) # sapply(pathList[[ii]], fisherz, simplify = FALSE)
#  if (pthType==1) zList[[ii]] <- lapply(pathList[[ii]], function(x) log(x^2))
#  if (pthType==2) zList[[ii]] <- lapply(pathList[[ii]], log)
#    zMat[[ii]] <- do.call(rbind, lapply(lapply(zList[[ii]], as.matrix), c))  # convert from list to matrix for easier handling when running 2-sample t-test
}   

# Instead of looping, we can also use the following aggregation approach: No, it doesn't work!!!
# resList <- apply(do.call(rbind, lapply(lapply(zList[[1]], as.matrix), c)), 2, t.test, do.call(rbind, lapply(lapply(zList[[2]], as.matrix), c)))


if (runMeta) {
#   libLoad("meta")
#   if(fishConv) {zMat <- lapply(zMat, fisherz); zSEMat <- lapply(zSEMat, fisherz)}   
   resList <- vector('list', nROIsG[1]^2)  # memory allocation
#   for (ii in 1:nROIsG[1]^2) resList[[ii]] <- metacont(rep.int(DF[[1]]+1, 19), zMat[[1]][,ii], zSEMat[[1]][,ii], rep.int(DF[[2]]+1, 18), zMat[[2]][,ii], zSEMat[[2]][,ii]) # meta analysis
   for (ii in 1:nROIsG[1]^2) resList[[ii]] <- rma(c(zMat[[1]][,ii], zMat[[2]][,ii]), c(zSEMat[[1]][,ii]^2, zSEMat[[2]][,ii]^2), 
       mods=c(rep(0, nSubjs[1]), rep(1, nSubjs[2])), method="REML") # meta analysis: REML is selected here! Group2-Group1

   zOutList <- lapply(resList, function(x) as.numeric(x$b[2])) # extract effect of group2-group1
   z1OutList <- lapply(resList, function(x) as.numeric(x$b[1])) # extract effect of group1, intercept
   z2OutList <- mapply("+", zOutList, z1OutList, SIMPLIFY = FALSE)  # effect of group2 = sum of two above
   pList <- lapply(resList, function(x) as.numeric(x$zp[2])) # extract p for effect of group2-group1
   p1List <- lapply(resList, function(x) as.numeric(x$zp[1])) # extract p for group1 effect
      
   varList <- mapply("^", lapply(resList, function(x) as.numeric(x$se[2])), 2, SIMPLIFY = FALSE) # variance of group2-group1
   var1List <- mapply("^", lapply(resList, function(x) as.numeric(x$se[1])), 2, SIMPLIFY = FALSE) # variance of group1   
   # se(grp2)=sqrt(se(grp2-grp1)^2-se(grp1)^2)
   zval2List <- mapply("/", z2OutList, lapply(mapply("-", varList, var1List, SIMPLIFY = FALSE), sqrt), SIMPLIFY = FALSE) # z-value of group2
   p2List <- lapply(zval2List, function(x) 2*(1-pnorm(abs(x))))   # p-value of group2
   qList <- lapply(resList, function(x) x$QEp)  # p-value of Q for homogeneity test
   grpR <- matrix(unlist(zOutList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grp1R <- matrix(unlist(z1OutList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grp2R <- matrix(unlist(z2OutList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grpP <- matrix(unlist(pList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grp1P <- matrix(unlist(p1List), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grp2P <- matrix(unlist(p2List), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grpQ <- matrix(unlist(qList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   
} else {  # no meta analysis   
   # two-sample t-test: group2 - group1
   resList <- vector('list', nROIsG[1]^2)
# want equal variance assumption, otherwise DF would be different from one test to another!
   for (ii in 1:nROIsG[1]^2) resList[[ii]] <- t.test(zMat[[2]][,ii], zMat[[1]][,ii], paired=FALSE, var.equal=TRUE)  # 2nd-1st   
   zOutList <- lapply(resList, function(x) as.numeric(x$estimate))
	pList <- lapply(resList, function(x) as.numeric(x$p.value))
   grpR <- matrix(unlist(zOutList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   grpP <- matrix(unlist(pList), nrow=nROIsG[1], ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))     
} # if (runMeta)

   print("NOTE!!! The results here are about the contrast of the two groups: Group2 - Group1.")
   print("That is, they should be interpreted as the amount Group2 is greater than Group1.")
   print("They make sense only when comparing to the two individual networks from the two groups.")

   print("-----------------")
   if (runMeta) {
      print("Group1 path matrix (direction goes from row to column):")
      print(grp1R)
      print("-----------------")
      saveRMat <- as.integer(readline("Save Group1 path matrix (0: no; 1: yes)? "))
         if (saveRMat) {
            matTName <- as.character(readline("File name prefix for path matrix? "))
            write.table(grp1R, file=sprintf("%s.1D", matRName), append=FALSE, row.names=TRUE, col.names=TRUE)
         }
      print("-----------------")   
      print("Group2 path matrix (direction goes from row to column):")
      print(grp2R)
      print("-----------------")
      saveRMat <- as.integer(readline("Save Group2 path matrix (0: no; 1: yes)? "))
      if (saveRMat) {
         matTName <- as.character(readline("File name prefix for path matrix? "))
         write.table(grp2R, file=sprintf("%s.1D", matRName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }
      print("-----------------")
   }
	print("Group2-Group1 path matrix (direction goes from row to column):")
   print(grpR)
	print("-----------------")	
	saveRMat <- as.integer(readline("Save Group2-Group1 path matrix (0: no; 1: yes)? "))
      if (saveRMat) {
         matTName <- as.character(readline("File name prefix for path matrix? "))
         write.table(grpR, file=sprintf("%s.1D", matRName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }	
	print("-----------------")
   if (runMeta) {
      print("Group1 p matrix (direction goes from row to column):")
      print(grp1P)
      print("-----------------")
      savePMat <- as.integer(readline("Save Group1 p matrix (0: no; 1: yes)? "))
         if (savePMat) {
            matPName <- as.character(readline("File name prefix for p matrix? "))
            write.table(grp1P, file=sprintf("%s.1D", matPName), append=FALSE, row.names=TRUE, col.names=TRUE)
         }	
      print("-----------------")
      print("Group2 p matrix (direction goes from row to column):")
      print(grp2P)
      print("-----------------")
      savePMat <- as.integer(readline("Save Group2 p matrix (0: no; 1: yes)? "))
         if (savePMat) {
            matPName <- as.character(readline("File name prefix for p matrix? "))
            write.table(grp2P, file=sprintf("%s.1D", matPName), append=FALSE, row.names=TRUE, col.names=TRUE)
         }	
      print("-----------------")
   }
   print("Group2-Group1 p matrix (direction goes from row to column):")
   print(grpP)
	print("-----------------")
   savePMat <- as.integer(readline("Save Group2-Group1 p matrix (0: no; 1: yes)? "))
      if (savePMat) {
         matPName <- as.character(readline("File name prefix for p matrix? "))
         write.table(grpP, file=sprintf("%s.1D", matPName), append=FALSE, row.names=TRUE, col.names=TRUE)
      }	
   print("-----------------")
   if (runMeta) {
      print("p-value matrix for cross-subject hetetrogeneity/variability (direction goes from row to column):")
      print(grpQ)
   }
   print("-----------------")
     
   anotherPthG <- TRUE
   while (anotherPthG) {
						
   pThreshG <- as.numeric(readline("Two-tailed p-threshold for group analysis (e.g., 0.05)? "))
   surviveR <- as.numeric(grpP<=pThreshG)*grpR   # for plotting
   showSigR <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpP<=pThreshG, grpR), nrow=nROIsG[1], 
      ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   showSigP <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpP<=pThreshG, grpP), nrow=nROIsG[1], 
      ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
   if (runMeta) {
      survive1R <- as.numeric(grp1P<=pThreshG)*grp1R
      survive2R <- as.numeric(grp2P<=pThreshG)*grp2R
      showSig1R <- matrix(mapply(function(x,y) ifelse(x, y, NA), grp1P<=pThreshG, grp1R), nrow=nROIsG[1], 
         ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
      showSig2R <- matrix(mapply(function(x,y) ifelse(x, y, NA), grp2P<=pThreshG, grp2R), nrow=nROIsG[1], 
         ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
      showSig1P <- matrix(mapply(function(x,y) ifelse(x, y, NA), grp1P<=pThreshG, grp1P), nrow=nROIsG[1], 
         ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
      showSig2P <- matrix(mapply(function(x,y) ifelse(x, y, NA), grp2P<=pThreshG, grp2P), nrow=nROIsG[1], 
         ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))
      showSigQ <- matrix(mapply(function(x,y) ifelse(x, y, NA), grpQ<=pThreshG, grpQ), nrow=nROIsG[1],
         ncol=nROIsG[1], dimnames = list(roiNames[[1]], roiNames[[1]]))   
      print("Group1 path matrix with insignificant path masked with NAs:")
      print(showSig1R)
      print("-----------------")
      print("Group2 path matrix with insignificant path masked with NAs:")
      print(showSig2R)
      print("-----------------")
   }
   print("Group2-Group1 path matrix with insignificant path differences masked with NAs:")
   print(showSigR)
   if (!runMeta) print(sprintf("DFs = %i", round(as.numeric(resList[[1]]$parameter))))
   print("-----------------")
   if (runMeta) {
      print("Group1 p matrix with insignificant path masked with NAs:")
      print(showSig1P)
      print("-----------------")
      print("Group2 p matrix with insignificant path masked with NAs:")
      print(showSig2P)
      print("-----------------")
   }
   print("Group2-Group1 p matrix with insignificant path differences masked with NAs:")
   print(showSigP)
   print("-----------------")
   if (runMeta) {
      print("p-value matrix for cross-subject hetetrogeneity/variability with insignificant ones masked with NAs:")
      print(showSigQ)
   }
   print("-----------------")				
					
   plotNetG <- as.integer(readline("Plot out the identified network (0: no; 1: yes)? "))
   if (plotNetG) {
      print("The thickness of a path indicates the difference magnitude.")
      print("A path in red means positive difference while blue is the opposite.")
      set.seed(1702)
      net <- network.initialize(nROIsG[1])
      attr(net, "vertex.names") <- names(pathList[[1]][[1]])
      edgeScaleG <- as.numeric(readline("Scale factor for path thickness (e.g., 1)? "))
      arrowScaleG <- as.numeric(readline("Scale factor for arrows (e.g., 2)? "))
      selfLoop <- as.integer(readline("Show self-loops in the network (0: no; 1: yes)? "))
	 # if path +, col<-2 (red); if path -, col<-4 (blue)
   	if (runMeta) {
         plotNet(survive1R, selfLoop, survive1R*edgeScaleG, arrowScaleG, 3-sign(survive1R), "Network of Group1")
         plotNet(survive2R, selfLoop, survive2R*edgeScaleG, arrowScaleG, 3-sign(survive2R), "Network of Group2")
      }
      plotNet(surviveR, selfLoop, surviveR*edgeScaleG, arrowScaleG, 3-sign(surviveR), "Network of Group2-Group1")
   } # if (plotNetG)
  
anotherPthG <- as.integer(readline("Want to try another p-threshold/plotting set-up for group network (0: no; 1: yes)? "))
} # while (anotherPthG)

} # more than one group

doneGrp <- as.integer(readline("Next (0: done; 1: another group analysis)? "))
} # while (doneGrp)

} # if (anaType==2 || anotherAna==2)

if (anaType==0 || doneGrp==0 || anotherAna==0) {
   print("Make sure to save all desirable figures before leaving!")
   quitMe <- as.integer(readline("Quit R (0: no; 1: yes)? "))
	print("***********Thanks for using the program!***********")
	print("Any feedback will be welcome - Gang Chen (gangchen@mail.nih.gov)")
   if (quitMe) {dev.off(); q()}
}
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 3dMEMA.R ==================          ")
print("AFNI Mixed-Effects Meta Analysis Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.1,  July 3, 2009")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website: http://afni.nimh.nih.gov/sscc/gangc/3dMetaAna.html")
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
  if (all(values$BRICK_TYPES==0) | all(values$BRICK_TYPES==1)) myttt<- readBin(conbrik, "int", n=dx*dy*dz*dt, size=size, signed=TRUE, endian=endian) # unsigned charater or short
  if (all(values$BRICK_TYPES==3)) myttt<- readBin(conbrik, "numeric", n=dx*dy*dz*dt, size=size, signed=TRUE, endian=endian) # float        
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


### modified rma version 0.562 by Wolfgang Viechtbauer, Department of Methodology and Statistics, University of Maastricht
# Further modifications

rma <- function(yi, vi, mods=NULL, method="REML", addint=TRUE, ci=95, digits=4, btt=NULL, tau2=NULL, knha=FALSE, subset=NULL, ll=FALSE, control = list()) {

# addint: add intercept? (yes)
# btt: ??? part of regressors for computing QME?
# knha: Knapp & Hartung method
# subset: only analyze part of the data; can be dropped for my case


   k     <- length(yi)   # number of data points
   ids   <- 1:k
   
   ########## in case mods is a vector!!! But I'll REMOVE this when dealing with those 0s!!!!!!!!!!!!!!! $$$$$$$$$$$$$
#   mods <- as.matrix(mods)   

   #browser()
   if (sum(vi <= 0) > 0) {  # take it out of the rma?
      null.ids <- which(vi<=0)
      yi    <- yi[-c(null.ids)]
      vi    <- vi[-c(null.ids)]
      mods  <- mods[-c(null.ids), , drop=FALSE]
      ids   <- ids[-c(null.ids)]
      k     <- length(yi)   
#      warning("Outcomes with non-positive sampling variances have been excluded from the analysis.")
   }
  
#   if (is.null(mods) && addint == FALSE) {
#      warning("Must either include an intercept (addint=TRUE) and/or moderators in model. Coerced intercept into the model.")
#      addint <- TRUE
#   }
   
	Y <- as.matrix(yi)
	
	if (addint == TRUE) {
      X <- cbind(intrcpt=rep(1,k), mods)
   } else {
      X <- mods
   }
   # browser()
   p <- dim(X)[2]   # number of regressors (fixed effects): excluding the intercept

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

#   se.tau2  <- NA
   I2       <- NA
   s2w      <- NA
      
   if (is.null(tau2) == TRUE) {

#   	if (method == "HE") {
#   		P      <- diag(k) - X %*% solve(t(X) %*% X) %*% t(X)
#   		RSS    <- t(Y) %*% P %*% Y
#   		tau2   <- ( RSS - tr( P %*% diag(vi) ) ) / (k-p)
#   	}
   
     if (method == "DL") {
        wi     <- 1/vi
        W      <- diag(wi)
        P      <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
        RSS    <- t(Y) %*% P %*% Y
        tau2   <- ( RSS - (k-p) ) / tr(P)
     }
  
#     if (method == "SJ") {
#        tau2.0 <- var(yi) * (k-1)/k
#        wi     <- 1/(vi + tau2.0)
#        W      <- diag(wi)
#        P      <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
#        tau2   <- (tau2.0/(k-p)) * t(Y) %*% P %*% Y
#     }
  
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
   			iter     <- iter + 1
   			tau2.old <- tau2
   			wi       <- 1/(vi + tau2)
   			W		   <- diag(wi)
   			P		   <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   			if (method == "REML") {
   				adj	<- solve( tr(P%*%P) ) %*% ( t(Y)%*%P%*%P%*%Y - tr(P) )
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

         if (method == "REML") {
            se.tau2 <- sqrt( 2/tr(P%*%P) )
         }
   	
      }
   
   	tau2 <- max(0, c(tau2))

   }

   wi    <- 1/vi
   W     <- diag(wi)
   P     <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
   QE    <- t(Y) %*% P %*% Y

   if (int.only == TRUE) {
      s2  <- (k-1)*sum(wi) / ( sum(wi)^2 - sum(wi^2) )
      I2  <- 100 * tau2 / (tau2 + s2)
   }

      wi  <- 1/(vi + tau2)
   	W   <- diag(wi)
   	b   <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
   	vb  <- solve(t(X) %*% W %*% X)

	if (knha == TRUE) {  # Knapp & Hartung method
      P     <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
      s2w   <- c( t(Y) %*% P %*% Y ) / (k-p)
     vb    <- s2w * vb
  }

#  if (is.null(btt)) {
#     if (p > 1) {
#        if (addint == TRUE) {
#           btt <- 2:p
#        } else {
#           btt <- 1:p
#        }
#     } else {
#         btt <- 1
#      }
#   }

#   m <- length(btt)

#	QME <- t(b)[btt] %*% solve(vb[btt,btt]) %*% b[btt]
	
#	if (knha == FALSE) {
#	  QMEp <- 1 - pchisq(QME, df=m)
#	} else {
#	  QMEp <- 1 - pf( QME/m, df1=m, df2=k-p )
#	}
   
   se    <- sqrt(diag(vb))
	z     <- b / se


#   res         <- list(b, se, z, zp, ci.lb, ci.ub, vb, tau2, se.tau2, k, p, m, fit.stats, QE, QEp, QME, QMEp, s2w, I2, int.only, yi, vi, X, ids, method, knha, btt, addint, digits, ci, ll, control)
   res         <- list(b, se, z, tau2, QE)

#   names(res)  <- c("b", "se", "z", "zp", "ci.lb", "ci.ub", "vb", "tau2", "se.tau2", "k", "p", "m", "fit.stats", "QE", "QEp", "QME", "QMEp", "s2w", "I2", "int.only", "yi", "vi", "X", "ids", "method", "knha", "btt", "addint", "digits", "ci", "ll", "control")
   names(res)  <- c("b", "se", "z", "tau2", "QE")

   class(res)  <- c("rma")
   res

}


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


tolL <- 1e-7 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's
tolU <- 1e6  # upper tolerance for those variances of 0
tTop <- 100   # upper bound for t-statistic

#options(show.error.messages = FALSE)  # suppress error message when running with single processor

runRMA <- function(inData, dummy, nGrp, outData, rma, KHtest, nNonzero, nCov, nBrick, tol) {  
   fullLen <- length(inData); halfLen <- fullLen/2  # remove this line by providing halfLen as function argument later?
   Y <- inData[1: halfLen]; V <- inData[(halfLen +1): fullLen]
   if((sum(abs(Y)>tol)>2) & (sum(abs(V)>tol)>nNonzero)) {  # run only when there are more than 2 non-zeros in both Y and V
   #if((sum(abs(Y)>tol)>2) & (all(abs(V)>tol))) {  # run only when there are more than 2 non-zeros in Y
   tag <- TRUE
   try(resList <- rma(Y, V, mods=dummy, method="REML", knha=KHtest), tag <- FALSE)
   if(!tag) {tag <- TRUE; try(resList <- rma(Y, V, mods=dummy, method="DL", knha=KHtest), tag <- FALSE)}  # if REML fails, try 2nd chance with Hedges   

   if (tag) {
   if (nGrp==1) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+1] <- resList$b[ii+1]
         outData[2*ii+2] <- resList$z[ii+1]   
      } # for(ii in 1:nCov)
   } # fif (nGrp==1)
   if (nGrp==2) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      outData[5] <- resList$b[2]  # beta of group2-group1
      outData[6] <- resList$z[2]  # z score of group2-group1
      outData[3] <- outData[1]+outData[5]  # beta of group2
      outData[4] <- outData[3]/sqrt(resList$se[2]^2-resList$se[1]^2) # z-score of group2
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+5] <- resList$b[ii+2]
         outData[2*ii+6] <- resList$z[ii+2]   
      } # for(ii in 1:nCov)
   } # if (nGrp==2)
    
   outData[nBrick-1] <- resList$tau2
   outData[nBrick] <- resList$QE
   }  # if(tag)
   }  # not all 0's
   return(outData)
} # end of runRMAt
print("################################################################")
print("Please consider citing the following if this program is useful for you:")
cat("\n\tGang Chen, Manual or manuscript coming soon. \n") 
print("################################################################")


print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")

outFN <- readline("Output file name (just prefix, no view+suffix needed, e.g., myOutput): ")
outFN <- paste(outFN, "+orig", sep="") # write.AFNI doesn't handle tlrc yet

print("On a multi-processor machine, parallel computing will speed up the program significantly.")
print("Choose 1 for a single-processor computer.")
nNodes <- as.integer(readline("Number of parallel jobs for the running (e.g., 2)? "))
nGrp <- as.integer(readline("Number of groups (1 or 2)? "))

   grpLab <- vector('list', nGrp)
   nSubj <- vector('integer', nGrp)
   nFiles <- vector('integer', nGrp) # number of input files for each group
   
   print("-----------------")
   print("The following types of group analysis are currently available:")
   print("1: one condition with one group;")
   print("2: one condition with two groups;")
   print("3: two conditions with one group.")
   anaType <- as.integer(readline("Which analysis type (1, 2, 3): "))
   
   if(anaType==1 | anaType==2) {
      bFN <- vector('list', nGrp)
      tFN <- vector('list', nGrp)
      bList <- vector('list', nGrp)
      tList <- vector('list', nGrp) 
      varList <- vector('list', nGrp)
   
      for(ii in 1:nGrp) {

  	   if(nGrp==1) grpLab[[ii]] <- readline("Label for the test? ") else 
         grpLab[[ii]] <- readline(sprintf("Label for group %i? ", ii))
  	   nSubj[ii] <- as.integer(readline(sprintf("Number of subjects in group %s (e.g., 12)? ", grpLab[[ii]])))
  	   nFiles[ii] <- 2*nSubj[ii]
  	   bFN[[ii]] <- vector('integer', nSubj[ii]) # list of beta file names
  	   tFN[[ii]] <- vector('integer', nSubj[ii]) # list of t-stat file names
  	   #print("Provide beta or linear combination of betas files first. Only one sub-brick input files are accepted!")
      for(jj in 1:nSubj[ii]) {
         bFN[[ii]][[jj]] <- readline(sprintf("No. %i subject file for beta or linear combination of betas in group %s: ", jj, grpLab[[ii]]))
      # print("Now provide the corresponding t-statistic files in SAME subject order as beta files. Only one sub-brick input files are accepted!")
         tFN[[ii]][[jj]] <- readline(sprintf("No. %i subject file for the corresponding t-statistic in group %s: ", jj, grpLab[[ii]]))
      print("-----------------")
      }
      bList[[ii]] <- lapply(bFN[[ii]], read.AFNI); tList[[ii]] <- lapply(tFN[[ii]], read.AFNI)
      if(ii==1) {myNote=bList[[1]][[1]]$header$HISTORY_NOTE; myOrig=bList[[1]][[1]]$origin; myDelta=bList[[1]][[1]]$delta; myDim <- bList[[1]][[1]]$dim}
      lapply(lapply(bList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch among the beta input files!"))
      lapply(lapply(tList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch between beta and t-statistic files!"))

   } # for(ii in 1:nGrp)
   } # if(anaType==1 | anaType==2)
   
   if(anaType==3) {
      
      nLevel <- 2
      bFN <- vector('list', nLevel)
      tFN <- vector('list', nLevel)
      conLab <- vector('list', nLevel)
      bList <- vector('list', nLevel)
      tList <- vector('list', nLevel)
      varList <- vector('list', nLevel)
      print("Since the contrast between the 2 conditions will be the 1st minus the 2nd, choose")
      print("an appropriate order between the 2 conditions to get the desirable contrast.")
      grpLab[[1]] <- readline("Label for the contrast? ")
      nSubj[1] <- as.integer(readline("Number of subjects: "))
      nFiles[1] <- 2*nSubj[1]  # 2 because of 1 beta and 1 t-statistic
      for(ii in 1:nLevel) {
         conLab[[ii]] <- readline(sprintf("Label for condition %i? ", ii))
         bFN[[ii]] <- vector('integer', nSubj[1]); tFN[[ii]] <- vector('integer', nSubj[1])
         for(jj in 1:nSubj[1]) {
            bFN[[ii]][[jj]] <- readline(sprintf("No. %i subject file for beta or linear combination of betas with condition %s: ", jj, conLab[[ii]]))
            tFN[[ii]][[jj]] <- readline(sprintf("No. %i subject file for the corresponding t-statistic with condition %s: ", jj, conLab[[ii]]))
            print("-----------------")
         }
         bList[[ii]] <- lapply(bFN[[ii]], read.AFNI); tList[[ii]] <- lapply(tFN[[ii]], read.AFNI)
         if(ii==1) {myNote=bList[[1]][[1]]$header$HISTORY_NOTE; myOrig=bList[[1]][[1]]$origin; myDelta=bList[[1]][[1]]$delta; myDim <- bList[[1]][[1]]$dim}
         lapply(lapply(bList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch among the beta input files!"))
         lapply(lapply(tList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch between beta and t-statistic files!"))

      } # for(ii in 1:nLevel)  
   
   } # if(anaType==3)
   
   nNonzero <- as.integer(readline(sprintf("Number of subjects with non-zero t-statistic? (0-%i) ", sum(nSubj))))
   print("Masking is optional, but will alleviate unnecessary penalty on q values of FDR correction.")
   # Hartung-Knapp method with t-test? 
   KHtest <- as.logical(as.integer(readline("Z- or t-statistic for the output? (0: Z; 1: t) ")))
   
   masked <- as.integer(readline("Any mask (0: no; 1: yes)? "))
   if(masked) {maskFN <- readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); maskData <- read.AFNI(maskFN)$ttt}
   if(masked) if(!all(dim(maskData[,,,1])==myDim[1:3])) stop("Mask dimensions don't match the input files!")
   
   print("Covariates are continuous variables (e.g., age, behavioral data) that can be partialled out in the model.")
   #anyCov <- as.logical(as.integer(readline("Any covariates (0: no; 1: yes)? ")))
   anyCov <- 0
   if(anyCov) {
      nCov <- as.integer(readline("Number of covariates (0: no; 1: yes)? "))
      print("Each subject is assumed to have one number per covariate. Covariates as input files can be")
      print("in multi-column or one-column format. Header with the 1st line as labels is optional in")
      print("multi-column files, but NOT allowed in one-column files. Vertically the sequence in each")
      print("column has to follow the same order as the beta/t-statistic files.")
      covForm <- as.integer(readline("Covariates data type (0: MULTIPLE one-column files; 1: ONE multi-column file)? "))
      if (covForm) {
         covFN <- readline("Covariates file name: ")
         covHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? "))
         if(covHeader == 1) covData <- read.table(covFN, header=TRUE) else {
            covData <- read.table(covFN, header=FALSE)
            if(nCov!=dim(covData)[2]) stop(sprintf("Mismatch: file %s has %i column while you said %i covariate(s)!", covFN, dim(covData)[2], nCov)) else
            for (ii in 1:nCov) names(covData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
         } # if(covHeader == 1)
      } else {
         covData <- data.frame(matrix(data=NA, nrow=sum(nSubj), ncol=nCov, dimnames = NULL))
         covData <- readMultiFiles(nCov, 1, "covariate")  # 1: assuming no header   
      }
   } else nCov <- 0
   
   
   exclude <- FALSE  # exclude those voxels with 0 variance
   
   # Maybe I should avoid doing this below part for those voxels in the mask!!!
   if(anaType==1 | anaType==2) for(ii in 1:nGrp) {
      bList[[ii]] <- lapply(bList[[ii]], function(x) x$ttt); tList[[ii]] <- lapply(tList[[ii]], function(x) x$ttt)
      varList[[ii]] <- mapply(function(x, y) ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), bList[[ii]], tList[[ii]], SIMPLIFY = FALSE)  # variances
      if(exclude) for (jj in 1:nSubj[ii]) varList[[ii]][[jj]][varList[[ii]][[jj]] < tolL] <- tolU  # replace those 0 variances with a big number
      #vv[[ii]][[jj]][vv[[ii]][[jj]] < tolL] <- tolL 
   }
   
   if(anaType==3) {
      for(ii in 1:nLevel) {
      bList[[ii]] <- lapply(bList[[ii]], function(x) x$ttt); tList[[ii]] <- lapply(tList[[ii]], function(x) x$ttt)
      varList[[ii]] <- mapply(function(x, y) ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), bList[[ii]], tList[[ii]], SIMPLIFY = FALSE)  # variances
      if(exclude) for (jj in 1:nSubj[1]) varList[[ii]][[jj]][varList[[ii]][[jj]] < tolL] <- tolU  # replace those 0 variances with a big number
      }
      contrBList <- mapply("-", bList[[1]], bList[[2]], SIMPLIFY = FALSE)
      contrVarList <- mapply("+", varList[[1]], varList[[2]], SIMPLIFY = FALSE)
   }
   
   nBrick <- 4*nGrp+(anyCov)*2*nCov
   outArr <- array(0, dim=c(myDim[1:3], nBrick))  
   outData<-vector(mode="numeric", length= nBrick)  # initialization for use in runRMA: 3 beta's + 3 z-scores

   #bListC <- c(bList[[1]], bList[[2]]); varListC <- c(varList[[1]], varList[[2]])  # two groups combined   
   #bListC <- array(unlist(c(bList[[1]], bList[[2]])), dim=c(myDim[1:3], nSubj[[1]]+nSubj[[2]]))
   #varListC <- array(unlist(c(varList[[1]], varList[[2]])), dim=c(myDim[1:3], nSubj[[1]]+nSubj[[2]]))  
   
   #two groups combined, and beta and var also combined since parApply can only take one array as argument
   #len1 <- 2*nSubj[1]
   #if (nGrp==2) {
      #len2 <- 2*nSubj[2]; len <- len1+len2
      #comArr <- array(c(unlist(c(bList[[1]], bList[[2]])), unlist(c(varList[[1]], varList[[2]]))), dim=c(myDim[1:3], sum(nFiles)))
   if(anaType==1 | anaType==2) comArr <- array(c(unlist(c(bList)), unlist(c(varList))), dim=c(myDim[1:3], sum(nFiles)))
   if(anaType==3) comArr <- array(c(unlist(c(contrBList)), unlist(c(contrVarList))), dim=c(myDim[1:3], sum(nFiles)))

   #rm(bList, tList, varList)
   # mask out the junk: one slice at a time due to potential memory issue
   
   if(as.logical(masked)) { for (kk in 1:myDim[3]) 
      comArr[,,kk,] <- array(apply(comArr[,,kk,], 3, function(x) x*maskData[,,kk,1]), dim=c(myDim[1:2],sum(nFiles)))
      rm(maskData)
   }

   if(nGrp==1) if(anyCov) myMods <- as.matrix(covData) else myMods <- NULL      
   if(nGrp==2) if(anyCov) myMods <- as.matrix(cbind(c(rep(0, nSubj[1]), rep(1, nSubj[2]))), covData) else 
      myMods <- cbind(c(rep(0, nSubj[1]), rep(1, nSubj[2]))) # dummy variable for two groups
   #}
   
   print("-----------------")
   print(sprintf("Totally %i slices in the data.", myDim[3]))
   print("-----------------")
   # single processor
   
   if(nNodes==1) for (ii in 1:myDim[3]) {
      outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runRMA, dummy=myMods, nGrp=nGrp, outData=outData, 
         rma=rma, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, tol=tolL), c(2,3,1))
      cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }
   
   # multi-processing
   if(nNodes>1) {
   libLoad('snow')
   cl <- makeCluster(nNodes, type = "SOCK")
   for(ii in 1:myDim[3]) {
      outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), runRMA, dummy=myMods, nGrp=nGrp, outData=outData, 
         rma=rma, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, tol=tolL), c(2,3,1))
      cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }
   stopCluster(cl)
   }  # if(nNodes>1)
 
   print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
   
   print("#++++++++++++++++++++++++++++++++++++++++++++")

#   if (nGrp==2) {
   for(ii in 1:nGrp) {
      if(ii==1) outLabel <- paste(sprintf("%s:b", grpLab[[ii]])) else
         outLabel <- append(outLabel, sprintf("%s:b", grpLab[[ii]]))
      if(KHtest) outLabel <- append(outLabel, sprintf("%s:t", grpLab[[ii]])) else
      outLabel <- append(outLabel, sprintf("%s:Z", grpLab[[ii]]))
   }
   if (nGrp==2) {
      outLabel <- append(outLabel, sprintf("%s-%s:b", grpLab[[2]],grpLab[[1]]))
      if(KHtest) outLabel <- append(outLabel, sprintf("%s-%s:t", grpLab[[2]],grpLab[[1]])) else
      outLabel <- append(outLabel, sprintf("%s-%s:Z", grpLab[[2]],grpLab[[1]]))
   } # if (nGrp==2)
   if(anyCov) for(ii in 1:nCov) {
      outLabel <- append(outLabel, sprintf("%s:b", names(covData)[ii]))
      if(KHtest) outLabel <- append(outLabel, sprintf("%s:t", names(covData)[ii])) else
      outLabel <- append(outLabel, sprintf("%s:Z", names(covData)[ii]))
   } 
   
   outLabel <- append(outLabel, "tau^2")
   outLabel <- append(outLabel, "QE:Chisq")

   ############ NEED TO MODIFY HERE!!!!!!!!!!!!!!!!!!############################
   # t and chi-sq for QE with df = sum(nFiles)/2 - nGrp (need changes later with more complicated models
   
   nDF <- sum(nFiles)/2-nGrp-nCov
   
   statpar <- "3drefit"
   if (KHtest) for (ii in 1:(2*nGrp-1)) statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fitt ", nDF) else
      for (ii in 1:(2*nGrp-1)) statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fizt")
   
   if(anyCov) if (KHtest) for(ii in 1:nCov) statpar <- paste(statpar, " -substatpar ", nBrick-3-2*(nCov-ii), " fitt ", nDF) else
      for(ii in 1:nCov) statpar <- paste(statpar, " -substatpar ", nBrick-3-2*(nCov-ii), " fizt")
   
   statpar <- paste(statpar, " -substatpar ", nBrick-1, " fict ", nDF)  # last brick: QE with chi-sq

   outArr[outArr>tTop] <- tTop  # Avoid outflow!!!!
   outArr[outArr < (-tTop)] <- -tTop  # Avoid outflow!!!!
   write.AFNI(outFN, outArr, outLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")
   
   statpar <- paste(statpar, " -view tlrc -addFDR -newid ", outFN)  # assume tlrc space
   system(statpar)
   geterrmessage()

 
# } # if (nGrp==1)

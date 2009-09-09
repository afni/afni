print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 3dMetaAna.R ==================          ")
print("AFNI Mixed-Effects Meta-Analysis Modeling Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.1.1,  Sept. 9, 2009")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website - http://afni.nimh.nih.gov/sscc/gangc/MEMA.html")
print("SSCC/NIMH, National Institutes of Health, Bethesda MD 20892")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

#########
##  Working version for handling covariates and residual statistics which are saved as 2 separate output file. 
##  type 4 (two groups with heteroskedasticity) should NOT be used when modeling with different slope across groups
##  Outliers can be modeled now! 
#########


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
  if (all(values$BRICK_TYPES==3)) myttt<- readBin(conbrik, "numeric", n=dx*dy*dz, size=size, signed=TRUE, endian=endian) # float        
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

# handles one group or two groups with homogeneity assumption
rmaB <- function(yi, vi, n, p, X, resOut, lapMod, knha=FALSE, con=list(thr=10^-8, maxiter=50, thrZ=1.3)) {

# yi: vector of dependent variable values
# vi: corresponding variances of yi 
# n: number of rows
# p: number of regressors (columns in design matrix X) including covariates   
# X: fixed effects matrix including covariates 
# knha: adopting knha t-test?
# con$thr: converging threshold for REML
# con$maxiter: maximum # of converging iterations for REML

	Y <- as.matrix(yi)
	   
	tr <- function(X) sum(diag(X))

   W0     <- diag(1/vi)
   tmp0   <- t(X) %*% W0
   P0     <- W0 - t(tmp0) %*% solve(tmp0 %*% X) %*% tmp0
   QE     <- t(Y) %*% P0 %*% Y   
   
   collect <- function(Y, vb, R, P, n, p, knha, con) {
      if(knha) vb <- (c( t(Y) %*% P %*% Y ) / (n-p)) * vb
      se <- sqrt(diag(vb))
      b    <- R %*% Y
      z <- ifelse(se>con$thr, b/se, 0)
      out <- list(se, b, z)
      names(out) <- c("se", "b", "z")
      out
   }

   MoM <- function(Y, vi, n, p, X, P0, QE, knha, con) {
      #RSS    <- t(Y) %*% P0 %*% Y
      tau2   <- ( QE - (n-p) ) / tr(P0)  # RSS = QE
      tau2 <- max(0, c(tau2))
      W   <- diag(1/(vi + tau2))
      tmp <- t(X) %*% W
      vb  <- solve(tmp %*% X)
      R   <- vb %*% tmp  # projection of Y to space spanned by X
      P   <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out <- list(stat$se, stat$b, stat$z, tau2, P, 0, 0)
      names(out) <- c("se", "b", "z", "tau2", "P", "meth", "iter")
      out
   }                 

   
   Laplace <- function(Y, vi, n, p, X, knha, con=list(thr=2*10^-5, maxiter=50)) {
   ### looks like you can't get the precision lower than con$thr = 10^-5!!!

   conv		<- 1
	change_b	<- 1000
   change_nu	<- 1000
	iter		<- 0
   
   nu       <- sqrt(max(0, var(Y) - mean(vi)))/sqrt(2)
   if(nu < 10^-10) nu <- sqrt(max(0, var(Y) - mean(vi)/3))
   W		   <- diag(1/(vi + 2*nu^2))  # need to deal with 0s' here?
   tmp  <- t(X) %*% W
   b    <- solve(tmp %*% X) %*% tmp %*% Y
   visr <- sqrt(vi)

	# seems two ways to update beta (b): one through iterative WLS, while the other via its own iterations
   
   if(nu > 10^-10) {
   while (any(change_b > con$thr) & (change_nu > con$thr)) {
		iter     <- iter + 1
      if (iter > con$maxiter) {  # Laplace fails
			conv    <- 0
			break
		}
		b.old    <- b
      nu.old   <- nu    # scalor
      ei <- Y - X %*% b.old    # n X 1 vector
      Ei <- exp(ei/nu)       # n X 1 vector
      temp1 <- visr/nu; temp2 <- ei/visr  # n X 1 vector: dealing 0s for visr?
      Phi1 <- pnorm(-temp1 - temp2)   # n X 1 vector
      Phi2 <- pnorm(temp2 - temp1)    # n X 1 vector
      phi1 <- dnorm(-temp1 - temp2)   # n X 1 vector
      phi2 <- dnorm(temp2 - temp1)    # n X 1 vector
      Gi <- Phi1 * Ei + Phi2 / Ei     # n X 1 vector: dealing 0s for Ei?
      
      dLb <- vector(mode="numeric", length=p)
      H <- matrix(data = 0, nrow = p+1, ncol = p+1, byrow = FALSE, dimnames = NULL)
      g <- matrix(data = 0, nrow = p+1, ncol = 1, byrow = FALSE, dimnames = NULL)
      for(ii in 1:n) { 
         dLb  <- (phi1[ii]*t(X[ii,])*Ei[ii]/visr[ii] - t(X[ii,])*Ei[ii]*Phi1[ii]/nu
                 + (t(X[ii,])*Phi2[ii]/Ei[ii])/nu - (phi2[ii]*t(X[ii,])/visr[ii])/Ei[ii])/Gi[ii]
         dLnu <- -1/nu - vi[ii]/nu^3 + (Ei[ii]*ei[ii]*Phi1[ii]/nu^2 + visr[ii]*Ei[ii]*phi1[ii]/nu^2
                 - (ei[ii]*Phi2[ii]/Ei[ii])/nu^2 + (visr[ii]*phi2[ii]/Ei[ii])/nu^2)/Gi[ii]        
         ttemp <- t(dLb)*dLnu
         #H <- H + rbind(cbind(dLb %*% t(dLb), t(ttemp)), cbind(ttemp, dLnu^2))
         H <- H + rbind(cbind(t(dLb) %*% dLb, ttemp), cbind(t(ttemp), dLnu^2))
         g <- g + rbind(t(dLb), dLnu)
      }      
      
      if(any(is.infinite(H)) | any(is.infinite(H)) | any(is.infinite(g)) | any(is.infinite(g)) |
         any(is.nan(H)) | any(is.nan(H)) | any(is.nan(g)) | any(is.nan(g))) { # failed because of infinity issue
         conv    <- 0
         break
		}
      
      suc <- TRUE
      tryCatch(adj <- solve(H) %*% g, error = function(w) suc <<- FALSE)   # (p+1) X 1 vector
      if(!suc) {  # Laplace fails
			conv    <- 0
			break
		}
      
      b  <- b.old + adj[1:p]
      nu <- nu + adj[p+1]
      change_b	   <- abs(b.old - b)
      change_nu	<- abs(nu.old - nu)
	}
   } else conv <- 0

   if(conv==0) {
      out <- list(conv)
      names(out) <- c("conv")
   } else {   # Laplace succeeded
      W	  <- diag(1/(vi + 2*nu^2))
      tmp  <- t(X) %*% W
      vb   <- solve(tmp %*% X)   # variance-covariance matrix
      R    <- vb %*% tmp  # projection of Y to space spanned by X
      P    <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      tau2 <- 2*nu^2
      
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out  <- list(stat$se, stat$b, stat$z, tau2, P, 1, 1, iter)
      names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
   }
   out
   } # end of Laplace
   
   REML <- function(Y, X, v, knha, n, p, con) {
      conv		<- 1
      adj	   <- 1000
      iter		<- 0
      tau2     <- max(0, var(Y) - mean(v))  # tau^2 for group 1
      W		   <- diag(1/(v + tau2))
      tmp      <- t(X) %*% W
      vb       <- solve(tmp %*% X)   # variance-covariance matrix
      R        <- vb %*% tmp  # projection of Y to space spanned by X
      P        <- W - t(tmp) %*% R
      while(abs(adj) > con$thr) {
         iter     <- iter + 1  # iteration counter
         if (iter > con$maxiter) {
            conv    <- 0
            break
         }
         tau2.old <- tau2
         py    <- P%*%Y
         adj	<- solve( tr(P%*%P) ) %*% ( t(py)%*%py - tr(P) )
         while (tau2 + adj < 0) adj <- adj / 2
         tau2		<- tau2 + adj
         W		   <- diag(1/(v + tau2))
         tmp <- t(X) %*% W
         vb  <- solve(tmp %*% X)   # variance-covariance matrix
         R   <- vb %*% tmp  # projection of Y to space spanned by X
         P <- W - t(tmp) %*% R
      }

      if(conv==0) { # REML failed
         out <- list(conv)
         names(out) <- c("conv")
      } else {   # REML succeeded         
         stat <- collect(Y, vb, R, P, n, p, knha, con)
         out <- list(stat$se, stat$b, stat$z, tau2, P, 1, 2, iter)
         names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
      }
      out
   }  # end of REML

   # try MoM first: if not significant for sure, then not worth trying other costy methods
   outMoM <- MoM(Y, vi, n, p, X, P0, QE, knha, con)
   se   <- outMoM$se
   b    <- outMoM$b
   z    <- outMoM$z
   tau2 <- outMoM$tau2
   P    <- outMoM$P
   meth <- outMoM$meth
   iter <- outMoM$iter   
   
   if(lapMod==1) {
      resZ <- P %*% Y / sqrt(diag(P %*% tcrossprod(diag(tau2+vi), P)))
      # try Laplace only if either likely significant or suspicious of outliers
      #browser()
      noMoM <- any(abs(outMoM$z) > con$thrZ) | any(abs(resZ) > con$thrZ)
      if(noMoM) {
         outLap <- Laplace(Y, vi, n, p, X, knha)
         if(outLap$conv == 1) {
            se   <- outLap$se
            b    <- outLap$b
            z    <- outLap$z
            tau2 <- outLap$tau2
            P    <- outLap$P
            meth <- outLap$meth
            iter <- outLap$iter
         } else lapMod <- 0  # switch to REML first here when Laplace fails
      } 
   }
   
   if(lapMod==0) { # if Laplace is not selected or fails, try REML
      # try REML only if likely significant
      noMoM <- any(abs(outMoM$z) > con$thrZ) 
      if(noMoM) {
         outREML <- REML(yi, X, vi, knha, n, p, con)
         if(outREML$conv == 1) {
            se   <- outREML$se
            b    <- outREML$b
            z    <- outREML$z
            tau2 <- outREML$tau2
            P    <- outREML$P
            meth <- outREML$meth
            iter <- outREML$iter
         }
      }
   }
   
   if(resOut==1) { # residual statistics requested
      vTot <- tau2+vi  # total variance
      lamc <- ifelse(vTot>con$thr, vi/vTot, 0)  # Like ICC, lamda shows percent of variation at group level   
      # need to scale this for Knapp & Hartung method as done above by s2w <- c( t(Y) %*% P %*% Y ) / (n-p)?
      if((lapMod==0) | noMoM) resZ <- P %*% Y / sqrt(diag(P %*% tcrossprod(diag(vTot), P)))
   
      res         <- list(b, se, z, tau2, QE, lamc, resZ, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "lamc", "resZ", "meth", "iter")
   } else {  # no residual statistics
      res         <- list(b, se, z, tau2, QE, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "meth", "iter")
   }
   res
}

# two groups with heterogeneity assumption
rmaB2 <- function(yi, vi, n1, nT, p, X, resOut, lapMod, knha=FALSE, con=list(thr=10^-8, maxiter=50, thrZ=1.3)) {

	Y <- as.matrix(yi)
   Y1 <- as.matrix(Y[1:n1,]); Y2 <- as.matrix(Y[(n1+1):nT,])
   v1 <- vi[1:n1]; v2 <- vi[(n1+1):nT]
   n2 <- nT-n1
   X1 <- as.matrix(X[1:n1,-2]); X2 <- as.matrix(X[(n1+1):nT,-2])   
	   
	tr <- function(X) sum(diag(X))

   W01	 <- diag(1/v1)
   W02    <- diag(1/v2)
   tmp01   <- t(X1) %*% W01
   tmp02   <- t(X2) %*% W02
   P01    <- W01 - t(tmp01) %*% solve(tmp01 %*% X1) %*% tmp01
   P02    <- W02 - t(tmp02) %*% solve(tmp02 %*% X2) %*% tmp02
   QE     <- c(t(Y1) %*% P01 %*% Y1, t(Y2) %*% P02 %*% Y2)

   collect <- function(Y, vb, R, P, n, p, knha, con) {
      if(knha) vb <- (c( t(Y) %*% P %*% Y ) / (n-p)) * vb
      se <- sqrt(diag(vb))
      b    <- R %*% Y
      z <- ifelse(se>con$thr, b/se, 0)
      out <- list(se, b, z)
      names(out) <- c("se", "b", "z")
      out
   }
   
   MoM <- function(Y, vi, n, p, X, P0, QE, knha, con) {
      #RSS    <- t(Y) %*% P0 %*% Y
      tau2   <- ( QE - (n-p) ) / tr(P0)  # RSS = QE
      tau2 <- max(0, c(tau2))
      W   <- diag(1/(vi + tau2))
      tmp <- t(X) %*% W
      vb  <- solve(tmp %*% X)
      R   <- vb %*% tmp  # projection of Y to space spanned by X
      P   <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out <- list(stat$se, stat$b, stat$z, tau2, P, 0, 0)
      names(out) <- c("se", "b", "z", "tau2", "P", "meth", "iter")
      out
   }                 

   Laplace <- function(Y, vi, n, p, X, knha, con=list(thr=2*10^-5, maxiter=50)) {
   ### looks like you can't get the precision lower than con$thr = 10^-5!!!

   conv		<- 1
	change_b	<- 1000
   change_nu	<- 1000
	iter		<- 0
   
   nu       <- sqrt(max(0, var(Y) - mean(vi)))/sqrt(2)
   if(nu < 10^-10) nu <- sqrt(max(0, var(Y) - mean(vi)/3))
   W		   <- diag(1/(vi + 2*nu^2))  # need to deal with 0s' here?
   tmp  <- t(X) %*% W
   b    <- solve(tmp %*% X) %*% tmp %*% Y
   visr <- sqrt(vi)

	# seems two ways to update beta (b): one through iterative WLS, while the other via its own iterations
   
   if(nu > 10^-10) {
   while (any(change_b > con$thr) & (change_nu > con$thr)) {
		iter     <- iter + 1
      if (iter > con$maxiter) {  # Laplace fails
			conv    <- 0
			break
		}
		b.old    <- b
      nu.old   <- nu    # scalor
      ei <- Y - X %*% b.old    # n X 1 vector
      Ei <- exp(ei/nu)       # n X 1 vector
      temp1 <- visr/nu; temp2 <- ei/visr  # n X 1 vector: dealing 0s for visr?
      Phi1 <- pnorm(-temp1 - temp2)   # n X 1 vector
      Phi2 <- pnorm(temp2 - temp1)    # n X 1 vector
      phi1 <- dnorm(-temp1 - temp2)   # n X 1 vector
      phi2 <- dnorm(temp2 - temp1)    # n X 1 vector
      Gi <- Phi1 * Ei + Phi2 / Ei     # n X 1 vector: dealing 0s for Ei?
      
      dLb <- vector(mode="numeric", length=p)
      H <- matrix(data = 0, nrow = p+1, ncol = p+1, byrow = FALSE, dimnames = NULL)
      g <- matrix(data = 0, nrow = p+1, ncol = 1, byrow = FALSE, dimnames = NULL)
      for(ii in 1:n) { 
         dLb  <- (phi1[ii]*t(X[ii,])*Ei[ii]/visr[ii] - t(X[ii,])*Ei[ii]*Phi1[ii]/nu
                 + (t(X[ii,])*Phi2[ii]/Ei[ii])/nu - (phi2[ii]*t(X[ii,])/visr[ii])/Ei[ii])/Gi[ii]
         dLnu <- -1/nu - vi[ii]/nu^3 + (Ei[ii]*ei[ii]*Phi1[ii]/nu^2 + visr[ii]*Ei[ii]*phi1[ii]/nu^2
                 - (ei[ii]*Phi2[ii]/Ei[ii])/nu^2 + (visr[ii]*phi2[ii]/Ei[ii])/nu^2)/Gi[ii]        
         ttemp <- t(dLb)*dLnu
         #H <- H + rbind(cbind(dLb %*% t(dLb), t(ttemp)), cbind(ttemp, dLnu^2))
         H <- H + rbind(cbind(t(dLb) %*% dLb, ttemp), cbind(t(ttemp), dLnu^2))
         g <- g + rbind(t(dLb), dLnu)
         
      }      
      
      if(any(is.infinite(H)) | any(is.infinite(H)) | any(is.infinite(g)) | any(is.infinite(g)) |
         any(is.nan(H)) | any(is.nan(H)) | any(is.nan(g)) | any(is.nan(g))) { # failed because of infinity issue
         conv    <- 0
         break
		}
      
      suc <- TRUE
      tryCatch(adj <- solve(H) %*% g, error = function(w) suc <<- FALSE)   # (p+1) X 1 vector
      if(!suc) {  # Laplace fails
			conv    <- 0
			break
		}
      
      b  <- b.old + adj[1:p]
      nu <- nu + adj[p+1]
      change_b	   <- abs(b.old - b)
      change_nu	<- abs(nu.old - nu)
	}
   } else conv <- 0

   if(conv==0) {
      out <- list(conv)
      names(out) <- c("conv")
   } else {   # Laplace succeeded
      W	  <- diag(1/(vi + 2*nu^2))
      tmp  <- t(X) %*% W
      vb   <- solve(tmp %*% X)   # variance-covariance matrix
      R    <- vb %*% tmp  # projection of Y to space spanned by X
      P    <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      tau2 <- 2*nu^2
      
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out  <- list(stat$se, stat$b, stat$z, tau2, P, 1, 1, iter)
      names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
   }
   out
   } # end of Laplace
 
   REML <- function(Y, X, v, knha, n, p, con) {
      conv		<- 1
      adj	   <- 1000
      iter		<- 0
      tau2     <- max(0, var(Y) - mean(v))  # tau^2 for group 1
      W		   <- diag(1/(v + tau2))
      tmp      <- t(X) %*% W
      vb       <- solve(tmp %*% X)   # variance-covariance matrix
      R        <- vb %*% tmp  # projection of Y to space spanned by X
      P        <- W - t(tmp) %*% R
      while(abs(adj) > con$thr) {
         iter     <- iter + 1  # iteration counter
         if (iter > con$maxiter) {
            conv    <- 0
            break
         }
         tau2.old <- tau2
         py    <- P%*%Y
         adj	<- solve( tr(P%*%P) ) %*% ( t(py)%*%py - tr(P) )
         while (tau2 + adj < 0) adj <- adj / 2
         tau2		<- tau2 + adj
         W		   <- diag(1/(v + tau2))
         tmp <- t(X) %*% W
         vb  <- solve(tmp %*% X)   # variance-covariance matrix
         R   <- vb %*% tmp  # projection of Y to space spanned by X
         P <- W - t(tmp) %*% R
      }

      if(conv==0) { # REML failed
         out <- list(conv)
         names(out) <- c("conv")
      } else {   # REML succeeded         
         stat <- collect(Y, vb, R, P, n, p, knha, con)
         out <- list(stat$se, stat$b, stat$z, tau2, P, 1, 2, iter)
         names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
      }
      out
   }  # end of REML

   # try MoM first: if not significant for sure, then not worth trying other costy methods
   # Be careful: p-1 here because each group has p-1 columns in the design matrix
   outMoM1 <- MoM(Y1, v1, n1, p-1, X1, P01, QE[1], FALSE, con) # force knha FASLE so that modification happens later
   outMoM2 <- MoM(Y2, v2, n2, p-1, X2, P02, QE[2], FALSE, con) # force knha FASLE
   
   se   <- c(outMoM1$se, outMoM2$se)
   b    <- c(outMoM1$b, outMoM2$b)
   z    <- c(outMoM1$z, outMoM2$z)
   tau2 <- c(outMoM1$tau2, outMoM2$tau2)
   #tau2 <- outMoM$tau2
   P1    <- outMoM1$P; P2 <- outMoM2$P
   meth <- outMoM1$meth
   iter <- c(outMoM1$iter, outMoM2$iter)   
   
   if(lapMod==1) {
      resZ1 <- P1 %*% Y1 / sqrt(diag(P1 %*% tcrossprod(diag(tau2[1]+v1), P1)))
      resZ2 <- P2 %*% Y2 / sqrt(diag(P2 %*% tcrossprod(diag(tau2[2]+v2), P2)))
      noMoM <- (any(abs(z) > con$thrZ)) | any(abs(resZ1) > con$thrZ) | any(abs(resZ2) > con$thrZ)
      if(noMoM) {
         #browser()
         outLap1 <- Laplace(Y1, v1, n1, p-1, X1, FALSE) # force knha FASLE
         outLap2 <- Laplace(Y2, v2, n2, p-1, X2, FALSE) # force knha FASLE
         if(outLap1$conv == 1 & outLap2$conv == 1) {
            se   <- c(outLap1$se, outLap2$se)
            b    <- c(outLap1$b, outLap2$b)
            z    <- c(outLap1$z, outLap2$z)
            tau2 <- c(outLap1$tau2, outLap2$tau2)
            P1   <- outLap1$P; P2 <- outLap2$P
            meth <- outLap1$meth
            iter <- c(outLap1$iter, outLap2$iter)
        } else lapMod <- 0  # switch to REML first here when Laplace fails
      }
   }

   if(lapMod==0) { # if Laplace is not selected or fails, try REML      
	   noMoM <- (any(abs(z) > con$thrZ))
      if(noMoM) {
         outREML1 <- REML(Y1, X1, v1, FALSE, n1, p-1, con)  # force knha FASLE
         outREML2 <- REML(Y2, X2, v2, FALSE, n2, p-1, con)  # force knha FASLE
         if(outREML1$conv == 1 & outREML2$conv == 1) {
            se   <- c(outREML1$se, outREML2$se)
            b    <- c(outREML1$b, outREML2$b)
            z    <- c(outREML1$z, outREML2$z)
            tau2 <- c(outREML1$tau2, outREML2$tau2)
            P1   <- outREML1$P; P2 <- outREML2$P
            meth <- outREML1$meth
            iter <- c(outREML1$iter, outREML2$iter)
         }
      }
      #browser()
   } # if(lapMod==0)
   
   if(knha) { # scaling done in runRMA
      scl <- c(sqrt((c( t(Y1) %*% P1 %*% Y1 ) / (n1-p+1))),
         sqrt((c( t(Y2) %*% P2 %*% Y2 ) / (n2-p+1))))
      W <- diag(c(1/(v1 + tau2[1]), 1/(v2 + tau2[2])))
      tmp <- t(X) %*% W
      P <- W - t(tmp) %*% solve(tmp %*% X) %*% tmp
      scl <- c(scl, sqrt((c( t(Y) %*% P %*% Y ) / (nT-p))))
   } else scl <- NA

   if(resOut==1) {  # residual statistics requested
      vTot1 <- tau2[1]+v1  # total variance for group1
      vTot2 <- tau2[2]+v2  # total variance for group1
      # Like ICC, lamda shows percent of variation at group lev
      lamc <- c(ifelse(vTot1>con$thr, v1/vTot1, 0),
         ifelse(vTot2>con$thr, v2/vTot2, 0))
      
      resZ <- c(P1 %*% Y1/sqrt(diag(P1 %*% tcrossprod(diag(vTot1), P1))),
         P2 %*% Y2/sqrt(diag(P2 %*% tcrossprod(diag(vTot2), P2))))
 
      res         <- list(b, se, z, tau2, QE, scl, lamc, resZ, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "scl", "lamc", "resZ", "meth", "iter")
   } else {  # no residual statistics requested
      res         <- list(b, se, z, tau2, QE, scl, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "scl", "meth", "iter")
   }
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


runRMA <- function(inData, nGrp, n, p, xMat, outData, mema, lapMod, KHtest, nNonzero, nCov, nBrick, anaType, resZout, tol) {  
   
   # n[1]: # subjects in group1; n[2]: total # subjects in both groups; n[2]-n[1]: # subjects in group2   
   fullLen <- length(inData); halfLen <- fullLen/2  # remove this line by providing halfLen as function argument later?
   Y <- inData[1: halfLen]; V <- inData[(halfLen +1): fullLen]
   if(sum(abs(Y)>tol)>nNonzero) {  # run only when there are more than 2 non-zeros in both Y and V
   tag <- TRUE
   if(anaType==4) try(resList <- mema(Y, V, n[1], n[2], p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE) else
      if(length(n)==1) try(resList <- mema(Y, V, n, p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE) else
      try(resList <- mema(Y, V, n[2], p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE)  # for the case of 2 groups with homoskedasticiy

   if (tag) {
   if (nGrp==1) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+1] <- resList$b[ii+1]
         outData[2*ii+2] <- resList$z[ii+1]   
      } # for(ii in 1:nCov)
   } # if (nGrp==1)
   if (anaType==2) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      outData[5] <- resList$b[2]  # beta of group2-group1
      outData[6] <- resList$z[2]  # z score of group2-group1
      outData[3] <- outData[1]+outData[5]  # beta of group2
      tmp <- sqrt(resList$se[2]^2-resList$se[1]^2)
      outData[4] <- ifelse(tmp>tol, outData[3]/tmp, 0) # z-score of group2
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+5] <- resList$b[ii+2]
         outData[2*ii+6] <- resList$z[ii+2]   
      } # for(ii in 1:nCov)
   } # if (nGrp==2)

   if(anaType==4) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      outData[3] <- resList$b[2]  # beta of group2
      outData[4] <- resList$z[2]  # z-score of group2
      outData[5] <- resList$b[2]-resList$b[1]  # beta of group2-group1
      tmp <- sqrt(resList$se[1]^2+resList$se[2]^2)
      outData[6] <- ifelse(tmp>tol, outData[5]/tmp, 0) #z score of group2-group1
      if(KHtest) {
         outData[2] <- outData[2]/resList$scl[1]
         outData[4] <- outData[4]/resList$scl[2]
         outData[6] <- outData[6]/resList$scl[3]
      }
   
   if(resZout==0) {
   outData[nBrick-3] <- resList$QE[1]
   outData[nBrick-2]   <- resList$QE[2]
   outData[nBrick-1] <- ifelse(resList$tau2[2] > tol, resList$tau2[1]/resList$tau2[2], 0)
   outData[nBrick]   <- ifelse(resList$tau2[1] > tol, resList$tau2[2]/resList$tau2[1], 0)
   } else {
   
   outData[nBrick-2*n[2]-3] <- resList$QE[1]
   outData[nBrick-2*n[2]-2]   <- resList$QE[2]
   outData[nBrick-2*n[2]-1] <- ifelse(resList$tau2[2] > tol, resList$tau2[1]/resList$tau2[2], 0)
   outData[nBrick-2*n[2]]   <- ifelse(resList$tau2[1] > tol, resList$tau2[2]/resList$tau2[1], 0)
   
   for(ii in 1:n[1]) {
      outData[nBrick-2*(n[2]-ii)-1] <- resList$lamc[ii]   # lamda = 1-I^2
      outData[nBrick-2*(n[2]-ii)]   <- resList$resZ[ii]    # Z-score for residuals
   } # from nBrick-2*n[2]+1 to nBrick-2*(n[2]-n[1])
   
   for(ii in 1:(n[2]-n[1])) {
      outData[nBrick-2*(n[2]-n[1]-ii)-1] <- resList$lamc[ii+n[1]]   # lamda = 1-I^2
      outData[nBrick-2*(n[2]-n[1]-ii)]   <- resList$resZ[ii+n[1]]    # Z-score for residuals
   } # from nBrick-2*(n[2]-n[1])+1 to nBrick
   } # if(resZout==0)
   } else {  # not anaType==4
   if(resZout==0) {
      outData[nBrick-1] <- resList$tau2
      outData[nBrick]   <- resList$QE
   } else {
      outData[nBrick-2*n-1] <- resList$tau2
      outData[nBrick-2*n]   <- resList$QE
      for(ii in 1:n) {
      outData[nBrick-2*(n-ii)-1] <- resList$lamc[ii]   # lamda = 1-I^2
      outData[nBrick-2*(n-ii)]   <- resList$resZ[ii]    # Z-score for residuals
   }
   }
   }  # if(anaType==4)
   }  # if(tag)
   }  # not all 0's
   return(outData)
} # end of runRMA


tolL <- 1e-7 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's
tolU <- 1e8  # upper tolerance for those variances of 0
tTop <- 100   # upper bound for t-statistic

#options(show.error.messages = FALSE)  # suppress error message when running with single processor


print("################################################################")
print("Please consider citing the following if this program is useful for you:")
cat("\n\tGang Chen et al., hopefully something coming soon...\n")
cat("\n\thttp://afni.nimh.nih.gov/sscc/gangc/MEMA.html\n")
#cat("\n\tGang Chen, J. Paul Hamilton, Moriah E. Thomason, Ian H. Gotlib, Ziad S. Saad\n")
#cat("\tRobert W. Cox, Granger causality via vector auto-regression (VAR) attuned for\n")
#cat("\tFMRI data analysis. ISMRM 17th Scientific Meeting, Hawaii, 2009.\n\n")
print("################################################################")


print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")

outFNexist <- TRUE
while (outFNexist) {
   outFN <- readline("Output file name (just prefix, no view+suffix needed, e.g., myOutput): ")
   if(file.exists(paste(outFN,"+orig.HEAD", sep="")) || file.exists(paste(outFN,"+tlrc.HEAD", sep=""))) {
      print("File exsists! Try a different name.")
      outFNexist <- TRUE
   } else outFNexist <- FALSE }
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
   print("2: one condition across 2 groups with homoskedasticity (same variability);")
   print("3: two conditions with one group;")
   print("4: one condition across 2 groups with heteroskedasticity (different variability).")
   anaType <- as.integer(readline("Which analysis type (1, 2, 3, 4): "))
   
   if(anaType==1 | anaType==2 | anaType==4) {
      bFN <- vector('list', nGrp)
      tFN <- vector('list', nGrp)
      subjLab <- vector('list', nGrp)
      bList <- vector('list', nGrp)
      tList <- vector('list', nGrp) 
      varList <- vector('list', nGrp)
   
      for(ii in 1:nGrp) {

  	   if(nGrp==1) grpLab[[ii]] <- readline("Label for the test? ") else 
         grpLab[[ii]] <- readline(sprintf("Label for group %i? ", ii))
  	   nSubj[ii] <- as.integer(readline(sprintf("Number of subjects in group %s (e.g., 12)? ", grpLab[[ii]])))
  	   nFiles[ii] <- 2*nSubj[ii]
      subjLab[[ii]] <- vector('list', nSubj[ii])
  	   bFN[[ii]] <- vector('integer', nSubj[ii]) # list of beta file names
  	   tFN[[ii]] <- vector('integer', nSubj[ii]) # list of t-stat file names
  	   #print("Provide beta or linear combination of betas files first. Only one sub-brick input files are accepted!")
      
      for(jj in 1:nSubj[ii]) subjLab[[ii]][[jj]] <- readline(sprintf("No. %i subject label in group %s: ", jj, grpLab[[ii]]))
      
      for(jj in 1:nSubj[ii]) {
         bFN[[ii]][[jj]] <- readline(sprintf("No. %i subject (%s) file for beta or linear combination of betas in group %s: ", jj, subjLab[[ii]][[jj]], grpLab[[ii]]))
      # print("Now provide the corresponding t-statistic files in SAME subject order as beta files. Only one sub-brick input files are accepted!")
         tFN[[ii]][[jj]] <- readline(sprintf("No. %i subject (%s) file for the corresponding t-statistic in group %s: ", jj, subjLab[[ii]][[jj]], grpLab[[ii]]))
      print("-----------------")
      }
      bList[[ii]] <- lapply(bFN[[ii]], read.AFNI); tList[[ii]] <- lapply(tFN[[ii]], read.AFNI)
      if(ii==1) {myNote=bList[[1]][[1]]$header$HISTORY_NOTE; myOrig=bList[[1]][[1]]$origin; myDelta=bList[[1]][[1]]$delta; myDim <- bList[[1]][[1]]$dim}
      lapply(lapply(bList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch among the beta input files!"))
      lapply(lapply(tList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch between beta and t-statistic files!"))

   } # for(ii in 1:nGrp)
   } # if(anaType==1 | anaType==2 | anaType==4)
   
   if(anaType==3) {
      
      nLevel <- 2   
      bFN <- vector('list', nLevel)
      tFN <- vector('list', nLevel)
      conLab <- vector('list', nLevel)
      subjLab <- vector('list', 1)
      bList <- vector('list', nLevel)
      tList <- vector('list', nLevel)
      varList <- vector('list', nLevel)
      print("Since the contrast between the 2 conditions will be the 1st minus the 2nd, choose")
      print("an appropriate order between the 2 conditions to get the desirable contrast.")
      grpLab[[1]] <- readline("Label for the contrast? ")
      nSubj[1] <- as.integer(readline("Number of subjects: "))
      nFiles[1] <- 2*nSubj[1]  # 2 because of 1 beta and 1 t-statistic
      for(jj in 1:nSubj[1]) subjLab[[1]][[jj]] <- readline(sprintf("No. %i subject label: ", jj))      
      for(ii in 1:nLevel) {
         conLab[[ii]] <- readline(sprintf("Label for condition %i? ", ii))
         bFN[[ii]] <- vector('integer', nSubj[1]); tFN[[ii]] <- vector('integer', nSubj[1])
         for(jj in 1:nSubj[1]) {
            bFN[[ii]][[jj]] <- readline(sprintf("No. %i subject file for beta or linear combination of betas with condition %s: ", jj, conLab[[ii]]))
            tFN[[ii]][[jj]] <- readline(sprintf("~/3dMEMA/test/No. %i subject file for the corresponding t-statistic with condition %s: ", jj, conLab[[ii]]))
            print("-----------------")
         }
         bList[[ii]] <- lapply(bFN[[ii]], read.AFNI); tList[[ii]] <- lapply(tFN[[ii]], read.AFNI)
         if(ii==1) {myNote=bList[[1]][[1]]$header$HISTORY_NOTE; myOrig=bList[[1]][[1]]$origin; myDelta=bList[[1]][[1]]$delta; myDim <- bList[[1]][[1]]$dim}
         lapply(lapply(bList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch among the beta input files!"))
         lapply(lapply(tList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch between beta and t-statistic files!"))

      } # for(ii in 1:nLevel)  
   
   } # if(anaType==3)
   
   #print("-----------------")
   print("There may have some missing t values at some voxels in some subjects.")
   print("The following threshold would allow the program not waste runtime on those")
   print("voxels where most subjects don't t-values. ")
   nNonzero <- as.integer(readline(sprintf("Minimum number of subjects with non-zero t-statistic? (0-%i, e.g., 3/4 of total subjects) ", sum(nSubj))))
   # Hartung-Knapp method with t-test?
   print("-----------------")
   print("t-statistic is a little more conservative but also more appropriate for significance testing than Z")
   print("especially when sample size, number of subjects, is relatively small.")
   KHtest <- as.logical(as.integer(readline("Z- or t-statistic for the output? (0: Z; 1: t) ")))
   #KHtest <- FALSE
   
   print("-----------------")
   print("Masking is optional, but will alleviate unnecessary penalty on q values of FDR correction.")
   masked <- as.integer(readline("Any mask (0: no; 1: yes)? "))
   if(masked) {maskFN <- readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); maskData <- read.AFNI(maskFN)$ttt}
   if(masked) if(!all(dim(maskData[,,,1])==myDim[1:3])) stop("Mask dimensions don't match the input files!")

   if(nGrp==1) xMat <- rep(1, sum(nSubj))      
   if(nGrp==2) xMat <- cbind(rep(1, sum(nSubj)), c(rep(0, nSubj[1]), rep(1, nSubj[2]))) # dummy variable for two groups
   print("-----------------")   
   print("Covariates are continuous variables (e.g., age, behavioral data) that can be partialled out in the model.")
   anyCov <- as.logical(as.integer(readline("Any covariates (0: no; 1: yes)? ")))
   if(anyCov) {
      nCov <- as.integer(readline("Number of covariates (e.g., 1)? "))
      #print("Each subject is assumed to have one number per covariate. Covariates as input files can be")
      #print("in multi-column or one-column format. Header with the 1st line as labels is optional in")
      #print("multi-column files. The vertical sequence in each column has to follow the same subject order")
      #print("as the beta/t-statistic input files listed above.")
      print("Each subject is assumed to have one number per covariate. All covariates should be put into one")
      print("file in one- or multi-column format. Header at the 1st line as labels is optional. The vertical")
      print("sequence in each column has to follow exactly the same subject order as the beta/t-statistic")
      print("input files listed above.")
      #covForm <- as.integer(readline("Covariates data type (0: MULTIPLE one-column files; 1: ONE single/multi-column file)? "))
      covForm <- 1
      if (covForm) {
         covFN <- readline("Covariates file name: ")
         covHeader <- as.integer(readline("Does this multi-column file have a header (0: no; 1: yes)? "))
         if(covHeader == 1) covData <- read.table(covFN, header=TRUE) else {
            covData <- read.table(covFN, header=FALSE)
            if(nCov!=dim(covData)[2]) stop(sprintf("Mismatch: file %s has %i column while you said %i covariate(s)!", covFN, dim(covData)[2], nCov)) else
            for (ii in 1:nCov) names(covData)[ii] <- readline(sprintf("Name for covariate number %i? ", ii))
         } # if(covHeader == 1)
         covName <- names(covData)
      } else {
         covData <- data.frame(matrix(data=NA, nrow=sum(nSubj), ncol=nCov, dimnames = NULL))
         covData <- readMultiFiles(nCov, 1, "covariate")  # 1: assuming no header   
      }
      print("Each covariate should be centered around its mean or some other meaningful value so that group effect")
      print("can be interpreted with the covariate being at the mean or other user-specified value. Otherwise")
      print("the interpretation would be with the covariate being at 0!")
      centerType <- as.integer(readline("What value will covariate(s) be centered around (0: mean; 1: other value)? "))
      
      if(nGrp == 1) {
         if(centerType == 0) covData <- apply(covData, 2, scale, scale=F)  # center around mean for each covariate (column)
         if(centerType == 1) {  # center around a user-specified value
            centerVal <- vector(mode = "numeric", length = nCov)
            for(ii in 1:nCov) centerVal[ii] <- as.numeric(readline(sprintf("Centering value for covariate no. %i (%s): ? ", ii, names(covData)[ii])))
            covData <- as.matrix(apply(covData, 1, "-", centerVal))
         }
      } # if(nGrp == 1)

      if (nGrp == 2) {
         centerType2 <- as.integer(readline("How to model covariate(s) across groups (0: same center & slope; 1: same center but different slope; 
            2: different center but same slope; 3: different center & slope)? "))
         if(centerType2 == 0 | centerType2 == 1) {   # same center 
            if(centerType == 0) covData <- apply(covData, 2, scale, scale=F)  # center around mean for each covariate (column)
            if(centerType == 1) {  # center around other value for each covariate (column)
               centerVal <- vector(mode = "numeric", length = nCov)
               for(jj in 1:nCov) centerVal[jj] <- as.numeric(readline(sprintf(
                  "Centering value for covariate no. %i (%s) for both groups: ? ", jj, names(covData)[jj])))
               covData <- as.matrix(apply(covData, 1, "-", centerVal))
            }
         } # if(centerType2 == 0 | centerType2 == 1)
        
         if(centerType2 == 2 | centerType2 == 3) {  # different center 
            if(centerType == 0) covData <- rbind(apply(as.matrix(covData[1:nSubj[1],]), 2, scale, scale=F), 
               apply(as.matrix(covData[(nSubj[1]+1):(nSubj[1]+nSubj[2]),]), 2, scale, scale=F))
            if(centerType == 1) {
               covList <- vector('list', nGrp)               
               for(ii in 1:nGrp) {
                  centerVal <- vector(mode = "numeric", length = nCov)
                  for(jj in 1:nCov) centerVal[jj] <- as.numeric(readline(sprintf(
                     "Centering value for covariate no. %i (%s) in group %i (%s): ? ", jj, names(covData)[jj], ii, grpLab[[ii]])))
                  covList[[ii]] <- t(apply(as.matrix(covData[((ii-1)*nSubj[1]+1):(nSubj[1]+(ii-1)*nSubj[2]),]), 1, "-", centerVal))
               }
               covData <- rbind(t(covList[[1]]), t(covList[[2]]))
            } # if(centerType == 1)
         } # if(centerType2 == 3)
         
         if(centerType2 == 1 | centerType2 == 3) { # different slope
            covData <- cbind(covData, covData*xMat[,2])  # add one column per covariate for interaction
            nCov <- 2*nCov # double number of covariates due to interactions
            covName<-c(covName, paste(covName, "X", sep=''))  # add names for those interactions
         }
      }
      #browser()
      xMat <- as.matrix(cbind(xMat, covData))           
   } else {nCov <- 0; xMat <- as.matrix(xMat)}
   print("-----------------")
   print("If outliers exist at voxel/subject level, a special model can be adopted to account for outliers")
   print("in the data, leading to increased statistical power at slightly higher computation cost.")
   lapMod <- as.integer(readline("Model outliers (0: no; 1: yes)? "))
   print("-----------------")
   print("The Z-score of residuals for a subject indicates the significance level the subject is an outlier at a voxel.")
   print("Turn off this option and select 0 if memory allocation problem occurs later on.")
   resZout <- as.integer(readline("Want residuals Z-score for each subject (0: no; 1: yes)? "))
   if(resZout==1) {
      icc_FN  <- paste(outFN, "_ICC+orig", sep="")
      resZ_FN <- paste(outFN, "_resZ+orig", sep="") # write.AFNI doesn't handle tlrc yet
   }

   exclude <- TRUE  # exclude those voxels with 0 variance
   
   # Maybe I should avoid doing this below part for those voxels in the mask!!!
   if(anaType==1 | anaType==2 | anaType==4) for(ii in 1:nGrp) {
      bList[[ii]] <- lapply(bList[[ii]], function(x) x$ttt); tList[[ii]] <- lapply(tList[[ii]], function(x) x$ttt)
      varList[[ii]] <- mapply(function(x, y) ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), bList[[ii]], tList[[ii]], SIMPLIFY = FALSE)  # variances
      if(exclude) for (jj in 1:nSubj[ii]) varList[[ii]][[jj]][varList[[ii]][[jj]] < tolL] <- tolU  # replace those 0 variances with a big number
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
   
   rm(tList)
   
   #if(anaType==4) nBrick  <- 4*nGrp+(anyCov)*2*nCov+2+2*sum(nSubj)*resZout else nBrick <- 4*nGrp+(anyCov)*2*nCov+2*sum(nSubj)*resZout
   # each subject has two number: one for lambda, and the other, deviation, for outlier identificaiton - need to do the same for type 4
   
   nBrick0 <- 4*nGrp+(anyCov)*2*nCov   # no. sub-bricks in the main output
   nBrick <- 4*nGrp+(anyCov)*2*nCov+2*sum(nSubj)*resZout  # total sub-bricks in all output
   if(anaType==4) {nBrick0 <- nBrick0+2; nBrick <- nBrick+2}  # two more for anaType==4
      
   if(anaType==1 | anaType==2 | anaType==4) comArr <- array(c(unlist(c(bList)), unlist(c(varList))), dim=c(myDim[1:3], sum(nFiles)))
   if(anaType==3) comArr <- array(c(unlist(c(contrBList)), unlist(c(contrVarList))), dim=c(myDim[1:3], sum(nFiles)))

   rm(bList, varList)
   
   # mask out the junk: one slice at a time due to potential memory issue  
   if(as.logical(masked)) { for (kk in 1:myDim[3]) 
      comArr[,,kk,] <- array(apply(comArr[,,kk,], 3, function(x) x*maskData[,,kk,1]), dim=c(myDim[1:2],sum(nFiles)))
      rm(maskData)
   }

   outArr <- array(0, dim=c(myDim[1:3], nBrick))
   #outArr <- array(0, dim=c(myDim[1:3], nBrick0)) 
   #if(resZout==1) outArrRes <- array(0, dim=c(myDim[1:3], 2*sum(nSubj)))
   outData<-vector(mode="numeric", length= nBrick)  # initialization for use in runRMA: 3 beta's + 3 z-scores

   print("-----------------")
#   oTh <- as.numeric(readline("Outlier definition: how much in unit of standard deviation (e.g., 1, 1.5, 2)? ")) 
   
   print(sprintf("Totally %i slices in the data.", myDim[3]))
   print("-----------------")
   print("Starting to analyze data slice by slice...")
   # single processor
   
   if(anaType==4) {
      if(nNodes==1) for (ii in 1:myDim[3]) {
         outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runRMA, nGrp=nGrp, n=c(nSubj[1], sum(nSubj)), p=dim(xMat)[2], xMat=xMat, outData=outData, 
            mema=rmaB2, lapMod=lapMod, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, anaType=anaType, resZout=resZout, tol=tolL), c(2,3,1))
         cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   
      # multi-processing
      if(nNodes>1) {
         libLoad('snow')
         cl <- makeCluster(nNodes, type = "SOCK")
         for(ii in 1:myDim[3]) {
            outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), runRMA, nGrp=nGrp, n=c(nSubj[1], sum(nSubj)), p=dim(xMat)[2], xMat=xMat, outData=outData, 
               mema=rmaB2, lapMod=lapMod, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, anaType=anaType, resZout=resZout, tol=tolL), c(2,3,1))
            cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      }  # if(nNodes>1)
   } else {
      if(nNodes==1) for (ii in 1:myDim[3]) {
         outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runRMA, nGrp=nGrp, n=sum(nSubj), p=dim(xMat)[2], xMat=xMat, outData=outData, 
            mema=rmaB, lapMod=lapMod, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, anaType=anaType, resZout=resZout, tol=tolL), c(2,3,1))
         cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   
      # multi-processing
      if(nNodes>1) {
         libLoad('snow')
         cl <- makeCluster(nNodes, type = "SOCK")
         for(ii in 1:myDim[3]) {
            outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), runRMA, nGrp=nGrp, n=sum(nSubj), p=dim(xMat)[2], xMat=xMat, outData=outData, 
               mema=rmaB, lapMod=lapMod, KHtest=KHtest, nNonzero=nNonzero, nCov=nCov, nBrick=nBrick, anaType=anaType, resZout=resZout, tol=tolL), c(2,3,1))
            cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      }  # if(nNodes>1)
   } # if(anaType==4)

   print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
   
   print("#++++++++++++++++++++++++++++++++++++++++++++")

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
      outLabel <- append(outLabel, sprintf("%s:b", covName[ii]))
      if(KHtest) outLabel <- append(outLabel, sprintf("%s:t", covName[ii])) else
      outLabel <- append(outLabel, sprintf("%s:Z", covName[ii]))
   } 
   
   if(anaType==4) {for(ii in 1:nGrp) {
      #outLabel <- append(outLabel, "tau1^2")
      #outLabel <- append(outLabel, "tau2^2")
      #outLabel <- append(outLabel, sprintf("%s:tau^2", grpLab[[ii]]))
      outLabel <- append(outLabel, sprintf("%s:QE", grpLab[[ii]]))
      }
      outLabel <- append(outLabel, "tau1^2>tau2^2")
      outLabel <- append(outLabel, "tau2^2>tau1^2")
   } else {
      outLabel <- append(outLabel, "tau^2")
      outLabel <- append(outLabel, "QE:Chisq")  
   }
   
   if(resZout==1) for(ii in 1:nGrp) for(jj in 1:nSubj[ii]) {
      if(ii==1 & jj==1) {
         iccLabel  <- paste(sprintf("%s-lambda", subjLab[[ii]][[jj]])) 
         resZLabel <- paste(sprintf("%s-Res:Z", subjLab[[ii]][[jj]]))
      } else {
         iccLabel  <- append(iccLabel, sprintf("%s-lambda", subjLab[[ii]][[jj]]))
         resZLabel <- append(resZLabel, sprintf("%s-Res:Z", subjLab[[ii]][[jj]]))
      }  
   }   

   ############ NEED TO MODIFY HERE!!!!!!!!!!!!!!!!!!############################
   # t and chi-sq for QE with df = sum(nFiles)/2 - nGrp (need changes later with more complicated models
   
   nDF <- sum(nFiles)/2-nGrp-nCov
   
   statpar <- "3drefit"
   if (KHtest) for (ii in 1:(2*nGrp-1)) statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fitt ", nDF) else
      for (ii in 1:(2*nGrp-1)) statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fizt")
   
   if(anyCov) if (KHtest) for(ii in 1:nCov) statpar <- paste(statpar, " -substatpar ", nBrick0-3-2*(nCov-ii), " fitt ", nDF) else
      for(ii in 1:nCov) statpar <- paste(statpar, " -substatpar ", nBrick0-3-2*(nCov-ii), " fizt")
   
   if(anaType==4) {
      statpar <- paste(statpar, " -substatpar ", nBrick0-3, " fict ", nSubj[1]-nCov) # Chi-sq for QE: group 1
      statpar <- paste(statpar, " -substatpar ", nBrick0-2, " fict ", nSubj[2]-nCov) # Chi-sq for QE: group 2
   } else statpar <- paste(statpar, " -substatpar ", nBrick0-1, " fict ", nDF)
   
   # last brick: QE with chi-sq
 
   # Z-score for residuals
   if(resZout==1) {
      statparICC <- "3drefit"; statparResZ <- "3drefit"
      for(ii in 1:sum(nSubj)) statparResZ <- paste(statparResZ, " -substatpar ", ii-1, " fizt")
   }

   #rm(comArr)
   #outArr[outArr[1:(2*sum(nSubj))]>tTop] <- tTop  # Avoid outflow!!!!
   #outArr[outArr[1:(2*sum(nSubj))] < (-tTop)] <- -tTop  # Avoid outflow!!!!
   write.AFNI(outFN, outArr[,,,1:nBrick0], outLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")   
   statpar <- paste(statpar, " -view tlrc -addFDR -newid ", outFN)  # assume tlrc space: wrong for monkey study, for example
   system(statpar)

   #if(resZout==1) {
   #   write.AFNI(resFN, outArr[,,,(nBrick0+1):nBrick], resLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")
   #   statparRes <- paste(statparRes, " -view tlrc -addFDR -newid ", resFN)
   #   system(statparRes)
   #}
   
   if(resZout==1) {
      write.AFNI(icc_FN, outArr[,,,seq((nBrick0+1), nBrick, by=2)], iccLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")
      write.AFNI(resZ_FN, outArr[,,,seq((nBrick0+2), nBrick, by=2)], resZLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")
      statparICC  <- paste(statparICC, " -view tlrc -newid ", icc_FN)
      statparResZ <- paste(statparResZ, " -view tlrc -addFDR -newid ", resZ_FN)
      system(statparICC)
      system(statparResZ)
   }
   
   geterrmessage()

 
# } # if (nGrp==1)

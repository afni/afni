print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 3dKS.R ==================          ")
print("AFNI Kolmogorov-Smirnov testing program!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.1,  Nov. 23, 2009")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website - http://afni.nimh.nih.gov/sscc/gangc/")
print("SSCC/NIMH, National Institutes of Health, Bethesda MD 20892")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

#########
##  
##  
##   
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
  if (all(values$BRICK_TYPES==0) | all(values$BRICK_TYPES==1)) mybrk<- readBin(conbrik, "int", n=dx*dy*dz*dt, size=size, signed=TRUE, endian=endian) # unsigned charater or short
  if (all(values$BRICK_TYPES==3)) mybrk<- readBin(conbrik, "numeric", n=dx*dy*dz, size=size, signed=TRUE, endian=endian) # float        
    close(conbrik)
    dim(mybrk) <- c(dx,dy,dz,dt)
#    for (k in 1:dt) {
#      if (scale[k] != 0) {
#        cat("scale",k,"with",scale[k],"\n")
#        cat(range(mybrk[,,,k]),"\n")
#        mybrk[,,,k] <- scale[k] * mybrk[,,,k]
#        cat(range(mybrk[,,,k]),"\n")
#      }
#    }
    for (k in 1:dt) if (scale[k] != 0) mybrk[,,,k] <- scale[k] * mybrk[,,,k]

  mask <- array(TRUE,c(dx,dy,dz))
  mask[mybrk[,,,1] < quantile(mybrk[,,,1],0.75)] <- FALSE
    z <-
      list(brk=mybrk,format="HEAD/BRIK",delta=values$DELTA,origin=values$ORIGIN,orient=values$ORIENT_SPECIFIC,dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)
#      list(brk=writeBin(as.numeric(mybrk),raw(),4),format="HEAD/BRIK",delta=values$DELTA,origin=values$ORIGIN,orient=values$ORIENT_SPECIFIC,dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)

  } else {
    warning("Error reading file: Could not detect size per voxel\n")
    z <- list(brk=NULL,format="HEAD/BRIK",delta=NULL,origin=NULL,orient=NULL,dim=NULL,weights=NULL,header=values,mask=NULL)    
  }

  class(z) <- "fmridata"
  attr(z,"file") <- paste(filename,".HEAD/BRIK",sep="")
  invisible(z)
}

write.AFNI <- function(filename, brk, label, note="", origin=c(0,0,0), delta=c(4,4,4), idcode="WIAS_noid") {
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
  mm <- minmax(brk)
  writeChar(AFNIheaderpart("float-attribute","BRICK_STATS",mm),conhead,eos=NULL)
  writeChar(AFNIheaderpart("integer-attribute","DATASET_RANK",c(3,dim(brk)[4],0,0,0,0,0,0)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","DATASET_DIMENSIONS",c(dim(brk)[1:3],0,0)),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","BRICK_TYPES",rep(1,dim(brk)[4])),conhead,eos=NULL)  

  scale <- rep(0,dim(brk)[4])
  for (k in 1:dim(brk)[4]) {
    scale[k] <- max(abs(mm[2*k-1]),abs(mm[2*k]))/32767
    brk[,,,k] <- brk[,,,k] / scale[k]
  }

  writeChar(AFNIheaderpart("float-attribute","BRICK_FLOAT_FACS",scale),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BRICK_LABS",paste(label,collapse="~")),conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BYTEORDER_STRING","MSB_FIRST"),conhead,eos=NULL)  
  close(conhead)

  conbrik <- file(paste(filename, ".BRIK", sep=""), "wb")
  dim(brk) <- NULL
  writeBin(as.integer(brk), conbrik,size=2, endian="big")
  close(conbrik)
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


runKS2 <- function(x, n1, n) {
   options(warn=-1)
   z <- ks.test(x[1:n1], x[(n1+1):n])  # two-tailed by default
   outData <- c(unname(z$statistic), z$p.value)
   return(outData)
}

#options(show.error.messages = FALSE)  # suppress error message when running with single processor


print("################################################################")
#print("Please consider citing the following if this program is useful for you:")
#cat("\n\tGang Chen et al., hopefully something coming soon...\n")
#cat("\n\thttp://afni.nimh.nih.gov/sscc/gangc/MEMA.html\n")
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
#nGrp <- 2

grpLab <- vector('list', nGrp)
nFiles <- vector('integer', nGrp) # number of input files for each group

print("-----------------")

bFN <- vector('list', nGrp)
bList <- vector('list', nGrp)
bArr <- vector('list', nGrp)
   
for(ii in 1:nGrp) {
   if(nGrp==1) grpLab[[ii]] <- "Group" else 
      grpLab[[ii]] <- readline(sprintf("Label for group %i? ", ii))
   nFiles[ii] <- as.integer(readline(sprintf("Number of subjects in group %s (e.g., 12)? ", grpLab[[ii]])))
   bFN[[ii]] <- vector('integer', nFiles[ii]) # list of beta file names
         
   for(jj in 1:nFiles[ii]) bFN[[ii]][[jj]] <- readline(sprintf("No. %i subject input file in group %s: ", jj, grpLab[[ii]]))

   bList[[ii]] <- lapply(bFN[[ii]], read.AFNI)
   
   if(ii==1) {myNote=bList[[1]][[1]]$header$HISTORY_NOTE; myOrig=bList[[1]][[1]]$origin; myDelta=bList[[1]][[1]]$delta; myDim <- bList[[1]][[1]]$dim}
   lapply(lapply(bList[[ii]], function(x) x$dim), function(x) if(!all(x==myDim)) stop("Dimension mismatch among the input files!"))
   
   bList[[ii]] <- lapply(bList[[ii]], function(x) x$brk)
#   bArr[[ii]] <- array(unlist(c(bList[[ii]])), dim=c(myDim[1:3], nFiles[ii]))   

} # for(ii in 1:nGrp)
   
   nTot <- sum(nFiles)
   comArr <- array(unlist(c(bList)), dim=c(myDim[1:3], nTot))
   rm(bList)   
   print("-----------------")
   #print("Masking is optional, but will alleviate unnecessary penalty on q values of FDR correction.")
   print("Masking is optional.")
   
   masked <- as.integer(readline("Any mask (0: no; 1: yes)? "))
   if(masked) {maskFN <- readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); maskData <- read.AFNI(maskFN)$brk}
   if(masked) if(!all(dim(maskData[,,,1])==myDim[1:3])) stop("Mask dimensions don't match the input files!")
      
   nBrick <- 2   # no. sub-bricks in the main output
   
   
   # mask out the junk: one slice at a time due to potential memory issue  
   if(as.logical(masked)) { for(ii in 1:nGrp) for (kk in 1:myDim[3]) 
      bArr[[ii]][,,kk,] <- array(apply(bArr[[ii]][,,kk,], 3, function(x) x*maskData[,,kk,1]), dim=c(myDim[1:2],nFiles[ii]))
      rm(maskData)
   }

   outArr <- array(0, dim=c(myDim[1:3], nBrick))
   print("-----------------")
   
   print(sprintf("Totally %i slices in the data.", myDim[3]))
   print("-----------------")
   print("Starting to analyze data slice by slice...")
   # single processor
   
      if(nNodes==1) for (ii in 1:myDim[3]) {
         outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runKS2, nFiles[1], nTot), c(2,3,1))
         cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   
      # multi-processing
      if(nNodes>1) {
         libLoad('snow')
         cl <- makeCluster(nNodes, type = "SOCK")
         for(ii in 1:myDim[3]) {
            outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), runKS2, nFiles[1], nTot), c(2,3,1))
            cat("Z slice #", ii, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      }  # if(nNodes>1)

   
   outArr[,,,2] <- qnorm(1-outArr[,,,2]/2)  # convert two-tailed p to Z-score
   print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
   
   print("#++++++++++++++++++++++++++++++++++++++++++++")

   
   outLabel <- paste("D"); outLabel <- append(outLabel, "Z")
   statpar <- "3drefit"
   
   write.AFNI(outFN, outArr[,,,1:nBrick], outLabel, note=myNote, origin=myOrig, delta=myDelta, idcode="whatever")   
   statpar <- paste(statpar, " -substatpar 1 fizt -view tlrc -addFDR -newid ", outFN)  # assume tlrc space
   system(statpar)

   
   geterrmessage()

 
# } # if (nGrp==1)

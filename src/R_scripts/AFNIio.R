#------------------------------------------------------------------
# Extracted (and modified) from fmri library by Karsten Tabelow, 
# tabelow@wias-berlin.de and Joerg Polzehl (polzehl@wias-berlin.de)
#------------------------------------------------------------------


read.AFNI <- function(filename) {
  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- tolower(fileparts[length(fileparts)])
  
  #deal with sub-brick and range selectors
  selecs <- strsplit(ext,"\\[|\\{|<")[[1]];
  
  ext <- selecs[1];
  brsel <- '';
  rosel <- '';
  rasel <- '';
  for (ss in selecs[2:length(selecs)]) {
   if (length(grep("]",ss))) {
      brsel <- strsplit(ss,"\\]")[[1]][1];
   } else if (length(grep("}",ss))) {
      rosel <- strsplit(ss,"\\}")[[1]][1];
   } else if (length(grep(">",ss))) {
      rasel <- strsplit(ss,">")[[1]][1];
   }
  } 
  
  if (ext == "head") {
    filename.head <- paste(c(fileparts[-length(fileparts)],"HEAD"),collapse=".")
    filename.brik <- paste(c(fileparts[-length(fileparts)],"BRIK"),collapse=".")
  } else if (ext == "brik") {
    filename.head <- paste(c(fileparts[-length(fileparts)],"HEAD"),collapse=".")
    filename.brik <- paste(c(fileparts[-length(fileparts)],"BRIK"),collapse=".")
  } else {
    filename.head <- paste(filename,".HEAD",sep="")
    filename.brik <- paste(filename,".BRIK",sep="")
  }
  
  vp <- strsplit(filename.head, ".HEAD|\\+")[[1]];
  filename.prefix <- vp[1];
  filename.view   <- paste('+',vp[2],sep='');
   
  
  if (0) {
   cat ('ZSS:ext=', ext, '\n',
        'ZSS:brsel=', brsel, '\n',
        'ZSS:rosel=', rosel, '\n',
        'ZSS:rasel=', rasel, '\n',
        'ZSS:pref.=', filename.prefix, '\n',
        'ZSS:view =', filename.view, '\n',
        'ZSS:head =', filename.head, '\n');
  }
  
  #If you have any selectors, use 3dbucket to get what you want, then read
  #temp dset. This is an ugly fix for now, but will change it later if
  #I/O is issue
  rmtmp <- 0;
  if (length(brsel) > 0 || length(rosel) > 0 ||  length(rasel) > 0) {
    rmtmp <- 1;
    com <- paste ('3dcalc -overwrite -prefix ___R.read.AFNI.' ,
               filename.prefix, ' -a "', filename,'" -expr "a" >& /dev/null', 
               sep = '');
    if (try(system(com)) != 0) {
      warning(paste("Failed to execute:\n   ", com),
              immediate. = TRUE);
      return(NULL);
    }
    filename.head <- paste('___R.read.AFNI.',filename.head, sep = '');
    filename.brik <- paste('___R.read.AFNI.',filename.brik, sep = '');
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
  if (all(values$BRICK_TYPES==0) | all(values$BRICK_TYPES==1)) {
    myttt<- readBin( conbrik, "int", n=dx*dy*dz*dt, size=size, 
                     signed=TRUE, endian=endian) # unsigned charater or short
  }
  if (all(values$BRICK_TYPES==3)) {
    myttt<- readBin(conbrik, "numeric", n=dx*dy*dz*dt, size=size, 
                    signed=TRUE, endian=endian) # float        
  }
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
  z <- list(ttt=myttt,format="HEAD/BRIK",delta=values$DELTA,
            origin=values$ORIGIN,
            orient=values$ORIENT_SPECIFIC,
            dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)
#      list(ttt=writeBin(as.numeric(myttt),raw(),4),
#            format="HEAD/BRIK",delta=values$DELTA,origin=values$ORIGIN,
#            orient=values$ORIENT_SPECIFIC,dim=c(dx,dy,dz,dt),weights=weights, 
#            header=values,mask=mask)

  } else {
    warning("Error reading file: Could not detect size per voxel\n")
    z <- list(ttt=NULL,format="HEAD/BRIK",delta=NULL,
              origin=NULL,orient=NULL,dim=NULL,weights=NULL,
              header=values,mask=NULL)    
  }

  class(z) <- "fmridata"
  attr(z,"file") <- paste(filename,".HEAD/BRIK",sep="")
  invisible(z)
  
  if (rmtmp == 1) {
   if (0) {
      cat ('ZSS: Will remove tmp files\n');
   }
   system('\\rm -f ___R.read.AFNI.* >& /dev/null');
  }else{
   if (0) {
      cat ('ZSS: No temps to remove\n');
   }
  }
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

read.NIFTI <- function(filename) {
  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- tolower(fileparts[length(fileparts)])

  if (ext == "nii") {
    filename.nii <- filename
    filename.hdr <- paste(c(fileparts[-length(fileparts)],"hdr"),collapse=".")
    filename.img <- paste(c(fileparts[-length(fileparts)],"img"),collapse=".")
  } else if (ext == "hdr") {
    filename.hdr <- filename
    filename.img <- paste(c(fileparts[-length(fileparts)],"img"),collapse=".")
  } else if (ext == "img") {
    filename.hdr <- paste(c(fileparts[-length(fileparts)],"hdr"),collapse=".")
    filename.img <- filename
  } else {
    filename.nii <- paste(filename,".nii",sep="")
    filename.hdr <- paste(filename,".hdr",sep="")
    filename.img <- paste(filename,".img",sep="")
  }

  if ((ext != "hdr") && (ext != "img") && (!is.na(file.info(filename.nii)$size))) {
    con <- file(filename.nii,"rb")
    header <- read.NIFTI.header(con)
    if (!(header$magic == "n+1") && !(header$magic == "ni1")) 
      warning("Hmmm! Dont see the magic NIFTI string! Try to proceed, but maybe some weird results will occur!");
    bytes <- header$voxoffset - 348
    header$extension <- readBin(con,"raw",bytes)
  } else {
    if (is.na(file.info(filename.hdr)$size) | (file.info(filename.hdr)$size < 348))
      stop("Hmmm! This does not seem to be a NIFTI header (hdr/img-pair)! Wrong size or does not exist!");
    con <- file(filename.hdr,"rb")
    header <- read.NIFTI.header(con)
    header$extension <- NULL  
    close(con)
    if (is.na(file.info(filename.img)$size))     
      stop("Hmmm! This does not seem to be a NIFTI header (hdr/img-pair)! img-file not found!");
    con <- file(filename.img,"rb")
  }
    
  dx <- header$dimension[2]
  dy <- header$dimension[3]
  dz <- header$dimension[4]
  dt <- header$dimension[5]
  endian <- header$endian
  if (header$datatype == 1) { # logical
    what <- "raw"
    signed <- TRUE
    size <- 1
  } else if (header$datatype == 2) { # unsigned char????
    what <- "int"
    signed <- FALSE
    size <- if (header$bitpix) header$bitpix/8 else 2
  } else if (header$datatype == 4) { # signed short
    what <- "int"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 2
  } else if (header$datatype == 8) { # signed integer
    what <- "int"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 4
  } else if (header$datatype == 16) { # float
    what <- "double"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 4
  } else if (header$datatype == 32) { # complex
    what <- "complex"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 8
  } else if (header$datatype == 64) { # double
    what <- "double"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 8
  } else { # all other
    what <- "raw"
    signed <- TRUE
    size <- 1
  }
  ttt <- readBin(con, what, n=dx*dy*dz*dt, size=size, signed=signed, endian=endian) 
  close(con)

  if (min(abs(header$pixdim[2:4])) != 0) {
    weights <-
      abs(header$pixdim[2:4]/min(abs(header$pixdim[2:4])))
  } else {
    weights <- NULL
  }
  dim(ttt) <- c(dx,dy,dz,dt)

  mask <- array(TRUE,c(dx,dy,dz))
  mask[ttt[,,,1] < quantile(ttt[,,,1],0.75)] <- FALSE

  z <- list(ttt=writeBin(as.numeric(ttt),raw(),4),format="NIFTI",delta=header$pixdim[2:4],
                origin=NULL,orient=NULL,dim=header$dimension[2:5],weights=weights,header=header,mask=mask)

  class(z) <- "fmridata"

  invisible(z)
}

extract.data <- function(z,what="data") {
  if (!("fmridata"%in%class(z))) {
    warning("extract.data: data not of class <fmridata>. Try to proceed but strange things may happen")
  }
  if (what=="residuals") {  
      if(!is.null(z$resscale)){
          ttt <- readBin(z$res,"integer",prod(z$dim),2)*z$resscale 
          dim(ttt) <- z$dim
          } else {
          warning("extract.data: No residuals available, returning NULL")
          ttt <- NULL
      }
      } else { 
      ttt <- readBin(z$ttt,"numeric",prod(z$dim),4)
      dim(ttt) <- z$dim
      }
 
  invisible(ttt)
}

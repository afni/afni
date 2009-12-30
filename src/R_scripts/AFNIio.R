#------------------------------------------------------------------
# Functions to deal with AFNI file names
#------------------------------------------------------------------

parse.AFNI.name <- function(filename, verb = 0) {
  an <- list()
  an$view <- NULL
  an$prefix <- NULL
  an$head <- NULL
  an$brik <- NULL
  an$compress <- ''
  an$brsel <- NULL;
  an$rosel <- NULL;
  an$rasel <- NULL;
  
  if (verb) { cat ('Parsing >>',filename,'<<\n', sep=''); }
  if (!is.character(filename)) {
   warning(paste('filename >>', filename, '<< not a character string\n', sep=''),
              immediate. = TRUE);
   traceback();
   return(NULL);
  }  
  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- fileparts[length(fileparts)]
  if (!is.na(match(ext,"Z")) ||
      !is.na(match(ext,"gz")) ) {
   #bext <- paste("BRIK",ext,collapse='.') 
   bext <- 'BRIK'
   an$compress <- ext
   ext <- tolower(fileparts[length(fileparts)-1])   
  } else {
    bext <- 'BRIK'
    ext <- tolower(ext)
  }
  
  #deal with sub-brick and range selectors
  selecs <- strsplit(ext,"\\[|\\{|<")[[1]];
  
  ext <- selecs[1];
  for (ss in selecs[2:length(selecs)]) {
   if (length(grep("]",ss))) {
      an$brsel <- strsplit(ss,"\\]")[[1]][1];
   } else if (length(grep("}",ss))) {
      an$rosel <- strsplit(ss,"\\}")[[1]][1];
   } else if (length(grep(">",ss))) {
      an$rasel <- strsplit(ss,">")[[1]][1];
   }
  } 

  if (ext == "head") {
    an$head <- paste(c(fileparts[-length(fileparts)],"HEAD"),collapse=".")
    an$brik <- paste(c(fileparts[-length(fileparts)],bext),collapse=".")
  } else if (ext == "brik") {
    an$head <- paste(c(fileparts[-length(fileparts)],"HEAD"),collapse=".")
    an$brik <- paste(c(fileparts[-length(fileparts)],bext),collapse=".")
  } else {
    an$head <- paste(filename,".HEAD",sep="")
    an$brik <- paste(filename,".BRIK",sep="")
  }
  
  
  vp <- strsplit(an$head, ".HEAD|\\+")[[1]];
  an$prefix <- vp[1];
  an$view   <- paste('+',vp[2],sep='');
  
  return(an)
}

exists.AFNI <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   
   ans <- 0
   if (file.exists(an$head)) ans <- ans + 1;
   
   if (file.exists(an$brik) ||
       file.exists(paste(an$brik,'.gz', sep='')) ||
       file.exists(paste(an$brik,'.Z', sep=''))) ans <- ans + 2;
   return(ans);
}

uncompress.AFNI <- function(an, verb = 1) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   
   ans <- 0
   
   zz <- paste(an$brik,'.Z', sep='');
   if (file.exists(zz)) {
      if (verb) {
         cat ('Uncompressing', zz, '\n');
      }
      system(paste('uncompress', zz));
   }

   zz <- paste(an$brik,'.gz', sep='');
   if (file.exists(zz)) {
      if (verb) {
         cat ('gzip -d-ing', zz, '\n');
      }
      system(paste('gzip -d', zz));
   }
   
   
   return(an);
}
  
show.AFNI.name <- function(an) {
   cat ('\n',
        'pref.=', an$prefix, '\n',
        'view =', an$view, '\n',
        'head =', an$head, '\n',
        'brik =', an$brik, '\n',
        'brsel=', an$brsel, '\n',
        'rosel=', an$rosel, '\n',
        'rasel=', an$rasel, '\n',
        'exist=', exists.AFNI(an), '\n');
}

#------------------------------------------------------------------
# Functions to parse command-line arguments
#------------------------------------------------------------------
show.AFNI.args <- function (ops) {
   if (is.null(ops)) {
      cat ('NULL options\n');
   } else {
      cat ('Allowed Options:\n');
      if (length(ops[['allowed_options']])) {
         for (i in 1:length(ops[['allowed_options']])) {
            cat (' ', ops[['allowed_options']][i], '\n');
         }
      } else {
         cat ('whatever grinds your beans');
      }
      cat ('User options:\n');
      for (i in 1:length(ops)) {
         if ((names(ops)[i] != 'allowed_options')) {
            cat (' ', names(ops)[i], ': ', ops[[i]],'\n');
         }
      }
   }
}

check.AFNI.args <- function ( ops, params = NULL, help = NULL) {
   if (!is.null(params) && !is.null(ops)) {
      for (i in 1:length(ops)) {
         #str(names(ops)[i])
         #str(params[names(ops)[i]][[1]])
         #cat('\nChecking on ', paste(ops[[i]],collapse=','),'\n');
         ipar <- which(names(ops)[i] == names(params));
         if (length(ipar)) {
            pp <- params[ipar[1]][[1]];
            opsvec <- ops[[i]];
            if (length(pp) == 1) { #exact number 
               if (length(opsvec) !=  pp) {
                  warning(paste( 'Expecting ',pp, ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = ''),
                          immediate. = TRUE);
                  return(0);                  
               }
            } else if (length(pp) == 2) { #range
               if (length(opsvec) <  pp[1] || length(opsvec) >  pp[2]) {
                  if (pp[2] == Inf) {
                     warning(paste( 'Expecting more than ',pp[1],  
                                 ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = ''),
                          immediate. = TRUE);
                  } else {
                     warning(paste( 'Expecting ',pp[1], ' to ', pp[2], 
                                 ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = ''),
                          immediate. = TRUE);
                  }
                  return(0);                  
               }
               
            } else {
               warning(paste( 'I do not know what to do here'),
                        immediate. = TRUE);
               return(0);   
            }  
         } else {
            #ok
         } 
      }
   }
   return(1); #all ok
}

parse.AFNI.args <- function ( args, params = NULL, 
                              other_ok=TRUE, duplicate_ok=vector('character'),
                              verb = 0, help = NULL) {
   #for (i in 1:length(args)) {
   #   cat (i, args[[i]],'\n');
   #}
   if (!is.null(params)) {
      allowed_options <- names(params);
   } else {
      allowed_options <- vector('character');
   }
   #Sanity
   { 
      if (length(duplicate_ok) && !length(allowed_options)) {
         warning(paste('Cannot specify duplicate_ok without params'),
                      immediate. = TRUE);
         return(NULL);
      }
      if (length(duplicate_ok) && length(allowed_options)) {
         dd <- duplicate_ok[!(duplicate_ok %in% allowed_options)];
         #str(dd)
         if (length(dd) ) {
            warning(paste('Cannot have duplicate_ok conditions for options ',
                          'not in params.\n',
                          '  Option(s) "', paste(dd, collapse=', '),
                          '" in duplicate_ok and not in params.'),
                      immediate. = TRUE);
            return(NULL);
         }
      }
   }

   #find locations of -*
   ii <- grep ('^-.*', args);
   iflg <- vector('numeric')
   for (i in 1:length(ii)) {
      if (!is.num.string(args[[ii[i]]])) {
         if (!length(allowed_options)) {
            iflg <- append(iflg, ii[i]);
         } else { #Make sure it is an acceptable name
            if (length(which(args[[ii[i]]] == allowed_options))) {
               iflg <- append(iflg, ii[i]);
            }
         }
      }     
   }
   
   if (verb) paste(args[iflg])
   
   ops = list()
   used <- vector('logical', length(args));
   if (length(iflg)) {
      iflg <- append(iflg,length(args)+1)
      #store results
      nm <- vector('character');
      for (i in 1:(length(iflg)-1)) {
         if (0) { # Why remove the -?, makes things inconsistent elsewhere
            #newnm <- strsplit(args[[iflg[i]]],'-')[[1]][2] 
         } else {
            newnm <- args[[iflg[i]]]
         }
         if (length(nm) && length(which(newnm == nm)) &&
             (!length(duplicate_ok) || length(which(iflg[i] == duplicate_ok))) ){
            warning(paste('option ', newnm, 'already specified.\n'),
                     immediate. = TRUE);
            show.AFNI.args(ops)
            return(NULL); 
         }
         nm <- append(nm, newnm)
         
         used[iflg[i]] <- TRUE;
         istrt = iflg[i]+1;
         pp <- vector('character');
         if (istrt <= length(args) && istrt != iflg[i+1]) {
            iend <- max(c(iflg[i+1]-1, istrt));
            for (ii in istrt:iend) {
               pp <- append(pp, args[[ii]]);
               used[ii] <- TRUE;
            }
         }
         #create a cleaned up string
         pp <- paste(pp, collapse = ' ')
         pp <- strsplit(clean.args.string(pp), ' ')
         ops <- c(ops, (pp));
         names(ops)[length(ops)] <- newnm;
      }
   }
   
   #cleanup
   if (length(ops)) {
      for (i in 1:length(ops)) {
         #ops[[i]] <- clean.args.string(ops[[i]])
      }
   }
   
   #numeric changes 
   if (length(ops)) {
      for (i in 1:length(ops)) {
         if (is.num.string(ops[[i]])) {
            ops[[i]] <- as.numeric(ops[[i]]);
         }
      }
   }
   
   #defaults
   pp <- c(args[used == FALSE])
   ops <- c (ops, list("other"=pp));
   
   #add allowed options
   ops <- c (ops, list("allowed_options"=allowed_options));
   
   if (!other_ok) {
      if (length(ops[['other']])) {
         warning(paste('Illegal parameters on command line\n',
                       ops['other'],'\n'),
                       immediate. = TRUE);
         show.AFNI.args(ops)
         return(NULL); 
      }
   }
   
   #check 
   if (!check.AFNI.args(ops, params)) {
      return(NULL);
   } else {
      return(ops);
   }
}

#------------------------------------------------------------------
#   Some utilities
#------------------------------------------------------------------
#return 1 if all strings in vector ss can be changed to numbers
is.num.string <- function(ss) {
   if (is.null(ss) || !length(ss) || ss == '' ||
       is.null(tryCatch(as.numeric(ss), 
                           warning=function(ex) {}))) {
      return(0);
   } else {
      return(1);
   }
}

clean.args.string <- function(ss) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   #remove trailing whites
   ss <- sub('^[[:space:]]*','',ss);
   ss <- sub('[[:space:]]*$','',ss);  
   #remove multiple whites
   ss <- gsub('[[:space:]]+',' ',ss);  
   #treat = nicely
   ss <- gsub('[[:space:]]*=[[:space:]]*','=',ss)
   return(ss)
}

as.num.vec <- function(ss) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   ss <- clean.args.string(ss)
   dd <- strsplit(ss,' ')[[1]];
   nn <- vector('numeric');
   ww <- vector('character');
   for (ii in 1:length(dd)) {
      vv <- strsplit(dd[ii],'=')[[1]];
      if (length(vv) > 1) {
         ll <- vv[1] 
         vv <- as.numeric(vv[length(vv)]);
         if (is.na(vv)) { return(NULL); }
      } else {
         vv <- as.numeric(vv[1]);
         if (is.na(vv)) { return(NULL); }
         ll <- paste('v',as.numeric(vv[1]), sep='')
      }
      
      nn <- c(nn,vv)
      ww <- c(ww,ll)
   }
   names(nn) <- ww
   return(nn)
}
as.char.vec <- function(ss) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   ss <- clean.args.string(ss)
   dd <- strsplit(ss,' ')[[1]];
   nn <- vector('character');
   ww <- vector('character');
   for (ii in 1:length(dd)) {
      vv <- strsplit(dd[ii],'=')[[1]];
      if (length(vv) > 1) ll <- vv[1] 
      else ll <- paste('v',as.character(vv[1]), sep='')
      
      vv <- as.character(vv[length(vv)]);
      if (is.na(vv)) { return(NULL); }
      
      nn <- c(nn,vv)
      ww <- c(ww,ll)
   }
   names(nn) <- ww
   return(nn)
}
#------------------------------------------------------------------
# Extracted (and modified) from fmri library by Karsten Tabelow, 
# tabelow@wias-berlin.de and Joerg Polzehl (polzehl@wias-berlin.de)
#
# Updates by ZSS & GC
#------------------------------------------------------------------

read.AFNI <- function(filename, verb = 0) {
  an <- parse.AFNI.name(filename);
  
  if (verb) {
   show.AFNI.name(an);
  }
  
  #If you have any selectors, use 3dbucket to get what you want, then read
  #temp dset. This is an ugly fix for now, but will change it later if
  #I/O is issue
  
  if (verb) { cat ('Need tmp?\n'); }
  rmtmp <- 0;
  if (!is.null(an$brsel) || !is.null(an$rosel) ||  !is.null(an$rasel)) {
    rmtmp <- 1;
    com <- paste ('3dcalc -overwrite -prefix ___R.read.AFNI.' ,
               basename(an$prefix), 
               ' -a "', filename,'" -expr "a" >& /dev/null', 
               sep = '');
    if (try(system(com)) != 0) {
      warning(paste("Failed to execute:\n   ", com),
              immediate. = TRUE);
      return(NULL);
    }
    an$head <- paste('___R.read.AFNI.',basename(an$head), sep = '');
    an$brik <- paste('___R.read.AFNI.',basename(an$brik), sep = '');
    if (!(exists.AFNI(an$head))) {
      warning(paste("Failed to create:   ", an$head, an$brik, '\n'),
              immediate. = TRUE);
      return(NULL);
    }
  }
  
  if (verb) { cat ('Checking existence\n'); }
  if (!(exists.AFNI(an$head))) {
    warning(paste("Failed to read:   ", an$head, an$brik, '\n'),
              immediate. = TRUE);
    return(NULL);
  }
  
  #Cannot read compressed stuff (see size usage below)
  if (verb) { cat ('Uncompressing\n'); }
  uncompress.AFNI(an$head);
  
  conhead <- file(an$head,"r")
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

  size <- file.info(an$brik)$size/(dx*dy*dz*dt)

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
  if (verb) { cat ('Reading Bin\n'); }
  if (as.integer(size) == size) {
    conbrik <- file(an$brik,"rb")
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
  if (verb) { cat ('Scaling\n'); }
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
  
  if (rmtmp == 1) {
   if (verb) {
      cat ('ZSS: Will remove tmp files\n');
   }
   system('\\rm -f ___R.read.AFNI.* >& /dev/null');
  }else{
   if (verb) {
      cat ('ZSS: No temps to remove\n');
   }
  }

  invisible(z);
  #return(z);
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

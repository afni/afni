#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

## top ####################
## 07/2023 discoraj
## run a chi squared test voxel wise
## for KiSueng Choi


##Clean up
rm(list = ls())

first.in.path <- function(file) {
  ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
  ff<-ff[lapply(ff,file.exists)==TRUE];
  return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))

ExecName <- '3dChiSquared'

## The help function ###########
help.opts <- function (params, alpha = TRUE, 
                       itspace='   ', adieu=FALSE, targ ='RAW') {
  
  intro <- 
    '
Usage:
------ 
 
'
  
  ex1 <- 
    "
Example 1 --- 


\n"      
  
  
  parnames <- names(params)
  ss <- vector('character')
  if (alpha) {
    parnames <- sort(parnames)   
    ss <- paste('Options in alphabetical order:\n',
                '------------------------------\n', sep='')
  } else {
    ss <- paste('Options:\n',
                '--------\n', sep='')
  }
  for (ii in 1:length(parnames)) {
    op <- params[parnames[ii]][[1]]
    if (!is.null(op$help)) {
      ss <- c(ss , paste(itspace, op$help, sep=''));
    } else {
      ss <- c(ss, paste(itspace, parnames[ii], 
                        '(no help available)\n', sep='')); 
    }
  }
  ss <- paste(ss, sep='\n');
  
  
  cat(intro, ex1, ss, sep='\n', 
      file=help.cat.file.AFNI(ExecName,targ));
  
  if (adieu) exit.AFNI();
}


## command line arguments ###################
read.opts.batch <- function (args=NULL, verb = 0) {
  params <- list (
    '-prefix' = apl(n = 1, d = NA,  h = paste(
      "-prefix PREFIX: Output prefix (just prefix, no view+suffix needed)\n"
    ) ),
    
    '-input' = apl (n = 1, d = NA, dup = TRUE, h = paste (
      "-input DSET1                       \\\n",
      "     Specify the dataset to be scaled. Note that you can use\n",
      "     the various sub-brick selectors used by AFNI\n",
      "     e.g: -input pb05.Regression+tlrc'[face#0_Beta]'  \\\n",
      "     You can use multiple instances of -input in one command line\n",
      "     to process multiple datasets in the same manner.\n" 
    ) ),
    
    '-scale' = apl(n = 1, d = 5, h = paste(
      "-scale SS: Multiply each voxel by SS \n"
    ) ),
    
    '-mask' = apl(1, h = paste(
      "-mask MASK: Process voxels inside this mask only.\n",
      "            Default is no masking.\n"
    ) ),
    
    '-dataTable' = apl(n=c(1,1000000),d=NA,h=paste(
      "-dataTable TABLE: List the data structure with a header as the first line.\n",
      "         NOTE:\n",
      sep = '\n'
    ) )
    
  );
  
  ops <- parse.AFNI.args(args,  params,other_ok=FALSE)
  
  if (is.null(ops)) {
    errex.AFNI('Error parsing arguments. See 3dChiSquared -help for details.')
  }
  
  #initialize with defaults
  com_history <- AFNI.command.history(ExecName,args,NULL)
  lop <- AFNI.new.options.list(history=com_history,parsed_args=ops)
  lop$input <- NULL
  lop$prefix <- 'ChiChi'
  lop$mask <- NULL
  lop$verb <- 0
  lop$iometh <- 'clib'
  lop$scale <- 6
  
  #Get user's input
  for (i in 1:length(ops)) {
    opname <- strsplit(names(ops)[i],'^-')[[1]];
    opname <- opname[length(opname)];
    switch(opname,
           prefix = lop$prefix  <- pprefix.AFNI.name(ops[[i]]),
           input  = lop$input <- c(lop$input, ops[[i]]),
           scale = lop$scale <- ops[[i]],
           mask = lop$mask <- ops[[i]],
           verb = lop$verb <- ops[[i]],
           dataTable = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
           group = lop$group <- ops[[i]],
           help = help.opts(params, adieu=TRUE, targ='RAW'),
           h_raw = help.opts(params, adieu=TRUE, targ='RAW'),
           h_spx = help.opts(params, adieu=TRUE, targ='SPX'),
           h_aspx = help.opts(params, adieu=TRUE, targ='ASPX'),
           h_txt = help.opts(params, adieu=TRUE, targ='TXT'),
           show_allowed_options = show.AFNI.args(ops, verb=0, 
                                                 hstr="3dRprogDemo's",adieu=TRUE)
    ) 
  }
  
  if (length(lop$input) < 1) {
    if (lop$verb) {
      str(lop)
      errex.AFNI('No input? Check dumped input struct above');
    } else {
      errex.AFNI('No input?');
    }
    return(NULL)
  }
  
  
  return(lop)
}# end of read.opts.batch

## Chi Squared by voxel NOT parallelized #################
ChiSq.slow <- function(idset=NULL,mdset=NULL,scale=2){
  
  ## check the data frame (expecting a file for now)
  lop$dataStr <- dtCheck_str2frame(lop$dataTable)
  
  ## input file column
  FileCol <- dim(lop$dataStr)[2]
  
  ## Number of input files
  lop$NoFile <- dim(lop$dataStr[1])[1]
  
  # cat('Reading input files now...\n\n')
  
  ## Read in the 1st input file so that we have the dimension information
  inData.1 <- read.AFNI(lop$dataStr[1,FileCol],verb=lop$verb,
                        meth=lop$iometh,forcedset=TRUE)
  dimx <- inData.1$dim[1] ; dimy <- inData.1$dim[2] ; dimz <- inData.1$dim[3]
  
  # ## test voxels
  # xinit <- dimx%/%3
  # if(dimy==1) { xinit <-1; yinit <- 1 } else { yinit <- dimy%/%2 }
  # if(dimz==1) { xinit <-1; zinit <- 1 } else { zinit <- dimz%/%2 }
  # ii <- xinit ; jj <- yinit ; kk <- zinit
  
  
  ## Save for writing output
  head <- inData.1
  
  ## get ALL the data in one big mess
  inData <- unlist(lapply(
    lapply(lop$dataStr[,FileCol],read.AFNI,verb=lop$verb,meth=lop$iometh,
           forcedset=TRUE),'[[',1))
  
  ## make it the correct dimensions. x,y,z,t
  dim(inData) <- c(dimx,dimy,dimz,lop$NoFile)
  
  ## get the data table
  all.df <- lop$dataStr
  
  ##
  all.df$Subj <- factor(all.df$Subj)
  
  ## fix here for finding the correct grouping variable (hardcoded "Group")
  all.df$Group <- factor(all.df$Group)
  
  ## for output volume (3 bricks manual for now)
  stat.out <- array(0,dim=c(dimx,dimy,dimz,3))
  
  ## for the stat info later
  deg.free <- NA
  
  ## loop by all voxels. need to add mask and parallelization 
  for( i in 1:dimx ){
    for( j in 1:dimy ){
      for( k in 1:dimz ){
        
        ## get the data for the current voxel and add to the data table
        all.df$Beta<-inData[i,j,k,1:lop$NoFile]
        
        ## check for "binary".  need to change here for more than binary
        if( length(unique(all.df$Beta)) == 2 ){
          
          ## do the test and save out in the correct voxel location
          chisq.out <- suppressWarnings(chisq.test(all.df$Beta,all.df$Group))
          stat.out[i,j,k,1] <- chisq.out$statistic
          stat.out[i,j,k,2] <- chisq.out$p.value
          stat.out[i,j,k,3] <- 1 - chisq.out$p.value
        }
        
      } ## end dimz
    }   ## end dimy
  }     ## end dimx
  
  ## save out degrees of freedom (need a better way to do this...)
  if( is.na(deg.free) ){ deg.free <- chisq.out$parameter }
  
  ## sb is subbrik starting at 0
  stat.sym <- list(list(sb=0,typ="fict",par=1))
  
  ## old one for reference:
  # write.AFNI("my_file.nii.gz", stat.out, c("t-stat","pval"), defhead=head, idcode=newid.AFNI(),
  #     com_hist=lop$com_history, statsym=statsym, addFDR=2, type='MRI_float',
  #     scale=FALSE, overwrite=lop$overwrite)
  
  ## write out (fix for prefix here)
  write.AFNI("prospective_with_pval.nii.gz",stat.out,
             c("chisq","pval","1-pval"),statsym=stat.sym,
             defhead=head,idcode=newid.AFNI(),type='MRI_float',
             scale=FALSE,overwrite=lop$overwrite)
  
}



#################################################################################
######################## Begin RprogDemo main ##################################
#################################################################################



#Load that input dataset
for (iin in 1:length(lop$input)) {
  if (lop$verb) {
    note.AFNI(sprintf("Processing volume %s", lop$input[iin]));
  }
  
  idset <- read.AFNI(lop$input[iin], verb = max(0,lop$verb-1), meth=lop$iometh)
  
  #For convenience, change the dimensions so that the first 3 
  #dimensions are turned into 1
  ddi <- dset.dimBRKarray(idset)
  idset <- dset.3DBRKarrayto1D(idset);
  
  #Load mask and check dimensions
  mdset <- NULL
  if (!is.null(lop$mask)) {
    if (lop$verb) {
      note.AFNI(sprintf("Loading mask %s", lop$mask));
    }
    mdset <- read.AFNI(lop$mask, verb = max(lop$verb-1), meth=lop$iometh)
    mdset <-  dset.3DBRKarrayto1D(mdset);
    if (!dset.gridmatch(mdset, idset)) {
      errex.AFNI(sprintf("Mismatch between grids of mask %s and input %s",
                         lop$mask, lop$input[iin]));
    }
  }
  
  
  
  #Scale entire volume
  brk <- ChiSq.slow(idset, mdset, lop$scale);

}

# warnings() ; cat("\n")

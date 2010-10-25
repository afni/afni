#!/usr/bin/env AFNI_Batch_R

#Clean up
rm(list = ls())

#And load R's libsvm 
library("e1071")

libLoad <- function(myLib) {
   sucLoad <- FALSE
   sucCheck <- FALSE
   try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
   if (sucLoad) {
      print(sprintf("Package %s successfully loaded!", myLib)); sucCheck <- TRUE
   } else {
	  	try(install.packages(myLib))
      try(sucLoad <- library(myLib, character.only = TRUE, 
                              logical.return = TRUE))
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
source(first.in.path('AFNIplot.R'))



greeting.SigsClassify <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to SigsClassify.R ==================          #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      
reference.SigsClassify <- function ()
   return(
""
   )


#The help function for SigsClassify batch (command line mode)
help.SigsClassify.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
Usage:
------ 
 SigsClassify is a program for plotting a 1D file'
   
   ex1 <- 
"
Example 1 --- :
--------------------------------
      SigsClassify  -input IBSR_01.SubClasses.skew.1D.repsig    

      SigsClassify  -input IBSR_01.SubClasses.skew.1D.repsig \\
               -ColumnGroups 'R:c(rep(1,10), rep(2,10), rep(3,10))' \\
               -GroupLabels CSF GM WM \\
               -NoZeros -NoOffsetBase \\
               -filesuffix ZePlotIzSaved.pdf
"   

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
   cat(intro, ex1, ss, reference.SigsClassify(), sep='\n');
   
   if (adieu) exit.AFNI();
}

parse.SigsClassify.ColumnGroups <- function(op) {
   mm <- read.AFNI.matrix(op)
   return(mm)
}

init.SigsClassify.lop <- function () {
   lop <- list()
   lop$tset <- list()
   lop$Tset <- list()
   lop$volsuffix = NULL;
   lop$filesuffix = NULL;
   lop$thislabset= NULL;
   lop$labeltable = NULL;
   lop$verb=0;
   lop$noX11=FALSE;
   lop$svmscale=TRUE;
   lop$reuseTrain=FALSE;
   lop$loadTrain=NULL;
   return(lop)
}

parse.Set.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(sig=NULL,
                     signame=op[1], 
                     lab=NULL,
                     labname=op[2], 
                     labselecs=NULL)))
   return(iset)
}

#Change command line arguments into an options list
read.SigsClassify.opts.batch <- function (args=NULL, verb = 0) {
   
   params <- list (
      '-train' = apl(n = 2, d = NA, dup = TRUE,  h = paste(
   "-train sigsdset labeldset : labelvolume and signaturesvolume\n"
                     ) ),
      '-test' = apl(n = 2, d = NA,  dup = TRUE, h = paste(
   "-test sigsdset labeldset: labelvolume and signaturesvolume\n"
                     ) ),

      '-class_inds' = apl(n = c(1,Inf), d = NA,  h = paste(
   "-class_inds Ci1 Ci2 ...: \n"
                     ) ),

      '-parent_grouping' = apl(n = c(1,Inf), d = NA,  h = paste(
   "-parent_grouping  ...: \n"
                     ) ),

      '-parent_group_labeling' = apl(n = c(1,Inf), d = NA,  h = paste(
   "-parent_group_labeling  ...: \n"
                     ) ),
                     
      '-labeltable' = apl(n = 1, d = NA,  h = paste(
   "-labeltable labletable: \n"
                     ) ),
      '-volsuffix' = apl(n = 1, d = NA,  h = paste(
   "-volsuffix PREFIX: Output suffix\n"
                     ) ),
      '-filesuffix' = apl(n = 1, d = NA,  h = paste(
   "-filesuffix PREFIX: Output suffix\n"
                     ) ),
      '-loadTrain' = apl(n = 1, d = NA,  h = paste(
   "-loadTrain PREFIX: Output suffix\n"
                     ) ),
      '-noX11' = apl(n=0, d = 0, h = paste(
   "-noX11 : No X11 display possible\n"
                        ) ),
      '-reuseTrain' = apl(n=0, d = 0, h = paste(
   "-reuseTrain : Reuse training result, if it exists\n"
                        ) ),
      '-featscale' = apl(n=1, d = 1, h = paste(
   "-featscale : 0/[1] scaling of features\n"
                        ) ),
                  
      '-verb' = apl(n=1, d = 0, h = paste(
   "-verb VERB: VERB is an integer specifying verbosity level.\n",
   "            0 for quiet (Default). 1 or more: talkative.\n"
                        ) ),
      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )

         );
                     
   ops <- parse.AFNI.args(args,  params,
                          other_ok=FALSE, verb=verb
                          )
   if (verb) show.AFNI.args(ops, verb=verb-1, hstr='');
   if (is.null(ops)) {
      errex.AFNI('Error parsing arguments. See SigsClassify -help for details.');
   }
   
   #Parse dems options
   #initialize with defaults
   lop <- init.SigsClassify.lop()
   
   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             train = lop$tset <- parse.Set.opts(lop$tset, ops[[i]]),
             test = lop$Tset <- parse.Set.opts(lop$Tset, ops[[i]]),
             volsuffix = lop$volsuffix  <- ops[[i]],
             filesuffix = lop$filesuffix  <- ops[[i]],
             loadTrain = lop$loadTrain <- ops[[i]],
             class_inds = lop$thislabset <- read.AFNI.matrix(ops[[i]]),
             parent_grouping = lop$ColumnGroups <- read.AFNI.matrix(ops[[i]]),
             parent_group_labeling = lop$GroupLabels <- ops[[i]],
             labeltable = lop$labeltable <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             help = help.SigsClassify.opts(params, adieu=TRUE),
             show_allowed_options = show.AFNI.args(ops, verb=0, 
                                              hstr="SigsClassify's",adieu=TRUE),
             noX11 = lop$noX11 <- TRUE,
             reuseTrain = lop$reuseTrain <- TRUE,
             featscale = lop$svmscale <- as.logical(ops[[i]])
             )
   }
   if (lop$svmscale != FALSE && lop$svmscale != TRUE) {
      err.AFNI("Bad -featscale value");
      return(lop)
   }
   if (lop$noX11 == FALSE) {
      #Make sure that is possible or turn option off. 
      if (!length(kk <- capabilities(what='x11')) || kk[1] == FALSE) {
         lop$noX11 <- TRUE
      }
   }

   return(lop)
}# end of read.SigsClassify.opts.batch

#Change options list to SigsClassify variable list 
process.SigsClassify.opts <- function (lop, verb = 0) {
   return(lop)
}


Train.SigsClassify <- function (lvols, samples_frac=NULL, 
                           max_samples = Inf, min_samples = 0,
                           thislabset=NULL, 
                           PlotColumnGroups=NULL, PlotGroupLabels=NULL,
                           PlotTitle=NULL,
                           noX11=FALSE,
                           svmscale=TRUE,
                           verb = 1) {
   if (length(samples_frac) != 1 || samples_frac > 1 || samples_frac <= 0) {
      err.AFNI("Bad sample fraction")
      return(NULL);
   } 
   lsvm <- list(sigs = NULL, labs = NULL, Nlabsamp=NULL, model=NULL)
   for (lvol in lvols) {
      #Load label thingy
      if (is.null(lvol$lab)) {
         if (verb) note.AFNI(sprintf("Loading %s", lvol$labname),tic=1) 
         lvol$lab <- dset.3DBRKarrayto1D(read.AFNI(lvol$labname))
      } else {
         lvol$lab <- dset.3DBRKarrayto1D(lvol$lab)
      }
      if (is.null(lvol$labselecs)) {
         if (verb) note.AFNI(sprintf("Selecting set"),tic=1) 
         if (is.null(thislabset)) {
            if (is.null(lvol$labset)) 
               lvol$labset <- setdiff(unique(lvol$lab$brk), 0)
         } else {
            lvol$labset <- setdiff(thislabset, 0)
         }
         if (verb) note.AFNI(sprintf("Forming labselecs"),tic=1) 
         lvol$labselecs <- list()
         for (lv in lvol$labset) {
            ilab <- which(lvol$lab$brk == lv)
            nsmax <- length(ilab);
            if (!is.null(samples_frac)) {
               ns <- as.integer(samples_frac*nsmax)
               if (ns > max_samples) ns <- max_samples
               else if (ns < min_samples) ns <- min(min_samples, nsmax)
               ii <- sample(1:nsmax, ns)
               ilab <- ilab[ii]
            } 
            if (verb) 
               note.AFNI(
                  sprintf(
                     "  Got %d labels (%.2f%%) from max. of %d, for subclass %d",                           ns, ns/nsmax*100, nsmax, lv),tic=1) 
            lvol$labselecs <- 
               c (lvol$labselecs, 
                  list(  list(
                         labval=lv, ilab = ilab) ) )
            names(lvol$labselecs[length(lvol$labselecs)]) <-
                                       sprintf('VoxSelection.%d', lv)
         }
      }
      #Load signatures
      if (is.null(lvol$sig)) {
         if (verb) note.AFNI(sprintf("Loading %s", lvol$signame),tic=1) 
         lvol$sig <- dset.3DBRKarrayto1D(read.AFNI(lvol$signame))
      } else {
         lvol$sig <- dset.3DBRKarrayto1D(lvol$sig)
      }
      #Now build the set of features for each label
      if (verb) note.AFNI(sprintf("Building sigs and labs"),tic=1) 
      for (lbsel in lvol$labselecs) {
         if (length(lbsel$ilab)) {
            lsvm$sigs <- rbind(lsvm$sigs,lvol$sig$brk[lbsel$ilab,])
            lsvm$labs <- c(lsvm$labs,rep(lbsel$labval,length(lbsel$ilab)))      
            iul <- which(names(lsvm$Nlabsamp) == as.character(lbsel$labval))
            if (length(iul)) {
               lsvm$Nlabsamp[iul] <- lsvm$Nlabsamp[iul]+length(lbsel$ilab)
            } else {
               lsvm$Nlabsamp <- c(lsvm$Nlabsamp, length(lbsel$ilab))
               names(lsvm$Nlabsamp)[length(lsvm$Nlabsamp)] <- 
                  as.character(lbsel$labval)
            }
         }
      }  
   }
   
   if (verb) note.AFNI(sprintf(
      "SVM model building %d samples of %d classes, and %d features",
      dim(lsvm$sigs)[1], length(lsvm$Nlabsamp), dim(lsvm$sigs)[2] ),
      paste('      Label:',names(lsvm$Nlabsamp), 
            ', Nsamples',lsvm$Nlabsamp, '\n',
            collapse=''), 
      tic=1)
   
   #Before testing, a safety check 
   mm <- matrix(0, dim(lsvm$sigs)[2], length(lvol$labset))
   for (l in 1:length(lvol$labset)) {
      for (i in  1:dim(lsvm$sigs)[2]) {
         ifnd <- which (lsvm$labs==lvol$labset[l])
         if (length(ifnd)) {
            mm[i,l] <-median(lsvm$sigs[ifnd,i])
         }
      }
   }
   plot.1D(mm, OneSubplot=TRUE, OffsetBase=FALSE, 
               ColumnGroups=PlotColumnGroups, GroupLabels=PlotGroupLabels, 
               prefix =sprintf('%s.jpg',PlotTitle), Title=PlotTitle,
               CloseAfterSave = noX11)
               
   if (verb>1) browser() 
   if (0) {
      lsvm$model <- svm(lsvm$sigs, factor(lsvm$labs), 
                     scale = svmscale, probability=TRUE,
                     type="C-classification")
   } else {
      if (verb) note.AFNI("With fine tuning...")
      lsvm$modeltuned <- tune(svm, 
                     train.x=lsvm$sigs, train.y=factor(lsvm$labs), 
                     ranges = list( gamma = c(2^(-6:3), 1/dim(lsvm$sigs)[2]), 
                                    cost = 2^(-4:4)),
                     scale = svmscale, probability=TRUE,
                     type="C-classification" )
      lsvm$model <- lsvm$modeltuned$best.model
   }  
   
   if (verb) note.AFNI(sprintf("SVM modeling completed"), tic=1) 

   return(lsvm)
}

Test.SigsClassify <- function (lvols, lsvm=NULL, verb = 1, 
                               ltfile=NULL, volsuffix=NULL,overwrite=FALSE) {
   if (is.null(lsvm) || is.null(lsvm$model))  {
      err.AFNI("Bad input")
      return(NULL);
   } 
   lsvm <- c(lsvm, list(classy=NULL))
   for (i in 1:length(lvols)) {
      #prep names
      if (is.null(volsuffix)) {
         an <- parse.AFNI.name(lvols[i][[1]]$signame)
         volout   <- sprintf('%s.cls%s'   , an$prefix, an$view)
         voloutp  <- sprintf('%s.clsp%s'  , an$prefix, an$view)
         voloutdv <- sprintf('%s.clsdv%s' , an$prefix, an$view)
      } else {
         an <- parse.AFNI.name(lvols[i][[1]]$signame)
         volout   <- sprintf('%s.%s.cls%s'   , an$prefix, volsuffix, an$view)
         voloutp  <- sprintf('%s.%s.clsp%s'  , an$prefix, volsuffix, an$view)
         voloutdv <- sprintf('%s.%s.clsdv%s' , an$prefix, volsuffix, an$view)
      }
      str(volout)
      str(exists.AFNI.name(volout))
      if (!overwrite && exists.AFNI.name(volout)) {
         note.AFNI(sprintf("Volume %s exists. Skipping...", volout));
      } else {
         note.AFNI(sprintf("About to load signatures %s", 
                           lvols[i][[1]]$signame));
         #Load signatures
         if (is.null(lvols[i][[1]]$sig)) {
            if (verb) note.AFNI(sprintf("Loading %s", 
                              lvols[i][[1]]$signame),tic=1) 
            lvols[i][[1]]$sig <- 
                           dset.3DBRKarrayto1D(read.AFNI(lvols[i][[1]]$signame))
         } else {
            lvols[i][[1]]$sig <- dset.3DBRKarrayto1D(lvols[i][[1]]$sig)
         }
         if (is.null(lvols[i][[1]]$sig)) {
            err.AFNI("NULL sig");
            if (verb>1) browser()
            else return(NULL);
         }  
         #Now do the testing
         note.AFNI(sprintf("About to test on %s", lvols[i][[1]]$signame));
         if (verb>1) browser()
         ii <- dset.nonzero.indices(lvols[i][[1]]$sig)
         lvols[i][[1]] <- c(lvols[i][[1]],
                    list(classy=predict(lsvm$model, lvols[i][[1]]$sig$brk[ii,],
                                           probability=TRUE, 
                                           decision.values = TRUE),
                            ivox=ii) )

         lvols[i][[1]] <- c(lvols[i][[1]],
                       list(classout=volout)) 

         #Make a new volume data
            dd <- c(lvols[i][[1]]$sig$dim[1:3],1)
            brk <- array(0,dd)
            #Change brk to 1D
            dim(brk) <- c(prod(dd[1:3]),dd[4])
            brk[lvols[i][[1]]$ivox,1] <- 
                  as.integer(levels(lvols[i][[1]]$classy))[lvols[i][[1]]$classy]
               #Fastest way to get factor levels back as integer labels
              #http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg08378.html
            #brk back to 3D
            dim(brk) <- dd

            #write out dset 
            write.AFNI( volout, brk, 
                        label=c('test_labels'), defhead=lvols[i][[1]]$sig$header,
                        verb = verb, scale=FALSE)

            if (!is.null(ltfile)) { 
               scom <- sprintf('3drefit -labeltable %s %s', ltfile, volout);
               system(scom)
            }

            dv <- as.matrix(attr(lvols[i][[1]]$classy, 
                                             "decision.values"))
            dd <- c(lvols[i][[1]]$sig$dim[1:3],dim(dv)[2])
            brk <- array(0,dd)
            dim(brk) <- c(prod(dd[1:3]),dd[4])
            brk[lvols[i][[1]]$ivox,] <- dv
            dv<-NULL
            dim(brk) <- dd
            write.AFNI(voloutdv, brk, 
                        label=paste('tdecis_val', seq(1,dd[4]), sep='.'),
                        defhead=lvols[i][[1]]$sig$header,
                        verb = verb, scale=TRUE)   

            dv <- as.matrix(attr(lvols[i][[1]]$classy, 
                                             "probabilities"))
            dd <- c(lvols[i][[1]]$sig$dim[1:3],dim(dv)[2])
            brk <- array(0,dd)
            dim(brk) <- c(prod(dd[1:3]),dd[4])
            brk[lvols[i][[1]]$ivox,] <- dv
            dv<-NULL
            dim(brk) <- dd
            write.AFNI(voloutp, brk, label=paste('tprob', seq(1,dd[4]), sep='.'),
                        defhead=lvols[i][[1]]$sig$header,
                        verb = verb, scale=TRUE)

         #Need space so delete a little
         llvv <- lvols[i]
         save(llvv, file=sprintf('%s.lvols%d.Rdat',volout,i))
         llvv <- NULL

         #delete the big stuff. everything's been pretty much saved
         #This can grow quite large with probabilities
         if (verb) memory.hogs(msg='Pre NULLing')
         lvols[i][[1]]$sig$brk <- NULL;
         lvols[i][[1]]$classy <- NULL; gc(); 
         if (verb) memory.hogs(msg='Post NULLing')
      }
   }
   return(lvols)   
}


#################################################################################
########################## Begin SigsClassify main ###################################
#################################################################################


   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      save(args, file=".SigsClassify.dbg.AFNI.args", ascii = TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      lop <- init.SigsClassify.lop()
      lop$verb=1
      lop$labeltable = '~/LocalSeg/IBSR_V2.0/DepthSubClasses.niml.lt' 
      lop$tset <- list()
      if (1) {
         sname <- '~/LocalSeg/IBSR_V2.0/SegResults/s01/IBSR_01_ana.median+orig'
         lname <- '~/LocalSeg/IBSR_V2.0/SegResults/s01/IBSR_01_segTRI.DepthSubClasses+orig'
         s2name <- '~/LocalSeg/IBSR_V2.0/SegResults/s02/IBSR_02_ana.median+orig'
         l2name <- '~/LocalSeg/IBSR_V2.0/SegResults/s02/IBSR_02_segTRI.DepthSubClasses+orig'
         labs=c(1,4,5, 11, 14, 15, 21, 24, 25)
         ColumnGroups = c(1,1,1,2,2,2,3,3,3)
         GroupLabels=c('CSF','GM','WM')  
      } else {
         sname <- '~/SUMA_test_dirs/3dsvm/trainvol.ftr+orig'
         lname <- '~/SUMA_test_dirs/3dsvm/trainvol.lbl+orig'
         s2name <- '~/SUMA_test_dirs/3dsvm/testvol.ftr+orig'
         l2name <- '~/SUMA_test_dirs/3dsvm/testvol.lbl+orig'
         labs=c(1,2,3)    
         ColumnGroups = c(1,2,3)
         GroupLabels=c('CSF','GM','WM')  
      }
      lop$tset <- c(lop$tset,
                    list( 
                     list(sig=NULL,
                     signame=sname, 
                     lab=NULL,
                   labname=lname, 
                     labselecs=NULL)))
      lop$tset <- c(lop$tset,
                    list( 
                     list(sig=NULL, 
                     signame = s2name, 
                     lab=NULL,
                   labname=l2name, 
                     labselecs=NULL)))
      
      train <- Train.SigsClassify(lop$tset, samples_frac=0.06, 
                              max_samples = 1000, min_samples = 100, 
                              thislabset=labs, PlotColumnGroups=ColumnGroups,
                              PlotGroupLabels=GroupLabels,PlotTitle='toy',
                              verb=lop$verb, noX11=lop$noX11)
      test <- Test.SigsClassify(lop$tset, lsvm=train, 
                           verb=lop$verb, ltfile = lop$labeltable)
   } else {
      if (!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }  
      if (is.null(lop <- read.SigsClassify.opts.batch(args, verb = 0))) {
         stop('Error parsing input');
      }
      #str(lop);
      if (is.null(lop <- process.SigsClassify.opts(lop, verb = lop$verb))) {
         stop('Error processing input');
      }
   }
   if (lop$verb) { 
      str(lop);
   }
   
   
      if (!is.null(lop$loadTrain)) {
         if (file.exists(lop$loadTrain)) {
            note.AFNI(sprintf("Loading training set from pre-existing %s", 
                       lop$loadTrain ))
            if (exists('train')) { 
               rm('train')            
            }
            load(lop$loadTrain);
            if (!exists('train') || is.null(train)) {
               warn.AFNI("Variable 'train' not found or null!")
               train <- NULL
            }
         }
      } else {
         trainname <- sprintf("train%s.Rdat",lop$filesuffix);
         train <- NULL
         if (lop$reuseTrain) {
            if (file.exists(trainname)) {
               note.AFNI(sprintf("Loading training set from pre-existing %s", 
                           trainname))
               rm('train')            
               load(trainname);
               if (!exists('train') || is.null(train)) {
                  warn.AFNI("Variable 'train' not found or null!")
                  train <- NULL
               }
            }
         }
      }
      if (is.null(train)) {
         note.AFNI("Creating new training object");
         train <- Train.SigsClassify(lop$tset, samples_frac=0.03, 
                              max_samples = 100, min_samples = 100, 
                              thislabset=lop$thislabset,
                              PlotColumnGroups=lop$ColumnGroups,
                              PlotGroupLabels=lop$GroupLabels,
                              PlotTitle=sprintf("trainset%s", lop$filesuffix),
                              verb=lop$verb, noX11=lop$noX11,
                              svmscale=lop$svmcale)
         save(train, file=sprintf("train%s.Rdat",lop$filesuffix), ascii = TRUE) 
      } else {
         note.AFNI("Reusing/Or Reloaded train object");
      }
      
      test <- Test.SigsClassify(lop$Tset, lsvm=train, 
                           verb=lop$verb, ltfile = lop$labeltable,
                           volsuffix = lop$volsuffix)
                           
      save(test, file=sprintf("test%s.Rdat",lop$filesuffix), ascii = TRUE) 

      

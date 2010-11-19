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
source(first.in.path('Signatures.R'))
source(first.in.path('AFNIplot.R'))


ExecName <- '3dSignatures'

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
 3dSignatures is a program creating voxel membership priors based on
              a collection of features termed "signatures".'

   ex1 <- 
"
Example 1 --- :
--------------------------------
      SigsClassify  -input IBSR_01.SubClasses.skew.1D.repsig    

      SigsClassify  -input IBSR_01.SubClasses.skew.1D.repsig \\
               -ColumnGroups 'R:c(rep(1,10), rep(2,10), rep(3,10))' \\
               -GroupLabels CSF GM WM \\
               -NoZeros -Nocol.ystack \\
               -trainsuffix ZePlotIzSaved.pdf
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
   lop$odir = NULL;
   lop$opref = 'test';
   lop$volsuffix = NULL;
   lop$trainsuffix = 'train';
   lop$testsuffix = 'test';
   lop$thiskeyset= NULL;
   lop$thislabset= NULL;
   lop$labeltablefile = NULL;
   lop$groupedlabeltablefile = NULL;
   lop$verb=0;
   lop$no_X11=FALSE;
   lop$doprob = TRUE;
   lop$no_tune=FALSE;
   lop$svmscale=TRUE;
   lop$reuse_train=FALSE;
   lop$load_train=NULL;
   lop$samples_frac <- 0.03
   lop$max_samples <- 100
   lop$min_samples <- 100
   return(lop)
}

parse.TeSetSF.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(subj=op[1],
                     anat=NULL,
                     anatname=NULL,
                     sig=NULL,
                     signame=op[2], 
                     lab=NULL,
                     labname=NULL, 
                     labselecs=NULL)))
   return(iset)
}
parse.TeSetSFL.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(subj=op[1],
                     anat=NULL,
                     anatname=NULL,
                     sig=NULL,
                     signame=op[2], 
                     lab=NULL,
                     labname=op[3], 
                     labselecs=NULL)))
   return(iset)
}

parse.TeSetSAFL.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(subj=op[1],
                     anat=NULL,
                     anatname=op[2],
                     sig=NULL,
                     signame=op[3], 
                     lab=NULL,
                     labname=op[4], 
                     labselecs=NULL)))
   return(iset)
}

parse.TrSetSFL.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(subj=op[1],
                     sig=NULL,
                     signame=op[2], 
                     lab=NULL,
                     labname=op[3], 
                     labselecs=NULL)))
   return(iset)
}

parse.CSet.opts <- function (iset, op) {
   iset <- c(iset,
                    list( 
                     list(subj=op[1],
                     sig=NULL,
                     signame=op[2], 
                     lab=NULL,
                     labname=NULL, 
                     labselecs=NULL)))
   return(iset)
}

#Change command line arguments into an options list
read.SigsClassify.opts.batch <- function (args=NULL, verb = 0) {
   
   params <- list (
      '-train' = apl(n = 3, d = NA, dup = TRUE,  h = paste(
   "See -train_SFL"
                     ) ),
      '-train_SFL' = apl(n = 3, d = NA, dup = TRUE,  h = paste(
   "-train_SFL SUBJ FEATURES LABELS: Specify a training set's \n",
   "                         subject ID, volume of signatures, and\n",
   "                         volume of labels"
                     ) ),
      '-train_suffix' = apl(n = 1, d = NA,  h = paste(
   "-train_suffix TRAINSUFFIX: Suffix of file containing training results\n"
                     ) ),
      '-no_tune' = apl(n=0, d = 0, h = paste(
   "-no_tune : No tuning of training model\n"
                        ) ),
      '-test' = apl(n = 2, d = NA,  dup = TRUE, h = paste(
   "See -test_SF\n"
                     ) ),
      '-test_SF' = apl(n = 2, d = NA,  dup = TRUE, h = paste(
   "-test_S SUBJ FEATURES : Specify a test set's \n",
   "                         subject ID, volume of signatures\n"
                     ) ),
      '-test_SFL' = apl(n = 3, d = NA,  dup = TRUE, h = paste(
   "-test_SL SUBJ FEATURES LABELS: Specify a test set's \n",
   "                         subject ID, volume of signatures, and\n",
   "                         volume of labels"
                     ) ),
      '-test_SAFL' = apl(n = 4, d = NA,  dup = TRUE, h = paste(
   "-test_ASL SUBJ ANAT FEATURES LABELS: Specify a test set's \n",
   "                         subject ID, Anatomical, \n",
   "                         volume of signatures, and volume of labels."
                     ) ),
      '-class' = apl(n = 2, d = NA,  dup = TRUE, h = paste(
   "-test SUBJ SIGS LABELS: Specify a classification set's \n",
   "                         subject ID, and volume of signatures"
                     ) ),
      '-odir' = apl(n = 1, d = NA, dup = FALSE, h = paste(
   "-odir ODIR: Specify a directory for output of classification results"
                     ) ),
      '-class_labels' = apl(n = c(1,Inf), d = NA,  h = paste(
   "-class_labels CS1 CS2 ...: \n"
                     ) ),
      '-class_keys' = apl(n = c(1,Inf), d = NA,  h = paste(
   "-class_keys Ci1 Ci2 ...: \n"
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
      '-grouped_labeltable' = apl(n = 1, d = NA,  h = paste(
   "-grouped_labeltable grouped_labletable: \n"
                     ) ),
      '-vol_suffix' = apl(n = 1, d = NA,  h = paste(
   "-vol_suffix PREFIX: Output suffix\n"
                     ) ),
      '-load_train' = apl(n = 1, d = NA,  h = paste(
   "-load_train PREFIX: Output suffix\n"
                     ) ),
      '-no_X11' = apl(n=0, d = 0, h = paste(
   "-no_X11 : No X11 display possible\n"
                        ) ),
      '-no_prob' = apl(n=0, d = 0, h = paste(
   "-no_prob : No probability output\n"
                        ) ),
      '-reuse_train' = apl(n=0, d = 0, h = paste(
   "-reuse_train : Reuse training result, if it exists\n"
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
      '-msg.trace' = apl(n=0, h=
   "-msg.trace: Output trace information along with errors and notices\n" )

         );
                     
   ops <- parse.AFNI.args(args,  params,
                          other_ok=FALSE, verb=verb
                          )
   if (verb) show.AFNI.args(ops, verb=verb-1, hstr='');
   if (is.null(ops)) {
      errex.AFNI(paste(
         'Error parsing arguments. See ',ExecName, 
         'SigsClassify -help for details.', sep=''));
   }
   
   #Parse dems options
   #initialize with defaults
   lop <- init.SigsClassify.lop()
   
   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             train = lop$tset <- parse.TrSetSFL.opts(lop$tset, ops[[i]]),
             train_SFL = lop$tset <- parse.TrSetSFL.opts(lop$tset, ops[[i]]),
             test = lop$Tset <- parse.TeSetS.opts(lop$Tset, ops[[i]]),
             test_SF = lop$Tset <- parse.TeSetS.opts(lop$Tset, ops[[i]]),
             test_SFL = lop$Tset <- parse.TeSetSFL.opts(lop$Tset, ops[[i]]),
             test_SAFL = lop$Tset <- parse.TeSetSAFL.opts(lop$Tset, ops[[i]]),
             class = lop$Tset <- parse.CSet.opts(lop$Tset, ops[[i]]),
             odir = lop$odir <- ops[[i]],
             opref = lop$opref <- ops[[i]],
             vol_suffix = lop$volsuffix  <- ops[[i]],
             train_suffix = lop$trainsuffix  <- ops[[i]],
             test_suffix = lop$testsuffix  <- ops[[i]],
             load_train = lop$load_train <- ops[[i]],
             class_labels = lop$thislabset <- ops[[i]],
             class_keys = lop$thiskeyset <- read.AFNI.matrix(ops[[i]]),
             parent_grouping = lop$ColumnGroups <- read.AFNI.matrix(ops[[i]]),
             parent_group_labeling = lop$GroupLabels <- ops[[i]],
             labeltable = lop$labeltablefile <- ops[[i]],
             grouped_labeltable = lop$groupedlabeltablefile <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             help = help.SigsClassify.opts(params, adieu=TRUE),
             show_allowed_options = show.AFNI.args(ops, verb=0, 
                                              hstr="SigsClassify's",adieu=TRUE),
             msg.trace = set.AFNI.msg.trace(TRUE),
             no_prob = lop$doprob <- FALSE,
             no_X11 = lop$no_X11 <- TRUE,
             no_tune = lop$no_tune <- TRUE,
             reuse_train = lop$reuse_train <- TRUE,
             featscale = lop$svmscale <- as.logical(ops[[i]])
             )
   }
   if (lop$svmscale != FALSE && lop$svmscale != TRUE) {
      errex.AFNI("Bad -featscale value");
   }
   if (lop$no_X11 == FALSE) {
      #Make sure that is possible or turn option off. 
      if (!length(kk <- capabilities(what='x11')) || kk[1] == FALSE) {
         lop$no_X11 <- TRUE
      }
   }
   
   if (!is.null(lop$odir) && !file_test('-d', lop$odir)) {
      errex.AFNI(sprintf('Output directory "%s" does not exist', lop$odir));
   }
   
   if (!is.null(lop$labeltablefile) && !file_test('-f', lop$labeltablefile)) {
      errex.AFNI(sprintf('labeltable file "%s" does not exist', 
                           lop$labeltablefile));
   }
   
   if (!is.null(lop$thislabset)) {
      if (!is.null(lop$thiskeyset)) {
         errex.AFNI("You have used both -class_keys and -class_labels")
      }
      lop$thiskeyset <- labelkey.labeltable(lop$thislabset, lop$labeltablefile) 
      if (is.null(lop$thiskeyset)) {
         errex.AFNI("Failed to create thiskeyset from labels");
      }
   }
   
   if (  !is.null(lop$groupedlabeltablefile) && 
         !file_test('-f', lop$groupedlabeltablefile)) {
      errex.AFNI(sprintf('labeltable file "%s" does not exist', 
                           lop$groupedlabeltablefile));
   }
   
   if (!is.null(lop$Tset) && length(lop$Tset) > 1) {
      sv <- lop$Tset[[1]]$subj;
      for (kk in 2:length(lop$Tset)) {
         sv <- c(sv, lop$Tset[[kk]]$subj)
      }
      sv <- unique(sv)
      if (length(sv) != length(lop$Tset)) {
         
         errex.AFNI(paste("Have", length(sv), "unique test subject labels (", 
                          paste(sv, collapse=','),
                       " ) but", length(lop$Tset)," '-test/-class' options"  ) )
         
      }
   }
   if (!is.null(lop$tset) && length(lop$tset) > 1) {
      sv <- lop$tset[[1]]$subj;
      for (kk in 2:length(lop$tset)) {
         sv <- c(sv, lop$tset[[kk]]$subj)
      }
      sv <- unique(sv)
      if (length(sv) != length(lop$tset)) {
         
         errex.AFNI(paste("Have", length(sv), "unique test subject labels (", 
                          paste(sv, collapse=','),
                          " ) but", length(lop$tset)," '-train' options"  ) )
         
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
                           thiskeyset=NULL, ltfile=NULL, 
                           PlotColumnGroups=NULL, PlotGroupLabels=NULL,
                           PlotTitle=NULL,
                           no_X11=FALSE,
                           svmscale=TRUE,
                           verb = 1, no_tune = FALSE, doprob=TRUE) {
   if (length(samples_frac) != 1 || samples_frac > 1 || samples_frac <= 0) {
      err.AFNI("Bad sample fraction")
      return(NULL);
   }
   if (is.null(ltfile)) {
      err.AFNI("ltfile is now mandatory")
      return(NULL)
   } 
   lsvm <- list(sigs = NULL, labs = NULL, Nlabsamp=NULL, 
                  model=NULL, labeltable=NULL)
   for (lvol in lvols) {
      #Load label thingy
      if (is.null(lvol$lab)) {
         if (verb) 
            note.AFNI(sprintf("Loading labels volume %s", lvol$labname),tic=1) 
         lvol$lab <- dset.3DBRKarrayto1D(read.AFNI(lvol$labname))
      } else {
         if (verb) note.AFNI(sprintf("Labels volume already loaded"))
         lvol$lab <- dset.3DBRKarrayto1D(lvol$lab)
      }
      if (is.null(lvol$lab)) {
         err.AFNI('Failed to get labels');
         return(NULL)
      }
      if (is.null(lvol$labselecs)) {
         if (verb > 1) note.AFNI(sprintf("Selecting set"),tic=1) 
         if (is.null(thiskeyset)) {
            if (is.null(lvol$labset)) 
               lvol$labset <- setdiff(unique(lvol$lab$brk), 0)
         } else {
            lvol$labset <- setdiff(thiskeyset, 0)
         }
         #name labset by the labels from the labeltable
         lsvm$labeltable <- build.labeltable(labeltable=ltfile, 
                                              keys=lvol$labset)
         if (is.null(lsvm$labeltable)) {
            err.AFNI("Failed to build label table list");
            return(NULL)
         }
      
         if (verb > 1) note.AFNI(sprintf("Forming labselecs"),tic=1) 
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
            if (verb > 1) 
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
      } else {
         errex.AFNI("Not expecting this here. Check logic before allowing");
      }
      #Load signatures
      if (is.null(lvol$sig)) {
         if (verb > 1) note.AFNI(sprintf("Loading %s", lvol$signame),tic=1) 
         lvol$sig <- dset.3DBRKarrayto1D(read.AFNI(lvol$signame))
      } else {
         lvol$sig <- dset.3DBRKarrayto1D(lvol$sig)
      }
      if (is.null(lvol$sig)) {
         err.AFNI('Failed to get signatures');
         return(NULL)
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
   plot.1D(mm, oneplot=TRUE, col.ystack=FALSE, 
               col.grp=PlotColumnGroups, grp.label=PlotGroupLabels, 
               prefix =sprintf('%s.jpg',PlotTitle), Title=PlotTitle,
               CloseAfterSave = no_X11)
               
   #if (verb>1) browser() 
   if (no_tune) {
      if (verb) note.AFNI(sprintf("NO fine tuning. Probability=%d", doprob))
      lsvm$model <- svm(lsvm$sigs, factor(lsvm$labs), 
                     scale = svmscale, probability=doprob,
                     type="C-classification")
   } else {
      if (verb) note.AFNI(sprintf("With fine tuning. Probability=%d", doprob))
      lsvm$modeltuned <- tune(svm, 
                     train.x=lsvm$sigs, train.y=factor(lsvm$labs), 
                     ranges = list( gamma = c(2^(-6:3), 1/dim(lsvm$sigs)[2]), 
                                    cost = 2^(-4:4)),
                     scale = svmscale, probability=doprob,
                     type="C-classification" )
      lsvm$model <- lsvm$modeltuned$best.model
   }  
   
   if (verb) note.AFNI(sprintf("SVM modeling completed"), tic=1) 

   return(lsvm)
}

Test.SigsClassify <- function (lvols, lsvm=NULL, verb = 1, 
                               ltfile=NULL, volsuffix=NULL,overwrite=FALSE,
                               opath = NULL, opref = NULL,
                               ltgroupedfile = NULL, doprob=TRUE) {
   if (is.null(lsvm) || is.null(lsvm$model))  {
      err.AFNI("Bad input")
      return(NULL);
   } 
   if (is.null(volsuffix)) volsuffix <- ''
   else {
      #make sure it has a .
      if (length(grep ('^\\.', volsuffix)) == 0) {
         volsuffix <- sprintf('.%s', volsuffix);
      }
   }
   if (!is.null(opath)) {
      if (!file_test('-d', opath)) {
         err.AFNI(sprintf('Output path %s does not exist', opath));
         return(NULL);
      }
   }
   lsvm <- c(lsvm, list(classy=NULL))
   for (i in 1:length(lvols)) {
      #prep names
      an <- parse.AFNI.name(lvols[i][[1]]$signame)
      if (is.null(opath)) ppo <- an$path
      else ppo <- opath
      if (is.null(opref)) {
         ppp <- sprintf('%s.%s%s', lvols[i][[1]]$subj, an$prefix, volsuffix)
      } else {
         ppp <- sprintf('%s.%s%s', lvols[i][[1]]$subj, opref, volsuffix)
      }
      volout   <- sprintf('%s/%s.cls%s'  , ppo, ppp, an$view)
      voloutp  <- sprintf('%s/%s.clsp%s' , ppo, ppp, an$view)
      voloutdv <- sprintf('%s/%s.clsdv%s', ppo, ppp, an$view)
      diceout  <- sprintf('%s/%s.cls.dice', ppo, ppp)
      voloutgrp   <- sprintf('%s/%s.gcls%s'  , ppo, ppp, an$view)
      voloutpgrp  <- sprintf('%s/%s.gclsp%s' , ppo, ppp, an$view)
      voloutdvgrp <- sprintf('%s/%s.gclsdv%s', ppo, ppp, an$view)

      diceout     <- sprintf('%s/%s.DiceCoef', ppo, ppp)
      diceoutgrp  <- sprintf('%s/%s.gDiceCoef', ppo, ppp)
      
      goldout     <- sprintf('%s/%s.%s.cls%s', 
                           ppo, lvols[i][[1]]$subj, 'gold',an$view)
      goldoutgrp  <- sprintf('%s/%s.%s.gcls%s', 
                           ppo, lvols[i][[1]]$subj, 'gold',an$view)
      
      anatout     <- sprintf('%s/%s.%s%s', 
                           ppo, lvols[i][[1]]$subj, 'anat',an$view)                     
      if (!overwrite && exists.AFNI.name(volout)) {
         note.AFNI(sprintf("Volume %s exists. Skipping...", volout));
      } else {
         if (verb > 1) note.AFNI(sprintf("About to load signatures %s", 
                           lvols[i][[1]]$signame));
         #Load signatures
         if (is.null(lvols[i][[1]]$sig)) {
            if (verb > 1) note.AFNI(sprintf("Loading %s", 
                              lvols[i][[1]]$signame),tic=1) 
            lvols[i][[1]]$sig <- 
                           dset.3DBRKarrayto1D(read.AFNI(lvols[i][[1]]$signame))
         } else {
            if (verb > 1) note.AFNI(sprintf("signatures already loaded")) 
            lvols[i][[1]]$sig <- dset.3DBRKarrayto1D(lvols[i][[1]]$sig)
         }
         if (is.null(lvols[i][[1]]$sig)) {
            err.AFNI("NULL sig");
            #if (verb>1) browser()
            else return(NULL);
         }  
         #Now do the testing
         note.AFNI(sprintf("About to test on %s, probability = %d",
                        lvols[i][[1]]$signame, doprob));
         #if (verb>1) browser()
         ii <- dset.nonzero.indices(lvols[i][[1]]$sig)
         lvols[i][[1]] <- c(lvols[i][[1]],
                    list(classy=predict(lsvm$model, lvols[i][[1]]$sig$brk[ii,],
                                           probability=doprob, 
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
                        verb = max(0,verb-1), scale=FALSE)

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
                        verb = max(0,verb-1), scale=TRUE)   

            if (doprob) {
               dv <- as.matrix(attr(lvols[i][[1]]$classy, 
                                                "probabilities"))
               dd <- c(lvols[i][[1]]$sig$dim[1:3],dim(dv)[2])
               brk <- array(0,dd)
               dim(brk) <- c(prod(dd[1:3]),dd[4])
               brk[lvols[i][[1]]$ivox,] <- dv
               dv<-NULL
               dim(brk) <- dd
               if (dd[4] != length(lsvm$labeltable$labels)) {
                  err.AFNI("paste have", dd[4], "subbricks, but ", 
                           length(lsvm$labeltable$labels), "training labels\n",
                           "Something is fishy here");
                  return(NULL);
               }
               write.AFNI(voloutp, brk, 
                           label=paste('p', lsvm$labeltable$labels, sep='.'),
                           defhead=lvols[i][[1]]$sig$header,
                           verb = max(0,verb-1), scale=TRUE)
            }
            
         #Need space so delete a little
         llvv <- lvols[i]
         if (verb > 1) {
            fout <- sprintf('%s.lvols%d.Rdat',volout,i)
            note.AFNI(sprintf("Saving svm result in %s", fout));
            save(llvv, file=fout)
         }
         llvv <- NULL

         #delete the big stuff. everything's been pretty much saved
         #This can grow quite large with probabilities
         if (verb) memory.hogs(msg='Pre NULLing')
         lvols[i][[1]]$sig$brk <- NULL;
         lvols[i][[1]]$classy <- NULL; gc(); 
         if (verb) memory.hogs(msg='Post NULLing')
         
      }
      
      #Put a copy of the anat in place
      if (!is.null(lvols[i][[1]]$anatname)) {
         if (!copy.AFNI.dset(lvols[i][[1]]$anatname,anatout, overwrite = TRUE)) {
            err.AFNI(sprintf(
                  "Failed to create copy of anatomical volume %s", anatout))
            return(NULL);
         }
      }
      
      #Create a merged version if necessary
      if (!is.null(ltgroupedfile) && 
            (  overwrite || 
               !exists.AFNI.name(voloutgrp) || !exists.AFNI.name(voloutpgrp)) ) {
         if (exists.AFNI.name(voloutgrp)) 
            system(sprintf('\\rm -f %s.HEAD %s.BRIK*', voloutgrp, voloutgrp))
         if (doprob && exists.AFNI.name(voloutpgrp)) 
            system(sprintf('\\rm -f %s.HEAD %s.BRIK*', voloutpgrp, voloutpgrp))

         if (doprob) {
            if (group.vollabels(  labelvol = volout, pvol=voloutp,
                              grptable=ltgroupedfile,
                              grplabelpref=voloutgrp, 
                              grpppref=voloutpgrp) == 0) {
               err.AFNI("Failed to group labels");
               return(NULL);                 
            }
         } else {
            if (group.vollabels(  labelvol = volout, pvol=NULL,
                              grptable=ltgroupedfile,
                              grplabelpref=voloutgrp, 
                              grpppref=NULL) == 0) {
               err.AFNI("Failed to group labels");
               return(NULL);                 
            }
         }
                           
      }
      #Do some dice action
      if (!is.null(lvols[i][[1]]$labname)) {
         if (!copy.AFNI.dset(lvols[i][[1]]$labname,goldout, overwrite = TRUE)) {
            err.AFNI(sprintf(
                  "Failed to create copy of reference label volume %s", goldout))
            return(NULL);
         }
         lvols[i][[1]]$dice <- dice.vollabels(basevol=goldout,
                  labeltable=ltfile,
                  prefix=diceout, labelvol=volout, maskby ='LABEL') 
         if (!is.null(ltgroupedfile)) {
            #first group the base
            if (group.vollabels(  labelvol = goldout, pvol=NULL,
                           grptable=ltgroupedfile,
                           grplabelpref=goldoutgrp, 
                           grpppref=NULL) == 0) {
               err.AFNI("Failed to group base labels");
               return(NULL);                 
            }
            #Now do the dice
            lvols[i][[1]]$gdice <- dice.vollabels(
                  goldoutgrp,
                  labeltable=ltgroupedfile,
                  prefix=diceoutgrp, labelvol=voloutgrp,
                  maskby ='LABEL')
         } 
      }
      
   }
   
   return(lvols)   
}


#################################################################################
########################## Begin SigsClassify main ###################################
#################################################################################


   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))
      rfile <- first.in.path(sprintf('%s.R',ExecName))  
      save(args, rfile, file=".SigsClassify.dbg.AFNI.args", ascii = TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      lop <- init.SigsClassify.lop()
      lop$verb=1
      note.AFNI("I am not going to have a hard coded list of options here.
   For testing, run the command line version once. Then launch R in that
   directory and do the following:
   source('~/AFNI/src/R_scripts/AFNIio.R');
   load.debug.AFNI.args()
   source('~/AFNI/src/R_scripts/3dSignatures.R');");
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
   
   
   if (!is.null(lop$load_train)) {
      if (!file.exists(lop$load_train)) {
         
         if (file.exists(kk <- sprintf('%s.Rdat',lop$load_train))) {
            lop$load_train <- kk
         } else {
            errex.AFNI(c(lop$load_train, " does not exist."));
         }   
      }
          
      note.AFNI(sprintf("Loading training set from pre-existing %s", 
                 lop$load_train ))
      if (exists('train')) { 
         rm('train')            
      }
      #Cannot override existing lop !, some .Rdat files
      #contain lop <- stupid thing to do
      lop_buf <- lop; rm('lop');
      load(lop_buf$load_train);
      if (!exists('train') || is.null(train)) {
         warn.AFNI("Variable 'train' not found or null!")
         train <- NULL
      }
      if (exists('lop')) {#part of the bug fix above
         lop_train <- lop; 
         rm('lop');
      }
      lop <- lop_buf; rm('lop_buf'); 
   } else {
      if (is.null(lop$trainsuffix)) {
         errex.AFNI("Have no trainsuffix");
      }
      trainname <- sprintf("%s.Rdat",lop$trainsuffix);
      train <- NULL
      if (lop$reuse_train) {
         if (file.exists(trainname)) {
            note.AFNI(sprintf("Loading training set from pre-existing %s", 
                        trainname))
            rm('train')            
         lop_buf <- lop; rm('lop');
         load(trainname);
            if (!exists('train') || is.null(train)) {
               warn.AFNI("Variable 'train' not found or null!")
               train <- NULL
            }
         if (exists(lop)) {#part of the bug fix above
            lop_train <- lop; 
            rm('lop');
         }
         lop <- lop_buf; rm('lop_buf'); 
         }
      }
   }
   if (is.null(train)) {
      note.AFNI("Creating new training object");
      train <- Train.SigsClassify(lop$tset, samples_frac=lop$samples_frac, 
                           max_samples = lop$max_samples, 
                           min_samples = lop$min_samples, 
                           thiskeyset=lop$thiskeyset, 
                           ltfile = lop$labeltablefile,
                           PlotColumnGroups=lop$ColumnGroups,
                           PlotGroupLabels=lop$GroupLabels,
                           PlotTitle=sprintf("%s", lop$trainsuffix),
                           verb=lop$verb, no_X11=lop$no_X11,
                           svmscale=lop$svmcale, no_tune = lop$no_tune,
                           doprob = lop$doprob)
      if (is.null(train)) errex.AFNI("Failed to get training set");
      lop_train <- lop
      save(train, lop_train, 
            file=sprintf("%s.Rdat",lop$trainsuffix), ascii = TRUE)
   } else {
      note.AFNI("Reusing/Or Reloaded train object");
      if (is.null(train$labeltable)) {
         uk <- unique(train$labs)
         warn.AFNI(paste("Old format training struct.\n",
                         "Assuming ", lop$labeltablefile,
                         "is the proper label table for label keys [",
                         paste(uk,collapse=' '), "]",
                         collapse=''));

         train$labeltable <- build.labeltable(
                              labeltable=lop$labeltablefile, keys=uk)
         if (is.null(train$labeltable)) {
            err.AFNI("Failed to build test label table list");
            return(NULL)
         }
      } 

   }
   if (is.null(train)) {
      errex.AFNI("End of the line, no train here");
   }
   #browser()
   test <- Test.SigsClassify(lop$Tset, lsvm=train, 
                        verb=lop$verb, ltfile = lop$labeltablefile,
                        volsuffix = lop$volsuffix,
                        opath = lop$odir, opref = lop$opref,
                        ltgroupedfile = lop$groupedlabeltablefile,
                        doprob = lop$doprob)  
   lop_test <- lop
   save(test, lop_test, file=sprintf("%s.Rdat",lop$testsuffix), ascii = TRUE)
   note.AFNI("All done.");  

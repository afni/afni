#!/usr/bin/env AFNI_Batch_R

#Clean up
rm(list = ls())

if (1) {
   #MASS's fitdistr, does not converge always
   #library(MASS)
   #fitdistrplus may be better
   #install.packages("fitdistrplus")
   if (!require(fitdistrplus)) {
      require(MASS)
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

#And load R's libsvm 
libLoad("e1071")


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
   lop$samples_frac <- NULL;
   lop$max_samples <- NULL;
   lop$min_samples <- NULL;
   lop$method <- 'svm'
   lop$uidp <- ''
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
      '-method' = apl(n=1, d="svm", dup=FALSE, h=paste(
   "-method METH: Choose approach. Either 'svm', or 'prob'"
                     ) ),
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
   "-test_SF SUBJ FEATURES : Specify a test set's \n",
   "                         subject ID, volume of signatures\n"
                     ) ),
      '-test_SFL' = apl(n = 3, d = NA,  dup = TRUE, h = paste(
   "-test_SFL SUBJ FEATURES LABELS: Specify a test set's \n",
   "                         subject ID, volume of signatures, and\n",
   "                         volume of labels"
                     ) ),
      '-test_SAFL' = apl(n = 4, d = NA,  dup = TRUE, h = paste(
   "-test_SAFL SUBJ ANAT FEATURES LABELS: Specify a test set's \n",
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
      '-min_samples' = apl(n=1, d = 100, h=paste(
   "-min_samples MINL : Min number of training samples\n"
                        ) ), 
      '-max_samples' = apl(n=1, d = 100, h=paste(
   "-max_samples MAX : Max number of training samples\n"
                        ) ),
      '-sampling_fraction' = apl(n=1, d = NA, h = paste(
   "-sampling_fraction: Fraction of voxel from each training volume\n",
   "                    to use for model building.\n"
                        ) ),
      '-uidp' = apl(n=1, d = NA, h = paste(
   "-uidp: Unique ID prefix\n"
                        ) ),           
      '-verb' = apl(n=1, d = 0, h = paste(
   "-verb VERB: VERB is an integer specifying verbosity level.\n",
   "            0 for quiet (Default). 1 or more: talkative.\n"
                        ) ),
      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" ),
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
             method = lop$method <-  ops[[i]],
             train = lop$tset <- parse.TrSetSFL.opts(lop$tset, ops[[i]]),
             train_SFL = lop$tset <- parse.TrSetSFL.opts(lop$tset, ops[[i]]),
             test = lop$Tset <- parse.TeSetSF.opts(lop$Tset, ops[[i]]),
             test_SF = lop$Tset <- parse.TeSetSF.opts(lop$Tset, ops[[i]]),
             test_SFL = lop$Tset <- parse.TeSetSFL.opts(lop$Tset, ops[[i]]),
             test_SAFL = lop$Tset <- parse.TeSetSAFL.opts(lop$Tset, ops[[i]]),
             class = lop$Tset <- parse.CSet.opts(lop$Tset, ops[[i]]),
             odir = lop$odir <- ops[[i]],
             opref = lop$opref <- ops[[i]],
             vol_suffix = lop$volsuffix  <- ops[[i]],
             train_suffix = lop$trainsuffix  <- 
                              strip.extension(ops[[i]], 
                                              c(".Rdat", ".niml.td"))$name_noext,
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
             sampling_fraction = lop$samples_frac <- ops[[i]],
             min_samples = lop$min_samples <- as.numeric(ops[[i]]),
             max_samples = lop$max_samples <- as.numeric(ops[[i]]),           
             uidp = lop$uidp <- ops[[i]],
             featscale = lop$svmscale <- as.logical(ops[[i]])
             )
   }
   if (lop$svmscale != FALSE && lop$svmscale != TRUE) {
      errex.AFNI("Bad -featscale value");
   }
   if (lop$no_X11 == FALSE) {
      #Make sure that is possible or turn option off. 
      if (!length(kk <- capabilities(what='X11')) || kk[1] == FALSE) {
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
   
   if (lop$method == 'svm' || lop$method == 'SVM') {
      lop$method <- 'svm' 
      if (is.null(lop$samples_frac)) lop$samples_frac <- 0.03
      if (is.null(lop$max_samples)) lop$max_samples <- 100
      if (is.null(lop$min_samples)) lop$min_samples <- 100
   } else if (lop$method == 'prob' || lop$method == 'PROB') {
      lop$method <- 'prob' 
      if (is.null(lop$samples_frac)) lop$samples_frac <- 0.03
      if (is.null(lop$max_samples)) lop$max_samples <- 100
      if (is.null(lop$min_samples)) lop$min_samples <- 100
   }
   return(lop)
}# end of read.SigsClassify.opts.batch

#Change options list to SigsClassify variable list 
process.SigsClassify.opts <- function (lop, verb = 0) {
   return(lop)
}


prob.Train.SigsClassify <- function (lvols, samples_frac=NULL, 
                           max_samples = Inf, min_samples = 0,
                           thiskeyset=NULL, ltfile=NULL, 
                           no_X11=FALSE, prefix="prob.Train.SigsClassify",
                           verb = 1) {
   if (is.null(lsamp <- get.SampleSet(lvols, samples_frac = samples_frac, 
                         max_samples = max_samples, 
                         min_samples = min_samples,
                         thiskeyset=thiskeyset, ltfile=ltfile, prefix=prefix,
                         verb = verb, no_X11=no_X11))) {
                         
      errex("Failed to get sample set");
   }
   
   #Copy all fields from lsamp into lprob 
   lprob <- list(sigs = NULL, labs = NULL, Nlabsamp=NULL, 
                 model=NULL, labeltable=NULL)
   for (i in 1:length(lsamp)) {
      nn <- names(lsamp[i])
      lprob[nn] <- lsamp[i]
   }
   rm(lsamp)
   
   if (verb) note.AFNI(sprintf(
      "Prob model building %d samples of %d classes, and %d features",
      dim(lprob$sigs)[1], length(lprob$Nlabsamp), dim(lprob$sigs)[2] ),
      paste('      Label:',names(lprob$Nlabsamp), 
            ', Nsamples',lprob$Nlabsamp, '\n',
            collapse=''), 
      tic=1)
   
   
   #foreach feature, and foreach class, fit the distribution
   lprob$pfci <- matrix( 
                        ncol=length(lprob$class.set), 
                        nrow=length(lprob$feat.labels))
   colnames(lprob$pfci) <- lprob$class.set
   rownames(lprob$pfci) <- lprob$feat.labels
   lprob$pfc <- list()
   for (feat in lprob$feat.labels) {
      for (cls in lprob$class.set) {
         note.AFNI(paste("Fitting ", feat, cls))
         #You must apply proper scaling (scl=1), or else
         #fit fails in many instances
         smp <- get.train.samples(lprob, feat=feat, cls=cls, scl = 1,
                                    verb=max(0,verb-1))
         disp.train.samples(smp);
         #prompt.AFNI()
         kk<-list(fd=fit.pdist(smp))
         lprob$pfc <- c(lprob$pfc, kk) 
         lprob$pfci[feat,cls] <- length(lprob$pfc)
      }
   }
   
   
   return(lprob);
   
}

svm.Train.SigsClassify <- function (lvols, samples_frac=NULL, 
                           max_samples = Inf, min_samples = 0,
                           thiskeyset=NULL, ltfile=NULL, 
                           no_X11=FALSE, prefix="svm.Train.SigsClassify",
                           svmscale=TRUE,
                           verb = 1, no_tune = FALSE, doprob=TRUE) {
   if (is.null(lsamp <- get.SampleSet(lvols, samples_frac = samples_frac, 
                         max_samples = max_samples, 
                         min_samples = min_samples,
                         thiskeyset=thiskeyset, ltfile=ltfile, prefix=prefix,
                         verb = verb))) {
                         
      errex.AFNI("Failed to get sample set");
   }
   #Copy all fields from lsamp into lsvm 
   lsvm <- list(sigs = NULL, labs = NULL, Nlabsamp=NULL, 
                model=NULL, labeltable=NULL)
   for (i in 1:length(lsamp)) {
      nn <- names(lsamp[i])
      lsvm[nn] <- lsamp[i]
   }
   rm(lsamp)
   
   if (verb) note.AFNI(sprintf(
      "SVM model building %d samples of %d classes, and %d features",
      dim(lsvm$sigs)[1], length(lsvm$Nlabsamp), dim(lsvm$sigs)[2] ),
      paste('      Label:',names(lsvm$Nlabsamp), 
            ', Nsamples',lsvm$Nlabsamp, '\n',
            collapse=''), 
      tic=1)
   
               
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

prob.Test.SigsClassify <- function (lvol, lprob, i, ltfile, onames, 
                                    mixfrac, verb=1) {
   if (mixfrac=='uniform') {
      mixfrac <- rep(1,length(lprob$class.set))/length(lprob$class.set)
      names(mixfrac) <- lprob$class.set
   } else {
      err.AFNI(paste("Not ready for mixfrac other than uniform.\nHave ",
                     mixfrac))
      return(NULL)
   }

   #Now do the prob. testing
   note.AFNI(sprintf("About to PROB test on %s", lvol[[1]]$signame));
   #if (verb>1) browser()
   ivox <- dset.nonzero.indices(lvol[[1]]$sig)
   
   if (0) { #WAY too slow
      #Compute probabilities 
      dd <- c(lvol[[1]]$sig$dim[1:3],length(lprob$class.set))
      brk <- array(0,dd) 
      dim(brk) <- c(prod(dd[1:3]),dd[4])  
      uid <- newid.AFNI()
      uid <- sprintf('subj%d',i)
      for (v in 1:dd[4]) {
         note.AFNI(paste("SB ",v));
         brk[ivox,v] <- p.cv_GIV_A(lvol[[1]]$sig$brk[ivox,], 
                              lprob$feat.labels, cls = lprob$class.set[v], 
                              lprob, mixfrac, uid = uid, verb=0)
      }
      note.AFNI("Writing probs");
      write.AFNI(test.vol(lvol,onames,'p','prob'), brk, 
                  label=paste('p', lprob$class.set, sep='.'),
                  defhead=lvol[[1]]$sig$header,
                  verb = max(0,verb-1), scale=TRUE)
   } else {
      nn <- parse.AFNI.name(lvol[[1]]$signame)
      pp <- parse.AFNI.name(test.vol(lvol,onames,'p','prob'))
      cc <- parse.AFNI.name(test.vol(lvol,onames,'c','prob'))
      if (exists.AFNI.name(test.vol(lvol,onames,'p','prob'))) {
         note.AFNI(sprintf("Volume %s exists. Reusing...", 
                             test.vol(lvol,onames,'p','prob')));
      } else {
         anatopt <- ''
         if (!is.null(lvol[[1]]$anatname)) 
            anatopt <- sprintf('-anat %s', lvol[[1]]$anatname)
         cm <- paste('3dGenPriors -sig ', lvol[[1]]$signame,
                   '-uid ', sprintf('%s%s',lop$uidp, nn$prefix), 
                   '-do pc', '-tdist ', sprintf("%s.niml.td",lop$trainsuffix),
                   anatopt, 
                   '-labeltable ', ltfile,
                   '-pprefix ', pp$pprefix,
                   '-cprefix ', cc$pprefix)
         sys.AFNI(cm, echo=TRUE);
      }
      #now loadit back in
      if (is.null(pset <- 
            dset.3DBRKarrayto1D(read.AFNI(test.vol(lvol,onames,'p','prob'))))) {
         errex.AFNI("Failed to read output");
      }             
   }
   if (0) { #Now done in GenPriors
      #Do the classes
      note.AFNI("Getting imax prob");
      ddm <- c(lvol[[1]]$sig$dim[1:3],1)
      brkm <- array(0,ddm) 
      dim(brkm) <- c(prod(ddm[1:3]),ddm[4])
      for (i in ivox) {
         brkm[i] <- lprob$classkey.set[which.max(pset$brk[i,])]
      }
      dim(brkm) <- ddm
      note.AFNI("Writing max prob");
      write.AFNI( test.vol(lvol,onames,'c','prob'), brkm, 
                     label=c('maxprob_labels'), defhead=lvol[[1]]$sig$header,
                     verb = max(0,verb-1), scale=FALSE)
      if (!is.null(ltfile)) { 
         scom <- sprintf('3drefit -labeltable %s %s', 
                           ltfile, test.vol(lvol,onames,'c','prob'));
         system(scom)
      }
   }         
   return()
}

svm.Test.SigsClassify <- function (lvol,lsvm,i, doprob, ltfile, onames, verb=1) {
   #Now do the SVM testing
   note.AFNI(sprintf("About to SVM test on %s, probability = %d",
                  lvol[[1]]$signame, doprob));
   #if (verb>1) browser()
   ivox <- dset.nonzero.indices(lvol[[1]]$sig)
   classy<-predict(lsvm$model, lvol[[1]]$sig$brk[ivox,],
                                     probability=doprob, 
                                     decision.values = TRUE)


   #Make a new volume data
      dd <- c(lvol[[1]]$sig$dim[1:3],1)
      brk <- array(0,dd)
      #Change brk to 1D
      dim(brk) <- c(prod(dd[1:3]),dd[4])
      brk[ivox,1] <- 
            as.integer(levels(classy))[classy]
         #Fastest way to get factor levels back as integer labels
        #http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg08378.html
      #brk back to 3D
      dim(brk) <- dd

      #write out dset 
      write.AFNI( test.vol(lvol,onames,'c','svm'), brk, 
                  label=c('test_labels'), defhead=lvol[[1]]$sig$header,
                  verb = max(0,verb-1), scale=FALSE)

      if (!is.null(ltfile)) { 
         scom <- sprintf('3drefit -labeltable %s %s', 
                           ltfile, test.vol(lvol,onames,'c','svm'));
         system(scom)
      }

      dv <- as.matrix(attr(classy, "decision.values"))
      dd <- c(lvol[[1]]$sig$dim[1:3],dim(dv)[2])
      brk <- array(0,dd)
      dim(brk) <- c(prod(dd[1:3]),dd[4])
      brk[ivox,] <- dv
      dv<-NULL
      dim(brk) <- dd
      write.AFNI(test.vol(lvol,onames,'dv','svm'), brk, 
                  label=paste('tdecis_val', seq(1,dd[4]), sep='.'),
                  defhead=lvol[[1]]$sig$header,
                  verb = max(0,verb-1), scale=TRUE)   

      if (doprob) {
         dv <- as.matrix(attr(classy, "probabilities"))
         dd <- c(lvol[[1]]$sig$dim[1:3],dim(dv)[2])
         brk <- array(0,dd)
         dim(brk) <- c(prod(dd[1:3]),dd[4])
         brk[ivox,] <- dv
         dv<-NULL
         dim(brk) <- dd
         if (dd[4] != length(lsvm$class.set)) {
            err.AFNI(paste ("have", dd[4], "subbricks, but ", 
                     length(lsvm$labeltable$labels), "training labels\n",
                     "Something is fishy here"));
            return(NULL);
         }
         write.AFNI(test.vol(lvol,onames,'p','svm'), brk, 
                     label=paste('p', lsvm$class.set, sep='.'),
                     defhead=lvol[[1]]$sig$header,
                     verb = max(0,verb-1), scale=TRUE)
      }

   #Need space so delete a little
   if (verb > 1) {
      llvv <- lvol
      llvv <- c(llvv, classy=classy, ivox=ivox)
      fout <- sprintf('%s.lvols%d.Rdat',test.vol(lvol,onames,'c','svm'),i)
      note.AFNI(sprintf("Saving svm result in %s", fout));
      save(llvv, file=fout)
      llvv <- NULL
   }
   
   return()
}

#Standard names of testing output volumes
test.vol <- function (lvol, onames, tp, mode) {
   volsuffix <- onames$volsuffix
   opath <- onames$opath
   opref <- onames$opref
   
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
   #prep names
   an <- parse.AFNI.name(lvol[[1]]$signame)
   if (is.null(opath)) ppo <- an$path
   else ppo <- opath
   if (is.null(opref)) {
      ppp <- sprintf('%s.%s%s', lvol[[1]]$subj, an$prefix, volsuffix)
   } else {
      ppp <- sprintf('%s.%s%s', lvol[[1]]$subj, opref, volsuffix)
   }
   if (mode=='svm') { mm <- 'S' }
   else if (mode=='prob') { mm <- 'P' }
   else return(NULL)
   
   if (tp == 'c') return(sprintf('%s/%s.%s.cls%s'  , ppo, ppp, mm, an$view))
   else if (tp == 'p') return(sprintf('%s/%s.%s.clsp%s' , ppo, ppp, mm, an$view))
   else if (tp == 'dv') return(sprintf('%s/%s.%s.clsdv%s', 
                                 ppo, ppp, mm, an$view))
   else if (tp == 'dc') return(sprintf('%s/%s.%s.DiceCoef', ppo, ppp, mm))
   else if (tp == 'gc') return(sprintf('%s/%s.%s.gcls%s'  , 
                                 ppo, ppp, mm, an$view))
   else if (tp == 'gp') return(sprintf('%s/%s.%s.gclsp%s' , 
                                 ppo, ppp, mm, an$view))
   else if (tp == 'gdv') return(sprintf('%s/%s.%s.gclsdv%s', 
                                 ppo, ppp, mm, an$view))
   else if (tp == 'gdc') return(sprintf('%s/%s.%s.gDiceCoef', ppo, ppp, mm))
   else if (tp == 'gld') return(sprintf('%s/%s.%s.cls%s', 
                           ppo, lvol[[1]]$subj, 'gold',an$view))
   else if (tp == 'ggld') return(sprintf('%s/%s.%s.gcls%s', 
                           ppo, lvol[[1]]$subj,  'gold',an$view))
   else if (tp == 'a') return(sprintf('%s/%s.%s%s', 
                           ppo, lvol[[1]]$subj, 'anat',an$view))
   else  return(NULL)
   
}      

Test.SigsClassify <- function (lvols, ltrain=NULL, verb = 1, 
                               ltfile=NULL, overwrite=FALSE,
                               onames = list(volsuffix=NULL, 
                                       opath = NULL, opref = NULL),
                               ltgroupedfile = NULL, doprob=TRUE, 
                               method='svm', mixfrac='uniform') {
   
   if (method=='svm') {
      if (is.null(ltrain) || is.null(ltrain$model))  {
         err.AFNI("Bad input")
         return(NULL);
      }
   } else if (method == 'prob') {
      if (is.null(ltrain) )  {
         err.AFNI("Bad input")
         return(NULL);
      }
   } else return(NULL);
   
   for (i in 1:length(lvols)) {
            
      if (!overwrite && 
            exists.AFNI.name(test.vol(lvols[i],onames,'c',method))) {
         note.AFNI(sprintf("Volume %s exists. Skipping...", 
                              test.vol(lvols[i],onames,'c',method)));
      } else {
         if (verb) note.AFNI(sprintf("About to process signatures %s", 
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
            return(NULL);
         }  
         
         
         lvols[i][[1]] <- c(lvols[i][[1]], 
               list(classout=test.vol(lvols[i],onames,'c',method))) 
         if (method == 'svm') {
            svm.Test.SigsClassify(lvols[i], ltrain, i, doprob, ltfile, 
                                    onames, verb)
         } else {
            prob.Test.SigsClassify(lvols[i], ltrain, i, ltfile, onames,
                                       mixfrac, verb)
         }
      #delete the big stuff. everything's been pretty much saved
      #This can grow quite large with probabilities
      if (verb) memory.hogs(msg='Pre NULLing')
      lvols[i][[1]]$sig$brk <- NULL;
      gc(); 
      if (verb) memory.hogs(msg='Post NULLing')

   }
      
      #Put a copy of the anat in place
      if (!is.null(lvols[i][[1]]$anatname)) {
         if (!copy.AFNI.dset(lvols[i][[1]]$anatname,
                  test.vol(lvols[i],onames,'a',method), overwrite = TRUE)) {
            err.AFNI(sprintf(
                  "Failed to create copy of anatomical volume %s", 
                  test.vol(lvols[i],onames,'a',method)))
            return(NULL);
         }
      }
      
      #Create a merged version if necessary
      if (!is.null(ltgroupedfile) && 
            (  overwrite || 
               !exists.AFNI.name(test.vol(lvols[i],onames,'gc',method)) || 
               !exists.AFNI.name(test.vol(lvols[i],onames,'gp',method))) ) {
         if (exists.AFNI.name(test.vol(lvols[i],onames,'gc',method))) 
            system(sprintf('\\rm -f %s.HEAD %s.BRIK*', 
                  test.vol(lvols[i],onames,'gc',method), 
                  test.vol(lvols[i],onames,'gc',method)))
         if (doprob && 
               exists.AFNI.name(test.vol(lvols[i],onames,'gp',method))) 
            system(sprintf('\\rm -f %s.HEAD %s.BRIK*', 
                  test.vol(lvols[i],onames,'gp',method), 
                  test.vol(lvols[i],onames,'gp',method)))

         if (doprob) {
            if (group.vollabels( 
                  labelvol = test.vol(lvols[i],onames,'c',method), 
                     pvol=test.vol(lvols[i],onames,'p',method),
                     grptable=ltgroupedfile,
                     grplabelpref=test.vol(lvols[i],onames,'gc',method), 
                     grpppref=test.vol(lvols[i],onames,'gp',method)) == 0) {
               err.AFNI("Failed to group labels");
               return(NULL);                 
            }
         } else {
            if (group.vollabels(  
                  labelvol = test.vol(lvols[i],onames,'c',method), 
                        pvol=NULL,
                        grptable=ltgroupedfile,
                        grplabelpref=test.vol(lvols[i],onames,'gc',method), 
                        grpppref=NULL) == 0) {
               err.AFNI("Failed to group labels");
               return(NULL);                 
            }
         }
                           
      }
      #Do some dice action
      if (!is.null(lvols[i][[1]]$labname)) {
         if (!copy.AFNI.dset(lvols[i][[1]]$labname,
                              test.vol(lvols[i],onames,'gld',method), 
                              overwrite = TRUE)) {
            err.AFNI(sprintf(
                  "Failed to create copy of reference label volume %s", 
                  test.vol(lvols[i],onames,'gld',method)))
            return(NULL);
         }
         lvols[i][[1]]$dice <- 
               dice.vollabels(
                  basevol=test.vol(lvols[i],onames,'gld',method),
                  labeltable=ltfile,
                  prefix=test.vol(lvols[i],onames,'dc',method), 
                  labelvol=test.vol(lvols[i],onames,'c',method), 
                  maskby ='LABEL') 
         if (!is.null(ltgroupedfile)) {
            #first group the base
            if (group.vollabels(  
                     labelvol = test.vol(lvols[i],onames,'gld',method),
                     pvol=NULL,
                     grptable=ltgroupedfile,
                     grplabelpref=test.vol(lvols[i],onames,'ggld',method), 
                     grpppref=NULL) == 0) {
               err.AFNI("Failed to group base labels");
               return(NULL);                 
            }
            #Now do the dice
            lvols[i][[1]]$gdice <- dice.vollabels(
                  test.vol(lvols[i],onames,'ggld',method),
                  labeltable=ltgroupedfile,
                  prefix=test.vol(lvols[i],onames,'gdc',method), 
                  labelvol=test.vol(lvols[i],onames,'gc',method),
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
      farg <- sprintf(".%s.dbg.AFNI.args", ExecName)
      # do not save these log files on -help    31 Mar 2016 [rickr]
      if ( ! '-help' %in% args ) {
         save(args, rfile, file=farg, ascii = TRUE) 
         note.AFNI(paste("Saved command line arguments in ", farg))
      }
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
   
   if (is.null(lop$method) || lop$method != 'prob') {
      note.AFNI("SVM approach");
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
            if (exists('lop')) {#part of the bug fix above
               lop_train <- lop; 
               rm('lop');
            }
            lop <- lop_buf; rm('lop_buf'); 
            }
         }
      }
      if (is.null(train)) {
         if (length(lop$tset) < 1) 
            errex.AFNI("Have no training data or training datasets")
         note.AFNI("Creating new training object");
         train <- svm.Train.SigsClassify(lop$tset, samples_frac=lop$samples_frac, 
                              max_samples = lop$max_samples, 
                              min_samples = lop$min_samples, 
                              thiskeyset=lop$thiskeyset, 
                              ltfile = lop$labeltablefile,
                              prefix=lop$trainsuffix,
                              verb=lop$verb, no_X11=lop$no_X11,
                              svmscale=lop$svmcale, no_tune = lop$no_tune,
                              doprob = lop$doprob)
         if (is.null(train)) errex.AFNI("Failed to get training set");
         lop_train <- lop
         save(train, lop_train, 
               file=sprintf("%s.Rdat",lop$trainsuffix), ascii = TRUE)
      } else {
         note.AFNI("Reusing/Or reloading SVM train object");
         if (is.null(train$labeltable)) {
            uk <- unique(train$labs)
            warn.AFNI(paste("Old format training struct.\n",
                            "Assuming ", lop$labeltablefile,
                            "is the proper label table for label keys [",
                            paste(uk,collapse=' '), "]",
                            collapse=''));

            train$labeltable <- read.AFNI.labeltable(ltfile=lop$labeltablefile)
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
      test <- Test.SigsClassify(lop$Tset, ltrain=train, 
                           verb=lop$verb, ltfile = lop$labeltablefile,
                           onames = list(volsuffix = lop$volsuffix,
                                 opath = lop$odir, opref = lop$opref),
                           ltgroupedfile = lop$groupedlabeltablefile,
                           doprob = lop$doprob, method = lop$method)  
      lop_test <- lop
      save(test, lop_test, file=sprintf("%s.Rdat",lop$testsuffix), ascii = TRUE)
   } else {
      #Prob. based approach
      if (!is.null(lop$load_train)) {
         if (!file.exists(lop$load_train)) {
            if (file.exists(kk <- sprintf('%s.Rdat',lop$load_train))) {
               lop$load_train <- kk
            } else {
               errex.AFNI(paste(lop$load_train, " does not exist."));
            }   
         }

         note.AFNI(sprintf("Loading training set from pre-existing %s", 
                    lop$load_train ))
         if (exists('train')) rm('train')            

         load(lop$load_train);
         if (!exists('train') || is.null(train)) {
            warn.AFNI("Variable 'train' not found or null!")
            train <- NULL
         }
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
               load(trainname);
               if (!exists('train') || is.null(train)) {
                  warn.AFNI("Variable 'train' not found or null!")
                  train <- NULL
               }
            }
         }
      }
      if (is.null(train)) {
         if (length(lop$tset) < 1) 
            errex.AFNI("Have no training data or training datasets")
         note.AFNI("Creating new training object");
         train <- prob.Train.SigsClassify(
                           lop$tset, samples_frac=lop$samples_frac, 
                              max_samples = lop$max_samples, 
                              min_samples = lop$min_samples, 
                              prefix=lop$trainsuffix,
                              thiskeyset=lop$thiskeyset, 
                              ltfile = lop$labeltablefile,
                              verb=lop$verb, no_X11=lop$no_X11)
         if (is.null(train)) errex.AFNI("Failed to get training set");
         lop_train <- lop
         save(train, lop_train, 
               file=sprintf("%s.Rdat",lop$trainsuffix), ascii = TRUE)
         NI.write.train.dists(train, fname=sprintf("%s.niml.td",lop$trainsuffix))
      } else {
         note.AFNI("Reusing/Or reloading PROB train object");
      }
      if (is.null(train)) {
         errex.AFNI("End of the line, no train here");
      }
      #browser()
      test <- Test.SigsClassify(lop$Tset, ltrain=train, 
                           verb=lop$verb, ltfile = lop$labeltablefile,
                           onames= list(volsuffix = lop$volsuffix,
                                        opath = lop$odir, opref = lop$opref),
                           ltgroupedfile = lop$groupedlabeltablefile,
                           method=lop$method, mixfrac='uniform')  
      lop_test <- lop
      save(test, lop_test, file=sprintf("%s.Rdat",lop$testsuffix), ascii = TRUE)

   }
   note.AFNI("All done.");  

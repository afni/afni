#------------------------------------------------------------------
# Functions to support 3dSignatures.R
#------------------------------------------------------------------

keylabel.labeltable <- function (key=NULL, ltfile=NULL) {
   com <- sprintf(
           '@MakeLabelTable -quiet_death -labeltable %s -klabel %d',
                  ltfile, key)
   lab <- system(com, ignore.stderr = TRUE, intern=TRUE)
   if (length(lab) == 0) {
      err.AFNI(paste("Failed to get key label from labeltable.\n",
                     "Command ",com,"Failed"));
      return(NULL);
   }
   return(lab);
}

labelkey.labeltable <- function (label=NULL, ltfile=NULL) {
   kk <- vector()
   for (ll in label) {
      com <- sprintf(
     '@MakeLabelTable -word_label_match -quiet_death -labeltable %s -lkeys %s',
                  ltfile, ll)
      lab <- system(com, ignore.stderr = TRUE, intern=TRUE)
      if (length(lab) == 0) {
         err.AFNI(paste("Failed to get key of label", 
                        label," from labeltable",ltfile, ".\n",
                        "Command ",com,"Failed"));
         return(NULL);
      }
      kk <- c(kk, as.numeric(lab))
      
      if (is.na(kk[length(kk)])) {
         err.AFNI(paste("Failed to get key of label", 
                        label," from labeltable.\n",
                        "Command ",com,"Failed"));
         return(NULL);
      }
   }
   return(kk)
}

build.labeltable <- function(labeltable=NULL, keys=NULL) {
   lt <- list(labels=NULL, keys=keys)
   for (key in keys) {
      if (is.null(lab <- keylabel.labeltable(key=key, ltfile=labeltable))) {
         return(NULL);
      }

      lt$labels <- c(lt$labels, lab) 
   }
   return(lt)
}

labels.labeltable  <- function(ltfile=NULL) {
   com <- paste('@MakeLabelTable -labeltable ', ltfile,
                '-all_labels',
                collapse = ' ');
   labs <- system(com, ignore.stderr = TRUE, intern=TRUE)
   if (length(labs) == 0) {
      err.AFNI(paste("Failed to get key label from labeltable.\n",
                     "Command ",com,"Failed"));
      return(NULL);
   }
   return(labs)   
}


dice.vollabels  <- function(basevol=NULL,
                        labeltable=NULL,
                        prefix=NULL, labelvol=NULL,
                        diceout=NULL, maskby = 'BASE') {
   if (is.null(diceout)) {
      diceout <- sprintf('%s.1D', prefix)
   }
   if (is.null(labelvol)) {
      err.AFNI("Need labelvol")
      return(NULL)
   }
   if (is.null(maskby) || maskby == '') {
      mm <- ''
   } else if (maskby == 'BASE') {
      mm <- '-mask_by_base'
   } else if (maskby == 'LABEL') {
      mm <- '-mask_by_dset_vals'
   } else {
      err.AFNI(paste("Bad value ", maskby))
      return(NULL)
   }
   com <- paste('@DiceMetric -base ', basevol,
                '-save_match -save_diff ', mm,
                '-prefix ', prefix,
                '-labeltable ',labeltable ,
                '-dsets ',  labelvol,
                '> ', diceout,
                collapse = ' ')
   
   sys.AFNI(com, echo = TRUE)
   #system(sprintf("echo '#Command: %s' >> %s", com, diceout));
   dd <- read.AFNI.matrix(diceout)
   ll <- list(command=com, coef=dd[,2])
   names(ll$coef) <- labels.labeltable(labeltable)
   
   return(ll)
}

group.vollabels <- function(labelvol=NULL, pvol=NULL, 
                            grptable=NULL, 
                            grplabelpref=NULL, grpppref=NULL) {
   if (!is.null(pvol)) {
      com <- paste('@RegroupLabels -grouped_labeltable ',
             grptable, '-labdset ',labelvol ,
             '-grpd_labprefix ', grplabelpref, 
             '-pdset', pvol,
             '-grpd_pprefix', grpppref,
             collapse = '')
   } else {
      com <- paste('@RegroupLabels -grouped_labeltable ',
             grptable, '-labdset ',labelvol ,
             '-grpd_labprefix ', grplabelpref, 
             collapse = '')
   }
   note.AFNI(com)
   mm <- system(com, ignore.stderr = FALSE, intern=TRUE)
   note.AFNI(mm)
   
   if (is.null(pvol)) return(used.AFNI.prefix(grplabelpref))
   else return(used.AFNI.prefix(grplabelpref)*used.AFNI.prefix(grpppref))
}


library(lattice)
library(fitdistrplus)

#------------------------------------------------------------------
# Functions to support 3dSignatures.R
#------------------------------------------------------------------
groupofKey.labeltable <- function (key, labeltable) {
   if (is.character(labeltable)) 
      labeltable <- read.AFNI.labeltable(labeltable)
   gok <- vector()
   for (k in key) {
      g <- which (labeltable$keys == k)
      if (length(g)) gok <- c(gok,labeltable$groups[g])
      else {
         err.AFNI("Failed")
         return(NULL)
      }
   }
   return(gok)
}

keylabel.labeltable <- function (key=NULL, ltfile=NULL) {
   if (0) { #OLD
      com <- sprintf(
              '@MakeLabelTable -quiet_death -labeltable %s -klabel %d',
                     ltfile, key)
      lab <- system(com, ignore.stderr = TRUE, intern=TRUE)
      if (length(lab) == 0) {
         err.AFNI(paste("Failed to get key label from labeltable.\n",
                        "Command ",com,"Failed"));
         return(NULL);
      }
   } else {
      if (is.character(ltfile)) 
         ltfile <- read.AFNI.labeltable(ltfile)
      lab <- vector()
      for (kk in key) {
         lab <-c(lab, ltfile$labels[which(ltfile$keys==kk)])
      }
   }
   return(lab);
}

labelkey.labeltable <- function (label=NULL, ltfile=NULL) {
   if (0) { #old
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
   } else {
      if (is.character(ltfile)) 
         ltfile <- read.AFNI.labeltable(ltfile)
      kk <- vector()
      for (ll in label) {
         kk <- c(kk, ltfile$keys[which(ltfile$labels==ll)])
      }
      
   }
   return(kk)
}

build.labeltable <- function(labeltable=NULL, keys=NULL, 
                           grp.label = c('CSF','GM','WM','InSk','OutSk','Out')) {
   #OBSOLETE, use read.AFNI.labeltable instead
   if (is.null(keys)) {
      com <- sprintf(
           '@MakeLabelTable -quiet_death -labeltable %s -all_keys', labeltable)
      ss <- sys.AFNI(com)
      if (ss$stat == 1) {
         err.AFNI(paste("Failed to get keys from labeltable",ltfile, ".\n",
                        "Command ",com,"Failed"));
         return(NULL);
      }
      keys <- as.numeric(ss$out)
   }
   lt <- list(labels=NULL, keys=keys, grp.label=grp.label, groups=NULL)
   for (key in keys) {
      if (is.null(lab <- keylabel.labeltable(key=key, ltfile=labeltable))) {
         return(NULL);
      }
     
      lt$labels <- c(lt$labels, lab)
      if (!is.null(grp.label)) {
         nn <- 0
         ig <- 0
         for (i in 1:length(grp.label)) {
            if (length(grep(grp.label[i],lab))) {
               if (ig == 0) {
                  nn <- 1
                  ig <- i
               } else {
                  if (  length(grp.label[ig]) != length(lab) ) {
                     if (length(grp.label[i]) == length(lab) ) {
                        # A better match
                        nn <- 1
                        ig <- i
                     } else {
                        #Another partial
                        nn <- nn + 1
                        ig <- i
                     }
                  } 
               }
            }  
            
         }
         if (nn != 1) {
            warn.AFNI(paste(
               "Failed to find 1 group for ",lab, ". Found ",
               nn ,"candidates. Setting grp to",
               length(grp.label)+1))
            ig <- length(grp.label)+1
         }
         lt$groups <- c(lt$groups, ig) 
         
      }
   }
   if (!is.null(grp.label)) {
      if (max(lt$groups) > length(grp.label)) {
         lt$grp.label <- c(grp.label,"Unknown")
      } else {
         lt$grp.label <- grp.label
      }
   }
   return(lt)
}

labels.labeltable  <- function(ltfile=NULL) {
   if (0) { #OLD APPROACH
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
   } else {
      if (is.character(ltfile)) 
         ltfile <- read.AFNI.labeltable(ltfile)
      return(ltfile$labels)
   }
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

get.SampleSet <- function (lvols, samples_frac=NULL, 
                           max_samples = Inf, min_samples = 0,
                           thiskeyset=NULL, ltfile=NULL, prefix=NULL,
                           verb = 1, no_X11=TRUE) {
   if (length(samples_frac) != 1 || samples_frac > 1 || samples_frac <= 0) {
      err.AFNI("Bad sample fraction")
      return(NULL);
   }
   if (is.null(ltfile)) {
      err.AFNI("ltfile is now mandatory")
      return(NULL)
   } 
   
   
   lsamp <- list(sigs = NULL, labs = NULL, Nlabsamp=NULL, 
                  model=NULL, labeltable=NULL, feat.labels=NULL)
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
            if (is.null(classkey.set)) 
               classkey.set <- setdiff(unique(lvol$lab$brk), 0)
         } else {
            classkey.set <- setdiff(thiskeyset, 0)
         }
         #name classkey.set by the labels from the labeltable
         lsamp$labeltable <- read.AFNI.labeltable(ltfile)
         if (is.null(lsamp$labeltable)) {
            err.AFNI("Failed to load label table ");
            return(NULL)
         }
         lsamp$classkey.set <- classkey.set
         lsamp$class.set <- keylabel.labeltable(classkey.set, lsamp$labeltable)
         
         if (verb > 1) note.AFNI(sprintf("Forming labselecs"),tic=1) 
         labselecs <- list()
         for (lv in classkey.set) {
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
            labselecs <- 
               c (labselecs, 
                  list(  list(
                         labval=lv, ilab = ilab) ) )
            names(labselecs[length(labselecs)]) <-
                                       sprintf('VoxSelection.%d', lv)
         }
      } else {
         errex.AFNI("Not implemented yet");
      }
      #Load signatures
      if (is.null(lvol$sig)) {
         if (verb) 
            note.AFNI(sprintf("Loading signatures %s", lvol$signame),tic=1) 
         lvol$sig <- dset.3DBRKarrayto1D(read.AFNI(lvol$signame))
         
      } else {
         lvol$sig <- dset.3DBRKarrayto1D(lvol$sig)
      }
      if (is.null(lsamp$feat.labels)) 
         lsamp$feat.labels <- dset.labels(lvol$sig)
         
      if (is.null(lvol$sig)) {
         err.AFNI('Failed to get signatures');
         return(NULL)
      }
      #Now build the set of features for each label
      if (verb) note.AFNI(sprintf("Building sigs and labs"),tic=1) 
      for (lbsel in labselecs) {
         if (length(lbsel$ilab)) {
            lsamp$sigs <- rbind(lsamp$sigs,lvol$sig$brk[lbsel$ilab,])
            lsamp$labs <- c(lsamp$labs,rep(lbsel$labval,length(lbsel$ilab)))      
            iul <- which(names(lsamp$Nlabsamp) == as.character(lbsel$labval))
            if (length(iul)) {
               lsamp$Nlabsamp[iul] <- lsamp$Nlabsamp[iul]+length(lbsel$ilab)
            } else {
               lsamp$Nlabsamp <- c(lsamp$Nlabsamp, length(lbsel$ilab))
               names(lsamp$Nlabsamp)[length(lsamp$Nlabsamp)] <- 
                  as.character(lbsel$labval)
            }
         }
      }  
   }
   
   #Before testing, a safety check 
   mm <- matrix(0, dim(lsamp$sigs)[2], length(lsamp$class.set))
   for (l in 1:length(lsamp$class.set)) {
      for (i in  1:dim(lsamp$sigs)[2]) {
         ifnd <- which (lsamp$labs==lsamp$class.set[l])
         if (length(ifnd)) {
            mm[i,l] <-median(lsamp$sigs[ifnd,i])
         }
      }
   }
   
   plot.1D(dmat = mm, oneplot=1, col.ystack=FALSE, 
               col.grp=groupofKey.labeltable(lsamp$class.set, lsamp$labeltable), 
               grp.label=lsamp$labeltable$grp.label, 
               prefix =sprintf('%s.jpg',prefix), ttl.main=prefix,
               nodisp = no_X11, save.Rdat=TRUE)

   #And show the sample set
   note.AFNI("About to show all samples...")
   disp.all.train.samples( lsamp, nodisp = no_X11, 
                              prefix =sprintf('%s.trset.pdf',prefix))
   
   #compute the scaling 
   lsamp$feat.scale <- compute.feature.scaling (lsamp)
   
   return(lsamp)
}

disp.all.train.samples <- function (lsamp, nodisp = FALSE, prefix='hello.pdf') {
   
   n_col <- ncol(lsamp$sigs)
   n_samps <- nrow(lsamp$sigs)
   
   #Class factor
   cls <- matrix(ncol=n_col, nrow=n_samps)
   for (i in 1:n_col) cls[,i] = lsamp$labs
   cls.f <- factor(cls, levels=lsamp$labeltable$keys, 
               labels=lsamp$labeltable$labels)

   #feature factor
   feat <- matrix(ncol=n_col, nrow=n_samps)
   for (i in 1:n_col) feat[,i] = rep(i, n_samps)
   ll <- gsub('_mm','',lsamp$feat.labels)
   ll <- gsub('MEDIAN.','m',ll)
   ll <- gsub('MAD.','M',ll)
   ll <- gsub('P2skew.','s',ll)
   feat.f <- factor(feat, levels=seq(1:n_col),
               labels=ll)
               
   #setup device
   P <- list(prefix=prefix, nodisp=nodisp, dev.this=NULL, dev.new=FALSE)
   P$dev.this <- plot.1D.setupdevice(P)
   print(xyplot(lsamp$sigs~feat.f|cls.f, scales=list(rot=90)))
   plot.1D.unsetupdevice(P)

   return(0)
}

get.train.samples <- function (lsamp, feat=NULL, cls=NULL, scl = 0, verb=0) {
   #Safety check
   if (ncol(lsamp$sigs) != length(lsamp$feat.labels)) {
      err.AFNI(paste("Bad news! You must have ", 
               ncol(lsamp$sigs), "elements in lsamp$feat.labels",
               "Currently have", length(lsamp$feat.labels)))
      return(NULL);
   }
   
   if (verb) 
      note.AFNI(paste("Fetching samples of feature", feat,", class ", cls))
   
   icol <- which(lsamp$feat.labels == feat)
   if (length(icol) != 1) {
      err.AFNI(paste("Feature", feat,"not found"))
      return(NULL);
   }
   if (verb > 1)  note.AFNI(paste("Column ",icol, "matches feature", feat))
   
   if (cls == 'ALL') {
      ss <- lsamp$sigs[,icol]
   } else {   
      ikey <- which(lsamp$class.set == cls)
      if (length(ikey) != 1) {
         err.AFNI(paste("CLass", cls,"not found"))
         return(NULL);
      }
      key <- lsamp$classkey.set[ikey]
      if (verb > 1)  note.AFNI(paste("Key ",key, "matches class", cls))

      ss <- lsamp$sigs[which(lsamp$labs==key),icol]
   }
   if (scl) {
      sclp <- lsamp$feat.scale[feat][[1]]
      ss <- (ss*sclp$s+sclp$d)
      attr(ss,"scale.params") <- sclp
   }

   attr(ss,"feature") <- feat
   attr(ss,"seg.class") <- cls

   return(ss)
}

compute.feature.scaling <- function (lsamp, a=1, b=10) {
   fs <- list()
   if (0) {
      for (i in 1:ncol(lsamp$sigs)) {
         d <- min(lsamp$sigs[,i])
         s <- 10/(max(lsamp$sigs[,i])-d)
         kk <- list(d=d, s= s)
         fs <- c(fs, list(feat.scale = kk))
      }
   } else {
      for (i in 1:ncol(lsamp$sigs)) {
         MI <- min(lsamp$sigs[,i])
         MX <- max(lsamp$sigs[,i])
         d <- (a*MX-b*MI)/(MX-MI)
         s <- (b-a)/(MX-MI)
         kk <- list(d=d, s= s)
         fs <- c(fs, list(feat.scale = kk))
      }
   }
   names(fs) <- lsamp$feat.labels
   return(fs)
}

#Write distribution parameters to niml file
NI.write.train.dists <- function (lprob, fname='') {
   #form the feature scale string
   for (i in 1:length(lprob$feat.scale)) {
      kk <- paste(sprintf('%s_Scale+Shift ="', lprob$feat.labels[i]),
               lprob$feat.scale[i][[1]]$s, ' , ' ,lprob$feat.scale[i][[1]]$d, 
               '"\n',sep='')
      if (i==1) tt<-kk
      else tt <- paste(tt, kk, sep='')
   }
   hd <- paste('<TRAIN_DISTS\n',
               'ni_type="2*String,2*float"\n',
               'ni_dimen="', nrow(lprob$pfci)*ncol(lprob$pfci),'"\n',
               'ColumnLabels="Feature ; Class ; shape ; rate"\n',
               'ClassSet="', paste(lprob$class.set,collapse=' ; '),'"\n',
               'FeatureSet="', paste(lprob$feat.labels,collapse=' ; '),'"\n',
               'Dist="',lprob$pfc[1][[1]]$distname,'"\n',
               tt,
               '>',
               sep='')
   
   cat(hd, file=fname, sep='\n', append=FALSE)
   
   fn <- rownames(lprob$pfci)
   cn <- colnames(lprob$pfci)
   for (f in 1:nrow(lprob$pfci)) {
      for (c in 1:ncol(lprob$pfci)) {
         i <- lprob$pfci[f,c]
         par <- get.train.pdist(lprob, f, c, 0)
         ll <- paste('"',fn[f],'" "', cn[c],'"',
                     ' ', par$estimate['shape'],' ', par$estimate['rate'],
                     sep = '')
         cat(ll, file=fname, sep='\n', append=TRUE)
      }
   }
   cat('</TRAIN_DISTS>', file=fname, sep='\n', append=TRUE)
}


#Get the parameters of the gamma fit for some feature, and class 
get.train.pdist <- function (lsamp, feat=NULL, cls=NULL, verb=1) {
   pdist <- lsamp$pfc[lsamp$pfci[feat,cls]][[1]]
   if (verb) plot(pdist)
   return(pdist)
}

pdf.gam <- function (x,ash,brt, uns=0) {
   if (uns) {
      an <- (brt^ash)/gamma(ash)*(x^(ash-1))*exp(-brt*x)
   } else {
      lan <- ash*log(brt)-lgamma(ash)+(ash-1)*log(x)-brt*x;
      an <- exp(lan)
   }
   return(an)
}

area.gam <- function (x, binw=0.1, shape, rate, meth=1) {
   #reference
   if (meth != 2) 
      a0 <- (pgamma(x-binw/2.0, 
               shape=shape, rate=rate,
               lower.tail=FALSE)       - 
           pgamma(x+binw/2.0, 
               shape=shape, rate=rate,
               lower.tail=FALSE));
   #approx
   if (meth != 1)
      a1 <- (pdf.gam((x-binw/2.0), shape, rate)+pdf.gam((x+binw/2.0), shape, rate))/2.0*binw;
      
   if (meth == 0) {
      #debug
      cat ('Reference, Approx = ', a0, ',', a1, '\n');
      return(-1.0);
   } else if (meth == 1) {
      #reference
      return(a0)
   } else {
      #approx
      return(a1)
   }
}

disp.train.pdist <- function (lsamp, feats=NULL, clss=NULL, stp=0.1) {
   if (is.null(feats)) feats <- lsamp$feat.labels
   if (is.null(clss)) clss <- lsamp$class.set
   imax <- length(clss)*length(feats)
   k<-1
   for (cls in clss) {
      for (feat in feats) {
         k <- k + 1
         plot(par <- get.train.pdist(lsamp, feat, cls, verb=0))
         s<-0.0;
         for (x in seq(1,10,stp)) 
            s<-s+pdf.gam(x,20.8156340614560, 3.5370637717458)*stp;
         if (k<imax) ans <- prompt.AFNI(paste("Showing ", feat, cls, 
                  "[shape=",par$estimate['shape'], 
                  ',rate=' , par$estimate['rate'],
                  ', sum = ', s,
                           "]\n   Hit enter to proceed."))
         if (ans != 1) return(0)
      }
   }
}

#Estimate the probability of feature amplitude given a class
# 4-5 secs per call
p.a_GIV_cvfu <- function (af , feat, cls, lsamp, binwidth=0.01, uid, verb=1) {
   #note.AFNI(paste("p.a_GIV_cvfu in"));
   fsave <- sprintf('/tmp/%s.p.a_GIV_cvfu-%s-%s.Rdat',
                     uid, feat, cls)
   if (file.exists(fsave)) {
      load(fsave)
   } else {
      par <- get.train.pdist(lsamp, feat, cls, verb=0)
      #apply feature scaling
      sclp <- lsamp$feat.scale[feat][[1]]
      af <- (af*sclp$s+sclp$d)
      p <- pgamma(af, 
               shape=par$estimate['shape'], rate=par$estimate['rate'],
               lower.tail=FALSE)       - 
           pgamma(af+binwidth, 
               shape=par$estimate['shape'], rate=par$estimate['rate'],
               lower.tail=FALSE)
      save(p, file=fsave)
   }
   #note.AFNI(paste("p.a_GIV_cvfu out"));
   return(p)
}

#Estimate the probability of a class, given a feature
p.cv_GIV_afu <- function (af, feat, cls, lsamp, mixfrac, uid, verb=0) {
   ss <- 0
   nn <- 0
   for (v in lsamp$class.set) {
      #note.AFNI(paste("p.cv_GIV_afu", v))
      pp <- mixfrac[v] * p.a_GIV_cvfu(af,feat=feat,cls=v, 
                                       lsamp=lsamp, uid=uid, verb=0)
      if (v == cls) nn <- pp
      ss <- ss + pp
   }
   return(nn/ss)
}

#Estimate the probability of a class, given all features
p.cv_GIV_A <- function (A, Afeat, cls, lsamp, mixfrac, uid, verb=0) {
   p <- 0
   for (i in 1:ncol(A)) {
      note.AFNI(paste("p.cv_GIV_A", i))
      p <- p + log(p.cv_GIV_afu(A[,i], feat=Afeat[i], 
                     cls=cls, lsamp=lsamp, mixfrac=mixfrac, uid=uid,
                     verb=0))
   }
   return(exp(p))
}

disp.train.samples <- function (smp, fitprms=NULL) {
   h<-hist(smp,breaks=100)
   xhist<-c(min(h$breaks),h$breaks)
   yhist<-c(0,h$density,0)
   if (is.null(fitprms)) yfit <- NULL 
   if (is.null(atr <- attr(smp,"scale.params"))) xlab <- "feature value"
   else {
      xlab <- paste("feature value scaled d,s=[", atr$d, ',', atr$s, "]")
   }
   plot( xhist,yhist,
         type="s",ylim=c(0,max(yhist,yfit)),
         xlab=xlab,ylab="Density",cex.lab=1.5,cex.axis=1.5)
   title(paste('pdf(',attr(smp,"feature"),',',attr(smp,"seg.class"),')'))
   
}

lp.norm <- function (dv) {
   if (0) {
      #method 0, unstable
      dvn <- vector(length=length(dv))
      ddf <- 0.0
      for (i in 1:length(dv)) {
         ddf <- ddf+exp(dv[i])
      }
      for (i in 1:length(dv)) {
         dvn[i] = exp(dv[i])/ddf
      }
   } else {
      #method 1
      dvn <- vector(length=length(dv))
      for (i in 1:length(dv)) {
         ddf <- 1.0
         for (ii in 1:length(dv)) {
            if (ii != i) ddf <- ddf + exp(dv[ii]-dv[i])
         }
         dvn[i] = 1.0 / ddf
      }
   }
   return(dvn) 
}
 
fit.pdist <- function (smp, dist="gamma", verb=1) {
   
   parms <- NULL
   if (dist == "gamma") {
      if (!exists('fitdist')) { #If we do not have fitdistrplus
         parms<-fitdistr(as.vector(smp),"gamma", lower=0.001) 
      } else {
         parms<-try(fitdist(as.vector(smp),"gamma"))
      }
   } else if (dist == "norm") {
      if (!exists('fitdist')) { #If we do not have fitdistrplus
         parms<-fitdistr(as.vector(smp),"norm", lower=0.001) 
      } else {
         parms<-fitdist(as.vector(smp),"norm")
      }
   } else {
      err.AFNI("Bad distribution name");
      return(NULL)
   }
   
   if (attr(parms,"class") != 'fitdist' && attr(parms,"class") != 'fitdistr' ) {
      warn.AFNI(paste("Failed to fit sample ", 
                  attr(smp,"feature"),',',attr(smp,"seg.class")))
      parms <- NULL
   } 
      
   return(parms)  

}

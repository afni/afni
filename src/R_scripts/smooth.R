#!/usr/bin/env AFNI_Batch_R
#iplots are quite cool, 
#  but when it comes to plotting multiple time series (say 576 x 42) 
#  it gets reaaaaal slow. JAVA SUCKS.
#tkrplot has the stupid feature of not allowing autoresize of the plot.
#  you can do it manually by setting hscale and vscale, but I'd rather
#  play with a copperhead instead

require('graphics')
require('tcltk')
require('signal')

expand_1D_string <- function (t) {
   vvf = vector(length = 0, mode="numeric")
   #replace .. with : and split at ,
   s = strsplit(sub("..",":",t, fixed=TRUE), ",")[[1]]

   #Now loop and form vector of components
   for (si in s) {
      #cat ("working ", si, "\n")
      if (length(grep(":",si))) {
         vv = eval(parse(text=si))
      } else if (length(grep("@",si))) {
         ssi = as.numeric(strsplit(si,"@")[[1]])
         #cat ("ssi = ", ssi, "\n")
         vv = rep(ssi[2], ssi[1])
      } else {
         vv = as.numeric(si)
      }
      #cat(si," = ",vv, "\n")
      vvf <- c(vvf, vv)
      #cat("vvnow = ",vvf, "\n")
   }
   return(vvf)
}


show_xmat <- function (ff, isel=1:1:ncol(ff), descr="") {
   #cat ('isel in show_xmat')
   #show(isel)
   #Set colors based on group and offset by 2 so that -1 and 0 get colored
   colvec <- cg <- attr(ff,'ColumnGroups')
   #first color regressors of no interest
   colvec[cg == -1] = 1
   colvec[cg == 0 ] = 2
   N_basegroup = 2 #2 types of baseline (RONI)
   
   #now color regressors of interest, skip yellow
   collist = c(3,4,5,6,8)  #all but 1st two and yellow
   N_collist = length(collist)
   for (i in min(colvec[colvec > 0]):max(colvec[colvec > 0])) {
      colvec[cg == i] <- collist[(i-N_basegroup) %% N_collist + 1] 
   }
   
   if (length(isel) > 10) {
      tp = 'single'
      #Calculate normalized version for full display
      ra = matrix(nrow=ncol(ff), ncol=3)
      for (i in 1:ncol(ff)) {
         ra[i,] <- as.vector(quantile(ff[,i], c(0,1,0.5)))
      }
      ffr <- ff
      offs <-vector(length = ncol(ff), mode="numeric")
      for (i in 1:ncol(ff)) {
         ffr[,i] <- (ff[,i]-ra[i,1])/(ra[i,2]-ra[i,1])+1.4*i
         offs[i] = mean(ffr[,i])
      }
      if (is.null(dev.list())) x11()
      plot(ffr[,isel], plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel = lines, nc = 3, yax.flip = FALSE,
           axes = TRUE, col = colvec[isel])
      text (0, offs, colnames(ff), col = colvec[isel], adj=c(1,0))
   } else {
      tp = 'multiple'
      if (is.null(dev.list())) x11()
      if (length(isel) == 1) {
         colm <- colvec[isel]
      } else {
         colm <- 1   #ARRRRGH only one col is used for multiple line drawings!
      }
      plot(ff[,isel], plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel = lines, nc = 1, yax.flip = FALSE,
           axes = TRUE, col = colm) 
   }
   
   cat           ( 'Xmat ', attr(ff,'FileName'), '\n',
                  'Baseline indices: ', xmat_base_index(ff), '\n',
                  'Motion indices: ', xmat_motion_index(ff), '\n',
                  'Task indices: ', xmat_alltasks_index(ff), '\n');
   
   view_cond <- kappa(ff[,isel], exact=TRUE)
   stim_cond <- kappa(ff[,xmat_alltasks_index(ff)], exact=TRUE)
   all_cond <- kappa(ff, exact=TRUE)
   all_cond_no_mot <- kappa(ff[,-xmat_motion_index(ff)], exact=TRUE)
   stit = paste ( 'Rall          : ', sprintf('%.2f', all_cond), '\n',
                  'Rall-motion   : ', sprintf('%.2f', all_cond_no_mot), '\n',
                  'Rall-roni     : ', sprintf('%.2f', stim_cond),'\n',
                  'Rviewed       : ', sprintf('%.2f', view_cond),'\n');
      
   title (paste('Xmat: ', attr(ff,'FileName'),'\n'));
   
   #update report window
   ttc <<- condition_report(ttc, ff, isel, descr=descr)

   return(0)
}


read_1D <- function (xmatfile, nheadmax=10) {

   #Now load the whole deal, comments are skipped by defaults
   ff <- ts(as.matrix(read.table(xmatfile)), deltat = 1)
   
   return(ff)
}

set_filteropt <- function (ff) {
   bf <<-butter(ff[1], ff[2])
}

filter_1D <- function (ff) {
   ffs <- filter(bf,ff)
   return (ffs)
}
main_loop <- function (fn) {
   #read file
   ff <- read_1D(fn)
   set_filteropt(c(3,0.1))
   ffs <- filter_1D(ff)
   plot (ff,type="l")
   lines(ffs, col='red'); 
   
   return(ffs)
}

if (0) {
#main   
   #begin windgetting
   fn <- tclvalue(tkgetOpenFile(filetypes = "{{1D Files} {.1D}} {{All files} *}"))
   if (nchar(fn)) {
      ffs = main_loop(fn)
   } else {
      tkmessageBox(message="So long.")  
   }
}

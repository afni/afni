#!/usr/bin/env AFNI_Batch_R
#
#  WARNING: This script is solely for testing R and C interface
#           It is needed by 3dTsmoothR.c and is not intended for
#           anything but a demonstration of R & C in harmony
#
#
require('graphics')
require('tcltk')
require('signal')




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

#Call demo_1D_smooth()
#select a 1D time series and see it smoothed
demo_1D_smooth <- function () {
   fn <- tclvalue(   tkgetOpenFile( filetypes = 
                                       "{{1D Files} {.1D}} {{All files} *}"))
   if (!nchar(fn)) {
      tkmessageBox(message="So long.")  
   }
   
   #read file
   ff <- read_1D(fn)
   set_filteropt(c(3,0.1))
   ffs <- filter_1D(ff)
   plot (ff,type="l")
   lines(ffs, col='red'); 
   
   return(ffs)
}


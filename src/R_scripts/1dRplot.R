#!/usr/bin/env AFNI_Batch_R


#Clean up
rm(list = ls())

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

ExecName <- '1dRplot'


greeting.1dRplot <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 1dRplot.R ==================          
             An R beta version of 1dplot
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      
reference.1dRplot <- function ()
   return(
""
   )


#The help function for 1dRplot batch (command line mode)
help.1dRplot.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
Usage:
------ 
 1dRplot is a program for plotting a 1D file'
   
   ex1 <- 
"
Example 1 --- :
--------------------------------
      1dRplot  -input IBSR_01.SubClasses.skew.1D.repsig    

      1dRplot  -input IBSR_01.SubClasses.skew.1D.repsig \\
               -col.grp 'R:c(rep(1,10), rep(2,10), rep(3,10))' \\
               -grp.labels CSF GM WM \\
               -col.nozeros -Nocol.ystack \\
               -prefix ZePlotIzSaved.pdf
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
   cat(intro, ex1, ss, reference.1dRplot(), sep='\n');
   
   if (adieu) exit.AFNI();
}

parse.1dRplot.Groups <- function(op) {
   mm <- read.AFNI.matrix(op)
   return(mm)
}

init.1DRplot.lop <- function () {
   lop <- plot.1D.optlist()
   return(lop)
}

#Change command line arguments into an options list
read.1dRplot.opts.batch <- function (args=NULL, verb = 0) {
   
   params <- list (
      '-input' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input 1D_INPUT: file to plot.\n"
                     ) ),
                     
      '-input_delta' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input_delta 1D_INPUT: file containing value for error bars\n"
                     ) ),
                     
      '-x' = apl(n = c(1), h = paste (
   "-x 1D_INPUT: x axis\n"
                  ) ), 

      '-save' = apl(n = 1, d = NA,  h = paste(
   "-save SAVE_PREFIX: Save plot and quit\n"
                     ) ),
                     
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output prefix \n"
                     ) ),

      '-title' = apl(n = 1, d = NULL,  h = paste(
   "-title TITLE: Graph title\n"
                     ) ),
            
      '-grp.labels' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-grp.labels GROUP1 [GROUP2]: Labels assigned to each group.\n",
   "                         Default is no labeling\n"
                     ) ),
      '-col.plot.char' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.plot.char : Symbols for each column in -input.\n"
                     ) ),
                     
      '-col.colors' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.colors : Colors for each column in -input.\n"
                     ) ),
                     
      '-col.line.types' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.line.types : Line type for each column in -input.\n"
                     ) ),
                     
      '-col.names' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.names : Name of each column in -input.\n"
                     ) ),

      '-leg.names' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.names : Names to use for items in legend.\n",
   "             Default is taken from column names.\n"
                     ) ),

      '-leg.position' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.position : Legend position. Choose from:\n",
   "                bottomright, bottom, bottomleft\n",
   "                left, topleft, top, topright, right,\n", 
   "                and center"
                     ) ),
      
      '-leg.fontsize' = apl(n = 1, d = 1.0, h = paste(
   "-leg.fontsize : fontsize for legend text.\n"
                     ) ),

      '-leg.ncol' = apl(n = 1, d = 4, h = paste(
   "-leg.ncol : Number of columns in legend.\n"
                     ) ),
                     
      '-leg.show' = apl(n = 0, d = FALSE, h = paste(
   "-leg.show : Show legend.\n"
                     ) ),
                     
      '-NoZeros' = apl (n = 0, d = FALSE, h = paste (
   "-NoZeros:  Do not plot all zeros time series"
                        ) ),
      '-Zeros' = apl (n = 0, d = TRUE, h = paste (
   "-Zeros:  Do  plot all zeros time series"
                        ) ),
                        
      '-col.ystack' = apl (n = 0, d = TRUE, h = paste (
   "-col.ystack:  Scale each column and offset it based on its",
   "               column index. This is useful for stacking",
   "               a large number of columns on one plot."
                        ) ),
                        
      '-Nocol.ystack' = apl (n = 0, d = FALSE, h = paste (
   "-Nocol.ystack:  "
                        ) ),
      
      '-oneplot' = apl (n = 0, d = FALSE, h = paste (
   "-oneplot:  "
                        ) ),   
                        
      '-one' = apl (n = 0, d = FALSE, h = paste (
   "-one:  "
                        ) ),                                     
      '-col.grp' = apl(n = c(1, Inf), h = paste (
   "-col.grp 1Dfile or Rexp: integer labels defining column belonging\n",
   "                For example: 1D\n"   
                  ) ), 
      '-col.text.lym' = apl(n = c(1, Inf), h = paste (
   "-col.text.lym 1Dfile or Rexp: integer labels defining column belonging\n",
   "                For example: 1D\n"   
                  ) ), 
      '-col.text.rym' = apl(n = c(1, Inf), h = paste (
   "-col.text.rym 1Dfile or Rexp: integer labels defining column belonging\n",
   "                For example: 1D\n"   
                  ) ), 
      '-xax.lim' = apl(n = c(2,3), h = paste (
   "-xax.lim\n",
   "                For example: 1D\n"   
                  ) ), 
      '-yax.lim' = apl(n = c(2,3), h = paste (
   "-yax.lim\n",
   "                For example: 1D\n"   
                  ) ),  
      '-addavg' = apl (n = 0, d = FALSE, h = paste (
   "-addavg:  "
                        ) ),              
      
      '-xax.label' = apl(n = 1, h = paste (
   "-xax.label\n",
   "                For example: 1D\n"   
                  ) ), 
      '-xax.tic.text' = apl(n = c(1, Inf), h = paste (
   "-xax.tic.text\n",
   "                For example: 1D\n"   
                  ) ), 
      '-yax.label' = apl(n = 1, h = paste (
   "-yax.label\n",
   "                For example: 1D\n"   
                  ) ), 
      '-yax.tic.text' = apl(n = c(1, Inf), h = paste (
   "-yax.tic.text\n",
   "                For example: 1D\n"   
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
      errex.AFNI('Error parsing arguments. See 1dRplot -help for details.');
   }
   
   #Parse dems options
   #initialize with defaults
   lop <- init.1DRplot.lop()

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      
      switch(opname,
             input = lop$dmat <- ops[[i]],
             input_delta = lop$dmat.err <- ops[[i]],
             x = lop$dmat.xval <- parse.1dRplot.Groups(ops[[i]]),
             prefix = lop$prefix  <- ops[[i]],
             save = {lop$prefix <- ops[[i]]; lop$nodisp=TRUE;} ,
             NoZeros = lop$col.nozeros <- TRUE,
             Zeros = lop$col.nozeros <- FALSE,
             oneplot = lop$oneplot <- TRUE,
             one = lop$oneplot <- TRUE,
             col.grp = 
               lop$col.grp <- parse.1dRplot.Groups(ops[[i]]),
             Nocol.ystack = lop$col.ystack <- FALSE,
             col.ystack = lop$col.ystack <- TRUE,
             grp.labels = lop$grp.labels <- ops[[i]],
             xlim = lop$xax.lim <- ops[[i]],
             ylim = lop$yax.lim <- ops[[i]],
             addavg = lop$col.mean.line <- TRUE,
             title  = lop$ttl.main <- ops[[i]],
             xlabel  = lop$xax.label <- ops[[i]],
             xtext = lop$xax.tic.text <- ops[[i]],
             ylabel  = lop$yax.label <- ops[[i]],
             ytext = lop$yax.tic.text <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             col.plot.char = lop$col.plot.char <- parse.1dRplot.Groups(ops[[i]]),
             col.colors = lop$col.colors <- parse.1dRplot.Groups(ops[[i]]),
             col.line.types = lop$col.line.type <- 
                                    parse.1dRplot.Groups(ops[[i]]),
             col.names = lop$col.names <- ops[[i]],
             col.text.lym = lop$col.text.lym <- ops[[i]],
             col.text.rym = lop$col.text.rym <- ops[[i]],
             leg.show = lop$leg.show <- TRUE,
             leg.names = lop$leg.names <- ops[[i]],
             leg.position = lop$leg.position <- ops[[i]],
             leg.fontsize = lop$leg.fontsize <- ops[[i]],
             leg.ncol = lop$leg.ncol <- ops[[i]],
             help = help.1dRplot.opts(params, adieu=TRUE),
             show_allowed_options = show.AFNI.args(ops, verb=0, 
                                              hstr="1dRplot's",adieu=TRUE)
             )
   }


   return(lop)
}# end of read.1dRplot.opts.batch

#Change options list to 1dRplot variable list 
process.1dRplot.opts <- function (lop, verb = 0) {
   return(lop)
}





#################################################################################
########################## Begin 1dRplot main ###################################
#################################################################################


   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))  
      save(args, rfile, file=".1dRplot.dbg.AFNI.args", ascii = TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      err.AFNI("No parameters");
   } else {
      if (!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }  
      if (is.null(lop <- read.1dRplot.opts.batch(args, verb = 0))) {
         stop('Error parsing input');
      }
      #str(lop);
      if (is.null(lop <- process.1dRplot.opts(lop, verb = lop$verb))) {
         stop('Error processing input');
      }
   }
   if (lop$verb) { 
      str(lop);
   }      

   thisplot <- plot.1D( dmat = lop$dmat, dmat.err=lop$dmat.err,
            dmat.colsel = lop$dmat.colsel,  
            col.nozeros = lop$col.nozeros, 
            col.grp = lop$col.grp,
            col.ystack = lop$col.ystack,
            grp.labels = lop$grp.labels,
            ttl.main = lop$ttl.main, 
            prefix = lop$pprefix, 
            nodisp = lop$nodisp,
            oneplot = lop$oneplot,
            col.mean.line = lop$col.mean.line,
            xax.lim=lop$xax.lim, xax.tic.text = lop$xax.tic.text,
            yax.lim=lop$yax.lim, yax.tic.text = lop$yax.tic.text,
            xax.label=lop$xax.label,
            yax.label=lop$yax.label, 
            col.plot.char=lop$col.plot.char,
            col.colors = lop$col.colors,
            col.line.type = lop$col.line.types,
            col.names = lop$col.names,
            col.text.lym = lop$col.text.lym,
            col.text.rym = lop$col.text.rym,
            leg.show = lop$leg.show,
            leg.names = lop$leg.names,
            leg.position = lop$leg.position,
            leg.fontsize = lop$leg.fontsize,
            leg.ncol = lop$leg.ncol,
            dmat.xval = lop$dmat.xval)

   if (BATCH_MODE) { #do not quit until device is closed
      while (length(which(dev.list()==thisplot))) Sys.sleep(0.25);
   }
   

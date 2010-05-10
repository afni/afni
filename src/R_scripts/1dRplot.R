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
               -ColumnGroups 'R:c(rep(1,10), rep(2,10), rep(3,10))' \\
               -GroupLabels CSF GM WM \\
               -NoZeros -NoOffsetBase \\
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
   lop <- list()
   lop$ff = NULL;
   lop$ffdelta = NULL;
   lop$x = NULL;
   lop$isel=NULL;
   lop$NoZeros=FALSE;
   lop$ColumnGroups=NULL;
   lop$OffsetBase=TRUE;
   lop$GroupLabels=NULL;
   lop$Title=NULL
   lop$CloseAfterSave=FALSE
   lop$OneSubplot = FALSE
   lop$xlim=NULL
   lop$ylim=NULL
   lop$addavg=FALSE
   lop$verb=0
   lop$xlabel=NULL
   lop$ylabel=NULL
   lop$ColumnSymbs=NULL
   lop$ColumnCols=NULL
   lop$ColumnNames=NULL
   lop$LegendNames=NULL
   lop$LegendPosition="topright"
   lop$ltypes=NULL
   return(lop)
}

#Change command line arguments into an options list
read.1dRplot.opts.batch <- function (args=NULL, verb = 0) {
   
   params <- list (
      '-input' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input 1Dfile: file to plot\n"
                     ) ),
                     
      '-input_delta' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input_delta 1Dfile: file containing value for error bars\n"
                     ) ),
                     
      '-x' = apl(n = c(1), h = paste (
   "-x 1Dfile or Rexp: x axis\n",
   "                For example: 1D\n"   
                  ) ), 

      '-save' = apl(n = 1, d = NA,  h = paste(
   "-save SAVE_PREFIX: Save plot and quit\n"
                     ) ),
                     
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output prefix (just prefix, no view+suffix needed)\n"
                     ) ),

      '-title' = apl(n = 1, d = NULL,  h = paste(
   "-title TITLE: Graph title\n"
                     ) ),
            
      '-GroupLabels' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-GroupLabels GROUP1 [GROUP2]: Labels to column grouping.\n",
   "                         Default is no labeling\n"
                     ) ),
      '-ColumnSymbs' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-ColumnSymbs : Symbols for each column.\n"
                     ) ),
                     
      '-ColumnCols' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-ColumnCols : Symbols for each column.\n"
                     ) ),
                     
      '-ColumnLtypes' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-ColumnLtypes : Symbols for each column.\n"
                     ) ),
                     
      '-ColumnNames' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-ColumnNames : Symbols for each column.\n"
                     ) ),

      '-LegendNames' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-LegendNames : Symbols for each column.\n"
                     ) ),

      '-LegendPosition' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-LegendPosition : Symbols for each column.\n"
                     ) ),
                     
      '-NoZeros' = apl (n = 0, d = FALSE, h = paste (
   "-NoZeros:  Do not plot all zeros time series"
                        ) ),
      '-Zeros' = apl (n = 0, d = TRUE, h = paste (
   "-Zeros:  Do  plot all zeros time series"
                        ) ),
                        
      '-OffsetBase' = apl (n = 0, d = TRUE, h = paste (
   "-OffsetBase:  "
                        ) ),
      '-NoOffsetBase' = apl (n = 0, d = FALSE, h = paste (
   "-NoOffsetBase:  "
                        ) ),
      
      '-OneSubplot' = apl (n = 0, d = FALSE, h = paste (
   "-OneSubplot:  "
                        ) ),   
                        
      '-one' = apl (n = 0, d = FALSE, h = paste (
   "-one:  "
                        ) ),                                     
      '-ColumnGroups' = apl(n = c(1), h = paste (
   "-ColumnGroups 1Dfile or Rexp: integer labels defining column belonging\n",
   "                For example: 1D\n"   
                  ) ), 
      '-xlim' = apl(n = c(2,3), h = paste (
   "-xlim\n",
   "                For example: 1D\n"   
                  ) ), 
      '-ylim' = apl(n = c(2,3), h = paste (
   "-ylim\n",
   "                For example: 1D\n"   
                  ) ),  
      '-addavg' = apl (n = 0, d = FALSE, h = paste (
   "-addavg:  "
                        ) ),              
      
      '-xlabel' = apl(n = 1, h = paste (
   "-xlabel\n",
   "                For example: 1D\n"   
                  ) ), 
      '-ylabel' = apl(n = 1, h = paste (
   "-ylabel\n",
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
             input = lop$ff <- ops[[i]],
             input_delta = lop$ffdelta <- ops[[i]],
             x = lop$x <- parse.1dRplot.Groups(ops[[i]]),
             prefix = lop$prefix  <- ops[[i]],
             save = {lop$prefix <- ops[[i]]; lop$CloseAfterSave=TRUE;} ,
             NoZeros = lop$NoZeros <- TRUE,
             Zeros = lop$NoZeros <- FALSE,
             OneSubplot = lop$OneSubplot <- TRUE,
             one = lop$OneSubplot <- TRUE,
             ColumnGroups = 
               lop$ColumnGroups <- parse.1dRplot.Groups(ops[[i]]),
             NoOffsetBase = lop$OffsetBase <- FALSE,
             OffsetBase = lop$OffsetBase <- TRUE,
             GroupLabels = lop$GroupLabels <- ops[[i]],
             xlim = lop$xlim <- ops[[i]],
             ylim = lop$ylim <- ops[[i]],
             addavg = lop$addavg <- TRUE,
             title  = lop$Title <- ops[[i]],
             xlabel  = lop$xlabel <- ops[[i]],
             ylabel  = lop$ylabel <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             ColumnSymbs = lop$ColumnSymbs <- parse.1dRplot.Groups(ops[[i]]),
             ColumnCols = lop$ColumnCols <- parse.1dRplot.Groups(ops[[i]]),
             ColumnLtypes = lop$ColumnLtypes <- parse.1dRplot.Groups(ops[[i]]),
             ColumnNames = lop$ColumnNames <- ops[[i]],
             LegendNames = lop$LegendNames <- ops[[i]],
             LegendPosition = lop$LegendPosition <- ops[[i]],
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
      save(args, file=".1dRplot.dbg.AFNI.args", ascii = TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      lop <- init.1DRplot.lop()
      lop$ff = c('all.sc5.mMs.tr_s051114.tfs1.DSC.dice.03302317.1D',
                 'all.sc5.mMs.tr_s051114.tfs1.DSC.dice.03302317.1D')
      lop$NoZeros = TRUE
      lop$ColumnGroups = c(rep(1,10), rep(2,10), rep(3,10))
      lop$OffsetBase = FALSE
      lop$GroupLabels = c('CSF','GM','WM')
      lop$CloseAfterSave = FALSE
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
   thisplot <- plot.1D( ff = lop$ff, ffd=lop$ffdelta,
            isel = lop$isel, descr = "",  
            NoZeros = lop$NoZeros, 
            ColumnGroups = lop$ColumnGroups,
            OffsetBase = lop$OffsetBase,
            GroupLabels = lop$GroupLabels,
            Title = lop$Title, 
            prefix = lop$prefix, 
            CloseAfterSave = lop$CloseAfterSave,
            OneSubplot = lop$OneSubplot,
            addmean = lop$addavg,
            xlim=lop$xlim,
            ylim=lop$ylim,
            xlabel=lop$xlabel,
            ylabel=lop$ylabel, 
            symbs=lop$ColumnSymbs,
            cols = lop$ColumnCols,
            ltypes = lop$ColumnLtypes,
            cnames = lop$ColumnNames,
            leg.names = lop$LegendNames,
            leg.position = lop$LegendPosition,
            x = lop$x)

   if (BATCH_MODE) { #do not quit until device is closed
      while (length(which(dev.list()==thisplot))) Sys.sleep(0.25);
   }
   

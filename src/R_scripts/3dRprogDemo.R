#!/usr/bin/env AFNI_Batch_R

#Clean up
rm(list = ls())

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))

#Make sure you set this variable to your own program name. 
#Do not include the .R part
ExecName <- '3dRprogDemo'

#################################################################################
################### Begin RprogDemo Input functions #############################
#################################################################################

greeting.RprogDemo <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 3dRprogDemo ==================          
          A sample program illustrating AFNI API to read/write volumes
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.0, Apr. 15th 2014
Author: Ziad S. Saad (saadz@mail.nih.gov)
Website - https://afni.nimh.nih.gov
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      

#The help function for 3dRprogDemo batch (command line mode)
help.RprogDemo.opts <- function (params, alpha = TRUE, 
                                 itspace='   ', adieu=FALSE, targ ='TXT') {

   intro <- 
'
Usage:
------ 
 3dRprogDemo is a template program to help users write their own R
 processing routines on MRI volumes without having to deal with
 things like volume I/O or command line argument parsing.
 
 This template program shows rudimentary command line option parsing,
 volume reading, calling a silly processing function on each voxel time series,
 and writing the output. 
 
 This 3dRprogDemo.R file is paired with the script 3dRprogDemo which
 allows users to run R programs directlt from the shell. To create your
 own 3dSOMETHING program you would do at least the following:
 
 cp 3dRprogDemo.R 3dSOMETHING.R 
 cp 3dRprogDemo 3dSOMETHING
 Modify the variable ExecName in 3dSOMETHING.R to reflect your program name
 Replace the function  RprogDemo.Scale() with your own function 

 Unfortunately at this stage, there is little help for the AFNI R API
 beyond this sample code. If you find yourself using this and need
 to ask questions about other dataset utility functions contact the author 
 for help. The AFNIio.R file in the AFNI distribution contains most of the IO
 functions. Below are some notable ones, grep for them in the .R files for 
 usage examples.
   
   dset.attr() for getting and setting attributes, such as the TR in seconds
               e.g. dset$NI_head <- dset.attr(dset$NI_head, "TR", val = 1.5)
   read.AFNI()
   write.AFNI()
   show.dset.attr()
   dset.index3Dto1D()
   dset.index1Dto3D()
   dset.dimBRKarray()
   dset.3DBRKarrayto1D()
   dset.1DBRKarrayto3D()
   
   parse.AFNI.name() for parsing a filename into AFNI relevant parameters
   exists.AFNI.name()
   note.AFNI(), err.AFNI(), warn.AFNI(), exit.AFNI()
      
 Debugging Note:
 ===============
 When running the program from the shell prompt, you cannot use R\'s
 browser() function to halt execution and step through the code.
 However, the utility function load.debug.AFNI.args() makes it very easy
 for you to run the command line equivalent from the R prompt. Doing so 
 would make available the browser() functionality. To use load.debug.AFNI.args()
 follow these steps: 
 1- Run the program from the shell command line. The program will
 automatically create a hidden file called .YOUR_PROGRAM_NAME.dbg.AFNI.args
 2- Start R from the same directory or change to the directory where 
 you ran the program if you started R elesewhere
 3- Run the function:  load.debug.AFNI.args() and follow the prompts.
 The function will look for possible debug files, prompt you to pick
 the one you want, and start the execution from the R shell.
'
   
   ex1 <- 
"
Example 1 --- Read a dataset, scale it, then write the results:
-----------------------------------------------------------------------------
      3dRprogDemo       -input epi.nii    \
                        -mask mask.nii    \
                        -scale 7          \
                        -prefix toy.nii

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


#Change command line arguments into an options list
read.RprogDemo.opts.batch <- function (args=NULL, verb = 0) {
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
                     
      '-verb' = apl(n=1, d = 0, h = paste(
   "-verb VERB: VERB is an integer specifying verbosity level.\n",
   "            0 for quiet (Default). 1 or more: talkative.\n"
                        ) ),
                        
      '-help' = apl(n=0, h = '-help: this help message, in simple text.\n'),
      '-h_raw' = apl(n=0, h = '-h_raw: this help message, as is in the code.\n'),
      '-h_txt' = apl(n=0, h = '-h_txt: this help message, in simple text\n'),
      '-h_spx' = apl(n=0, h = '-h_spx: this help message, in sphinx format\n'),
      '-h_aspx' = apl(n=0, h = '-h_aspx: like -h_spx, with autolabeling\n'),
      
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )
   
         );
                     
   ops <- parse.AFNI.args(args,  params,
                          other_ok=FALSE
                          )
   if (verb) show.AFNI.args(ops, verb=0, hstr='');
   if (is.null(ops)) {
      errex.AFNI('Error parsing arguments. See 3dRprogDemo -help for details.');
   }
   
   #initialize with defaults
   com_history <-AFNI.command.history(ExecName, args,NULL)
   lop <- AFNI.new.options.list(history = com_history, parsed_args = ops)
   lop$input <- NULL
   lop$prefix <- 'RprogDemo'
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
             help = help.RprogDemo.opts(params, adieu=TRUE),
             h_raw = help.RprogDemo.opts(params, adieu=TRUE, targ='RAW'),
             h_spx = help.RprogDemo.opts(params, adieu=TRUE, targ='SPX'),
             h_aspx = help.RprogDemo.opts(params, adieu=TRUE, targ='ASPX'),
             h_txt = help.RprogDemo.opts(params, adieu=TRUE, targ='TXT'),
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
}# end of read.RprogDemo.opts.batch


#################################################################################
############### Begin RprogDemo Computation functions ###########################
#################################################################################


# The big function
RprogDemo.Scale <- function( idset=NULL, mdset = NULL, scale = 2) {
   #Create an output volume (you can also overwrite the input)
   brk <- array(0, dim(idset$brk))

   #Loop over each voxel and set the values
   for (ivox in 1:dset.dimBRKarray(idset)[1]) {
      if (is.null(mdset) || mdset$brk[ivox,1]) {
         brk[ivox,] = idset$brk[ivox,] * scale;      
      }
   }      

   return(brk);
}



#################################################################################
######################## Begin RprogDemo main ##################################
#################################################################################


if (!exists('.DBG_args')) { 
   args = (commandArgs(TRUE))  
   rfile <- first.in.path(sprintf('%s.R',ExecName))  
   # save only on -dbg_args          28 Apr 2016 [rickr]
   if ( '-dbg_args' %in% args ) {
      save(args, rfile, file=sprintf("%s.dbg.AFNI.args", ExecName), ascii = TRUE) 
   }
} else {
   note.AFNI("Using .DBG_args resident in workspace");
   args <- .DBG_args
}
if (!length(args)) {
   errex.AFNI('Error parsing interactive input');
} else {
   if (!exists('.DBG_args')) {
      BATCH_MODE <<- 1
   } else {
      BATCH_MODE <<- 0
   }  
   if (is.null(lop <- read.RprogDemo.opts.batch(args, verb = 0))) {
      errex.AFNI('Error parsing input');
   }
   #str(lop);
}
   
if (lop$verb > 1) { 
   str(lop);
}

 
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
   brk <- RprogDemo.Scale(idset, mdset, lop$scale);
         
   #Reshape brk back to 3D mode
   dim(brk) <- ddi

   if (length(lop$input) > 1) {
      oname <- sprintf("%s.%02d", lop$prefix, iin);
   } else {
      oname <- lop$prefix
   }
   if (lop$verb) 
      note.AFNI ( paste('Writing results to', oname, '\n'));
   dout <- write.AFNI(oname, brk, defhead=idset, com_hist = lop$com_history, 
              overwrite = lop$overwrite);
}

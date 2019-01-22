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
ExecName <- 'rPkgsInstall'


greeting.1dRplot <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to rPkgsInstall.R ==================          
            A program to install/update/remove R packages required by various AFNI programs
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      
reference.rPkgsInstall <- function ()
   return(
""
   )
examples.rPkgsInstall <- function () {
   return("
To install one or more R packages:
-----------------------------------------------------------

")
}

#The help function for rPkgsInstall batch (command line mode)
help.rPkgsInstall.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <-
'
          ================== Welcome to rPkgsInstall ==================          
                     Install/update/remove R packages for AFNI
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.2, April 18, 2018
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/sscc/gangc
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 rPkgsInstall is a program for installing, checking, updating, or removing any
 R packages. It conveniently runs on the shell terminal instead of the R prompt.
 Check out the examples below or the option specifications for usage details.'

   ex1 <- 
"\n--------------------------------
Example 1 --- Install all the R packages that are currently required in 
AFNI programs:
   rPkgsInstall -pkgs ALL
   rPkgsInstall -pkgs ALL -site 'http://cran.us.r-project.org'\n"

   ex2 <- 
"\n--------------------------------
Example 2 --- Install user-specified R packages:
   rPkgsInstall -pkgs 'gsl'
   rPkgsInstall -pkgs 'afex,phia,paran'
   rPkgsInstall -pkgs 'snow,nlme,psych' -site 'http://cran.us.r-project.org'\n"

   ex3 <- 
"\n--------------------------------
Example 3 --- check/update/remove R packages:
   rPkgsInstall -pkgs ALL -check
   rPkgsInstall -pkgs ALL -update
   rPkgsInstall -pkgs ALL -remove
   rPkgsInstall -pkgs ALL -update -site 'http://cran.fhcrc.org/'
   rPkgsInstall -pkgs 'lme4,pixmap,plotrix' -check
   rPkgsInstall -pkgs 'afex,phia,paran' -update
   rPkgsInstall -pkgs 'boot' -remove
   rPkgsInstall -pkgs 'snow,nlme,vars' -update -site 'http://cran.cs.wwu.edu/'\n\n"

   parnames <- names(params)
   ss <- vector('character')
   if(alpha) {
       parnames <- sort(parnames)   
       ss <- paste('Options in alphabetical order:\n',
                  '==============================\n', sep='')
   } else ss <- paste('Options:\n', '--------\n', sep='')
   for(ii in 1:length(parnames)) {
      op <- params[parnames[ii]][[1]]
      if(!is.null(op$help)) ss <- c(ss , paste(itspace, op$help, sep='')) else
         ss <- c(ss, paste(itspace, parnames[ii], '(no help available)\n', sep='')) 
   }
   ss <- paste(ss, sep='\n')
   cat(intro, ex1, ex2, ex3, ss, sep='\n')
   
   if (adieu) exit.AFNI();
}

#Change command line arguments into an options list
read.rPkgsInstall.opts.batch <- function (args=NULL, verb = 0) {
   #browser()
   params <- list (
      '-pkgs' = apl(n = c(1, 40), d = NA,  h = paste(
   "-pkgs package_list: List all the packages that you would like to install,",
   "         update or move. This option is required for installation, update,",
   "         or removal. The absence of both options -update and -remove means",
   "         installation. The package names should be separated with comma (,)",
   "         without any other characters such as spaces, and should be surrounded",
   "         within single/double quotes. For example, -pkgs \"afex,phia\". If",
   "         package_list is set as ALL, all the following packages required for",
   "         AFNI programs will be installed, updated, or removed:\n",
   "         'afex', 'phia', 'snow', 'nlme', 'lme4', 'paran', 'psych'.\n",
   "         You can use rPkgsInstall to install, update, or remove any R packages,",
   "         and they do not have to be in the list above. \n", sep = '\n'
             ) ),

      '-check' = apl(n = 0,  h = paste(
   "-check: This option verifies whether all or the user-specified R packages",
   "         listed in option -pkgs are installed on the computer, but it does not",
   "         install/update/remove the packages.\n",sep = '\n'
                     ) ),

                   
      '-update' = apl(n = 0,  h = paste(
   "-update: This option indicates that all or the user-specified R packages in AFNI",
   "         will be updated. The absence of the option (default) means no updating.",
   "         A package specified in '-pkgs package_list' that has not been installed on",
   "         the computer will be installed under this option.",
   "         WARNING: Updating some R packages may require that R be ugraded to the",
   "                  most recent version. \n",sep = '\n'
                     ) ),

      '-remove' = apl(n = 0,  h = paste(
   "-remove: This option indicates that all or the user-specified R packages in AFNI",
   "         will be purged from your computer. The absence of the option (default)",
   "         means installing or updating, but no removing. \n",sep = '\n'
                     ) ),
                     
      '-site' = apl(n = c(1, 1), d = NA,  h = paste(
   "-site download_website: You can specify the package repository website within",
   "        single/double quotes. The current sites can be found at\n",
   "        http://cran.r-project.org/mirrors.html\n",
   "        The default is 'http://cran.us.r-project.org'",
   "        University, Houghton, MI.\n",sep = '\n' 
                     ) ),
                                      
      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )
         );
                     #browser()
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   ops$other <- NULL
   ops$allowed_options <- NULL
   if (verb) show.AFNI.args(ops, verb=verb-1, hstr='');
   if (is.null(ops)) {
      errex.AFNI('Error parsing arguments. See rPkgsInstall -help for details.');
   }
   
   #Parse dems options
   #initialize with defaults
   com_history<-AFNI.command.history(ExecName, args,NULL)
   lop <- list (com_history = com_history)
   lop$pkgs   <- NA
   lop$update <- 0
   lop$check <- 0
   lop$remove  <- 0
   lop$site   <- 'http://cran.us.r-project.org'
   lop$verb   <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      
      switch(opname,
             pkgs   = lop$pkgs   <- ops[[i]],
             check  = lop$check  <- TRUE,
             update = lop$update <- TRUE,
             remove = lop$remove <- TRUE,
             site   = lop$site   <- ops[[i]],
             help  = help.rPkgsInstall.opts(params, adieu=TRUE),
             #errex.AFNI(paste("Option '", opname,"' not recognized", sep=''))  
             )
   }
   return(lop)
}# end of read.rPkgsInstall.opts.batch

#Change options list to rPkgsInstall variable list 
process.rPkgsInstall.opts <- function (lop, verb = 0) {
   #browser()
   if(is.na(lop$pkgs[1])) errex.AFNI(paste("Option '-pkgs' not specified!", sep=''))
   if(lop$pkgs[1]=='ALL') lop$PKGS <- c('afex', 'phia', 'snow', 'nlme', 'lme4','paran', 'psych', 'brms') else
   if(!is.na(lop$pkgs[1])) lop$PKGS <- strsplit(lop$pkgs, '\\,')[[1]]
   return(lop)
}

getPkgs <- function(PKGS, check=0, update=0, remove=0, site='http://cran.us.r-project.org') {    
   pkgs_miss <- PKGS[which(!PKGS %in% installed.packages()[, 1])]
   if(check) {
      if(length(pkgs_miss) > 0) warn.AFNI(paste("These packages are not installed on the computer: ", pkgs_miss, '!\n', sep='')) else 
         note.AFNI(paste("\tThis package has been verified on the computer: ", PKGS, "\n", sep=''))
   } else {
      pkgs_hit  <- PKGS[which(PKGS %in% installed.packages()[, 1])]
      if((length(pkgs_miss) > 0) & !remove) {
         install.packages(pkgs_miss, dep=TRUE, repos=site)
         note.AFNI(paste("\tThis package has been installed on the computer: ", pkgs_miss, "\n", sep=''))
     }
      if(update) {
         update.packages(pkgs_hit, repos=site)
         note.AFNI(paste("\tThis package has been updated on the computer: ", pkgs_hit, "\n", sep=''))
      }  
      if(remove) {
         remove.packages(pkgs_hit)
         note.AFNI(paste("\tThis package has been removed from the computer: ", pkgs_hit, "\n", sep=''))
     }
   }   
}



#################################################################################
####################### Begin rPkgsInstall main ##################################
#################################################################################


   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))  
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbg_args' %in% args ) {
         save(args, rfile, file="rPkgsInstall.dbg.AFNI.args", ascii = TRUE) 
      }
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
      if (is.null(lop <- read.rPkgsInstall.opts.batch(args, verb = 0))) {
         stop('Error parsing input');
      }
      if (is.null(lop <- process.rPkgsInstall.opts(lop, verb = lop$verb))) {
         stop('Error processing input');
      }
   }
   if (lop$verb) { 
      str(lop);
   }

   getPkgs(lop$PKGS, lop$check, lop$update, lop$remove, lop$site)

#!/usr/bin/env AFNI_Batch_R
##!/usr/bin/env afni_run_R

# Command line to run this script: TRR.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- 'TRR'

# Global variables

#################################################################################
##################### Begin TRR Input functions ################################
#################################################################################

#The help function for TRR batch (AFNI-style script mode)
help.TRR.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
                      Welcome to TRR ~1~
    Test-Retest Reliability Program through Bayesian Multilevel Modeling 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.4, Febarch 13, 2022 
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
 TRR performs test-rest reliability analysis for behavior data as well as 
 region-based neuroimaging data. If no multiple trials are involved in a
 dataset, use the conventional intraclass correlation (ICC) with, for 
 example, 3dICC for neuroimaging data. However, when there are multiple 
 trials for each condition, the traditional intraclass correlation may 
 underestimate TRR to various extent. 3dLMEr could be utilized with the
 option -TRR to estimate test-retest reliability with trial-level data for 
 whole-brain analysis; however, it may only work for data with strong 
 effects such as a single effect (e.g., one condition or average across 
 conditions). 

 The input data for the program TRR have to be at the trial level without 
 any summarization at the condition level. The TRR estimation is conducted 
 through a Byesian multilevel model with a shell script (as shown in the 
 examples below). The input data should be formulated in a pure-text table 
 that codes all the variables.

 Citation: ~1~
 If you want to cite the modeling approach for TRR, consider the following:~2~

 Chen G, et al., Beyond the intraclass correlation: A hierarchical modeling
 approach to test-retest assessment.
 https://www.biorxiv.org/content/10.1101/2021.01.04.425305v1

=============================== 
 Read the following carefully!
 ===============================
 A data table in pure text format is needed as input for an TRR script. The
 data table should contain at least 3 (with a single condition) or 4 (with
 two conditions) columns that specify the information about subjects, 
 sessions and response variable values:

 Subj   session    Y     
 S1      T1     0.2643  
 S1      T2     0.3762 
 ...

 Subj condition  session    Y     
 S1    happy      T1     0.2643  
 S1    happy      T2     0.3762 
 S1     sad       T1     0.3211 
 S1     sad       T2     0.3341
 ...
 

 0) Through Bayesian analysis, a whole TRR distribution will be presented in
    the end as a density plot in PDF. In addition, the distribution is 
    summarized with a mode (peak) and a highest density interval that are
    stored in a text file with a name specified through -prefix with the 
    appendix .txt.

 1) Avoid using pure numbers to code the labels for categorical variables. The
    column order does not matter. You can specify those column names as you
    prefer, but it saves a little bit scripting if you adopt the default naming
    for subjects (\'Subj\'), sessions (\'sess\') and response variable (\'Y\').

 2) Sampling error for the trial-level effects can be incorporated into the 
    model. This is especially applicable to neuroimaging data where the trial
    level effects are typically estimated through time series regression with
    GLS (e.g., 3dREMLfit in AFNI); thus, the standard error or t-statistic can
    be provided as part of the input through an extra column in the data table
    and through the option -se in the TRR script.

 3) If there are more than 4 CPUs available, one could take advantage of within
    chain parallelization through the option -WCP. However, extra stpes are 
    required: both \'cmdstan\' and \'cmdstanr\' have to be installed. To install
    \'cmdstanr\', execute the following command in R:

    install.packages(\'cmdstanr\', repos = c(\'https://mc-stan.org/r-packages/\', getOption(\'repos\')))

    Then install \'cmdstan\' using the following command in R:

    cmdstanr::install_cmdstan(cores = 2)

 4) The results from TRR can be slightly different from each execution or 
    different computers and R package versions due to the nature of randomness 
    involved in Monte Carlo simulations, but the differences should be negligle
    unless numerical failure occurs.

 =========================
 
 Installation requirements: ~1~
 In addition to R installation, the R packages "brms", "coda" and "ggplot2" are 
 required for TRR. Make sure you have a recent version of R. To install these
 packages, run the following command at the terminal:

 rPkgsInstall -pkgs "brms,coda,ggplot2" -site http://cran.us.r-project.org"

 Alternatively you may install them in R:
 install.packages("brms")
 install.packages("coda")
 install.packages("ggplot2")

 To take full advantage of parallelization, install both \'cmdstan\' and \'cmdstanr\'
 and use the option -WCP in TRR (see comments above).

 Running: ~1~
 Once the TRR command script is constructed saved as a text file, for example,
 called myTRR.txt, execute it with the following (assuming on tcsh shell),
 
 nohup tcsh -x myTRR.txt > diary.txt &
 nohup tcsh -x myTRR.txt |& tee diary.txt &

 The progression of the analysis is stored in the text file diary.txt and can
 be examined later. The \'nohup\' command allows the analysis running in the 
 background even if the terminal is killed.'
 
   ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- TRR estimation for a single effect - simple scenario: one 
          condition, two sessions. Trial level effects are the input 
          from each subject, and test-retest reliability between two sessions is 
          the research focus.

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -dataTable myData.tbl   \\

   If a computer is equipped with as many CPUs as a factor 4 (e.g., 8, 16, 24,
   ...), a speedup feature can be adopted through within-chain parallelization
   with the option -WCP. For example, the script assumes a 
   computer with 24 CPUs (6 CPUs per chain): 

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -WCP 6 -dataTable myData.tbl   \\

   If the data are skewed or have outliers, use exGaussian or Student's t:

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -distY exgaussian -dataTable myData.tbl   \\

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -distY student -dataTable myData.tbl   \\

   The input file 'myData.txt' is a data table in pure text format as below: 
                                                             
     Subj  sess          Y
     S01   sess1      0.162
     S01   sess1      0.212
     ...
     S01   sess2     -0.598
     S01   sess2      0.327
     ...
     S02   sess1      0.249
     S02   sess1      0.568
     ...
 \n"         
     
   ex2 <-
"--------------------------------
Example 2 --- TRR estimation for a contrast between two conditions. Input
   data include trial-level effects for two conditions during two sessions.
    
   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -dataTable myData.tbl   \\

   A version with within-chain parallelization through option '-WCP 6' on a
   computer with 24 CPUs:

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -WCP 6 \\
      -dataTable myData.tbl   \\

   Another version with the assumption of student t-distribution:

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -distY student -dataTable myData.tbl   \\

   The input file 'myData.txt' is a data table in pure text format as below:

     Subj  sess   cond       Y
     S01   sess1  C1    0.162
     S01   sess1  C1    0.212
     ...
     S01   sess1  C2    0.262
     S01   sess1  C2    0.638
     ...
     S01   sess2  C1   -0.598
     S01   sess2  C1    0.327
     ...
     S01   sess2  C2    0.249
     S01   sess2  C2    0.568
     ...

\n"
     
   ex3 <-
"---------------------------------
Example 3 --- TRR estimation for a contrast between two conditions. Input
   data include trial-level effects plus their t-statistic or standard error
   values for two conditions during two sessions.
   
   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -tstat tvalue -dataTable myData.tbl   \\

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -se SE -dataTable myData.tbl   \\

   A version with within-chain parallelization through option '-WCP 6' on a
   computer with 24 CPUs:

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -tstat tvalue -WCP 6 \\
      -dataTable myData.tbl \\

   Another version with the assumption of Student t-distribution:

   TRR -prefix myTRR -chains 4 -iterations 1000 -Y RT -subject Subj \\
      -repetition sess -condition cond -tstat tvalue -distY student \\
      -dataTable myData.tbl   \\

   The input file 'myData.txt' is a data table in pure text format as below:

     Subj  sess  cond  tvalue     Y
     S01   sess1  C1    2.315   0.162
     S01   sess1  C1    3.212   0.341
     ...
     S01   sess1  C2    1.262   0.234
     S01   sess1  C2    0.638   0.518
     ...
     S01   sess2  C1   -2.598  -0.213
     S01   sess2  C1    3.327   0.423
     ...
     S01   sess2  C2    4.249   0.791
     S01   sess2  C2    3.568   0.351
     ...

   
   Options: ~1~
   \n"   
   
   parnames <- names(params)
   ss <- vector('character')
   if(alpha) {
       parnames <- sort(parnames)   
       ss <- paste('Options in alphabetical order:\n',
                  '------------------------------\n', sep='')
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
   
# options list 
read.TRR.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Prefix is used to specify output file names. The main output is",
   "        a text with prefix appended with .txt and stores inference information ",
   "        for effects of interest in a tabulated format depending on selected ",
   "        options. The prefix will also be used for other output files such as ",
   "        visualization plots, and saved R data in binary format. The .RData can",
   "        be used for post hoc processing such as customized processing and plotting.",
   "        Remove the .RData file to save disk space once you deem such a file is no",
   "        longer useful.\n", sep = '\n'
                     ) ),

      '-chains' = apl(n = 1, d = 1, h = paste(
   "-chains N: Specify the number of Markov chains. Make sure there are enough",
   "         processors available on the computer. Most of the time 4 cores are good",
   "         enough. However, a larger number of chains (e.g., 8, 12) may help achieve",
   "         higher accuracy for posterior distribution. Choose 1 for a single-processor",
   "         computer, which is only practical only for simple models.\n", sep = '\n'
                     ) ),

      '-iterations' = apl(n = 1, d = 1, h = paste(
   "-iterations N: Specify the number of iterations per Markov chain. Choose 1000 (default)",
   "         for simple models (e.g., one or no explanatory variables). If convergence",
   "         problem occurs as indicated by Rhat being great than 1.1, increase the number of",
   "         iterations (e.g., 2000) for complex models, which will lengthen the runtime.",
   "         Unfortunately there is no way to predict the optimum iterations ahead of time.\n", sep = '\n'
                     ) ),

      '-verb' = apl(n = 1, d = 1, h = paste(
   "-verb VERB: Speicify verbose level.\n", sep = '\n'
                     ) ),

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: This option specifies the effects associated with explanatory",
   "         variables. By default (without user input) the model is specified as",
   "         1 (Intercept). Currently only between-subjects factors (e.g., sex, ",
   "         patients vs. controls) and quantitative variables (e.g., age) are",
   "         allowed. When no between-subject factors are present, simply put 1",
   "         (default) for FORMULA. The expression FORMULA with more than one",
   "         variable has to be surrounded within (single or double) quotes (e.g.,",
   "         '1+sex', '1+sex+age'. Variable names in the formula should be consistent",
   "         with the ones used in the header of data table. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. Subject as a variable should not occur in",
   "         the model specification here.\n", sep = '\n'
             ) ),

      '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a file called",
   "         .TRR.dbg.AFNI.args in the current directory so that debugging can be",
   "         performed.\n", sep='\n')),

      '-se'  = apl(n = 1, d = 0, h = paste(
   "-se: This option indicates that standard error for the response variable is",
   "         available as input, and a column is designated for the standard error",
   "         in the data table. If effect estimates and their t-statistics are the",
   "         output from preceding analysis, standard errors can be obtained by",
   "         dividing the effect estimatrs ('betas') by their t-statistics. The",
   "         default assumes that standard error is not part of the input.\n", sep='\n')),

      '-WCP' = apl(n = 1, d = 1, h = paste(
   "-WCP k: This option will invoke within-chain parallelization to speed up runtime.",
   "         To take advantage of this feature, you need the following: 1) at least 8",
   "         or more CPUs; 2) install 'cmdstan'; 3) install 'cmdstanr'. The value 'k'",
   "         is the number of thread per chain that is requested. For example, with 4",
   "         chains on a computer with 24 CPUs, you can set 'k' to 6 so that each",
   "         chain will be assigned with 6 threads.\n", sep='\n')),

      '-cVars' = apl(n=c(1,100), d=NA, h = paste(
   "-cVars variable_list: Identify categorical (qualitive) variables (or",
   "         factors) with this option. The list with more than one variable",
   "         has to be separated with comma (,) without any other characters such",
   "         as spaces and should be surrounded within (single or double) quotes.",
   "         For example, -cVars \"sex,site\"\n",
             sep = '\n'
             ) ),

      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars variable_list: Identify quantitative variables (or covariates) with",
   "         this option. The list with more than one variable has to be",
   "         separated with comma (,) without any other characters such as",
   "         spaces and should be surrounded within (single or double) quotes.",
   "         For example, -qVars \"Age,IQ\"\n",
             sep = '\n'
             ) ),

      '-Y' = apl(n = 1, d = NA,  h = paste(
   "-Y var_name: var_name is used to specify the column name that is designated as",
   "        as the response/outcome variable. The default (when this option is not",
   "        invoked) is 'Y'.\n", sep = '\n'
                     ) ),

      '-subject' = apl(n = 1, d = NA,  h = paste(
   "-subject var_name: var_name is used to specify the column name that is",
   "        designated as for the subject variable. The default (when this option",
   "        is not invoked) is 'subj'.\n", sep = '\n'
                     ) ),

      '-repetition' = apl(n = 1, d = NA,  h = paste(
   "-repetition var_name: var_name is used to specify the column name that is",
   "        designated as for the repetition variable such as sess<ion. The default",
   "        (when this option is not invoked) is 'repetition'. Currently it only allows",
   "        two repetitions in a test-test scenario.\n", sep = '\n'
                     ) ),

      '-condition' = apl(n = 1, d = NA,  h = paste(
   "-condition var_name: var_name is used to specify the column name that is",
   "        designated as the condition variable. Currently TRR can only handle",
   "        two conditions. Note that when this option is not invoked, no",
   "        condition variable is assumed to be present, and the TRR analysis",
   "        will proceed with a singl effect instead of a contrast between two",
   "        conditions.\n", sep = '\n'
                     ) ),

      '-se' = apl(n = 1, d = NA,  h = paste(
   "-se var_name: var_name is used to specify the column name that lists",
   "        the standard errors, if available, for the response variable 'Y'.",
   "        In the case where t-statistic values are available for the effect",
   "        estiamtes of 'Y', use the option -tstat.\n", sep = '\n'
                     ) ),

      '-tstat' = apl(n = 1, d = NA,  h = paste(
   "-tstat var_name: var_name is used to specify the column name that lists",
   "        the t-statistic values, if available, for the response variable 'Y'.", 
   "        In the case where standard errors are available for the effect", 
   "        estiamtes of 'Y', use the option -se.\n", sep = '\n'
                     ) ),

      '-distY' = apl(n = 1, d = NA,  h = paste(
   "-distY distr_name: Use this option to specify the distribution for the response",
   "        variable. The default is Gaussian when this option is not invoked. When",
   "        skewness or outliers occur in the data, consider adopting the Student's",
   "        t-distribution, exGaussian, log-normal etc. by using this option with",
   "        'student', 'exgaussian', 'lognormal' and so on.\n", sep = '\n'
                     ) ),

      '-subject' = apl(n = 1, d = NA,  h = paste(
   "-subject var_name: var_name is used to specify the column name that is designated as",
   "        as the measuring unit variable (usually subject). The default (when this",
   "        option is not invoked) is 'Subj'.\n", sep = '\n'
                     ) ),

      '-PDP' = apl(n = 2, d = NA, h = paste(
   "-PDP width height: Specify the layout of posterior distribution plot (PDP) with",
   "         the size of the figure windown is specified through the two parameters of",
   "         width and height in inches.\n", sep = '\n'
                     ) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure in a table of long format (cf. wide",
   "         format) in R with a header as the first line. \n",
   "         NOTE:\n",
   "         1) There should have at least three columns in the table. These minimum",
   "         three columns can be in any order but with fixed and reserved with labels:",
   "         'Subj', 'ROI', and 'Y'. The column 'ROI' is meant to code the regions",
   "         that are associated with each value under the column Y. More columns can",
   "         be added in the table for explanatory variables (e.g., groups, age, site)",
   "         if applicable. Only subject-level (or between-subjects) explanatory variables",
   "         are allowed at the moment. The labels for the columns of 'Subj' and 'ROI'",
   "         can be any identifiable characters including numbers.",
   "         2) Each row is associated with one and only one 'Y' value, which is the",
   "         response variable in the table of long format (cf. wide format) as",
   "         defined in R. With n subjects and m regions, there should have totally mn",
   "         rows, assuming no missing data. ",
   "         3) It is fine to have variables (or columns) in the table that are not used",
   "         in the current analysis.",
   "         4) The context of the table can be saved as a separate file, e.g., called",
   "         table.txt. In the script specify the data with '-dataTable table.txt'.",
   "         This option is useful when: (a) there are many rows in the table so that",
   "         the program complains with an 'Arg list too long' error; (b) you want to",
   "         try different models with the same dataset.\n",
             sep = '\n'
                     ) ),

      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )
            )
                     
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   if (verb) show.AFNI.args(ops, verb=0, hstr='')
   if (is.null(ops)) 
      errex.AFNI('Error parsing arguments. See TRR -help for details.')

   #Parse dems options
   #initialize with defaults
      lop <- AFNI.new.options.list(history = '', parsed_args = ops)
      lop$chains <- 4
      lop$iterations <- 1000
      lop$model  <- 1
      lop$cVars  <- NULL
      lop$qVars  <- 'Intercept'
      lop$Y      <- 'Y'
      lop$subject<- 'subj'
      lop$distY  <- 'gaussian'
      lop$se     <- NULL
      lop$tstat  <- NULL
      lop$PDP    <- NULL
      lop$WCP    <- FALSE
      lop$repetition <- 'sess'
      lop$condition  <- NULL

      lop$dbgArgs <- FALSE # for debugging purpose
      lop$verb    <- 0
   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix     = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             chains     = lop$chains <- ops[[i]],
             iterations = lop$iterations <- ops[[i]],
             verb       = lop$verb   <- ops[[i]],
             model      = lop$model  <- ops[[i]],
             cVars      = lop$cVars  <- ops[[i]],
             qVars      = lop$qVars  <- ops[[i]],
             qContr     = lop$qContr <- ops[[i]],
             Y          = lop$Y      <- ops[[i]],
             subject    = lop$subject<- ops[[i]],
             distY      = lop$distY  <- ops[[i]],
             se         = lop$se     <- ops[[i]],
             tstat      = lop$tstat  <- ops[[i]],
             subject    = lop$subject   <- ops[[i]],
             PDP        = lop$PDP    <- ops[[i]],
	     WCP        = lop$WCP    <- ops[[i]],
             repetition = lop$repetition <- ops[[i]],
             condition  = lop$condition  <- ops[[i]],
             help       = help.TRR.opts(params, adieu=TRUE),
             dbgArgs    = lop$dbgArgs <- TRUE,
             dataTable  = lop$dataTable <- read.table(ops[[i]], header=T),
             )
   }

   return(lop)
}# end of read.TRR.opts.batch
                                               
                                               
#Change options list to TRR variable list 
process.TRR.opts <- function (lop, verb = 0) {
   if(is.null(lop$outFN)) errex.AFNI(c("Output filename not specified! Add filename with -prefix.\n"))
   an <- parse.AFNI.name(lop$outFN)
   if(!lop$overwrite && (
            file.exists(paste0(lop$outFN,".txt")) ||
            file.exists(paste0(lop$outFN,".RData")) ||
            file.exists(paste0(lop$outFN,".pdf"))) ) {
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
         return(NULL)
   }      

   if(!is.null(lop$cVars[1])) lop$CV <- strsplit(lop$cVars, '\\,')[[1]]   
   if(!is.na(lop$qVars[1])) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]

 
   if(lop$chains < 1) lop$chains <- 1

   return(lop)
}
# process.TRR.opts(lop, verb = lop$verb)                                               

#################################################################################
########################## Begin TRR main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
       # save only on -dbgArgs          28 Apr 2016 [rickr]
       if ('-dbgArgs' %in% args) try(save(args, rfile, file=".TRR.dbg.AFNI.args", ascii = TRUE), silent=TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.TRR(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.TRR.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.TRR.opts.batch(args, verb = 0)))
         stop('Error parsing input')                
      
      #str(lop);
      if(is.null(lop <- process.TRR.opts(lop, verb = lop$verb))) 
         stop('Error processing input')
      
   }
   #if(lop$verb > 1) { 
      #Too much output, big dump of header structs of input dsets..
  #   str(lop)
   #}


########################################################################

library("brms")

# write data.frame to a file
outDF <- function(DF, fl) cat(capture.output(DF), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)

# standardize the names for Y, repetition, condition and subject
names(lop$dataTable)[names(lop$dataTable)==lop$subject] <- 'subj'
names(lop$dataTable)[names(lop$dataTable)==lop$Y] <- 'Y'
names(lop$dataTable)[names(lop$dataTable)==lop$repetition] <- 'rep'
names(lop$dataTable)[names(lop$dataTable)==lop$condiiton]  <- 'cond'

# make sure sess and subject are treated as factors
if(!is.factor(lop$dataTable$rep)) lop$dataTable$rep <- as.factor(lop$dataTable$rep)
if(!is.factor(lop$dataTable$subject)) lop$dataTable$subj <- as.factor(lop$dataTable$subj)

cat('===== Summary of variable information =====', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(sprintf('Response variable Y - mean: %f; SD: %f', mean(lop$dataTable$Y), sd(lop$dataTable$Y)),
   file = paste0(lop$outFN, '.txt'), append=TRUE)
outDF(summary(lop$dataTable$Y), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), append=TRUE)
cat('Data structure:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(str(lop$dataTable), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), append=TRUE)
cat('subjects:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$subj), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), append=TRUE)
cat('repetitions:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$rep), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), append=TRUE)
cat('conditions:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$cond), lop$outFN)

# check for conditions and dummy-code conditions
if(!is.null(lop$cond)) {
   if(nlevels(lop$dataTable$cond) > 2) errex.AFNI(paste(nlevels(lop$dataTable$cond), 
      'conditions are detected:', levels(lop$dataTable$cond), 
      ". TRR currently does not support analysis with more than 2 conditions.\n")) else
   lop$dataTable$cond <- ifelse(lop$dataTable$cond ==  levels(lop$dataTable$cond)[1], -0.5, 0.5)
}

# check for repetitions
if(!is.null(lop$dataTable$rep)) {
   if(nlevels(lop$dataTable$rep) > 2) errex.AFNI(paste(nlevels(lop$dataTable$rep),
      'repetitions are detected:', levels(lop$dataTable$rep),
      ". TRR crurrently does not support analysis with more than 2 repetitions.\n"))
}

cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

# deviation coding: -1/0/1 - the intercept is associated with the mean across the levels of the factor
# each coding variable corresponds to the level relative to the mean: alphabetically last level is
# is baseline or reference level
#options(contrasts = c("contr.sum", "contr.poly"))

options(mc.cores = parallel::detectCores())

# within-chain parallelization?
if(lop$WCP) {
   require('cmdstanr')
}

# change the se column name when se is provided as input
if(!is.null(lop$se)) names(lop$dataTable)[which(names(lop$dataTable)==lop$se)] <- 'se'

# convert tstat to se when tstat is provided as input
if(!is.null(lop$tstat)) {
   lop$se <- TRUE
   lop$dataTable$se <- lop$dataTable$Y/lop$dataTable[[lop$tstat]]
}

# model specifications: one effect or contrast; se as input or not
if(is.null(lop$cond)) {
   if(is.null(lop$se)) modelForm <- as.formula(Y~0+rep+(0+rep|subj)) else
   modelForm <- as.formula(Y|se(se, sigma = TRUE)~0+rep+(0+rep|subj))
} else {
   if(is.null(lop$se)) 
      modelForm <- as.formula(Y~0+rep+rep:cond+(0+rep|subj)+(0+rep:cond|subj)) else
   modelForm <- as.formula(Y|se(se, sigma = TRUE)~0+rep+rep:cond+(0+rep|subj)+(0+rep:cond|subj))
}

set.seed(1234)
# Start the clock!
ptm <- proc.time()

##################### MCMC ####################

if(lop$WCP) 
   fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, family=lop$distY,
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15),
      backend = "cmdstanr", threads = threading(lop$WCP)) else
fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, family=lop$distY,
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))


#   fm <- brm(modelForm, data=lop$dataTable,
#      prior=c(prior(normal(0, 1), class = "Intercept"), prior(gamma(2, 0.5), class = "sd")),
#      chains = lop$chains, iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

print(format(Sys.time(), "%D %H:%M:%OS3"))

# Stop the clock
proc.time() - ptm

save.image(file=paste0(lop$outFN, ".RData"))

cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(capture.output(proc.time() - ptm), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

##################### Post Processing ####################

options(width=300)
cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of model information *****\n')
(rs <- summary(fm))
cat('\n***** End of model information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('\n***** Summary information of model results *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

#  Rhat checking #
#dd <- function(ll) any(ll[,'Rhat'] > 1.1)
#if(any(sapply(c(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars), rs$random), dd))) {
#   cat('\n***** Warning: convergence issue!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}

#d1 <- function(ll, mm, nn) any(ll[,mm] > nn)
## Rhat check
#if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars, rs$random$ROI, rs$random$Subj), d1, 'Rhat', 1.1))) {
#   cat('\n***** WARNING: convergence issue - Rhat > 1.1!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}
#d2 <- function(ll, mm, nn) any(ll[,mm] < nn)
## effective size check
#if(is.null(attr(rs$cor_pars, 'dimnames')[[1]])) {
#   if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, rs$random$ROI, rs$random$Subj), d2, 'Eff.Sample', 75))) {
#      cat('\n***** WARNING: effective size too small!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#      cat('Consider increasing the number of chains or iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   } 
#} else if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars, rs$random$ROI, rs$random$Subj), dd, 'Eff.Sample', 75))) {
#   cat('\n***** WARNING: effective size too small!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of chains or iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}

cat(capture.output(rs), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)   

ns <- lop$iterations*lop$chains/2
subjE <- ranef(fm, summary = FALSE) # subject-level effect
pE <- fixef(fm, summary = FALSE) # Population-Level Estimates

# function to obtain mode for a posterior density
m <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

if(is.null(lop$cond)) { # single effect
   ll <- rep(0, 2000)
   for(ii in 1:ns)
      ll[ii] <- cor(subjE[[1]][ii, ,1], subjE[[1]][ii, ,2])
   lp <- m(ll)
   require(coda)
   cat('######\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   lq <- HPDinterval(as.mcmc(ll))
   # average between the two conditions
   cat(sprintf('Population-level average effect between the two conditions: mean - %f; sd - %f',
      mean((pE[,1]+pE[,2])/2), sd((pE[,1]+pE[,2])/2)),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('TRR across the two repetions: mode - %f;', lp),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('TRR: 95%% highest density interval - [%f, %f]', lq[1], lq[2]),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   require(ggplot2)
   ggplot(data.frame(x=ll), aes(x=x)) + theme_bw() + geom_density(size=2) +
     geom_vline(aes(xintercept= lp), color="blue", linetype="dashed", size=2) +
     geom_area(data = subset(data.frame(x = density(ll)$x, y = density(ll)$y),
        x >= lq[1] & x <= lq[2]), aes(x=x,y=y, color='gray'), alpha=0.15) +
     xlab("TRR") +
     theme(legend.position="none", axis.text=element_text(size=18), axis.title=element_text(size=18))
   ggsave(paste0(lop$outFN,'-ave.pdf'))
} else { # contrast
   ll <- rep(0, 2000); mm <- rep(0, 2000)
   for(ii in 1:ns) {
      ll[ii] <- cor(subjE[[1]][ii, ,1], subjE[[1]][ii, ,2])
      mm[ii] <- cor(subjE[[1]][ii, ,3], subjE[[1]][ii, ,4])
   }
   lp <- m(ll); mp <- m(mm)
   require(coda)
   cat('######\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   lq <- HPDinterval(as.mcmc(ll)); mq <- HPDinterval(as.mcmc(mm))
   # average between the two conditions
   cat(sprintf('Population-level average effect between the two conditions: mean - %f; sd - %f',
      mean((pE[,1]+pE[,2])/2), sd((pE[,1]+pE[,2])/2)),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('TRR for the average effect between the two conditions: mode - %f;', lp),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('TRR: 95%% highest density interval - [%f, %f]', lq[1], lq[2]),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n-------\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   # contrast between the two conditions
   cat(sprintf('Population-level contrast between the two conditions: mean - %f; sd - %f',
      mean((pE[,3]+pE[,4])/2), sd((pE[,3]+pE[,4])/2)),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('TRR for the contrast between the two conditions: mode - %f;', mp),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('95%% highest density interval - [%f, %f]', mq[1], mq[2]),
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   require(ggplot2) 
   ggplot(data.frame(x=ll), aes(x=x)) + theme_bw() + geom_density(size=2) + 
     geom_vline(aes(xintercept= lp), color="blue", linetype="dashed", size=2) +
     geom_area(data = subset(data.frame(x = density(ll)$x, y = density(ll)$y), 
        x >= lq[1] & x <= lq[2]), aes(x=x,y=y, color='gray'), alpha=0.15) +
     xlab("TRR") +
     theme(legend.position="none", axis.text=element_text(size=18), axis.title=element_text(size=18))
   ggsave(paste0(lop$outFN,'-ave.pdf'))

   ggplot(data.frame(x=mm), aes(x=x)) + theme_bw() + geom_density(size=2) +
     geom_vline(aes(xintercept= mp), color="blue", linetype="dashed", size=2) +
     geom_area(data = subset(data.frame(x = density(mm)$x, y = density(mm)$y), 
        x >= mq[1] & x <= mq[2]), aes(x=x,y=y, color='gray'), alpha=0.15) +
     xlab("TRR") +
     theme(legend.position="none", axis.text=element_text(size=18), axis.title=element_text(size=18))
   ggsave(paste0(lop$outFN,'-con.pdf'))
}

################

# save it again
save.image(file=paste0(lop$outFN, ".RData"))
cat("\nCongratulations: TRR analysis was successful! Results are saved in file ", lop$lop$outFN, "\n\n", sep='')


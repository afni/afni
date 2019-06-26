#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

# Command line to run this script: RBA.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- 'RBA'

# Global variables

#################################################################################
##################### Begin RBA Input functions ################################
#################################################################################

#The help function for RBA batch (AFNI-style script mode)
help.RBA.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
                      Welcome to RBA ~1~
    Region-Based Analysis Program through Bayesian Multilevel Modeling 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.4, June 26, 2019
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
 RBA performs region-based analysis (RBA) as theoretically elaborated in the
 manuscript: https://rdcu.be/bhhJp and is conducted with a shell script (as
 shown in the examples below). The input data should be formulated in a
 pure-text table that codes the regions and variables. The response variable
 is some effect at the individual subject level.

 Thanks to Paul-Christian Bürkner and the Stan/R communities for the strong support.

 Citation: ~1~
 If you want to cite the approach for RBA, consider the following:~2~

 Chen G, Xiao Y, Taylor PA, Riggins T, Geng F, Redcay E, 2019. Handling Multiplicity
 in Neuroimaging through Bayesian Lenses with Multilevel Modeling. Neuroinformatics.
 https://rdcu.be/bhhJp

 =============================== 
 Read the following carefully!!!
 ===============================
 A data table in pure text format is needed as input for an RBA script. The
 data table should contain at least 3 columns that specify the information
 about subjects, regions and the response variable values with the following
 fixed header. The header lables are case-sensitive, and their order does not
 matter.

 Subj   ROI        Y      Age
 S1     Amyg    0.2643    11
 S2     BNST    0.3762    16
 ...

 0) You are performing Bayesian analysis!!! So, you will directly obtain
    the probability of an effect being positive or negative with your data,
    instead of witch hunt-hunting the straw man of p-value (weirdness of your
    data when pretending that absolutely nothing exists).

 1) Avoid using pure numbers to code the labels for categorical variables. The
    column order does not matter. You can specify those column names as you
    prefer, but it saves a little bit scripting if you adopt the default naming
    for subjects (\'Subj\'), regions (\'ROI\') and response variable (\'Y\').

 2) Add more columns if explanatory variables are considered in the model. Currently
    only between-subjects variables (e.g., sex, patients vs. controls, age) are
    allowed. Capability of modeling within-subject or repeated-measures variables
    may be added in the future. Each label in a between-subjects factor (categorical
    variable) should be coded with at least 1 character (labeling with pure numbers
    is fine but not recommended). If preferred, you can quantitatively code the
    levels of a factor yourself by creating k-1 columns for a factor with k levels.
    However, be careful with your coding strategy because it would impact how to
    interpret the results. Here is a good reference about factor coding strategies:
    https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

 3) It is strongly suggested that a quantitative explanatory variable be
    standardized with option -stdz; that is, remove the mean and scale by
    the standard deviation. This will improve the chance of convergence
    with each Markov chain. If a between-subjects factor (e.g., sex) is
    involved, it may be better to standardize a quantitative variable
    within each group in terms of interpretability if the mean value differs
    substantially. However, do not standardize a between-subjects factor if
    you quantitatively code it. And do not standardize the response variable
    if the intercept is of interest!

 4) For within-subject variables, try to formulate the data as a contrast
    between two factor levels or as a linear combination of multiple levels.

 5) The results from RBA are effect estimates for each region. They can be
    slightly different across different runs or different computers and R
    package versions due to the nature of randomness involved in Monte Carlo
    simulations.

 6) The evidence for region effects in the output can be assessed through P+,
    the probability that the effect is postive conditional on the current
    dataset. The following criterion for highlighting the results is only
    suggestive:
     P+ >= 0.975 or <= 0.025 - very strong evidence
     P+ >= 0.95 or <= 0.05   - strong evidence
     P+ >= 0.90 or <= 0.10   - moderate evidence
     else                    - little evidence

    The same criterion is applied to the color coding in the posterior
    distribution plots generated with the option -PDP:
     P+ >= 0.975 or <= 0.025 - green:    very strong evidence
     P+ >= 0.95 or <= 0.05   - yellow:   strong evidence
     P+ >= 0.90 or <= 0.10   - gray:     moderate evidence
     else                    - no color: little evidence

 =========================
 
 Installation requirements: ~1~
 In addition to R installation, the R package "brms" is required for RBA. Make
 sure you have the most recent version of R. To install "brms", run the following
 command at the terminal:

 rPkgsInstall -pkgs "brms" -site http://cran.us.r-project.org"

 Alternatively you may install them in R:

 install.packages("brms")
 
 Running: ~1~
 Once the RBA command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called myRBA.txt, and execute it with the 
 following  (assuming on tcsh shell),
 
 nohup tcsh -x myRBA.txt > diary.txt &
 nohup tcsh -x myRBA.txt |& tee diary.txt &

 The advantage of the commands above is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 The \'nohup\' command allows the analysis running in the background even if
 the terminal is killed.'
 
   ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- Simplest scenario. Values from regions are the input from
          each subject. No explanatory variables are considered. Research
	  interest is about the population effect at each region.

   RBA -prefix output -dataTable myData.txt  \\

   The above script is equivalent to

   RBA -prefix myWonderfulResult -chains 4 -iterations 1000 -model 1 -EOI 'Intercept' \\
   -r2z -dataTable myData.txt  \\

   The 2nd version above is recommended because of its explicit specifications.

   The input file 'myData.txt' is a data table in pure text format as below: 
                                                             
     Subj  ROI          Y
     S01   lFFA       0.162
     S02   lAmygdala -0.598
     S03   DMNLAG     0.249
     S04   DMNPCC     0.568
     ...
 \n"         
     
   ex2 <-
"--------------------------------
Example 2 --- 2 between-subjects factors (sex and group): ~2~

   RBA -prefix output -Subj subject -ROI region -Y zscore -PDP 4 4 \\
   -chains 4 -iterations 1000 -model '1+sex+group' \\
   -cVars 'sex,group' -EOI 'Intercept,sex,group' \\
   -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   subject region  zscore  sex group
   S1      DMNLAG  0.274    F  patient
   S1      DMNLHC  0.443    F  patient
   S2      DMNRAG  0.455    M  contorl
   S2      DMNRHC  0.265    M  control
   ...

   Notice that the interaction between 'sex' and 'group' is not modeled in
   this case. The option -PDP 4 4 allows the program to generate a 4 x 4
   posterior distribution plots for the 16 regions.
\n"
     
   ex3 <-
"---------------------------------
Example 3 --- one between-subjects factor (sex), one within-subject factor (two
   conditions), one between-subjects covariate (age), and one quantitative
   variable: ~2~
    
   RBA -prefix result -PDP 4 4 -Subj Subj -ROI region -Y value \\
   -chains 4 -iterations 1000 -model '1+sex+age+SA' -qVars 'sex,age,SA' \\
   -EOI 'Intercept,sex,age,SA' -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   Subj   region   value sex  age   SA
   S1    DMNLAG    0.274  1  1.73  1.73
   S1    DMNLHC    0.443  1  1.73  1.73
   S2    DMNRAG    0.455 -1 -0.52 -0.52
   S2    DMNRHC    0.265 -1 -0.52 -0.52
   ...

   Notice

   1) the 'Y' column is the contrast between the two conditions.
   2) since we want to model the interaction between 'sex' and 'age', 'sex' is
      coded through deviation coding.
   3) 'age' has already been standardized within each sex due to large age
      difference between the two sexes.
   4) the 'SA' column codes for the interaction between 'sex' and 'age', which
      is the product of the two respective columns.
   
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
read.RBA.opts.batch <- function (args=NULL, verb = 0) {
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
   "         .RBA.dbg.AFNI.args in the current directory so that debugging can be",
   "         performed.\n", sep='\n')),

      '-MD' = apl(n=0, h = paste(
   "-MD: This option indicates that there are missing data in the input. With n",
   "         regions, at least n(n-1)/2 values are assumed from each subject in the",
   "         input with no missing data (default). When missing data are present,",
   "         invoke this option so that the program will handle it properly.\n", sep='\n')),

      '-r2z' = apl(n=0, h = paste(
   "-r2z: This option performs Fisher transformation on the response variable",
   "         (column Y) if it is correlation coefficient.\n", sep='\n')),

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

      '-stdz' = apl(n=c(1,100), d=NA, h = paste(
   "-stdz variable_list: Identify quantitative variables (or covariates) to be",
   "         standardized. To obtain meaningful and interpretable results and to",
   "         achieve better convergence of Markov chains with reasonable iterations,",
   "         it is recommended that all quantitative variables be standardized",
   "         except for the response variable and indicator variables that code for",
   "         factors. For example, -stdz \"Age,IQ\". If the mean of a quantitative",
   "         variables varies substantially between groups, it may make sense to",
   "         standardize the variable within each group before plugging the values",
   "         into the data table. Currently RBA does not offer the option to perform",
   "         within-group standardization.\n",
             sep = '\n'
             ) ),

      '-EOI' = apl(n=c(1,100), d=NA, h = paste(
   "-EOI variable_list: Identify effects of interest in the output by specifying the",
   "         variable names separated with comma (,). For example, -EOI \"sex,age\".",
   "         By default the Intercept is considered to be an effect of interest.",
   "         Currently only variables, not their interactions, can be directly",
   "         requested for output. However, most interaction effects can be obtained by",
   "         either properly coding the variables (see example 3) or post processing.\n",
             sep = '\n'
             ) ),

      '-qContr' = apl(n=c(1,100), d=NA, h = paste(
   "-qContr contrast_list: Identify comparisons of interest between quantitative",
   "         variables in the output separated with comma (,). It only allows for",
   "         pair-wise comparisons between two quantitative variables. For example,",
   "         -qContr \"age vs IQ, age vs weight, IQ vs weight\", where V1, V2, and V3 are three",
   "         quantitative variables and three comparisons, V1 - V2, V1 - V3 and V2 - V3",
   "         will be provided in the output. Make sure that such comparisons are",
   "         meaningful (e.g., with the same scale and unit. This can be used to",
   "         formulate comparisons among factor levels if the user quantitatively",
   "         codes the factor levels.\n",
             sep = '\n'
             ) ),

      '-Y' = apl(n = 1, d = NA,  h = paste(
   "-Y var_name: var_name is used to specify the column name that is designated as",
   "        as the response/outcome variable. The default (when this option is not",
   "        invoked) is 'Y'.\n", sep = '\n'
                     ) ),

      '-Subj' = apl(n = 1, d = NA,  h = paste(
   "-Subj var_name: var_name is used to specify the column name that is designated as",
   "        as the measuring unit variable (usually subject). The default (when this",
   "        option is not invoked) is 'Subj'.\n", sep = '\n'
                     ) ),

      '-ROI' = apl(n = 1, d = NA,  h = paste(
   "-ROI var_name: var_name is used to specify the column name that is designated as",
   "        as the region variable. The default (when this option is not invoked) is",
   "        'ROI'.\n", sep = '\n'
                     ) ),

      '-PDP' = apl(n = 2, d = NA, h = paste(
   "-PDP nr nc: Specify the layout of posterior distribution plot (PDP) with nr rows",
   "         and nc columns among the number of plots. For example, with 16 regions,",
   "         you can set nr = 4 and nc = 4. The region names will be shown in each plot.",
   "         So label the regions concisely.\n", sep = '\n'
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
      errex.AFNI('Error parsing arguments. See RBA -help for details.')

   #Parse dems options
   #initialize with defaults
      lop <- AFNI.new.options.list(history = '', parsed_args = ops)
      lop$chains <- 1
      lop$iterations <- 1000
      lop$model  <- 1
      lop$cVars  <- NULL
      lop$qVars  <- 'Intercept'
      lop$stdz   <- NA
      lop$EOI    <- 'Intercept'
      lop$qContr <- NA
      lop$Y      <- 'Y'
      lop$Subj   <- 'Subj'
      lop$ROI    <- 'ROI'
      lop$PDP    <- NA

      lop$dbgArgs <- FALSE # for debugging purpose
      lop$MD      <- FALSE # for missing data 
      lop$r2z     <- FALSE # Fisher transformation
      lop$verb    <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             chains   = lop$chains <- ops[[i]],
             iterations = lop$iterations <- ops[[i]],
             verb   = lop$verb   <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             cVars  = lop$cVars  <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             stdz   = lop$stdz   <- ops[[i]],
             EOI    = lop$EOI    <- ops[[i]],
             qContr = lop$qContr <- ops[[i]],
             Y      = lop$Y      <- ops[[i]],
             Subj   = lop$Subj   <- ops[[i]],
             ROI    = lop$ROI    <- ops[[i]],
             PDP    = lop$PDP    <- ops[[i]],
             help    = help.RBA.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,
             MD      = lop$MD      <- TRUE,
             r2z     = lop$r2z     <- TRUE,
             dataTable  = lop$dataTable <- read.table(ops[[i]], header=T),
             )
   }

   return(lop)
}# end of read.RBA.opts.batch
                                               
                                               
#Change options list to RBA variable list 
process.RBA.opts <- function (lop, verb = 0) {
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
# process.RBA.opts(lop, verb = lop$verb)                                               

#################################################################################
########################## Begin RBA main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
       # save only on -dbgArgs          28 Apr 2016 [rickr]
       if ('-dbgArgs' %in% args) try(save(args, rfile, file=".RBA.dbg.AFNI.args", ascii = TRUE), silent=TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.RBA(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.RBA.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.RBA.opts.batch(args, verb = 0)))
         stop('Error parsing input')                
      
      #str(lop);
      if(is.null(lop <- process.RBA.opts(lop, verb = lop$verb))) 
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

# standardize the names for Y, ROI and subject
names(lop$dataTable)[names(lop$dataTable)==lop$Subj] <- 'Subj'
names(lop$dataTable)[names(lop$dataTable)==lop$Y] <- 'Y'
names(lop$dataTable)[names(lop$dataTable)==lop$ROI] <- 'ROI'

# make sure ROI and Subj are treated as factors
if(!is.factor(lop$dataTable$ROI)) lop$dataTable$ROI <- as.factor(lop$dataTable$ROI)
if(!is.factor(lop$dataTable$Subj)) lop$dataTable$Subj <- as.factor(lop$dataTable$Subj)

# verify variable types
if(lop$model==1) terms <- 1 else terms <- strsplit(lop$model, '\\+')[[1]]
if(length(terms) > 1) {
   #terms <- terms[terms!='1']
   for(ii in 1:length(terms)) {
       if(!is.null(lop$cVars[1])) if(terms[ii] %in% strsplit(lop$cVars, '\\,')[[1]] & !is.factor(lop$dataTable[[terms[ii]]])) # declared factor with quantitative levels
         lop$dataTable[[terms[ii]]] <- as.factor(lop$dataTable[[terms[ii]]])
      if(terms[ii] %in% strsplit(lop$qVars, '\\,')[[1]] & is.factor(lop$dataTable[[terms[ii]]])) # declared numerical variable contains characters
         stop(sprintf('Column %s in the data table is declared as numerical, but contains characters!', terms[ii]))
   }
}

# number of ROIs
nR <- nlevels(lop$dataTable$ROI)

cat('===== Summary of variable information =====', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(sprintf('Total number of ROIs: %i', nR), 
   file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(sprintf('Response variable Y - mean: %f; SD: %f', mean(lop$dataTable$Y), sd(lop$dataTable$Y)), 
   file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$Y), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat('Data structure:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(str(lop$dataTable), lop$outFN)
cat('Subjects:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$Subj), lop$outFN)
cat('ROIs:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
outDF(summary(lop$dataTable$ROI), lop$outFN)

cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

if(!lop$MD) if(nlevels(lop$dataTable$Subj)*nR < nrow(lop$dataTable))
   stop(sprintf('Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to toally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD', nR, nlevels(lop$dataTable$Subj), nR, nlevels(lop$dataTable$Subj)*nR, nrow(lop$dataTable)))

lop$EOIq <- strsplit(lop$qVars, '\\,')[[1]]
if(!('Intercept' %in% lop$EOIq)) lop$EOIq <- c('Intercept', lop$EOIq)
lop$EOIq <- intersect(strsplit(lop$EOI, '\\,')[[1]], lop$EOIq)
if(is.null(lop$cVars)) lop$EOIc <- NA else 
   lop$EOIc <- intersect(strsplit(lop$EOI, '\\,')[[1]], strsplit(lop$cVars, '\\,')[[1]])

if(!is.na(lop$qContr)) {
   qContrL <- unlist(strsplit(lop$qContr, '\\,'))
   # verify 'vs' in alternating location
   ll <- which(qContrL %in% 'vs')
   if(!all(ll == seq(2,300,3)[1:length(ll)]))
      stop(sprintf('Quantitative contrast specification -qContr is incorrect!'))
   lop$qContrL <- qContrL[!qContrL %in% 'vs']
   # verify that variable names are correct
   if(!all(lop$qContrL %in% c(lop$QV, 'Intercept'))) 
      stop(sprintf('At least one of the variable labels in quantitative contrast specification -qContr is incorrect!'))
}

# deviation coding: -1/0/1 - the intercept is associated with the mean across the levels of the factor
# each coding variable corresponds to the level relative to the mean: alphabetically last level is
# is baseline or reference level
options(contrasts = c("contr.sum", "contr.poly"))
options(mc.cores = parallel::detectCores())

# Fisher transformation
fisher <- function(r) ifelse(abs(r) < .995, 0.5*(log(1+r)-log(1-r)), stop('Are you sure that you have correlation values so close to 1 or -1?'))
if(lop$r2z) lop$dataTable$Y <- fisher(lop$dataTable$Y)

# standardization
if(!is.na(lop$stdz)) {
   sl <- strsplit(lop$stdz, '\\,')[[1]]
   for(ii in 1:length(sl)) if(is.numeric(lop$dataTable[[sl[ii]]])) 
      lop$dataTable[[sl[ii]]] <- scale(lop$dataTable[[sl[ii]]], center = TRUE, scale = TRUE) else
   stop(sprintf('The column %s is categorical, not numerical! Why are you asking me to standardize it?', sl[ii]))
}

set.seed(1234)
lop$dataTable$w <- 1

# Start the clock!
ptm <- proc.time()

## for testing only: remove this soon ####
#lop$dataTable$V1 <- rnorm(nrow(lop$dataTable))
#lop$dataTable$V2 <- rnorm(nrow(lop$dataTable), mean=0.5, sd=1)
#lop$model <- '1+V1+V2'

##################### MCMC ####################
if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|Subj) + (1|ROI)')) else
   modelForm <- as.formula(paste('Y~', lop$model, '+(1|Subj)+(', lop$model, '|ROI)'))

if(lop$model==1) fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, 
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)) else
   fm <- brm(modelForm, data=lop$dataTable, 
      prior=c(prior(normal(0, 1), class = "Intercept"), prior(normal(0, 0.5), class = "sd")),
      chains = lop$chains, iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

print(format(Sys.time(), "%D %H:%M:%OS3"))

# Stop the clock
proc.time() - ptm

save.image(file=paste0(lop$outFN, ".RData"))

cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(capture.output(proc.time() - ptm), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

##################### Post Processing ####################

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of model information *****\n')
(rs <- summary(fm))
cat('\n***** End of model information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('\n***** Summary information of model results *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

#  Rhat checking #
dd <- function(ll) any(ll[,'Rhat'] > 1.2)
if(any(sapply(c(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars), rs$random), dd))) {
   cat('\n***** Warning: convergence issue!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
}

cat(capture.output(rs), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)   

ns <- lop$iterations*lop$chains/2
aa <- fixef(fm, summary = FALSE) # Population-Level Estimates
bb <- ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimates

# compute P+
cnt <- function(x, ns) return(sum(x>0)/ns)

########## region effects #############
# posterior samples at ROIs for a term
psROI <- function(aa, bb, tm) {
  ps <- apply(bb[['ROI']][,,tm], 2, '+', aa[,tm])
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)

# summary for ROIs: nd - number of digits to output
sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(apply(R0, 2, mean), apply(R0, 2, sd), apply(R0, 2, cnt, ns), t(apply(R0, 2, quantile, 
      probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs,nd))
}
#gg <- sumROI(gg, ns, 3)

#is.even <- function(x) x %% 2 == 0

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

plotPDP <- function(fn, ps, nR, nr, nc, w=8) {
   h <- ceiling(8*nr/(nc*2))  # plot window height
   pdf(paste0(fn, ".pdf"), width=w, height=h)
   #dev.new(width=w, height=h)
   par(mfrow=c(lop$PDP[1], nc), mar=c(2.5,0,0.0,0.8), oma=c(0,0,0,0))
   qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)) # 95% central interval
   kk <- 0
   for(ii in 1:nR) {
      kk <- kk+1
      #x <- quantile(ps[,ii], probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
      dens <- density(ps[,ii])
      #par(mar=c(1.85,0.2,0.0,0.8))
      plot(dens, main='', axes=F, bty="n", xlab='', ylab='')
      axis(side = 1)
      abline(v=0, col='blue')
      #if(is.even(kk)) mtext(dimnames(ps)[[2]][ii], side = 1, line=-7, las=0) else
      mtext(dimnames(ps)[[2]][ii], side = 3, line=-2, las=0)
      x1 <- min(which(dens$x >= qq[6,ii]))  # 97.5% 
      x2 <- max(which(dens$x <  4e10))         # infinity
      x3 <- min(which(dens$x >= -4e10))        # -infinity
      x4 <- max(which(dens$x <  qq[1,ii]))  # 2.5%
      x5 <- min(which(dens$x >= qq[5,ii]))  # 95%
      x6 <- max(which(dens$x <  qq[2,ii]))  # 5%
      x7 <- min(which(dens$x >= qq[4,ii]))  # 90%
      x8 <- max(which(dens$x <  qq[3,ii]))  # 10%
      with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=addTrans('green',175))) # right tail
      with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=addTrans('green',175))) # left tail
      with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col=addTrans('gray',125)))
      with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col=addTrans('gray',125)))
      #with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="green")) # right tail
      #with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="green")) # left tail
      #with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col="gray"))
      #with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col="gray"))
      if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('green',200), lwd=3)
      if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('orange',150), lwd=3)
      if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('gray',100), lwd=3)
      #if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'green', lwd=3)
      #if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'orange', lwd=3)
      #if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'gray', lwd=3)
   }
   dev.off()
}

# for Intercept and quantitative variables
if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
   cat(sprintf('===== Summary of region effects for %s =====', lop$EOIq[ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, lop$EOIq[ii])
   gg <- sumROI(ps0, ns, 4)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(lop$PDP) == TRUE)) plotPDP(lop$EOIq[ii], ps0, nR, lop$PDP[1], lop$PDP[2], 8)
}

# for contrasts among quantitative variables
if(any(!is.na(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, lop$qContrL[2*ii-1]) - psROI(aa, bb, lop$qContrL[2*ii])
   gg <- sumROI(ps0, ns, 4)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(lop$PDP) == TRUE)) plotPDP(paste0(lop$qContrL[2*ii-1], '-', lop$qContrL[2*ii]), ps0, nR, lop$PDP[1], lop$PDP[2], 8)
}

# for factor
if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
   lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
   nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(lop$EOIc[ii],jj))
   ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect 
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,] 
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level
   
   oo <- apply(psa, 1, sumROI, ns, 4)
   
   cat(sprintf('===== Summary of region effects for %s =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }

   if(any(!is.na(lop$PDP) == TRUE)) for(jj in 1:nl)
      plotPDP(lop$EOIc[ii], psa[jj,,], nR, lop$PDP[1], lop$PDP[2], 8)
   
   cat(sprintf('===== Summary of region effects for %s comparisons =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 4)
      cat(capture.output(oo), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      if(any(!is.na(lop$PDP) == TRUE))  plotPDP(paste0(lvl[jj], '-', lvl[kk]), psa[jj,,] - psa[kk,,], nR, lop$PDP[1], lop$PDP[2], 8)
   }
}

##################### conventional GLM #####################
mm <- list()
GLM <- as.formula(paste('Y ~', lop$model))
for(ii in levels(lop$dat$ROI)) mm[[ii]] = lm(GLM, data=lop$dat[lop$dat$ROI==ii,])
nn <- lapply(mm, summary)
ll <- lapply(nn, `[`, 'coefficients')

sumGLM <- function(ll, tm, nR, DF, nd) {
   th <- qt(c(0.025, 0.05, 0.5, 0.95, 0.975), DF)
   rr <- matrix(, nrow = nR, ncol = 8, dimnames=list(levels(lop$dat$ROI), c('mean', 'SD', '2-sided-p', '2.5%', '5%', '50%', '95%', '97.5%')))
   rownames(rr) <- levels(lop$dat$ROI)
   if(tm == 'Intercept') tm <- '(Intercept)'
   for(ii in 1:nR) {
     u1 <- ll[[ii]]$coefficients[tm,1] # mean
     u2 <- ll[[ii]]$coefficients[tm,2] # sd
     u3 <- ll[[ii]]$coefficients[tm,4] # 2-sided p
     rr[ii,] <- round(c(u1, u2, u3, u1+u2*th),nd)
   } 
   return(rr)
}

# for Intercept and quantitative variables
if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
   cat(sprintf('===== Summary of region effects under GLM for %s: no adjustment for multiplicity =====', lop$EOIq[ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   gg <- sumGLM(ll, lop$EOIq[ii], nR, nn[[ii]]$df, 4)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
}

# for contrasts among quantitative variables
if(any(!is.na(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
}

# for factor
if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
   lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
   nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(lop$EOIc[ii],jj))
   ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect 
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,] 
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level
   
   oo <- apply(psa, 1, sumROI, ns, 4)
   
   cat(sprintf('===== Summary of region effects for %s =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
   
   cat(sprintf('===== Summary of region effects for %s comparisons =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 4)
      cat(capture.output(oo), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
}
################

# save it again
save.image(file=paste0(lop$outFN, ".RData"))
cat("\nCongratulations! The above results are saved in file ", lop$lop$outFN, "\n\n", sep='')

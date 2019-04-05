#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

# Command line to run this script: MBA.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- 'MBA'

# Global variables

#################################################################################
##################### Begin MBA Input functions ################################
#################################################################################

#The help function for MBA batch (AFNI-style script mode)
help.MBA.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
                      Welcome to MBA ~1~
    Matrix-Based Analysis Program through Bayesian Multilevel Modeling 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.6, April 5, 2019
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
 MBA performs matrix-based analysis (MBA) as theoretically elaborated in the
 manuscript: https://www.biorxiv.org/content/10.1101/459545v1
 MBA is conducted with a shell script (as shown in the examples below). The
 input data should be formulated in a pure-text table that codes the regions
 and variables. The response variable is usually correlation values (with or
 without Fisher-transformation) or white-matter properties (e.g., fractional
 anisotropy, mean diffusivity, radial diffusivity, axial diffusivity, etc.),
 but it can also be any values from a symmetric matrix (e.g., coherence,
 mutual information, entropy). In other words, the effects are assumed to be
 non-directional or non-causal. Diagonals can be included in the input if
 sensible.

 Thanks to Zhihao Li for motivating me to start the MBA work, and to 
 Paul-Christian Bürkner and the Stan/R communities for the strong support.

 Citation: ~1~
 If you want to cite the approach for MBA, consider the following:~2~

 Chen, G., Bürkner, P.-C., Taylor, P.A., Li, Z., Yin, L., Glen, D.R., Kinnison, J.,
 Cox, R.W., Pessoa, L., 2019. An Integrative Approach to Matrix-Based Analyses in
 Neuroimaging. https://doi.org/10.1101/459545

 =============================== 
 Read the following carefully!!!
 ===============================
 A data table in pure text format is needed as input for an MBA script. The
 data table should contain at least 4 columns that specify the information
 about subjects, region pairs and the response variable values with the
 following fixed header. The header lables are case-sensitive, and their order
 does not matter.

 Subj   ROI1   ROI2   Y
 S1     Amyg   SMA   0.2643
 S2     BNST   MPFC  0.3762
 ...

 0) Be proud of yourself: you are performing Bayesian analysis!!! You directly
    get the probability of an effect being positive or negative with your data,
    instead of witch hunt-hunting the straw man of p-value (weirdness of your
    data when pretending that absolutely nothing exists).

 1) Avoid using pure numbers to code the labels for categorical variables. The
    column order does not matter, but the names of the four variables above
    in the header are reserved and fixed (case sensitive). The column labels
    ROI1 and ROI2 are meant to indicate the two regions associated with
    each response value, and they do not mean any sequence or directionality.

 2) Only provide half of the off-diagonals in the table (no duplicates allowed).
    Missing data are fine (e.g., white-matter property deemed nonexistent).

 3) Simple analysis can be done in a few minutes, but computational cost can be
    very high (e.g., weeks or even months) when the number of regions or subjects
    is large or when a few explanatory variables are involved. Be patient: there
    is hope in the near future that further parallelization can be implemented.

 4) Add more columns if explanatory variables are considered in the model. Currently
    only between-subjects variables (e.g., sex, patients vs. controls, age) are
    allowed. Each label in a between-subjects factor (categorical variable)
    should be coded with at least 1 character (labeling with pure numbers is fine
    but not recommended). If preferred, you can quantitatively code the levels of a
    factor yourself by creating k-1 columns for a factor with k levels. However, be
    careful with your coding strategy because it would impact how to interpret the
    results. Here is a good reference about factor coding strategies:
    https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

 5) It is strongly suggested that a quantitative explanatory variable be
    standardized with option -stdz; that is, remove the mean and scale by
    the standard deviation. This will improve the chance of convergence
    with each Markov chain. If a between-subjects factor (e.g., sex) is
    involved, it may be better to standardize a quantitative variable
    within each group in terms of interpretability if the mean value differs
    substantially. However, do not standardize a between-subjects factor if
    you quantitatively code it. And do not standardize the response variable
    if the intercept is of interest!

 6) With within-subject variables, try to formulate the data as a contrast
    between two factor levels or as a linear combination of multiple levels.

 7) The results from MBA are effect estimates for each region pair and at each
    region. They can be slightly different across different runs or different
    computers and R package versions due to the nature of randomness involved
    in Monte Carlo simulations.

 8) The range in matrix plot may vary across different effects within an analysis.
    It is possible to force the same range for all plots through fine-tuning
    within R using the output of .RData. The criteria of color coding for the
    strength of evidence in matrix plots in the output is as follows:
     Green - two-tailed 95% compatible/uncertainty interval (or probability of effect
             being positive >= 0.975 or <= 0.025)
     Blue  - one-tailed 95% compatible/uncertainty interval (or probability of effect
             being positive >= 0.95 or <= 0.05)
     Yellow- one-tailed 90% compatible/uncertainty interval (or probability of effect
             being positive >= 0.90 or <= 0.10)
     white - anything else

 =========================
 
 Installation requirements: ~1~
 In addition to R installation, the R package "brms" is required for MBA. Make
 sure you have the most recent version of R. To install "brms", run the following
 command at the terminal:

 rPkgsInstall -pkgs "brms" -site http://cran.us.r-project.org"

 Alternatively you may install them in R:

 install.packages("brms")
 
 Running: ~1~
 Once the MBA command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called myMBA.txt, and execute it with the 
 following  (assuming on tcsh shell),
 
 nohup tcsh -x myMBA.txt > diary.txt &
 nohup tcsh -x myMBA.txt |& tee diary.txt &

 The advantage of the commands above is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 The \'nohup\' command allows the analysis running in the background even if
 the terminal is killed.'
 
   ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- Simplest scenario. Values from region pairs are the input from
          each subject. No explanatory variables are considered. Research
	  interest is about the population effect at each region pair plus
	  the relative strength of each region.

   MBA -prefix myWonderfulResult -r2z -dataTable myData.txt  \\

   The above script is equivalent to

   MBA -prefix myWonderfulResult -chains 4 -iterations 1000 -model 1 -EOI 'Intercept' \\
   -r2z -dataTable myData.txt  \\

   The 2nd version is recommended because of its explicit specifications.

   The input file 'myData.txt' is a data table in pure text format as below: 
                                                             
     Subj  ROI1   ROI2      Y
     S01   lFFA lAmygdala  0.162
     S02   lFFA lAmygdala -0.598
     S03   lFFA lAmygdala  0.249
     S04   lFFA lAmygdala  0.568
     ...
 \n"         
     
   ex2 <-
"--------------------------------
Example 2 --- 2 between-subjects factors (sex and group): ~2~

   MBA -prefix giveMeGreatResult -chains 4 -iterations 1000 -model '1+sex+group' \\
   -cVars 'sex,group' -r2z -EOI 'Intercept,sex,group' \\
   -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   Subj ROI1  ROI2    Y  sex group
   S1 DMNLAG DMNLHC 0.274 F  patient
   S1 DMNLAG DMNPCC 0.443 F  patient
   S2 DMNLAG DMNRAG 0.455 M  contorl
   S2 DMNLAG DMNRHC 0.265 M  control
   ...

   Notice that the interaction between 'sex' and 'group' is not modeled in this case.
\n"
     
   ex3 <-
"---------------------------------
Example 3 --- one between-subjects factor (sex), one within-subject factor (two
   conditions), and one quantitative variable: ~2~
    
   MBA -prefix iLoveMyResult -chains 4 -iterations 1000 -model '1+sex+age+SA' \\
   -qVars 'sex,age,SA' -r2z -EOI 'Intercept,sex,age,SA' \\
   -dataTable myData.txt

   The input file 'myData.txt' is formatted as below:
   
   Subj ROI1  ROI2    Y   sex  age    SA
   S1 DMNLAG DMNLHC 0.274  1  1.73  1.73
   S1 DMNLAG DMNPCC 0.443  1  1.73  1.73
   S2 DMNLAG DMNRAG 0.455 -1 -0.52 -0.52
   S2 DMNLAG DMNRHC 0.265 -1 -0.52 -0.52
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
read.MBA.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Prefix is used to specify output file names. The main output is",
   "        a text with prefix appended with .txt and stores inference information ",
   "        for effects of interest in a tabulated format depending on selected ",
   "        options. The prefix will also be used for other output files such as ",
   "        visualization plots such as matrix plot, and saved R data in binary",
   "        mode. The .RData can be used for post hoc processing such as customized",
   "        processing and plotting. Remove the .RData file to save disk space once",
   "        you deem such a file is no longer useful.\n", sep = '\n'
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
   "         .MBA.dbg.AFNI.args in the current directory so that debugging can be",
   "         performed.\n", sep='\n')),

      '-MD' = apl(n=0, h = paste(
   "-MD: This option indicates that there are missing data in the input. With n",
   "         regions, at least n(n-1)/2 values are assumed from each subject in the",
   "         input with no missing data (default). When missing data are present,",
   "         invoke this option so that the program will handle it properly.\n", sep='\n')),

      '-r2z' = apl(n=0, h = paste(
   "-r2z: This option performs Fisher transformation on the response variable",
   "         (column Y) if it is correlation coefficient. Do not invoke the option",
   "         if the transformation has already been applied or the variable is",
   "         not correlation coefficient.\n", sep='\n')),

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
   "         into the data table. Currently MBA does not offer the option to perform",
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

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure in a table of long format (cf. wide",
   "         format) in R with a header as the first line. \n",
   "         NOTE:\n",
   "         1) There should have at least four columns in the table. These minimum",
   "         four columns can be in any order but with fixed and reserved with labels:",
   "         'Subj', 'ROI1', 'ROI2', and 'Y'. The two columns 'ROI1' and 'ROI2' are",
   "         meant to code the two regions that are associated with each value under the",
   "         column Y, and they do not connotate any indication of directionality other",
   "         than you may want to keep track of a consistent order, for example, in the",
   "         correlation matrix. More columns can be added in the table for explanatory",
   "         variables (e.g., groups, age, site) if applicable. Only subject-level",
   "         (or between-subjects) explanatory variables are allowed at the moment. The ",
   "         columns of 'Subj', 'ROI1' and 'ROI2' code each subject and the two regions ",
   "         associated with each region pair, and these labels that can be any identifiable",
   "         characters including numbers. The column 'Y' can be correlation value, ",
   "         Fisher-transformed correlation value, or white-matter property between ",
   "         grey-matter regions (e.g., mean diffusivity, fractional anisotropy, radial",
   "         diffusivity and axial diffusivity).",
   "         2) Each row is associated with one and only one 'Y' value, which is the",
   "         response variable in the table of long format (cf. wide format) as",
   "         defined in R. In the case of correlation matrix or white-matter property",
   "         matrix, provide only half of the off-diagonals. With n regions, there",
   "         should have at least n(n-1)/2 rows per subject, assuming no missing data. ",
   "         3) It is fine to have variables (or columns) in the table that are",
   "         not used in the current analysis.",
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
      errex.AFNI('Error parsing arguments. See MBA -help for details.')

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
      lopqContr  <- NA

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
             verb   = lop$verb  <- ops[[i]],
             model  = lop$model <- ops[[i]],
             cVars  = lop$cVars <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             stdz   = lop$stdz  <- ops[[i]],
             EOI    = lop$EOI   <- ops[[i]],
             qContr = lop$qContr <- ops[[i]],       
             help    = help.MBA.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,
             MD      = lop$MD      <- TRUE,
             r2z     = lop$r2z     <- TRUE,
             dataTable  = lop$dataTable <- read.table(ops[[i]], header=T),
             )
   }

   return(lop)
}# end of read.MBA.opts.batch
                                               
                                               
#Change options list to MBA variable list 
process.MBA.opts <- function (lop, verb = 0) {
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
# process.MBA.opts(lop, verb = lop$verb)                                               

#################################################################################
########################## Begin MBA main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
       # save only on -dbgArgs          28 Apr 2016 [rickr]
       if ('-dbgArgs' %in% args) try(save(args, rfile, file=".MBA.dbg.AFNI.args", ascii = TRUE), silent=TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.MBA(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.MBA.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.MBA.opts.batch(args, verb = 0)))
         stop('Error parsing input')                
      
      #str(lop);
      if(is.null(lop <- process.MBA.opts(lop, verb = lop$verb))) 
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

# make sure ROI1, ROI2 and Subj are treated as factors
if(!is.factor(lop$dataTable$ROI1)) lop$dataTable$ROI1 <- as.factor(lop$dataTable$ROI1)
if(!is.factor(lop$dataTable$ROI2)) lop$dataTable$ROI2 <- as.factor(lop$dataTable$ROI2)
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
nR <- length(union(levels(lop$dataTable$ROI1), levels(lop$dataTable$ROI2)))

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
outDF(summary(lop$dataTable$ROI1), lop$outFN)
outDF(summary(lop$dataTable$ROI2), lop$outFN)
cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

if(!lop$MD) if(nlevels(lop$dataTable$Subj)*nR*(nR-1)/2 < nrow(lop$dataTable))
   stop(sprintf('Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to toally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD', nR, nlevels(lop$dataTable$Subj), nR*(nR-1)/2, nlevels(lop$dataTable$Subj)*nR*(nR-1)/2, nrow(lop$dataTable)))

lop$EOIq <- strsplit(lop$qVars, '\\,')[[1]]
if(!('Intercept' %in% lop$EOIq)) lop$EOIq <- c('Intercept', lop$EOIq)
lop$EOIq <- intersect(strsplit(lop$EOI, '\\,')[[1]], lop$EOIq)
if(is.null(lop$cVars)) lop$EOIc <- NA else 
   lop$EOIc <- intersect(strsplit(lop$EOI, '\\,')[[1]], strsplit(lop$cVars, '\\,')[[1]])

if(!is.null(lop$qContr)) {
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

# combine the levels between the two region lists: NO! It seems to mess up the modeling wih brm
#levels(lop$dataTable$ROI1) <- union(levels(lop$dataTable$ROI1), levels(lop$dataTable$ROI2))
#levels(lop$dataTable$ROI2) <- union(levels(lop$dataTable$ROI1), levels(lop$dataTable$ROI2))

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
if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|Subj) + (1|ROI1:ROI2) + 
      (1|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE)) + 
      (1|mm(ROI1:Subj, ROI2:Subj, weights = cbind(w, w), scale=FALSE))')) else
   modelForm <- as.formula(paste('Y~', lop$model, '+(1|Subj)+(', lop$model, '|ROI1:ROI2)+(', 
      lop$model, '|mm(ROI1, ROI2, weights = cbind(w, w), scale=FALSE))'))

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

#union(levels(lop$dataTable$ROI1), levels(lop$dataTable$ROI2))

#lop <- list(outFN='Tara', EOI=c('Intercept', 'e4', 'site'), EOIc=c('e4', 'site'), EOIq='Intercept')
#lop[['EOIq']] <- 'Intercept'

ns <- lop$iterations*lop$chains/2
#nR <- nlevels(lop$dataTable$ROI1)
aa <- fixef(fm, summary = FALSE) # Population-Level Estimates
bb <- ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimates
if(nR != length(dimnames(bb$mmROI1ROI2)[[2]])) 
   cat('\n***** Warning: something strange about the ROIs! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

# compute P+
cnt <- function(x, ns) return(sum(x>0)/ns)

# extract region-pair posterior samples for an effect 'tm'
ww <- function(aa, bb, tm, nR) {
  ps0 <- array(apply(bb[['mmROI1ROI2']][,,tm], 2, "+", bb[['mmROI1ROI2']][,,tm]), c(ns, nR, nR))
  ps <- apply(ps0, c(2,3), '+', aa[,tm])
  
  dimnames(ps) <- list(1:ns, dimnames(bb$mmROI1ROI2)[[2]], dimnames(bb$mmROI1ROI2)[[2]])
  tmp <- ps
  
  sel1 <- match(dimnames(bb$`ROI1:ROI2`)[[2]], outer(dimnames(ps)[[2]],dimnames(ps)[[3]], function(x,y) paste(x,y,sep="_")))
  sel2 <- match(dimnames(bb$`ROI1:ROI2`)[[2]], outer(dimnames(ps)[[2]],dimnames(ps)[[3]], function(x,y) paste(y,x,sep="_")))
  
  ad <- function(tt,bb,s1,s2) {tt[s1] <- tt[s1] + bb; tt[s2] <- tt[s2] + bb; return(tt)}
  for(ii in 1:ns) tmp[ii,,] <- ad(tmp[ii,,], bb$`ROI1:ROI2`[ii,,tm], sel1, sel2)
  ps <- tmp
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)

# obtain summary informatin of posterior samples for RPs
vv <- function(ps, ns, nR) {
  mm <- apply(ps, c(2,3), mean)
  for(ii in 1:nR) for(jj in 1:nR) ps[,ii,jj] <- sqrt(2)*(ps[,ii,jj] - mm[ii,jj]) + mm[ii,jj]
  RP <- array(NA, dim=c(nR, nR, 8))
  RP[,,1] <- apply(ps, c(2,3), mean)
  RP[,,2] <- apply(ps, c(2,3), sd)
  RP[,,3] <- apply(ps, c(2,3), cnt, ns)
  RP[,,4:8] <- aperm(apply(ps, c(2,3), quantile, probs=c(0.025, 0.05, 0.5, 0.95, 0.975)), dim=c(2,3,1))
  dimnames(RP)[[1]] <- dimnames(ps)[[2]]
  dimnames(RP)[[2]] <- dimnames(ps)[[3]] 
  dimnames(RP)[[3]] <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(RP)
}

# full region pair result without thresholding
#xx <- vv(ww(aa, bb, 'Intercept', nR), ns, nR)
#subset(xx[,,c(1,8)], xx[,,'P+'] >= 0.975 | xx[,,'P+'] <= 0.025)

# graded thresholding
res <- function(bb, xx, pp, nd) {
   RP <- which(xx[,,'P+'] >= 1-pp | xx[,,'P+'] <= pp, arr.ind = T)
   RP <- RP[RP[,1] < RP[,2],]
   tmp <- data.frame(ROI1=factor(), ROI2=factor(), mean=factor(), SD=factor(), `P+`=factor(), check.names = FALSE)
   if(length(RP) > 2) {
      tmp <- cbind(dimnames(bb$mmROI1ROI2)[[2]][RP[,1]], dimnames(bb$mmROI1ROI2)[[2]][RP[,2]], 
         round(t(mapply(function(i, j) xx[i, j, 1:3], RP[,1], RP[,2])), nd)) 
      colnames(tmp)[1:2] <- c('ROI1', 'ROI2') 
      tmp <- data.frame(tmp, row.names = NULL, check.names = FALSE) } else
      if(length(RP)==2) {
         tmp <- c(dimnames(bb$mmROI1ROI2)[[2]][RP[1]], dimnames(bb$mmROI1ROI2)[[2]][RP[2]], round(xx[RP[1], RP[2], 1:3],3))
         #tmp <- paste(RP[1], RP[2], round(xx[RP[1], RP[2], 1:3], nd))
         #names(tmp)[1:2] <- c('ROI1', 'ROI2')
         tmp <- data.frame(t(tmp), row.names = NULL, check.names = FALSE) 
      }
   return(tmp)
}

# standardize the output
prnt <- function(pct, side, dat, fl, entity) {
   cat(sprintf('***** %i %s based on %i-sided %i uncertainty interval *****', 
      nrow(dat), entity, side, pct), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)
   if(nrow(dat) > 0) cat(capture.output(dat), file = paste0(fl, '.txt'), sep = '\n', append=TRUE) else
      cat('NULL', file = paste0(fl, '.txt'), sep = '\n', append=TRUE)
}

# matrix plot for RPs: assuming no diagonals for now
require(corrplot)
mPlot <- function(xx, fn) {
   mm <- xx[,,6]  # median
   pp <- xx[,,3]  # P+
   BC1 <- ((pp >= 0.975 ) | (pp <= 0.025))   # background color
   BC  <- ((pp >= 0.95 ) | (pp <= 0.05))   # background color
   BC2 <- (((pp > 0.9) & (pp < 0.95)) | ((pp < 0.1) & (pp > 0.05)))
   BC[BC  == T] <- "blue"
   BC[BC1 == T] <- "green"
   BC[BC  == F] <- "white"
   BC[BC2 == T] <- 'yellow'
   rng <- range(mm)
   diag(mm) <- NA  # diagonals are meaningful in the case of correlation matrix
   diag(BC) <- "white" # if the diagonal values shall be white
   ii <- !kronecker(diag(1, nrow(BC)), matrix(1, ncol=1, nrow=1))
   BC <- matrix(BC[ii], ncol = ncol(BC)-1)
   col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                "#4393C3", "#2166AC", "#053061"))
   pdf(paste0(fn, ".pdf"), width=8, height=8)
   corrplot(mm, method="circle", type = "full", is.corr = FALSE, bg=BC, tl.pos='lt', tl.col='black', col=rev(col2(200)), cl.pos='r', na.label = "square", na.label.col='white')
   dev.off()
}

########## region pair effects #############
# for intercept or quantitative variable
if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
   xx <- vv(ww(aa, bb, lop$EOIq[ii], nR), ns, nR)   
   cat(sprintf('===== Summary of region pair effects for %s =====', lop$EOIq[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   prnt(90, 1, res(bb, xx, 0.1, 3), lop$outFN, 'region pairs')
   prnt(95, 1, res(bb, xx, 0.05, 3), lop$outFN, 'region pairs')
   prnt(95, 2, res(bb, xx, 0.025, 3), lop$outFN, 'region pairs')
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   mPlot(xx, lop$EOIq[ii])
}

# for contrasts among quantitative variables
if(any(!is.null(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
   xx <- vv(ww(aa, bb, lop$qContrL[2*ii-1], nR)-ww(aa, bb, lop$qContrL[2*ii], nR), ns, nR)   
   cat(sprintf('===== Summary of region pair effects for %s vs %s =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   prnt(90, 1, res(bb, xx, 0.1, 3), lop$outFN, 'region pairs')
   prnt(95, 1, res(bb, xx, 0.05, 3), lop$outFN, 'region pairs')
   prnt(95, 2, res(bb, xx, 0.025, 3), lop$outFN, 'region pairs')
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   mPlot(xx, paste0(lop$qContrL[2*ii-1], 'vs', lop$qContrL[2*ii]))
}

# for factor
if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
   lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
   nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,,] <- ww(aa, bb, paste0(lop$EOIc[ii],jj), nR)
   ps[nl,,,] <- ww(aa, bb, 'Intercept', nR)
   psa <- array(0, dim=c(nl, ns, nR, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,,] <- ps[nl,,,] + ps[jj,,,]
      psa[nl,,,] <- psa[nl,,,] + ps[jj,,,]
   }
   psa[nl,,,] <- ps[nl,,,] - psa[nl,,,]  # reference level
   
   #oo <- array(apply(psa, 1, vv, ns, nR), dim=c(nR, nR, 8, nl))
   #dimnames(oo)[[3]] <- c('mean', 'sd', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
   
   cat(sprintf('===== Summary of region pair effects for %s =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- vv(psa[jj,,,], ns, nR)
      prnt(90, 1, res(bb, oo, 0.1, 3),  lop$outFN, 'region pairs')
      prnt(95, 1, res(bb, oo, 0.05, 3),  lop$outFN, 'region pairs')
      prnt(95, 2, res(bb, oo, 0.025, 3), lop$outFN, 'region pairs')
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      mPlot(oo, paste0(lop$EOIc[ii], '_', lvl[jj])) 
   }
   
   cat(sprintf('===== Summary of region pair effects for %s comparisons =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- vv(psa[jj,,,] - psa[kk,,,], ns, nR)
      prnt(90, 1, res(bb, oo, 0.1),   lop$outFN, 'region pairs')
      prnt(95, 1, res(bb, oo, 0.05),  lop$outFN, 'region pairs')
      prnt(95, 2, res(bb, oo, 0.025), lop$outFN, 'region pairs')
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      mPlot(oo, paste0(lop$EOIc[ii], '_', lvl[jj], 'vs', lvl[kk])) 
   }
}

########## region effects #############
# posterior samples at ROIs for a term
psROI <- function(aa, bb, tm, nR) {
  R0 <- apply(bb$mmROI1ROI2[,,tm], 2, '+', 0.5*aa[,tm])
  for(jj in 1:nR) {
      mm <-quantile(R0[,jj], probs=.5)
      R0[,jj] <- sqrt(2)*(R0[,jj] - mm)+mm
  }
  return(R0)  
}  

#gg <- psROI(aa, bb, 'Intercept', nR)

# summary for ROIs: nd - number of digits to output
sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(apply(R0, 2, mean), apply(R0, 2, sd), apply(R0, 2, cnt, ns), t(apply(R0, 2, quantile, 
      probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs,nd))
}
#gg <- sumROI(gg, ns, 3)

# for Intercept and quantitative variables
if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
   cat(sprintf('===== Summary of region effects for %s =====', lop$EOIq[ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   gg <- sumROI(psROI(aa, bb, lop$EOIq[ii], nR), ns, 3)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
}

# for contrasts among quantitative variables
if(any(!is.null(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   gg <- sumROI(psROI(aa, bb, lop$qContrL[2*ii-1], nR) - psROI(aa, bb, lop$qContrL[2*ii], nR), ns, 3)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
}

# for factor
if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
   lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
   nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(lop$EOIc[ii],jj), nR)
   ps[nl,,] <- psROI(aa, bb, 'Intercept', nR) # Intercept: averge effect 
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,] 
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level
   
   oo <- apply(psa, 1, sumROI, ns, 3)
   
   cat(sprintf('===== Summary of region effects for %s =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
   
   cat(sprintf('===== Summary of region effects for %s comparisons =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 3)
      cat(capture.output(oo), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
}

# save it again
save.image(file=paste0(lop$outFN, ".RData"))
cat("\nCongratulations! The above results are saved in file ", lop$lop$outFN, "\n\n", sep='')

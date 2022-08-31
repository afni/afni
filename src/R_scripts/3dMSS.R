#!/usr/bin/env AFNI_Batch_R

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
  #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE))
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dMSS'
# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin 3dMSS Input functions ################################
#################################################################################

#The help function for 3dMSS batch (AFNI-style script mode)
help.MSS.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <-
'
             ================== Welcome to 3dMSS ==================
       Program for Voxelwise Multilevel Smoothing Spline (MSS) Analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.16, Aug 13, 2022
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892, USA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Introduction
------

 Multilevel Smoothing-Spline (MSS) Modeling 

 The linearity assumption surrounding a quantitative variable in common 
 practice may be a reasonable approximation especially when the variable 
 is confined within a narrow range, but can be inappropriate under some 
 circumstances when the variable\'s effect is non-monotonic or tortuous. 
 As a more flexible and adaptive approach, multilevel smoothing splines 
 (MSS) offers a more powerful analytical tool for population-level 
 neuroimaging data analysis that involves one or more quantitative
 predictors. More theoretical discussion can be found in

 Chen et al. (2020). Beyond linearity: Capturing nonlinear relationships 
 in neuroimaging. https://doi.org/10.1101/2020.11.01.363838

 To be able to run 3dMSS, one needs to have the following R packaages 
 installed: "gamm4" and "snow". To install these R packages, run the
 following command at the terminal:

 rPkgsInstall -pkgs "gamm4,snow"

 Alternatively you may install them in R:

 install.packages("gamm4")
 install.packages("snow")

 It is best to go through all the examples below to get hang of the MSS
 scripting interface. Once the 3dMSS script is constructed, it can be run 
 by copying and pasting to the terminal. Alternatively (and probably better)
 you save the script as a text file, for example, called MSS.txt, and execute
 it with the following (assuming on tc shell),

 nohup tcsh -x MSS.txt &

 or,

 nohup tcsh -x MSS.txt > diary.txt &

 or,

 nohup tcsh -x MSS.txt |& tee diary.txt &

 The advantage of the latter commands is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.\n'

  ex1 <-
"Example 1 --- simplest case: one group of subjects with a between-subject 
  quantitative variable that does not vary within subject. MSS analysis is 
  set up to model the trajectory or trend along age, and can be specified 
  through the option -mrr, which is solved via a model formuation of ridge 
  regression. Again, the following exemplary script assumes that 'age' is 
  a between-subjects variable (not varying within subject):

   3dMSS -prefix MSS -jobs 16                     \\
          -mrr 's(age)'                           \\
          -qVars 'age'                            \\
          -mask myMask.nii                        \\
          -bounds  -2 2                           \\
          -prediction @pred.txt                   \\
          -dataTable  @data.txt

  The function 's(age)' indicates that 'age' is modeled via a smooth curve.
  No empty space is allowed in the model formulation. With the option 
  -bounds, values beyond [-2, 2] will be treated as outliers and considered 
  as missing. If you want to set a range, choose one that make sense with 
  your specific input data. 

   The file pred.txt lists all the expl1anatory variables (excluding lower-level variables
   such as subject) for prediction. The file should be in a data.frame format as below:

   label  age 
   t1      1   
   t2      2   
   t3      3   
    ...
   t8      8  
   t9      9 
   t10    10
    ...

   The file data.txt stores the information for all the variables and input data in a
   data.frame format. For example:

   Subj   age   InputFile
   S1      1   ~/alex/MSS/S1.nii
   S2      2   ~/alex/MSS/S2.nii
    ...

  In the output the first sub-brick shows the statistical evidence in the 
  form of chi-square distribution with 2 degrees of freedom (2 DFs do not mean
  anything, just for the convenience of information coding). This sub-brick is
  the statistical evidence for the trejectory of the group. If you want to
  estimate the trend at the population level, use the option -prediction with a
  table that codes the ages you would like to track the trend. In the output
  there is one predicted value for each age plus the associated uncertainty
  (standard error). For example, with 10 age values, there will be 10 predicted
  values plus 10 standard errors. The sub-bricks for prediction and standard
  errors are interleaved.
   \n"

   ex2 <-
"Example 2 --- Largely same as Example 1, but with 'age' as a within-subject 
  quantitative variable (varying within each subject). The model is better 
  specified by replacing the line of -mrr in Example 1 with the following 
  two lines:

          -mrr 's(age)+s(Subj,bs=\"re\")'         \\
          -vt Subj 's(Subj)'                      \\

  The second term 's(Subj,bs=\"re\")' in the model specification means that
  each subject is allowed to have a varying intercept or random effect ('re'). 
  To estimate the smooth trajectory through the option -prediction, the option
  -vt has to be included in this case to indicate the varying term (usually 
  subjects). That is, if prediction is desirable, one has to explicitly
  declare the variable (e.g., Subj) that is associated with the varying term
  (e.g., s(Subj)). No empty space is allowed in the model formulation and the
  the varying term. 

  The full script version is

   3dMSS -prefix MSS -jobs 16                     \\
          -mrr 's(age)+s(Subj,bs=\"re\")'         \\
          -vt Subj 's(Subj)'                      \\
          -qVars 'age'                            \\
          -mask myMask.nii                        \\
          -bounds  -2 2                           \\
          -prediction @pred.txt                   \\
          -dataTable  @data.txt

  All the rest remains the same as Example 1.

  Alternatively, this model with varying subject-level intercept can be
  specified with

          -lme 's(age)'                        \\
          -ranEff 'list(Subj=~1)'                      \\

  which is solved through the linear mixed-effect (lme) platform. The -vt is
  not needed when making prediction through the option -prediction. The two
  specifications, -mrr and -lme, would render similar results, but the 
  runtime may differ depending on the amount of data and model complexity.
   \n"

   ex3 <-
"Example 3 --- two groups and one quantitative variable (age). MSS analysis is 
  set up to compare the trajectory or trend along age between the two groups,
  which are quantitatively coded as -1 and 1. For example, if the two groups
  are females and males, you can code females as -1 and males as 1. The following
  script applies to the situation when  the quantitative variable does not vary 
  within subject, 

  3dMSS -prefix MSS -jobs 16                     \\
          -mrr 's(age)+s(age,by=grp)'             \\
          -qVars 'age'                            \\
          -mask myMask.nii                        \\
          -bounds  -2 2                           \\
          -prediction @pred.txt                   \\
          -dataTable  @data.txt

  On the other hand, go with the script below when the quantitative variable 
  varies within subject,

  3dMSS -prefix MSS -jobs 16                     \\
          -mrr 's(age)+s(age,by=grp)+s(Subj,bs=\"re\")' \\
          -vt  Subj 's(Subj)'                \\
          -qVars 'age'                            \\
          -mask myMask.nii                        \\
          -bounds  -2 2                           \\
          -prediction @pred.txt                   \\
          -dataTable  @data.txt

  or an LME version:

  3dMSS -prefix MSS -jobs 16                     \\
          -lme 's(age)+s(age,by=grp)'             \\
          -ranEff 'list(Subj=~1)'                      \\
          -qVars 'age'                            \\
          -mask myMask.nii                        \\
          -bounds  -2 2                           \\
          -prediction @pred.txt                   \\
          -dataTable  @data.txt
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
   
#Change command line arguments into an options list
read.MSS.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output file name. For AFNI format, provide prefix only,",
   "         with no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached (otherwise the output would be saved in AFNI format).\n", sep = '\n'
      ) ),

      '-mask' = apl(n=1,  d = NA, h = paste(
   "-mask MASK: Process voxels inside this mask only.\n",
   "         Default is no masking.\n"
                     ) ),

      '-jobs' = apl(n = 1, d = 1, h = paste(
   "-jobs NJOBS: On a multi-processor machine, parallel computing will speed ",
   "         up the program significantly.",
   "         Choose 1 for a single-processor computer.\n", sep = '\n'
                     ) ),

      '-IF' = apl(n = 1, d = NA,  h = paste(
   "-IF var_name: var_name is used to specify the column name that is designated for",
   "        input files of effect estimate. The default (when this option is not invoked",
   "        is 'InputFile', in which case the column header has to be exactly as 'InputFile'",
   "        This input file for effect estimates has to be the last column.\n", sep = '\n'
                     ) ),

      '-lme' = apl(n = 1, d = 1, h = paste(
   "-lme FORMULA: Specify the fixed effect components of the model. The",
   "         expression FORMULA with more than one variable has to be surrounded",
   "         within (single or double) quotes. Variable names in the formula",
   "         should be consistent with the ones used in the header of -dataTable.",
   "         See examples in the help for details.\n", sep = '\n'
             ) ),

      '-ranEff' = apl(n = 1, d = 1, h = paste(
   "-ranEff FORMULA: Specify the random effect components of the model. The",
   "         expression FORMULA with more than one variable has to be surrounded",
   "         within (single or double) quotes. Variable names in the formula",
   "         should be consistent with the ones used in the header of -dataTable.",
   "         In the MSS context the simplest model is \"list(Subj=~1)\" in which the",
   "         varying or random effect from each subject is incorporated in the model.",
   "         Each random-effects factor is specified within paratheses per formula",
   "         convention in R.\n", sep = '\n'
             ) ),

      '-mrr' = apl(n = 1, d = 1, h = paste(
   "-mrr FORMULA: Specify the model formulation through multilevel smoothing splines.",
   "         Expression FORMULA with more than one variable has to be surrounded",
   "         within (single or double) quotes. Variable names in the formula",
   "         should be consistent with the ones used in the header of -dataTable.",
   "         The nonlinear trajectory is specified through the expression of s(x,k=?)",
   "         where s() indicates a smooth function, x is a quantitative variable with",
   "         which one would like to trace the trajectory and k is the number of smooth",
   "         splines (knots). The default (when k is missing) for k is 10, which is good",
   "         enough most of the time when there are more than 10 data points of x. When",
   "         there are less than 10 data points of x, choose a value of k slightly less",
   "         than the number of data points.\n", sep = '\n'
             ) ),

       '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dMSS.dbg.AFNI.args in the current directory",
   "          so that debugging can be performed.\n", sep='\n')),

       '-bounds' = apl(n=2, h = paste(
   "-bounds lb ub: This option is for outlier removal. Two numbers are expected from",
   "         the user: the lower bound (lb) and the upper bound (ub). The input data will",
   "         be confined within [lb, ub]: any values in the input data that are beyond",
   "         the bounds will be removed and treated as missing. Make sure the first number",
   "         less than the second. You do not have to use this option to censor your data!\n", sep='\n')),

       '-vt' = apl(n=2, h = paste(
   "-vt var formulation: This option is for specifying varying smoothing terms. Two components",
   "         are required: the first one 'var' indicates the varaible (e.g., subject) around",
   "         which the smoothing will vary while the second component specifies the smoothing",
   "         formulation (e.g., s(age,subject)). When there is no varying smoothing terms (e.g.,",
   "         no within-subject variables), do not use this option.\n", sep='\n')),

      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars variable_list: Identify quantitative variables (or covariates) with",
   "         this option. The list with more than one variable has to be",
   "         separated with comma (,) without any other characters such as",
   "         spaces and should be surrounded within (single or double) quotes.",
   "         For example, -qVars \"Age,IQ\"",
   "         WARNINGS:",
   "         1) Centering a quantitative variable through -qVarsCenters is",
   "         very critical when other fixed effects are of interest.",
   "         2) Between-subjects covariates are generally acceptable.",
   "         However EXTREME caution should be taken when the groups",
   "         differ substantially in the average value of the covariate.",
   "         \n",
             sep = '\n'
             ) ),

     '-prediction' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-prediction TABLE: Provide a data table so that predicted values could be generated for",
   "graphical illustration. Usually the table should contain similar structure as the input",
   "file except that 1) reserve the first column for effect labels which will be used for",
   "sub-brick names in the output for those predicted values; 2) columns for those varying",
   "smoothing terms (e.g., subject) and response variable (i.e., Y) should not be includes.",
   "Try to specify equally-spaced values with a small for the quantitative variable of",
   "modeled trajectory (e.g., age) so that smooth curves could be plotted after the",
   "analysis. See Examples in the help for a couple of specific tables used for predictions.\n", sep = '\n'
          ) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure with a header as the first line.\n",
   "         NOTE:\n",
   "         1) This option has to occur last in the script; that is, no other",
   "         options are allowed thereafter. Each line should end with a backslash",
   "         except for the last line.\n",
   "         2) The order of the columns should not matter except that the last",
   "         column has to be the one for input files, 'InputFile'. Each row should",
   "         contain only one input file in the table of long format (cf. wide format)",
   "         as defined in R. Input files can be in AFNI, NIfTI or surface format.",
   "         AFNI files can be specified with sub-brick selector (square brackets",
   "         [] within quotes) specified with a number or label.\n",
   "         3) It is fine to have variables (or columns) in the table that are",
   "         not modeled in the analysis.\n",
   "         4) When the table is part of the script, a backslash is needed at the end",
   "         of each line to indicate the continuation to the next line. Alternatively,",
   "         one can save the context of the table as a separate file, e.g.,",
   "         calling it table.txt, and then in the script specify the data with",
   "         '-dataTable @table.txt'. However, when the table is provided as a separate",
   "         file, do NOT put any quotes around the square brackets for each sub-brick,",
   "         otherwise the program would not properly read the files, unlike the",
   "         situation when quotes are required if the table is included as part of the",
   "         script. Backslash is also not needed at the end of each line, but it would",
   "         not cause any problem if present. This option of separating the table from",
   "         the script is useful: (a) when there are many input files so that",
   "         the program complains with an 'Arg list too long' error; (b) when",
   "         you want to try different models with the same dataset.\n",
             sep = '\n'
                     ) ),

      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" ),

       '-cio' = apl(n=0, h = paste(
   "-cio: Use AFNI's C io functions, which is default. Alternatively -Rio",
   "         can be used.\n", sep='\n')),
       '-Rio' = apl(n=0, h = "-Rio: Use R's io functions. The alternative is -cio.\n")

         )
   #browser()
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   if (verb) show.AFNI.args(ops, verb=0, hstr='')
   if (is.null(ops))
      errex.AFNI('Error parsing arguments. See 3dMSS -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$cutoff <- 0
      lop$maskFN <- NA
      lop$IF     <- 'InputFile' #default

      lop$lme <- NULL
      lop$mrr  <- NULL
      lop$ranEff <- NULL 
      lop$qVars  <- NA
      lop$bounds <- NULL
      lop$vt     <- NULL
      #lop$qVarCenters <- NA
      lop$dataTable   <- NULL
      lop$prediction  <- NULL

      lop$iometh <- 'clib'
      lop$dbgArgs  <- FALSE # for debugging purpose
      lop$verb <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             mask   = lop$maskFN <- ops[[i]],
             jobs   = lop$nNodes <- ops[[i]],
	     lme = lop$lme <- ops[[i]],
	     mrr  = lop$mrr  <- ops[[i]],
             ranEff = lop$ranEff <- ops[[i]],
	     IF     = lop$IF     <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             bounds = lop$bounds <- ops[[i]],
             vt     = lop$vt     <- ops[[i]],
             #qVarCenters = lop$qVarCenters <- ops[[i]],
             dataTable   = lop$dataTable   <- dataTable.AFNI.parse(ops[[i]]),
             prediction  = lop$prediction  <- dataTable.AFNI.parse(ops[[i]]),

             help = help.MSS.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }

   return(lop)
}# end of read.MSS.opts.batch

#Change options list to 3dMSS variable list
process.MSS.opts <- function (lop, verb = 0) {
   if(is.null(lop$outFN)) errex.AFNI(c("Output filename not specified! Add filename with -prefix.\n"))
   an <- parse.AFNI.name(lop$outFN)
   if(an$type == "NIML") {
      if(file.exists(lop$outFN)) errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
   } else if(file.exists(paste(lop$outFN,"+tlrc.HEAD", sep="")) ||
     file.exists(paste(lop$outFN,"+tlrc.BRIK", sep="")) ||
     file.exists(paste(lop$outFN,"+orig.HEAD", sep="")) ||
     file.exists(paste(lop$outFN,"+orig.BRIK", sep=""))) {
     errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
     return(NULL)
   }
   #browser()
   if(an$type != 'BRIK' && lop$iometh != 'clib')
      errex.AFNI(c('Must of use -cio option with any input/output ',
                   'format other than BRIK'))
   # assume the quantitative variables are separated by + here
   if(!is.na(lop$qVars)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]

   if(!(is.null(lop$bounds))) {
      if(lop$bounds[1] > lop$bounds[2])
         errex.AFNI(paste0('Incorrect setting with option -bounds! The lower bound ', lop$bounds[1],
            ' should be smaller than the upper bound ', lop$bounds[2], '!'))
   }

   len <- length(lop$dataTable)
   wd <- which(lop$dataTable == lop$IF)  # assuming the input file is the last column here!
   hi <- len / wd - 1

   if(len %% wd != 0)
      errex.AFNI(paste('The content under -dataTable is not rectangular!', len, wd)) else {
      lop$dataStr <- NULL
      for(ii in 1:wd) lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      for(ii in 1:(wd-1)) if(sapply(lop$dataStr, class)[ii] == "character")
         lop$dataStr[,ii] <- as.factor(lop$dataStr[,ii])

   }

   if(!is.null(lop$prediction)) {
      tmp <- lop$prediction %in% names(lop$dataStr)
      len <- length(lop$prediction)
      wd  <- sum(tmp)+1  # 
      hi <- len / wd - 1
      if(len %% wd != 0)
         errex.AFNI(paste('The content under -prediction is not rectangular!', len, wd)) else {
         lop$Pred <- NULL
         for(ii in 1:wd) lop$Pred <- data.frame(cbind(lop$Pred, lop$prediction[seq(wd+ii, len, wd)]))
         names(lop$Pred) <- lop$prediction[1:wd]
         if(!is.na(lop$qVars)) for(jj in lop$QV) lop$Pred[,jj] <- as.numeric(as.character(lop$Pred[,jj]))
         lop$nr <- nrow(lop$Pred)
      }
   }

   if(!is.null(lop$vt)) {
      #browser()
      if(lop$vt[1] %in% names(lop$Pred)) errex.AFNI(c("The varying smoothing unit ", lop$vt[1], " is already a variable column in the prediction file ", lop$prediction, "!\n")) else
      if(!lop$vt[1] %in% names(lop$dataStr)) errex.AFNI(c("The varying smoothing unit ", lop$vt[1], " is not a column in the data table. Check your spelling!\n")) else {
         #nr <- nrow(lop$Pred)
         lop$Pred <- lop$Pred[rep(seq_len(lop$nr), times=nlevels(lop$dataStr[, lop$vt[1]])), ]
         lop$Pred[,lop$vt[1]] <- as.factor(rep(levels(lop$dataStr[,lop$vt[1]]), each = lop$nr))
      }
   }

   if(lop$iometh == 'Rlib') {
      lop$outFN <- paste(lop$outFN, "+tlrc", sep="")
      if(!is.null(lop$resid)) lop$resid <- paste(lop$resid, "+tlrc", sep="")
   } else {
      if(an$type == "BRIK" && an$ext == "" && is.na(an$view))
         lop$outFN <- paste(lop$outFN, "+tlrc", sep="")
      if (exists.AFNI.name(lop$outFN) ||
          exists.AFNI.name(modify.AFNI.name(lop$outFN,"view","+tlrc")))
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
      if(!is.null(lop$resid)) {
         if(an2$type == "BRIK" && an2$ext == "" && is.na(an2$view))
            lop$resid <- paste(lop$resid, "+tlrc", sep="")
         if (exists.AFNI.name(lop$resid) ||
             exists.AFNI.name(modify.AFNI.name(lop$resid,"view","+tlrc")))
             errex.AFNI(c("File ", lop$resid, " exists! Try a different name.\n"))
      }
   }

   if(lop$nNodes < 1) lop$nNodes <- 1

   if(!is.na(lop$maskFN)) {
      if(verb) cat("Will read ", lop$maskFN,'\n')
      if(is.null(mm <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk)) {
         warning("Failed to read mask", immediate.=TRUE)
         return(NULL)
      }
      #if(dim(mm)[3] == 1) { # when the mask is a slice)
      #   lop$maskData <- mm[,,,1]
      #   dim(lop$maskData) <- c(dim(lop$maskData), 1)
      #} else lop$maskData <- mm[,,,1]
      #lop$maskData <- mm$brk
      lop$maskData <- mm
      if(verb) cat("Done read ", lop$maskFN,'\n')
      if(dim(mm)[4] > 1) stop("More than 1 sub-brick in the mask file!")
   }
   #if(!is.na(lop$maskFN))
   #   if(!all(dim(lop$maskData)==lop$myDim[1:3]))
   #      stop("Mask dimensions don't match the input files!")
   return(lop)
}
# process.MSS.opts(lop, 0)

#################################################################################
################# MSS Computation functions ##############################
#################################################################################

# MSS: multilevel smoothing splines using gam() in mgcv
#runMSS <- function(myData, DM, tag) {
#   #browser()
#   Stat <- rep(0, lop$nBrk)
#   if(!all(myData == 0)) {
#      #DM$yy <- myData
#      fm <- NULL
#      options(warn=-1)
#      lop$mm$mf$yy <- myData
#      lop$mm$y    <- myData
#      #try(fm <- gam(lop$mrr, data=DM, method='REML'), silent=TRUE)
#      #try(fm <- gam(lop$mrr, data=DM), silent=TRUE)
#      try(fm <- gam(G=lop$mm), silent=TRUE)
#      if(!is.null(fm)) { # model successful
#         tmp <- NULL;
#	 ll <- c(t(summary(fm)$p.table[,c('Estimate', 't value')])) # parameters
#	 pp <- summary(fm)$s.table[,'p-value'] # smooths
#         pp <- replace(pp, pp<1e-16, 1e-16) # prevent 0 p-value in the output, causing NANs in chi-sq 
#         if(is.null(lop$vt)) try(tmp <- predict(fm, lop$Pred, se.fit = T), silent=TRUE) else
#            try(tmp <- predict(fm, lop$Pred, se.fit = T, exclude=lop$vt[2]), silent=TRUE)
#         if(!is.null(tmp)) { # prediction successful
#            if(is.null(lop$vt)) { 
#               #Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
#               Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
#            } else
#            #Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit[1:lop$nr], 
#            Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit[1:lop$nr],
#                      tmp$se.fit[1:lop$nr])))
#         } else Stat[1:(length(ll)+length(pp))] <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq)
#         #Stat[1:(length(ll)+length(pp))] <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq)
#      }
#   }
#   return(Stat)
#}
# runMSS(inData[30,30,30,], lop$dataStr, 0)

runMSS <- function(myData, DM, tag) {
   #browser()
   Stat <- rep(0, lop$nBrk)
   if(!all(myData == 0)) {
      DM$yy <- myData
      fm <- NULL
      options(warn=-1)
      #lop$mm$mf$yy <- myData
      #lop$mm$y    <- myData
      try(fm <- gam(lop$mrr, data=DM, method='REML'), silent=TRUE)
      #try(fm <- gam(lop$mrr, data=DM), silent=TRUE)
      #try(fm <- gam(G=lop$mm), silent=TRUE)
      if(!is.null(fm)) { # model successful
         tmp <- NULL;
         ll <- c(t(summary(fm)$p.table[,c('Estimate', 't value')])) # parameters
         pp <- summary(fm)$s.table[,'p-value'] # smooths
         pp <- replace(pp, pp<1e-16, 1e-16) # prevent 0 p-value in the output, causing NANs in chi-sq
         if(is.null(lop$vt)) try(tmp <- predict(fm, lop$Pred, se.fit = T), silent=TRUE) else
            try(tmp <- predict(fm, lop$Pred, se.fit = T, exclude=lop$vt[2]), silent=TRUE)
         if(!is.null(tmp)) { # prediction successful
            if(is.null(lop$vt)) {
               Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
               #Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
            } else
            Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit[1:lop$nr],
            #Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq, c(rbind(tmp$fit[1:lop$nr],
                      tmp$se.fit[1:lop$nr])))
         } else #Stat[1:(length(ll)+length(pp))] <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm)$r.sq)
         Stat[1:(length(ll)+length(pp))] <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm)$r.sq)
      }
   }
   return(Stat)
}
# runMSS(inData[30,30,30,], lop$dataStr, 0)
# lop$dataStr$yy <- inData[25, 45, 31, ]
# m1 <- gam(lop$mmr, data=lop$dataStr, method='REML')

# modeling through mixed-effects approach with gamm()
runLME <- function(myData, DM, tag) {
   #browser()
   Stat <- rep(0, lop$nBrk)
   if(!all(myData == 0)) { #DM$yy <- myData
      fm <- NULL
      options(warn=-1)
      DM$yy <- myData
      try(fm <- gamm(lop$lme, data=DM, random=lop$ranEff, method='REML'), silent=TRUE)
      #try(fm <- gamm(lop$lme, data=DM, random=lop$ranEff), silent=TRUE)
      if(!is.null(fm)) {
         tmp <- NULL;
	 ll <- c(t(summary(fm$gam)$p.table[,c('Estimate', 't value')]))
	 if(!is.null(summary(fm$gam)$s.table)) {
            pp <- summary(fm$gam)$s.table[,'p-value'] # smooths
            pp <- replace(pp, pp<1e-16, 1e-16) # prevent 0 p-value in the output, causing NANs in chi-sq
	 }
         if(is.null(lop$vt)) try(tmp <- predict(fm$gam, lop$Pred, se.fit = T), silent=TRUE) else
            try(tmp <- predict(fm$gam, lop$Pred, se.fit = T, exclude=lop$vt[2]), silent=TRUE)
         if(is.null(summary(fm$gam)$s.table)) { # having no smooth terms
            if(!is.null(tmp)) { # prediction successful
               Stat <- c(ll, summary(fm$gam)$r.sq, c(rbind(tmp$fit[1:lop$nr],
                      tmp$se.fit[1:lop$nr])))
            } else Stat[1:length(ll)] <- c(ll, summary(fm$gam)$r.sq)
         } else { # having smooth terms
            pp <- summary(fm$gam)$s.table[,'p-value']
	    pp <- replace(pp, pp<1e-16, 1e-16) # prevent 0 p-value in the output, causing NANs in chi-sq
            if(!is.null(tmp)) { # prediction successful
               if(is.null(lop$vt)) {
                  Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm$gam)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
                  #Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm$gam)$r.sq, c(rbind(tmp$fit, tmp$se.fit)))
               } else
               #Stat <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm$gam)$r.sq, c(rbind(tmp$fit[1:lop$nr],
               #       tmp$se.fit[1:lop$nr])))
               Stat <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm$gam)$r.sq, c(rbind(tmp$fit[1:lop$nr], tmp$se.fit[1:lop$nr])))
            } else #Stat[1:(length(ll)+length(pp))] <- c(ll, qnorm(pp/2, lower.tail = F), summary(fm$gam)$r.sq)
            Stat[1:(length(ll)+length(pp))] <- c(ll, qchisq(pp, 2, lower.tail = F), summary(fm$gam)$r.sq)
         } # if(is.null(summary(fm$gam)$s.table))
      } # if(!is.null(fm))
   } # if(!all(myData == 0))
   return(Stat)
}
# runLME(inData[30,30,30,], lop$dataStr, 0)
# lop$dataStr$yy <- inData[25, 45, 31, ]
# m1 <- gamm(lop$lme, random=lop$ranEff, data=lop$dataStr, method='REML')

#################################################################################
########################## Begin MSS main ######################################
#################################################################################

   if(!exists('.DBG_args')) {
      args = (commandArgs(TRUE))
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dMSS.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.MSS(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n",
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.MSS.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)

   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.MSS.opts.batch(args, verb = 0)))
         stop('Error parsing input')

      #str(lop);
      if(is.null(lop <- process.MSS.opts(lop, verb = lop$verb)))
         stop('Error processing input')

   }
   #if(lop$verb > 1) {
      #Too much output, big dump of header structs of input dsets..
   #   str(lop)
   #}


########################################################################

#if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])

# effect coding leads to the same type III as SAS
options(contrasts = c("contr.sum", "contr.poly"))

# standardize the names for Y, ROI and subject
#names(lop$dataStr)[names(lop$dataStr)==lop$Subj] <- 'Subj'
names(lop$dataStr)[names(lop$dataStr)==lop$IF] <- 'InputFile'

# Maybe not list for these two, or yes?
#lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)

# center on user-speficied value or mean
#if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters)))
#   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F)[,,drop=T] else
#   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)[,,drop=T]

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of data structure *****\n')

nF <- dim(lop$dataStr[1])[1] # number of input files
cat(nF, 'response values\n')

if(dim(lop$dataStr)[2] > 2) for(ii in 2:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':',
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'numeric' | class(lop$dataStr[,ii]) == 'matrix')  # numeric doesn't work
   cat(length(lop$dataStr[,ii]), 'values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')

cat('***** End of data structure information *****\n')
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('Reading input files now...\n\n')

#errex.AFNI(c('OK, here you go!'))

# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(lop$dataStr[1, lop$IF], verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData

# Read in all input files
inData <- unlist(lapply(lapply(lop$dataStr[, lop$IF], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
tryCatch(dim(inData) <- c(dimx, dimy, dimz, nF), error=function(e)
   errex.AFNI(c("At least one of the input files has different dimensionsr:\n",
   "either (1) numbers of voxels along X, Y, Z axes are different across files;\n",
   "or     (2) some input files have more than one value per voxel.\n",
   "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
   "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files for effect estimates: Done!\n\n')

# masking
if(!is.na(lop$maskFN)) {
   if(!all(c(dimx, dimy, dimz)==dim(lop$maskData)[1:3])) stop("Mask dimensions don't match the input files!")
   lop$maskData <- array(lop$maskData, dim=c(dimx, dimy, dimz))
   inData <- array(apply(inData, 4, function(x) x*(abs(lop$maskData)>tolL)), dim=c(dimx,dimy,dimz,nF))
}

# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's

###############################

# show the range of input data
rg <- range(inData)
cat(paste0('Range of input data: [', sprintf(rg[1], fmt = '%#.3f'), ', ', sprintf(rg[2], fmt = '%#.3f'), ']\n'))

# outlier removal
if(!is.null(lop$bounds)) {
   inData[inData > lop$bounds[2]] <- NA
   inData[inData < lop$bounds[1]] <- NA
   cat(paste0('Input data confined within [', lop$bounds[1], ', ', lop$bounds[2], ']\n\n'))
}

require(gamm4)

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or something else\n')
cat('is likely inappropriate.\n\n')

# pick up a test voxel
if(!is.na(lop$maskFN)) {
  idx <- which(lop$maskData == 1, arr.ind = T)
  idx <- idx[floor(dim(idx)[1]/2),1:3]
  xinit <- idx[1]; yinit <- idx[2]; zinit <- idx[3]
  ii <- xinit; jj <- yinit; kk <- zinit
} else {
  xinit <- dimx%/%3
  if(dimy==1) {xinit <-1; yinit <- 1} else yinit <- dimy%/%3
  if(dimz==1) {xinit <-1; zinit <- 1} else zinit <- dimz%/%3
  ii <- xinit; jj <- yinit; kk <- zinit
}

#errex.AFNI(c('OK, here you go!'))

fm<-NULL
if(is.null(lop$mrr)) {
   lop$lme <- as.formula(paste0('yy~',lop$lme))
   if(!is.null(lop$ranEff)) #lop$ranEff <- as.formula(paste0('~',lop$ranEff))
      lop$ranEff <- eval(parse(text=lop$ranEff))
} else lop$mrr <- as.formula(paste0('yy~',lop$mrr))
#chi_DF <- NULL
while(is.null(fm)) {
   lop$dataStr$yy <- inData[ii, jj, kk,]
   options(warn=-1)
   if(is.null(lop$mrr)) { # use gamm
      tt <- system.time(try(fm <- gamm(lop$lme, data=lop$dataStr, random=lop$ranEff, method='REML'), silent=TRUE))
      #tt <- system.time(try(fm <- gamm(lop$lme, data=lop$dataStr, random=lop$ranEff), silent=TRUE))
      if(!is.null(fm)) {
         print(sprintf("Runtime per spatial element: %.3f sec", tt[3])); cat('\n')
         #lop$mm <- gamm(lop$lme, data=lop$dataStr, random=lop$ranEff)
         if(is.null(summary(fm$gam)$s.table)) lop$nBrk <- 2*nrow(summary(fm$gam)$p.table)+1 else
            lop$nBrk <- 2*nrow(summary(fm$gam)$p.table)+nrow(summary(fm$gam)$s.table)+1  # +1 for R.sq
         if(!is.null(lop$prediction)) {
            tmp <- NULL
            if(is.null(lop$vt)) try(tmp <- predict(fm$gam, lop$Pred, se.fit = T), silent=TRUE) else
               try(tmp <- predict(fm$gam, lop$Pred, exclude=lop$vt[2], se.fit = T), silent=TRUE)
               if(is.null(tmp)) fm <- NULL else lop$nBrk <- lop$nBrk + 2*lop$nr
         #   if(is.null(lop$vt) lop$nBrk <- lop$nBrk + 2*length(tmp$fit) else
         #   lop$nBrk <- lop$nBrk + 2*lop$nr
         }
      }
   } else { # use gam() in mgcv
      tt <- system.time(try(fm <- gam(lop$mrr, data=lop$dataStr, method='REML'), silent=TRUE))
      #tt <- system.time(try(fm <- gam(lop$mrr, data=lop$dataStr), silent=TRUE))
      if(!is.null(fm)) {
         print(sprintf("Runtime per spatial element: %.3f sec", tt[3]))
         lop$mm <- gam(lop$mrr, fit=FALSE, data=lop$dataStr, method='REML')
         tmp <- NULL; try(tmp <- summary(fm), silent=TRUE)
         if(is.null(tmp)) fm <- NULL else {
            lop$nBrk <- 2*nrow(summary(fm)$p.table)+nrow(summary(fm)$s.table)+1  # +1 for R.sq
            if(!is.null(lop$prediction)) {
               tmp <- NULL
               #if(is.null(lop$vt)) try(tmp <- predict(fm$gam, lop$Pred, se.fit = T), silent=TRUE) else
               try(tmp <- predict(fm, lop$Pred, exclude=lop$vt[2], se.fit = T), silent=TRUE)
               if(is.null(tmp)) fm <- NULL else lop$nBrk <- lop$nBrk + 2*lop$nr
            #   if(is.null(lop$vt) lop$nBrk <- lop$nBrk + 2*length(tmp$fit) else
            #   lop$nBrk <- lop$nBrk + 2*lop$nr
            }
         }
      }
   }
   if(!is.null(fm))  {
      print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
   } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')
      cat('Possible reasons:\n\n')
      cat('0) Make sure that R package lmerTest has been installed. See the 3dMSS\n')
      cat('help documentation for more details.\n\n')
      cat('1) Inappropriate model specification with options -mrr, or -qVars.\n\n')
      cat('2) In correct specifications for random effect with -ranEff.\n\n')
      cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n\n')
      cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
      cat('named Scanner in model specification and then listed as scanner in the table hader\n')
      cat('would cause grief for 3dMSS.\n')
      errex.AFNI("Quitting due to model test failure...")
   }
}

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

options(contrasts = c("contr.sum", "contr.poly"))

if(dimy==1 & dimz==1) { # 1D data
   nSeg <- 20
   # drop the dimensions with a length of 1
   inData <- inData[, , ,]
   # break into 20 segments, leading to 5% increamental in parallel computing
   dimx_n <- dimx%/%nSeg + 1
   # number of datasets need to be filled
   fill <- nSeg-dimx%%nSeg
   # pad with extra 0s
   inData <- rbind(inData, array(0, dim=c(fill, NoFile)))
   # break input multiple segments for parrel computation
   dim(inData) <- c(dimx_n, nSeg, NoFile)
   Stat <- array(0, dim=c(dimx_n, nSeg, lop$nBrk))
   if (lop$nNodes==1) { # no parallization
      if(!is.null(lop$mrr)) for (kk in 1:nSeg) {
         Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runMSS,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      if(!is.null(lop$lme)) for (kk in 1:dimz) {
         Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   } else { # parallization
      if(!is.null(lop$mrr)) for (kk in 1:nSeg) {
         Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runMSS,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      if(!is.null(lop$lme)) for (kk in 1:dimz) {
         Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   }
} else { # volumetric data
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$nBrk))
   if (lop$nNodes==1) { # no parallization
      if(!is.null(lop$mrr)) for (kk in 1:dimz) {
         Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runMSS,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      if(!is.null(lop$lme)) for (kk in 1:dimz) {
         Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runLME,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   } else { # parallization
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterExport(cl, "lop", envir=environment())
      clusterEvalQ(cl, library(gamm4))
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      if(!is.null(lop$mrr)) for (kk in 1:dimz) { # using gam
         Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runMSS,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      if(!is.null(lop$lme)) for (kk in 1:dimz) { # using gamm
         Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME,
                  DM=lop$dataStr, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      stopCluster(cl)
   } 
   # runMSS(inData[30,30,30,], lop$mrr, lop$dataStr, lop$glt, nF, 0)
}

#Top <- 100
Stat[is.nan(Stat)] <- 0
#Stat[Stat > Top] <- Top
#Stat[Stat < (-Top)] <- -Top
Stat[is.na(Stat)] <- 0

if(!is.null(lop$mrr))
   brickNames <- c(c(rbind(rownames(summary(fm)$p.table), paste0(rownames(summary(fm)$p.table), '-Z'))),
      rownames(summary(fm)$s.table), 'R.sq', c(rbind(as.character(lop$Pred[1:lop$nr,1]),
      paste0(as.character(lop$Pred[1:lop$nr,1]),'.se'))))
if(!is.null(lop$lme))
   brickNames <- c(c(rbind(rownames(summary(fm$gam)$p.table), paste0(rownames(summary(fm$gam)$p.table), '-Z'))),
      rownames(summary(fm$gam)$s.table), 'R.sq', c(rbind(as.character(lop$Pred[1:lop$nr,1]),
      paste0(as.character(lop$Pred[1:lop$nr,1]),'.se'))))

statsym <- NULL
if(!is.null(lop$mrr)) {
   for(n in 1:nrow(summary(fm)$p.table))
      statsym <- c(statsym, list(list(sb=2*n-1, typ="fizt")))
   for(n in 1:nrow(summary(fm)$s.table))
      #statsym <- c(statsym, list(list(sb=2*nrow(summary(fm)$p.table)+n-1, typ="fizt")))
      statsym <- c(statsym, list(list(sb=2*nrow(summary(fm)$p.table)+n-1, typ="fict", par=2)))
}
if(!is.null(lop$lme)) {
   for(n in 1:nrow(summary(fm$gam)$p.table))
      statsym <- c(statsym, list(list(sb=2*n-1, typ="fizt")))
   if(!is.null(summary(fm$gam)$s.table)) { # having no smooth terms
      for(n in 1:nrow(summary(fm$gam)$s.table))
         #statsym <- c(statsym, list(list(sb=2*nrow(summary(fm$gam)$p.table)+n-1, typ="fizt")))
         statsym <- c(statsym, list(list(sb=2*nrow(summary(fm$gam)$p.table)+n-1, typ="fict", par=2)))
   }
}

write.AFNI(lop$outFN, Stat, brickNames, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_float', scale=FALSE)

print(sprintf("Congratulations: 3dMSS mission has been accomplished with an output %s", lop$outFN))
##############  END  ############

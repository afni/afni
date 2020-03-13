#!/usr/bin/env AFNI_Batch_R

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dICC'

# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin 3dICC Input functions ################################
#################################################################################

#The help function for 3dICC batch (AFNI-style script mode)
help.ICC.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
          ================== Welcome to 3dICC ==================          
          AFNI Program for IntraClass Correlatin (ICC) Analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.1.0, Mar 13, 2020
Author: Gang Chen (gangchen@mail.nih.gov)
Website - ATM
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892, USA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 Intraclass correlation (ICC) measures the extent of consistency, agreement or
 reliability of an effect (e.g., BOLD respoonse) across two or more measures. 
 3dICC is a program that computes whole-brain voxel-wise ICC when each subject
 has two or more effect estimates (e.g., sessions, scanners, etc. ). All three
 typical types of ICC are available through proper model specification:
 ICC(1, 1), ICC(2,1) and ICC(3,1). The latter two types are popular in
 neuroimaging because ICC(1,1) is usually applicable for scenarios such as twins.
 The program can be applied to even wider situations (e.g., incorporation of
 confounding effects or more than two random-effects variables). The modeling
 approaches are laid out in the following paper:

 Chen et al., 2017. Intraclass correlation: Improved modeling approaches and
 applications for neuroimaging. Human Brain Mapping 39(3): 1187-1206.
 https://doi.org/10.1002/hbm.23909

 Currently it provides in the output the ICC value and the corresponding
 F-statistic at each voxel. In future, inferences for intercept and covariates
 may be added.
 
 Input files for 3dICC can be in AFNI, NIfTI, or surface (niml.dset) format.
 Two input scenarios are considered: 1) effect estimates only, and 2) effect
 estimates plus their t-statistic values which are used for weighting based
 on the precision contained in the t-statistic.
 
 In addition to R installtion, the following R packages need to be installed
 in R first before running 3dICC: "lme4", "blme" and "metafor". In addition,
 the "snow" package is also needed if one wants to take advantage of parallel
 computing. To install these packages, run the following command at the terminal:

 rPkgsInstall -pkgs "blme,lme4,metafor,snow"

 Alternatively you may install them in R:
 
 install.packages("blme")
 install.packages("lme4")
 install.packages("metafor")
 install.packages("snow") 

 Once the 3dICC command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called ICC.txt, and execute it with the 
 following (assuming on tc shell),
 
 nohup tcsh -x ICC.txt &
 
 or,
 
 nohup tcsh -x ICC.txt > diary.txt &
 nohup tcsh -x ICC.txt |& tee diary.txt &

 The advantage of the latter commands is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.\n'


   ex1 <-  
"Example 1 --- Compute ICC(2,1) values between two sessions.

-------------------------------------------------------------------------
    3dICC -prefix ICC2 -jobs 12                                   \\
          -model  '1+(1|session)+(1|Subj)'                        \\
          -dataTable                                              \\
          Subj      session        InputFile                      \\
          s1         one    s1_1+tlrc\'[pos#0_Coef]\'               \\
          s1         one    s1_1+tlrc\'[neg#0_Coef]\'               \\
          s1         one    s1_1+tlrc\'[neu#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[pos#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[neg#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[neu#0_Coef]\'               \\
          ... 
          s21        two   s21_2+tlrc\'[pos#0_Coef]\'               \\
          s21        two   s21_2+tlrc\'[neg#0_Coef]\'               \\
          s21        two   s21_2+tlrc\'[neu#0_Coef]\'               \\
          ...                                   
   \n"

   ex2 <-
"Example 2 --- Compute ICC(3,1) values between two sessions.

-------------------------------------------------------------------------
    3dICC -prefix ICC3 -jobs 12                                   \\
          -model  '1+session+(1|Subj)'                            \\
          -dataTable                                              \\
          Subj      session        InputFile                      \\
          s1         one    s1_1+tlrc\'[pos#0_Coef]\'               \\
          s1         one    s1_1+tlrc\'[neg#0_Coef]\'               \\
          s1         one    s1_1+tlrc\'[neu#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[pos#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[neg#0_Coef]\'               \\
          s1         two    s1_2+tlrc\'[neu#0_Coef]\'               \\
          ... 
          s21        two   s21_2+tlrc\'[pos#0_Coef]\'               \\
          s21        two   s21_2+tlrc\'[neg#0_Coef]\'               \\
          s21        two   s21_2+tlrc\'[neu#0_Coef]\'               \\
         ...
     \n"

      ex3 <-  
"Example 3 --- Compute ICC(3,1) values between two sessions with both effect
   estimates and their t-statistics as input. The subject column is explicitly
   declared because it is named differently from the default ('Subj').

-------------------------------------------------------------------------
    3dICC -prefix ICC2a -jobs 12                                     \\
          -model  '1+age+(1|session)+(1|Subj)'                       \\
          -Subj   'subject'                                           \\
          -tStat 'tFile'                                             \\
          -dataTable                                                 \\
       subject age session       tFile                    InputFile                          \\
          s1    21   one   s1_1+tlrc\'[pos#0_tstat]\'    s1_1+tlrc\'[pos#0_Coef]\'               \\
          s1    21   one   s1_1+tlrc\'[neg#0_tstat]\'    s1_1+tlrc\'[neg#0_Coef]\'               \\
          s1    21   one   s1_1+tlrc\'[neu#0_tstat]\'    s1_1+tlrc\'[neu#0_Coef]\'               \\
          s1    21   two   s1_2+tlrc\'[pos#0_tstat]\'    s1_2+tlrc\'[pos#0_Coef]\'               \\
          s1    21   two   s1_2+tlrc\'[neg#0_tstat]\'    s1_2+tlrc\'[neg#0_Coef]\'               \\
          s1    21   two   s1_2+tlrc\'[neu#0_tstat]\'    s1_2+tlrc\'[neu#0_Coef]\'               \\
          ... 		                               
          s21   28   two   s21_2+tlrc\'[pos#0_tstat]\'   s21_2+tlrc\'[pos#0_Coef]\'              \\
          s21   28   two   s21_2+tlrc\'[neg#0_tstat]\'   s21_2+tlrc\'[neg#0_Coef]\'              \\
          s21   28   two   s21_2+tlrc\'[neu#0_tstat]\'   s21_2+tlrc\'[neu#0_Coef]\'              \\
         ...
     \n"

      ex4 <-  
"Example 4 --- Compute ICC(2,1) values between two sessions while controlling
   for age effect.

-------------------------------------------------------------------------
    3dICC -prefix ICC2a -jobs 12                                  \\
          -model  '1+age+(1|session)+(1|Subj)'                    \\
          -Subj   'subjct'                                        \\
          -InputFile 'inputfile'                                  \\
          -dataTable                                              \\
       subject age session        inputfile                       \\
          s1    21   one    s1_1+tlrc\'[pos#0_Coef]\'               \\
          s1    21   one    s1_1+tlrc\'[neg#0_Coef]\'               \\
          s1    21   one    s1_1+tlrc\'[neu#0_Coef]\'               \\
          s1    21   two    s1_2+tlrc\'[pos#0_Coef]\'               \\
          s1    21   two    s1_2+tlrc\'[neg#0_Coef]\'               \\
          s1    21   two    s1_2+tlrc\'[neu#0_Coef]\'               \\
          ... 	
          s21   28   two   s21_2+tlrc\'[pos#0_Coef]\'               \\
          s21   28   two   s21_2+tlrc\'[neg#0_Coef]\'               \\
          s21   28   two   s21_2+tlrc\'[neu#0_Coef]\'               \\
         ...
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
   cat(intro, ex1, ex2, ex3, ex4, ss, sep='\n')
   
   if (adieu) exit.AFNI();
}
   
#Change command line arguments into an options list
read.ICC.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output file name. For AFNI format, provide prefix only,",
   "         with no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached, while file name for surface data is expected",
   "         to end with .niml.dset. The sub-brick labeled with the '(Intercept)',",
   "         if present, should be interpreted as the effect with each factor",
   "         at the reference level (alphabetically the lowest level) for each",
   "         factor and with each quantitative covariate at the center value.\n", sep = '\n'
      ) ),

#      '-resid' = apl(n = 1, d = NA,  h = paste(
#   "-resid PREFIX: Output file name for the residuals. For AFNI format, provide",
#   "         prefix only without view+suffix. Filename for NIfTI format should",
#   "         have .nii attached, while file name for surface data is expected",
#   "         to end with .niml.dset. The sub-brick labeled with the '(Intercept)',",
#   "         if present, should be interpreted as the effect with each factor",
#   "         at the reference level (alphabetically the lowest level) for each",
#   "         factor and with each quantitative covariate at the center value.\n", sep = '\n'
#                     ) ),       

      '-mask' = apl(n=1,  d = NA, h = paste(
   "-mask MASK: Process voxels inside this mask only.\n",
   "         Default is no masking.\n"
                     ) ),

      '-jobs' = apl(n = 1, d = 1, h = paste(
   "-jobs NJOBS: On a multi-processor machine, parallel computing will speed ",
   "         up the program significantly.",
   "         Choose 1 for a single-processor computer.\n", sep = '\n'
                     ) ),

      '-Subj' = apl(n = 1, d = NA,  h = paste(
   "-Subj var_name: var_name is used to specify the column name that is designated as",
   "        as the measuring entity variable (usually subject). The default (when this",
   "        option is not invoked) is 'Subj', in which case the column header has to be",
   "        exactly as 'Subj'.\n", sep = '\n'
                     ) ),

      '-IF' = apl(n = 1, d = NA,  h = paste(
   "-IF var_name: var_name is used to specify the last column name that is designated for",
   "        input files of effect estimate. The default (when this option is not invoked",
   "        is 'InputFile', in which case the column header has to be exactly as 'InputFile'.\n", sep = '\n'
                     ) ),

      '-tStat' = apl(n = 1, d = NA,  h = paste(
   "-tStat col_name: col_name is used to specify the column name that is designated as",
   "        as the t-statistic. The default (when this option is not invoked) is 'NA',",
   "        in which case no t-stat is provided as part of the input; otherwise declare",
   "        the t-stat column name with this option.\n", sep = '\n'
                     ) ),

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: Specify the model structure for all the variables. The",
   "         expression FORMULA with more than one variable has to be surrounded",
   "         within (single or double) quotes. Variable names in the formula",
   "         should be consistent with the ones used in the header of -dataTable.",
   "         Suppose that each subject ('subj') has two sessions ('ses'), a model",
   "         ICC(2,1) without any covariate is \"1+(1|ses)+(1|subj)\" while one",
   "         for ICC(3,1) is \"1+ses+(1|subj)\". Each random-effects factor is",
   "         specified within paratheses per formula convention in R. Any",
   "         confounding effects (quantitative or categorical variables) can be",
   "         added as fixed effects without paratheses.\n", sep = '\n'
             ) ),

       '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dICC.dbg.AFNI.args in the current directory",
   "          so that debugging can be performed.\n", sep='\n')),
       
      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars variable_list: Identify quantitative variables (or covariates) with",
   "         this option. The list with more than one variable has to be",
   "         separated with comma (,) without any other characters such as",
   "         spaces and should be surrounded within (single or double) quotes.",
   "          For example, -qVars \"Age,IQ\"",
   "         WARNINGS:",
   "         1) Centering a quantitative variable through -qVarsCenters is",
   "         very critical when other fixed effects are of interest.",
   "         2) Between-subjects covariates are generally acceptable.",
   "         However EXTREME caution should be taken when the groups",
   "         differ significantly in the average value of the covariate.",
   "         3) Within-subject covariates are better modeled with 3dICC.\n",
             sep = '\n'
             ) ),

#      '-vVars' = apl(n=c(1,100), d=NA, h = paste(
#   "-vVars variable_list: Identify voxel-wise covariates with this option.",
#   "         Currently one voxel-wise covariate is allowed only, but this",
#   "         may change if demand occurs...",
#   "         By default mean centering is performed voxel-wise across all",
#   "         subjects. Alternatively centering can be specified through a",
#   "         global value under -vVarsCenters. If the voxel-wise covariates",
#   "         have already been centered, set the centers at 0 with -vVarsCenters.\n",
#             sep = '\n'
#             ) ),

     '-qVarCenters' = apl(n=c(1,100), d=NA, h = paste(
   "-qVarCenters VALUES: Specify centering values for quantitative variables",
   "         identified under -qVars. Multiple centers are separated by ",
   "         commas (,) without any other characters such as spaces and should",
   "         be surrounded within (single or double) quotes. The order of the",
   "         values should match that of the quantitative variables in -qVars.",
   "         Default (absence of option -qVarsCetners) means centering on the",
   "         average of the variable across ALL subjects regardless their",
   "         grouping. If within-group centering is desirable, center the",
   "         variable YOURSELF first before the values are fed into -dataTable.\n",
             sep = '\n'
                     ) ),

#     '-vVarCenters' = apl(n=1, d=NA, h = paste(
#   "-vVarCenters VALUES: Specify centering values for voxel-wise covariates",
#   "         identified under -vVars. Multiple centers are separated by ",
#   "         commas (,) within (single or double) quotes. The order of the",
#   "         values should match that of the quantitative variables in -qVars.",
#   "         Default (absence of option -vVarsCetners) means centering on the",
#   "         average of the variable across ALL subjects regardless their",
#   "         grouping. If within-group centering is desirable, center the",
#   "         variable YOURSELF first before the files are fed into -dataTable.\n",
#             sep = '\n'
#
#) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure with a header as the first line.\n",
   "         NOTE:\n",
   "         1) This option has to occur last; that is, no other options are",
   "         allowed thereafter. Each line should end with a backslash except for",
   "         the last line.\n",
   "         2) The first column is fixed and reserved with label 'Subj', and the",
   "         last is reserved for 'InputFile'. Each row should contain only one",
   "         effect estimate in the table of long format (cf. wide format) as",
   "         defined in R. The level labels of a factor should contain at least",
   "         one character. Input files can be in AFNI, NIfTI or surface format.",
   "         AFNI files can be specified with sub-brick selector (square brackets",
   "         [] within quotes) specified with a number or label.\n",
   "         3) It is fine to have variables (or columns) in the table that are",
   "         not modeled in the analysis.\n",
   "         4) The context of the table can be saved as a separate file, e.g.,",
   "         called table.txt. In the 3dICC script, specify the data with",
   "         '-dataTable @table.txt'. Do NOT put any quotes around the square",
   "         brackets for each sub-brick; Otherwise, the program cannot properly",
   "         read the files. This option is useful: (a) when there are many input",
   "         files so that the program complains with an 'Arg list too long' error;",
   "         (b) when you want to try different models with the same dataset.\n",
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
      errex.AFNI('Error parsing arguments. See 3dICC -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$cutoff <- 0
      #lop$fixEff  <- 1
      lop$maskFN <- NA
      lop$Subj   <- 'Subj' #default
      lop$tStat  <- NA
      lop$IF     <- 'InputFile' #default

      #lop$ranEff <- NA
      lop$model  <- NA
      lop$qVars  <- NA   
      #lop$vVars  <- NA
      #lop$vQV    <- NA
      lop$qVarCenters <- NA
      #lop$vVarCenters <- NA

      #lop$ICC <- 2
      lop$dataTable <- NULL
      
      lop$iometh <- 'clib'
      lop$dbgArgs  <- FALSE # for debugging purpose 
      lop$verb <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             resid  = lop$resid  <- pprefix.AFNI.name(ops[[i]]),
             mask   = lop$maskFN <- ops[[i]],
             jobs   = lop$nNodes <- ops[[i]],
	     model  = lop$model  <- ops[[i]],
             Subj   = lop$Subj     <- ops[[i]],
	     IF     = lop$IF     <- ops[[i]],
	     tStat  = lop$tStat  <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             #vVars  = lop$vVars  <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             #vVarCenters = lop$vVarCenters <- ops[[i]],
             #ICC     = lop$ICC     <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
             
             help = help.ICC.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }

   return(lop)
}# end of read.ICC.opts.batch

#Change options list to 3dICC variable list 
process.ICC.opts <- function (lop, verb = 0) {
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
   
   #Make sure new io must be used with anything but BRIK format
   #an <- parse.AFNI.name(lop$outFN)
   if(an$type != 'BRIK' && lop$iometh != 'clib') 
      errex.AFNI(c('Must of use -cio option with any input/output ',
                   'format other than BRIK'))

   if(!is.null(lop$resid)) {
      an2 <- parse.AFNI.name(lop$resid)
      if(an2$type == "NIML") {
         if(file.exists(lop$resid)) errex.AFNI(c("File ", lop$resid, " exists! Try a different name.\n"))
      } else if(file.exists(paste(lop$resid,"+tlrc.HEAD", sep="")) || 
        file.exists(paste(lop$resid,"+tlrc.BRIK", sep="")) || 
        file.exists(paste(lop$resid,"+orig.HEAD", sep="")) || 
        file.exists(paste(lop$resid,"+orig.BRIK", sep=""))) {
        errex.AFNI(c("File ", lop$resid, " exists! Try a different name.\n"))
        return(NULL)
      }  
      #Make sure new io must be used with anything but BRIK format
      #an <- parse.AFNI.name(lop$resid)
      if(an2$type != 'BRIK' && lop$iometh != 'clib') 
          errex.AFNI(c('Must of use -cio option with any input/output ',
                       'format other than BRIK'))   
   }
   # assume the quantitative variables are separated by + here
   if(!is.na(lop$qVars)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]
   #if(!is.na(lop$vVars[1])) lop$vQV <- strsplit(lop$vVars, '\\,')[[1]]

   len <- length(lop$dataTable)
   wd <- which(lop$dataTable == lop$IF)  # assuming the input file is the last column here!
   hi <- len / wd - 1
   
   if(len %% wd != 0)
      errex.AFNI(paste('The content under -dataTable is not rectangular !', len, wd)) else {
      lop$dataStr <- NULL
      #browser()
      for(ii in 1:wd) 
         lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      # wow, terrible mistake here with as.numeric(lop$dataStr[,jj])
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
      if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      #if(!is.na(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
   }
   
   if(lop$iometh == 'Rlib') {
      lop$outFN <- paste(lop$outFN, "+tlrc", sep="")
      if(!is.null(lop$resid)) lop$resid <- paste(lop$resid, "+tlrc", sep="")
   } else {
      #an <- parse.AFNI.name(lop$outFN)
      if(an$type == "BRIK" && an$ext == "" && is.na(an$view))
         lop$outFN <- paste(lop$outFN, "+tlrc", sep="")      
      if (exists.AFNI.name(lop$outFN) || 
          exists.AFNI.name(modify.AFNI.name(lop$outFN,"view","+tlrc")))
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
      if(!is.null(lop$resid)) {
         #an2 <- paste(lop$resid, "+tlrc", sep="")      
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
      if(is.null(mm <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE))) {
         warning("Failed to read mask", immediate.=TRUE)
         return(NULL)
      }
      lop$maskData <- mm$brk
      if(verb) cat("Done read ", lop$maskFN,'\n')
   }
   if(!is.na(lop$maskFN)) 
      if(!all(dim(lop$maskData[,,,1])==lop$myDim[1:3])) 
         stop("Mask dimensions don't match the input files!")

   return(lop)
}

#################################################################################
################# ICC Computation functions ##############################
#################################################################################

# LME: bare-bone approach with LME for ICC: some voxels may have 0 ICC values: effect estimates as input only
runLME <- function(myData, ModelForm, dataframe, nBrk, tag) {
   #browser()
   myStat<-vector(mode="numeric", length= nBrk)
   if(!all(myData == 0)) {     
      dataframe$eff <- myData
      try(fm<-lmer(ModelForm, data=dataframe), tag<-1)
      if(tag != 1) {    
         #for(ii in 1:(nBrk-1)) myStat[ii] <- VarCorr(fm)[[ii]][1]  # factor variances
         #myStat[1] <- summary(fm)$coefficients[1,1]  #b0
         #myStat[2] <- summary(fm)$coefficients[1,3]  #b0 t
         #myStat[3] <- summary(fm)$coefficients[2,1]  #b1
         #myStat[4] <- summary(fm)$coefficients[2,3]  #b1 t
         #myStat[1] <- VarCorr(fm)[[me]][1]/(VarCorr(fm)[[1]][1]+VarCorr(fm)[[2]][1]+attr(VarCorr(fm), "sc")^2)  # ICC
         #myStat[2] <- (nrow(dataframe)/nlevels(dataframe$Subj))*VarCorr(fm)[[1]][1]/attr(VarCorr(fm), "sc")^2+1 # F
         myStat[1] <- VarCorr(fm)$Subj[1]/(sum(unlist(lapply(VarCorr(fm), `[`, 1))) + attr(VarCorr(fm), "sc")^2)  # ICC
         myStat[2] <- (nrow(dataframe)/nlevels(dataframe$Subj))*VarCorr(fm)$Subj[1]/attr(VarCorr(fm), "sc")^2+1 # F
         #myStat[nBrk] <- attr(VarCorr(fm), "sc")^2  # residual variance
         #myStat <- myStat/sum(myStat)
      }
   }
   #browser()
   return(myStat)
}
# runLME(inData[30,30,30,], lop$model, lop$dataStr, 2, 0)

# MME: mixed-effects with effect estimates plus their t-stat
runMME <- function(myData, dataframe, fe, re, nBrk, tag) {
   #browser()
   myStat<-vector(mode="numeric", length= nBrk)
   if(!all(myData == 0)) { 
      hlf <- length(myData)/2
      dataframe$eff <- myData[1:hlf]
      dataframe$vi  <- myData[(1+hlf):length(myData)]
      dataframe <- dataframe[dataframe$vi != 0, ]  # remove those rows with 0 variance
      try(fm <- rma.mv(yi=eff, V=vi, mods=fe, random=re, data=dataframe, test='t'), tag<-1)  
      if(tag != 1) {    
         #myStat[1] <- fm$b
         #myStat[2] <- fm$zval
         wm <- mean(dataframe$vi) # default mean in case the weighted mean below fails
         try(wm <- (nrow(fm$X)-ncol(fm$X))/sum(diag((solve(fm$V)-solve(fm$V)%*%fm$X%*%solve(t(fm$X)%*%solve(fm$V)%*%fm$X)%*%t(fm$X)%*%solve(fm$V)))), tag <-1) #weighted mean of the sampling variances
         # wm <- mean(dataframe$vi) # arithmetic mean
         myStat[1] <- fm$sigma2[which(fm$s.names=='Subj')]/(sum(fm$sigma2)+wm)  # factor variances
         myStat[2] <- (nrow(dataframe)/nlevels(dataframe$Subj))*fm$sigma2[which(fm$s.names=='Subj')]/wm+1    # F-stat for ICC(2,1) and ICC(3,1)
     }
   }
   return(myStat)
}
# runMME(comArr[30,30,30,], dataframe=lop$dataStr, fe=lop$fe, re=lop$re, nBrk=lop$NoBrick, tag=0)


#################################################################################
########################## Begin ICC main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dICC.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.ICC(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.ICC.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.ICC.opts.batch(args, verb = 0)))
         stop('Error parsing input')
      
      #str(lop);
      if(is.null(lop <- process.ICC.opts(lop, verb = lop$verb))) 
         stop('Error processing input')
      
   }
   #if(lop$verb > 1) { 
      #Too much output, big dump of header structs of input dsets..
   #   str(lop)
   #}


########################################################################

# in case the user didn't put space around each colon (:), this 
#lop$gltCode <- lapply(lop$gltCode, function(ss) unlist(strsplit(ss, split="(?=:)", perl=TRUE)))                                             

if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])

# effect coding leads to the same type III as SAS   
options(contrasts = c("contr.sum", "contr.poly"))
   
# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
#if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model)) else
   ModelForm <- paste("Beta ~", lop$fixEff)

# standardize the names for Y, ROI and subject
names(lop$dataStr)[names(lop$dataStr)==lop$Subj] <- 'Subj'
names(lop$dataStr)[names(lop$dataStr)==lop$IF] <- 'InputFile'
names(lop$dataStr)[names(lop$dataStr)==lop$tStat] <- 'Tstat'

# Maybe not list for these two, or yes?
lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)
#if(is.null(lop$Tstat)) lop$dataStr$Tstat <-  as.character(lop$dataStr$Tstat)

# center on user-speficied value or mean
if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters))) 
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F)[,,drop=T] else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)[,,drop=T]

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of data structure *****\n')

cat(nlevels(lop$dataStr$Subj), 'subjects : ', levels(lop$dataStr$Subj), '\n')
cat(length(lop$dataStr$InputFile), 'response values\n')
for(ii in 2:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':', 
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'numeric' | class(lop$dataStr[,ii]) == 'matrix')  # numeric doesn't work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')
#cat(lop$num_glt, 'post hoc tests\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')
#if(lop$ICC | lop$ICCb) showTab <- as.formula(paste('~', gsub("\\*", "+", lop$ranEff))) else {
#   showTab <- as.formula(paste('~', gsub("\\:", "+", gsub("\\*", "+", lop$fixEff))))
#   if(!is.na(lop$qVars)) for(ii in rev(levels(ordered(lop$QV)))) # reversing the oder of those quantitative covariates so that
#      showTab <- gsub(paste('\\*', ii, sep=''), '', gsub(paste('\\+', ii, sep=''), '', showTab))
#   if(!is.na(lop$vVars)) showTab <- sub(lop$vQV, "", showTab)
#   #showTab <- as.formula(gsub("\\*", "+", showTab))  # in case there are still some *'s like between-subjects factors
#}
##print(xtabs(showTab, data=lop$dataStr))                                           
#
#if(!lop$ICC | lop$ICCb) {
#   cat('\nTabulation of subjects against all categorical variables')
#   all_vars <- names(lop$dataStr)
#   for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
#      cat('\n~~~~~~~~~~~~~~')
#      cat('\nSubj vs ', var, ':\n', sep='')
#      print(table(lop$dataStr$Subj, lop$dataStr[,var]))
#   }
#}

cat('***** End of data structure information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

#Number of input files
NoFile <- dim(lop$dataStr[1])[1]

# Repeated-measures (use lme) or not (use lm)
#if (length(unique(lop$dataStr$Subj)) != length(lop$dataStr$Subj)) RM <- TRUE else RM <- FALSE

cat('Reading input files now...\n\n')

# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(lop$dataStr[1, lop$IF], verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData


# Read in all input files
inData <- unlist(lapply(lapply(lop$dataStr[,lop$IF], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
tryCatch(dim(inData) <- c(dimx, dimy, dimz, NoFile), error=function(e)
   errex.AFNI(c("At least one of the input files has different dimensions!\n",
   "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
   "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files for effect estimates: Done!\n\n')

if(!is.na(lop$tStat)) {
   lop$dataStr[,'Tstat'] <-  as.character(lop$dataStr[,'Tstat'])
   inDataV <- unlist(lapply(lapply(lop$dataStr[,'Tstat'], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
   tryCatch(dim(inDataV) <- c(dimx, dimy, dimz, NoFile), error=function(e)
      errex.AFNI(c("At least one of the input files has different dimensions!\n",
      "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
      "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
      "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
   cat('Reading input files for tStat: Done!\n\n')
}

if (!is.na(lop$maskFN)) {
   Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk[,,,1]
   inData <- array(apply(inData, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,NoFile))
   if(!is.na(lop$tStat)) inDataV <- array(apply(inDataV, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,NoFile))
}
  
# voxel-wise covariate files
#if(!is.na(lop$vQV)) {
#   tmpDat <- read.AFNI(as.character(unique(lop$dataStr[,lop$vQV[1]])[1]), verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
#   dimx <- tmpDat$dim[1]
#   dimy <- tmpDat$dim[2]
#   dimz <- tmpDat$dim[3]
#   head <- tmpDat
#   for(ii in lop$vQV)
#   if(length(unique(lop$dataStr[,ii])) != nlevels(lop$dataStr$Subj))
#      errex.AFNI(c("Error with voxel-wise covariate ", ii, ": Each subject is only\n",
#                "allowed to have one volume; that is, the covariate has to be at the\n",
#                "subject level.")) else {  # currently consider one voxel-wise covariate only: may generalize later?
#      #vQV <- unlist(lapply(lapply(unique(lop$dataStr[,lop$vQV[1]]), read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
#      vQV <- unlist(lapply(lapply(as.character(unique(lop$dataStr[,lop$vQV[1]])), read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
#      dim(vQV) <- c(dimx, dimy, dimz, length(unique(lop$dataStr[,lop$vQV[1]])))
#      inData <- c(inData, vQV)
#      dim(inData) <- c(dimx, dimy, dimz, lop$nVVars+lop$nSubj)
#   }
#} else vQV <- NULL



# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
#ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

###############################

# add a new column to store the voxel-wise filenames
if(any(!is.na(lop$vVars))) {
   lop$dataStr <- cbind(lop$dataStr, lop$dataStr[, lop$vQV[1]])                                                
   names(lop$dataStr)[length(names(lop$dataStr))] <- paste(lop$vQV[1], '_fn', sep='')
   lop$nVVars <- nlevels(lop$dataStr[,paste(lop$vQV[1], '_fn', sep='')])
}                                             

if(is.na(lop$tStat)) require(lme4) else {
   require(metafor)
   inDataV <- unlist(mapply(function(x, y) ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), inData, inDataV, SIMPLIFY = FALSE))
#   inDataV <- unlist(mapply(function(x, y) ifelse(abs(y)<tolL, 1e16, (x/y)^2), inData, inDataV, SIMPLIFY = FALSE))
   dim(inDataV) <- c(dimx, dimy, dimz, NoFile)
   comArr <- array(c(inData, inDataV), dim=c(dim(inData)[1:3], sum(dim(inData)[4], dim(inDataV)[4])))
}

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or something else\n')
cat('is likely inappropriate.\n\n')

xinit <- dimx%/%3
if(dimy==1) yinit <- 1 else yinit <- dimy%/%3
if(dimz==1) zinit <- 1 else zinit <- dimz%/%3

ii <- xinit; jj <- yinit; kk <- zinit

if(is.na(lop$tStat)) { # no tStat input
   lop$model <- as.formula(paste('eff ~ ', lop$model))
   fm<-NULL  
   while(is.null(fm)) {
      lop$dataStr$eff <- inData[ii, jj, kk,]
      options(warn=-1)
      try(fm <- lmer(lop$model, data=lop$dataStr), silent=TRUE)
      #fm <- 1
      if(!is.null(fm))  {
         print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
      } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
         ii<-xinit; jj <- yinit; kk <- kk+1 } else {
         cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')    
         cat('Possible reasons:\n\n')
         cat('0) Make sure that R package lme4 has been installed. See the 3dLME\n')
         cat('help documentation for more details.\n\n')
         cat('1) Inappropriate model specification with options -model, or -qVars.\n\n')
         cat('2) In correct specifications for random effect with -ranEff.\n\n')
         cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
         cat('whether there are any inconsistencies.\n\n')
         cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
         cat('named Scanner in model specification and then listed as scanner in the table hader\n')
         cat('would cause grief for 3dICC.\n')
         errex.AFNI("Quitting due to model test failure...")
      }
   }
} else {   # with tStat
   tm <- unlist(strsplit(lop$model, split="[+]"))
   fe <- tm[which(!grepl('[(]', tm))]
   re <- gsub("[()]", "", tm[which(grepl('[(]', tm))])
   lop$fe <- paste('~', fe[1])
   if(length(fe) > 1) for(nn in 2:length(fe)) lop$fe <- paste(lop$fe, '+', fe[nn])
   lop$fe <- as.formula(lop$fe)
      
   lop$re <- list(as.formula(paste("~",re[1])))
   if(length(re) > 1) for(nn in 2:length(re)) lop$re <- c(lop$re, list(as.formula(paste("~",re[nn]))))

   fm<-NULL
   while(is.null(fm)) {
      fm<-NULL
      lop$dataStr$eff <- inData[ii, jj, kk,]
      lop$dataStr$vi  <- inDataV[ii, jj, kk,]
      options(warn=-1)
      try(fm <- rma.mv(yi=eff,V=vi, mods=lop$fe, random=lop$re, data=lop$dataStr, test='t'), silent=TRUE)
      #try(fm <- lmer(ModelForm, data=lop$dataStr), silent=TRUE)
      if(!is.null(fm))  {
         print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
      } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
         ii<-xinit; jj <- yinit; kk <- kk+1 } else {
         cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')    
         cat('Possible reasons:\n\n')
         cat('0) Make sure that R package lme4 has been installed. See the 3dLME\n')
         cat('help documentation for more details.\n\n')
         cat('1) Inappropriate model specification with options -model, or -qVars.\n\n')
         cat('2) In correct specifications for random effect with -ranEff.\n\n')
         cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
         cat('whether there are any inconsistencies.\n\n')
         cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
         cat('named Scanner in model specification and then listed as scanner in the table hader\n')
         cat('would cause grief for 3dICC.\n')
         errex.AFNI("Quitting due to model test failure...")
      }
   }
}

dfD <- nrow(lop$dataStr) - nlevels(lop$dataStr$Subj) - nrow(coefficients(summary(fm))) + 1  # DFs for denominator
dfN <- nlevels(lop$dataStr$Subj) - 1 # DFs for numerator
lop$NoBrick <- 2 # ICC plus its F-stat

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################

options(contrasts = c("contr.sum", "contr.poly"))

if(dimy == 1 & dimz == 1) {  # 1D scenarios
   nSeg <- 20
   # drop the dimensions with a length of 1
   # break into 20 segments, leading to 5% increamental in parallel computing
   #dimx_n <- ifelse(dimx%%nSeg==0, dimx%/%nSeg, dimx%/%nSeg + 1)
   dimx_n <- dimx%/%nSeg + 1
   # number of datasets need to be filled
   fill <- nSeg-dimx%%nSeg
   # pad with extra 0s

   if(lop$nNodes==1) for (kk in 1:dimz) {  # no parallel computation
   
      Stat <- array(0, dim=c(dimx_n, nSeg, lop$NoBrick))
      if(is.na(lop$tStat)) {  # without t-stat as input
	 inData <- inData[, , ,]
	 inData <- rbind(inData, array(0, dim=c(fill, dim(inData)[2])))
	 dim(inData) <- c(dimx_n, nSeg, dim(inData)[2])
         # declare output receiver
         for (kk in 1:nSeg) {
            Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME, ModelForm=lop$model,
                        dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,1)) 
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
         } 
      } else {  # with t-stat as input
	 comArr <- comArr[, , ,]
	 comArr <- rbind(comArr, array(0, dim=c(fill, dim(comArr)[2])))
	 dim(comArr) <- c(dimx_n, nSeg, dim(comArr)[2])
         for (kk in 1:nSeg) {
            Stat[,kk,] <- aperm(apply(comArr[,kk,], 1, runMME, dataframe=lop$dataStr, fe=lop$fe, re=lop$re,
                        nBrk=lop$NoBrick, tag=0), dim=c(2,1)) 
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')
         }
      }
      # convert to 4D
      dim(Stat) <- c(dimx_n*nSeg, 1, 1, lop$NoBrick)
      # remove the trailing 0s (padded 0s)
      Stat <- Stat[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]     
      
   } else {   # parallel computation    
      Stat <- array(0, dim=c(dimx_n, nSeg, lop$NoBrick))
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      if(is.na(lop$tStat)) {  # without t-stat as input
         clusterEvalQ(cl, library(lme4))
	 inData <- inData[, , ,]
	 inData <- rbind(inData, array(0, dim=c(fill, dim(inData)[2])))
	 dim(inData) <- c(dimx_n, nSeg, dim(inData)[2])
         # declare output receiver
         for (kk in 1:nSeg) {
            Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME, ModelForm=lop$model,
                        dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,1)) 
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
         } 
      } else {  # with t-stat as input
         clusterEvalQ(cl, library(metafor))
	 comArr <- comArr[, , ,]
	 comArr <- rbind(comArr, array(0, dim=c(fill, dim(comArr)[2])))
	 dim(comArr) <- c(dimx_n, nSeg, dim(comArr)[2])
         for (kk in 1:nSeg) {
            Stat[,kk,] <- aperm(parApply(cl, comArr[,kk,], 1, runMME, dataframe=lop$dataStr, fe=lop$fe, re=lop$re,
                        nBrk=lop$NoBrick, tag=0), dim=c(2,1)) 
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')
         }
      }
      stopCluster(cl)
      # convert to 4D
      dim(Stat) <- c(dimx_n*nSeg, 1, 1, lop$NoBrick)
      # remove the trailing 0s (padded 0s)
      Stat <- Stat[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]     
  }
} else {  # 3D scenarios

   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick))
   if (lop$nNodes==1) for (kk in 1:dimz) { # no parallel computation
      if(is.na(lop$tStat)) {
         for (kk in 1:dimz) {
            Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runLME, ModelForm=lop$model,
	                dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1)) 
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         } 
      } else {
         for (kk in 1:dimz) {
            Stat[,,kk,] <- aperm(apply(comArr[,,kk,], c(1,2), runMME, dataframe=lop$dataStr, fe=lop$fe, re=lop$re,
	                nBrk=lop$NoBrick, tag=0), c(2,3,1)) 
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         } 
      }
   } else {  # parallel computation
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      #clusterExport(cl, "lop", envir=environment())
      if(is.na(lop$tStat)) {
         clusterEvalQ(cl, library(lme4))
         clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
         for (kk in 1:dimz) {
            Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME, ModelForm=lop$model,
	                dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1)) 
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         } 
         stopCluster(cl)
      } else {
         clusterEvalQ(cl, library(metafor))
         clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
         for (kk in 1:dimz) {
            Stat[,,kk,] <- aperm(parApply(cl, comArr[,,kk,], c(1,2), runMME, dataframe=lop$dataStr, fe=lop$fe, re=lop$re,
	                nBrk=lop$NoBrick, tag=0), c(2,3,1)) 
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         } 
      }
   }
}  

Top <- 100
Stat[is.nan(Stat)] <- 0
Stat[Stat > Top] <- Top  
Stat[Stat < (-Top)] <- -Top  

outLabel <- c("ICC", "ICC F")
statsym <- NULL
statsym <- c(statsym, list(list(sb=1,typ="fift", par=c(dfN,dfD))))

write.AFNI(lop$outFN, Stat[,,,1:lop$NoBrick], outLabel, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_short')

#system(statpar)
print(sprintf("Congratulations! You've got an output %s", lop$outFN))

##############  END  ############

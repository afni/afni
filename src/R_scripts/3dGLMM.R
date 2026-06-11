##!/usr/bin/env AFNI_Batch_R

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
  #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE))
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dGLMM'
# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avoiding analyzing data with most 0's

#################################################################################
##################### Begin 3dGLMM Input functions ##############################
#################################################################################

#The help function for 3dGLMM batch (AFNI-style script mode)
help.GLMM.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <-
'
             ================== Welcome to 3dGLMM ==================
          Program for Voxelwise Generalized Linear Mixed-Models (GLMMs) 
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.3, Feb 18, 2025
Author: Gang Chen (gangchen@mail.nih.gov)
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892, USA
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Introduction
------

 ### Generalized Linear Mixed-Models (GLMM) Overview

 Generalized Linear Mixed-Models (GLMMs) extend Linear Mixed-Models (LMMs) to
 handle non-normal response variables, such as binary, count, or categorical data.
 The response variable in GLMMs can follow distributions like binomial, Poisson,
 or other members of the exponential family.

 ### 3dGLMM: Extension of 3dLMEr

 The program **3dGLMM** builds on **3dLMEr**, adding support for Student\'s
 *t*-distribution for model residuals in addition to the standard normal
 distribution. This functionality requires the R packages **glmmTMB**,
 **car**, and **emmeans**.

 Like **3dLMEr**, 3dGLMM automatically provides outputs for all main effects
 and interactions. However, users must explicitly request marginal effects
 and their comparisons through the options `-level` or `-slope` in 3dGLMM 
 instead of `-gltcode` or -glfCode in 3dLMEr.

 1. **Random-Effects Specification**:

 Random-effects components must be directly incorporated into the model
 specification via the `-model` option. The `-ranEff` option used in
 3dLMEr is no longer needed. Users are responsible for formulating
 appropriate model structures. For detailed guidance, refer to the blog post:
 [How to specify individual-level random effects in hierarchical modeling]
 (https://discuss.afni.nimh.nih.gov/t/how-to-specify-individual-level-random-effects-in-hierarchical-modeling/6462).

 2. **Marginal Effects and Pairwise Comparisons**:

 Users can specify marginal effects and pairwise comparisons through the
 options `-level` and `-slope`.

 ### Input and Output Formats

 3dGLMM accepts input files in various formats, including AFNI, NIfTI,
 surface (`niml.dset`), or 1D. To match the output format with the
 input, append an appropriate suffix to the output option `-prefix`
 (e.g., `.nii` for NIfTI, `.niml.dset` for surface, or `.1D` for 1D).

 ### Incorporation of Explanatory Variables

 3dGLMM supports various types of explanatory variables and covariates:
 - **Categorical variables**: Between- and within-subject factors.
 - **Quantitative variables**: Continuous predictors like age or
 behavioral data.

 #### Declaring Quantitative Variables

 When including quantitative variables, you must explicitly declare
 them using the `-qVars` option. Additionally, consider the
 **centering** of these variables:

 - Global centering.
 - Within-group (or within-condition) centering.

 For further guidance on centering, see: [AFNI documentation on centering]
 (https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/statistics/center.html).

 ### Installation of Required R Packages

 Before running 3dGLMM, ensure the following R packages are installed:
 - `glmmTMB`
 - `car`
 - `emmeans`
 - `snow`

 You can install them via AFNI’s R installation script:

 rPkgsInstall -pkgs "glmmTMB,car,emmeans,snow"

 Alternatively, install them directly in R:

 ```
 install.packages("glmmTMB")
 install.packages("car")
 install.packages("emmeans")
 install.packages("snow")
 ```

 ### Example Scripts

 The following example scripts demonstrate 3dGLMM applications. More
 examples will be added as scenarios are crowdsourced from users. If
 one of the examples matches your data structure, use it as a template
 to build your own script.

 ### Running 3dGLMM
 Once you’ve constructed your command script, run it in the terminal.
 Save the script as a text file (e.g., `GLMM.txt`) and execute it with:

 ```
 nohup tcsh -x GLMM.txt &
 ```

 Alternatively, for progress tracking, redirect output to a log file:

 ```
 nohup tcsh -x GLMM.txt > diary.txt &
 nohup tcsh -x GLMM.txt |& tee diary.txt &
 ```
 
 This method saves output in `diary.txt`, allowing you to review
 progress and troubleshoot if needed.\n'                                                                                                                           
   ex1 <-
"Here’s a revised version with improved clarity, grammar, and formatting:

---

### Example 1: one within-individual factor and a quantitiave predictor

-------------------------------------------------------------------------
  3dGLMM -prefix glmm.student -jobs 12                              \\
         -family student.t                                          \\
         -model 'task*age+(1|Subj)'                                 \\
         -qVars 'age'                                               \\
         -qVarCenters 0                                             \\
         -level LAB task CAT task                                   \\
         -level LAB pos.slp2    CAT 1  FIX task=pos,age=2           \\
         -slope LAB pos.age     CAT 1  FIX task=pos       QUANT age \\
         -slope LAB task.by.age CAT task                  QUANT age \\
         -dataTable                                                 \\
         Subj    age   task  InputFile                              \\
         s1     3.03   pos   data/pos_s1+tlrc.                      \\
         s1     0.82   neg   data/neg_s1+tlrc.                      \\
         s2     2.67   pos   data/pos_s2+tlrc.                      \\
         s2     0.24   neg   data/neg_s2+tlrc.                      \\

          ...

  #### Data Structure Overview  

  This example involves a **within-individual factor** (task with two levels:
  *pos* and *neg*) and a **between-individual quantitative variable** (*age*).
  The GLMM analysis is conducted using a **Student's t-distribution** for the
  model residuals.  
  
  #### Reserved Keywords for Post-Hoc Estimations  
  The following four reserved keywords should not be used in custom
  specifications for post-hoc estimations:  
  
  - **LAB**:   Used to define a label for the estimated effect.  
  - **CAT**:   Specifies a categorical variable for which effects are estimated
               each level, and all possible pairwise comparisons. Use *1* for
               the intercept or overall mean of the model.  
  - **FIX**:   Indicates variables fixed at specific levels or values.  
  - **QUANT**: Specifies the estimation of a slope for a quantitative variable.  
  
  ---
  
  #### Explanations for Post-Hoc Estimations  
  
  1. **`-level LAB task CAT task`**  
     - Estimates the effects for both levels of the task (*pos* and *neg*) and
       their contrast (evaluated at *age = 0*).  
  
  2. **`-level LAB pos.slp2 CAT 1 FIX task=pos,age=2`**  
     - Estimates the effect of the *pos* task at *age = 2* (relative to the
       centered value of age). The number *1* represents the intercept or grand
       mean of the model.  
  
  3. **`-slope LAB pos.age CAT 1 FIX task=pos QUANT age`**  
     - Estimates the slope effect of *age* for the *pos* task. The number *1* 
       represents the intercept or grand mean of the model.
  
  4. **`-slope LAB task.by.age CAT task QUANT age`**  
     - Estimates the slope effect of *age* for both *pos* and *neg* tasks and
       their contrast.
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
   cat(intro, ex1, ss, sep='\n')

   if (adieu) exit.AFNI();
}
   
#Change command line arguments into an options list
read.GLMM.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output file name. For AFNI format, provide prefix only,",
   "         with no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached (otherwise the output would be saved in AFNI format).\n", sep = '\n'
      ) ),

      '-resid' = apl(n = 1, d = NA,  h = paste(
   "-resid PREFIX: Output file name for the residuals. For AFNI format, provide",
   "         prefix only without view+suffix. Filename for NIfTI format should",
   "         have .nii attached, while file name for surface data is expected",
   "         to end with .niml.dset. The sub-brick labeled with the '(Intercept)',",
   "         if present, should be interpreted as the effect with each factor",
   "         at the reference level (alphabetically the lowest level) for each",
   "         factor and with each quantitative covariate at the center value.\n", sep = '\n'
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

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: Specify the model structure for all the variables. The",
   "         expression FORMULA with more than one variable has to be surrounded",
   "         within (single or double) quotes. Variable names in the formula",
   "         should be consistent with the ones used in the header of -dataTable.",
   "         In the GLMM context the simplest model is \"1+(1|Subj)\" in",
   "         which the random effect from each of the two subjects in a pair is",
   "         symmetrically incorporated in the model. Each random-effects factor is",
   "         specified within parentheses per formula convention in R. Any",
   "         effects of interest and confounding variables (quantitative or",
   "         categorical variables) can be added as fixed effects without parentheses.\n", sep = '\n'
             ) ),

       '-dbgArgs' = apl(n=0, d = NA, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dGLMM.dbg.AFNI.args in the current directory",
   "          so that debugging can be performed.\n", sep='\n')),
   
       '-family' = apl(n=1, h = paste(
   "-family: This option specifies the distribution of model residuals. Currently",
   "         two families are supported: \"Gaussian\" (default) and \"student.t\".\n", sep='\n')),
   
       '-R2' = apl(n=0, h = paste(
   "-R2: Enabling this option will prompt the program to provide both",
   "         conditional and marginal coefficient of determination (R^2)",
   "         values associated with the adopted model. Marginal R^2 indicates",
   "         the proportion of variance explained by the fixed effects in the",
   "         model, while conditional R^2 represents the proportion of variance",
   "         explained by the entire model, encompassing both fixed and random",
   "         effects. Two sub-bricks labeled 'R2m' and 'R2c' will be provided",
   "         in the output.\n", sep='\n')),

       '-bounds' = apl(n=2, h = paste(
   "-bounds lb ub: This option is for outlier removal. Two numbers are expected from",
   "         the user: the lower bound (lb) and the upper bound (ub). The input data will",
   "         be confined within [lb, ub]: any values in the input data that are beyond",
   "         the bounds will be removed and treated as missing. Make sure the first number",
   "         is less than the second. The default (the absence of this option) is no",
   "         outlier removal. ",
   "         **NOTE**: Using the -bounds option to remove outliers should be approached",
   "         with caution due to its arbitrariness. A more principled alternative is to",
   "         use the -family option with a Student's t-distribution.\n", sep='\n')),

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

      '-vVars' = apl(n=c(1,100), d=NA, h = paste(
   "-vVars variable_list: Identify voxel-wise covariates with this option.",
   "         Currently one voxel-wise covariate is allowed only. By default",
   "         mean centering is performed voxel-wise across all subjects.",
   "         Alternatively centering can be specified through a global value",
   "         under -vVarsCenters. If the voxel-wise covariates have already",
   "         been centered, set the centers at 0 with -vVarsCenters.\n",
             sep = '\n'
             ) ),

     '-vVarCenters' = apl(n=1, d=NA, h = paste(
   "-vVarCenters VALUES: Specify centering values for voxel-wise covariates",
   "         identified under -vVars. Multiple centers are separated by ",
   "         commas (,) within (single or double) quotes. The order of the",
   "         values should match that of the quantitative variables in -qVars.",
   "         Default (absence of option -vVarsCenters) means centering on the",
   "         average of the variable across ALL subjects regardless their",
   "         grouping. If within-group centering is desirable, center the",
   "         variable yourself first before the files are fed under -dataTable.\n",
             sep = '\n'
                     ) ),

     '-level' = apl(n=c(1,1000), d=NA, h = paste(
   "-level LAB ... CAT ... BY ... FIX ...: Specify the label, categorical variable",
   "       ....  \n", sep = '\n'
                     ) ), 

     '-slope' = apl(n=c(1,1000), d=NA, h = paste(
   "-slope LAB ... CAT ... BY ... FIX ... QUANT ...: Specify the label, categorical variable",
   "       ....  \n", sep = '\n'
                     ) ),

     '-qVarCenters' = apl(n=c(1,100), d=NA, h = paste(
   "-qVarCenters VALUES: Specify centering values for quantitative variables",
   "         identified under -qVars. Multiple centers are separated by ",
   "         commas (,) without any other characters such as spaces and should",
   "         be surrounded within (single or double) quotes. The order of the",
   "         values should match that of the quantitative variables in -qVars.",
   "         Default (absence of option -qVarCenters) means centering on the",
   "         average of the variable across ALL subjects regardless their",
   "         grouping. If within-group centering is desirable, center the",
   "         variable YOURSELF first before the values are fed into -dataTable.\n",
             sep = '\n'
                     ) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure with a header as the first line.\n",
   "         NOTE:\n",
   "         1) This option has to occur last in the script; that is, no other",
   "         options are allowed thereafter. Each line should end with a backslash",
   "         except for the last line.\n",
   "         2) The order of the columns should not matter except that the last",
   "         column has to be the one for input files, 'InputFile'. Unlike 3dGLMM, the",
   "         subject column (Subj in 3dGLMM) does not have to be the first column;",
   "         and it does not have to include a subject ID column under some situations",
   "         Each row should contain only one input file in the table of long format",
   "         (cf. wide format) as defined in R. Input files can be in AFNI, NIfTI or",
   "         surface format. AFNI files can be specified with sub-brick selector (square",
   "         brackets [] within quotes) specified with a number or label.\n",
   "         3) It is fine to have variables (or columns) in the table that are",
   "         not modeled in the analysis.\n",
   "         4) When the table is part of the script, a backslash is needed at the end",
   "         of each line (except for the last line) to indicate the continuation to the",
   "         next line. Alternatively, one can save the context of the table as a separate",
   "         file, e.g., calling it table.txt, and then in the script specify the data",
   "         with '-dataTable @table.txt'. However, when the table is provided as a",
   "         separate file, do NOT put any quotes around the square brackets for each",
   "         sub-brick, otherwise the program would not properly read the files, unlike the",
   "         situation when quotes are required if the table is included as part of the",
   "         script. Backslash is also not needed at the end of each line, but it would",
   "         not cause any problem if present. This option of separating the table from",
   "         the script is useful: (a) when there are many input files so that the program",
   "         complains with an 'Arg list too long' error; (b) when you want to try",
   "         different models with the same dataset.\n",
             sep = '\n'
                     ) ),

     '-SS_type' = apl(n=1, d=3, h = paste(
   "-SS_type NUMBER: Specify the type for sums of squares in the F-statistics.",
   "         Three options are: sequential (1), hierarchical (2), and marginal (3).",
   "         When this option is absent (default), marginal (3) is automatically set.",
   "         Some discussion regarding their differences can be found here:",
   "         https://sscc.nimh.nih.gov/sscc/gangc/SS.html\n ",
             sep = '\n'
                     ) ),		     

      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" ),

       '-cio' = apl(n=0, h = paste(
   "-cio: Use AFNI's C io functions, which is the default. Alternatively, -Rio",
   "         can be used.\n", sep='\n')),
       '-Rio' = apl(n=0, h = "-Rio: Use R's io functions. The alternative is -cio.\n")

         )
   #browser()
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   if (verb) show.AFNI.args(ops, verb=0, hstr='')
   if (is.null(ops))
      errex.AFNI('Error parsing arguments. See 3dGLMM -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$cutoff <- 0
      #lop$fixEff  <- 1
      lop$maskFN <- NA
      lop$IF     <- 'InputFile' #default

      #lop$ranEff <- NA
      lop$model  <- NA
      lop$qVars  <- NA
      lop$bounds <- NULL
      lop$vVars  <- NA
      lop$vQV    <- NA
      lop$qVarCenters <- NA
      lop$vVarCenters <- NA
      lop$level    <- NULL
      lop$slope    <- NULL
      lop$dataTable <- NULL
      lop$SS_type <- 3
      lop$R2      <- FALSE 

      lop$iometh  <- 'clib'
      lop$dbgArgs <- FALSE # for debugging purpose
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
             family = lop$family <- ops[[i]],
	     model  = lop$model  <- ops[[i]],
	     IF     = lop$IF     <- ops[[i]],
	     level   = lop$level   <- ops[[i]],
	     slope   = lop$slope <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             bounds = lop$bounds <- ops[[i]],
             vVars  = lop$vVars  <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             vVarCenters = lop$vVarCenters <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
	     SS_type = lop$SS_type <- ops[[i]],
	     
	     R2      = lop$R2 <- TRUE,
             help = help.GLMM.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }

   return(lop)
}# end of read.GLMM.opts.batch

glfConstr <- function(cStr, dataStr) {
   pos <- which(cStr==":")
   vars  <- cStr[pos-1]  # factors to be specified
   nvar <- length(vars)
   pos <- c(pos, length(cStr)+2) # add an artificial one for convenient usage below
   varsOK <- vars %in% colnames(dataStr)
   if(all(varsOK)) {
      glfList <- vector('list', nvar)
      names(glfList) <- vars
      for(ii in 1:nvar) {
	 lvl  <- levels(dataStr[,vars[ii]])   # all the levels for each involved factor
         fpos <- which(cStr[(pos[ii]+1):(pos[ii+1]-2)]=="&")  # positions of &'s
         nc   <- length(fpos) # number of columns for this factor minus 1
            glfList[[ii]] <- matrix(0, nrow=length(lvl), ncol=nc+1)
            fpos <- c(0, fpos, length(cStr[(pos[ii]+1):(pos[ii+1]-2)])+1)
            for(jj in 1:(length(fpos)-1)) {
               sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)][(fpos[jj]+1):(fpos[jj+1]-1)], strsplit, '\\*'))
	       lvlInv <- sepTerms[seq(2,length(sepTerms),2)]   # levels involved
	       lvlOK <- lvlInv %in% lvl
	       if(all(lvlOK)) {
	          sq <- match(lvlInv, lvl)
                  glfList[[ii]][sq,jj] <- as.numeric(sepTerms[seq(1,length(sepTerms),2)])
	       } else errex.AFNI(paste("Incorrect level coding in variable", vars[ii],
	         ": ", lvlInv[which(!lvlOK)], " \n   "))
            }
         #}
      }
      return(glfList)
   } else errex.AFNI(paste("Incorrect variable name in GLF coding: ", vars[which(!varsOK)], " \n   "))
}
# glfConstr(lop$glfCode[[1]], lop$dataStr)


#Change options list to 3dGLMM variable list
process.GLMM.opts <- function (lop, verb = 0) {
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
      if(an2$type != 'BRIK' && lop$iometh != 'clib')
          errex.AFNI(c('Must of use -cio option with any input/output ',
                       'format other than BRIK'))
   }
   # assume the quantitative variables are separated by + here
   #if(!is.na(lop$qVars)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]
   if(!identical(lop$qVars, NA)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]
   if(!is.na(lop$vVars[1])) lop$vQV <- strsplit(lop$vVars, '\\,')[[1]]

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
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      if(!identical(lop$qVars, NA)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      #if(!is.na(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
      for(ii in 1:(wd-1)) if(sapply(lop$dataStr, class)[ii] == "character")
         lop$dataStr[,ii] <- as.factor(lop$dataStr[,ii])
      if(!is.na(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
   }


   parse_strings <- function(input) {
     output <- rep(NA, 4)  # Initialize output with NA values
     
     # Find indices of key markers
     lab_idx <- which(input == "LAB")
     cat_idx <- which(input == "CAT")
     fix_idx <- which(input == "FIX")
     quant_idx <- which(input == "QUANT")
     
     # 1. Between LAB and CAT
     if (length(lab_idx) > 0 && length(cat_idx) > 0 && cat_idx > lab_idx + 1) {
       output[1] <- input[lab_idx + 1]
     }
     
     # 2. After CAT (before FIX or QUANT)
     if (length(cat_idx) > 0) {
       end_idx <- min(c(ifelse(length(fix_idx) > 0, fix_idx, Inf), 
                        ifelse(length(quant_idx) > 0, quant_idx, Inf))) - 1
       if (is.finite(end_idx) && end_idx >= cat_idx + 1) {
         output[2] <- paste0("pairwise~", input[cat_idx + 1])
       } else if (cat_idx + 1 <= length(input)) {
         # Handle case when there are no FIX or QUANT delimiters
         output[2] <- paste0("pairwise~", input[cat_idx + 1])
       }
     }
     
     # 3. After FIX and before QUANT
     if (length(fix_idx) > 0) {
       end_idx <- ifelse(length(quant_idx) > 0, quant_idx - 1, length(input))
       if (end_idx > fix_idx) {
         section <- input[(fix_idx + 1):end_idx]
         # Split key-value pairs by comma
         section_split <- unlist(strsplit(section, ","))
         section_parsed <- sapply(section_split, function(x) {
           if (grepl("=", x)) {
             key_value <- strsplit(x, "=")[[1]]
             if (!grepl("^\\d+$", key_value[2])) {
               key_value[2] <- paste0("'", key_value[2], "'")
             }
             paste0(key_value[1], "=", key_value[2])
           } else {
             x
           }
         })
         output[3] <- paste0("c(", paste(section_parsed, collapse = ","), ")")
       }
     }
     
     # 4. After QUANT
     if (length(quant_idx) > 0 && quant_idx < length(input)) {
       output[4] <- input[quant_idx + 1]
     }
     
     return(output)
   }

   if(!is.null(lop$level)) {
      strings <- lapply(lop$level, parse_strings)
      lop$level.LAB  <- unlist(lapply(strings, `[`, 1))
      lop$level.pair <- lapply((lapply(strings, `[`, 2)), formula)
      lop$level.fix  <- unlist(lapply(strings, `[`, 3))
   }

   if(!is.null(lop$slope)) {
      strings <- lapply(lop$slope, parse_strings)
      lop$slope.LAB  <- unlist(lapply(strings, `[`, 1))
      lop$slope.pair <- lapply((lapply(strings, `[`, 2)), formula)
      lop$slope.fix  <- unlist(lapply(strings, `[`, 3))
      lop$slope.slp  <- unlist(lapply(strings, `[`, 4))
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
      #lop$maskData <- mm$brk[,,,1]
      #lop$maskData <- mm$brk
      lop$maskData <- ifelse(abs(mm$brk) > tolL, 1, 0) # 01/17/2023: sometimes mask is defined as 0s and nonzeros
      if(verb) cat("Done read ", lop$maskFN,'\n')
      if(dim(mm$brk)[4] > 1) stop("More than 1 sub-brick in the mask file!") 
   }
   #if(!is.na(lop$maskFN))
   #   if(!all(dim(lop$maskData)==lop$myDim[1:3]))
   #      stop("Mask dimensions don't match the input files!")
   return(lop)
}
# process.GLMM.opts(lop, 0)

#################################################################################
################# GLMM Computation functions ##############################
#################################################################################

# GLMM
#runGLMM <- function(myData, DM, tag) {
#    
#  # Helper function for voxel-wise centering
#  voxel_wise_centering <- function(DM, vQV, myData, vVarCenters) {
#    if (any(!is.na(vQV))) {
#      DM <- assVV2(DM, vQV, myData[(length(myData) / 2 + 1):length(myData)], all(is.na(vVarCenters)))
#    }
#    return(DM)
#  }
#  
#  # Helper function to attempt glmmTMB fitting with various start values
#  glmmTMB_t <- function(DM, model, start_DFs) {
#    for (psi_value in start_DFs) {
#      captured_result <- tryCatch({
#        capture.output({
#          fm <- glmmTMB(
#            model,
#            data = DM,
#            family = t_family(),
#            start = list(psi = log(psi_value)),
#            map = list(psi = factor(NA))
#          )
#        }, type = "message")
#        list(fm = fm, success = TRUE)
#      }, error = function(e) {
#        list(fm = NULL, success = FALSE)
#      })
#      if (captured_result$success) return(captured_result$fm)
#    }
#    return(NULL)
#  }
#  
#  # Helper function for post hoc tests
#  post_hoc_tests <- function(fm, levels, level_pairs, level_fix, slope_pairs, slope_fix, slope_slp, z) {
#    Stat <- NULL
#    # Level-based contrasts
#    for (i in seq_along(levels)) {
#      glt <- suppressMessages(emmeans(fm, level_pairs[[i]], at = level_fix[i]))
#      tmp1 <- as.data.frame(glt$emmeans)
#      Stat <- c(Stat, c(rbind(tmp1$emmean, tmp1$emmean / tmp1$SE)))
#      tmp2 <- as.data.frame(glt$contrasts)
#      if (!any(is.na(tmp2$SE))) {
#        ratio <- if (z) tmp2$z.ratio else tmp2$t.ratio
#        Stat <- c(Stat, c(rbind(tmp2$estimate, ratio)))
#      }
#    }
#    # Slope-based contrasts
#    for (i in seq_along(slope_pairs)) {
#      glt <- suppressMessages(emmeans(fm, slope_pairs[[i]], at = slope_fix[i], var = slope_slp[i]))
#      tmp1 <- as.data.frame(glt$emmeans)
#      Stat <- c(Stat, c(rbind(tmp1$emmean, tmp1$emmean / tmp1$SE)))
#      tmp2 <- as.data.frame(glt$contrasts)
#      if (!any(is.na(tmp2$SE))) {
#        ratio <- if (z) tmp2$z.ratio else tmp2$t.ratio
#        Stat <- c(Stat, c(rbind(tmp2$estimate, ratio)))
#      }
#    }
#    return(Stat)
#  }
#
#    
#  ## main part ##
#  Stat <- NULL
#  
#  # Handle missing residuals
#  resid <- if (!is.null(lop$resid)) rep(0, nrow(lop$dataStr)) else NULL
#  res.na <- if (!is.null(lop$resid)) which(is.na(myData)) else NULL
#  
#  # Voxel-wise centering
#  DM <- voxel_wise_centering(DM, lop$vQV, myData, lop$vVarCenters)    
#  DM$yy <- myData[1:nrow(DM)]
#
#  # Skip voxels with >75% zeros
#  if (mean(na.omit(myData) != 0) < 0.75) {
#    #if (!is.null(Stat)) Stat <- c(Stat, resid)
#    #return(ifelse(is.null(Stat), rep(0, lop$NoBrick), Stat))
#    Stat <- ifelse(is.null(lop$resid), rep(0, lop$NoBrick), c(rep(0, lop$NoBrick), resid))
#    return(Stat)
#  } else {
#
#    # Model fitting      
#    start_DFs <- c(9, 30, 50) # DFs for Student-t 
#    fm <- glmmTMB_t(DM, lop$model, start_DFs)
#        
#    if (!is.null(fm)) {
#      # Collect statistics
#      Stat <- Anova(fm, type = lop$SS_type)[, 1]
#      Stat <- c(Stat, post_hoc_tests(fm, lop$level, lop$level.pair, lop$level.fix, lop$slope.pair, lop$slope.fix, lop$slope.slp, lop$z))
#      if (lop$R2) Stat <- c(Stat, r.squaredGLMM(fm))
#      if (!is.null(lop$resid)) {
#        resid <- unname(residuals(fm))
#        if (!is.null(res.na)) resid <- replace(resid, res.na, 0)
#        Stat <- c(Stat, resid)
#      }
#    }
#    
#    # Fallback to default
#    if (is.null(Stat)) Stat <- rep(0, lop$NoBrick)
#    if (!is.null(lop$resid)) Stat <- c(Stat, resid)
#    return(Stat)   
#  }
#}
# runGLMM(inData[30,30,30,], lop$dataStr, 0)


runGLMM <- function(myData, DM, tag) {
   #browser()
   Stat <- NULL
   if(!is.null(lop$resid)) {
      resid <- rep(0, nrow(lop$dataStr))
      res.na <- which(is.na(myData)) # indices for missing data
   }
   if(any(!is.na(lop$vQV))) {  # voxel-wise centering for voxel-wise covariate
      DM <- assVV2(DM, lop$vQV, myData[(length(myData)/2+1):length(myData)], all(is.na(lop$vVarCenters)))
   }

   if(mean(na.omit(myData) != 0) >= 0.75) {
      DM$yy <- myData[1:nrow(DM)]
      options(warn=-1)
      fm <- NULL
      if(is.null(lop$family))
         try(fm <- glmmTMB(lop$model, data=DM), silent=TRUE) else {
         if(lop$family=='student.t') 
            try(fm <-glmmTMB_t(lop$model, DM, c(9,30,50)), silent=TRUE)
      }

      is_model_ok <- function(fm) {
        ll <- suppressWarnings(logLik(fm))
        coefs <- suppressWarnings(unlist(coef(fm))) #coefs <- suppressWarnings(coef(fm)$cond[[1]])
        is.finite(ll) && all(is.finite(coefs))
      }
      if(!is_model_ok(fm)) {
        if(is.null(lop$family))
          try(fm <- glmmTMB(lop$model, data=DM, control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))), silent=TRUE) else {
         if(lop$family=='student.t') 
            try(fm <-glmmTMB_t(lop$model, DM, c(9,30,50), control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))), silent=TRUE)
      }
      }         
      if(!is.null(fm)) {
         Stat <- Anova(fm, type=lop$SS_type)[,1]
         if(length(lop$level) > 0) for(ii in 1:length(lop$level)) {
            glt  <- suppressMessages(emmeans(fm, lop$level.pair[[ii]], at=lop$level.fix[ii]))
            tmp1 <- as.data.frame(glt$emmeans)
            Stat <- c(Stat,  c(rbind(tmp1[,'emmean'], tmp1[,'emmean']/tmp1[,'SE'])))
            if(!any(is.na(as.data.frame(glt$contrasts)['SE']))) {
               tmp2 <- as.data.frame(glt$contrasts)
               if(lop$z) Stat <- c(Stat,  c(rbind(tmp2[,'estimate'], tmp2[,'z.ratio'])) ) else
                  Stat <- c(Stat,  c(rbind(tmp2[,'estimate'], tmp2[,'t.ratio'])))
            }
         }

         if(length(lop$slope) > 0) for(ii in 1:length(lop$slope)) {
            glt  <- suppressMessages(emmeans(fm, lop$slope.pair[[ii]], at=lop$slope.fix[ii], var=lop$slope.slp[ii]))
            tmp1 <- as.data.frame(glt$emmeans)
            Stat <- c(Stat,  c(rbind(tmp1[,'emmean'], tmp1[,'emmean']/tmp1[,'SE'])))
            if(!any(is.na(as.data.frame(glt$contrasts)['SE']))) {
               tmp2 <- as.data.frame(glt$contrasts)
               if(lop$z) Stat <- c(Stat,  c(rbind(tmp2[,'estimate'], tmp2[,'z.ratio']))) else
                  Stat <- c(Stat,  c(rbind(tmp2[,'estimate'], tmp2[,'t.ratio'])))
            }
         }
         if(lop$R2) Stat <- c(Stat, r.squaredGLMM(fm))
         if(!is.null(lop$resid)) {
            resid <- unname(residuals(fm))
            if(!is.null(res.na)) for(aa in res.na) resid <- append(resid, 0, after=aa-1) # fill in missing data with 0s 
         }
      }
   }
   if(is.null(Stat)) Stat <- rep(0, lop$NoBrick)
   if(!is.null(lop$resid)) Stat <- c(Stat, resid)
   return(Stat)
}
# runGLMM(inData[1,2,3,], lop$dataStr, 0)

assVV2 <- function(DF, vQV, value, c) {
      # centering - c: center; value: voxel-wise value; vQV: voxel-wise variable name; DF: dataframe
   #browser()
   if(is.na(c)) DF[, vQV] <- scale(value, center=TRUE, scale=F) else
       DF[, vQV] <- scale(value, center=c, scale=F)
   DF[, vQV] <- as.numeric(DF[, vQV])
   return(DF)
}

#################################################################################
########################## Begin GLMM main ######################################
#################################################################################

   if(!exists('.DBG_args')) {
      args = (commandArgs(TRUE))
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dGLMM.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.GLMM(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n",
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.GLMM.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)

   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.GLMM.opts.batch(args, verb = 0)))
         stop('Error parsing input')

      #str(lop);
      if(is.null(lop <- process.GLMM.opts(lop, verb = lop$verb)))
         stop('Error processing input')

   }
   #if(lop$verb > 1) {
      #Too much output, big dump of header structs of input dsets..
   #   str(lop)
   #}


########################################################################

pkgLoad('glmmTMB')
pkgLoad('car')
pkgLoad('emmeans')
options(contrasts = c("contr.sum", "contr.poly"))

if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])

# effect coding leads to the same type III as SAS
options(contrasts = c("contr.sum", "contr.poly"))

# standardize the names for Y, ROI and subject
#names(lop$dataStr)[names(lop$dataStr)==lop$Subj] <- 'Subj'
names(lop$dataStr)[names(lop$dataStr)==lop$IF] <- 'InputFile'

# Maybe not list for these two, or yes?
#lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)
#if(is.null(lop$Tstat)) lop$dataStr$Tstat <-  as.character(lop$dataStr$Tstat)

# center on user-speficied value or mean
if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters)))
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F)[,,drop=T] else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)[,,drop=T]

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Data Structure Overview *****\n')

#nS <- levels(lop$dataStr$Subj) # total number of subjects
nF <- dim(lop$dataStr[1])[1] # number of input files

#cat(nS, 'subjects in total:', nS, '\n')
cat(nF, 'response values\n')

if(dim(lop$dataStr)[2] > 3) for(ii in 3:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':',
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'numeric' | class(lop$dataStr[,ii]) == 'matrix')  # numeric doesn't work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')

cat('***** End of data structure information *****\n')
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('Reading input files now...\n\n')

# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(lop$dataStr[1, 'InputFile'], verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData

# Read in all input files
inData <- unlist(lapply(lapply(lop$dataStr[, 'InputFile'], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
tryCatch(dim(inData) <- c(dimx, dimy, dimz, nF), error=function(e)
   errex.AFNI(c("Dimension mismatch!\n",
   "Check files in the InputFile column all have\n",
   " (1) a single timepoint/value per voxel\n",
   "     Use sub-brick selectors if \"3dinfo -nt\" is >1 volume",
   " (2) the same number of voxels along X, Y, and Z axes\n",
   "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
   "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files for effect estimates: Done!\n\n')

# masking
if(!is.na(lop$maskFN)) {
   #Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk[,,,1]
   if(!all(c(dimx, dimy, dimz)==dim(lop$maskData)[1:3])) stop("Mask dimensions don't match the input files!")
   lop$maskData <- array(lop$maskData, dim=c(dimx, dimy, dimz))
   inData <- array(apply(inData, 4, function(x) x*(abs(lop$maskData)>tolL)), dim=c(dimx,dimy,dimz,nF))
   #if(!is.na(lop$dataStr$tStat)) inDataV <- array(apply(inDataV, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,nF))
}

# voxel-wise covariate files
if(!is.na(lop$vQV)) {
   tmpDat <- read.AFNI(as.character(unique(lop$dataStr[,lop$vQV[1]])[1]), verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
   dimx <- tmpDat$dim[1]
   dimy <- tmpDat$dim[2]
   dimz <- tmpDat$dim[3]
   head <- tmpDat
   #for(ii in lop$vQV)
   #if(length(unique(lop$dataStr[,ii])) != nlevels(lop$dataStr$Subj))
   #   errex.AFNI(c("Error with voxel-wise covariate ", ii, ": Each subject is only\n",
   #             "allowed to have one volume; that is, the covariate has to be at the\n",
   #             "subject level.")) else {  # currently consider one voxel-wise covariate only: may generalize later?
      #vQV <- unlist(lapply(lapply(unique(lop$dataStr[,lop$vQV[1]]), read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
      vQV <- unlist(lapply(lapply(as.character(lop$dataStr[,lop$vQV[1]]), read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
      #dim(vQV) <- c(dimx, dimy, dimz, length(unique(lop$dataStr[,lop$vQV[1]])))
      dim(vQV) <- c(dimx, dimy, dimz, length(lop$dataStr[,lop$vQV[1]]))
      inData <- c(inData, vQV)
      #dim(inData) <- c(dimx, dimy, dimz, lop$nVVars+lop$nSubj)
      dim(inData) <- c(dimx, dimy, dimz, 2*length(lop$dataStr[,lop$vQV[1]]))
   #}
} else vQV <- NULL

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

# show the range of input data
rg <- range(inData)
cat(paste0('\nRange of input data: [', sprintf(rg[1], fmt = '%#.3f'), ', ', sprintf(rg[2], fmt = '%#.3f'), ']\n\n'))

# outlier removal
if(!is.null(lop$bounds)) {
   inData[inData > lop$bounds[2]] <- NA
   inData[inData < lop$bounds[1]] <- NA
   cat(paste0('\nInput data confined within [', lop$bounds[1], ', ', lop$bounds[2], ']\n\n'))
}

cat('If the program hangs for an extended period (e.g., more than 5\n')
cat('minutes), terminate the process as it likely indicates an issue with\n')
cat('the model specification or other factors.\n\n')


# wrapper function glmmTMB_t() for modeling through a t-distribuion with 
# currently with 3 start DFs (currently 9, 30, 50). Often DF of 9 works
# as a start value leads to convergence, but it may occasionally fail.
# Possible convergence indicators: 
# 1) error message: even though various estimates are provided; 
# 2) NA information criteria (AIC/BIC), logLik, deviance: any(is.na(summary(fm)$AICtab))
# 3) NaNs standard errors for estimated parameters: any(sapply(Anova(fm, type=3)[,c('Chisq', 'Pr(>Chisq)')], is.nan))
glmmTMB_t <- function(model, DM, start_DFs) {
  for (psi_value in start_DFs) {
    captured_result <- tryCatch({
      capture.output({
        fm <- glmmTMB(
          model,
          data = DM,
          family = t_family(),
          start = list(psi = log(psi_value)),
          map = list(psi = factor(NA))
        )
      }, type = "message")
      if(!any(is.na(summary(fm)$AICtab)) & !any(sapply(Anova(fm, type=3)[,c('Chisq', 'Pr(>Chisq)')], is.nan)))
          list(fm = fm, success = TRUE) else
          list(fm = NULL, success = FALSE)
    }, error = function(e) {
      list(fm = NULL, success = FALSE)
    })
    if (captured_result$success) return(captured_result$fm)
  }
  return(NULL)
}

# pick up a test voxel
if(!is.na(lop$maskFN)) {
   idx <- which(lop$maskData == 1, arr.ind = T)

  # make sure there was something in the mask
  #if(length(idx)==0L) errex.AFNI(
  # c("Input mask has no voxels == 1!\n",
  #   "If your mask has many non-zero values (e.g an atlas),\n",
  #   "Run: 3dcalc -m ",lop$maskFN," -expr 'step(m)'"))

  idx <- idx[floor(dim(idx)[1]/2),1:3]
  xinit <- idx[1]; yinit <- idx[2]; zinit <- idx[3]
  ii <- xinit; jj <- yinit; kk <- zinit
} else {
  xinit <- dimx%/%3
  if(dimy==1) {xinit <-1; yinit <- 1} else yinit <- dimy%/%3
  if(dimz==1) {xinit <-1; zinit <- 1} else zinit <- dimz%/%3
  ii <- xinit; jj <- yinit; kk <- zinit
}

lop$model <- as.formula(paste('yy ~ ', lop$model))

fm <- NULL
if(any(!is.na(lop$vQV))) {
     lop$dataStr <- assVV2(lop$dataStr, lop$vQV, inData[ii,jj,kk,(nrow(lop$dataStr)+1):(2*nrow(lop$dataStr))], all(is.na(lop$vVarCenters)))
}

while(is.null(fm)) {
  if(mean(na.omit(inData[ii, jj, kk,1:nrow(lop$dataStr)]) != 0) >= 0.75) {
   lop$dataStr$yy <- inData[ii, jj, kk,1:nrow(lop$dataStr)]
   options(warn=-1)
   if(is.null(lop$family)) try(fm <- glmmTMB(lop$model, data=lop$dataStr), silent=TRUE) else 
      if(lop$family=='student.t') try(fm <- glmmTMB_t(lop$model, DM=lop$dataStr, c(9,30,50)), silent=TRUE)
   if(!is.null(fm)) {    
      brickNames <- paste(row.names(Anova(fm, type=lop$SS_type)), 'chisq')
      lop$n.omni <- length(brickNames)
      chisq.df <- (Anova(fm, type=lop$SS_type))[,'Df']
      lop$n.t <- 0; t.df <- NULL
      if(length(lop$level) > 0) {
         for(ll in 1:length(lop$level)) {
            glt  <- suppressMessages(emmeans(fm, lop$level.pair[[ll]], at=lop$level.fix[ll]))
            lop$n.t <- lop$n.t + 2*nrow(as.data.frame(glt$emmeans))
            t.df <- c(t.df, as.data.frame(glt$emmeans)[,'df'])
            if(any(is.infinite(t.df))) brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ll],'.', as.data.frame(glt$emmeans)[,1]),
                                  paste0(lop$level.LAB[ll],'.', as.data.frame(glt$emmeans)[,1], ' z'))) else
            brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ll],'.', as.data.frame(glt$emmeans)[,1]),
                                  paste0(lop$level.LAB[ll],'.', as.data.frame(glt$emmeans)[,1], ' t')))
            if(!any(is.na(as.data.frame(glt$contrasts)['SE']))) {
               lop$n.t <- lop$n.t + 2*nrow(as.data.frame(glt$contrasts))
               t.df <- c(t.df, as.data.frame(glt$contrasts)[,'df'])
               if(any(is.infinite(t.df))) brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ll],'.', as.data.frame(glt$contrast)[,1]),
                                  paste0(lop$level.LAB[ll],'.', as.data.frame(glt$contrast)[,1], ' z'))) else
               brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ll],'.', as.data.frame(glt$contrast)[,1]),
                                  paste0(lop$level.LAB[ll],'.', as.data.frame(glt$contrast)[,1], ' t')))
            }
         }
      }
      if(length(lop$slope) > 0) {
         for(ss in 1:length(lop$slope)) {
            glt  <- suppressMessages(emtrends(fm, lop$slope.pair[[ss]], at=lop$slope.fix[ss], var=lop$slope.slp[ss]))
            lop$n.t <- lop$n.t + 2*nrow(as.data.frame(glt$emtrends))
            t.df <- c(t.df, as.data.frame(glt$emtrends)[,'df'])
            if(any(is.infinite(t.df))) brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ss],'.', as.data.frame(glt$emtrends)[,1]),
                 paste0(lop$level.LAB[ss],'.', as.data.frame(glt$emtrends)[,1], ' z'))) else
            brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ss],'.',as.data.frame(glt$emtrends)[,1]),
                 paste0(lop$level.LAB[ss],'.', as.data.frame(glt$emtrends)[,1], ' t')))
            if(!any(is.na(as.data.frame(glt$contrasts)['SE']))) {
               lop$n.t <-lop$n.t + 2*nrow(as.data.frame(glt$contrasts))
               t.df <- c(t.df, as.data.frame(glt$contrasts)[,'df'])
               if(any(is.infinite(t.df))) brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ss],'.', as.data.frame(glt$contrast)[,1]),
                  paste0(lop$level.LAB[ss],'.', as.data.frame(glt$contrast)[,1], ' z'))) else
               brickNames <- c(brickNames, rbind(paste0(lop$level.LAB[ss],'.', as.data.frame(glt$contrast)[,1]),
                  paste0(lop$level.LAB[ss],'.', as.data.frame(glt$contrast)[,1], ' t')))
            }             
         }
      }
      lop$NoBrick <- lop$n.omni + lop$n.t
      if(any(is.infinite(t.df))) lop$z <- 1 else lop$z <- 0
      #fail <- FALSE
   }  
  } else fm <- NULL
   if(!is.null(fm))  {
      print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
   } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')
      cat('Possible causes include::\n\n')
      cat('0) Missing required R packages: glmmTMB, car, or emmeans. \n')
      cat('1) Incorrect model specification, such as errors in the -model or -qVars options.\n\n')
      cat('2) Errors in the specifications for -level or -slope options.\n\n')
      cat('3) Inconsistencies or errors in the data table.\n')
      cat('4) Variable name issues, such as typos or case sensitivity mismatches.\n')
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
      # break into 20 segments, leading to 5% incremental in parallel computing
      dimx_n <- dimx%/%nSeg + 1
      # number of datasets need to be filled
      fill <- nSeg-dimx%%nSeg
      # pad with extra 0s
      inData <- rbind(inData, array(0, dim=c(fill, nF)))
      # break input multiple segments for parrel computation
      dim(inData) <- c(dimx_n, nSeg, nF)
      Stat <- array(0, dim=c(dimx_n, nSeg, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))
      if (lop$nNodes==1) for(kk in 1:nSeg) {
         for(kk in 1:nSeg) {
            Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runGLMM, DM=lop$dataStr, tag=0), c(2,1))
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')
         }
      } # runGLMM(inData[30,1,], lop$model, lop$dataStr, lop$gltM, intercept, nF, nS, 0)
      
      if (lop$nNodes>1) {
         pkgLoad('snow')
         cl <- makeCluster(lop$nNodes, type = "SOCK")
         clusterExport(cl, c("lop", "assVV2", "glmmTMB_t"), envir=environment())
         clusterEvalQ(cl, library(glmmTMB)); clusterEvalQ(cl, library(car)); clusterEvalQ(cl, library(emmeans))
         if(lop$R2) clusterEvalQ(cl, library(MuMIn))
         clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
         for(kk in 1:nSeg) {
            Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runGLMM, DM=lop$dataStr, tag=0), c(2,1))
            cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
         } # runGLMM(inData[30,1,], lop$model, lop$dataStr, lop$gltM, intercept, nF, nS, 0)
         stopCluster(cl)
      }
      # convert to 4D
      dim(Stat) <- c(dimx_n*nSeg, 1, 1, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr))
      # remove the trailers (padded 0s)
      Stat <- Stat[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]
   } else { # volumetric data
      Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))
      if (lop$nNodes==1) {
         for (kk in 1:dimz) {
            if((lop$NoBrick > 1) | (!is.null(lop$resid))) Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runGLMM,
               DM=lop$dataStr, tag=0), c(2,3,1)) else
            Stat[,,kk,1] <- apply(inData[,,kk,], c(1,2), runGLMM, DM=lop$dataStr, tag=0)
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
      } else {
         pkgLoad('snow')
         cl <- makeCluster(lop$nNodes, type = "SOCK")
         clusterExport(cl, c("lop", "assVV2", "glmmTMB_t"), envir=environment())
         clusterEvalQ(cl, library(glmmTMB)); clusterEvalQ(cl, library(car)); clusterEvalQ(cl, library(emmeans))
         if(lop$R2) clusterEvalQ(cl, library(MuMIn))
         clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
         for (kk in 1:dimz) {
            if((lop$NoBrick > 1) | (!is.null(lop$resid))) Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runGLMM,
               DM=lop$dataStr, tag=0), c(2,3,1)) else
            Stat[,,kk,1] <- parApply(cl, inData[,,kk,], c(1,2), runGLMM, DM=lop$dataStr, tag=0)
            cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      } # volumetric data
      # runGLMM(inData[30,30,30,], lop$dataStr, 0)
   } # if (lop$nNodes>1)

Stat[is.nan(Stat)] <- 0
if(lop$R2) {
   brickNames <- append(brickNames, c('R2m', 'R2c'))
   lop$NoBrick <- lop$NoBrick+2
}   
statsym <- NULL
for(n in 1:lop$n.omni) statsym <- c(statsym, list(list(sb=n-1, typ="fict", par=chisq.df[n])))      
for(n in 1:lop$n.t) if(lop$z)
    statsym <- c(statsym, list(list(sb=lop$n.omni+2*n-1, typ="fizt"))) else
    statsym <- c(statsym, list(list(sb=lop$n.omni+2*n-1, typ="fitt", par=t.df[n])))

write.AFNI(lop$outFN, Stat[,,,1:lop$NoBrick,drop=FALSE], brickNames, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_float', scale=FALSE)

if(!is.null(lop$resid))
   write.AFNI(lop$resid, Stat[,,,(lop$NoBrick+1):(lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)), drop=FALSE],
      label=0:(dim(lop$dataStr)[1]-1), defhead=head, idcode=newid.AFNI(), com_hist=lop$com_history, type='MRI_float', scale=FALSE)

print(sprintf("Congratulations! Here's an output file from 3dGLMM -- hope it meets or even exceeds your expectations: %s", lop$outFN))

##############  END  ############

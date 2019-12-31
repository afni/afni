#!/usr/bin/env AFNI_Batch_R

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
  #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dLMEr'
# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin 3dLMEr Input functions ################################
#################################################################################

#The help function for 3dLMEr batch (AFNI-style script mode)
help.LME.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
             ================== Welcome to 3dLMEr ==================          
       Program for Voxelwise Linear Mixed-Effects (LME) Analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.1, Dec 31, 2019
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892, USA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Introduction
------
 
 Linear Mixed-Effects (LME) analysis adopts the traditional approach that
 differentiates two types of effects: fixed effects capture the population-
 level components while random effects characterize the lower-level components
 such as subjects, families, scanning sites, etc. 

 3dLMEr is a revised and advanced version of its older brother 3dLME in the sense
 that the former is much more flexible in specifying the random-effects components
 than the latter. Also, 3dLMEr uses the R package \'lmerTest\' while 3dLME was
 written  with the R package \'nlme\', and the statistic values for main effects
 and interactions are approximated with the Satterthwaite\'s approach. The greater
 flexibility of 3dLMEr lies in its adoption of random-effects notations by the R
 package 'lme4', as nicely summarized in the following table:

 http://afni.nimh.nih.gov/sscc/staff/gangc/pub/lmerNotations.pdf
 (adopted from https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)

 Similar to 3dLME, all the main effects and interactions are automatically available
 in the output while simple effects that tease apart those main effects and
 interactions would have to be requested through options -gltCode or -glfCode. Also,
 the 3dLMEr interface is largely similar to 3dLME except

 1) the random-effects components are incorporated as part of the model
 specification, and thus the user is fully responsible in properly formulating the
 model structure through \'-model ...\' (option -ranEeff in 3dLME is no longer
 necessary for 3dLMEr);

 2) the specifications for simple and composite effects through -gltCode and
 -glfCode are slightly simplified (the lable for each effect is part of -gltCode
 and -glfCode, and no more -gltLabel is needed); and

 3) all the statistic values for simple effects (specified through -gltCode) are
 stored in the output as Z-statisc while main effects, interactions and the
 composite effects (specified through -gltCode) are represented in the output as
 chi-square with 2 degrees of freedom. The fixed number of DFs (i.e., 2) for the
 chi-square statistic, regardless of the specific situation, is adopted for
 convenience because of the varying DFs due to the Satterthwaite approximation.

 If you want to cite the analysis approach, use the following reference:
 
 Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). Linear
 Mixed-Effects Modeling Approach to FMRI Group Analysis. NeuroImage 73:176-190.
 http://dx.doi.org/10.1016/j.neuroimage.2013.01.047

 Input files can be in AFNI, NIfTI, surface (niml.dset) or 1D format. To obtain 
 the output int the same format of the input, append a proper suffix to the 
 output specification option -prefix (e.g., .nii, .niml.dset or .1D for NIfTI,
 surface or 1D).

 3dLMEr allows for the incorporation of various types of explanatory variables
 including categorical (between- and within-subject factors) and
 quantitative variables (e.g., age, behavioral data). The burden of properly
 specifying the structure of lower-level effects is placed on the user\'s
 shoulder, so familiarize yourself with the following FAQ in case you want some
 clarifications: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

 Whenever a quantitative variable is involved, it is required to explicitly
 declare the variable through option -qVars. In addition, be mindful about the
 centering issue of each quantitive quantitative variable: you have to decide
 which makes more sense in the research context - global centering or within-
 condition (or within-group) centering? Here is some background and discussion
 about the issue:
 https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/statistics/center.html

 The following exemplifying scripts are good demonstrations. More examples will
 be added in the future if I could crowdsource more scenarios from the users
 (including you the reader). In case you find one example similar to your data
 structure, use the example(s) as a template and then build up your own script.
 
 In addition to R installtion, the following R packages need to be installed
 first before running 3dLMEr: "lmerTest", "phia" and "snow". To install these R
 packages, run the following command at the terminal:

 rPkgsInstall -pkgs "lmerTest,phia,snow"

 Alternatively you may install them in R:
 
 install.packages("lmerTest")
 install.packages("phia")
 install.packages("snow") 

 Once the 3dLMEr command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called LME.txt, and execute it with the 
 following (assuming on tc shell),
 
 nohup tcsh -x LME.txt &
 
 or,
 
 nohup tcsh -x LME.txt > diary.txt &
 nohup tcsh -x LME.txt |& tee diary.txt &

 The advantage of the latter commands is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.\n'

   ex1 <-  
"Example 1 --- Simplest case: LME analysis for one group of subjects each of
  which has three effects associated with three emotions (pos, neg and neu), 
  and the effects of interest are the comparisons among the three emotions
  at the populaton level (missing data allowed). This data structure is usually
  considered as one-way repeated-measures (or within-subject) ANOVA if no
  missing data occured. The LME model is typically formulated with a random
  intercept in this case. 

-------------------------------------------------------------------------
    3dLMEr -prefix LME -jobs 12                                     \\
          -model  'emotion+(1|Subj)                                 \\
          -gltCode pos      'emotion : 1*pos'                       \\
          -gltCode neg      'emotion : 1*neg'                       \\
          -gltCode neu      'emotion : 1*neu'                       \\
          -gltCode pos-neg  'emotion : 1*pos -1*neg'                \\
          -gltCode pos-neu  'emotion : 1*pos -1*neu'                \\
          -gltCode neg-neu  'emotion : 1*neg -1*neu'                \\
          -gltCode em-eff1  'emotion : 0.5*pos +0.5*neg -1*neu'     \\
          -glfCode em-eff2  'emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -dataTable                              \\
          Subj emotion  putFile                   \\
          s1    pos     s1_pos+tlrc               \\
          s1    neg     s1_neg+tlrc               \\
          s1    neu     s1_neu+tlrc               \\
          s2    pos     s2_pos+tlrc               \\
          s2    neg     s2_neg+tlrc               \\
          s2    pos     s2_neu+tlrc               \\
          ... 
          s20   pos     s20_pos+tlrc              \\
          s20   neg     s20_neg+tlrc              \\
          s20   neu     s20_neu+tlrc              \\
          ...                                   
   \n"

   ex2 <-  
"Example 2 --- LME analysis for one group of subjects each of  which has 
  three effects associated with three emotions (pos, neg and neu), and the 
  effects of interest are the comparisons among the three emotions  at the 
  populaton level. In addition, reaction time (RT) is avaible per emotion 
  from each subject. An LME model can be formulated to include both random 
  intercept and random slope. Be careful about the centering issue
  about any quantitive quantitative variable: you have to decide which makes
  more sense - global centering or within-condition (or within-group)
  centering?

-------------------------------------------------------------------------
    3dLMEr -prefix LME -jobs 12                   \\
          -model  'emotion*RT+(RT|Subj)           \\
          -qVars  'RT'                            \\
          -qVarCenters 0                          \\
          -gltCode pos      'emotion : 1*pos'                       \\
          -gltCode neg      'emotion : 1*neg'                       \\
          -gltCode neu      'emotion : 1*neu'                       \\
          -gltCode pos-neg  'emotion : 1*pos -1*neg'                \\
          -gltCode pos-neu  'emotion : 1*pos -1*neu'                \\
          -gltCode neg-neu  'emotion : 1*neg -1*neu'                \\
          -gltCode em-eff1  'emotion : 0.5*pos +0.5*neg -1*neu'     \\
          -glfCode em-eff2  'emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -dataTable                              \\
          Subj emotion  RT  InputFile             \\
          s1    pos     23   s1_pos+tlrc          \\
          s1    neg     34   s1_neg+tlrc          \\
          s1    neu     28   s1_neu+tlrc          \\
          s2    pos     31   s2_pos+tlrc          \\
          s2    neg     22   s2_neg+tlrc          \\
          s2    pos     29   s2_neu+tlrc          \\
          ... 		
          s20   pos     12   s20_pos+tlrc         \\
          s20   neg     20   s20_neg+tlrc         \\
          s20   neu     30   s20_neu+tlrc         \\
          ...                                   
   \n"

   ex3 <-  
"Example 3 --- LME analysis for one group of subjects each of which has three 
  effects associated with three emotions (pos, neg and neu), and the effects 
  of interest are the comparisons among the three emotions  at the populaton 
  level. As the data were acquired across 12 scanning sites,  we set up an LME 
  model with a crossed random-effects structure, one for cross-subjects and one 
  for cross-sites variability.

-------------------------------------------------------------------------
    3dLMEr -prefix LME -jobs 12                       \\
          -model  'emotion+(1|Subj)+(1|site)          \\
          -gltCode pos      'emotion : 1*pos'                       \\
          -gltCode neg      'emotion : 1*neg'                       \\
          -gltCode neu      'emotion : 1*neu'                       \\
          -gltCode pos-neg  'emotion : 1*pos -1*neg'                \\
          -gltCode pos-neu  'emotion : 1*pos -1*neu'                \\
          -gltCode neg-neu  'emotion : 1*neg -1*neu'                \\
          -gltCode em-eff1  'emotion : 0.5*pos +0.5*neg -1*neu'     \\
          -glfCode em-eff2  'emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -dataTable                                  \\
          Subj emotion  site  InputFile               \\
          s1    pos     site1   s1_pos+tlrc           \\
          s1    neg     site1   s1_neg+tlrc           \\
          s1    neu     site2   s1_neu+tlrc           \\
          s2    pos     site1   s2_pos+tlrc           \\
          s2    neg     site2   s2_neg+tlrc           \\
          s2    pos     site3   s2_neu+tlrc           \\
          ... 
          s80   pos     site12  s80_pos+tlrc          \\
          s80   neg     site12  s80_neg+tlrc          \\
          s80   neu     site10  s80_neu+tlrc          \\
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
   cat(intro, ex1, ex2, ex3, ss, sep='\n')
   
   if (adieu) exit.AFNI();
}
   
#Change command line arguments into an options list
read.LME.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output file name. For AFNI format, provide prefix only,",
   "         with no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached (otherwise the output would be saved in AFNI format).\n", sep = '\n'
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
   "         In the LME context the simplest model is \"1+(1|Subj1)+(1|Subj2)\"in",
   "         while the random effect from each of the two subjects in a pair is",
   "         symmetrically incorporated in the model. Each random-effects factor is",
   "         specified within paratheses per formula convention in R. Any",
   "         effects of intereste and confounding variables (quantitative or",
   "         categorical variables) can be added as fixed effects without paratheses.\n", sep = '\n'
             ) ),

       '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dLMEr.dbg.AFNI.args in the current directory",
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
   "         differ substantially in the average value of the covariate.",
   "         \n",
             sep = '\n'
             ) ),
	     
     '-gltCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltCode label weights: Specify the label and weights of interest. The",
   "       weights should be surrounded with quotes. For example, the specification",
   "       '-gltCode AvB 'Condition : 1*A -1*B'' compares A and B with a label",
   "       'AvB' for the output sub-bricks.  \n", sep = '\n'
                     ) ),

     '-glfCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-glfCode label CODING: Specify a general linear F-statistic (GLF) formulation",
   "         with the weights among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the coding",
   "         '-glf AvBvc 'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos''",
   "         examines the main effect of Condition at the positive Emotion with",
   "         the output labeled as AvBvC. Similarly the coding '-glf CondByEmo'",
   "         'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos -1*neg' looks",
   "         for the interaction between the three levels of Condition and the",
   "         two levels of Emotion and the resulting sub-brick is labeled as",
   "         'CondByEmo'.\n",
   "         NOTE:\n",
   "         1) The weights for a variable do not have to add up to 0.\n",   
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate unless the covariate",
   "         value is specified as, for example, 'Group : 1*Old Age : 2', where",
   "         the Old Group is tested at the Age of 2 above the center.\n",
   "         3)  The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLF.\n",
   "         4) The appearance of a categorical variable has to be followed",
   "         by the linear combination of its levels.\n",
             sep = '\n'
             ) ),

#      '-cVars' = apl(n=c(1,100), d=NA, h = paste(
#   "-cVars variable_list: Identify categorical (qualitive) variables (or",
#   "         factors) with this option. The list with more than one variable",
#   "         has to be separated with comma (,) without any other characters such",
#   "         as spaces and should be surrounded within (single or double) quotes.",
#   "         For example, -cVars \"sex,site\"\n",
#             sep = '\n'
#             ) ),
#
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
   "         4) The context of the table can be saved as a separate file, e.g.,",
   "         called table.txt. Do not forget to include a backslash at the end of",
   "         each row. In the script specify the data with '-dataTable @table.txt'.",
   "         However, when the table is provided as a separate file, do NOT put any",
   "         quotes around the square brackets for each sub-brick, otherwise the",
   "         program cannot properly read the files, unlike the situation when ",
   "         quotes are required if the table is included as part of the script.",
   "         Backslash is also not needed at the end of each line, but it would cause",
   "         any problem if it is present. This option of separating the table from",
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
      errex.AFNI('Error parsing arguments. See 3dLMEr -help for details.')

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
      #lop$vVars  <- NA
      #lop$vQV    <- NA
      lop$qVarCenters <- NA
      #lop$vVarCenters <- NA
      lop$gltCode    <- NULL
      lop$glfCode  <- NULL      
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
             Subj1  = lop$Subj1  <- ops[[i]],
	     Subj2  = lop$Subj2  <- ops[[i]],
	     IF     = lop$IF     <- ops[[i]],
	     #num_glt   = lop$num_glt   <- ops[[i]],
	     gltCode   = lop$gltCode   <- ops[[i]],
	     glfCode  = lop$glfCode <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             #vVars  = lop$vVars  <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             #vVarCenters = lop$vVarCenters <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
             
             help = help.LME.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }

   return(lop)
}# end of read.LME.opts.batch

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


#Change options list to 3dLMEr variable list 
process.LME.opts <- function (lop, verb = 0) {
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
   if(!is.na(lop$qVars)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]
   #if(!is.na(lop$vVars[1])) lop$vQV <- strsplit(lop$vVars, '\\,')[[1]]

   len <- length(lop$dataTable)
   wd <- which(lop$dataTable == lop$IF)  # assuming the input file is the last column here!
   hi <- len / wd - 1
   
   if(len %% wd != 0)
      errex.AFNI(paste('The content under -dataTable is not rectangular !', len, wd)) else {
      lop$dataStr <- NULL
      for(ii in 1:wd) lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      # wow, terrible mistake here with as.numeric(lop$dataStr[,jj])
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
      if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      #if(!is.na(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
   }

   if(!is.null(lop$gltCode)) {
      lop$gltLabel <- unlist(lapply(lop$gltCode, `[`, 1))
      lop$num_glt     <- length(lop$gltLabel)
      glt             <- gl_Constr(lop$num_glt, lapply(lop$gltCode, '[',-1), lop)
      lop$gltList     <- glt[[1]]
      lop$slpList     <- glt[[2]]
      lop$covValList  <- glt[[3]]
   }

   if(!is.null(lop$glfCode)) {
      lop$glfLabel <- unlist(lapply(lop$glfCode, `[`, 1))
      lop$num_glf     <- length(lop$glfLabel)
      glf             <- gl_Constr(lop$num_glf, lapply(lop$glfCode, '[',-1), lop)
      lop$glfList     <- glf[[1]]
      lop$slpListF     <- glf[[2]]
      lop$covValListF  <- glf[[3]]
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
# process.LME.opts(lop, 0)

#################################################################################
################# LME Computation functions ##############################
#################################################################################

# LME: bare-bone approach with LME for LME: some voxels may have 0 LME values: effect estimates as input only
runLME <- function(myData, DM, tag) {
   #browser()
   Stat <- rep(0, lop$NoBrick)
   if(!all(myData == 0)) {     
      DM$yy <- myData
      options(warn=-1)
      try(fm <- lmer(lop$model, data=DM), silent=TRUE)

      if(!is.null(fm)) {      
         #Stat[1:lop$nF] <- anova(fm)$`F value` # F-stat
	 Stat[1:lop$nF] <- qchisq(anova(fm)$`Pr(>F)`, 2, lower.tail = F) # convert to chisq
	 #qnorm(anova(fm)$`Pr(>F)`/2, lower.tail = F) # Z-stat: should use one-tailed!
	 if(lop$num_glt > 0) for(ii in 1:lop$num_glt) {
            tt <- NULL
            if(is.na(lop$gltList[[ii]])) tt <- tryCatch(testInteractions(fm, pairwise=NULL, slope=lop$slpList[[ii]], 
               covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL) else
            tt <- tryCatch(testInteractions(fm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
               covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL)
            if(!is.null(tt)) {
               Stat[lop$nF[1]+2*ii-1] <- tt[1,'Value']
	       Stat[lop$nF[1]+2*ii]   <- sign(tt[1,'Value'])*qnorm(tt[1,'Pr(>Chisq)']/2, lower.tail = F)  # convert chisq to Z
            } 
         }
	 if(lop$num_glf > 0) for(ii in 1:lop$num_glf) {
            ff <- NULL
            if(is.na(lop$glfList[[ii]])) ff <- tryCatch(testFactors(fm, pairwise=NULL,
	       slope=lop$slpListF[[ii]], covariates=lop$covValListF[[ii]],
	       adjustment="none")$terms$`(Intercept)`$test, error=function(e) NULL) else
	    ff <- tryCatch(testFactors(fm, levels=lop$glfList[[ii]],
	       slope=lop$slpListF[[ii]], covariates=lop$covValListF[[ii]],
	       adjustment="none")$terms$`(Intercept)`$test, error=function(e) NULL)
            if(!is.null(ff)) {
               Stat[lop$nF[1]+2*lop$num_glt+ii] <- qchisq(ff[2,'Pr(>Chisq)'], 2, lower.tail = F) # glf[2,2]  # convert chisq to Z
            } 
         }
      }
   }
   return(Stat)
}
# runLME(inData[30,30,30,], lop$dataStr, 0)

#################################################################################
########################## Begin LME main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dLMEr.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.LME(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.LME.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.LME.opts.batch(args, verb = 0)))
         stop('Error parsing input')
      
      #str(lop);
      if(is.null(lop <- process.LME.opts(lop, verb = lop$verb))) 
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
cat('***** Summary information of data structure *****\n')

#nS <- levels(lop$dataStr$Subj) # total number of subjects
nF <- dim(lop$dataStr[1])[1] # number of input files

#cat(nS, 'subjects in total:', nS, '\n') 
cat(nF, 'response values\n')

if(dim(lop$dataStr)[2] > 3) for(ii in 3:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':', 
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'numeric' | class(lop$dataStr[,ii]) == 'matrix')  # numeric doesn't work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')
#cat(lop$num_glt, 'post hoc tests\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')

cat('***** End of data structure information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('Reading input files now...\n\n')

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
   errex.AFNI(c("At least one of the input files has different dimensions!\n",
   "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
   "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files for effect estimates: Done!\n\n')

if (!is.na(lop$maskFN)) {
   Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk[,,,1]
   inData <- array(apply(inData, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,nF))
   #if(!is.na(lop$dataStr$tStat)) inDataV <- array(apply(inDataV, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,nF))
}


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

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or something else\n')
cat('is likely inappropriate.\n\n')

xinit <- dimx%/%3
if(dimy==1) yinit <- 1 else yinit <- dimy%/%3
if(dimz==1) zinit <- 1 else zinit <- dimz%/%3

ii <- xinit; jj <- yinit; kk <- zinit
#if(unlist(strsplit(lop$model, split="[+]"))[1]==1) {
#   intercept <- 1
#   lop$NoBrick <- 2
#} else {
#   intercept <- 0
#   lop$NoBrick <- 2*nrow(lop$gltM) # LME plus its F-stat
#}

lop$model <- as.formula(paste('yy ~ ', lop$model))
require(lmerTest)
require(phia)
fm<-NULL
gltRes <- vector('list', lop$num_glt)
glfRes <- vector('list', lop$num_glf)
#chi_DF <- NULL
while(is.null(fm)) {
   lop$dataStr$yy <- inData[ii, jj, kk,]
   options(warn=-1)  
   try(fm <- lmer(lop$model, data=lop$dataStr), silent=TRUE)
   if(!is.null(fm)) if (lop$num_glt > 0) {
      n <- 1
      while(!is.null(fm) & (n <= lop$num_glt)) {
         if(is.na(lop$gltList[[n]])) gltRes[[n]] <- tryCatch(testInteractions(fm, pairwise=NULL,
            covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA) else
         gltRes[[n]] <- tryCatch(testInteractions(fm, custom=lop$gltList[[n]],
            covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA)
         if(is.null(gltRes[[n]])) fm <- NULL
         n <- n+1
      }
   }  
   if(!is.null(fm)) if (lop$num_glf > 0) {  
      n <- 1
      while(!is.null(fm) & (n <= lop$num_glf)) {
         if(is.na(lop$glfList[[n]])) glfRes[[n]] <- tryCatch(testFactors(fm, pairwise=NULL,
            covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA) else
         glfRes[[n]] <- tryCatch(testFactors(fm, levels=lop$glfList[[n]],
            covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA)
         if(is.null(glfRes[[n]])) fm <- NULL #else chi_DF <- c(chi_DF, glfRes[[n]]$terms$`(Intercept)`$test[2,'Df'])
         n <- n+1
      }
   }
   if(!is.null(fm))  {
      print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
   } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')    
      cat('Possible reasons:\n\n')
      cat('0) Make sure that R package lmerTest has been installed. See the 3dLME\n')
      cat('help documentation for more details.\n\n')
      cat('1) Inappropriate model specification with options -model, or -qVars.\n\n')
      cat('2) In correct specifications for random effect with -ranEff.\n\n')
      cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n\n')
      cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
      cat('named Scanner in model specification and then listed as scanner in the table hader\n')
      cat('would cause grief for 3dLMEr.\n')
      errex.AFNI("Quitting due to model test failure...")
   }
   lop$nF      <- nrow(anova(fm))    # total number of F-stat
   nT      <- 2*lop$num_glt
   lop$NoBrick <- lop$nF + nT + lop$num_glf
}

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

options(contrasts = c("contr.sum", "contr.poly"))

   if(dimy==1 & dimz==1) Stat <- array(0, dim=c(dimx, lop$NoBrick)) else
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick))

   if (lop$nNodes==1) for (kk in 1:dimz) {
      if(dimy==1 & dimz==1) Stat <- aperm(apply(drop(comArr[,,kk,]), 1, runMeta, dataframe=lop$dataStr, tag=0), c(2,1)) else
      Stat[,,kk,] <- aperm(apply(comArr[,,kk,], c(1,2), runLME, dataframe=lop$dataStr, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }         

if (lop$nNodes>1) {
   pkgLoad('snow')
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterExport(cl, "lop", envir=environment())
   clusterEvalQ(cl, library(lmerTest)); clusterEvalQ(cl, library(phia))
   clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
   for (kk in 1:dimz) {
      Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME,
                  DM=lop$dataStr, tag=0), c(2,3,1)) 
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }
    stopCluster(cl)
}
# runLME(inData[30,30,30,], lop$model, lop$dataStr, lop$glt, nF, 0)

Top <- 100
Stat[is.nan(Stat)] <- 0
Stat[Stat > Top] <- Top  
Stat[Stat < (-Top)] <- -Top  

brickNames <- paste(rownames(anova(fm)), 'Chi-sq')
if(lop$num_glt > 0) for (n in 1:lop$num_glt) {
   brickNames <- append(brickNames, lop$gltLabel[n])
   brickNames <- append(brickNames, paste(lop$gltLabel[n], "Z"))
}
if(lop$num_glf > 0) for (n in 1:lop$num_glf) {
   brickNames <- append(brickNames, paste(lop$glfLabel[n], "Chi-sq"))
}

statsym <- NULL
if(lop$nF > 0) for (n in 1:lop$nF) 
   statsym <- c(statsym, list(list(sb=n-1, typ="fict", par=2)))
#   statsym <- c(statsym, list(list(sb=n-1, typ="fizt", par=NULL)))
if(lop$num_glt > 0) for (n in 1:lop$num_glt) 
   statsym <- c(statsym, list(list(sb=lop$nF+2*n-1, typ="fizt", par=NULL)))
if(lop$num_glf > 0) for (n in 1:lop$num_glf)
   statsym <- c(statsym, list(list(sb=lop$nF+lop$num_glt+n-1, typ="fict", par=2)))
#      statsym <- c(statsym, list(list(sb=lop$nF+lop$num_glt+n, typ="fizt", par=NULL)))

write.AFNI(lop$outFN, Stat[,,,1:lop$NoBrick], brickNames, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_short')

print(sprintf("Congratulations! You've got an output %s", lop$outFN))

##############  END  ############

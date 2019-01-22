#!/usr/bin/env AFNI_Batch_R


first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dLME'

# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin 3dLME Input functions ################################
#################################################################################

#The help function for 3dLME batch (AFNI-style script mode)
help.LME.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
          ================== Welcome to 3dLME ==================          
    AFNI Group Analysis Program with Linear Mixed-Effects Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 1.9.8, Jan 19, 2018
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/sscc/gangc/lme.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 3dLME is a group-analysis program that performs linear mixed-effects (LME) 
 modeling analysis. One simple criterion to decide whether 3dLME is appropriate
 is that each subject has to have two or more measurements at each spatial 
 location (except for a small portion of subjects with missing data). In other
 words, at least one within-subject (or repeated-measures) factor serves as
 explanatory variable.
 
 F-statistics for main effects and interactions are automatically included in 
 the output for all variables. In addition, Student t-tests for quantitative 
 variables are also in the output. In addition, general linear tests (GLTs) can 
 be requested via symbolic coding.
 
 If you want to cite the analysis approach, use the following:
 
 Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). Linear
 Mixed-Effects Modeling Approach to FMRI Group Analysis. NeuroImage 73:176-190.
 http://dx.doi.org/10.1016/j.neuroimage.2013.01.047
 
 Input files for 3dLME can be in AFNI, NIfTI, or surface (niml.dset) format.
 
 In addition to R installtion, the following two R packages need to be acquired
 in R first before running 3dLME: "nlme", "lme4" and "phia". In addition, the "snow"
 package is also needed if one wants to take advantage of parallel computing.
 To install these packages, run the following command at the terminal:

 rPkgsInstall -pkgs ALL

 Alternatively you may install them in R:
 
 install.packages("nlme")
 install.packages("lme4")
 install.packages("phia")
 install.packages("snow")
 
 More details about 3dLME can be found at 
 https://afni.nimh.nih.gov/sscc/gangc/LME.html

 Once the 3dLME command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called LME.txt, and execute it with the 
 following (assuming on tc shell),
 
 tcsh -x LME.txt &
 
 or,
 
 tcsh -x LME.txt > diary.txt &
 tcsh -x LME.txt |& tee diary.txt &

 The advantage of the latter command is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 
 Thanks to the R community, Henrik Singmann and Helios de Rosario for the strong
 technical support.'

   ex1 <- 
"
Example 1 --- one condition modeled with 8 basis functions (e.g., TENT or TENTzero)
for one group of 13 subjects:
--------------------------------
   3dLME -prefix myOutput -jobs   4               \\
         -model '0+Time'                        \\
         -qVars order                           \\
         -qVarCenters 0                         \\
         -ranEff '~1'                              \\
         -corStr 'order : AR1'                  \\
         -SS_type 3                             \\
         -num_glf 1                             \\
         -glfLabel 1 4TimePoints -glfCode 1 'Time : 1*Diff2 & 1*Diff3 & 1*Diff4 & 1*Diff5' \\
         -dataTable                             \\
         Subj   Time  order InputFile           \\
         c101   Diff0 0 testData/c101time0+tlrc \\
         c101   Diff1 1 testData/c101time1+tlrc \\
         c101   Diff2 2 testData/c101time2+tlrc \\
         c101   Diff3 3 testData/c101time3+tlrc \\
         c101   Diff4 4 testData/c101time4+tlrc \\
         c101   Diff5 5 testData/c101time5+tlrc \\
         c101   Diff6 6 testData/c101time6+tlrc \\
         c101   Diff7 7 testData/c101time7+tlrc \\
         c103   Diff0 0 testData/c103time0+tlrc \\
         c103   Diff1 1 testData/c103time1+tlrc \\
         ...
     \n"
      
   ex2 <-
"Example 2 --- one within-subject factor (conditions: House and Face), one
within-subject quantitative variable (reaction time, RT) and one between-
subjects covariate (age). RT values don't differ significantly between the
two conditions, and thus are centered via grand mean. Random effects are
intercept and RT effect whose correlation is estimated from the data.
-------------------------------------------------------------------------
   3dLME -prefix Example2 -jobs 24                                         \\
          -model  \"cond*RT+age\"                                            \\
          -qVars \"RT,age\"                                                  \\
          -qVarCenters \"105.35,34.7\"                                       \\
          -ranEff '~1+RT'                                                  \\
          -SS_type 3                                                       \\
          -num_glt 4                                                       \\
          -gltLabel 1 'House' -gltCode  1 'cond : 1*House'    \\
          -gltLabel 2 'Face-House' -gltCode  2 'cond : 1*Face -1*House'    \\
          -gltLabel 3 'House-AgeEff' -gltCode  3 'cond : 1*House age :'    \\
          -gltLabel 4 'House-Age2' -gltCode  4 'cond : 1*House age : 5.3'    \\
          -num_glf 1                                                       \\
          -glfLabel 1 'cond_age' -glfCode  1 'cond : 1*House & 1*Face age :'    \\
          -dataTable                                                       \\
          Subj  cond        RT   age        InputFile                      \\
          s1    House      124   35  s1+tlrc\'[House#0_Coef]\'               \\
          s2    House       97   51  s2+tlrc\'[House#0_Coef]\'               \\
          s3    House      107   25  s3+tlrc\'[House#0_Coef]\'               \\
          ... 
          s1    Face       110   35  s1+tlrc\'[Face#0_Coef]\'                \\
          s2    Face        95   51  s2+tlrc\'[Face#0_Coef]\'                \\
          s3    Face       120   25  s3+tlrc\'[Face#0_Coef]\'                \\
          ...                                   
   \n"

   ex3 <-   
"Example 3 --- one within-subject factor (conditions: positive, negative,
and neutral), and one between-subjects factors (groups: control and patients).
Effect estimates for a few subjects are available for only one or two
conditions. These subjects with missing data would have to be abandoned in
the traditional ANOVA approach. All subjects can be included with 3dLME, and
a random intercept is considered.
-------------------------------------------------------------------------
   3dLME -prefix Example3 -jobs 24                                     \\
          -model  \"cond*group\"                                         \\
          -ranEff '~1'                                                 \\
          -SS_type 3                                                   \\
          -num_glt 6                                                   \\
          -gltLabel 1 'pos-neu' -gltCode  1 'cond : 1*pos -1*neu'      \\
          -gltLabel 2 'neg' -gltCode  2 'cond : 1*neg '      \\
          -gltLabel 3 'pos+nue-neg' -gltCode  3 'cond : 1*pos +1*neu -1*neg'      \\
          -gltLabel 4 'pat_pos-neu' -gltCode  4 'cond : 1*pos -1*neu group : 1*pat'    \\
          -gltLabel 5 'pat_neg-neu' -gltCode  5 'cond : 1*neg -1*neu group : 1*pat'    \\
          -gltLabel 6 'pat_pos-neg' -gltCode  6 'cond : 1*pos -1*neg group : 1*pat'    \\
          -num_glf 1                                                   \\
          -glfLabel 1 'pos-neu' -glfCode  1 'Group : 1*ctr & 1*pat cond : 1*pos -1*neu & 1*pos -1*neg'      \\
          -dataTable                                                  \\
          Subj  cond      group        InputFile                      \\
          s1    pos        ctr    s1+tlrc\'[pos#0_Coef]\'               \\
          s1    neg        ctr    s1+tlrc\'[neg#0_Coef]\'               \\
          s1    neu        ctr    s1+tlrc\'[neu#0_Coef]\'               \\
          ... 
          s21   pos        pat   s21+tlrc\'[pos#0_Coef]\'               \\
          s21   neg        pat   s21+tlrc\'[neg#0_Coef]\'               \\
          s21   neu        pat   s21+tlrc\'[neu#0_Coef]\'               \\
          ...                                   
   \n"
   
   ex4 <-   
"Example 4 --- Computing ICC values for two within-subject factor (Cond:
positive, negative, and neutral; Scanner: one, and two) plus subjects (factor
Subj).
-------------------------------------------------------------------------
   3dLME -prefix Example4 -jobs 12                                      \\
          -model  \"1\"                                                   \\
          -ranEff 'Cond+Scanner+Subj'                                   \\
          -ICCb                                                         \\
          -dataTable                                                    \\
          Subj  Cond      Scanner        InputFile                      \\
          s1    pos        one    s1_1+tlrc\'[pos#0_Coef]\'               \\
          s1    neg        one    s1_1+tlrc\'[neg#0_Coef]\'               \\
          s1    neu        one    s1_1+tlrc\'[neu#0_Coef]\'               \\
          s1    pos        two    s1_2+tlrc\'[pos#0_Coef]\'               \\
          s1    neg        two    s1_2+tlrc\'[neg#0_Coef]\'               \\
          s1    neu        two    s1_2+tlrc\'[neu#0_Coef]\'               \\
          ... 
          s21   pos        two   s21_2+tlrc\'[pos#0_Coef]\'               \\
          s21   neg        two   s21_2+tlrc\'[neg#0_Coef]\'               \\
          s21   neu        two   s21_2+tlrc\'[neu#0_Coef]\'               \\
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
read.LME.opts.batch <- function (args=NULL, verb = 0) {
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

      '-cutoff' = apl(n = 1, d = 1, h = paste(
   "-cutoff threshold: Specify the cutoff value to obtain voxel-wise accuracy",
   "         in logistic regression analysis. Default is 0 (no accuracy will",
   "         be estimated).\n", sep = '\n'
                     ) ),
       
      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: Specify the terms of fixed effects for all explanatory,",
   "         including quantitative, variables. The expression FORMULA with more",
   "         than one variable has to be surrounded within (single or double)",
   "         quotes. Variable names in the formula should be consistent with",
   "         the ones used in the header of -dataTable. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. Subject should not occur in the model",
   "         specification here.\n", sep = '\n'
             ) ),

      '-ranEff' = apl(n=c(1,100), d=NA, h = paste(
   "-ranEff FORMULA: Specify the random effects. The simplest and most common",
   "         one is random intercept, \"~1\", meaning each subject deviates some",
   "         amount (called random effect) from the group average. \"~RT\" or \"~1+RT\"",
   "         means that each subject has a unique intercept as well as a slope,",
   "         and the correlation between the two random effects are estimated, not",
   "         assumed, from the data. \"~0+RT\" indicates that only a random effect",
   "         of slope is desired. Compound symmetry for a variance-covariance metric",
   "         across the levels of factor A can be specified through pdCompSymm(~0+A)",
   "         The list of random terms should be separated by space within (single or",
   "         double) quotes.",
   "         Notice: In the case of computing ICC values, list all the factors with",
   "         which the ICC is to be obtained. For example, with two factors \"Scanner\"",
   "         and \"Subj\", set it as -ranEff \"Scanner+Subj\". See Example 4 in the",
   "         the help.\n", sep = '\n'
                             ) ),

       '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dLME.dbg.AFNI.args in the current directory",
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
   "         3) Within-subject covariates are better modeled with 3dLME.\n",
             sep = '\n'
             ) ),

      '-vVars' = apl(n=c(1,100), d=NA, h = paste(
   "-vVars variable_list: Identify voxel-wise covariates with this option.",
   "         Currently one voxel-wise covariate is allowed only, but this",
   "         may change if demand occurs...",
   "         By default mean centering is performed voxel-wise across all",
   "         subjects. Alternatively centering can be specified through a",
   "         global value under -vVarsCenters. If the voxel-wise covariates",
   "         have already been centered, set the centers at 0 with -vVarsCenters.\n",
             sep = '\n'
             ) ),

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

     '-vVarCenters' = apl(n=1, d=NA, h = paste(
   "-vVarCenters VALUES: Specify centering values for voxel-wise covariates",
   "         identified under -vVars. Multiple centers are separated by ",
   "         commas (,) within (single or double) quotes. The order of the",
   "         values should match that of the quantitative variables in -qVars.",
   "         Default (absence of option -vVarsCetners) means centering on the",
   "         average of the variable across ALL subjects regardless their",
   "         grouping. If within-group centering is desirable, center the",
   "         variable YOURSELF first before the files are fed into -dataTable.\n",
             sep = '\n'
                     ) ),

     '-SS_type' = apl(n=1, d=3, h = paste(
   "-SS_type NUMBER: Specify the type for sums of squares in the F-statistics.",
   "         Two options are currently supported: sequential (1) and marginal (3).\n ",
             sep = '\n'
                     ) ),

     '-ICC' = apl(n=0, d=3, h = paste(
   "-ICC: This option allows 3dLME to compute voxel-wise intra-class correlation",
   "         for the variables specified through option -ranEff. See Example 4 in",
   "         in the help.\n ",
             sep = '\n'
                     ) ),

            '-ICCb' = apl(n=0, d=3, h = paste(
   "-ICCb: This option allows 3dLME to compute voxel-wise intra-class correlation",
   "         through a Bayesian approach with Gamma priors for the variables",
   "         specified through option -ranEff. The computation will take much",
   "         longer due the sophistication involved. However, the Bayesian method is",
   "         preferred to the old approach with -ICC for the typical FMRI data. R",
   "         package 'blme' is required for this option.\n ",
             sep = '\n'
                     ) ),

            '-logLik' = apl(n=0, d=3, h = paste(
   "-logLik: Add this option if the voxel-wise log likelihood is wanted in the output.",
   "         This option currently cannot be combined with -ICC, -ICCb, -LOGIT.\n",
             sep = '\n'
                     ) ),

            '-ML' = apl(n=0, d=3, h = paste(
   "-ML: Add this option if Maximum Likelihood is wanted instead of the default",
   "         method, Restricted Maximum Likelihood (REML).\n",
             sep = '\n'
                     ) ),   
       
     '-LOGIT' = apl(n=0, d=3, h = paste(
   "-LOGIT: This option allows 3dLME to perform voxel-wise logistic modeling.",
   "        Currently no random effects are allowed ('-ranEff NA'), but this",
   "        limitation can be removed later if demand occurs. The InputFile",
   "        column is expected to list subjects' responses in 0s and 1s. In",
   "        addition, one voxel-wise covariate is currently allowed. Each",
   "        regression coefficient (including the intercept) and its z-statistic",
   "        are saved in the output.\n ",
             sep = '\n'
                     ) ),
       
     '-corStr' = apl(n=c(1,100), d=NA, h = paste(
   "-corStr FORMULA: Specify the correlation structure of the residuals. For example,",
   "         when analyzing the effect estimates from multiple basis functions,",
   "         one may consider account for the temporal structure of residuals with",
   "         AR or ARMA.\n ",
             sep = '\n'
                     ) ),

     '-num_glt' = apl(n=1, d=0, h = paste(
   "-num_glt NUMBER: Specify the number of general linear t-tests (GLTs). A glt",
   "         is a linear combination of a factor levels. See details in ",
   "         -gltCode.\n", sep = '\n'
                     ) ),
                     
     '-gltLabel' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltLabel k label: Specify the label for the k-th general linear test",
   "         (GLT). A symbolic coding for the GLT is assumed to follow with",
   "         each -gltLabel.\n", sep = '\n'
                     ) ),

     '-gltCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltCode k CODING: Specify the k-th general linear test (GLT) through a",
   "         weighted combination among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the following",
   "         'Condition : 2*House -3*Face Emotion : 1*positive '",
   "         requests for a test of comparing 2 times House condition",
   "         with 3 times Face condition while Emotion is held at positive",
   "         valence.\n",
   "         NOTE:\n",
   "         1) The weights for a variable do not have to add up to 0.\n",   
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate unless the covariate",
   "         value is specified as, for example, 'Group : 1*Old Age : 2', where",
   "         the Old Group is tested at the Age of 2 above the center.\n",
   "         3) The effect for a quantitative variable can be specified with,",
   "         for example, 'Group : 1*Old Age : ', or ",
   "         'Group : 1*Old - 1*Young Age : '\n", 
   "         4) The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLT.\n",
   "         5) The appearance of a categorial variable has to be followed",
   "         by the linear combination of its levels. Only a quantitative",
   "         is allowed to have a dangling coding as seen in 'Age :'\n",         
             sep = '\n'
             ) ),

     '-num_glf' = apl(n=1, d=0, h = paste(
   "-num_glf NUMBER: Specify the number of general linear F-tests (GLFs). A glf",
   "         involves the union of two or more simple tests. See details in ",
   "         -glfCode.\n", sep = '\n'
                     ) ),
       
     '-glfLabel' = apl(n=c(1,1000), d=NA, h = paste(
   "-glfLabel k label: Specify the label for the k-th general linear F-test",
   "         (GLF). A symbolic coding for the GLF is assumed to follow with",
   "         each -glfLabel.\n", sep = '\n'
                     ) ),
       
     '-glfCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-glfCode k CODING: Specify the k-th general linear F-test (GLF) through a",
   "         weighted combination among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the coding",
   "         'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos' tests the main",
   "         effect of Condition at the positive Emotion. Similarly the coding",
   "         'Condition : 1*A -1*B & 1*A -1*C Emotion : 1*pos -1*neg' shows",
   "         the interaction between the three levels of Condition and the two.",
   "         levels of Emotion.\n",
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
   "         called table.txt. Do not forget to include a backslash at the end of",
   "         each row. In the script specify the data with '-dataTable @table.txt'.",
   "         This option is useful: (a) when there are many input files so that",
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
      errex.AFNI('Error parsing arguments. See 3dLME -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$cutoff <- 0
      lop$model  <- 1
      lop$maskFN <- NA

      lop$ranEff <- NA
      lop$qVars  <- NA   
      lop$vVars  <- NA
      lop$vQV    <- NA
      lop$qVarCenters <- NA
      lop$vVarCenters <- NA

      lop$corStr <- NA
      lop$SS_type <- 3
      lop$ICC     <- FALSE
      lop$ICCb    <- FALSE
      lop$logLik  <- FALSE
      lop$ML      <- FALSE
      lop$LOGIT   <- FALSE
      lop$num_glt <- 0
      lop$gltLabel <- NULL
      lop$gltCode  <- NULL
      lop$num_glf <- 0
      lop$glfLabel <- NULL
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
             cutoff = lop$cutoff <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             ranEff = lop$ranEff  <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             vVars  = lop$vVars <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             vVarCenters = lop$vVarCenters <- ops[[i]],
             corStr  = lop$corStr <- ops[[i]],
             SS_type = lop$SS_type <- ops[[i]],
             ICC     = lop$ICC     <- TRUE,
             ICCb    = lop$ICCb    <- TRUE,
             logLik  = lop$logLik  <- TRUE,
             ML      = lop$ML      <- TRUE,
             LOGIT   = lop$LOGIT   <- TRUE,
             num_glt = lop$num_glt <- ops[[i]],
             gltLabel = lop$gltLabel <- ops[[i]],
             gltCode  = lop$gltCode <- ops[[i]],
             num_glf = lop$num_glf <- ops[[i]],
             glfLabel = lop$glfLabel <- ops[[i]],
             glfCode  = lop$glfCode <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
             
             help = help.LME.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }


   return(lop)
}# end of read.LME.opts.batch

# construct a glt list for testInteraction in phia
# NEED to solve the problem when a quantitative variable is tested alone:
# with pairwise = NULL!!!                                                
gltConstr <- function(cStr, dataStr) {
   pos <- which(cStr==":")
   vars  <- cStr[pos-1]
   nvar <- length(vars)
   pos <- c(pos, length(cStr)+2) # add an artificial one for convenient usage below
   varsOK <- vars %in% colnames(dataStr)
   if(all(varsOK)) {
      gltList <- vector('list', nvar)      
      for(ii in seq_len(nvar)) {
         #browser()
	      lvl  <- levels(dataStr[,vars[ii]])
         gltList[[ii]] <- rep(0, length(lvl))
         sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)], strsplit, '\\*'))
	      lvlInv <- sepTerms[seq(2,length(sepTerms),2)]   # levels involved
	      lvlOK <- lvlInv %in% lvl
	      if(all(lvlOK)) {
	         sq <- match(lvlInv, lvl)
            gltList[[ii]][sq] <- as.numeric(sepTerms[seq(1,length(sepTerms),2)])
	      } else errex.AFNI(paste("Incorrect level coding in variable", vars[ii],
	         ": ", lvlInv[which(!lvlOK)], " \n   "))
      }
      names(gltList) <- vars
      return(gltList)
   } else errex.AFNI(paste("Incorrect variable name in GLT coding: ", vars[which(!varsOK)], " \n   "))
}


# test
# gltConstr(lop$gltCode[[1]], lop$dataStr)


# construct a list for contrast() in contrast: here only deals with factors???
# If a variable is *not* shown in the coding, it means that variable is collapsed!
#contraConstr <- function(cStr, dataStr, fixedVars, QV) {
#   pos <- which(cStr==":")  # colon positions: could be multiple colons
#   vars  <- cStr[pos-1]     # vector of vars in the glt/contrast: could be multiple variables; if not here, collapse
#   nvar <- length(fixedVars)     # number of variables involved in the model
#   pos <- c(pos, length(cStr)+2) # add an artificial one for convenient usage below
#   varsOK <- vars %in% colnames(dataStr)  # make sure the var names are correct
#   #browser()
#   if(all(varsOK)) {
#      contrList <- vector('list', 2)   # list to store the two conditions for contrast
#      for(ii in 1:2) {
#         contrList[[ii]] <- vector('list', nvar)
#         names(contrList[[ii]]) <- fixedVars
#      }
#      for(vv in fixedVars) {
#         #browser()
#	 if(vv %in% QV) for(ii in 1:2) contrList[[ii]][[vv]] <- 0 else {
#           lvl <- levels(dataStr[,vv])
#            if(vv %in% vars) {
#               #browser()
#               ii <- which(pos==which(cStr==vv)+1) # location of the closest colon
#               sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)], strsplit, '\\*'))
#               lvlInv <- sepTerms[seq(2,length(sepTerms),2)]   # levels involved
#               lvlOK <- lvlInv %in% lvl
#               #browser()
#	       if(all(lvlOK)) {
#                  if(length(lvlInv)==1) lvlInv <- c(lvlInv, lvlInv) # artificially add one so that it works 2 lines later
#                  sq <- match(as.numeric(sepTerms[c(1,3)]), c(1, -1))
#                  for(ii in 1:2) contrList[[ii]][[vv]] <- lvlInv[ii]
#	            } else errex.AFNI(paste("Incorrect level coding in variable", vars[ii],
#	               ": ", lvlInv[which(!lvlOK)], " \n   "))
#            } else for(ii in 1:2) contrList[[ii]][[vv]] <- lvl
#         } #if(vv %in% QV)        
#      } #for(vv in fixedVars)
#   } else errex.AFNI(paste("Incorrect variable name in GLT coding: ", vars[which(!varsOK)], " \n   "))
#   return(contrList) 
#}

# test
# contraConstr(lop$gltCode[[1]], lop$dataStr, fixedVars, lop$QV)

# construct a glf list for testFactors in phia
# NEED to solve the problem when a quantitative variable is tested alone:
# with pairwise = NULL!!!                                                
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
         #if(nc==0) { # one row: no & involved
         #   glfList[[ii]] <- rep(0, length(lvl))
         #   sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)], strsplit, '\\*'))
	 #   lvlInv <- sepTerms[seq(2,length(sepTerms),2)]   # levels involved
	 #   lvlOK <- lvlInv %in% lvl
	 #   if(all(lvlOK)) {
	 #      sq <- match(lvlInv, lvl)
         #      glfList[[ii]][sq] <- as.numeric(sepTerms[seq(1,length(sepTerms),2)])
	 #   } else errex.AFNI(paste("Incorrect level coding in variable", vars[ii],
	 #        ": ", lvlInv[which(!lvlOK)], " \n   "))
         #} else {  # more than one column: at least one &
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


#Change options list to 3dLME variable list 
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
   if(!is.na(lop$vVars[1])) lop$vQV <- strsplit(lop$vVars, '\\,')[[1]]

   if(!is.null(lop$gltLabel)) {
      sq <- as.numeric(unlist(lapply(lop$gltLabel, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glt)))) {
         lop$gltLabel <- unlist(lapply(lop$gltLabel, '[', 2))
         lop$gltLabel[sq] <- lop$gltLabel
      } else errex.AFNI(c('The number of -gltLabel is not consistent with that \n',
         'specified by -num_glt ',  lop$num_glt))
   }

  #if(!is.null(lop$gltCode)) {
  if(length(lop$gltCode)!=0) {
      sq <- as.numeric(unlist(lapply(lop$gltCode, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glt)))) {
         lop$gltCode <- lapply(lop$gltCode, '[',-1)
         lop$gltCode[sq] <- lop$gltCode
      } else errex.AFNI(c('The number of -gltCode is not consistent with that \n',
         'specified by -num_glt ',  lop$num_glt))
   }         

   if(!is.null(lop$glfLabel)) {
      sq <- as.numeric(unlist(lapply(lop$glfLabel, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glf)))) {
         lop$glfLabel <- unlist(lapply(lop$glfLabel, '[', 2))
         lop$glfLabel[sq] <- lop$glfLabel
      } else errex.AFNI(c('The number of -glfLabel is not consistent with that \n',
         'specified by -num_glf ',  lop$num_glf))
   }

  #if(!is.null(lop$glfCode)) {
  if(length(lop$glfCode)!=0) {
      sq <- as.numeric(unlist(lapply(lop$glfCode, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glf)))) {
         lop$glfCode <- lapply(lop$glfCode, '[',-1)
         lop$glfCode[sq] <- lop$glfCode
      } else errex.AFNI(c('The number of -glfCode is not consistent with that \n',
         'specified by -num_glf ',  lop$num_glf))
   }         

   len <- length(lop$dataTable)
   wd <- which(lop$dataTable == "InputFile")
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
      if(!is.na(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
   }

   # number of fixed-effects variables involved in the model
   # parseStr <- function(input, sep) unlist(strsplit(input, sep))
   #fixedVars <- unique(parseStr(parseStr(parseStr(lop$model, "\\:"), "\\*"), "\\+"))
   #nFix <- length(fixedVars)

   # contrcuct the glt's for phia, which is currently not so suitable for lme
   #if (lop$num_glt > 0) {
   #   lop$gltList    <- vector('list', lop$num_glt)
   #   lop$slpList    <- vector('list', lop$num_glt)
   #   for (n in 1:lop$num_glt) {
   #      if(!is.na(lop$qVars) & any(lop$QV %in% lop$gltCode[[n]])) {
   #         QVpos <- which(lop$gltCode[[n]] %in% lop$QV)
   #         lop$gltList[[n]]   <- gltConstr(lop$gltCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
   #         lop$slpList[[n]] <- lop$gltCode[[n]][QVpos]   
   #      } else lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr)
   #   }
   #}
   
   # constrcuct pairwise comparisons for function contrast
   # now assume quantitative variables are always set to 0 for contrast testing
   # may change to any value later
   #if (lop$num_glt > 0) {
   #   lop$gltList <- vector('list', lop$num_glt)  # list used by contrast()
   #   for (n in 1:lop$num_glt) lop$gltContrList[[n]] <- contraConstr(lop$gltCode[[n]], lop$dataStr, fixedVars, lop$QV)
   #}

   # set the covariate default values at their centers
   #lop$covVal <- NULL
   #if(length(lop$QV)>0) {
   #   lop$covVal <- rep(0, length(lop$QV))
   #   names(lop$covVal) <- lop$QV
   #}

   #lop$vQV <- NA; lop$vVars <- NA # no voxelwise quantitative variable for now

   if (lop$num_glt > 0) {
      glt <- gl_Constr(lop$num_glt, lop$gltCode, lop)
      lop$gltList     <- glt[[1]]
      lop$slpList     <- glt[[2]]
      lop$covValList  <- glt[[3]]
   }
   if (lop$num_glf > 0) {
      glf <- gl_Constr(lop$num_glf, lop$glfCode, lop)
      lop$glfList     <- glf[[1]]
      lop$slpListF    <- glf[[2]]
      lop$covValListF <- glf[[3]]
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
################# LME Computation functions ##############################
#################################################################################

# used only in read.LME.opts.from.file
scanLine <- function(file, lnNo=1, marker="\\:")
   unlist(strsplit(unlist(scan(file, what= list(""), skip=lnNo-1, 
   strip.white=TRUE, nline=1)), marker))[2]


# heavy computation with voxel-wise analysis
runLME <- function(inData, dataframe, ModelForm) {
   #if(is.null(lop$resid)) Stat <- rep(0, lop$NoBrick) else Stat <- rep(0, lop$NoBrick+length(inData))
   Stat <- rep(0, lop$NoBrick)
   if(!is.null(lop$resid)) resid <- rep(0, length(inData))
   #browser()
   if(any(!is.na(lop$vQV))) {  # voxel-wise centering for voxel-wise covariate
      dataframe <- assVV2(dataframe, lop$vQV, inData[(length(inData)/2+1):length(inData)], all(is.na(lop$vVarCenters)))
   }
   if (!all(abs(inData) < 10e-8)) {        
      dataframe$Beta<-inData[1:nrow(dataframe)]
      fm <- NULL
      if(lop$ML) {
         if(!is.na(lop$corStr[1]))
            try(fm <- lme(ModelForm, random = lop$ranEffList, dataframe, correlation=corAR1(0.3, form=lop$corStrList), method='ML'), silent=TRUE)
         # case of basis functions
         if(is.null(fm)) { # case of basis functions fails or other cases
            try(fm <- lme(ModelForm, random = lop$ranEffList, dataframe, method='ML'), silent=TRUE)
           if(is.null(fm)) try(fm <- lme(ModelForm, random = ~1|Subj, dataframe), silent=TRUE)
         }
      } else {
         if(!is.na(lop$corStr[1]))
            try(fm <- lme(ModelForm, random = lop$ranEffList, dataframe, correlation=corAR1(0.3, form=lop$corStrList)), silent=TRUE)
         # case of basis functions
         if(is.null(fm)) { # case of basis functions fails or other cases
            try(fm <- lme(ModelForm, random = lop$ranEffList, dataframe), silent=TRUE)
            if(is.null(fm)) try(fm <- lme(ModelForm, random = ~1|Subj, dataframe), silent=TRUE)
         }
      } 
      #if(is.null(fm) | class(fm$apVar)[1] == "character") {  # contrast cares about fm$apVar
      if(is.null(fm)) {  # phia doesn't care about fm$apVar
         fm <- NULL
         try(fm <- gls(ModelForm, dataframe), silent=TRUE)
      }

      if(!is.null(fm)) {      
         Stat[1:lop$nF] <- anova(fm, type=lop$SStype)$F[lop$Fseq] # F-stat		
         if(!is.na(lop$corStr[1])) { # basis functions
	    Stat[lop$nF+2*0.5:lop$nBasis] <- unname(summary(fm)$tTable[, "Value"])
            # unname(fm$coefficients$fixed) only works for lme, not for gls!!!
            # but unname(summary(fm)$tTable[, "Value"]) is immune to the type of models, lme or gls!
 	    Stat[lop$nF+2*1:lop$nBasis] <- unname(summary(fm)$tTable[,"t-value"])
         }
         if(lop$num_glt > 0) for(ii in 1:lop$num_glt) { # assuming no glts for basis functions
	    #con <- NULL
            #try(con <- contrast(fm, lop$gltList[[n]][[1]], lop$gltList[[n]][[2]], type="average"),silent=TRUE) 
	    #if(!is.null(con)) Stat[(lop$nF+2*n-1):(lop$nF+2*n)] <- c(con$Contrast, con$testStat)
            glt <- NULL
            if(is.na(lop$gltList[[ii]])) glt <- tryCatch(testInteractions(fm, pairwise=NULL, slope=lop$slpList[[ii]], 
               covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL) else
            glt <- tryCatch(testInteractions(fm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
               covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL)
            
            #glt <- testInteractions(fm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], adjustment="none")
            if(!is.null(glt)) {
               Stat[lop$nF[1]+2*lop$nBasis+2*ii-1] <- glt[1,1]
	       Stat[lop$nF[1]+2*lop$nBasis+2*ii]   <- sign(glt[1,1])*qnorm(glt[1,4]/2, lower.tail = F)  # convert chisq to Z
            }
         }
         # GLF part below
         if(lop$num_glf>=1) for(ii in 1:lop$num_glf) { # this first part below may need fixes!
            if(all(is.na(lop$glfList[[ii]]))) glfRes <- tryCatch(testFactors(fm, pairwise=NULL, slope=lop$slpListF[[ii]], 
               covariates=lop$covValListF[[ii]], adjustment="none")$terms$`(Intercept)`$test, error=function(e) NULL) else
            glfRes <- tryCatch(testFactors(fm, levels=lop$glfList[[ii]], slope=lop$slpListF[[ii]], 
               covariates=lop$covValListF[[ii]], adjustment="none")$terms$`(Intercept)`$test, error=function(e) NULL)
            if(!is.null(glfRes)) Stat[lop$nF[1]+2*lop$num_glt+2*lop$nBasis+ii] <- glfRes[2,2] # chi-sq value
            #Stat[lop$nF[1]+2*lop$num_glt+ii] <- qnorm(glfRes[2,3]/2, lower.tail = F)  # convert chisq to Z
         }
         if(lop$logLik) Stat[lop$NoBrick] <- fm$logLik
         resid <- unname(residuals(fm))
      }
   }
   if(!is.null(lop$resid)) Stat <- c(Stat, resid)
   return(Stat)	
}
# test runLME(inData[20,20,20,], dataframe=lop$dataStr, ModelForm=ModelForm)      

#runREML <- function(myData, fm, nBrk, tag) {
#   #browser()
#   myStat<-vector(mode="numeric", length= nBrk)
#   if(!all(myData == 0)) {     
#      try(fmAOV<-refit(fm, myData), tag<-1)   
#      if(tag != 1) {    
#         for(ii in 1:(nBrk-1)) myStat[ii] <- VarCorr(fmAOV)[[ii]][1]  # factor variances
#         myStat[nBrk] <- attr(VarCorr(fmAOV), "sc")^2  # residual variance
#         myStat <- myStat/sum(myStat)
#      }
#   }
#   return(myStat)
#}

runREML <- function(myData, ModelForm, dataframe, nBrk, tag) {
   #browser()
   myStat<-vector(mode="numeric", length= nBrk)
   if(!all(myData == 0)) {     
      dataframe$Beta<-myData
      try(fmAOV<-lmer(ModelForm, data=dataframe), tag<-1)
      if(tag != 1) {    
         for(ii in 1:(nBrk-1)) myStat[ii] <- VarCorr(fmAOV)[[ii]][1]  # factor variances
         myStat[nBrk] <- attr(VarCorr(fmAOV), "sc")^2  # residual variance
         myStat <- myStat/sum(myStat)
      }
   }
   return(myStat)
}

# Bayesian for ICC with gamma priors
runREMLb <- function(myData, ModelForm, dataframe, nBrk, tag) {
   #browser()
   myStat<-vector(mode="numeric", length= nBrk)
   if(!all(myData == 0)) {     
      dataframe$Beta<-myData
      #try(fmAOV<-blmer(ModelForm, data=dataframe, cov.prior=gamma), tag<-1)
      try(fmAOV<-blmer(ModelForm, data=dataframe, cov.prior=gamma(shape = 2, rate = 0.5, posterior.scale = 'sd')), tag<-1)  
      if(tag != 1) {    
         for(ii in 1:(nBrk-1)) myStat[ii] <- VarCorr(fmAOV)[[ii]][1]  # factor variances
         myStat[nBrk] <- attr(VarCorr(fmAOV), "sc")^2  # residual variance
         myStat <- myStat/sum(myStat)
      }
   }
   return(myStat)
}

# for logistic modeling only
assVV <- function(DF, vQV, value, c) {
      # centering - c: center; value: voxel-wise value; vQV: voxel-wise variable name; DF: dataframe
      if(is.na(c)) cvalue <- scale(value, center=TRUE, scale=F) else
         cvalue <- scale(value, center=c, scale=F)
      for(ii in 1:length(unique(DF[,vQV]))) {                                   
         # ii-th subject's rows
         fn <- paste(vQV, '_fn', sep='')
         sr <- which(unique(DF[,fn])[ii] == DF[,fn])
         #browser()
         DF[sr, vQV] <- rep(cvalue[ii], length(sr))
     }
     DF[, vQV] <- as.numeric(DF[, vQV])
     return(DF)
}

# for LME only
assVV2 <- function(DF, vQV, value, c) {
      # centering - c: center; value: voxel-wise value; vQV: voxel-wise variable name; DF: dataframe
   #browser()
   if(is.na(c)) DF[, vQV] <- scale(value, center=TRUE, scale=F) else
       DF[, vQV] <- scale(value, center=c, scale=F)
   DF[, vQV] <- as.numeric(DF[, vQV])
   return(DF)
}

runGLM <- function(inData, dataframe, ModelForm) {  
   Stat   <- rep(0, lop$NoBrick+2*(nlevels(lop$dataStr$Subj) + 1))
   if (!all(abs(inData) < 1e-8)) {
      dataframe$Beta<-inData[1:lop$nVVars]
      if(any(!is.na(lop$vQV))) {
         dataframe <- assVV(dataframe, lop$vQV, inData, all(is.na(lop$vVarCenters)))
      }
      #browser()
      dataframe$Beta <- lop$dataStr$InputFile
      fm <- NULL
      try(fm <- glm(ModelForm, family=binomial(logit), data=dataframe), silent=TRUE)
      if(!is.null(fm)) {
         Stat[seq(1,lop$NoBrick-(lop$cutoff>0),2)] <- summary(fm)$coefficients[,'Estimate']
         Stat[seq(2,lop$NoBrick-(lop$cutoff>0),2)] <- summary(fm)$coefficients[,'z value']
         pred <- prediction(fm$fitted.values, lop$dataStr$InputFile) # from ROCR
         acc <- pred@tp[[1]]+pred@tn[[1]]
         acc <- acc/(acc+pred@fp[[1]]+pred@fn[[1]]) # accuracy
         cutoff <- c(1, pred@cutoffs[[1]][-1])  # the 1st one is Inf, replaced by 1 here

         # accuracy associated the user-specified cutoff
         Stat[lop$NoBrick] <- acc[which.min(abs(cutoff-lop$cutoff))]

         # the 2 values below are for max acc and the corresponding cutoff
         #accMax <- max(acc)
         #Stat[lop$NoBrick-1] <- cutoff[which(abs(acc-accMax) < 1e-8)][1]  # just take the first cutoff if ambiguous
         #Stat[lop$NoBrick]   <- accMax
      
         Stat[(lop$NoBrick+1):(lop$NoBrick+nlevels(lop$dataStr$Subj) + 1)]  <- cutoff
         Stat[(lop$NoBrick+nlevels(lop$dataStr$Subj) + 2):(lop$NoBrick+2*nlevels(lop$dataStr$Subj) + 2)] <- acc
      }
   }
   return(Stat)
}
# test runGLM(inData[20,20,20,], dataframe=lop$dataStr, ModelForm=ModelForm)      

runGLM2 <- function(inData, dataframe, ModelForm, nBoot) {  
   Stat   <- rep(0, lop$NoBrick+2*(nlevels(lop$dataStr$Subj) + 1))
   if (!all(abs(inData) < 1e-8)) {
      dataframe$Beta<-inData[1:lop$nVVars]
      if(any(!is.na(lop$vQV))) {
         dataframe <- assVV(dataframe, lop$vQV, inData, all(is.na(lop$vVarCenters)))
      }
      #browser()
      dataframe$Beta <- lop$dataStr$InputFile
      fm <- NULL
      try(fm <- glm(ModelForm, family=binomial(logit), data=dataframe), silent=TRUE)
      if(!is.null(fm)) {
         Stat[seq(1,lop$NoBrick-(lop$cutoff>0)-1,2)] <- summary(fm)$coefficients[,'Estimate']
         Stat[seq(2,lop$NoBrick-(lop$cutoff>0)-1,2)] <- summary(fm)$coefficients[,'z value']
         pred <- prediction(fm$fitted.values, lop$dataStr$InputFile) # from ROCR
         acc <- pred@tp[[1]]+pred@tn[[1]]
         acc <- acc/(acc+pred@fp[[1]]+pred@fn[[1]]) # accuracy
         cutoff <- c(1, pred@cutoffs[[1]][-1])  # the 1st one is Inf, replaced by 1 here

         # accuracy associated the user-specified cutoff
         #Stat[lop$NoBrick-1] <- acc[which.min(abs(cutoff-lop$cutoff))]
        
         acc0 <- rep(0, lop$nB)
         nn <- 0
         for(ii in 1: lop$nB) {
            fm0 <- NULL
            try(fm0 <- glm(ModelForm, family=binomial(logit), data=dataframe[nBoot[,ii],]), silent=TRUE)
            if(!is.null(fm0)) {
               nn<-nn+1
               pred0 <- prediction(fm0$fitted.values, dataframe[nBoot[,ii],]$Beta)
               acc1 <- pred0@tp[[1]]+pred0@tn[[1]]
               acc1 <- acc1/(acc1+pred0@fp[[1]]+pred0@fn[[1]]) # accuracy
               cutoff1 <- c(1, pred0@cutoffs[[1]][-1])  # the 1st one is Inf, replaced by 1 here
               acc0[ii] <- acc1[which.min(abs(cutoff1-lop$cutoff))]
            }
         }
         Stat[lop$NoBrick-1] <- mean(acc0)
         Stat[lop$NoBrick] <- (Stat[lop$NoBrick-1]-0.5)/(sd(acc0)/sqrt(nn))
         # the 2 values below are for max acc and the corresponding cutoff
         #accMax <- max(acc)
         #Stat[lop$NoBrick-1] <- cutoff[which(abs(acc-accMax) < 1e-8)][1]  # just take the first cutoff if ambiguous
         #Stat[lop$NoBrick]   <- accMax
      
         Stat[(lop$NoBrick+1):(lop$NoBrick+nlevels(lop$dataStr$Subj) + 1)]  <- cutoff
         Stat[(lop$NoBrick+nlevels(lop$dataStr$Subj) + 2):(lop$NoBrick+2*nlevels(lop$dataStr$Subj) + 2)] <- acc
      }
   } # if (!all(abs(inData) < 1e-8))
   return(Stat)
}

# test runGLM2(inData[20,20,20,], dataframe=lop$dataStr, ModelForm=ModelForm, nBoot=nBoot)      

runGLM0 <- function(inData, dataframe, ModelForm, nBoot) {  
   Stat   <- rep(0, lop$NoBrick+2*(nlevels(lop$dataStr$Subj) + 1))
   if (!all(abs(inData) < 1e-8)) {
      dataframe$Beta<-inData[1:lop$nVVars]
      if(any(!is.na(lop$vQV))) {
         dataframe <- assVV(dataframe, lop$vQV, inData, all(is.na(lop$vVarCenters)))
      }
      #browser()
      dataframe$Beta <- lop$dataStr$InputFile
      fm <- NULL
      try(fm <- glm(ModelForm, family=binomial(logit), data=dataframe), silent=TRUE)
      if(!is.null(fm)) {
         Stat[seq(1,lop$NoBrick-(lop$cutoff>0)-1,2)] <- summary(fm)$coefficients[,'Estimate']
         Stat[seq(2,lop$NoBrick-(lop$cutoff>0)-1,2)] <- summary(fm)$coefficients[,'z value']
         pred <- prediction(fm$fitted.values, lop$dataStr$InputFile) # from ROCR
         acc <- pred@tp[[1]]+pred@tn[[1]]
         acc <- acc/(acc+pred@fp[[1]]+pred@fn[[1]]) # accuracy
         cutoff <- c(1, pred@cutoffs[[1]][-1])  # the 1st one is Inf, replaced by 1 here

         # accuracy associated the user-specified cutoff
         Stat[lop$NoBrick-1] <- acc[which.min(abs(cutoff-lop$cutoff))]
         nB <- 30
         acc0 <- rep(0, nB)
         nn <- 0
         for(ii in 1: nB) {
            fm0 <- NULL
            try(fm0 <- glm(ModelForm, family=binomial(logit), data=dataframe[nBoot[,ii],]), silent=TRUE)
            if(!is.null(fm0)) {
               nn<-nn+1
               pred0 <- prediction(fm0$fitted.values, dataframe[nBoot[,ii],]$Beta)
               acc1 <- pred0@tp[[1]]+pred0@tn[[1]]
               acc1 <- acc1/(acc1+pred0@fp[[1]]+pred0@fn[[1]]) # accuracy
               cutoff1 <- c(1, pred0@cutoffs[[1]][-1])  # the 1st one is Inf, replaced by 1 here
               acc0[ii] <- acc1[which.min(abs(cutoff1-lop$cutoff))]
            }
         }
         Stat[lop$NoBrick] <- (Stat[lop$NoBrick-1]-0.5)/(sd(acc0)/sqrt(nn))
         # the 2 values below are for max acc and the corresponding cutoff
         #accMax <- max(acc)
         #Stat[lop$NoBrick-1] <- cutoff[which(abs(acc-accMax) < 1e-8)][1]  # just take the first cutoff if ambiguous
         #Stat[lop$NoBrick]   <- accMax
      
         Stat[(lop$NoBrick+1):(lop$NoBrick+nlevels(lop$dataStr$Subj) + 1)]  <- cutoff
         Stat[(lop$NoBrick+nlevels(lop$dataStr$Subj) + 2):(lop$NoBrick+2*nlevels(lop$dataStr$Subj) + 2)] <- acc
      }
   }
   return(Stat)
}

# test runGLM2(inData[20,20,20,], dataframe=lop$dataStr, ModelForm=ModelForm, nBoot=nBoot)      


#################################################################################
########################## Read information from a file #########################
#################################################################################

#A function to parse all user input from a file

read.LME.opts.from.file <- function (modFile='model.txt', verb = 0) {
   lop <- list()  #List to hold all options from user input
   lop$verb <- verb
   lop$iometh <- 'clib'
   
   # Line 1: data type - volume or surface
   #datatype <- scanLine(modFile, lnNo=1)
   
   #  Line 2: Output filename
   lop$outFN <-scanLine(modFile, lnNo=2)
   lop$outFN <- paste(lop$outFN, "+tlrc", sep="")

   # check if the filename exists already
   if(any(file.exists(paste(lop$outFN,'.BRIK', sep=''), paste(lop$outFN,'.HEAD', sep=''))))
      errex.AFNI("Output filename already exists! Please rename it...")

   # Line 3: MASK
   lop$maskFN <- scanLine(modFile, lnNo=3)

   # Line 4: #jobs
   lop$nNodes<- as.integer(scanLine(modFile, lnNo=4))

   # Line 5: model formula
   lop$model <- scanLine(modFile, lnNo=5)

   # Line 6: within-subject variables
   lop$wsVars <- scanLine(modFile, lnNo=6)

   # Line 7: covariates lop$QV is NA if no covariates
   lop$qVars <- unlist(strsplit(scanLine(modFile, lnNo=7), "\\*"))
   lop$QV <- strsplit(lop$qVars, '\\+')[[1]]

   # Line 8: covariates center value: NA means mean
   lop$qVarCenters <- unlist(strsplit(scanLine(modFile, lnNo=8), "\\*"))
   #lop$qVarCenters <- as.numeric(strsplit(lop$qVarCenters, '\\,')[[1]])

   # header position (hp) defined by column name InputFile
   hp <- grep("InputFile", readLines(modFile)) 
   lop$dataStr <- read.table(modFile, skip=hp[1]-1, header=TRUE)

   # number of contrasts (pair-wise only right now)
   lop$num_glt   <- (hp-12)%/%2

   if (lop$num_glt > 0) {
      lop$gltLabel <- array(data=NA, dim=lop$num_glt)
      clist      <- vector('list', lop$num_glt)
      lop$gltList    <- vector('list', lop$num_glt)
      lop$slpList    <- vector('list', lop$num_glt)
      for (n in 1:lop$num_glt) {
         lop$gltLabel[n] <- paste(unlist(scan(file=modFile, what= list(""), skip=10+2*n-1, 
            strip.white=TRUE, nline=1)), collapse="")
         clist[[n]] <- scan(file=modFile, what= list(""), skip=10+2*n, nline=1)[[1]]
         if(any(lop$QV %in% clist[[n]])) {
            QVpos <- which(clist[[n]] %in% lop$QV)
            lop$gltList[[n]]   <- gltConstr(clist[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
            lop$slpList[[n]] <- clist[[n]][QVpos]   
         } else lop$gltList[[n]] <- gltConstr(clist[[n]], lop$dataStr)
      }
   }
   #browser()
   return(lop)
} # end of read.LME.from.file


#################################################################################
########################## Begin LME main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dLME.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
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
lop$gltCode <- lapply(lop$gltCode, function(ss) unlist(strsplit(ss, split="(?=:)", perl=TRUE)))                                             

if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])

if(lop$ICC) pkgLoad('lme4') else if(lop$LOGIT) pkgLoad('ROCR') else if(lop$ICCb) pkgLoad('blme') else pkgLoad(c('nlme', 'phia'))
# effect coding leads to the same type III as SAS   
options(contrasts = c("contr.sum", "contr.poly"))
   
#comArgs <- commandArgs()

#if(length(comArgs)<6) modFile <- "model.txt" else
#modFile <- comArgs[6]
#paste(commandArgs())


# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
#if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model)) else
   ModelForm <- paste("Beta ~", lop$model)
   

# Line 12 and the next few: pair-wise contrasts

# Maybe not list for these two, or yes?
lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)

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
cat(lop$num_glt, 'post hoc tests\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')
if(lop$ICC | lop$ICCb) showTab <- as.formula(paste('~', gsub("\\*", "+", lop$ranEff))) else {
   showTab <- as.formula(paste('~', gsub("\\:", "+", gsub("\\*", "+", lop$model))))
   if(!is.na(lop$qVars)) for(ii in rev(levels(ordered(lop$QV)))) # reversing the oder of those quantitative covariates so that
      showTab <- gsub(paste('\\*', ii, sep=''), '', gsub(paste('\\+', ii, sep=''), '', showTab))
   if(!is.na(lop$vVars)) showTab <- sub(lop$vQV, "", showTab)
   #showTab <- as.formula(gsub("\\*", "+", showTab))  # in case there are still some *'s like between-subjects factors
}
#print(xtabs(showTab, data=lop$dataStr))                                           

if(!lop$ICC | lop$ICCb) {
   cat('\nTabulation of subjects against all categorical variables')
   all_vars <- names(lop$dataStr)
   for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
      cat('\n~~~~~~~~~~~~~~')
      cat('\nSubj vs ', var, ':\n', sep='')
      print(table(lop$dataStr$Subj, lop$dataStr[,var]))
   }
}

cat('***** End of data structure information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')


# Assume the last column is input files
#FileCol <- length(colnames(lop$dataStr))
FileCol <- dim(lop$dataStr)[2]

#Number of input files
NoFile <- dim(lop$dataStr[1])[1]

# Repeated-measures (use lme) or not (use lm)
#if (length(unique(lop$dataStr$Subj)) != length(lop$dataStr$Subj)) RM <- TRUE else RM <- FALSE

cat('Reading input files now...\n\n')

# Read in the 1st input file so that we have the dimension information
if(lop$LOGIT) inData <- read.AFNI(lop$dataStr[,lop$vQV[1]][1], verb=lop$verb, meth=lop$iometh, forcedset = TRUE) else
   inData <- read.AFNI(lop$dataStr[1, FileCol], verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData

# ww <- inData$NI_head
#myHist <- inData$header$HISTORY_NOTE; myOrig <- inData$origin; myDelta <- inData$delta
# Read in all input files
if(lop$LOGIT)
   inData <- unlist(lapply(lapply(lop$dataStr[,lop$vQV[1]], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1)) else
   inData <- unlist(lapply(lapply(lop$dataStr[,FileCol], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
#dim(inData) <- c(dimx, dimy, dimz, NoFile)
   tryCatch(dim(inData) <- c(dimx, dimy, dimz, NoFile), error=function(e)
   errex.AFNI(c("At least one of the input files has different dimensions!\n",
   "Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory where\n",
   "the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files: Done!\n\n')

if (!is.na(lop$maskFN)) {
   Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk[,,,1]
   inData <- array(apply(inData, 4, function(x) x*(abs(Mask)>tolL)), dim=c(dimx,dimy,dimz,NoFile))
}
   
# voxel-wise covariate files
if(!lop$LOGIT & !is.na(lop$vQV)) {
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

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or the GLT coding\n')
cat('is likely inappropriate.\n\n')

xinit <- dimx%/%3
if(dimy==1) yinit <- 1 else yinit <- dimy%/%2
if(dimz==1) zinit <- 1 else zinit <- dimz%/%2

ii <- xinit; jj <- yinit; kk <- zinit

fm<-NULL
gltRes <- vector('list', lop$num_glt)
glfRes <- vector('list', lop$num_glf)
chi_DF <- NULL
# need to expand this later
#ranEff <- list(Subj=as.formula(c('~',lop$ranEff)))
#nRanEff <- length(lop$ranEff)
#browser()
if(lop$ICC) {  # ICC part
   lop$ranEff <- unlist(strsplit(lop$ranEff, split="[+]"))
   nRanEff <- length(lop$ranEff)
   for(nn in 1:nRanEff) ModelForm <- paste(ModelForm,"+(1|",lop$ranEff[nn],")")
   ModelForm <- as.formula(ModelForm)
   while(is.null(fm)) {
      fm<-NULL
      lop$dataStr$Beta<-inData[ii, jj, kk,]
      options(warn=-1)
      try(fm <- lmer(ModelForm, data=lop$dataStr), silent=TRUE)
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
         cat('would cause grief for 3dLME.\n')
         errex.AFNI("Quitting due to model test failure...")
      }
   }
   lop$NoBrick <- nRanEff+1
} else if(lop$ICCb) {  # Bayesian ICC
   lop$ranEff <- unlist(strsplit(lop$ranEff, split="[+]"))
   nRanEff <- length(lop$ranEff)
   for(nn in 1:nRanEff) ModelForm <- paste(ModelForm,"+(1|",lop$ranEff[nn],")")
   ModelForm <- as.formula(ModelForm)
   while(is.null(fm)) {
      fm<-NULL
      lop$dataStr$Beta<-inData[ii, jj, kk,]
      options(warn=-1)
      try(fm <- blmer(ModelForm, data=lop$dataStr), silent=TRUE)
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
         cat('would cause grief for 3dLME.\n')
         errex.AFNI("Quitting due to model test failure...")
      }
   }
   lop$NoBrick <- nRanEff+1
} else if(lop$LOGIT) {  # logistic regression part    
      fm <- NULL
      lop$dataStr$InputFile <- as.numeric(lop$dataStr$InputFile)
      lop$dataStr$Beta <- lop$dataStr$InputFile
      if(any(!is.na(lop$vQV))) {
         lop$dataStr <- assVV(lop$dataStr, lop$vQV, inData[ii,jj,kk,], all(is.na(lop$vVarCenters)))
      }     
      try(fm <- glm(ModelForm, family=binomial(logit), data=lop$dataStr), silent=TRUE)
      if(!is.null(fm))  {
         print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
         lop$NoBrick <- 2*dim(summary(fm)$coefficients)[1] + (lop$cutoff>0)+1 # add 1 sub-bricks for accMax and cutoff
      } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
         ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')    
      cat('Possible reasons:\n\n')
      cat('1) Inappropriate model specification with options -model, or -qVars.\n\n')
      cat('2) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n\n')
      cat('3) Inconsistent variable names which are case sensitive. For example, factor\n')
      cat('named Scanner in model specification and then listed as scanner in the table hader\n')
      cat('would cause grief for 3dLME.\n')
      errex.AFNI("Quitting due to model test failure...")
   }
} else {  # LME below
   ModelForm <- as.formula(ModelForm)
   nRanEff <- length(lop$ranEff)
   lop$ranEffList <-  vector('list', nRanEff)
   names(lop$ranEffList) <- rep('Subj', nRanEff)
   #for(n in 1:nRanEff) lop$ranEffList[[n]] <- as.formula(lop$ranEff[[n]])
   for(n in 1:nRanEff) lop$ranEffList[[n]] <-eval(parse(text=lop$ranEff[[n]]))
   if(!is.na(lop$corStr[1])) lop$corStrList <- as.formula(c('~', lop$corStr[1])) else lop$corStrList <- NA
   if(any(!is.na(lop$vQV))) {
     lop$dataStr <- assVV2(lop$dataStr, lop$vQV, inData[ii,jj,kk,(nrow(lop$dataStr)+1):(2*nrow(lop$dataStr))], all(is.na(lop$vVarCenters)))
   }     

   while(is.null(fm)) {
      fm<-NULL
      lop$dataStr$Beta<-inData[ii, jj, kk,1:nrow(lop$dataStr)]
      options(warn=-1)     
      if(!is.na(lop$corStr[1])) try(fm <- lme(ModelForm, random=lop$ranEffList, data=lop$dataStr, 
         correlation=corAR1(0.3, form=lop$corStrList)), silent=TRUE) else try(fm <- lme(ModelForm, 
         random=lop$ranEffList, data=lop$dataStr), silent=TRUE)
      if(!is.null(fm)) if (lop$num_glt > 0) {
         n <- 1
         while(!is.null(fm) & (n <= lop$num_glt)) {
            if(is.na(lop$gltList[[n]])) gltRes[[n]] <- tryCatch(testInteractions(fm, pairwise=NULL,
               covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA) else
            gltRes[[n]] <- tryCatch(testInteractions(fm, custom=lop$gltList[[n]],
               covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA)
            if(is.na(gltRes[[n]])) fm <- NULL
            n <- n+1
         }
      }
   
      if(!is.null(fm)) if (lop$num_glf > 0) {  # does this work the same way as the t-tests above? Or need changes as in 3dMVM?
         n <- 1
         while(!is.null(fm) & (n <= lop$num_glf)) {
            if(is.na(lop$glfList[[n]])) glfRes[[n]] <- tryCatch(testFactors(fm, pairwise=NULL,
               covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA) else
            glfRes[[n]] <- tryCatch(testFactors(fm, levels=lop$glfList[[n]],
               covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none"), error=function(e) NA)
            if(is.na(glfRes[[n]])) fm <- NULL else chi_DF <- c(chi_DF, glfRes[[n]]$terms$`(Intercept)`$test[2,1])
            n <- n+1
         }
      }
      
      if(!is.null(fm))  {
         print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
      } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
         ii<-xinit; jj <- yinit; kk <- kk+1 } else {
         cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')    
         cat('Possible reasons:\n\n')
         cat('0) Make sure that R packages nlme and phia have been installed. See the 3dLME\n')
         cat('help documentation for more details.\n\n')
         cat('1) Inappropriate model specification with options -model, or -qVars.\n\n')
         cat('2) In correct specifications in general linear test coding with -gltCode.\n\n')
         cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
         cat('whether there are any inconsistencies.\n\n')
         cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
         cat('named Group in model specification and then listed as group in the table hader\n')
         cat('would cause grief for 3dLME.\n')
         errex.AFNI("Quitting due to model test failure...")     
         #break
      }
   }
   lop$nF      <- nrow(anova(fm))    # total number of F-stat
   lop$nBasis  <- (!is.na(lop$corStr[1]))*nrow(summary(fm)$tTable)  # number of basis functions
   nT      <- 2*(lop$num_glt + lop$nBasis)
   lop$NoBrick <- lop$nF + nT + lop$num_glf + lop$logLik # total number of output values per voxel/node   
   lop$SStype <- ifelse(lop$SS_type==3, 'marginal', 'sequential')
   lop$Fseq   <- 1:lop$nF  
   # test if it works
   #runLME(inData[1,2,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)
}

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

#for(ii in 1:dimx) for(jj in 1:dimy) 
#   aa<-runAOV(inData[ii,jj,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0)
# test runLME(inData[20,20,20,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)      

if(lop$ICC) {  # ICC part
   if(dimy==1 & dimz==1) Stat <- array(0, dim=c(dimx, lop$NoBrick)) else
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick))
   if (lop$nNodes==1) for (kk in 1:dimz) {
      # 2/9/2016: for 1D input files. Should do this for other scenarios
      if(dimy==1 & dimz==1) Stat <- aperm(apply(drop(inData[,,kk,]), 1, runREML, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,1)) else
      Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runREML, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }         
   if (lop$nNodes>1) {
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterEvalQ(cl, library(lme4))
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      for (kk in 1:dimz) {
         Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runREML, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      } 
      stopCluster(cl)
   }
   outLabel <- append(names(VarCorr(fm)), "Residual")
   statsym <- NULL
   for(ii in 1:lop$NoBrick) statsym <- c(statsym, list(list(sb=ii-1,typ="fim")))
} else if(lop$ICCb) {  # Bayesian ICC

   if(dimy==1 & dimz==1) Stat <- array(0, dim=c(dimx, lop$NoBrick)) else
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick))
   if (lop$nNodes==1) for (kk in 1:dimz) {
      # 2/9/2016: for 1D input files. Should do this for other scenarios
      if(dimy==1 & dimz==1) Stat <- aperm(apply(drop(inData[,,kk,]), 1, runREMLb, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,1)) else
      Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runREMLb, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }         
   if (lop$nNodes>1) {
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterEvalQ(cl, library(blme))
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      for (kk in 1:dimz) {
         Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runREMLb, ModelForm=ModelForm, dataframe=lop$dataStr, nBrk=lop$NoBrick, tag=0), c(2,3,1))
         cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      } 
      stopCluster(cl)
   }
   outLabel <- append(names(VarCorr(fm)), "Residual")
   statsym <- NULL
   for(ii in 1:lop$NoBrick) statsym <- c(statsym, list(list(sb=ii-1,typ="fim")))
    
} else if(lop$LOGIT) {  # logistic regression: no random effects for now
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick+2*(nlevels(lop$dataStr$Subj) + 1)))
   # the following is for bootstrapping
   sam <- function(gA, gB) {       
      A <- sample(gA, length(gA), replace=T)
      B <- sample(gB, length(gB), replace=T)
      return(c(A, B))
   }
   lop$nB <- 30
   zero <- which(lop$dataStr$Beta==0)
   one <- which(lop$dataStr$Beta==1)
   nBoot <- replicate(lop$nB, sam(zero, one))
    
   if (lop$nNodes==1) for (kk in 1:dimz) {
      Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runGLM2, dataframe=lop$dataStr, ModelForm=ModelForm, nBoot=nBoot), c(2,3,1))
      #Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runGLM, dataframe=lop$dataStr, ModelForm=ModelForm), c(2,3,1))
      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }         
   if (lop$nNodes>1) {
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterExport(cl, c("ModelForm", "assVV", "lop"), envir=environment())
      clusterEvalQ(cl, library(ROCR))
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      for (kk in 1:dimz) {
         Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runGLM2, dataframe=lop$dataStr, ModelForm=ModelForm, nBoot=nBoot), c(2,3,1))
         #Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runGLM, dataframe=lop$dataStr, ModelForm=ModelForm), c(2,3,1))
         cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
      stopCluster(cl)
   }
   Stat[Stat==Inf] <- 0
   Stat[Stat==-Inf] <- 0
   cutoff   <- Stat[,,,(lop$NoBrick+1):(lop$NoBrick+nlevels(lop$dataStr$Subj) + 1)]
   acc      <- Stat[,,,(lop$NoBrick+nlevels(lop$dataStr$Subj) + 2):(lop$NoBrick+2*(nlevels(lop$dataStr$Subj) + 1))]
   Stat     <- Stat[,,,1:lop$NoBrick]
   outLabel <- NULL
   statsym  <- NULL
   for(ii in 1:(lop$NoBrick/2)) {
   #for(ii in 1:((lop$NoBrick-1)/2)) {
      if(ii==lop$NoBrick/2)  outLabel <- c(outLabel, 'accuracy', 'accuracy z') else
      outLabel <- c(outLabel, rownames(summary(fm)$coefficients)[ii], paste(rownames(summary(fm)$coefficients)[ii], 'z'))
      statsym <- c(statsym, list(list(sb=2*ii-1, typ="fizt", par=NULL)))
   }
   #outLabel <- c(outLabel, 'accuracy')
   #outLabel <- c(outLabel, 'cutoff', 'max accuracy')
} else {  # typical LME part
   if(dimy == 1 & dimz == 1) {  # LME with surface or 1D data
      nSeg <- 20
      # drop the dimensions with a length of 1
      inData <- inData[, , ,]
      # break into 20 segments, leading to 5% increamental in parallel computing
      dimx_n <- dimx%/%nSeg + 1
      # number of datasets need to be filled
      fill <- nSeg-dimx%%nSeg
      # pad with extra 0s
      inData <- rbind(inData, array(0, dim=c(fill, NoFile)))
      # declare output receiver
      Stat <- array(0, dim=c(dimx_n, nSeg, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))
      # break input multiple segments for parrel computation
      dim(inData) <- c(dimx_n, nSeg, NoFile)
      if (lop$nNodes==1) for(kk in 1:nSeg) {
         if(lop$NoBrick > 1) Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME, dataframe=lop$dataStr,
               ModelForm=ModelForm), c(2,1)) else
            Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME, dataframe=lop$dataStr,
               ModelForm=ModelForm), dim=c(dimx_n, 1))
         cat("Computation done: ", 100*kk/nSeg, "%", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
      }
      
      if (lop$nNodes>1) {
      #library(snow)
      pkgLoad('snow')
      cl <- makeCluster(lop$nNodes, type = "SOCK")
      clusterEvalQ(cl, library(nlme))
      clusterEvalQ(cl, library(phia))
      clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
      clusterExport(cl, c("ModelForm", "lop", "assVV2"), envir=environment())
      for(kk in 1:nSeg) {
         if(lop$NoBrick > 1) Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME, dataframe=lop$dataStr,
               ModelForm=ModelForm), c(2,1)) else
         Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME, dataframe=lop$dataStr,
               ModelForm=ModelForm), dim=c(dimx_n, 1))
         cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
      }
      stopCluster(cl)
      }
      # convert to 4D
      dim(Stat) <- c(dimx_n*nSeg, 1, 1, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr))
      # remove the trailers (padded 0s)
      Stat <- Stat[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]
      
   } else {  # LME with volumetric data  
      # Initialization
      Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))
   
      if (lop$nNodes==1) for (kk in 1:dimz) {
         if(lop$NoBrick > 1) Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runLME, dataframe=lop$dataStr, 
               ModelForm=ModelForm), c(2,3,1)) else
            Stat[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runLME, dataframe=lop$dataStr, 
               ModelForm=ModelForm), dim=c(dimx, dimy, 1))      
         cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      } 
   
      if (lop$nNodes>1) {
         #library(snow)
         pkgLoad('snow')
         cl <- makeCluster(lop$nNodes, type = "SOCK")
         clusterEvalQ(cl, library(nlme))
         clusterEvalQ(cl, library(phia))
         clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
         clusterExport(cl, c("ModelForm", "lop", "assVV2"), envir=environment())  # for some reason phia needs this for multiple CPUs
         for (kk in 1:dimz) {
            if(lop$NoBrick > 1) Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME, 
                  dataframe=lop$dataStr, ModelForm=ModelForm), c(2,3,1)) else
               Stat[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runLME, 
                  dataframe=lop$dataStr, ModelForm=ModelForm), dim=c(dimx, dimy, 1))
            cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         } 
         stopCluster(cl)
      }
   }
   tTop <- 100
   if(lop$logLik) {
      Stat[1:(lop$NoBrick-1)][Stat[1:(lop$NoBrick-1)] > tTop] <- tTop  # Avoid outflow!!!!
      Stat[1:(lop$NoBrick-1)][Stat[1:(lop$NoBrick-1)] < (-tTop)] <- -tTop  # Avoid outflow!!!!
   } else {
      Stat[Stat > tTop] <- tTop  # Avoid outflow!!!!
      Stat[Stat < (-tTop)] <- -tTop  # Avoid outflow!!!!
   }
   outLabel <- paste(rownames(anova(fm))[lop$Fseq], " F")
   if(!is.na(lop$corStr[1])) for(n in 1:dim(summary(fm)$tTable)[1]) {
      outLabel <- append(outLabel, rownames(summary(fm)$tTable)[n])
      outLabel <- append(outLabel, paste(rownames(summary(fm)$tTable)[n], "t"))
   }               
   if(lop$num_glt > 0) for (n in 1:lop$num_glt) {
      outLabel <- append(outLabel, lop$gltLabel[n])
      outLabel <- append(outLabel, paste(lop$gltLabel[n], "Z"))
   
   }
   if(lop$num_glf > 0) for (n in 1:lop$num_glf) outLabel <- append(outLabel, paste(lop$glfLabel[n], "Chisq"))
   if(lop$logLik) outLabel <- append(outLabel, "logLik")
   
   statsym <- NULL
   IdxAdj <- 1
   #IdxAdj <- as.integer(!is.na(lop$corStr[1]))
   for (i in (2-IdxAdj):(lop$nF+1-IdxAdj)) {  # has an intercept or not
      # DFs are acquired from the last solvable voxel 
      #     statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, 
      #        " fift ", anova(fm)$numDF[i], " ", anova(fm)$denDF[i])
           statsym <- c(statsym, list(list(sb=i-2+IdxAdj, 
                   typ="fift", par=c( anova(fm)$numDF[i], anova(fm)$denDF[i]))))
           
   }  # from 0 to NoF-1
   
   if(lop$num_glt > 0) for (n in 1:lop$num_glt) #statpar <- paste(statpar, " -substatpar ", lop$nF+2*n-1, " fizt ")
      statsym <- c(statsym, list(list(sb=lop$nF+2*n-1, typ="fizt", par=NULL)))
      #statpar <- paste(statpar, " -substatpar ", lop$nF+2*n-1, " fitt ",gltDF[n])
   
   #if(!is.null(lop$QV)) if(nCovVal>0) for (n in 1:nCovVal) 
   #   statpar <- paste(statpar, " -substatpar ", nF+2*lop$num_glt+2*n-1, " fitt ", summary(fm)$tTable[lop$Fseq[n],"DF"])
   bb <- as.numeric(strsplit(as.character(ModelForm), "\\+", fixed=F)[[3]][1])
   
   if(!is.na(bb)) if(bb==0 | bb == -1) for (n in 1:dim(summary(fm)$tTable)[1])  # basis functions
      statsym <- c(statsym, list(list(sb=lop$nF+2*n-1, typ="fitt", par=summary(fm)$tTable[n,"DF"])))
      #statpar <- paste(statpar, " -substatpar ", nF+2*n-1, " fitt ", summary(fm)$tTable[n,"DF"])
   
   if(lop$num_glf > 0) for (n in 1:lop$num_glf) #statpar <- paste(statpar, " -substatpar ", lop$nF+2*n-1, " fizt ")
      statsym <- c(statsym, list(list(sb=lop$NoBrick-lop$logLik-lop$num_glf+n-1, typ="fict", par=chi_DF)))
} # if(lop$ICC)

#statpar <- paste(statpar, " -addFDR -newid ", lop$outFN)
write.AFNI(lop$outFN, Stat[,,,1:lop$NoBrick, drop=FALSE], outLabel, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_short')
if(lop$LOGIT) {
   write.AFNI(paste(parse.AFNI.name(lop$outFN)$path, paste('/cutoff_', parse.AFNI.name(lop$outFN)$prefix, sep=''), sep=''),
      cutoff, label=NULL, defhead=head, idcode=newid.AFNI(), com_hist=lop$com_history, type='MRI_short')
   write.AFNI(paste(parse.AFNI.name(lop$outFN)$path, paste('/acc_', parse.AFNI.name(lop$outFN)$prefix, sep=''), sep=''),
      acc, label=NULL, defhead=head, idcode=newid.AFNI(), com_hist=lop$com_history, type='MRI_short')
}
if(!is.null(lop$resid))
   write.AFNI(lop$resid, Stat[,,,(lop$NoBrick+1):(lop$NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)), drop=FALSE],
      label=NULL, defhead=head, idcode=newid.AFNI(), com_hist=lop$com_history, type='MRI_short')

#system(statpar)
print(sprintf("Congratulations! You've got an output %s", lop$outFN))

##############  END  ############

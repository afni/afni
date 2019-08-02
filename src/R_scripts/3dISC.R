#!/usr/bin/env AFNI_Batch_R

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dISC'
# Global variables
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin 3dISC Input functions ################################
#################################################################################

#The help function for 3dISC batch (AFNI-style script mode)
help.ISC.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
             ================== Welcome to 3dISC ==================          
       Program for Voxelwise Inter-Subject Correlation (ISC) Analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.2, Aug 2, 2019
Author: Gang Chen (gangchen@mail.nih.gov)
Website - ATM
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892, USA
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 Intersubject correlation (ISC) measures the extent of BOLD response similarity
 or synchronization between two subjects who are scanned under the same
 experience such as movie watching, music learning, etc. The program performs
 voxelwise analysis with linear mixed-effects modeling that is laid out in the
 following paper:

 Chen, G., Taylor, P.A., Shin, Y.W., Reynolds, R.C., Cox, R.W., 2017. Untangling
 the Relatedness among Correlations, Part II: Inter-Subject Correlation Group
 Analysis through Linear Mixed-Effects Modeling. Neuroimage 147:825-840.

 Input files for 3dISC are voxelwise correlation values from all subject pairs.
 If they are not Fisher-transformed, use option -r2z in 3dISC to transform the
 data. If there are two or more groups of subjects, ISCs across groups are also
 required as input unless the data are analyzed for each group separately. 
 Input files can be in AFNI, NIfTI or surface (niml.dset) format. In other words,
 with totally n subjects, you should provide n(n-1)/2 input files (no duplicates
 allowed). Currently 3dISC provides in the output the effect estimate (e.g., ISC
 value) and the corresponding t-statistic at each voxel.

 The LME platform allows for the incorporation of various types of explanatory
 variables including categorical (between- and within-subject factors) and
 quantitative variables (e.g., age, behavioral data). The burden of properly
 specifying the weights for each effect (e.g., contrast) is unfortunately 
 placed on the user\'s shoulder, and sorting out the number and order of 
 predictors (regressors) can be overwhelming especially when there are more 
 than two factor levels or when an interaction effect is involved. So, 
 familiarize yourself with the details of the two popular factor coding 
 strageties (dummy and deviation coding):
 https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

 The four exemplifying scripts below are good demonstrations.More examples will
 be added in the future if I could crowdsource more scenarios from the users
 (including you the reader). In case you find one or two of them that
 are similar to your data structure, use the example(s) as a template and then
 build up your own script.
 
 In addition to R installtion, the following R packages need to be installed
 first before running 3dISC: "lme4" and "snow". To install these packages, 
 run the following command at the terminal:

 rPkgsInstall -pkgs "lme4,snow"

 Alternatively you may install them in R:
 
 install.packages("lme4")
 install.packages("snow") 

 Once the 3dISC command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called ISC.txt, and execute it with the 
 following (assuming on tc shell),
 
 nohup tcsh -x ISC.txt &
 
 or,
 
 nohup tcsh -x ISC.txt > diary.txt &
 nohup tcsh -x ISC.txt |& tee diary.txt &

 The advantage of the latter commands is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.\n'

   ex1 <-  
"Example 1 --- Simplest case: ISC analysis for one group of subjects without
  any explanatory variables. In other words, the effect of interest is the ISC
  at the populaton level. The output is the group ISC plus its t-statistic.
  The components within paratheses in the -model specifications are R 
  notations for random effects.

-------------------------------------------------------------------------
    3dISC -prefix ISC -jobs 12                    \\
          -model  '1+(1|Subj1)+(1|Subj2           \\
          -dataTable                              \\
          Subj1   Subj2  InputFile                \\
          s1       s2    s1_s2+tlrc               \\
          s1       s3    s1_s3+tlrc               \\
          s1       s4    s1_s4+tlrc               \\
          s1       s5    s1_s5+tlrc               \\
          s1       s6    s1_s6+tlrc               \\
          s1       s7    s1_s7+tlrc               \\
          ... 
          s2       s3    s2_s3+tlrc               \\
          s2       s4    s2_s4+tlrc               \\
          s2       s5    s2_s5+tlrc               \\
          ...                                   
   \n"

   ex2 <-
"Example 2 --- ISC analysis with two groups (G1 and G2). Three ISCs can be
  inferred at the population level, G11 (ISC among subjects within the first
  group G1), G22 (ISC among subjects within the second group G2), and G12 (ISC
  between subjects in the first group G1 and those in the second group G2). The 
  research interest can be various comparisons among G11, G22 and G12, and this
  is the reason the group column 'grp' is coded with three types of population
  ISC: G11, G22 and G12. By default each factor (categorical variable) is 
  internally quantified in the model using deviation coding with alphabetically
  the last level as the reference. Notice the semi-esoteric weights for those
  comparisons with -gltCode: the first weight corresponds to the intercept in 
  the model, which is the average effect across all the factor levels (and
  corresponds to the zero value of a quantitative variable if present). If dummy 
  coding is preferred, check out the next script below. The components within 
  paratheses in the -model specifications are R notations for random effects. 
  Here is a good reference about factor coding strategies:
  https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

-------------------------------------------------------------------------
    3dISC -prefix ISC2a -jobs 12                \\
          -model  'grp+(1|Subj1)+(1|Subj2)'     \\
          -gltCode ave     '1 0 -0.5'           \\
          -gltCode G11     '1 1 0'              \\
          -gltCode G12     '1 0 1'              \\
          -gltCode G22     '1 -1 -1'            \\
          -gltCode G11vG22 '0 2 1'              \\
          -gltCode G11vG12 '0 1 -2'             \\
          -gltCode G12vG22 '0 1 2'              \\
          -gltCode ave-G12 '0 0 -1.5'           \\
          -dataTable                            \\
          Subj1 Subj2    grp      InputFi       \\
          s1     s2      G11      s1_2+tlrc     \\
          s1     s3      G11      s1_3+tlrc     \\
          s1     s4      G11      s1_4+tlrc     \\
          ...	         
          s1     s25     G12      s1_25+tlr     \\
          s1     s26     G12      s1_26+tlr     \\
          s1     s27     G12      s1_26+tlr     \\
          ... 	         
          s25    s26     G22     s25_26+tlr     \\
          s25    s27     G22     s25_27+tlr     \\
          s25    s48     G22     s51_28+tlr     \\
         ...

  The above script is equivalent to the one below. The only difference is that
  we force 3dISC to adopt dummy coding by adding a zero in the -model
  specification, which makes the weight coding much more intuitive. In this
  particular case, the three weights are associated with the three
  categories, G11, G12 and G22 (no intercept is assumed in the model as
  requested with the zero (0) in the model specifications).

-------------------------------------------------------------------------
    3dISC -prefix ISC2b -jobs 12                  \\
          -model  '0+grp+(1|Subj1)+(1|Subj2)'     \\
          -gltCode ave     '0.5 0 0.5'            \\
          -gltCode G11     '1 0  0'               \\
          -gltCode G12     '0 1 0'                \\
          -gltCode G22     '0 0 1'                \\
          -gltCode G11vG22 '1 0 -1'               \\
          -gltCode G11vG12 '1 -1 0'               \\
          -gltCode G12vG22 '0 1 -1'               \\
          -gltCode ave-G12 '0.5 -1 0.5'           \\
          -dataTable                              \\
          Subj1 Subj2    grp      InputFile       \\
          s1     s2      G11      s1_2+tlrc       \\
          s1     s3      G11      s1_3+tlrc       \\
          s1     s4      G11      s1_4+tlrc       \\
          ...	         
          s1     s25     G12      s1_25+tlr       \\
          s1     s26     G12      s1_26+tlr       \\
          s1     s27     G12      s1_26+tlr       \\
          ... 	         
          s25    s26     G22     s25_26+tlr       \\
          s25    s27     G22     s25_27+tlr       \\
          s25    s48     G22     s51_28+tlr       \\
         ...
  
  There is a third way to analyze this same dataset if we are NOT
  interested in the between-group ISC, G12. First, we adopt deviation
  coding for the two groups by replacing two groups G1 and G2 with 0.5 and 
  -0.5. Then add up the two values for each row (each subject pair), 
  resulting in three possible values of 1, -1 and 0. Put those three values 
  in the group column in the data table.

-------------------------------------------------------------------------
    3dISC -prefix ISC2c -jobs 12                       \\
          -model  'grp+(1|Subj1)+(1|Subj2)'            \\
          -qVars   grp                                 \\
          -gltCode ave     '1 0'                       \\
          -gltCode G11vG22 '0 1'                       \\
          -gltCode G11     '1 0.5'                     \\
          -gltCode G22     '1 -0.5'                    \\
          -dataTable                                   \\
          Subj1 Subj2    grp      InputFile            \\
          s1     s2       1      s1_2+tlrc             \\
          s1     s3       1      s1_3+tlrc             \\
          s1     s4       1      s1_4+tlrc             \\
          ...	          
          s1     s25      0      s1_25+tlr             \\
          s1     s26      0      s1_26+tlr             \\
          s1     s27      0      s1_26+tlr             \\
          ... 	          
          s25    s26     -1     s25_26+tlr             \\
          s25    s27     -1     s25_27+tlr             \\
          s25    s48     -1     s51_28+tlr             \\
          ...
     \n"

      ex3 <-  
"Example 3 --- ISC analysis for one group of subjects. The only difference
  from Example 1 is that we want to add an explanatory variable 'Age'. 
  Before the age values are incoporated in the data table, do two things:
  1) center the age by subtracting the cener (e.g., overall mean) from each 
  subject's age, and 2) for each subject pair (each row in the data table)
  add up the two ages (after centering). The components within paratheses 
  in the -model specifications are R notations for random effects.

-------------------------------------------------------------------------
    3dISC -prefix ISC3 -jobs 12                       \\
          -model  'Age+(1|Subj1)+(1|Subj2)'           \\
          -qVars   Age                                \\
          -gltCode ave     '1 0'                      \\
          -gltCode Age     '0 1'                      \\
          -dataTable                                  \\
          Subj1   Subj2  Age  InputFile               \\
          s1       s2    2   s1_s2+tlrc               \\
          s1       s3    5   s1_s3+tlrc               \\
          s1       s4    -4  s1_s4+tlrc               \\
          s1       s5    3   s1_s5+tlrc               \\
          s1       s6    -2  s1_s6+tlrc               \\
          s1       s7    -1  s1_s7+tlrc               \\
          ... 		    
          s2       s3    2   s2_s3+tlrc               \\
          s2       s4    4   s2_s4+tlrc               \\
          s2       s5    -5  s2_s5+tlrc               \\
         ...
     \n"

      ex4 <-  
"Example 4 --- ISC analysis with two groups of subject (Sex: females and males)
  plus a quantitative explanatory variable (Age). We are going to combine the
  modeling strategy in the third analysis of Example 2 with Example 3. In 
  addition, we consider the interaction between Sex and Age by adding their
  product as another column (called 'SA' in the data table). The components 
  within paratheses in the -model specifications are R notations for random 
  effects.

-------------------------------------------------------------------------
    3dISC -prefix ISC2c -jobs 12                          \\
          -model  'Sex+Age+SA+(1|Subj1)+(1|Subj2)'        \\
          -qVars  'Sex,Age,SA'                            \\
          -gltCode ave       '1   0  0  0'                \\
          -gltCode G11vG22   '0   1  0  0'                \\
          -gltCode G11       '1  0.5 0  0'                \\
          -gltCode G22       '1 -0.5 0  0'                \\
          -gltCode Age       '0   0  1  0'                \\
          -gltCode Age1vAge2 '0   0  0  1'                \\
          -gltCode Age1      '0   0  1  0.5'              \\
          -gltCode Age2      '0   0  1 -0.5'              \\
          -dataTable                                      \\
          Subj1 Subj2    Sex Age SA     InputFile         \\
          s1     s2       1  2   2    s1_2+tlrc           \\
          s1     s3       1  5   5    s1_3+tlrc           \\
          s1     s4       1  -4 -4    s1_4+tlrc           \\
          ...	                    
          s1     s25      0  -2  0    s1_25+tlr           \\
          s1     s26      0  -1  0    s1_26+tlr           \\
          s1     s27      0   3  0    s1_26+tlr           \\
          ... 	                    
          s25    s26     -1  4  -4    s25_26+tlr          \\
          s25    s27     -1  -5  5    s25_27+tlr          \\
          s25    s48     -1  2  -2    s51_28+tlr          \\
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
read.ISC.opts.batch <- function (args=NULL, verb = 0) {
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

      '-Subj1' = apl(n = 1, d = NA,  h = paste(
   "-Subj1 var_name: var_name is used to specify the column name that is designated as",
   "        as the first measuring entity variable (usually subject). This option,",
   "        combined with the another option '-Subj2', forms a pair of two subjects;",
   "        the order between the two subjects does not matter. The default (when",
   "        the option is not invoked) is 'Subj1', in which case the column header has",
   "        to be exactly as 'Subj1'.\n", sep = '\n'
                     ) ),

      '-Subj2' = apl(n = 1, d = NA,  h = paste(
   "-Subj2 var_name: var_name is used to specify the column name that is designated as",
   "        as the first measuring entity variable (usually subject). This option,",
   "        combined with the another option '-Subj1', forms a pair of two subjects;",
   "        the order between the two subjects does not matter. The default (when",
   "        the option is not invoked) is 'Subj2', in which case the column header has",
   "        to be exactly as 'Subj1'.\n", sep = '\n'
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
   "         In the ISC context the simplest model is \"1+(1|Subj1)+(1|Subj2)\"in",
   "         while the random effect from each of the two subjects in a pair is",
   "         symmetrically incorporated in the model. Each random-effects factor is",
   "         specified within paratheses per formula convention in R. Any",
   "         effects of intereste and confounding variables (quantitative or",
   "         categorical variables) can be added as fixed effects without paratheses.\n", sep = '\n'
             ) ),

       '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dISC.dbg.AFNI.args in the current directory",
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
	     
#     '-num_glt' = apl(n=1, d=0, h = paste(
#   "-num_glt NUMBER: Specify the number of general linear test (GLT). A GLT",
#   "         can be a factor level or a contrast between two factor levels.",
#   "         See details in -gltCode.\n", sep = '\n') ),
#
     '-gltCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltCode label weights: Specify the label and weights of interest. The",
   "       weights should be surrounded with quotes. \n", sep = '\n'
                     ) ),

      '-r2z' = apl(n=0, h = paste(
   "-r2z: This option performs Fisher transformation on the response variable",
   "         (input files) if it is correlation value. Do not invoke the option",
   "         if the transformation has already been applied.\n", sep='\n')),

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
   "         2) The table should contain at least three columns, two of which are",
   "         for the two subjects in each pair, 'Subj1' and 'Subj2'. These two columns",
   "         code the labels of the two subjects involved",
   "         for each ISC file that is listed in the column 'InputFile'. The order of",
   "         the columns does not matter. Any subject-level explanatory variables",
   "         (e.g., age, sex, etc.) can be",
   "         specified as columns in the table. Each row should contain only one",
   "         ISC file in the table of long format (cf. wide format) as defined in R.",
   "         The level labels of a factor should contain at least",
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
      errex.AFNI('Error parsing arguments. See 3dISC -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$cutoff <- 0
      #lop$fixEff  <- 1
      lop$maskFN <- NA
      lop$Subj1   <- 'Subj1' #default
      lop$Subj2   <- 'Subj2' #default
      #lop$tStat  <- NA
      lop$IF     <- 'InputFile' #default

      #lop$ranEff <- NA
      lop$model  <- NA
      lop$qVars  <- NA   
      #lop$vVars  <- NA
      #lop$vQV    <- NA
      lop$qVarCenters <- NA
      #lop$vVarCenters <- NA
      #lop$num_glt     <- 0
      lop$gltCode    <- NULL
      

      #lop$ISC <- 2
      lop$dataTable <- NULL
      
      lop$iometh <- 'clib'
      lop$dbgArgs  <- FALSE # for debugging purpose
      lop$r2z     <- FALSE # Fisher transformation
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
	     #tStat  = lop$tStat <- ops[[i]],
             qVars  = lop$qVars  <- ops[[i]],
             #vVars  = lop$vVars  <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             #vVarCenters = lop$vVarCenters <- ops[[i]],
             #ISC     = lop$ISC     <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
             
             help = help.ISC.opts(params, adieu=TRUE),
             dbgArgs = lop$dbgArgs <- TRUE,
	     r2z     = lop$r2z     <- TRUE,

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }

   return(lop)
}# end of read.ISC.opts.batch

#Change options list to 3dISC variable list 
process.ISC.opts <- function (lop, verb = 0) {
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

   if(!is.null(lop$gltCode)) {
      sq <- as.numeric(unlist(lapply(lop$gltCode, '[', 1)))
#      if(identical(sort(sq), as.numeric(seq(1, lop$num_glt)))) {
      lop$gltLabel <- unlist(lapply(lop$gltCode, `[`, 1))
      lop$gltM <- do.call(rbind, lapply(lop$gltCode, function(x) as.numeric(x[2:length(x)])))
   } #else errex.AFNI(c('The number of -gltCode is not consistent with that \n',
#         'specified by -num_glt ',  lop$num_glt))
   
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
################# ISC Computation functions ##############################
#################################################################################

# LME: bare-bone approach with LME for ISC: some voxels may have 0 ISC values: effect estimates as input only
runLME <- function(myData, ModelForm, DM, gltM, intercept, nF, nS, tag) {
   #browser()
   if(!all(myData == 0)) {     
      DM$ISC <- myData
      options(warn=-1)
      DM <- rbind(DM, DM)
      DM$Subj1[(nF+1):(2*nF)] <- DM$Subj2[1:nF]
      DM$Subj2[(nF+1):(2*nF)] <- DM$Subj1[1:nF]
      
      try(fm <- summary(lmer(ModelForm, data=DM)), silent=TRUE)
     
      if(is.null(fm)) {
         if(intercept==1) return(rep(0,2)) else return(rep(0,2*nrow(gltM)))
      } else {
         if(intercept==1) {
            cc <- fm$coefficients
            tt <- cc[1]*sqrt(nS-1)/(cc[2]*sqrt(2*nS-1))  # new t-value
            return(c(z2r(cc[1]), tt))
	 } else {
	    vv <- t(gltM %*% coef(fm)[,1])
            se <- rep(1e8, nrow(gltM))
            for(ii in 1:nrow(gltM)) se[ii] <- as.numeric(sqrt(t(gltM[ii,]) %*% vcov(fm) %*% gltM[ii,]))
            tt <- (vv*sqrt(nS-1))/(se*sqrt(2*nS-1))
	    return(c(rbind(vv,tt)))
	 }
      }
   } else if(intercept==1) return(rep(0,2)) else return(rep(0,2*nrow(gltM)))
}
# runLME(inData[30,30,30,], lop$model, lop$dataStr, lop$gltM, intercept, nF, nS, 0)

runLME2 <- function(myData, ModelForm, DM, nF, nS, nBrk, tag) {
   #browser()
   #myStat<-vector(mode="numeric", length= nBrk)
   if(!all(myData == 0)) {     
      DM$ISC <- myData
      options(warn=-1)
      DM <- rbind(DM, DM)
      DM$Subj1[(nF+1):(2*nF)] <- DM$Subj2[1:nF]
      DM$Subj2[(nF+1):(2*nF)] <- DM$Subj1[1:nF]
      
      try(fm <- summary(lmer(ModelForm, data=DM)), silent=TRUE)
      #if(is.null(fm)) return(rep(0,14))) else {
      #   cc <- fm$coefficients
      #   tt <- cc[1]*sqrt(nS-1)/(cc[2]*sqrt(2*nS-1))  # new t-value
      #	 return(c(z2r(cc[1]), tt))
      #}
      if(is.null(fm)) return(rep(0,14)) else {
         ww <- matrix(c(0.5, 0, 0.5,    # average
                        1,0,0,    # G1
                        0,1,0,    # G12
                        0,0,1,  # G2
                        1,0,-1,   # G1 - G2
                        1,-1,0,   # G1 - G12
			0,1,-1,    # G12 - G2
                        0.5,-1,0.5), # (G1+G2)/2 - G12
                      nrow = 7, ncol = 3, byrow = TRUE)
         vv <- t(ww%*%coef(fm)[,1])
         se <- rep(1e8, 7)
         for(ii in 1:7) se[ii] <- as.numeric(sqrt(t(ww[ii,]) %*% vcov(fm) %*% ww[ii,]))
         tt <- (vv*sqrt(nS-1))/(se*sqrt(2*nS-1))
	 return(c(rbind(vv,tt)))
      }
   } else return(rep(0,14))
}
# runLME2(inData[30,30,30,], lop$model2, lop$dataStr, nF, nS, 2, 0)


#################################################################################
########################## Begin ISC main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbgArgs' %in% args ) {
         try(save(args, rfile, file=".3dISC.dbg.AFNI.args", ascii = TRUE), silent=TRUE)
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.ISC(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.ISC.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.ISC.opts.batch(args, verb = 0)))
         stop('Error parsing input')
      
      #str(lop);
      if(is.null(lop <- process.ISC.opts(lop, verb = lop$verb))) 
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
#   ModelForm <- paste("Beta ~", lop$fixEff)

# standardize the names for Y, ROI and subject
#names(lop$dataStr)[names(lop$dataStr)==lop$Subj] <- 'Subj'
names(lop$dataStr)[names(lop$dataStr)==lop$IF] <- 'InputFile'
#names(lop$dataStr)[names(lop$dataStr)==lop$tStat] <- 'Tstat'

# Maybe not list for these two, or yes?
lop$dataStr$Subj1 <-  as.factor(lop$dataStr$Subj1)
lop$dataStr$Subj2 <-  as.factor(lop$dataStr$Subj2)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)
#if(is.null(lop$Tstat)) lop$dataStr$Tstat <-  as.character(lop$dataStr$Tstat)

# center on user-speficied value or mean
if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters))) 
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F)[,,drop=T] else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)[,,drop=T]

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of data structure *****\n')

S1 <- levels(lop$dataStr$Subj1)
S2 <- levels(lop$dataStr$Subj2)
Sa <- union(S1, S2)  # all subject lables
nS <- length(Sa)  # total number of subjects
NN <- nS*(nS-1)
nF <- dim(lop$dataStr[1])[1] # number of input files

cat(length(S1), 'subjects in set1 :', S1, '\n')
cat(length(S2), 'subjects in set2 :', S2, '\n')
cat(nS, 'subjects in total:', Sa, '\n') 
cat(nF, 'response values\n')
# check number of rows/files
if(nF != NN/2) errex.AFNI(c("Error: the number of rows/files, ", nF, ", is not equal to ", NN/2, " - the\n",
                " possible subject pairs!\n"))

# make sure that all unique combinations (subject pairs) are in the data table  
#ll <- 0
for(ii in 1:(nS-1))
   for(jj in (ii+1):nS) {
#      ll <- ll+1
      mm <- sum((lop$dataStr$Subj1==Sa[ii] | lop$dataStr$Subj1==Sa[jj]) &
         (lop$dataStr$Subj2==Sa[ii] | lop$dataStr$Subj2==Sa[jj]))
      if(mm == 0) errex.AFNI(c("Subject pair of ", Sa[ii], " and ", Sa[jj]," is missing in the table!\n")) else
      if(mm > 1)  errex.AFNI(c("More than one copy of subject pair, ", Sa[ii], " and ", Sa[jj],", exists in the table!\n"))
      
#      allData[,,,ll] <- read.AFNI(allFiles[(allFiles$Subj == G1Subj[ii] | allFiles$Subj == G1Subj[jj]) &
#         (allFiles$Subj2 == G1Subj[ii] | allFiles$Subj2 == G1Subj[jj]), 'InputFile'], forcedset = TRUE)$brk
}

# combine the levels between the two region lists: NO! It seems to mess up the modeling wih brm
levels(lop$dataStr$Subj1) <- union(S1, S2)
levels(lop$dataStr$Subj2) <- union(S1, S2)

if(dim(lop$dataStr)[2] > 3) for(ii in 3:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':', 
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'numeric' | class(lop$dataStr[,ii]) == 'matrix')  # numeric doesn't work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')
#cat(lop$num_glt, 'post hoc tests\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')
#if(lop$ISC | lop$ISCb) showTab <- as.formula(paste('~', gsub("\\*", "+", lop$ranEff))) else {
#   showTab <- as.formula(paste('~', gsub("\\:", "+", gsub("\\*", "+", lop$fixEff))))
#   if(!is.na(lop$qVars)) for(ii in rev(levels(ordered(lop$QV)))) # reversing the oder of those quantitative covariates so that
#      showTab <- gsub(paste('\\*', ii, sep=''), '', gsub(paste('\\+', ii, sep=''), '', showTab))
#   if(!is.na(lop$vVars)) showTab <- sub(lop$vQV, "", showTab)
#   #showTab <- as.formula(gsub("\\*", "+", showTab))  # in case there are still some *'s like between-subjects factors
#}
##print(xtabs(showTab, data=lop$dataStr))                                           
#
#if(!lop$ISC | lop$ISCb) {
#   cat('\nTabulation of subjects against all categorical variables')
#   all_vars <- names(lop$dataStr)
#   for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
#      cat('\n~~~~~~~~~~~~~~')
#      cat('\nS vs ', var, ':\n', sep='')
#      print(table(lop$dataStr$Subj, lop$dataStr[,var]))
#   }
#}

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

# Fisher transformation
fisher <- function(r) ifelse(abs(r) < .995, 0.5*(log(1+r)-log(1-r)), stop('Are you sure that you have correlation values so close to 1 or -1?'))
z2r <- function(z) (exp(2*z)-1)/(exp(2*z)+1)
if(lop$r2z) inData <- fisher(inData)
  
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
#      dim(inData) <- c(dimx, dimy, dimz, lop$nVVars+lop$nS)
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

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or something else\n')
cat('is likely inappropriate.\n\n')

xinit <- dimx%/%3
if(dimy==1) yinit <- 1 else yinit <- dimy%/%3
if(dimz==1) zinit <- 1 else zinit <- dimz%/%3

ii <- xinit; jj <- yinit; kk <- zinit
if(unlist(strsplit(lop$model, split="[+]"))[1]==1) {
   intercept <- 1
   lop$NoBrick <- 2
} else {
   intercept <- 0
   lop$NoBrick <- 2*nrow(lop$gltM) # ISC plus its F-stat
}

lop$model <- as.formula(paste('ISC ~ ', lop$model))
require(lme4)
fm<-NULL  
while(is.null(fm)) {
   lop$dataStr$ISC <- inData[ii, jj, kk,]
   options(warn=-1)
   DM <- lop$dataStr
   DM <- DM[, -which(names(DM) %in% lop$IF)] # remove input file column
   DM <- rbind(DM, DM)
   DM$Subj1[(nF+1):(2*nF)] <- DM$Subj2[1:nF]
   DM$Subj2[(nF+1):(2*nF)] <- DM$Subj1[1:nF]
   
   try(fm <- lmer(lop$model, data=DM), silent=TRUE)
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
      cat('would cause grief for 3dISC.\n')
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

   if(dimy==1 & dimz==1) Stat <- array(0, dim=c(dimx, lop$NoBrick)) else
   Stat <- array(0, dim=c(dimx, dimy, dimz, lop$NoBrick))

   if (lop$nNodes==1) for (kk in 1:dimz) {
      # 2/9/2016: for 1D input files. Should do this for other scenarios
      if(dimy==1 & dimz==1) Stat <- aperm(apply(drop(comArr[,,kk,]), 1, runMeta, dataframe=lop$dataStr, ranFormMeta=lop$ranFormMeta, nBrk=lop$NoBrick, tag=0), c(2,1)) else
      Stat[,,kk,] <- aperm(apply(comArr[,,kk,], c(1,2), runMeta, dataframe=lop$dataStr, ranFormMeta=lop$ranFormMeta, nBrk=lop$NoBrick, tag=0), c(2,3,1))
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }         

if (lop$nNodes>1) {
   pkgLoad('snow')
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterExport(cl, "z2r", envir=environment())
   clusterEvalQ(cl, library(lme4))
   clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
   for (kk in 1:dimz) {
      Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME, ModelForm=lop$model,
                  DM=lop$dataStr, gltM=lop$gltM, intercept=intercept, nF=nF, nS=nS, tag=0), c(2,3,1)) 
      cat("Z slice #", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   }
    stopCluster(cl)
}
# runLME(inData[30,30,30,], lop$model, lop$dataStr, lop$gltM, intercept, nF, nS, 0)
Top <- 100
Stat[is.nan(Stat)] <- 0
Stat[Stat > Top] <- Top  
Stat[Stat < (-Top)] <- -Top  

brickNames <- c(rbind(lop$gltLabel, paste(lop$gltLabel, 't')))
statsym <- NULL
#if(lop$num_glt>0) for(ii in 1:lop$num_glt)
for(ii in 1:(lop$NoBrick/2)) statsym <- c(statsym, list(list(sb=ii-1, typ="fitt", par=nS-1)))

write.AFNI(lop$outFN, Stat[,,,1:lop$NoBrick], brickNames, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_short')

print(sprintf("Congratulations! You've got an output %s", lop$outFN))

##############  END  ############

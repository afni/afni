#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

# Command line to run this script: 3dMVM.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dMVM'

# Global variables
iterPar <- 'matrPar'
respVar <- c('InputFile', 'Inputfile', 'inputFile', 'inputfile', 'Ausgang_val', 'ausgang_val')
tolL <- 1e-16 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's

#################################################################################
##################### Begin MVM Input functions ################################
#################################################################################

#The help function for 3dMVM batch (AFNI-style script mode)
help.MVM.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
                      Welcome to 3dMVM ~1~
    AFNI Group Analysis Program with Multi-Variate Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 4.0.0, Sept 5, 2018
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/sscc/gangc/MVM.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
 3dMVM is a group-analysis program that performs traditional ANOVA- and ANCOVA-
 style computations. In addition, it can run multivariate modeling in the sense
 of multiple simultaneous response variables. For univariate analysis, no bound
 is imposed on the  numbers of explanatory variables, and these variables can be
 either categorical (factor) or numerical/quantitative (covariate). F-statistics
 for all main effects and interactions are automatically included in the output.
 In addition, general linear tests (GLTs) can be requested via symbolic coding.
 
 Input files for 3dMVM can be in AFNI, NIfTI, or surface (niml.dset) format.
 Note that unequal number of subjects across groups are allowed, but scenarios 
 with missing data for a within-subject factor are better modeled with 3dLME. 
 Cases with quantitative variables (covariates) that vary across the levels of 
 a within-subject variable are also better handled with 3dLME. Computational 
 cost with 3dMVM is very high compared to 3dttest++ or 3dANOVAx, but it has the
 capability to correct for sphericity violations when within-subject variables
 with more than two levels are involved.
 
 Please cite: ~1~
 If you want to cite the analysis approach for AN(C)OVA, use the following:~2~
 
 Chen, G., Adleman, N.E., Saad, Z.S., Leibenluft, E., Cox, R.W. (2014). 
 Applications of Multivariate Modeling to Neuroimaging Group Analysis: A
 Comprehensive Alternative to Univariate General Linear Model. NeuroImage 99,
 571-588. 10.1016/j.neuroimage.2014.06.027
 https://afni.nimh.nih.gov/pub/dist/HBM2014/Chen_in_press.pdf

 For group analyis with effect estimates from multiple basis funcitons, cite: ~2~

 Chen, G., Saad, Z.S., Adleman, N.E., Leibenluft, E., Cox, R.W. (2015). 
 Detecting the subtle shape differences in hemodynamic responses at the
 group level. Front. Neurosci., 26 October 2015.
 http://dx.doi.org/10.3389/fnins.2015.00375

 Installation requirements: ~1~
 In addition to R installation, the following two R packages need to be acquired
 in R first before running 3dMVM: "afex" and "phia". In addition, the "snow" package
 is also needed if one wants to take advantage of parallel computing. To install
 these packages, run the following command at the terminal:

 rPkgsInstall -pkgs ALL

 Alternatively you may install them in R:

 install.packages("afex")
 install.packages("phia")
 install.packages("snow")

 More details about 3dMVM can be found at 
 https://afni.nimh.nih.gov/sscc/gangc/MVM.html
 
 Running: ~1~
 Once the 3dMVM command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called MVM.txt, and execute it with the 
 following  (assuming on tc shell),
 
 tcsh -x MVM.txt &
 
 or,
 
 tcsh -x MVM.txt > diary.txt &
 tcsh -x MVM.txt |& tee diary.txt &

 The advantage of the latter command is that the progression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 
 Thanks to the R community, Henrik Singmann, and Helios de Rosario for the 
 strong technical support.'

   ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- 3 between-subjects and 2 within-subject variables: ~2~
   Three between-subjects (genotype, sex, and scanner) and two within-subject 
   (condition and emotion) variables.

   3dMVM  -prefix Example1 -jobs 4            \\
          -bsVars  'genotype*sex+scanner'      \\
          -wsVars \"condition*emotion\"         \\
          -SS_type 2                          \\
          -num_glt 14                         \\
          -gltLabel 1 face_pos_vs_neg -gltCode  1 'condition : 1*face emotion : 1*pos -1*neg'            \\
          -gltLabel 2 face_emot_vs_neu -gltCode 2 'condition : 1*face emotion : 1*pos +1*neg -2*neu'     \\
          -gltLabel 3 sex_by_condition_interaction -gltCode 3 'sex : 1*male -1*female condition : 1*face -1*house' \\
          -gltLabel 4 3way_interaction -gltCode 4 'sex : 1*male -1*female condition : 1*face -1*house emotion : 1*pos -1*neg' \\
          ...            
          -num_glf 3                         \\
          -glfLabel 1 male_condXEmo -glfCode 1 'sex : 1*male condition : 1*face -1*house emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -glfLabel 2 face_sexXEmo -glfCode 2 'sex : 1*male -1*female condition : 1*face emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -glfLabel 3 face_sex2Emo -glfCode 3 'sex : 1*male & 1*female condition : 1*face emotion : 1*pos -1*neg & 1*pos -1*neu' \\
          -dataTable                                                                                     \\
          Subj  genotype   sex    scanner  condition   emotion   InputFile                               \\
          s1    TT         male   scan1   face        pos       s1+tlrc\'[face_pos_beta]\'                 \\
          s1    TT         male   scan1   face        neg       s1+tlrc\'[face_neg_beta]\'                 \\
          s1    TT         male   scan1   face        neu       s1+tlrc\'[face_neu_beta]\'                 \\
          s1    TT         male   scan1   house       pos       s1+tlrc\'[house_pos_beta]\'                \\
          ...
          s68   TN         female scan2   house       pos       s68+tlrc\'[face_pos_beta]\'                \\
          s68   TN         female scan2   house       neg       s68+tlrc\'[face_neg_beta]\'                \\
          s68   TN         female scan2   house       neu       s68+tlrc\'[house_pos_beta]\'                    

   NOTE: ~3~
          1) The 3rd GLT is for the 2-way 2 x 2 interaction between sex and condition, which
          is essentially a t-test (or one degree of freedom for the numerator of F-statistic).
          Multiple degrees of freedom for the numerator of F-statistic can be obtained through
          option -glfCode (see GLFs #1, #2, and #3).
          2) Similarly, the 4th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
          interaction between the three factors because 'emotion' has three levels. The F-test for
          the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.
          3) The two GLFs showcase the user how to specify sub-interactions.
          5) Option '-SS_type 2' specifies the hierarchial type for the sume of squares in the
          omnibus F-statistics in the output. See more details in the help.\n"   
      
   ex2 <-
"--------------------------------
Example 2 --- 2 between-subjects, 1 within-subject, 2 quantitative variables: ~2~

   Two between-subjects (genotype and sex), one within-subject
   (emotion) factor, plus two quantitative variables (age and IQ).

   3dMVM -prefix Example2 -jobs 24        \\
          -bsVars  \"genotype*sex+age+IQ\"  \\
          -wsVars emotion                \\
          -qVars  \"age,IQ\"               \\
          -qVarCenters '25,105'          \\
          -num_glt 10                    \\
          -gltLabel 1 pos_F_vs_M   -gltCode 1 'sex : 1*female -1*male emotion : 1*pos'          \\
          -gltLabel 2 age_pos_vs_neg -gltCode 2 'emotion : 1*pos -1*neg age :'                  \\
          -gltLabel 3 age_pos_vs_neg -gltCode 3 'emotion : 1*pos -1*neg age : 5'                \\
          -gltLabel 4 genotype_by_sex -gltCode 4 'genotype : 1*TT -1*NN sex : 1*male -1*female' \\
          -gltLabel 5 genotype_by_sex_emotion -gltCode 5 'genotype : 1*TT -1*NN sex : 1*male -1*female emotion : 1*pos -1*neg' \\
          ...            
          -dataTable                                                                   \\
          Subj  genotype  sex    age  IQ     emotion   InputFile                       \\
          s1    TT         male   24   107    pos       s1+tlrc\'[pos_beta]\'            \\
          s1    TT         male   24   107    neg       s1+tlrc\'[neg_beta]\'            \\
          s1    TT         male   24   107    neu       s1+tlrc\'[neu_beta]\'            \\
          ... 
          s63   NN         female 29   110    pos       s63+tlrc\'[pos_beta]\'           \\
          s63   NN         female 29   110    neg       s63+tlrc\'[neg_beta]\'           \\
          s63   NN         female 29   110    neu       s63+tlrc\'[neu_beta]\'         

   NOTE: ~3~
          1) The 2nd GLT shows the age effect (slope) while the 3rd GLT reveals the contrast
          between the emotions at the age of 30 (5 above the center). On the other hand,
          all the other GLTs (1st, 4th, and 5th) should be interpreted at the center Age
          value, 25 year old.
          2) The 4rd GLT is for the 2-way 2 x 2 interaction between genotype and sex, which
          is essentially a t-test (or one degree of freedom for the numerator of F-statistic).
          Multiple degrees of freedom for the numerator of F-statistic is currently unavailable.
          3) Similarly, the 5th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
          interaction between the three factors because 'emotion' has three levels. The F-test for
          the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.\n"
     
   ex3 <-
"---------------------------------
Example 3 --- Getting more complicated: ~2~

   BOLD response was modeled with multiple basis functions at individual
   subject level. In addition, there are one between-subjects (Group) and one within-
   subject (Condition) variable. Furthermore, the variable corresponding to the number 
   of basis functions, Time, is also a within-subject variable. In the end, the F-
   statistics for the interactions of Group:Condition:Time, Group:Time, and 
   Condition:Time are of specific interest. And these interactions can be further
   explored with GLTs in 3dMVM.

   3dMVM -prefix Example3 -jobs 12   \\
         -bsVars Group               \\
         -wsVars 'Condition*Time'   \\
         -num_glt 32                \\
         -gltLabel 1 old_t0 -gltCode 1 'Group : 1*old Time : 1*t0' \\
         -gltLabel 2 old_t1 -gltCode 2 'Group : 1*old Time : 1*t1' \\
         -gltLabel 3 old_t2 -gltCode 3 'Group : 1*old Time : 1*t2' \\
         -gltLabel 4 old_t3 -gltCode 4 'Group : 1*old Time : 1*t3' \\
         -gltLabel 5 yng_t0 -gltCode 5 'Group : 1*yng Time : 1*t0' \\
         -gltLabel 6 yng_t1 -gltCode 6 'Group : 1*yng Time : 1*t1' \\
         -gltLabel 7 yng_t2 -gltCode 7 'Group : 1*yng Time : 1*t2' \\
         -gltLabel 8 yng_t3 -gltCode 8 'Group : 1*yng Time : 1*t3' \\
         ...
         -gltLabel 17 old_face_t0 -gltCode 17 'Group : 1*old Condition : 1*face Time : 1*t0' \\
         -gltLabel 18 old_face_t1 -gltCode 18 'Group : 1*old Condition : 1*face Time : 1*t1' \\
         -gltLabel 19 old_face_t2 -gltCode 19 'Group : 1*old Condition : 1*face Time : 1*t2' \\
         -gltLabel 20 old_face_t3 -gltCode 20 'Group : 1*old Condition : 1*face Time : 1*t3' \\
         ...         
         -dataTable                                            \\
         Subj  Group  Condition Time InputFile                 \\
         s1    old    face      t0   s1+tlrc\'[face#0_beta]\'    \\
         s1    old    face      t1   s1+tlrc\'[face#1_beta]\'    \\
         s1    old    face      t2   s1+tlrc\'[face#2_beta]\'    \\
         s1    old    face      t3   s1+tlrc\'[face#3_beta]\'    \\
         ...
         s40   yng    house     t0   s40+tlrc\'[house#0_beta]\'  \\
         s40   yng    house     t1   s40+tlrc\'[house#1_beta]\'  \\
         s40   yng    house     t2   s40+tlrc\'[house#2_beta]\'  \\
         s40   yng    house     t3   s40+tlrc\'[house#3_beta]\'      

   NOTE: ~3~
          The model for the analysis can also be set up as and is equivalent to 
          'Group*Condition*Time'.
   
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
   
#Change command line arguments into an options list
read.MVM.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output file name. For AFNI format, provide prefix only,",
   "         with no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached, while file name for surface data is expected",
   "         to end with .niml.dset. The sub-brick labeled with the '(Intercept)',",
   "         if present, should be interpreted as the overall average",
   "         across factor levels at the center value of each covariate.\n", sep = '\n'
                     ) ),

      '-resid' = apl(n = 1, d = NA,  h = paste(
   "-resid PREFIX: Output file name for the residuals. For AFNI format, provide",
   "         prefix only without view+suffix. Filename for NIfTI format should",
   "         have .nii attached, while file name for surface data is expected",
   "         to end with .niml.dset.\n", sep = '\n'
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

      '-verb' = apl(n = 1, d = 1, h = paste(
   "-verb VERB: Speicify ver level.\n", sep = '\n'
                     ) ),

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: This option will phase out at some point. So use -bsVars",
   "         instead. Specify the fixed effects for between-subjects factors ",
   "         and quantitative variables. When no between-subject factors",
   "         are present, simply put 1 for FORMULA. The expression FORMULA",
   "         with more than one variable has to be surrounded within (single or double)",
   "         quotes. Variable names in the formula should be consistent with",
   "         the ones used in the header of -dataTable. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. The effects of within-subject",
   "         factors, if present under -wsVars are automatically assumed",
   "         to interact with the ones specified here. Subject as a variable",
   "         should not occur in the model specification here.\n", sep = '\n'
             ) ),

      '-bsVars' = apl(n = 1, d = 1, h = paste(
   "-bsVars FORMULA: Specify the fixed effects for between-subjects factors ",
   "         and quantitative variables. When no between-subject factors",
   "         are present, simply put 1 for FORMULA. The expression FORMULA",
   "         with more than one variable has to be surrounded within (single or",
   "         double) quotes. No spaces are allowed in the FORMULA expression.",
   "         Variable names in the formula should be consistent with the ones",
   "         used in the header underneath -dataTable. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. The effects of within-subject",
   "         factors, if present under -wsVars are automatically assumed",
   "         to interact with the ones specified here. Subject as a variable",
   "         should not occur in the model specification here.\n", sep = '\n'
             ) ),

      '-wsVars' = apl(n=c(1,100), d=NA, h = paste(
   "-wsVars FORMULA: Within-subject factors, if present, have to be listed",
   "         here; otherwise the program will choke. If no within-subject ",
   "         exists, don't include this option in the script. Coding for",
   "         additive effects and interactions is the same as in -bsVars. The",
   "         FORMULA with more than one variable has to be surrounded ",
   "         within (single or double) quotes. Note that the within-subject",
   "         variables are assumed to interact with those between-subjects",
   "         variables specified under -bsVars. The hemodynamic response",
   "         time course are better modeled as simultaneous outcomes through",
   "         option -mVar, and not as the levels of a within-subject factor.",
   "         The varialbes under -wsVars and -mVar are exclusive from each",
   "         other.\n", sep = '\n') ),

       '-wsMVT' = apl(n=0, h = paste(
   "-wsMVT: By default 3dMVM provides an F-stat through univariate testing (UVT)",
   "         for each effect that involves a within-subject factor. If at least",
   "         one within-subject factor is involved in the model, option -wsMVT",
   "         provides within-subject multivariate testing for any effect",
   "         associated with a within-subject variable. The testing strategy is",
   "         different from the conventional univariate GLM, see more details in",
   "         Chen et al. (2014), Applications of Multivariate Modeling to",
   "         Neuroimaging Group Analysis: A Comprehensive Alternative to",
   "         Univariate General Linear Model. NeuroImage 99, 571-588. If",
   "         all the within-subject factors have two levels, the multivariate",
   "         testing would render the same results as the univariate version.",
   "         So use the option only if at least one within-subject factor has",
   "         more than two levels. The F-statistics from the multivariate",
   "         testing are labeled with -wsMVT- in the sub-brick names. Note that",
   "         the conventional univariate F-statistics are automatically included",
   "         in the beginning of the output regardless the presence of this option.\n", sep='\n')),

       '-wsE2' = apl(n=0, h = paste(
   "-wsE2: If at least one within-subject factor is involved in the model, any",
   "         omnibus F-test associated with a within-subject factor is assessed",
   "         with both univariate and within-subject multivariate tests. Use",
   "         the option only if at least one within-subject factor has more",
   "         than two levels. By default 3dMVM provides an F-stat through the",
   "         univariate testing (UVT) method for each effect that involves a",
   "         within-subject factor. With option -wsE2 UVT is combined with the",
   "         within-subject multivariate approach, and the merged result remains",
   "         the same as UVT most of the time (or in most brain regions), but",
   "         occasionally it may be more powerful.\n", sep='\n')),

       '-mvE5' = apl(n=0, h = paste(
   "", sep='\n')),

       '-mvE5a' = apl(n=0, h = paste(
   "", sep='\n')),

       '-parSubset' = apl(n=c(1,100), d=NA, h = paste(
   "", sep='\n')),

      '-mVar' = apl(n=c(1,100), d=NA, h = paste(
   "-mVar variable: With this option, the levels of the within-subject factor",
   "         will be treated as simultaneous variables in a multivariate model.",
   "         For example, when the hemodynamic response time course is modeled",
   "         through multiple basis functions such as TENT, TENTzero, CSPLIN,",
   "         CSPLINzero, SPMG2/3, etc., the effect estimates at the multiple",
   "         time points can be treated as simultaneous response variables in",
   "         a multivariate model. Only one within-subject variable is allowed",
   "         currently under -mVar. In addition, in the presence of -mVar, no",
   "         other within-subject factors should be included. If modeling",
   "         extra within-subject factors with -mVar is desirable, consider",
   "         flattening such factors; that is, perform multiple analyses",
   "         at each level or their contrasts of the factor. The output",
   "         for multivariate testing are labeled with -MV0- in the sub-brick",
   "         names.\n", sep = '\n') ),

       '-GES' = apl(n=0, h = paste(
   "-GES: As an analog of the determination coefficient R^2 in multiple",
   "         regression, generalized eta-squared (GES) provides a measure",
   "         of effect size for each F-stat in ANOVA or general GLM, and",
   "         renders a similar interpretation: proportion of variance in",
   "         the response variable by the explanatory variable on hand.",
   "         It ranges within [0, 1]. Notice that this option is only",
   "         available with R version 3.2 and afex version 0.14 or later.\n", sep='\n')),
#  option -GES would not work with -mvE5, but will with -mvE5a

       '-SC' = apl(n=0, h = paste(
   "-SC: If a within-subject factor with more than *two* levels is",
   "         involved in the model, 3dMVM automatically provides the",
   "         F-statistics for main and interaction effects with",
   "         sphericity assumption. If the assumption is violated,",
   "         the F-statistics could be inflated to some extent. This option,",
   "         will enable 3dMVM to additionally output the F-statistics of",
   "         sphericity correction for main and interaction effects, which",
   "         are labeled with -SC- in the sub-brick names.",
   "         NOTE: this option should be used only when at least one",
   "         within-subject factor has more than TWO levesl.\n", sep='\n')),

       '-robust' = apl(n=0, h = paste(
   "-robust: Robust regression is performed so that outliers can be",
   "         reasonably handled through MM-estimation. Currently it",
   "         only works without involving any within-subject factors.",
   "         That is, anything that can be done with 3dttest++ could",
   "         be analyzed through robust regression here (except for",
   "         one-sample which can be added later one if requested).",
   "         pairwise comparisons can be performed by providing",
   "         contrast from each subject as input). Post hoc F-tests",
   "         through option -glfCode are currently not available with",
   "         robust regression. This option requires that the user",
   "         install R package robustbase.\n", sep='\n')),

      '-dbgArgs' = apl(n=0, h = paste(
   "-dbgArgs: This option will enable R to save the parameters in a",
   "         file called .3dMVM.dbg.AFNI.args in the current directory",
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
   "         3) Within-subject covariates vary across the levels of a",
   "         within-subject factor, and can be analyzed with 3dLME,",
   "         but not 3dMVM.\n",
             sep = '\n'
             ) ),

      '-vVars' = apl(n=c(1,100), d=NA, h = paste(
   "-vVars variable_list: Identify voxel-wise covariates with this option.",
   "         Currently one voxel-wise covariate is allowed only, but this",
   "         may change if demand occurs...",
  # "         The list with more than one variable has to be",
  # "         separated with comma (,) without any other characters such as",
  # "         spaces and should be surrounded within (single or double) quotes.",
  # "         For example, -qVars \"Var1,Var2\".
   "         By default mean centering is performed voxel-wise across all",
   "         subjects. Alternatively centering can be specified through a",
   "         global value under -vVarsCenters. If the voxel-wise covariates",
   "         have already been centered, set the centers at 0 with -vVarsCenters.\n",
             sep = '\n'
             ) ),

     '-qvarcenters' = apl(n=1, d=NA, h = paste(
   "-qVarCenters VALUES: Specify centering values for quantitative variables",
   "         identified under -qVars. Multiple centers are separated by ",
   "         commas (,) within (single or double) quotes. The order of the",
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

     '-SS_type' = apl(n=1, d=0, h = paste(
   "-SS_type 2/3: Specify the type for the sums of squares for the omnibus",
   "         F-statistics. Type 2 is hierarchical or partially sequential",
   "         while type 3 is marginal. Type 2 is more powerful if all the",
   "         relevant higher-oder interactions do not exist. The default",
   "         is 3. The controversy surrounding the different types can be",
   "         found at https://afni.nimh.nih.gov/sscc/gangc/SS.html\n", sep = '\n'
             ) ),

     '-num_glt' = apl(n=1, d=0, h = paste(
   "-num_glt NUMBER: Specify the number of general linear t-tests (GLTs). A glt",
   "         is a linear combination of a factor levels. See details in ",
   "         -gltCode.\n", sep = '\n') ),

     '-num_glf' = apl(n=1, d=0, h = paste(
   "-num_glf NUMBER: Specify the number of general linear F-tests (GLFs). A glf",
   "         involves the union of two or more simple tests. See details in",
   "         -glfCode.\n", sep = '\n') ),
                     
     '-gltLabel' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltLabel k label: Specify the label for the k-th general linear t-test",
   "         (GLT). A symbolic coding for the GLT is assumed to follow with",
   "         each -gltLabel.\n", sep = '\n'
                     ) ),

     '-glfLabel' = apl(n=c(1,1000), d=NA, h = paste(
   "-glfLabel k label: Specify the label for the k-th general linear F-test",
   "         (GLF). A symbolic coding for the GLF is assumed to follow with",
   "         each -glfLabel.\n", sep = '\n'
                     ) ),

     '-gltCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-gltCode k CODING: Specify the k-th general linear t-test (GLT) through a",
   "         weighted combination among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the following",
   "         'Group : 1*A -1*B & 1*A -1*C' tests the main effect across the 3",
   "         groups A, B, and C. Important Note: this option is only valid for",
   "         a hypothesis that involves between-subjects variables. For an F-test",
   "         involving a within-subject variable, use -glfCode with 3dLME instead.\n",
   "         valence.\n",
   "         NOTE:\n",
   "         1) The weights for a variable do not have to add up to 0.\n",   
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate unless the covariate",
   "         value is specified as, for example, 'Group : 1*Old Age : 2', where",
   "         the Old Group is tested at the Age of 2 above the center.\n",
   "         3) The effect for a quantitative variable (or slope) can be specified",
   "         with, for example, 'Group : 1*Old Age : ', or ",
   "         'Group : 1*Old - 1*Young Age : '\n", 
   "         4) When a quantitative covariate is involved in the model, the\n",
   "         absence of the covariate in the GLT coding means that the test\n",
   "         will be performed at the center value of the covarite. However,\n",
   "         with a value after the colon, the effect would be tested at the\n",
   "         value of 2 above the center. For example, 'Group : 1*Old Age : 2'",
   "         shows the effect of the Old Group at the age of 2 years older than\n",
   "         the center age. On the other hand, 'Group : 1*Old' tests for the\n",
   "         effect of the Old Group at the center age.\n",
   "         5) The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLT.\n",
   "         6) The appearance of a categorical variable has to be followed",
   "         by the linear combination of its levels. Only a quantitative",
   "         is allowed to have a dangling coding as seen in 'Age :'\n",
   "         7) Some special interaction effects can be tested under -gltCode",
   "         when the numerical DF is 1. For example, 'Group : 1*Old - 1*Young",
   "         Condition : 1*House -1*Face Emotion : 1*positive'. Even though",
   "         this is typically an F-test that can be coded under -glfCode, it",
   "         can be tested under -gltCode as well. An extra bonus is that the",
   "         t-test shows the directionality while F-test does not.\n",
             sep = '\n'
             ) ),

     '-glfCode' = apl(n=c(1,1000), d=NA, h = paste(
   "-glfCode k CODING: Specify the k-th general linear F-test (GLF) through a",
   "         weighted combination among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the coding",
   "         'Condition : 1*A -1*B & 1*A -1*C Emotion : 1:pos' tests the main",
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
   "         [] within quotes) specified with a number or label. Unequal number of",
   "         subjects across groups is allowed, but situations with missing data",
   "         for a within-subject factor are better handled with 3dLME.\n",
   "         3) It is fine to have variables (or columns) in the table that are",
   "         not modeled in the analysis.\n",
   "         4) The context of the table can be saved as a separate file, e.g.,",
   "         called table.txt. Do not forget to include a backslash at the end of",
   "         each row. In the script specify the data with '-dataTable @table.txt'.",
   "         Do NOT put any quotes around the square brackets for each sub-brick!",
   "         Otherwise, the program cannot properly read the files for some reason.",
   "         This option is useful: (a) when there are many input files so that",
   "         the program complains with an 'Arg list too long' error; (b) when",
   "         you want to try different models with the same dataset (see 3) above).\n",
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
                     
   ops <- parse.AFNI.args(args, params, other_ok=FALSE)
   if (verb) show.AFNI.args(ops, verb=0, hstr='')
   if (is.null(ops)) 
      errex.AFNI('Error parsing arguments. See 3dMVM -help for details.')

   #Parse dems options
   #initialize with defaults
      com_history<-AFNI.command.history(ExecName, args,NULL)
      lop <- AFNI.new.options.list(history = com_history, parsed_args = ops)
      lop$nNodes <- 1
      lop$model  <- 1
      lop$maskFN <- NA

      lop$wsVars <- NA
      lop$mVar   <- NA
      lop$qVars  <- NA
      lop$vVars  <- NULL
      lop$vQV    <- NA
      lop$qVarCenters <- NA
      lop$vVarCenters <- NA
      lop$SS_type     <- 3
      lop$num_glt     <- 0
      lop$gltLabel    <- NULL
      lop$gltCode     <- NULL
      lop$num_glf     <- 0
      lop$glfLabel    <- NULL
      lop$glfCode     <- NULL
      lop$dataTable   <- NULL

      lop$GES    <- FALSE  # generalized eta-squared
      lop$SC     <- FALSE
      lop$robust     <- FALSE
      lop$wsMVT  <- FALSE
      lop$wsE2   <- FALSE  # combining UVT and wsMVT, and then replacing UVT: only applicable for an
                           # effect associated with a within-subject factor with more than 2 levels
      lop$mvE5    <- FALSE # combining 5 effects: 1st half UVT (AUC), 2nd half UVT (EXC), 1st half
                           # wsMVT (AUC), 2nd half wsMVT (EXC), and MVT: assuming ONLY one within-subject
                           # factor (e.g., effects from basis functions). Those 5 individual results 
                           # are not output (cf., lop$mvE5a).
      lop$mvE5a   <- FALSE # combining 5 effects: 1st half UVT (AUC), 2nd half UVT (EXC), 1st half
                           # wsMVT (AUC), 2nd half wsMVT (EXC), and MVT: assuming ONLY one within-subject
                           # factor (e.g., effects from basis functions). Results added (or appended) to 
                           # others (cf., lop$mvE5a).
      lop$parSubset <- NA  # parameter subset from option -matrPar for DTI data analysis
      lop$dbgArgs  <- FALSE # for debugging purpose 
      lop$iometh <- 'clib'
      lop$verb   <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             resid  = lop$resid  <- pprefix.AFNI.name(ops[[i]]),
             mask = lop$maskFN <- ops[[i]],
             jobs   = lop$nNodes <- ops[[i]],
             verb = lop$verb  <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             bsVars = lop$model  <- ops[[i]],
             wsVars = lop$wsVars  <- ops[[i]],
             mVar = lop$mVar  <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             vVars  = lop$vVars <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             vVarCenters = lop$vVarCenters <- ops[[i]],
             SS_type = lop$SS_type <- ops[[i]],
             num_glt = lop$num_glt <- ops[[i]],
             gltLabel = lop$gltLabel <- ops[[i]],
             gltCode  = lop$gltCode <- ops[[i]],
             num_glf = lop$num_glf <- ops[[i]],
             glfLabel = lop$glfLabel <- ops[[i]],
             glfCode  = lop$glfCode <- ops[[i]],
             dataTable  = lop$dataTable <- dataTable.AFNI.parse(ops[[i]]),
             parSubset  = lop$parSubset <- ops[[i]],
            
             help   = help.MVM.opts(params, adieu=TRUE),
             GES    = lop$GES    <- TRUE,
             SC     = lop$SC     <- TRUE,
             robust = lop$robust <- TRUE,
             wsMVT  = lop$wsMVT  <- TRUE,
             wsE2   = lop$wsE2   <- TRUE,
             mvE5   = lop$mvE5   <- TRUE,
             mvE5a  = lop$mvE5a  <- TRUE,
             cio    = lop$iometh <- 'clib',
             Rio    = lop$iometh <- 'Rlib',
             dbgArgs = lop$dbgArgs <- TRUE
             )
   }

   return(lop)
}# end of read.MVM.opts.batch
                                               
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
      for(ii in 1:nvar) {
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

# test: pairwise = NULL
# gltConstr(clist[[1]], lop$dataStr)
# QVpos <- which(lop$gltCode[[n]] %in% lop$QV)
# gltConstr(lop$gltCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)

# construct a glf list for testFactors in phia
# NEED to solve the problem when a quantitative variable is tested alone:
# with pairwise = NULL!!!                                                
glfConstr <- function(cStr, dataStr) {
   pos <- which(cStr==":")
   vars  <- cStr[pos-1]  # factors to be specified
   nvar <- length(vars)
   pos <- c(pos, length(cStr)+2) # add an artificial one for convenient usage below
   varsOK <- vars %in% colnames(dataStr)
   glfList <- vector('list', nvar)
   if(all(varsOK)) {
      #glfList <- vector('list', nvar)
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
	       #} else errex.AFNI(paste("Incorrect level coding in variable", vars[ii], ": ", lvlInv[which(!lvlOK)], " \n   "))
               } else glfList <- NULL
           }
         #}
      }
      return(glfList)
   #} else errex.AFNI(paste("Incorrect variable name in GLF coding: ", vars[which(!varsOK)], " \n   "))
   } else glfList <- NULL
}

# glfConstr(lop$glfCode[[1]], lop$dataStr)
                                                
#Change options list to 3dMVM variable list 
process.MVM.opts <- function (lop, verb = 0) {
   if(is.null(lop$outFN)) errex.AFNI(c("Output filename not specified! Add filename with -prefix.\n"))
   an <- parse.AFNI.name(lop$outFN)
   if(an$type == "NIML") {
      if(!lop$overwrite && file.exists(lop$outFN)) errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
   } else if(!lop$overwrite && (
                file.exists(paste(lop$outFN,"+tlrc.HEAD", sep="")) ||
                file.exists(paste(lop$outFN,"+tlrc.BRIK", sep="")) ||
                file.exists(paste(lop$outFN,"+orig.HEAD", sep="")) ||
                file.exists(paste(lop$outFN,"+orig.BRIK", sep=""))) ) {
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
         return(NULL)
   }      
   
   #Make sure new io must be used with anything but BRIK format
   an <- parse.AFNI.name(lop$outFN)
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

   if(!is.na(lop$qVars[1])) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]
   if(!is.null(lop$vVars[1])) lop$vQV <- strsplit(lop$vVars, '\\,')[[1]]

   if(!is.na(lop$parSubset[1])) lop$parSubsetVector <- strsplit(lop$parSubset, '\\,')[[1]]

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
   # in case the user misspells
   #wd <- which(lop$dataTable == "InputFile" | lop$dataTable == "Inputfile")
   wd <- which(lop$dataTable %in% respVar)
   hi <- len / wd - 1
   #browser()
   if(len %% wd != 0)
      errex.AFNI(paste('The content under -dataTable is not rectangular !', len, wd)) else {
      lop$dataStr <- NULL
      for(ii in 1:wd) 
         lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      # wow, terrible mistake here with as.numeric(lop$dataStr[,jj])
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
      if(!is.na(lop$qVars[1])) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      # or if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(levels(lop$dataStr[,jj]))[as.integer(lop$dataStr[,jj])]
      if(!is.null(lop$vVars[1])) for(jj in lop$vQV) lop$dataStr[,jj] <- as.character(lop$dataStr[,jj])
   }
   
   # set the covariate default values at their centers
   #lop$covVal <- NULL
   #if(length(lop$QV)>0) {
   #   lop$covVal <- rep(0, length(lop$QV))
   #   names(lop$covVal) <- lop$QV
   #}
   
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

   #if (lop$num_glt > 0) {
   #   lop$gltList    <- vector('list', lop$num_glt)
   #   lop$slpList    <- vector('list', lop$num_glt)
   #   lop$covValList <- vector('list', lop$num_glt)
   #   for (n in 1:lop$num_glt) { # assuming each GLT has one slope involved and placed last
   #      if(length(lop$QV)==0) lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr) else {
   #      if((length(lop$QV)>0) & any(lop$QV %in% lop$gltCode[[n]])) {
   #         QVpos <- which(lop$gltCode[[n]] %in% lop$QV)
   #         if(is.na(lop$gltCode[[n]][QVpos+2])) { # test for covariate effect
   #            if(QVpos==1) lop$gltList[[n]] <- NA else
   #               lop$gltList[[n]] <-gltConstr(lop$gltCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
   #            lop$slpList[[n]]    <- lop$gltCode[[n]][QVpos]
   #         } else { # effect at a specific covariate value
   #           if(QVpos==1) lop$gltList[[n]] <- NA else
   #               lop$gltList[[n]] <-gltConstr(lop$gltCode[[n]][-(QVpos:(QVpos+2))], lop$dataStr)
   #            lop$covValList[[n]] <- as.numeric(lop$gltCode[[n]][QVpos+2])
   #            names(lop$covValList[[n]]) <- lop$gltCode[[n]][QVpos]
   #         } # if(is.na(lop$gltCode[[n]][QVpos+2]))
   #      } else if(!is.na(lop$vVars) & any(lop$vQV %in% lop$gltCode[[n]])) { # voxel-wise covariate
   #         vQVpos <- which(lop$gltCode[[n]] %in% lop$vQV)
   #         if(is.na(lop$gltCode[[n]][vQVpos+2])) { # test for covariate effect
   #            if(vQVpos==1) lop$gltList[[n]] <- NA else
   #               lop$gltList[[n]] <-gltConstr(lop$gltCode[[n]][-c(vQVpos, vQVpos+1)], lop$dataStr)
   #            lop$slpList[[n]]    <- lop$gltCode[[n]][vQVpos]
   #         } else { # effect at a specific covariate value
   #           if(vQVpos==1) lop$gltList[[n]] <- NA else
   #               lop$gltList[[n]] <-gltConstr(lop$gltCode[[n]][-(vQVpos:(vQVpos+2))], lop$dataStr)
   #            lop$covValList[[n]] <- as.numeric(lop$gltCode[[n]][vQVpos+2])
   #            names(lop$covValList[[n]]) <- lop$gltCode[[n]][vQVpos]
   #        } # if(is.na(lop$gltCode[[n]][vQVpos+2]))
   #      } else lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr) # if((length(lop$QV)>0) & any(lop$QV %in% lop$gltCode[[n]]))
   #   }
   #   }
   #}
   #
   #if (lop$num_glf > 0) {
   #   lop$glfList    <- vector('list', lop$num_glf)
   #   lop$slpListF    <- vector('list', lop$num_glf)
   #   lop$covValListF <- vector('list', lop$num_glf)
   #   for (n in 1:lop$num_glf) { # assuming each GLT has one slope involved and placed last
   #      if(length(lop$QV)==0) lop$glfList[[n]] <- glfConstr(lop$glfCode[[n]], lop$dataStr) else {
   #      if((length(lop$QV)>0) & any(lop$QV %in% lop$glfCode[[n]])) {
   #         QVpos <- which(lop$glfCode[[n]] %in% lop$QV)
   #         if(is.na(lop$glfCode[[n]][QVpos+2])) { # test for covariate effect
   #            if(QVpos==1) lop$glfList[[n]] <- NA else
   #               lop$glfList[[n]] <-glfConstr(lop$glfCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
   #            lop$slpListF[[n]]    <- lop$glfCode[[n]][QVpos]
   #         } else { # effect at a specific covariate value
   #           if(QVpos==1) lop$glfList[[n]] <- NA else
   #               lop$glfList[[n]] <-glfConstr(lop$glfCode[[n]][-(QVpos:(QVpos+2))], lop$dataStr)
   #            lop$covValListF[[n]] <- as.numeric(lop$glfCode[[n]][QVpos+2])
   #            names(lop$covValListF[[n]]) <- lop$glfCode[[n]][QVpos]
   #         } # if(is.na(lop$gltCode[[n]][QVpos+2]))
   #      } else if(!is.na(lop$vVars) & any(lop$vQV %in% lop$glfCode[[n]])) { # voxel-wise covariate
   #         vQVpos <- which(lop$glfCode[[n]] %in% lop$vQV)
   #         if(is.na(lop$glfCode[[n]][vQVpos+2])) { # test for covariate effect
   #            if(vQVpos==1) lop$glfList[[n]] <- NA else
   #               lop$glfList[[n]] <-glfConstr(lop$glfCode[[n]][-c(vQVpos, vQVpos+1)], lop$dataStr)
   #            lop$slpListF[[n]]    <- lop$glfCode[[n]][vQVpos]
   #         } else { # effect at a specific covariate value
   #           if(vQVpos==1) lop$glfList[[n]] <- NA else
   #               lop$glfList[[n]] <-glfConstr(lop$glfCode[[n]][-(vQVpos:(vQVpos+2))], lop$dataStr)
   #            lop$covValListF[[n]] <- as.numeric(lop$glfCode[[n]][vQVpos+2])
   #            names(lop$covValListF[[n]]) <- lop$glfCode[[n]][vQVpos]
   #        } # if(is.na(lop$gltCode[[n]][vQVpos+2]))
   #      } else lop$glfList[[n]] <- glfConstr(lop$glfCode[[n]], lop$dataStr) # if((length(lop$QV)>0) & any(lop$QV %in% lop$gltCode[[n]]))
   #   }
   #   }
   #}
 
   if(lop$iometh == 'Rlib') {
      lop$outFN <- paste(lop$outFN, "+tlrc", sep="")
      if(!is.null(lop$resid)) lop$resid <- paste(lop$resid, "+tlrc", sep="") 
   } else {
      an <- parse.AFNI.name(lop$outFN)
      if(an$type == "BRIK" && an$ext == "" && is.na(an$view))
         lop$outFN <- paste(lop$outFN, "+tlrc", sep="")      
      if (!lop$overwrite && (
            exists.AFNI.name(lop$outFN) || 
            exists.AFNI.name(modify.AFNI.name(lop$outFN,"view","+tlrc"))))
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
# process.MVM.opts(lop, verb = lop$verb)                                               

#################################################################################
################# MVM Computation functions ##############################
#################################################################################

# used only in read.MVM.opts.from.file
scanLine <- function(file, lnNo=1, marker="\\:")
   unlist(strsplit(unlist(scan(file, what= list(""), skip=lnNo-1, 
   strip.white=TRUE, nline=1)), marker))[2]

# function to obtain multivariate ANOVA
# may totally abandon this and switch to linearHypothesis
# Pillai test only now
# does this work for multiple between-subjects variables (factors and quantitative variables)?????????
#maov <- function(mvfm)  # Pillai test with type = 3, need to remove the intercept: -1 below
#   return(stats:::Pillai(Re(eigen(qr.coef(qr(mvfm$SSPE), mvfm$SSP[[-1]]), symmetric = FALSE)$values), mvfm$df[-1], mvfm$error.df))

# general linear test for between-subjects variables only
# takes components from Anova(fm$lm, type=3, test='Pillai') as input  
maov <- function(SSPE, SSP, DF, error.DF)  # Pillai test with type = 3
   return(stats:::Pillai(Re(eigen(qr.coef(qr(SSPE), SSP), symmetric = FALSE)$values), DF, error.DF))

# Combines 3 types of tests, and outputs the minimum p-values among 5 tests:
# 2 from UVT, 2 from within-subject MVT, and 1 from MVT.
# takes model object from aov.car from afex as input: assuming that
# model fitting with afex: ONE within-subject factor ONLY!
mvCom5 <- function(fm, nF_mvE5) {
   if(lop$afex_new) uvfm <- tryCatch(summary(fm), error=function(e) NULL) else  # univariate model
      uvfm <- tryCatch(univ(fm$Anova), error=function(e) NULL)
   uvP  <- rep(1, 2*nF_mvE5)
   #p_wsmvt <- rep(1, nF_mvE5)
   p_wsmvt <- rep(1, 2*nF_mvE5)
   p_mvt   <- rep(1, nF_mvE5)
   if(!is.null(uvfm)) {
   #nTerms <- nrow(uvfm$anova)  # totaly number of effect estimates
   #outTerms <- nTerms/2        # half of them
   # UVT p-values
      if(lop$afex_new) uvP <- uvfm$univariate.test[,'Pr(>F)'] else uvP <- uvfm$anova[,'Pr(>F)'] # p-values for UVT
      #if(lop$afex_new) {
      #   uvfm0 <- anova(fm, intercept=T)
      #   uvP <- uvfm0[,'Pr(>F)']
      #   names(uvP) <- row.names(uvfm0)
      #} else uvP <- uvfm$anova[,'Pr(>F)'] # p-values for UVT
   # within-subject MVT: one set
   #p_wsmvt <- rep(1, nTerms)   # initiation for within-subject MVT
   #   for(ii in 1:nF_mvE4) {
   #      jj <- nF_mvE4 + ii
   #      wsmvt <- tryCatch(maov(fm$Anova$SSPE[[jj]], fm$Anova$SSP[[jj]], fm$Anova$df[jj], fm$Anova$error.df), error=function(e) NULL)
         #p-value for upper F
   #      if(!is.null(wsmvt)) p_wsmvt[ii] <- pf(wsmvt[2], wsmvt[3], wsmvt[4], lower.tail = FALSE)
   #   }
      for(ii in 1:(2*nF_mvE5)) {
         wsmvt <- tryCatch(maov(fm$Anova$SSPE[[ii]], fm$Anova$SSP[[ii]], fm$Anova$df[ii], fm$Anova$error.df), error=function(e) NULL)
         #p-value for upper F
         if(!is.null(wsmvt)) p_wsmvt[ii] <- pf(wsmvt[2], wsmvt[3], wsmvt[4], lower.tail = FALSE)
      }
      
   }
   # true MVT
   #mvfm <- Anova(fm$lm, type=fm$Anova$type, test='Pillai')
   mvfm <- tryCatch(Anova(fm$lm, type = fm$Anova$type, test = "Pillai"), error=function(e) NULL)
   if(is.null(mvfm))
      out_p <- apply(cbind(uvP[1:nF_mvE5], uvP[(nF_mvE5+1):(2*nF_mvE5)], p_wsmvt[1:nF_mvE5],  p_wsmvt[(nF_mvE5+1):(2*nF_mvE5)]), 1, min) else {
      if(fm$Anova$type=='III') for(kk in 1:nF_mvE5) {
         mvt <- tryCatch(stats:::Pillai(Re(eigen(qr.coef(qr(mvfm$SSPE), mvfm$SSP[[kk]]), symmetric = FALSE)$values), mvfm$df[[kk]], mvfm$error.df), error=function(e) NULL)
         #p-value for upper F
         p_mvt[kk] <- ifelse(is.null(mvt), 1, pf(mvt[2], mvt[3], mvt[4], lower.tail = FALSE))
      }
      if(fm$Anova$type=='II') for(kk in 1:nF_mvE5) { # no intercept
         if(kk==1) p_mvt[kk] <- 1 else {
         mvt <- tryCatch(stats:::Pillai(Re(eigen(qr.coef(qr(mvfm$SSPE), mvfm$SSP[[kk-1]]), symmetric = FALSE)$values), mvfm$df[[kk-1]], mvfm$error.df), error=function(e) NULL)
         #p-value for upper F
         p_mvt[kk] <- ifelse(is.null(mvt), 1, pf(mvt[2], mvt[3], mvt[4], lower.tail = FALSE))
         }
      }
      #out_p <- apply(cbind(uvP[1:nF_mvE5], uvP[(nF_mvE5+1):(2*nF_mvE5)], p_wsmvt, p_mvt), 1, min)
      out_p <- apply(cbind(uvP[1:nF_mvE5], uvP[(nF_mvE5+1):(2*nF_mvE5)], p_wsmvt[1:nF_mvE5],  p_wsmvt[(nF_mvE5+1):(2*nF_mvE5)], p_mvt), 1, min)
   }
   return(out_p)
}
                                                
runAOV <- function(inData, dataframe, ModelForm) {
   out <- lop$outInit
   if(!is.null(lop$resid)) residout <- rep(0, length(inData))
   options(warn = -1)
   if (!all(abs(inData) < 10e-8)) {       
      dataframe$Beta<-inData[1:lop$NoFile]
      if(any(!is.na(lop$vQV))) {
         dataframe <- assVV(dataframe, lop$vQV, inData[(lop$NoFile+1):(lop$NoFile+lop$nSubj)], all(is.na(lop$vVarCenters)))
      }
      fm <- NULL
      if(lop$robust) {
         suppressMessages(try(fm <- lmrob(lop$ModelForm, data=dataframe), silent=TRUE))
         if(fm$converged) {
            rlm_run <- TRUE; uvfm <- NULL 
            #tryCatch(uvfm <- Anova(fm, type=lop$SS_type), error=function(e) NULL)
            #if(!is.null(uvfm)) out[1:lop$nFu] <- qf(uvfm[1:lop$nFu,3], uvfm[1:lop$nFu, 1], fm$degree.freedom, lower.tail = FALSE)
            tryCatch(uvfm <- Anova(fm, test.statistic="F", type=3) , error=function(e) NULL)
            if(!is.null(uvfm)) out[1:lop$nFu] <- uvfm$F[1:lop$nF]
            if((lop$num_glt > 0) | (lop$num_glf > 0)) {
               gltIn <- fm; iData <- NULL
            }
        } else {
            rlm_run <- FALSE; fm <- NULL
          }
      }
      if(is.null(fm)) {
         rlm_run <- FALSE
         if(lop$afex_new) suppressMessages(try(fm <- aov_car(ModelForm, data=dataframe, factorize=FALSE, type=lop$SS_type), silent=TRUE)) else
            suppressMessages(try(fm <- aov.car(ModelForm, data=dataframe, factorize=FALSE, type=lop$SS_type, return='full'), silent=TRUE))
      }
      if(!is.null(fm) & !rlm_run) {
          uvfm <- NULL
            if(lop$afex_new) {
               uvfm <- tryCatch(summary(fm), error=function(e) NULL)
               uvfm0 <- tryCatch(anova(fm, intercept=T), error=function(e) NULL)
            } else uvfm <- tryCatch(univ(fm$Anova), error=function(e) NULL) # univariate model
           if(!is.null(uvfm)) {
            if(is.na(lop$wsVars) & is.na(lop$mVar)) {  # between-subjects factors/variables only
                  if(lop$afex_new) tryCatch(Fvalues <- unname(uvfm0[1:lop$nF,4]), error=function(e) NULL) else
                     tryCatch(Fvalues <- uvfm[1:lop$nF,3], error=function(e) NULL)
                  if(!is.null(Fvalues)) if(!any(is.nan(Fvalues))) out[1:lop$nFu] <- Fvalues
               } else if(lop$mvE5) { # combine 5 tests: assuming only one within-subject factor
                  tryCatch(out[(1:lop$nF)] <-
                  qchisq(mvCom5(fm, lop$nF_mvE5), 1, lower.tail = FALSE), error=function(e) NULL)
               } else {# contain within-subject variable(s)
                  if(lop$afex_new) tryCatch(Fvalues <- uvfm0[,'F'], error=function(e) NULL) else
                     tryCatch(Fvalues <- unname(uvfm$anova[,5]), error=function(e) NULL)
                  if(!is.null(Fvalues)) if(!any(is.nan(Fvalues))) {
                     out[1:lop$nFu] <- Fvalues  # univariate Fs: no spherecity correction
                     if(lop$SC) { # sphericity correction
                        if(lop$afex_new) { # new version of afex
                           getGG <- uvfm$pval.adjustments[,'HF eps'] < lop$crit
                           GG    <- uvfm$pval.adjustments[,'Pr(>F[GG])']; HF <- uvfm$pval.adjustments[,'Pr(>F[HF])']
                           #Fsc  <- ifelse(getGG, GG, HF)
                           Fsc  <- ifelse(uvfm$sphericity.tests[, 'p-value'] < 0.05, ifelse(getGG, GG, HF), uvfm$univariate.tests[dimnames(uvfm$sphericity.tests)[[1]], 'Pr(>F)'])
                        } else { # old version of afex
                           getGG <- uvfm$sphericity.correction[,'HF eps'] < lop$crit
                           GG    <- uvfm$sphericity.correction[,'Pr(>F[GG])']; HF <- uvfm$sphericity.correction[,'Pr(>F[HF])']
                           #Fsc  <- ifelse(getGG, GG, HF)
                           Fsc  <- ifelse(uvfm$mauchly[, 'p-value'] < 0.05, ifelse(getGG, GG, HF), uvfm$anova[dimnames(uvfm$mauchly)[[1]], 'Pr(>F)'])
                        }
                        tryCatch(out[(lop$nFu+1):(lop$nFu+lop$GES*lop$nFu+lop$nFsc)] <-
                              qf(Fsc, lop$numDF, lop$denDF, lower.tail = FALSE), error=function(e) NULL)
                     } #if(lop$SC)
                  if(lop$wsMVT) {  # within-subject MVT is requested
                     for(ii in 1:length(lop$mvtInd)) tryCatch(out[lop$nFu+lop$nFsc+ii] <-
                           maov(fm$Anova$SSPE[[lop$mvtInd[ii]]], fm$Anova$SSP[[lop$mvtInd[ii]]], fm$Anova$df[lop$mvtInd[ii]],
                           fm$Anova$error.df)[2], error=function(e) NULL) 
                  } # if(lop$wsMVT)
                  if(lop$wsE2) {  # option -wsE2: UVT and wsMVT combined 
                     for(ii in lop$mvtInd) {
                        wsmvt <- NULL
                        tryCatch(wsmvt <-
                           maov(fm$Anova$SSPE[[ii]], fm$Anova$SSP[[ii]], fm$Anova$df[ii], fm$Anova$error.df), error=function(e) NULL)
                        if(!is.null(wsmvt)) {
                           p_wsmvt <- pf(wsmvt[2], wsmvt[3], wsmvt[4], lower.tail = FALSE)
                           if(lop$afex_new) if(p_wsmvt < uvfm$univariate.test[ii,'Pr(>F)']) out[ii] <-
                              qf(p_wsmvt, uvfm$univariate.test[ii,'num Df'], uvfm$univariate.test[ii,'den Df'], lower.tail = FALSE) else
                              if(p_wsmvt < uvfm$anova[ii,'Pr(>F)']) out[ii] <-
                              qf(p_wsmvt, uvfm$anova[ii,'num Df'], uvfm$anova[ii,'den Df'], lower.tail = FALSE)
                        } # if(!is.null(wsmvt)) 
                     } # for(ii in lop$mvtInd)
                  } # if(lop$wsE2)
               } #if(!any(is.nan(Fvalues)))
              } #if(is.na(lop$wsVars) & is.na(lop$mVar)])
            } #if(!is.null(uvfm))
            
         if(!is.na(lop$mVar)) { # real MVM: currently not allowed to mix within-subject variables
            tryCatch(mvfm <- Anova(fm$lm, type=lop$SS_type, test='Pillai'), error=function(e) NULL)  # need to add options for type and test!
            #if(is.na(lop$wsVars)) tryCatch(out[(lop$nFu+lop$nFsc+1):lop$nF] <- maov(mvfm)[2], error=function(e) NULL)
            for(ii in 1:lop$nFm) # lop$nFm equals length(mvfm$terms)
               #tryCatch(out[lop$nFu+lop$nFsc+ii] <-
               tryCatch(out[lop$nFu+lop$nFsc+length(lop$mvtInd)+ii] <-
                  maov(mvfm$SSPE, mvfm$SSP[[ii]], mvfm$df[ii], mvfm$error.df)[2], error=function(e) NULL)
         }  #if(!is.na(lop$mVar)])

         if(lop$mvE5a) { # combine 4 tests: assuming only one within-subject factor
            tryCatch(out[(lop$nFu+lop$nFsc+length(lop$mvtInd)+lop$nFm+1):
               (lop$nFu+lop$nFsc+length(lop$mvtInd)+lop$nFm+lop$nF_mvE5)] <-
               qchisq(mvCom5(fm, lop$nF_mvE5), 1, lower.tail = FALSE), error=function(e) NULL)
         } # redundant computations in mvCom5 for option mvE5a

         # Generalized eta-squared
         if(lop$GES) out[(lop$nF+1):(lop$nF+lop$nFu)] <- uvfm0[,'ges']

          if((lop$num_glt > 0) | (lop$num_glf > 0)) {
              gltIn <- fm$lm; if(lop$afex_new) iData <- fm$data$idata else iData <- fm$idata
          }
         } # if(!is.null(fm) & !lop$robust)
      if(!is.null(fm)) {         
         # GLT part below
         if(lop$num_glt>=1) for(ii in 1:lop$num_glt) {  # these are multivariate tests!
             if(all(is.na(lop$gltList[[ii]]))) { # Covariate testing only without factors involved
               glt <- tryCatch(testInteractions(gltIn, pairwise=NULL, slope=lop$slpList[[ii]], 
                  covariates=lop$covValList[[ii]], adjustment="none", idata = iData), error=function(e) NULL) } else { # Involving factors
               glt <- tryCatch(testInteractions(gltIn, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
               covariates=lop$covValList[[ii]], adjustment="none", idata = iData), error=function(e) NULL)
            }
            if(!is.null(glt)) {
               out[lop$nF+lop$GES*lop$nFu+2*ii-1] <- glt[1,1]
	       #out[lop$nF+lop$GES*lop$nFu+2*ii]   <- sign(glt[1,1]) * sqrt(glt[1,4])  # convert F to t
               out[lop$nF+lop$GES*lop$nFu+2*ii]   <- ifelse(rlm_run, ifelse(glt[1,4]<0.5, qt(glt[1,4], glt$Df[2],
                  lower.tail = FALSE)*sign(glt[1,1]), -qt(glt[1,4], glt$Df[2], lower.tail = FALSE)*sign(glt[1,1])),
                  sign(glt[1,1]) * sqrt(glt[1,4]))
            } #if(!is.null(glt))
         } #if(pars[[3]]>=1) for(ii in 1:pars[[3]])

         # GLF part below
         if(lop$num_glf>=1) for(ii in 1:lop$num_glf) { # these are multivariate tests! # this first part below may need fixes!
            if(all(is.na(lop$glfList[[ii]]))) glfRes <- tryCatch(testFactors(gltIn, pairwise=NULL, slope=lop$slpListF[[ii]], 
               covariates=lop$covValListF[[ii]], adjustment="none", idata = iData)$terms$`(Intercept)`$test, error=function(e) NULL) else
            glfRes <- tryCatch(testFactors(gltIn, levels=lop$glfList[[ii]], slope=lop$slpListF[[ii]], 
               covariates=lop$covValListF[[ii]], adjustment="none", idata = iData)$terms$`(Intercept)`$test, error=function(e) NULL)
            if(!is.null(glfRes)) if(is.na(lop$wsVars) & is.na(lop$mVar)) # the output structure is different when no within-subject factors present
               out[lop$nF+lop$GES*lop$nFu+2*lop$num_glt+ii] <- glfRes$F[2] else
               tryCatch(out[lop$nF+lop$GES*lop$nFu+2*lop$num_glt+ii] <- maov(glfRes$SSPE, glfRes$SSPH, glfRes$df, glfRes$df.residual)[2], error=function(e) NULL)
         } #if(pars[[3]]>=1) for(ii in 1:pars[[3]])
         if(!is.null(lop$resid)) residout <- as.vector(t(unname(residuals(fm$lm))))  
      } # if(!is.null(fm))
   } # if (!all(abs(inData) < 10e-8))
   if(!is.null(lop$resid)) out <- c(out, residout)
   return(out)
}
# covariates=pars[[6]][7], adjustment="none", idata = fm[["idata"]]), error=function(e) NULL) else
# covariates=pars[[6]][7], adjustment="none", idata = fm[["idata"]]), error=function(e) NULL)
                                                
#################################################################################
########################## Read information from a file #########################
#################################################################################

#A function to parse all user input from a file

read.MVM.opts.from.file <- function (modFile='model.txt', verb = 0) {
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
} # end of read.MVM.from.file

#################################################################################
########################## Begin MVM main ######################################
#################################################################################

   if(!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))
       # save only on -dbgArgs          28 Apr 2016 [rickr]
       if ('-dbgArgs' %in% args) try(save(args, rfile, file=".3dMVM.dbg.AFNI.args", ascii = TRUE), silent=TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace")
      args <- .DBG_args
   }
   if(!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.MVM(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      #browser()
      if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
      if (is.null(lop <- read.MVM.opts.from.file(modFile, verb=0))) {
         stop('Error parsing input from file!');
      }

      if(0) str(lop)
      
   } else {
      if(!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }
      if(is.null(lop <- read.MVM.opts.batch(args, verb = 0)))
         stop('Error parsing input')

      # in case the user didn't put space around each colon (:), this 
lop$gltCode <- lapply(lop$gltCode, function(ss) unlist(strsplit(ss, split="(?=:)", perl=TRUE)))                                             
lop$glfCode <- lapply(lop$glfCode, function(ss) unlist(strsplit(ss, split="(?=&)", perl=TRUE)))                                             

      
      #str(lop);
      if(is.null(lop <- process.MVM.opts(lop, verb = lop$verb))) 
         stop('Error processing input')
      
   }
   #if(lop$verb > 1) { 
      #Too much output, big dump of header structs of input dsets..
   #   str(lop)
   #}


########################################################################

if(lop$SS_type != 2 & lop$SS_type != 3) errex.AFNI(c("The type for the sums of squares can be either 2 or 3!"))
                                                
if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])
if(!is.na(lop$vVarCenters)) lop$vVarCenters <- as.numeric(strsplit(as.character(lop$vVarCenters), '\\,')[[1]])
                                                
library("afex")
library("phia")
if(lop$robust) library("robustbase")
#if(lop$robust) pkgLoad(c('car', 'robustbase', 'phia')) else pkgLoad(c('afex', 'phia'))
options(contrasts = c("contr.sum", "contr.poly"))
                                               
#comArgs <- commandArgs()

#if(length(comArgs)<6) modFile <- "model.txt" else
#modFile <- comArgs[6]
#paste(commandArgs())

wd <- which(lop$dataTable %in% respVar)
# update respVar
respVar <- lop$dataTable[wd]

if(lop$robust) lop$ModelForm_rlm <- as.formula(paste("Beta ~", lop$model))                             

# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
if(is.na(lop$mVar)) {
   if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj)')) else {
      if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1)
         ModelForm <- as.formula(paste("Beta~", capture.output(cat(paste(strsplit(lop$model, '\\+')[[1]],"*", strsplit(lop$wsVars, '\\,')[[1]][1]), sep='+'))))   else {
      ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$wsVars, '))'))
      ModelForm2 <- as.formula(paste("Beta ~ (", lop$model, ')*', lop$wsVars, '+Error(Subj/(', lop$wsVars, '))'))
      }
   }
} else if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$mVar, '))')) else
   ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$wsVars, '*', lop$mVar, '))'))
                                             
# Maybe not list for these two, or yes?
lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr[[respVar]] <-  as.character(lop$dataStr[[respVar]])

# center on user-speficied value or mean
if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters))) 
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F) else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)[,collapse=T]

cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of data structure *****\n')
#print(sprintf('%i subjects: ', nlevels(lop$dataStr$Subj)))
#for(ii in 1:nlevels(lop$dataStr$Subj)) print(sprintf('%s ', levels(lop$dataStr$Subj)[ii]))

lop$nSubj <- nlevels(lop$dataStr$Subj)                                                
cat(lop$nSubj, 'subjects : ', levels(lop$dataStr$Subj), '\n')
cat(length(lop$dataStr[[respVar]]), 'response values\n')
for(ii in 2:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':', 
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'matrix' | class(lop$dataStr[,ii]) == 'numeric')  # numeric may not work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')
cat(lop$num_glt, 'post hoc tests\n')

cat('\nContingency tables of subject distributions among the categorical variables:\n\n')
if(is.na(lop$mVar)) { if(is.na(lop$wsVars)) showTab <- paste('~', lop$model) else {
    if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1) showTab <- paste('~', lop$model) else
    showTab <- paste('~', gsub("\\*", "+", lop$model), '+', gsub("\\*", "+", lop$wsVars))
   }
   } else {
   if(is.na(lop$wsVars)) showTab <- as.formula(paste('~', gsub("\\*", "+", lop$model), "+", gsub("\\*", "+", lop$mVar)))  else {                            
   showTab <- paste('~', lop$model, "+", gsub("\\*", "+", lop$wsVars), "+", gsub("\\*", "+", lop$mVar)) } }
#if(!is.na(lop$qVars)) for(ii in 1:length(lop$QV))
#   showTab <- gsub(paste('\\*',lop$QV[ii], sep=''), '', gsub(paste('\\+',lop$QV[ii], sep=''), '', showTab))
if(!is.na(lop$qVars)) for(ii in rev(levels(ordered(lop$QV)))) # reversing the oder of those quantitative covariates so that
   # situations like 'ARI', 'ARI1', ARI2' would not cause trouble here!
   showTab <- gsub(paste('\\*', ii, sep=''), '', gsub(paste('\\+', ii, sep=''), '', showTab))
showTab <- as.formula(gsub("\\*", "+", showTab))  # in case there are still some *'s like between-subjects factors
if(!(showTab == '~1')) print(xtabs(showTab, data=lop$dataStr))                                           
                                               
cat('\nTabulation of subjects against each of the categorical variables:')
all_vars <- names(lop$dataStr)
for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
   cat('\n~~~~~~~~~~~~~~')
   cat('\nlop$nSubj vs ', var, ':\n', sep='')
   print(table(lop$dataStr$Subj, lop$dataStr[,var]))
}

cat('\n***** End of data structure information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')


# Assume the last column is input files
#FileCol <- length(colnames(lop$dataStr))
FileCol <- dim(lop$dataStr)[2]

#Number of input files
lop$NoFile <- dim(lop$dataStr[1])[1]

# check afex version
lop$afex_new <- (packageVersion('afex') >= 0.14)
                                                
# Repeated-measures (use lme) or not (use lm)
#if (length(unique(lop$dataStr$Subj)) != length(lop$dataStr$Subj)) RM <- TRUE else RM <- FALSE

cat('Reading input files now...\n\n')
if(any(is.na(suppressWarnings(as.numeric(lop$dataStr[, FileCol]))))) {  # not elegant because "NAs introduced by coercion"
                                                
# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(lop$dataStr[1, FileCol], verb=lop$verb, meth=lop$iometh, forcedset = TRUE)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData

# Read in all input files
 inData <- unlist(lapply(lapply(lop$dataStr[,FileCol], read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
 tryCatch(dim(inData) <- c(dimx, dimy, dimz, lop$NoFile), error=function(e)
    errex.AFNI(c("Problem with input files! Two possibilities: 1) There is a specification error\n",
   "with either file path or file name. Use shell command \'ls\' on the last column in the\n",
   "data table to find out the problem. 2) At least one of the input files has different\n",
   "dimensions. Run \"3dinfo -header_line -prefix -same_grid -n4 *.HEAD\" in the directory\n",
   "where the files are stored, and pinpoint out which file(s) is the trouble maker.\n",
   "Replace *.HEAD with *.nii or something similar for other file formats.\n")))
cat('Reading input files: Done!\n\n')

# voxel-wise covariate files
if(any(!is.null(lop$vVars))) {
   for(ii in lop$vQV)
   if(length(unique(lop$dataStr[,ii])) != nlevels(lop$dataStr$Subj))
      errex.AFNI(c("Error with voxel-wise covariate ", ii, ": Each subject is only\n",
                "allowed to have one volume; that is, the covariate has to be at the\n",
                "subject level.")) else {  # currently consider one voxel-wise covariate only: may generalize later?
      vQV <- unlist(lapply(lapply(unique(lop$dataStr[,lop$vQV[1]]), read.AFNI, verb=lop$verb, meth=lop$iometh, forcedset = TRUE), '[[', 1))
      dim(vQV) <- c(dimx, dimy, dimz, length(unique(lop$dataStr[,lop$vQV[1]])))
      inData <- c(inData, vQV)
      dim(inData) <- c(dimx, dimy, dimz, lop$NoFile+lop$nSubj)
   }
} else vQV <- NULL

if (!is.na(lop$maskFN)) {
   Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh, forcedset = TRUE)$brk[,,,1]
   inData <- array(apply(inData, 4, function(x) x*(abs(Mask)>tolL)),
      dim=c(dimx,dimy,dimz,lop$NoFile+(!is.na(lop$vQV[1]))*lop$nSubj))
}
                                                
# assign voxel-wise covariate values
assVV <- function(DF, vQV, value, c) {
   if(is.na(c)) cvalue <- scale(value, center=TRUE, scale=F) else
      cvalue <- scale(value, center=c, scale=F)
   for(ii in 1:length(unique(DF[,vQV]))) {                                   
   # ii-th subject's rows
      fn <- paste(lop$vQV[1], '_fn', sep='')
      sr <- which(unique(DF[,fn])[ii] == DF[,fn])
      #browser()
      DF[sr, vQV] <- rep(cvalue[ii], length(sr))
    }
    DF[, vQV] <- as.numeric(DF[, vQV])
    return(DF)
}                                           

# add a new column to store the voxel-wise filenames
if(any(!is.null(lop$vVars))) {
   lop$dataStr <- cbind(lop$dataStr, lop$dataStr[, lop$vQV[1]])                                                
   names(lop$dataStr)[length(names(lop$dataStr))] <- paste(lop$vQV[1], '_fn', sep='')
}                                           
                                                
# DF: lop$dataStr[,lop$vQV[1]]
                                                
                                                
# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
#ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

###############################

xinit <- dimx%/%3
if(dimy==1) yinit <- 1 else yinit <- dimy%/%2
if(dimz==1) zinit <- 1 else zinit <- dimz%/%2

ii <- xinit; jj <- yinit; kk <- zinit

fm<-NULL
gltRes <- vector('list', lop$num_glt)
glfRes <- vector('list', lop$num_glf)

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or the GLT coding\n')
cat('is likely inappropriate.\n\n')


while(is.null(fm)) {
   fm<-NULL
   if (all(abs(inData[ii, jj, kk,]) < 10e-8)) fm<-NULL else {
   lop$dataStr$Beta<-inData[ii, jj, kk,1:lop$NoFile]
   if(any(!is.null(lop$vVars))) {
      #lop$dataStr <- assVV(lop$dataStr, lop$vQV[1], vQV[ii,jj,kk,])
      lop$dataStr <- assVV(lop$dataStr, lop$vQV[1], inData[ii,jj,kk,(lop$NoFile+1):(lop$NoFile+lop$nSubj)], lop$vVarCenters[1])
      #if(all(is.na(lop$vVarCenters))) 
      #   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F) else
      #   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$vVarCenters, scale=F)
   }   
   #options(warn=-1)
   if(lop$robust) {
       suppressMessages(try(fm <- lmrob(lop$ModelForm, data=lop$dataStr), silent=TRUE))
       if(!fm$converged) fm <- NULL
   } else {
   if(lop$afex_new) suppressMessages(try(fm <- aov_car(ModelForm, data=lop$dataStr, factorize=FALSE, type=lop$SS_type), silent=TRUE)) else
   suppressMessages(try(fm <- aov.car(ModelForm, data=lop$dataStr, factorize=FALSE, type=lop$SS_type, return='full'), silent=TRUE)) }

   if(!is.null(fm)) {
      if(lop$robust) uvfm <- Anova(fm, test.statistic="F", type=3) else {
         if(lop$afex_new) {
            uvfm  <- summary(fm)
            uvfm0 <- anova(fm, intercept=T) # contains intercept when no within-subject factors involved, and provides GES
         } else uvfm <- univ(fm$Anova)  # univariate modeling
          if(!is.na(lop$mVar)) if(is.na(lop$wsVars)) mvfm <- Anova(fm$lm, type=lop$SS_type, test='Pillai')
      }
   }

   if(!is.null(fm)) if((lop$num_glt > 0) | (lop$num_glf > 0)) if(lop$robust) {
      gltIn <- fm; iData <- NULL } else {
      gltIn <- fm$lm; if(lop$afex_new) iData <- fm$data$idata else iData <- fm$idata
   }

   if(!is.null(fm)) if (lop$num_glt > 0) {
      n <- 1
      while(!is.null(fm) & (n <= lop$num_glt)) {
         if(all(is.na(lop$gltList[[n]]))) {  # Covariate testing only without factors involved
            gltRes[[n]] <- tryCatch(testInteractions(gltIn, pairwise=NULL,
               covariates=lop$covValList[[n]], slope=lop$slpList[[n]], adjustment="none", idata = iData),
               error=function(e) NA) } else {     # Involving factors
            tryCatch(gltRes[[n]] <- testInteractions(gltIn, custom=lop$gltList[[n]], slope=lop$slpList[[n]], 
               covariates=lop$covValList[[n]], adjustment="none", idata = iData), error=function(e) NULL)
         }        
         if(any(is.null(gltRes[[n]]))) {
            fm <- NULL
            errex.AFNI(paste("Failed at GLT No. ", n, "! Make sure that the model or GLT specification syntax is correct.", sep=''))
         }
         n <- n+1
      }
   }

   if(!is.null(fm)) if (lop$num_glf > 0) {
      n <- 1
      while(!is.null(fm) & (n <= lop$num_glf)) { # this part may need fixes!
         if(all(is.na(lop$glfList[[n]]))) { # Covariate testing only without factors involved: possible?
            glfRes[[n]] <- tryCatch(testFactors(gltIn, pairwise=NULL,
               covariates=lop$covValListF[[n]], slope=lop$slpListF[[n]], adjustment="none", idata = iData)$terms$`(Intercept)`$test,
               error=function(e) NA) } else {  # Involving factors
            glfRes[[n]] <- tryCatch(testFactors(gltIn, levels=lop$glfList[[n]], slope=lop$slpListF[[n]], 
               covariates=lop$covValListF[[n]], adjustment="none", idata = iData)$terms$`(Intercept)`$test, error=function(e) NULL)
         }
         if(any(is.null(glfRes[[n]]))) fm <- NULL
         n <- n+1
      }      
   }   
   
   }
   if(!is.null(fm))  {
      print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
   } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed! ~~~~~~~~~~~~~~~~~~~\n')
      cat('Possible reasons:\n\n')
      cat('0) Make sure that R packages afex and phia have been installed. See the 3dMVM\n')
      cat('help documentation for more details.\n\n')
      cat('1) Inappropriate model specification with options -bsVars, -wsVars, or -qVars.\n')
      cat('Note that within-subject or repeated-measures variables have to be declared\n')
      cat('with -wsVars.\n\n')
      cat('2) Incorrect specifications in general linear test coding with -gltCode.\n\n')
      cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n\n')
      cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
      cat('named Group in model specification and then listed as group in the table header\n')
      cat('would cause grief for 3dMVM.\n\n')
      cat('5) Not enough number of subjects. This may happen when there are two or more\n')
      cat('withi-subject factors. For example, a model with two within-subject factors with\n')
      cat('m and n levels respectively requires more than (m-1)*(n-1) subjects to be able to\n')
      cat('model the two-way interaction with the multivariate approach.\n\n')
      errex.AFNI("Quitting due to model test failure...")
   }
}

#fm <- aov.car(ModelForm, data=Model, factorize=FALSE, return='lm')
#bm <- c(1,-1)%*%t(aa$lm$coefficients)
#Vm <- c(1,-1)%*%vcov(aa$lm)%*%c(1,-1)
#t(bm)%*%solve(Vm)%*%bm

# number of terms (or F-stats): the output structure is different without within-subject variables
# assuming univariate testing for within-subject variables here! May need to change later

#if(is.na(lop$mVar))
#   nF <- ifelse(is.na(lop$wsVars), dim(uvfm)[1]-2, dim(uvfm$anova)[1]-1) else
#   if(is.na(lop$wsVars)) nF <- length(mvfm$terms) else {}
                                                
lop$nFsc <- 0; nF_MVT <- 0; lop$nF_mvE5 <- 0; mvtInd <- NULL                                            
if(!is.na(lop$wsVars) | !is.na(lop$mVar)) {
   if(lop$SC) {
      if(lop$afex_new) corTerms <- rownames(uvfm$sphericity.tests) else  corTerms <- rownames(uvfm$sphericity.correction)
#      allTerms <- rownames(uvfm$anova)[-1]
      lop$nFsc <- length(corTerms)  # number of F-stat for spherecity correction
   }
#   if(lop$wsMVT) nF_MVT <- length(fm$Anova$SSPE)  # number of within-subject MVT
   if(lop$wsMVT | lop$wsE2) {  # only make sense if a within-subject factor has more than 2 levels, otherwise uvfm$sphericity.correction is NULL
      if(lop$afex_new) lop$mvtInd <- which(names(fm$Anova$SSPE) %in% dimnames(uvfm$sphericity.tests)[[1]]) else # indices for terms needed for MVT
         lop$mvtInd <- which(names(fm$Anova$SSPE) %in% dimnames(uvfm$sphericity.correction)[[1]])
      if(lop$wsMVT) nF_MVT <- length(lop$mvtInd)  # number of within-subject MVT, same as nFsc
   }
}

# lop$SS_type has impact on whether there is an intercept in the output for nFm: Anova(fm$lm),
# but no impact on all others (nFu, nFsc, nF_MVT)

# number of F-stat for univariate modeling 
#if(lop$afex_new) lop$nFu <- ifelse(is.na(lop$wsVars) & is.na(lop$mVar), dim(uvfm)[1], dim(uvfm$univariate.test)[1]) else  # contains intercept
if(lop$robust) lop$nFu <- dim(uvfm)[1]-1 else {
   if(lop$afex_new) lop$nFu <- dim(uvfm0)[1] else  # contains intercept
   lop$nFu <- ifelse(is.na(lop$wsVars) & is.na(lop$mVar), dim(uvfm)[1]-1, dim(uvfm$anova)[1])
}
# nFm: number of F-stat for real MVM
if(!is.na(lop$mVar)) if(is.na(lop$wsVars))
   lop$nFm <- length(mvfm$terms) else lop$nFm <- 0 else lop$nFm <- 0
#if(lop$mvE5a | lop$mvE5) if(lop$afex_new) lop$nF_mvE5 <- nrow(uvfm$univariate.test)/2 else
if(lop$mvE5a | lop$mvE5) if(lop$afex_new) lop$nF_mvE5 <- lop$nFu/2 else
   lop$nF_mvE5 <- nrow(uvfm$anova)/2 
lop$nF <- ifelse(lop$mvE5, lop$nF_mvE5, lop$nFu + lop$nFsc + nF_MVT + lop$nFm + lop$nF_mvE5)
#nF <- nFu + nFsc + nF_MVT + nFm + nF_mvE5
                                                
NoBrick <- lop$nF + lop$GES*lop$nF + 2*lop$num_glt + lop$num_glf

if(lop$robust) brickNames <- paste(dimnames(uvfm)[[1]][1:lop$nFu], 'F') else {
   if(is.na(lop$wsVars) & is.na(lop$mVar)) {
      if(lop$afex_new) brickNames <- paste(dimnames(uvfm0)[[1]], 'F') else
      brickNames <- paste(dimnames(uvfm)[[1]][1:(length(dimnames(uvfm)[[1]])-1)], 'F') } else {
      if(lop$afex_new) brickNames <- paste(dimnames(uvfm0)[[1]], 'F') else
         brickNames <- paste(dimnames(uvfm$anova)[[1]], 'F')
      if(lop$SC & (lop$nFsc > 0)) brickNames <- c(brickNames, paste(corTerms, '-SC-', 'F'))
   #   if(lop$wsMVT & (nF_MVT > 0)) brickNames <- c(brickNames, paste(names(fm$Anova$SSPE), '-wsMVT-', 'F'))
      if(lop$wsMVT & (nF_MVT > 0)) if(lop$afex_new) brickNames <- c(brickNames, paste(rownames(uvfm$sphericity.tests), '-wsMVT-', 'F')) else
         brickNames <- c(brickNames, paste(rownames(uvfm$sphericity.correction), '-wsMVT-', 'F'))
   }

   #brickNames <- ifelse(is.na(lop$wsVars) & is.na(lop$mVar),
   #          paste(dimnames(uvfm)[[1]][2:(length(dimnames(uvfm)[[1]])-1)], 'F'),
   #          paste(dimnames(uvfm$anova)[[1]][-1], 'F'))
   
   if(!is.na(lop$mVar)) brickNames <- c(brickNames, paste(mvfm$terms, '-MV0-', 'F'))                                    
   if(lop$mvE5a) if(lop$afex_new) brickNames <- c(brickNames, paste(dimnames(uvfm0)[[1]][1:lop$nF_mvE5], '-mvE5', 'Chisq')) else
      brickNames <- c(brickNames, paste(dimnames(uvfm$anova)[[1]][1:lop$nF_mvE5], '-mvE5', 'Chisq'))
   if(lop$mvE5) if(lop$afex_new) brickNames <- paste(dimnames(uvfm0)[[1]][1:lop$nF_mvE5], '-mvE5', 'Chisq') else # no appending
      brickNames <- paste(dimnames(uvfm$anova)[[1]][1:lop$nF_mvE5], '-mvE5', 'Chisq')
   if(lop$GES)  brickNames <- c(brickNames, paste(dimnames(uvfm0)[[1]], 'GES'))
}

if(lop$num_glt>0) for(ii in 1:lop$num_glt) {
   brickNames <- c(brickNames, lop$gltLabel[ii])
   if(lop$robust) brickNames <- c(brickNames, paste(lop$gltLabel[ii], 't')) else
      brickNames <- c(brickNames, paste(lop$gltLabel[ii], 't'))
}

if(lop$num_glf>0) for(ii in 1:lop$num_glf)
   if(lop$robust) brickNames <- c(brickNames, paste(lop$glfLabel[ii], 'F')) else
      brickNames <- c(brickNames, paste(lop$glfLabel[ii], 'Z'))

if(lop$robust) {
      F_DF <- vector('list', lop$nF)
      for(ii in 1:lop$nFu) F_DF[[ii]] <- c(uvfm[ii, 'Df'], uvfm[lop$nFu+1, 'Df'])
      t_DF <- NULL   
      if(lop$num_glt>0) for(ii in 1:lop$num_glt)
         t_DF <- c(t_DF, ifelse(is.na(lop$wsVars) & is.na(lop$mVar), gltRes[[ii]][2,2], gltRes[[ii]][,6]))
      lop$outInit <- rep(0, NoBrick) 
   } else { 
   if(lop$SC & (lop$nFsc > 0)) {
      if(lop$afex_new) {
         scTerms <- dimnames(uvfm0)[[1]] %in% corTerms
         lop$numDF <- uvfm0[,'num Df'][scTerms]
         lop$denDF <- uvfm0[,'den Df'][scTerms]
      } else {
         scTerms <- dimnames(uvfm$anova)[[1]] %in% corTerms
         lop$numDF <- uvfm$anova[,'num Df'][scTerms]
         lop$denDF <- uvfm$anova[,'den Df'][scTerms]
     }
   } else {
      lop$numDF <- NULL
      lop$denDF <- NULL
   }
                                                   
   # DFs for t-stat 
   t_DF <- NULL   
   if(lop$num_glt>0) for(ii in 1:lop$num_glt)
      t_DF <- c(t_DF, ifelse(is.na(lop$wsVars) & is.na(lop$mVar), gltRes[[ii]][2,2], gltRes[[ii]][,6]))
   
   # DFs for F-stat
   F_DF <- vector('list', lop$nF)
   for(ii in 1:lop$nFu) if(is.na(lop$mVar) & is.na(lop$wsVars)) {# between-subjects variables only
      if(lop$afex_new) F_DF[[ii]] <- c(uvfm0[ii, 'num Df'], uvfm0[ii, 'den Df']) else
      F_DF[[ii]] <- c(uvfm[ii, 'Df'], uvfm[lop$nF+1, 'Df']) } else # having within-subject factor
      # the DFs from uvfm0 (GG correction) are DIFFERENT from uvfm$univariate.test!!!
      if(lop$afex_new) F_DF[[ii]] <- c(unname(uvfm$univariate.test[ii,'num Df']), unname(uvfm$univariate.test[ii,'den Df'])) else # skip the intercept: ii+1
         F_DF[[ii]] <- c(unname(uvfm$anova[ii,'num Df']), unname(uvfm$anova[ii,'den Df']))
   if(lop$nFsc > 0) for(ii in 1:lop$nFsc) F_DF[[lop$nFu+ii]] <- c(lop$numDF[ii], lop$denDF[ii])
                                                   
   if(nF_MVT > 0) for(ii in 1:nF_MVT) F_DF[[lop$nFu+lop$nFsc+ii]] <- c(unname(maov(fm$Anova$SSPE[[lop$mvtInd[ii]]], fm$Anova$SSP[[lop$mvtInd[ii]]],
                                  fm$Anova$df[lop$mvtInd[ii]], fm$Anova$error.df)[3]),
                                  unname(maov(fm$Anova$SSPE[[lop$mvtInd[ii]]], fm$Anova$SSP[[lop$mvtInd[ii]]],
                                  fm$Anova$df[lop$mvtInd[ii]], fm$Anova$error.df)[4]))
                                                   
   if(lop$nFm > 0) for(ii in 1:lop$nFm) {
      mvtest <- maov(mvfm$SSPE, mvfm$SSP[[ii]], mvfm$df[ii], mvfm$error.df)
      F_DF[[lop$nFu+lop$nFsc+nF_MVT+ii]] <- c(mvtest[3], mvtest[4]) 
   }
   
   glf_DF <- vector('list', lop$num_glf)   
   #if(lop$num_glf>0) for(ii in 1:lop$num_glf)
   #   glf_DF[[ii]] <- maov(glfRes[[ii]]$SSPE, glfRes[[ii]]$SSPH, glfRes[[ii]]$df, glfRes[[ii]]$df.residual)[3:4]
   
   if(lop$num_glf>0) for(ii in 1:lop$num_glf) if(is.na(lop$wsVars) & is.na(lop$mVar))
      glf_DF[[ii]] <- c(glfRes[[ii]]$Df[2], glfRes[[ii]]$Res.Df[2]) else
      glf_DF[[ii]] <- maov(glfRes[[ii]]$SSPE, glfRes[[ii]]$SSPH, glfRes[[ii]]$df, glfRes[[ii]]$df.residual)[3:4]
   
   lop$outInit <- rep(0, NoBrick)  # initialization for the voxel-wise output
   lop$crit    <- 0.75  # switch between GG and HF
}

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################

if(dimy == 1 & dimz == 1) {
   nSeg <- 20
   # drop the dimensions with a length of 1
   inData <- inData[, , ,]
   # break into 20 segments, leading to 5% increamental in parallel computing
   dimx_n <- dimx%/%nSeg + 1
   # number of datasets need to be filled
   fill <- nSeg-dimx%%nSeg
   # pad with extra 0s
   inData <- rbind(inData, array(0, dim=c(fill, lop$NoFile)))
   # declare output receiver
   out <- array(0, dim=c(dimx_n, nSeg, NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))
   # break input multiple segments for parrel computation
   # test runAOV(inData[ii,kk,], dataframe=lop$dataStr, ModelForm=ModelForm)
   dim(inData) <- c(dimx_n, nSeg, lop$NoFile)
   if (lop$nNodes==1) for(kk in 1:nSeg) {
      if(NoBrick > 1) out[,kk,] <- aperm(apply(inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm), c(2,1)) else
         out[,kk,] <- aperm(apply(inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm), dim=c(dimx_n, 1))
      cat("Computation done: ", 100*kk/nSeg, "%", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   
   if (lop$nNodes>1) {
   pkgLoad('snow')
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   if(lop$robust) {
      clusterEvalQ(cl, library(robustbase))
      clusterEvalQ(cl, library(car))
   } else clusterEvalQ(cl, library(afex))
   clusterEvalQ(cl, library(phia))
   clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
   clusterExport(cl, c("mvCom5", "maov", "lop", "assVV"), envir=environment())
   for(kk in 1:nSeg) {
      if(NoBrick > 1) out[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm), c(2,1)) else
      out[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm), dim=c(dimx_n, 1))
      cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   stopCluster(cl)
   }
   # convert to 4D
   dim(out) <- c(dimx_n*nSeg, 1, 1, NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr))
   # remove the trailers (padded 0s)
   out <- out[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]
} else {

# Initialization
out <- array(0, dim=c(dimx, dimy, dimz, NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)))

# set up a voxel @ ii jj kk to test
#runAOV(inData[ii, jj, kk,], dataframe=lop$dataStr, ModelForm=ModelForm)

if (lop$nNodes==1) for (kk in 1:dimz) {
   if(NoBrick > 1) out[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm), c(2,3,1)) else
      out[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm), dim=c(dimx, dimy, 1))      
   cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 

if (lop$nNodes>1) {
   pkgLoad('snow')
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   if(lop$robust) clusterEvalQ(cl, library(robustbase))
   clusterEvalQ(cl, library(afex))
   clusterEvalQ(cl, library(phia))
   clusterEvalQ(cl, options(contrasts = c("contr.sum", "contr.poly")))
   clusterExport(cl, c("mvCom5", "maov", "lop", "assVV"), envir=environment())
   #clusterCall(cl, maov) # let all clusters access to function maov()
   #clusterExport(cl, c("maov"), envir=environment()) # let all clusters access to function maov()
   for (kk in 1:dimz) {
      if(NoBrick > 1) out[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm), c(2,3,1)) else
         out[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm), dim=c(dimx, dimy, 1))
      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
}
}

# test on Z slice

#if (lop$nNodes>1)    {
#   library(snow)
#   cl <- makeCluster(lop$nNodes, type = "SOCK")
#   clusterEvalQ(cl, library(afex)); #clusterEvalQ(cl, library(contrast))
#   kk<- 32
#   
#      out[,,kk,] <-aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, ModelForm=ModelForm), c(2,3,1))
#      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
#   stopCluster(cl)
#}

# avoid overflow
Top <- 100
out[is.nan(out)] <- 0
out[out > Top] <- Top  
out[out < (-Top)] <- -Top  

###############################

statsym <- NULL

#for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ", F_DF[[ii]][1], F_DF[[ii]][2])
if(lop$mvE5) for(ii in 1:lop$nF) statsym <- c(statsym, list(list(sb=ii-1, typ="fict", par=1))) else
if(lop$mvE5a) {
   for(ii in 1:(lop$nF-lop$nF_mvE5)) statsym <- c(statsym, list(list(sb=ii-1, typ="fift", par=c(F_DF[[ii]][1], F_DF[[ii]][2]))))
   for(ii in (lop$nF-lop$nF_mvE5+1):lop$nF) statsym <- c(statsym, list(list(sb=ii-1, typ="fict", par=1)))
} else for(ii in 1:lop$nF) statsym <- c(statsym, list(list(sb=ii-1, 
                typ="fift", par=c(F_DF[[ii]][1], F_DF[[ii]][2]))))
                       
if(lop$num_glt>0) for(ii in 1:lop$num_glt)
   statsym <- c(statsym, list(list(sb=lop$nF+lop$GES*lop$nFu+2*ii-1, typ="fitt", par=t_DF[ii])))

if(lop$num_glf>0) for(ii in 1:lop$num_glf)
   statsym <- c(statsym, list(list(sb=lop$nF+lop$GES*lop$nFu+2*lop$num_glt+ii-1, typ="fift", par=glf_DF[ii][[1]])))        
   
write.AFNI(lop$outFN, out, brickNames, defhead=head, idcode=newid.AFNI(),
   com_hist=lop$com_history, statsym=statsym, addFDR=1, type='MRI_short',
   overwrite=lop$overwrite)

if(!is.null(lop$resid))
   write.AFNI(lop$resid, out[,,,(NoBrick+1):(NoBrick+(!is.null(lop$resid))*nrow(lop$dataStr)), drop=FALSE],
      label=NULL, defhead=head, idcode=newid.AFNI(), com_hist=lop$com_history, type='MRI_short')    
    
cat("\nCongratulations! You have got an output ", lop$outFN, ".\n\n", sep='')

} else { # if(is.numeric(lop$dataStr[, FileCol])): the last column is values instead of input file names
   lop$dataStr$Beta <- as.numeric(lop$dataStr[, FileCol]) # convert characters to values
   lop$outFN <- paste(strsplit(lop$outFN, '\\+tlrc')[[1]], '.txt', sep='')
   capture.output(cat(''), file = lop$outFN, append = FALSE)
   if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1) pkgLoad('nlme')
   if(iterPar %in% dimnames(lop$dataStr)[[2]]) nPar <- nlevels(as.factor(lop$dataStr[[iterPar]])) else nPar <- 1
   for(nn in 1:nPar) {
   fm <- NULL
   if(nPar == 1) inData <- lop$dataStr else
   if((levels(as.factor(lop$dataStr[[iterPar]]))[nn] %in% lop$parSubsetVector) | is.null(lop$parSubsetVector)) 
      inData <- lop$dataStr[lop$dataStr[[iterPar]] == levels(as.factor(lop$dataStr[[iterPar]]))[nn],] else
      inData <- NULL
   if(!is.null(inData)) {
      if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1) {  # LME: at least one within-subject factor other than ROI
         fm <- lme(ModelForm, random=~1|Subj, data=inData)
      } else { # MVM
      if(lop$afex_new) suppressMessages(try(fm <- aov_car(ModelForm, data=inData, factorize=FALSE, type=lop$SS_type), silent=TRUE)) else
      suppressMessages(try(fm <- aov.car(ModelForm, data=inData, factorize=FALSE, type=lop$SS_type, return='full'), silent=TRUE)) } } else
      fm <-NULL
   #fm <- aov.car(ModelForm, data=inData, factorize=FALSE, return='full')
   if(!is.null(fm)) {
      if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1) {  # LME: at least one within-subject factor other than ROI
         termROI <- grep('ROI', rownames(anova(fm))) # term rows that contain 'ROI'
         termROIn <- (1:nrow(anova(fm)))[-termROI]   # term rows that do not contain 'ROI'
         out_p <- apply(cbind(anova(fm)[termROIn,][,'p-value'], anova(fm)[termROI,][,'p-value']), 1, min)
         names(out_p) <- rownames(anova(fm))[termROIn]
      } else { # MVM
      #out_p <- apply(cbind(uvP[1:outTerms], uvP[(outTerms+1):length(uvP)], p_wsmvt[1:outTerms],
      #   p_wsmvt[(outTerms+1):length(uvP)], p_mvt), 1, min)
         options(warn = -1)
         if(lop$afex_new) lop$nF_mvE5 <- tryCatch(nrow(summary(fm)$univariate.tests)/2, error=function(e) NULL) else
            lop$nF_mvE5 <- tryCatch(nrow(univ(fm$Anova)$anova)/2, error=function(e) NULL)
         if(is.null(lop$nF_mvE5)) {
            tryCatch(fm2 <- aov(ModelForm2, data=inData), error=function(e) NULL)
            if(is.null(fm2)) errex.AFNI('Model failure...') else {
            nF_aov <- dim(summary(fm2)[[1]][[1]])[1]-1
            out_p <- apply(cbind(summary(fm2)[[1]][[1]][1:nF_aov,'Pr(>F)'], summary(fm2)[[2]][[1]][2:(nF_aov+1),'Pr(>F)']), 1, min)
            names(out_p) <- dimnames(summary(fm2)[[1]][[1]])[[1]][1:nF_aov]
            }
         } else out_p <- mvCom5(fm, lop$nF_mvE5)
      }
      # chisq 
      out_chisq <- qchisq(out_p, 1, lower.tail = FALSE)
      out_chisq[out_chisq==Inf] <- 100
      out <- cbind(out_chisq, 1, out_p)
      dimnames(out)[[2]] <- c('# Chisq', 'DF', 'Pr(>Chisq)')
      dimnames(out)[[1]] <- sprintf('# %s', dimnames(out)[[1]])
      nC <- max(nchar(row.names(out)))
      term <- formatC(row.names(out), width=-nC)
      #or term <- sprintf("%-11s", row.names(out))
      if(lop$num_glt>=1) { 
          if(length(strsplit(lop$wsVars, '\\,')[[1]]) > 1) {  # # LME case: at least one within-subject factor other than ROI
             out_post <- matrix(0, nrow = lop$num_glt, ncol = 3)
             for(ii in 1:lop$num_glt) { # assuming no glts for basis functions
               glt <- NULL
               if(all(is.na(lop$gltList[[ii]]))) glt <- tryCatch(testInteractions(fm, pairwise=NULL, slope=lop$slpList[[ii]], 
                  covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL) else
               glt <- tryCatch(testInteractions(fm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
                  covariates=lop$covValList[[ii]], adjustment="none"), error=function(e) NULL)              
               if(!is.null(glt)) {
                  out_post[ii,1]   <- glt[1,1]
                  out_post[ii,2]   <- sign(glt[1,1])*qnorm(glt[1,4]/2, lower.tail = F)  # convert chisq to Z
                  out_post[ii,3]   <- glt[1,4]
               }
           }
           dimnames(out_post)[[1]] <- sprintf('# %s', lop$gltLabel)
           dimnames(out_post)[[2]] <- c('# value', 'z-stat', '2-sided-P')
           nC2 <- max(nchar(row.names(out_post)))
           term2 <- formatC(row.names(out_post), width=-nC2)
         } else {   # MVM case
            out_post <- matrix(0, nrow = lop$num_glt, ncol = 4)
            for(ii in 1:lop$num_glt) {
               if(all(is.na(lop$gltList[[ii]]))) {
                   if(lop$afex_new) glt <- tryCatch(testInteractions(fm$lm, pairwise=NULL, slope=lop$slpList[[ii]], 
                      covariates=lop$covValList[[n]], adjustment="none", idata = fm$data$idata), error=function(e) NULL) else
               glt <- tryCatch(testInteractions(fm$lm, pairwise=NULL, slope=lop$slpList[[ii]], 
                      covariates=lop$covValList[[n]], adjustment="none", idata = fmidata), error=function(e) NULL) } else {
               if(lop$afex_new) glt <- tryCatch(testInteractions(fm$lm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
                   covariates=lop$covValList[[ii]], adjustment="none", idata = fm$data$idata), error=function(e) NULL) else
               glt <- tryCatch(testInteractions(fm$lm, custom=lop$gltList[[ii]], slope=lop$slpList[[ii]], 
                   covariates=lop$covValList[[ii]], adjustment="none", idata = fm$idata), error=function(e) NULL) }
               if(!is.null(glt)) {
                  out_post[ii,1]   <- glt[1,1]
                  out_post[ii,2]   <- sign(glt[1,1]) * sqrt(glt[1,4])  # convert F to t
                  out_post[ii,3]   <- glt[1,6]
                  out_post[ii,4]   <- glt[1,7] 
               } #if(!is.null(glt))
            } # for(ii in 1:lop$num_glt)
            dimnames(out_post)[[1]] <- sprintf('# %s', lop$gltLabel)
            dimnames(out_post)[[2]] <- c('# value', 't-stat', 'DF', '2-sided-P')
            nC2 <- max(nchar(row.names(out_post)))
            term2 <- formatC(row.names(out_post), width=-nC2)
         }
      } # if(lop$num_glt>=1)
      options(width = 800)  # include the width so that each line has enough capacity
      if(nPar==1) cat('# RESULTS: ANOVA table\n')  else
         cat('# RESULTS: ANOVA table -', levels(as.factor(lop$dataStr[[iterPar]]))[nn], '\n')
      cat('-------------------------------------\n')
      print(setNames(data.frame(unname(out), term,stringsAsFactors=F), c(colnames(out), formatC("",width=-nC))), row.names=F)
      if(lop$num_glt>=1) {
         if(nPar==1) cat('\n# RESULTS: Post hoc tests\n') else
            cat('\n# RESULTS: Post hoc tests -', levels(as.factor(lop$dataStr[[iterPar]]))[nn], '\n')
         cat('-------------------------------------\n')
         print(setNames(data.frame(unname(out_post), term2,stringsAsFactors=F), c(colnames(out_post), formatC("",width=-nC2))), row.names=F)
      }
      #cat('-------------------------------------\n\n')
      cat('#####################################\n\n')
      if(nPar==1) capture.output(cat('\n# RESULTS: ANOVA table\n'), file = lop$outFN, append = TRUE) else
         capture.output(cat('\n# RESULTS: ANOVA table -', levels(as.factor(lop$dataStr[[iterPar]]))[nn], '\n'), file = lop$outFN, append = TRUE)
      capture.output(cat(dim(out)[1], '# Number of effects\n'), file = lop$outFN, append = TRUE)
      capture.output(print(setNames(data.frame(unname(out), term,stringsAsFactors=F),
         c(colnames(out), formatC("",width=-nC))), row.names=F), file = lop$outFN, append = TRUE)
      if(lop$num_glt>=1) {
         if(nPar==1) capture.output(cat('\n# RESULTS: Post hoc tests\n'), file = lop$outFN, append = TRUE) else
            capture.output(cat('\n# RESULTS: Post hoc tests -', levels(as.factor(lop$dataStr[[iterPar]]))[nn], '\n'), file = lop$outFN, append = TRUE)
         capture.output(cat(dim(out_post)[1], '# Number of tests\n'), file = lop$outFN, append = TRUE)
         capture.output(print(setNames(data.frame(unname(out_post), term2,stringsAsFactors=F),
            c(colnames(out_post), formatC("",width=-nC2))), row.names=F), file = lop$outFN, append = TRUE)
     }
   } # if(!is.null(fm))
   } # for(nn in unique(lop$dataStr[[iterPar]]))
   cat("\nCongratulations! The above results are saved in file ", lop$outFN, "\n\n", sep='')
}

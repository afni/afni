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


#################################################################################
##################### Begin MVM Input functions ################################
#################################################################################


greeting.MVM <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 3dMVM ==================          
   AFNI Group Analysis Program with Multivariate Linear Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 2.0.0, Sept 13, 2013
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/MVM.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )


#The help function for 3dMVM batch (AFNI-style script mode)
help.MVM.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
          ================== Welcome to 3dMVM ==================          
    AFNI Group Analysis Program with Multi-Variate Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 2.0.0, Sept 13, 2013
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/MVM.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 3dMVM is a group-analysis program that performs traditional ANOVA- and ANCOVA-
 style computations. In addition, it can run multivariate modeling in the sense
 of mulitple simultaneous response varibles. It does not impose any bound on the
 number of explanatory variables, and these variables can be either categorical
 (factor) or numerical/quantitative (covariate). F-statistics for all main
 effects and interactions are automatically included in the output. In addition,
 general linear tests (GLTs) can be requested via symbolic coding.
 
 Input files for 3dMVM can be in AFNI, NIfTI, or surface (niml.dset) format.
 Note that unequal number of subjects across groups are allowed, but scenarios 
 with missing data for a within-subject factor are better modeled with 3dLME. 
 Cases with quantitative variables (covariates) that vary across the levels of 
 a within-subject variable are also better handled with 3dLME. Computational 
 cost with 3dMVM is very high compared to 3dttest++ or 3dANOVAx, but it has the
 capability to correct for sphericity violations when within-subject variables
 with more than two levels are involved.
 
 If you want to cite the analysis approach, use the following at this moment:
 
 Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). Linear 
 Mixed-Effects Modeling Approach to FMRI Group Analysis. NeuroImage, in press.

 In addition to R installtion, the following two R packages need to be acquired
 in R first before running 3dMVM:
 
 install.packages("afex")
 install.packages("phia")

 The snow package is also needed if one wants to take advantage of parallel 
 computing:
 
 install.packages("snow")
 
 Once the 3dMVM command script is constructed, it can be run by copying and
 pasting to the terminal. Alternatively (and probably better) you save the 
 script as a text file, for example, called MVM.txt, and execute it with the 
 following  (assuming on tc shell),
 
 tcsh -x MVM.txt &
 
 or,
 
 tcsh -x MVM.txt > diary.txt &
 
 The advantage of the latter command is that the progrression is saved into
 the text file diary.txt and, if anything goes awry, can be examined later.
 
 More details about 3dMVM can be found at 
 http://afni.nimh.nih.gov/sscc/gangc/MVM.html
 
 Thank the R community and Henrik Singmann in particular for the strong
 technical support.'

   ex1 <- 
"\n--------------------------------
Example 1 --- three between-subjects (genotype, sex, and scanner) and two 
within-subject (condition and emotion) variables:
   3dMVM  -prefix Example1 -jobs 4            \\
          -model  'genotype*sex+scanner'      \\
          -wsVars \"condition*emotion\"         \\
          -num_glt 14                         \\
          -gltLabel 1 face_pos_vs_neg -gltCode  1 'condition : 1*face emotion : 1*pos -1*neg'            \\
          -gltLabel 2 face_emot_vs_neu -gltCode 2 'condition : 1*face emotion : 1*pos +1*neg -2*neu'     \\
          -gltLabel 3 sex_by_condition_interaction -gltCode 3 'sex : 1*male -1*female condition : 1*face -1*house' \\
          -gltLabel 4 3way_interaction -gltCode 4 'sex : 1*male -1*female condition : 1*face -1*house emotion : 1*pos -1*neg' \\
          ...            
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

   NOTE:  1) The model for the analysis can also be set up as and is equivalent to 
          'genotype*sex*condition*emotion+scanner*condition*emotion'.
          2) The 3rd GLT is for the 2-way 2 x 2 interaction between sex and condition, which
          is essentially a t-test (or one degree of freedom for the numerator of F-statatistic).
          Multiple degrees of freedom for the numerator of F-statistic is currently unavailable.
          3) Similarly, the 4th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
          interaction between the three factors because 'emotion' has three levels. The F-test for
          the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.\n"
      
   ex2 <-
"--------------------------------
Example 2 --- two between-subjects (genotype and sex), onewithin-subject
(emotion) factor, plus two quantitative variables (age and IQ).f

   3dMVM -prefix Example2 -jobs 24        \\
          -model  \"genotype*sex+age+IQ\"  \\
          -wsVars emotion                \\
          -qVars  \"age,IQ\"               \\
          -qVarCenters '25,105'          \\
          -num_glt 10                    \\
          -gltLabel 1 pos_F_vs_M   -gltCode 1 'sex : 1*female -1*male emotion : 1*pos' \\
          -gltLabel 2 age_pos_vs_neg -gltCode 2 'emotion : 1*pos -1*neg age :'           \\
          -gltLabel 3 genotype_by_sex -gltCode 3 'genotype : 1*TT -1*NN sex : 1*male -1*female' \\
          -gltLabel 4 genotype_by_sex_emotion -gltCode 4 'genotype : 1*TT -1*NN sex : 1*male -1*female emotion : 1*pos -1*neg' \\
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

   NOTE:  1) The model for the analysis can be also set up as and is equivalent to 
          'genotype*sex*emotion+age+IQ'.
          2) The 3rd GLT is for the 2-way 2 x 2 interaction between genotype and sex, which
          is essentially a t-test (or one degree of freedom for the numerator of F-statatistic).
          Multiple degrees of freedom for the numerator of F-statistic is currently unavailable.
          3) Similarly, the 4th GLT is a 3-way 2 x 2 x 2 interaction, which is a partial (not full)
          interaction between the three factors because 'emotion' has three levels. The F-test for
          the full 2 x 2 x 3 interaction is automatically spilled out by 3dMVM.\n"
     
   ex3 <-
"---------------------------------
Example 3 --- BOLD response was modeled with multiple basis functions at individual
subject level. In addition, there are one between-subjects (Group) and one within-
subject (Condition) variable. Furthermore, the variable corresponding to the number 
of basis functions, Time, is also a within-subject variable. In the end, the F-
statistics for the interactions of Group:Condition:Time, Group:Time, and 
Condition:Time are of specific interest. And these interactions can be further
explored with GLTs in 3dMVM.

   3dMVM -prefix Example3 -jobs 12   \\
         -model Group               \\
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

   NOTE:  The model for the analysis can also be set up as and is equivalent to 
          'Group*Condition*Time'.\n"   
   
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
   "         no view+suffix needed. Filename for NIfTI format should have",
   "         .nii attached, while file name for surface data is expected",
   "         to end with .niml.dset\n", sep = '\n'
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

      '-model' = apl(n = 1, d = 1, h = paste(
   "-model FORMULA: Specify the fixed effects for between-subjects factors ",
   "         and quantitative variables. The expression FORMULA with more",
   "         than one variable has to be surrounded within (single or double)",
   "         quotes. Variable names in the formula should be consistent with",
   "         the ones used in the header of -dataTable. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. The effects of within-subject",
   "         factors, if present under -wsVars are automatically assumed",
   "         to interact with the ones specified here. Subject as a variable",
   "         should not occur in the model specifiction here.\n", sep = '\n'
             ) ),

      '-wsVars' = apl(n=c(1,100), d=NA, h = paste(
   "-wsVars FORMULA: Within-subject factors, if present, have to be listed",
   "         here; otherwise the program will choke. If no within-subject ",
   "         exists, don't include this option in the script. Coding for",
   "         additive effects and interactions is the same as in -model. The",
   "         FORMULA with more than one variable has to be surrounded ",
   "         within (single or double) quotes. Note that the within-subject",
   "         variables are assumed to interact with those between-subjects",
   "         variables specified under -model. The hemodynamic response",
   "         time course are better modeled as simultaneous outcomes through",
   "         option -mVar, and not as the levels of a within-subject factor.",
   "         The varialbes under -wsVars and -mVar are exclusive from each",
   "         other.\n", sep = '\n'
                             ) ),

      '-mVar' = apl(n=c(1,100), d=NA, h = paste(
   "-mVar variable: The levels of the variable or factor will be treated",
   "         simultaneous variables in a multivariate model. For example, when",
   "         the hemodynamic response time course is modeled through multiple",
   "         basis functions such as TENT, TENTzero, CSPLIN, CSPLINzero, SPMG2/3,",
   "         etc., the effect estimates at the multiple time points can be treated",
   "         simultaneous response variables in a multivariate model. Only one",
   "         variable is allowed currently under -mVar. In addition, at the",
   "         presence of -mVar, no other within-subject factors should be included.",
   "         In other words, -wsVars and -mVar are currently exclustive with each",
   "         other. If modeling extra within-subject factors with -mVar is",
   "         desirable, consider flattening such factors; that is, perform multiple",
   "         analyses at each level or their contrasts of the factor. The output for",
   "         multivariate testing are labeled with -MV- in the sub-brick names. \n", sep = '\n'
                             ) ),

       '-SC' = apl(n=0, h = paste(
   "-SC: If a within-subject factor with more than *two* levels is involved in the",
   "         the model, 3dMVM automatically provides the F-statistics for main and",
   "         interaction effects with sphericity assumption. If the assumption is",
   "         violated, the F-statistics could be inflated to some extent. This option",
   "         will enable 3dMVM to additionally output the F-statistics of",
   "         sphericity correction for main and interaction effects, which are",
   "         labeled with -SC- in the sub-brick names. \n", sep='\n')),

      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars variable_list: Identify quantitative variables (or covariates) with",
   "         this option. The list with more than one variable has to be",
   "         separaated with comma (,) without any other characters such as",
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
   "         but not 3dLME.\n",
             sep = '\n'
             ) ),

     '-qVarCenters' = apl(n=1, d=NA, h = paste(
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

     '-num_glt' = apl(n=1, d=0, h = paste(
   "-num_glt NUMBER: Specify the number of general linear tests (GLTs). A glt",
   "         is a linear combination of a factor levels. See details in ",
   "         -gltLabel.\n", sep = '\n'
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
   "         NOTICE:",
   "         1) The weights for a variable do not have to add up to 0.",   
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate.",
   "         3) The effect for a quantitative variable can be specified with,",
   "         for example, 'Group : 1*Old Age : ', or ",
   "         'Group : 1*Old - 1*Young Age : '", 
   "         4) The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLT.",
   "         5) The appearance of a categorial variable has to be followed",
   "         by the linear combination of its levels. Only a quantitative",
   "         is allowed to have a dangling coding as seen in 'Age :'\n",         
             sep = '\n'
             ) ),

     '-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure with a header as the first line.",
   "         NOTE: this option has to occur last; that is, no other options are",
   "         allowed thereafter. Each line should end with a backslash except for",
   "         the last line.\n",
   "         The first column is fixed with 'Subj', and the last is reserved",
   "         for 'InputFile'. Each row should contain only one effect estimate",
   "         in the table of long format (cf. wide format) as defined in R. The",
   "         level labels of a factor should contain at least one character.",
   "         Input files can be in AFNI, NIfTI or surface format. AFNI files",
   "         can be specified with sub-brick selector(square brackets [] within",
   "         quotes) specified with a number oflabel. Unequal number of subjects",
   "         across groups is allowed, butsituations with missing data for a",
   "         within-subject factor are better handled with 3dLME.\n", 
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
      lop <- list (com_history = com_history)
      lop$nNodes <- 1
      lop$model  <- 1
      lop$maskFN <- NA

      lop$wsVars <- NA
      lop$mVar   <- NA
      lop$qVars  <- NA
      lop$qVarCenters    <- NA
      lop$num_glt <- 0
      lop$gltLabel <- NULL
      lop$gltCode  <- NULL
      lop$dataTable <- NULL

      lop$SC <- FALSE
      lop$iometh <- 'clib'
      lop$verb <- 0

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
             mask = lop$maskFN <- ops[[i]],
             jobs   = lop$nNodes <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             wsVars = lop$wsVars  <- ops[[i]],
             mVar = lop$mVar  <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             num_glt = lop$num_glt <- ops[[i]],
             gltLabel = lop$gltLabel <- ops[[i]],
             gltCode  = lop$gltCode <- ops[[i]],
             dataTable  = lop$dataTable <- ops[[i]],
             
             help = help.MVM.opts(params, adieu=TRUE),

             SC  = lop$SC <- TRUE,
             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }


   return(lop)
}# end of read.MVM.opts.batch

# construct a glt list for testInteraction in phia
# how to deal with basis functions?????????????
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

# test
# gltConstr(clist[[1]], lop$dataStr)


#Change options list to 3dMVM variable list 
process.MVM.opts <- function (lop, verb = 0) {
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
   an <- parse.AFNI.name(lop$outFN)
   if(an$type != 'BRIK' && lop$iometh != 'clib') 
      errex.AFNI(c('Must of use -cio option with any input/output ',
                   'format other than BRIK'))

   if(!is.na(lop$qVars[1])) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]

   if(!is.null(lop$gltLabel)) {
      sq <- as.numeric(unlist(lapply(lop$gltLabel, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glt)))) {
         lop$gltLabel <- unlist(lapply(lop$gltLabel, '[', 2))
         lop$gltLabel[sq] <- lop$gltLabel
      } else errex.AFNI(c('The number of -gltLabel is not consistent with that \n',
         'specified by -num_glt ',  lop$num_glt))
   }

  if(!is.null(lop$gltCode)) {
      sq <- as.numeric(unlist(lapply(lop$gltCode, '[', 1)))
      if(identical(sort(sq), as.numeric(seq(1, lop$num_glt)))) {
         lop$gltCode <- lapply(lop$gltCode, '[',-1)
         lop$gltCode[sq] <- lop$gltCode
      } else errex.AFNI(c('The number of -gltLabel is not consistent with that \n',
         'specified by -num_glt ',  lop$num_glt))
   }         

   len <- length(lop$dataTable)
   wd <- which(lop$dataTable == "InputFile")
   hi <- len / wd - 1
   #browser()
   if(len %% wd != 0)
      errex.AFNI('The content under -dataTable is not rectangular!') else {
      lop$dataStr <- NULL
      for(ii in 1:wd) 
         lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      # wow, terrible mistake here with as.numeric(lop$dataStr[,jj])
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
      if(!is.na(lop$qVars[1])) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      # or if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(levels(lop$dataStr[,jj]))[as.integer(lop$dataStr[,jj])]
   }

   
   if (lop$num_glt > 0) {
      lop$gltList    <- vector('list', lop$num_glt)
      lop$slpList    <- vector('list', lop$num_glt)
      for (n in 1:lop$num_glt) {
         #if(!is.na(lop$qVars)) { if(any(lop$QV %in% lop$gltCode[[n]])) {
         if(!is.na(lop$qVars) & any(lop$QV %in% lop$gltCode[[n]])) {
            QVpos <- which(lop$gltCode[[n]] %in% lop$QV)
            lop$gltList[[n]]   <- gltConstr(lop$gltCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
            lop$slpList[[n]] <- lop$gltCode[[n]][QVpos]   
         } else lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr)
         #} else lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr)
      }
   }
   
   if(lop$iometh == 'Rlib') 
      lop$outFN <- paste(lop$outFN, "+tlrc", sep="") else {
      an <- parse.AFNI.name(lop$outFN)
      if(an$type == "BRIK" && an$ext == "" && is.na(an$view))
         lop$outFN <- paste(lop$outFN, "+tlrc", sep="")      
      if (exists.AFNI.name(lop$outFN) || 
          exists.AFNI.name(modify.AFNI.name(lop$outFN,"view","+tlrc")))
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
   }

   if(lop$nNodes < 1) lop$nNodes <- 1


   if(!is.na(lop$maskFN)) {
      if(verb) cat("Will read ", lop$maskFN,'\n')
      if(is.null(mm <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh))) {
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
################# MVM Computation functions ##############################
#################################################################################

# used only in read.MVM.opts.from.file
scanLine <- function(file, lnNo=1, marker="\\:")
   unlist(strsplit(unlist(scan(file, what= list(""), skip=lnNo-1, 
   strip.white=TRUE, nline=1)), marker))[2]

# function to obtain multivariate ANOVA
# may totally aboandon this and switch to linearHypothesis
# Pillai test only now
# does this work for multiple between-subjects variables (factors and quantitative variables)?????????
#maov <- function(mvfm)  # Pillai test with type = 3, need to remove the intercept: -1 below
#   return(stats:::Pillai(Re(eigen(qr.coef(qr(mvfm$SSPE), mvfm$SSP[[-1]]), symmetric = FALSE)$values), mvfm$df[-1], mvfm$error.df))
                                                
#maov <- function(mvfm) { # with type = 3, need to remove the intercept: -1 below
#   return(stats:::Pillai(Re(eigen(qr.coef(qr(mvfm$SSPE), mvfm$SSP[[-1]]), symmetric = FALSE)$values), mvfm$df[-1], mvfm$error.df))
#}

# general linear test for between-subjects variables only
# takes components from Anova(fm$lm, type=3, test='Pillai') as input  
maov <- function(SSPE, SSP, DF, error.DF)  # Pillai test with type = 3, need to remove the intercept: -1 below
   return(stats:::Pillai(Re(eigen(qr.coef(qr(SSPE), SSP), symmetric = FALSE)$values), DF, error.DF))
                                                
runAOV <- function(inData, dataframe, ModelForm, pars) {
   maov <- function(SSPE, SSP, DF, error.DF)  # Pillai test with type = 3, need to remove the intercept: -1 below
      return(stats:::Pillai(Re(eigen(qr.coef(qr(SSPE), SSP), symmetric = FALSE)$values), DF, error.DF)) 
   out <- pars[[1]]
   options(warn = -1)
   if (!all(abs(inData) < 10e-8)) {       
      dataframe$Beta<-inData
      fm <- NULL
      try(fm <- aov.car(ModelForm, data=dataframe, factorize=FALSE, return='full'), silent=TRUE)
      if(!is.null(fm)) {
            uvfm <- tryCatch(univ(fm$Anova), error=function(e) NULL)   # univariate model 
            if(!is.null(uvfm)) {
               if(pars[[6]][1] & pars[[7]]) {  # between-subjects factors/variables only
                  tryCatch(Fvalues <- uvfm[1:pars[[2]][1],3], error=function(e) NULL)
                  if(!is.null(Fvalues)) if(!any(is.nan(Fvalues))) out[1:pars[[2]][2]] <- Fvalues
               }   else {# contain within-subject variable(s)
                  #tryCatch(Fvalues <- unname(uvfm$anova[-1,5]), error=function(e) NULL)
                  tryCatch(Fvalues <- unname(uvfm$anova[,5]), error=function(e) NULL)
                  if(!is.null(Fvalues)) if(!any(is.nan(Fvalues))) {
                     out[1:pars[[2]][2]] <- Fvalues  # univariate Fs: not spherecity correction
                     if(pars[[6]][2]) { # sphericity correction
                        getGG <- uvfm$sphericity.correction[,'HF eps'] < pars[[8]][1]
                        GG    <- uvfm$sphericity.correction[,'Pr(>F[GG])']
                        HF    <- uvfm$sphericity.correction[,'Pr(>F[HF])']
                        #Fsc  <- ifelse(getGG, GG, HF)
                        Fsc  <- ifelse(uvfm$mauchly[, 'p-value'] < 0.05, ifelse(getGG, GG, HF), uvfm$anova[, 'Pr(>F)'])
                        tryCatch(out[(pars[[2]][2]+1):(pars[[2]][2]+pars[[2]][3])] <-
                           qf(Fsc, pars[[8]][[2]], pars[[8]][[3]], lower.tail = FALSE), error=function(e) NULL)                                
                           #qf(Fsc, uvfm$anova[dimnames(uvfm$sphericity.correction)[[1]], 'num Df'],
                           #   uvfm$anova[dimnames(uvfm$sphericity.correction)[[1]], 'den Df'],
                           #   lower.tail = FALSE), error=function(e) NULL)
                  } #if(pars[[6]][2])     
               } #if(!any(is.nan(Fvalues)))
            } #if(pars[[6]][1] & pars[[7]])
         } #if(!is.null(uvfm))   
         if(!pars[[7]]) { # reall MVM: currently not allowed to mix within-subject variables
            tryCatch(mvfm <- Anova(fm$lm, type=3, test='Pillai'), error=function(e) NULL)  # need to add options for type and test!
            #if(pars[[6]][1]) tryCatch(out[(pars[[2]][2]+pars[[2]][3]+1):pars[[2]][1]] <- maov(mvfm)[2], error=function(e) NULL)
            for(ii in 1:pars[[2]][4]) # pars[[2]][4] equals length(mvfm$terms)
               tryCatch(out[pars[[2]][2]+pars[[2]][3]+ii] <-
                  maov(mvfm$SSPE, mvfm$SSP[[ii]], mvfm$df[ii], mvfm$error.df)[2], error=function(e) NULL)
         }  #if(!pars[[7]])                            
         if(pars[[3]]>=1) for(ii in 1:pars[[3]]) {
            glt <- tryCatch(testInteractions(fm$lm, custom=pars[[4]][[ii]], slope=pars[[5]][[ii]], 
               adjustment="none", idata = fm[["idata"]]), error=function(e) NULL)           
            if(!is.null(glt)) {
               out[pars[[2]][1]+2*ii-1] <- glt[1,1]
	       out[pars[[2]][1]+2*ii]   <- sign(glt[1,1]) * sqrt(glt[1,4])  # convert F to t
            } #if(!is.null(glt))
         } #if(pars[[3]]>=1) for(ii in 1:pars[[3]])
      }
   }
   return(out)
}

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
      save(args, rfile, file=".3dMVM.dbg.AFNI.args", ascii = TRUE) 
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
      
      #str(lop);
      if(is.null(lop <- process.MVM.opts(lop, verb = lop$verb))) 
         stop('Error processing input')
      
   }
   #if(lop$verb > 1) { 
      #Too much output, big dump of header structs of input dsets..
   #   str(lop)
   #}


########################################################################

if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])
                                                
require("afex")
require("phia")

#comArgs <- commandArgs()

#if(length(comArgs)<6) modFile <- "model.txt" else
#modFile <- comArgs[6]
#paste(commandArgs())


# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
if(is.na(lop$mVar)) {
   if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj)')) else
      ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$wsVars, '))')) 
} else 
   if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$mVar, '))')) else
      ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$wsVars, '*', lop$mVar, '))')) 

                                                
# Maybe not list for these two, or yes?
lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)

# center on user-speficied value or mean
if(any(!is.na(lop$qVars))) if(all(is.na(lop$qVarCenters))) 
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F) else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$qVarCenters, scale=F)


cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of data structure *****\n')
#print(sprintf('%i subjects: ', nlevels(lop$dataStr$Subj)))
#for(ii in 1:nlevels(lop$dataStr$Subj)) print(sprintf('%s ', levels(lop$dataStr$Subj)[ii]))

cat(nlevels(lop$dataStr$Subj), 'subjects : ', levels(lop$dataStr$Subj), '\n')
cat(length(lop$dataStr$InputFile), 'response values\n')
for(ii in 2:(dim(lop$dataStr)[2]-1)) if(class(lop$dataStr[,ii]) == 'factor')
   cat(nlevels(lop$dataStr[,ii]), 'levels for factor', names(lop$dataStr)[ii], ':', 
   levels(lop$dataStr[,ii]), '\n') else if(class(lop$dataStr[,ii]) == 'matrix' | class(lop$dataStr[,ii]) == 'numeric')  # numeric may not work
   cat(length(lop$dataStr[,ii]), 'centered values for numeric variable', names(lop$dataStr)[ii], ':', lop$dataStr[,ii], '\n')
cat(lop$num_glt, 'post hoc tests\n')

cat('\nTabulation of subjects against all categorical variables')
all_vars <- names(lop$dataStr)
for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
   cat('\n~~~~~~~~~~~~~~')
   cat('\nSubj vs ', var, ':\n', sep='')
   print(table(lop$dataStr$Subj, lop$dataStr[,var]))
}

cat('\n***** End of data structure information *****\n')   
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
inData <- read.AFNI(lop$dataStr[1, FileCol], verb=lop$verb, meth=lop$iometh)
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
# for writing output purpose
head <- inData

# ww <- inData$NI_head
#myHist <- inData$header$HISTORY_NOTE; myOrig <- inData$origin; myDelta <- inData$delta
# Read in all input files
inData <- unlist(lapply(lapply(lop$dataStr[,FileCol], read.AFNI, verb=lop$verb, meth=lop$iometh), '[[', 1))
dim(inData) <- c(dimx, dimy, dimz, NoFile)

if (!is.na(lop$maskFN)) {
	Mask <- read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh)$brk[,,,1]
	inData <- array(apply(inData, 4, function(x) x*read.AFNI(lop$maskFN, verb=lop$verb, meth=lop$iometh)$brk[,,,1]),
      dim=c(dimx,dimy,dimz,NoFile))
}

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

cat('If the program hangs here for more than, for example, half an hour,\n')
cat('kill the process because the model specification or the GLT coding\n')
cat('is likely inappropriate.\n\n')
                                                
while(is.null(fm)) {
   fm<-NULL
   lop$dataStr$Beta<-inData[ii, jj, kk,]
   options(warn=-1)     
   try(fm <- aov.car(ModelForm, data=lop$dataStr, factorize=FALSE, return='full'), silent=TRUE)
#   if(!is.null(fm)) if(!is.na(lop$mVar)) {
#      if(is.na(lop$wsVars)) mvfm <- Anova(fm$lm, type=2, test='Pillai')
#   } else {
#      uvfm <- univ(fm[[1]])  # univariate modeling
#   } # need to add options for type and test!

   if(!is.null(fm)) {
      uvfm <- univ(fm$Anova)  # univariate modeling
      if(!is.na(lop$mVar)) if(is.na(lop$wsVars)) mvfm <- Anova(fm$lm, type=3, test='Pillai')
   } 

   if(!is.null(fm)) if (lop$num_glt > 0) {
      n <- 1
      while(!is.null(fm) & (n <= lop$num_glt)) {
        gltRes[[n]] <- tryCatch(testInteractions(fm$lm, custom=lop$gltList[[n]], slope=lop$slpList[[n]], 
            adjustment="none", idata = fm[["idata"]]), error=function(e) NA)
         if(is.na(gltRes[[n]])) fm <- NULL
         n <- n+1
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
      cat('1) Inappropriate model specification with options -model, -wsVars, or -qVars.\n')
      cat('Note that within-subject or repeated-measures variables have to be declared\n')
      cat('with -wsVars.\n\n')
      cat('2) Misspecifications in general linear test coding with -gltCode.\n\n')
      cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n\n')
      cat('4) Inconsistent variable names which are case sensitive. For example, factor\n')
      cat('named Group in model specifiction and then listed as group in the table hader\n')
      cat('would cause grief for 3dMVM.\n')
      cat('5) Not enough number of subjects. This may happen when there are two or more\n')
      cat('withi-subject factors. For example, a model with two within-subject factors with\n')
      cat('m and n levels respectively requires more than (m-1)*(n-1) subjects to be able to\n')
      cat('model the two-way interaction with the multivariate approach.\n\n')
      errex.AFNI("Quitting due to model test failure...")
   }
}

#if(!is.null(fm))  {
#   print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
#   } else { 
#      errex.AFNI(sprintf("Ouch, model testing failed! Multiple reasons could cause the failure.... \n"))
#                "might be inappropriate, or there are incorrect specifications in model.txt \n",
#                "such as contrasts, factor levels, or other obscure problems.", lop$model))
      #break; next # won't run the whole brain analysis if the test fails
#}


#fm <- aov.car(ModelForm, data=Model, factorize=FALSE, return='lm')
#bm <- c(1,-1)%*%t(aa$lm$coefficients)
#Vm <- c(1,-1)%*%vcov(aa$lm)%*%c(1,-1)
#t(bm)%*%solve(Vm)%*%bm

# number of terms (or F-stats): the output structure is different without within-subject variables
# assuming univariate testing for within-subject variables here! May need to change later

    
#if(is.na(lop$mVar))
#   nF <- ifelse(is.na(lop$wsVars), dim(uvfm)[1]-2, dim(uvfm$anova)[1]-1) else
#   if(is.na(lop$wsVars)) nF <- length(mvfm$terms) else {}


# Remove this later!!!!!!!!!!!!!!!!!!!!!!!!!
#SC <- TRUE
                                                
if(is.na(lop$wsVars) & is.na(lop$mVar)) nFsc <- 0 else if(lop$SC) {
   corTerms <- rownames(uvfm$sphericity.correction)
   allTerms <- rownames(uvfm$anova)[-1]
   nFsc <- length(corTerms)  # number of F-stat for spherecity correction                               
} else nFsc <- 0
                                                
#ifelse(is.na(lop$wsVars) & is.na(lop$mVar), 0, 
                                                
# number of F-stat for univariate modeling 
nFu <- ifelse(is.na(lop$wsVars) & is.na(lop$mVar), dim(uvfm)[1]-1, dim(uvfm$anova)[1])
# nFm: number of F-stat for real MVM
if(!is.na(lop$mVar)) if(is.na(lop$wsVars))
   nFm <- length(mvfm$terms) else nFm <- 0 else nFm <- 0
nF <- nFu + nFsc + nFm 
                                                
NoBrick <- nF + 2*lop$num_glt
outInit <- rep(0, NoBrick)  # initialization for the voxel-wise output

if(is.na(lop$wsVars) & is.na(lop$mVar)) brickNames <-
   paste(dimnames(uvfm)[[1]][1:(length(dimnames(uvfm)[[1]])-1)], 'F') else {
   brickNames <- paste(dimnames(uvfm$anova)[[1]], 'F')
   if(lop$SC) brickNames <- c(brickNames, paste(corTerms, '-SC-', 'F'))
}                                             
#brickNames <- ifelse(is.na(lop$wsVars) & is.na(lop$mVar),
#          paste(dimnames(uvfm)[[1]][2:(length(dimnames(uvfm)[[1]])-1)], 'F'),
#          paste(dimnames(uvfm$anova)[[1]][-1], 'F'))

if(!is.na(lop$mVar))                                                
   brickNames <- c(brickNames, paste(mvfm$terms, '-MV-', 'F'))
                                                
for(ii in 1:lop$num_glt) {
   brickNames <- c(brickNames, lop$gltLabel[ii])
   brickNames <- c(brickNames, paste(lop$gltLabel[ii], 't'))
}

if(lop$SC) {
   scTerms <- dimnames(uvfm$anova)[[1]] %in% corTerms
   numDF <- uvfm$anova[,'num Df'][scTerms]
   denDF <- uvfm$anova[,'den Df'][scTerms]
} else {
   numDF <- NULL
   denDF <- NULL
}
                                                
# DFs for t-stat 
t_DF <- NULL   
if(lop$num_glt>0) for(ii in 1:lop$num_glt)
   t_DF <- c(t_DF, ifelse(is.na(lop$wsVars) & is.na(lop$mVar), gltRes[[ii]][2,2], gltRes[[ii]][,6]))

# DFs for F-stat
F_DF <- vector('list', nF)
for(ii in 1:nFu) if(is.na(lop$mVar) & is.na(lop$wsVars))
   F_DF[[ii]] <- c(uvfm[ii, 'Df'], uvfm[1+nF, 'Df']) else
   F_DF[[ii]] <- c(unname(uvfm$anova[ii,'num Df']), unname(uvfm$anova[ii,'den Df'])) # skip the intercept: ii+1

if(nFsc > 0) for(ii in 1:nFsc) F_DF[[nFu+ii]] <- c(numDF[ii], denDF[ii])

if(nFm > 0) for(ii in 1:nFm) {
   mvtest <- maov(mvfm$SSPE, mvfm$SSP[[ii]], mvfm$df[ii], mvfm$error.df)
   F_DF[[nFu+nFsc+ii]] <- c(mvtest[3], mvtest[4]) 
}
   
###############################

pars <- vector("list", 9)
#pars[[1]] <- NoBrick
pars[[1]] <- outInit                                              
#pars[[2]] <- nF
pars[[2]] <- c(nF, nFu, nFsc, nFm, numDF, denDF)                                              
pars[[3]] <- lop$num_glt
pars[[4]] <- lop$gltList
pars[[5]] <- lop$slpList
pars[[6]] <- c(is.na(lop$wsVars), lop$SC) # any within-subject factors?
pars[[7]] <- is.na(lop$mVar)   # any real multivariate modeling: currently for basis functions
if(lop$SC) pars[[8]] <- list(0.75, numDF, denDF) # switching threshold between GG and HF: 0.6
                                                
#runAOV(inData[ii, jj, kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

#for(ii in 1:dimx) for(jj in 1:dimy) 
#   aa<-runAOV(inData[ii,jj,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)

if(dimy == 1 & dimz == 1) {
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
   out <- array(0, dim=c(dimx_n, nSeg, NoBrick))
   # break input multiple segments for parrel computation
   dim(inData) <- c(dimx_n, nSeg, NoFile)
   if (lop$nNodes==1) for(kk in 1:nSeg) {
      if(NoBrick > 1) out[,kk,] <- aperm(apply(inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), c(2,1)) else
         out[,kk,] <- aperm(apply(inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), dim=c(dimx_n, 1))
      cat("Computation done: ", 100*kk/nSeg, "%", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   
   if (lop$nNodes>1) {
   library(snow)
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterEvalQ(cl, library(afex)); clusterEvalQ(cl, library(phia))
   for(kk in 1:nSeg) {
      if(NoBrick > 1) out[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), c(2,1)) else
      out[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runAOV, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), dim=c(dimx_n, 1))
      cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   stopCluster(cl)
   }
   # convert to 4D
   dim(out) <- c(dimx_n*nSeg, 1, 1, NoBrick)
   # remove the trailers (padded 0s)
   out <- out[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]
} else {

# Initialization
out <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

# set up a voxel @ ii jj kk to test
#runAOV(inData[ii, jj, kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)

if (lop$nNodes==1) for (kk in 1:dimz) {
   if(NoBrick > 1) out[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars), c(2,3,1)) else
      out[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars), dim=c(dimx, dimy, 1))      
   cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 


if (lop$nNodes>1) {
   library(snow)
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterEvalQ(cl, library(afex)); clusterEvalQ(cl, library(phia))
   #clusterCall(cl, maov) # let all clusters access to function maov()
   for (kk in 1:dimz) {
      if(NoBrick > 1) out[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars), c(2,3,1)) else
         out[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars), dim=c(dimx, dimy, 1))
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
#      out[,,kk,] <-aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars), c(2,3,1))
#      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
#   stopCluster(cl)
#}

###############################
#if(is.na(lop$mVar)) {
#if(is.na(lop$wsVars)) outLabel <- paste(dimnames(uvfm)[[1]][2:(1+nF)], " F") else
#   if(pars[[6]][2]) { # sphericity correction for within-subject factors
#      outLabel <- NULL
#      for(ii in (1:nF)[-pars[[9]]+1]) outLabel <- append(outLabel, sprintf("%s F", dimnames(uvfm$lm)[[1]][ii+1]))
#      for(ii in (pars[[9]]-1)) outLabel <- append(outLabel, sprintf("%s Z", dimnames(uvfm[[1]])[[1]][ii+1])) 
#   } else
#   outLabel <- paste(dimnames(uvfm[[1]])[[1]][-1], " F")
#} else {
#if(is.na(lop$wsVars)) outLabel <- paste(mvfm$terms[-1], " F") else  # need improvement here for multivariate case
#   {}
#}
   
#for(ii in 1:lop$num_glt) {
#   outLabel <- append(outLabel, sprintf("%s", lop$gltLabel[ii]))
#   outLabel <- append(outLabel, sprintf("%s:t", lop$gltLabel[ii]))
#}  

statpar <- "3drefit"
for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ", F_DF[[ii]][1], F_DF[[ii]][2])
if(lop$num_glt>0) for(ii in 1:lop$num_glt)
   statpar <- paste(statpar, " -substatpar ", nF+2*ii-1, " fitt", t_DF[ii])
   
#if(is.na(lop$mVar)) {
#   if(is.na(lop$wsVars)) for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ",
#      uvfm[ii+1, 'Df'], " ", uvfm[2+nF, 'Df']) else if(pars[[6]][2]) {  # sphericity correction for within-subject factors
#      for(ii in (1:nF)[-pars[[9]]+1]) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ")
#      for(ii in (pars[[9]]-1)) statpar <- paste(statpar, " -substatpar ", ii-1, " fizt ") } else
#      for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ",
#                          unname(uvfm[[1]][ii+1,'num Df']), " ", unname(uvfm[[1]][ii+1,'den Df'])) 
#} else {  
#   if(is.na(lop$wsVar)) {
#      for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ", maov(mvfm)[3],  maov(mvfm)[4])
#   } else {}
#}

#if(lop$num_glt>0) for(ii in 1:lop$num_glt) statpar <- paste(statpar, " -substatpar ", nF+2*ii-1, " fitt", 
#   ifelse(is.na(lop$wsVars) & is.na(lop$mVar), gltRes[[ii]][2,2], gltRes[[ii]][,6]))    
statpar <- paste(statpar, " -addFDR -newid ", lop$outFN)
   
#write.AFNI(lop$outFN, out, outLabel, note=myHist, origin=myOrig, delta=myDelta, idcode=newid.AFNI())
#write.AFNI(lop$outFN, out, outLabel, defhead=head, idcode=newid.AFNI(), type='MRI_short')
write.AFNI(lop$outFN, out, brickNames, defhead=head, idcode=newid.AFNI(), type='MRI_short')
                                                
system(statpar)
print(sprintf("Congratulations! You've got an output %s", lop$outFN))
                                                      

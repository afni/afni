#!/usr/bin/env AFNI_Batch_R


first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- '3dLME'


#################################################################################
##################### Begin 3dLME Input functions ################################
#################################################################################


greeting.lme <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 3dlme ==================          
   AFNI Group Analysis Program with Linear Mixed-Effcts Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.7, May 6, 2013
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/LME.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )


#The help function for 3dLME batch (AFNI-style script mode)
help.LME.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
          ================== Welcome to 3dLME ==================          
    AFNI Group Analysis Program with Multi-Variate Modeling Approach
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.0.6, May 6, 2013
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/LME.html
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
 variables are also in the ouput. In addition, general linear tests (GLTs) can 
 be requested via symbolic coding.
 
 If you want to cite the analysis approach, use the following:
 
 Chen, G., Saad, Z.S., Britton, J.C., Pine, D.S., Cox, R.W. (2013). Linear
 Mixed-Effects Modeling Approach to FMRI Group Analysis. NeuroImage,
 http://dx.doi.org/10.1016/j.neuroimage.2013.01.047
 
 Input files for 3dLME can be in AFNI, NIfTI, or surface (niml.dset) format.
 
 In addition to R installtion, the following two R packages need to be acquired
 in R first before running 3dLME:
 
 install.packages("nlme")
 install.packages("contrast")

 The snow package is also needed if one wants to take advantage of parallel 
 computing:
 
 install.packages("snow")
 
 More details about 3dLME can be found at 
 http://afni.nimh.nih.gov/sscc/gangc/LME.html
 
 Thank the R community for the strong technical support.'

   ex1 <- 
"
Example 1 --- one condition modeled with 8 basis functions (e.g., TENT or TENTzero)
for one group of 13 subjects:
--------------------------------
   3dLME -prefix myTest -jobs   4               \\
         -model '0+Time'                        \\
         -qVars order                           \\
         -qVarCenters 0                         \\
         -ranEff 1                              \\
         -corStr 'order : AR1'                  \\
         -SS_type 3                             \\
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
          -num_glt 2                                                       \\
          -gltLabel 1 'House-Face' -gltCode  1 'cond : 1*House -1*Face'    \\
          -gltLabel 2 'Face-House' -gltCode  2 'cond : 1*Face -1*House'    \\
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
          -gltLabel 2 'neg-neu' -gltCode  2 'cond : 1*neg -1*neu'      \\
          -gltLabel 3 'pos-neg' -gltCode  3 'cond : 1*pos -1*neg'      \\
          -gltLabel 4 'pat_pos-neu' -gltCode  4 'cond : 1*pos -1*neu group : 1*pat'    \\
          -gltLabel 5 'pat_neg-neu' -gltCode  5 'cond : 1*neg -1*neu group : 1*pat'    \\
          -gltLabel 6 'pat_pos-neg' -gltCode  6 'cond : 1*pos -1*neg group : 1*pat'    \\
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
   "-model FORMULA: Specify the terms of fixed effects for all explanatory,",
   "         including quantitative, variables. The expression FORMULA with more",
   "         than one variable has to be surrounded within (single or double)",
   "         quotes. Variable names in the formula should be consistent with",
   "         the ones used in the header of -dataTable. A+B represents the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. The effects of within-subject",
   "         factors, if present under -wsVars are automatically assumed",
   "         to interact with the ones specified here. Subject should not",
   "         occur in the model specifiction here.\n", sep = '\n'
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
   "         double) quotes.\n", sep = '\n'
                             ) ),

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
   "         3) Within-subject covariates are better modeled with 3dLME.\n",
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

     '-SS_type' = apl(n=1, d=3, h = paste(
   "-SS_type NUMBER: Specify the type for sums of squares in the F-statistics.",
   "         Two options are currently supported: sequential (1) and marginal (3).\n ",
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
   "         to be within (single or double) quotes. Currently only pairwise",
   "         comparison/contrasts are allowed (this may change in the future).",
   "         For example, the following setup",
   "         'Condition : 1*House -1*Face Emotion : 1*positive '",
   "         requests for a test of comparing House with Face condition while",
   "         Emotion is held at positive valence.\n",
   "         NOTICE:",
   "         1) Weights for a variable do not have to be equal and add up to 0.",   
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate.",
   "         3) Effects related a quantitative variable are automatically", 
   "         4) The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLT.",
             sep = '\n'
             ) ),

     '-dataTable' = apl(n=c(1, 10000), d=NA, h = paste(
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
   "         within-subject factor are betterhandled with 3dLME.",
   "         handled with 3dLME.\n", 
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
      lop$model  <- 1
      lop$maskFN <- NA

      lop$ranEff <- NA
      lop$qVars  <- NA
      lop$qVarCenters    <- NA
      lop$corStr <- NA
      lop$SS_type <- 3
      lop$num_glt <- 0
      lop$gltLabel <- NULL
      lop$gltCode  <- NULL
      lop$dataTable <- NULL
      
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
             ranEff = lop$ranEff  <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             corStr  = lop$corStr <- ops[[i]],
             SS_type = lop$SS_type <- ops[[i]],
             num_glt = lop$num_glt <- ops[[i]],
             gltLabel = lop$gltLabel <- ops[[i]],
             gltCode  = lop$gltCode <- ops[[i]],
             dataTable  = lop$dataTable <- ops[[i]],
             
             help = help.LME.opts(params, adieu=TRUE),

             cio = lop$iometh<-'clib',
             Rio = lop$iometh<-'Rlib'
             )
   }


   return(lop)
}# end of read.LME.opts.batch

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
# gltConstr(lop$gltCode[[1]], lop$dataStr)


# construct a list for contrast() in contrast: here only deals with factors???
# If a variable is *not* shown in the coding, it means that variable is collapsed!
contraConstr <- function(cStr, dataStr, fixedVars, QV) {
   pos <- which(cStr==":")  # colon positions: could be multiple colons
   vars  <- cStr[pos-1]     # vector of vars in the glt/contrast: could be multiple variables; if not here, collapse
   nvar <- length(fixedVars)     # number of variables involved in the model
   pos <- c(pos, length(cStr)+2) # add an artificial one for convenient usage below
   varsOK <- vars %in% colnames(dataStr)  # make sure the var names are correct
   #browser()
   if(all(varsOK)) {
      contrList <- vector('list', 2)   # list to store the two conditions for contrast
      for(ii in 1:2) {
         contrList[[ii]] <- vector('list', nvar)
         names(contrList[[ii]]) <- fixedVars
      }
      for(vv in fixedVars) {
         #browser()
	 if(vv %in% QV) for(ii in 1:2) contrList[[ii]][[vv]] <- 0 else {
            lvl <- levels(dataStr[,vv])
            if(vv %in% vars) {
               #browser()
               ii <- which(pos==which(cStr==vv)+1) # location of the closest colon
               sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)], strsplit, '\\*'))
               lvlInv <- sepTerms[seq(2,length(sepTerms),2)]   # levels involved
               lvlOK <- lvlInv %in% lvl
               #browser()
	       if(all(lvlOK)) {
                  if(length(lvlInv)==1) lvlInv <- c(lvlInv, lvlInv) # artificially add one so that it works 2 lines later
                  sq <- match(as.numeric(sepTerms[c(1,3)]), c(1, -1))
                  for(ii in 1:2) contrList[[ii]][[vv]] <- lvlInv[ii]
	            } else errex.AFNI(paste("Incorrect level coding in variable", vars[ii],
	               ": ", lvlInv[which(!lvlOK)], " \n   "))
            } else for(ii in 1:2) contrList[[ii]][[vv]] <- lvl
         } #if(vv %in% QV)        
      } #for(vv in fixedVars)
   } else errex.AFNI(paste("Incorrect variable name in GLT coding: ", vars[which(!varsOK)], " \n   "))
   return(contrList) 
}

# test
# contraConstr(lop$gltCode[[1]], lop$dataStr, fixedVars, lop$QV)


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
   an <- parse.AFNI.name(lop$outFN)
   if(an$type != 'BRIK' && lop$iometh != 'clib') 
      errex.AFNI(c('Must of use -cio option with any input/output ',
                   'format other than BRIK'))

   # assume the quantitative variables are separated by + here
   if(!is.na(lop$qVars)) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]

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
   
   if(len %% wd != 0)
      errex.AFNI('The content under -dataTable is not rectangular!') else {
      lop$dataStr <- NULL
      #browser()
      for(ii in 1:wd) 
         lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      # wow, terrible mistake here with as.numeric(lop$dataStr[,jj])
      #if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
      if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(as.character(lop$dataStr[,jj]))
      # or if(!is.na(lop$qVars)) for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(levels(lop$dataStr[,jj]))[as.integer(lop$dataStr[,jj])]
   }

   # number of fixed-effects variables involved in the model
   parseStr <- function(input, sep) unlist(strsplit(input, sep))
   fixedVars <- unique(parseStr(parseStr(parseStr(lop$model, "\\:"), "\\*"), "\\+"))
   nFix <- length(fixedVars)

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
   if (lop$num_glt > 0) {
      lop$gltList <- vector('list', lop$num_glt)  # list used by contrast()
      for (n in 1:lop$num_glt) lop$gltList[[n]] <- contraConstr(lop$gltCode[[n]], lop$dataStr, fixedVars, lop$QV)
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
################# LME Computation functions ##############################
#################################################################################

# used only in read.LME.opts.from.file
scanLine <- function(file, lnNo=1, marker="\\:")
   unlist(strsplit(unlist(scan(file, what= list(""), skip=lnNo-1, 
   strip.white=TRUE, nline=1)), marker))[2]


# heavy computation with voxel-wise analysis
runLME <- function(inData, dataframe, ModelForm, pars) {
   Stat <- rep(0, pars[[1]])
   if (!all(abs(inData) < 10e-8)) {        
      dataframe$Beta<-inData
      fm <- NULL
      if(!pars[[7]])
         try(fm <- lme(ModelForm, random = pars[[8]], dataframe, correlation=corAR1(0.3, form=pars[[9]])), silent=TRUE)
         # case of basis functions
      if(is.null(fm)) { # case of basis functions fails or other cases
         try(fm <- lme(ModelForm, random = pars[[8]], dataframe), silent=TRUE)
         if(is.null(fm)) try(fm <- lme(ModelForm, random = ~1|Subj, dataframe), silent=TRUE)
      } 
      if(is.null(fm) | class(fm$apVar)[1] == "character") {
         fm <- NULL
         try(fm <- gls(ModelForm, dataframe), silent=TRUE)
      }

      if(!is.null(fm)){      
         Stat[1:pars[[2]]] <- anova(fm, type=pars[[10]])$F[pars[[13]]] # F-stat		
         if(!pars[[7]]) { # basis functions
	    Stat[pars[[2]]+2*0.5:pars[[11]]] <- unname(summary(fm)$tTable[, "Value"])
            # unname(fm$coefficients$fixed) only works for lme, not for gls!!!
            # but unname(summary(fm)$tTable[, "Value"]) is immune to the type of models, lme or gls!
 	    Stat[pars[[2]]+2*1:pars[[11]]] <- unname(summary(fm)$tTable[,"t-value"])
         }
         if(pars[[3]] > 0) for(n in 1:pars[[3]]) { # assuming no glts for basis functions
	    con <- NULL
            try(con <- contrast(fm, pars[[4]][[n]][[1]], pars[[4]][[n]][[2]], type="average"),silent=TRUE) 
	    if(!is.null(con)) Stat[(pars[[2]]+2*n-1):(pars[[2]]+2*n)] <- c(con$Contrast, con$testStat)
         }
         if(pars[[5]] > 0) for (n in 1:pars[[5]]) {
            Stat[(pars[[2]]+2*pars[[3]]+2*n-1):(pars[[2]]+2*pars[[3]]+2*n)] <- 
               c(summary(fm)$tTable[pars[[12]][n], "Value"], summary(fm)$tTable[pars[[12]][n], "t-value"])
         }
      }
   }
   return(Stat)	
}
# test runLME(inData[1,2,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)      

#################################################################################
########################## Read information from a file #########################tryCatch(print(fm[[1]]), error=function(e) NULL)
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
      save(args, rfile, file=".3dLME.dbg.AFNI.args", ascii = TRUE) 
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



if(!is.na(lop$qVarCenters)) lop$qVarCenters <- as.numeric(strsplit(as.character(lop$qVarCenters), '\\,')[[1]])


require("nlme")
require("contrast")

#comArgs <- commandArgs()

#if(length(comArgs)<6) modFile <- "model.txt" else
#modFile <- comArgs[6]
#paste(commandArgs())


# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
#if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model)) else
   ModelForm <- as.formula(paste("Beta ~", lop$model)) 
   

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

cat('\nTabulation of subjects against all categorical variables')
all_vars <- names(lop$dataStr)
for(var in all_vars[-c(1, length(all_vars))]) if(!(var %in% lop$QV)) {
   cat('\n~~~~~~~~~~~~~~')
   cat('\nSubj vs ', var, ':\n', sep='')
   print(table(lop$dataStr$Subj, lop$dataStr[,var]))
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
# need to expand this later
#ranEff <- list(Subj=as.formula(c('~',lop$ranEff)))
nRanEff <- length(lop$ranEff)
ranEff <-  vector('list', nRanEff)
names(ranEff) <- rep('Subj', nRanEff)
for(n in 1:nRanEff) ranEff[[n]] <- as.formula(lop$ranEff[[n]])

if(!is.na(lop$corStr[1])) corStr <- as.formula(c('~', lop$corStr[1])) else corStr <- NA

while(is.null(fm)) {
   fm<-NULL
   lop$dataStr$Beta<-inData[ii, jj, kk,]
   options(warn=-1)     
   if(!is.na(lop$corStr[1])) try(fm <- lme(ModelForm, random=ranEff, data=lop$dataStr, 
      correlation=corAR1(0.3, form=corStr)), silent=TRUE) else try(fm <- lme(ModelForm, 
      random=ranEff, data=lop$dataStr), silent=TRUE)
   if(!is.null(fm)) if (lop$num_glt > 0) {
      n <- 1
      require(contrast)
      gltDF <- array(data=NA, dim=lop$num_glt)
      while(!is.null(fm) & (n <= lop$num_glt)) {
         gltDF[n] <- tryCatch(contrast(fm, lop$gltList[[n]][[1]], lop$gltList[[n]][[2]], type="average")$df,
            error=function(e) NA)
         if(is.na(gltDF[[n]])) fm <- NULL
         n <- n+1
      }      
   }
   if(!is.null(fm))  {
      print(sprintf("Great, test run passed at voxel (%i, %i, %i)!", ii, jj, kk))
   } else if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      cat('~~~~~~~~~~~~~~~~~~~ Model test failed  ~~~~~~~~~~~~~~~~~~~\n')
      cat('Possible reasons:\n\n')
      cat('1) Inappropriate model specification with options -model, -wsVars, or -qVars.\n')
      cat('Note that within-subject or repeated-measures variables have to be declared\n')
      cat('with -wsVars.\n\n')
      cat('2) Misspecifications in general linear test coding with -gltCode.\n\n')
      cat('3) Mistakes in data table. Check the data structure shown above, and verify\n')
      cat('whether there are any inconsistencies.\n')
      errex.AFNI("Quitting due to model test failure...")
      #break
   }
}


# number of terms (or F-stats): the output structure is different without within-subject variables
nF      <- ifelse(is.na(lop$corStr[1]), nrow(anova(fm))-1, nrow(anova(fm)))
nBasis  <- (!is.na(lop$corStr[1]))*nrow(summary(fm)$tTable)
if(is.null(lop$QV)) nCovVal <- 0 else nCovVal <- length(unique(unlist(sapply(lop$QV, grep, dimnames(summary(fm)$tTable)[[1]]))))
# nCovVal <- sum(grep(lop$QV, dimnames(summary(fm)$tTable)[[1]]))
nT      <- 2*(lop$num_glt + nBasis + nCovVal)
NoBrick <- nF + nT

###############################

pars      <- vector("list", 13)
pars[[1]] <- NoBrick                # total number of output values per voxel/node
pars[[2]] <- nF                     # total number of F-stat
pars[[3]] <- lop$num_glt            # total number of GLTs
pars[[4]] <- lop$gltList
pars[[5]] <- nCovVal
#pars[[6]] <- is.na(lop$wsVars)      # any within-subject factors?
pars[[7]] <- is.na(lop$corStr[1])   # correlationn structure?
pars[[8]] <- ranEff
pars[[9]] <- corStr                 # correlation structure for basis function modeling
pars[[10]]<- ifelse(lop$SS_type==3, 'marginal', 'sequential')
pars[[11]]<- (!is.na(lop$corStr[1]))*nrow(summary(fm)$tTable)   # number of basis functions
if(is.null(lop$QV)) pars[[12]] <- NULL else
pars[[12]]<- unique(unlist(sapply(lop$QV, grep, dimnames(summary(fm)$tTable)[[1]])))    # row numbers involving covariate values
pars[[13]]<- (1:nF)+is.na(lop$corStr[1])                        # row sequence for F-values

# test if it works
#runLME(inData[1,2,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)


print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time as shown below.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

#for(ii in 1:dimx) for(jj in 1:dimy) 
#   aa<-runAOV(inData[ii,jj,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0)
# test runLME(inData[1,2,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars)      

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
   Stat <- array(0, dim=c(dimx_n, nSeg, NoBrick))
   # break input multiple segments for parrel computation
   dim(inData) <- c(dimx_n, nSeg, NoFile)
   if (lop$nNodes==1) for(kk in 1:nSeg) {
      if(NoBrick > 1) Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), c(2,1)) else
         Stat[,kk,] <- aperm(apply(inData[,kk,], 1, runLME, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), dim=c(dimx_n, 1))
      cat("Computation done: ", 100*kk/nSeg, "%", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   
   if (lop$nNodes>1) {
   library(snow)
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterEvalQ(cl, library(afex)); clusterEvalQ(cl, library(phia))
   for(kk in 1:nSeg) {
      if(NoBrick > 1) Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), c(2,1)) else
      Stat[,kk,] <- aperm(parApply(cl, inData[,kk,], 1, runLME, dataframe=lop$dataStr,
            ModelForm=ModelForm, pars=pars), dim=c(dimx_n, 1))
      cat("Computation done ", 100*kk/nSeg, "%: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n", sep='')   
   }
   stopCluster(cl)
   }
   # convert to 4D
   dim(Stat) <- c(dimx_n*nSeg, 1, 1, NoBrick)
   # remove the trailers (padded 0s)
   Stat <- Stat[-c((dimx_n*nSeg-fill+1):(dimx_n*nSeg)), 1, 1,,drop=F]
} else {

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

if (lop$nNodes==1) for (kk in 1:dimz) {
   if(NoBrick > 1) Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runLME, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars), c(2,3,1)) else
      Stat[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runLME, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars), dim=c(dimx, dimy, 1))      
   cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 


if (lop$nNodes>1) {
   library(snow)
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterEvalQ(cl, library(nlme)); clusterEvalQ(cl, library(contrast)) 
   for (kk in 1:dimz) {
      if(NoBrick > 1) Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runLME, 
            dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars), c(2,3,1)) else
         Stat[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runLME, 
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
#      Stat[,,kk,] <-aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1))
#      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
#   stopCluster(cl)
#}

###############################
tTop <- 100

Stat[Stat > tTop] <- tTop  # Avoid outflow!!!!
Stat[Stat < (-tTop)] <- -tTop  # Avoid outflow!!!!

outLabel <- paste(rownames(anova(fm))[pars[[13]]], " F")
if(!is.na(lop$corStr[1])) for(n in 1:dim(summary(fm)$tTable)[1]) {
   outLabel <- append(outLabel, rownames(summary(fm)$tTable)[n])
   outLabel <- append(outLabel, paste(rownames(summary(fm)$tTable)[n], "t"))
}               
if(lop$num_glt > 0) for (n in 1:lop$num_glt) {
   outLabel <- append(outLabel, lop$gltLabel[n])
   outLabel <- append(outLabel, paste(lop$gltLabel[n], "t"))

}
if(nCovVal > 0) for (n in 1:nCovVal) {
   outLabel <- append(outLabel, dimnames(summary(fm)$tTable)[[1]][pars[[12]][n]])
   outLabel <- append(outLabel, paste(dimnames(summary(fm)$tTable)[[1]][pars[[12]][n]], "t"))
}

statpar <- "3drefit"
IdxAdj <- as.integer(!is.na(lop$corStr[1]))
for (i in (2-IdxAdj):(nF+1-IdxAdj)) {  # has an intercept or not
   # DFs are acquired from the last solvable voxel 
        statpar <- paste(statpar, " -substatpar ", i-2+IdxAdj, 
           " fift ", anova(fm)$numDF[i], " ", anova(fm)$denDF[i])
}  # from 0 to NoF-1

if(lop$num_glt > 0) for (n in 1:lop$num_glt) 
   statpar <- paste(statpar, " -substatpar ", nF+2*n-1, " fitt ",gltDF[n])

if(!is.null(lop$QV)) for (n in 1:nCovVal) 
   statpar <- paste(statpar, " -substatpar ", nF+2*lop$num_glt+2*n-1, " fitt ", summary(fm)$tTable[pars[[12]][n],"DF"])
statpar <- paste(statpar, " -addFDR -newid ", lop$outFN)

write.AFNI(lop$outFN, Stat, outLabel, defhead=head, idcode=newid.AFNI(), type='MRI_short')
system(statpar)
print(sprintf("Congratulations! You've got an output %s", lop$outFN))

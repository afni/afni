#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R


# Commannd line to run this script: 3dMVM.R dataStr.txt diary.txt &
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
Version 0.0.1, Oct 19, 2012
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
Version 0.0.1, Oct 15, 2012
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/MVM.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage:
------ 
 3dMVM is a group-analysis program that performs traditional ANOVA- and ANCOVA-
 style computations. There is no limit on the number of explanatory variable, or
 categorical or quantitative variables (factors or covariates). F-statistics for
 main effects and interactions are automatically included in the output. In
 addition, general linear tests (GLTs) can be requested via symbolic coding.
 
 Unequal number of subjects across groups are allowed, but scenarios with missing
 data for a within-subject factor are better modeled with 3dLME. Cases with
 quantitative variables (covariates) that vary across the levels of a within-
 subject variable are also better handled with 3dLME. Computational cost with
 3dMVM is very high compared to 3dttest++ or 3dANOVAx, but 3dMVM has the
 capability to correct for sphericity violations.
 
 Two R packages need to be installed first before running 3dMVM:
 
 install.packages("afex")
 install.packages("phia")

 See more details at 
 http://afni.nimh.nih.gov/sscc/gangc/MVM.html'

   ex1 <- 
"
Example 1 --- three between-subjects (genotype, sex, and scanner) and two 
within-subject (condition and emotion) variables:
--------------------------------
   3dMVM  -prefix Example1 -jobs 4            \\
          -model  'genotype*sex+scanner'      \\
          -wsVars \"condition*emotion\"         \\
          -num-glt 14                         \\
          -gltLabel 1 face_pos_vs_neg -gltCode  1 'condition : 1*face emotion : 1*pos -1*neg'        \\
          -gltLabel 2 face_emot_vs_neu -gltCode 1 'condition : 1*face emotion : 1*pos +1*neg -2*neu' \\
          ...            
          -dataTable                                                                                 \\
          Subj  genotype   sex    scanner  condition   emotion   InputFile                           \\
          s1    TT         male   scan1   face        pos       s1+tlrc\'[face_pos_beta]\'            \\
          s1    TT         male   scan1   face        neg       s1+tlrc\'[face_neg_beta]\'            \\
          s1    TT         male   scan1   face        neu       s1+tlrc\'[face_neu_beta]\'            \\
          s1    TT         male   scan1   house       pos       s1+tlrc\'[house_pos_beta]\'           \\
          ...
          s68   TN         female scan2   house       pos       s68+tlrc\'[face_pos_beta]\'           \\
          s68   TN         female scan2   house       neg       s68+tlrc\'[face_neg_beta]\'           \\
          s68   TN         female scan2   house       neu       s68+tlrc\'[house_pos_beta]\'                    
     \n"
      
   ex2 <-
"Example 2 --- 
:
-------------------------------------------------------------------------
   3dMVM -prefix Example2 -jobs 4        \\
          -model  \"genotype*sex+age+IQ\"  \\
          -wsVars emotion                \\
          -qVars  \"age+IQ\"               \\
          -qVarCenters '25,105'          \\
          -num-glt 10                    \\
          -gltLabel 1 pos_F_vs_M   -gltCode 1 'sex : 1*F -1*M emotion : 1*pos'        \\
          -gltLabel 2 age_TT_vs_NN -gltCode 2 'geneotype : 1*TT -1*NN age :'          \\
          ...            
          -dataTable                                                                  \\
          Subj  geneotype  sex    age  IQ     emotion   InputFile                     \\
          s1    TT         male   24   107    pos       s1+tlrc\'[pos_beta]\'           \\
          s1    TT         male   24   107    neg       s1+tlrc\'[neg_beta]\'           \\
          s1    TT         male   24   107    neu       s1+tlrc\'[neu_beta]\'           \\
          ... 
          s63   NN         female 29   110    pos       s63+tlrc\'[pos_beta]\'          \\
          s63   NN         female 29   110    pos       s63+tlrc\'[pos_beta]\'          \\
          s63   NN         female 29   110    neg       s63+tlrc\'[neg_beta]\'         
     
   \n"
   
   
   ex3 <-
"Example 3 --- BOLD response was modeled with multiple basis functions at individual
subject level. In addition, there are one between-subjects (Group) and one within-
subject (Condition) variable. Furthermore, the variable corresponding to the number 
of basis functions, Time, is also a within-subject variable. In the end, the F-
statistics for the interactions of Group:Condition:Time, Group:Time, and 
Condition:Time are of specific interest. And these interactions can be further
explored with GLTs in 3dMVM.

---------------------------------
   3dMVM -prefix Example3 -jobs 4   \\
         -model Group               \\
         -wsVars 'Condition*Time'   \\
         -num-glt 32                \\
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
   "-prefix PREFIX: Output prefix (just prefix, no view+suffix needed)\n"
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
   "         the ones used in the header of -dataTable. A+B includes the",
   "         additive effects of A and B, A:B is the interaction between A",
   "         and B, and A*B = A+B+A:B. The effects of within-subject",
   "         factors, if present under -wsVars are automatically assumed",
   "         to interact with the ones specified here.\n", sep = '\n'
             ) ),

      '-wsVars' = apl(n=c(1,100), d=NA, h = paste(
   "-wsVars FORMULA: Provide within-subject factors only. Coding for",
   "         additive and interaction effects is the same as in-model. The",
   "         FORMULA with more than one variable has to be surrounded ",
   "         within (single or double) quotes\n", sep = '\n'
                             ) ),

      '-qVars' = apl(n=c(1,100), d=NA, h = paste(
   "-qVars FORMULA: Provide quantitative variables (or covariates) only.",
   "         Coding for additive and interaction effects is the same as in",
   "         -model. The expression FORMULA with more than one variable has",
   "         to be surrounded within (single or double) quotes. Variable",
   "         WARNINGS:",
   "         1) Centering a quantitative variable through -qVarsCenters is",
   "         very critical when other fixed effects are of interest.",
   "         2) Between-subjects covariates are generally acceptable.",
   "         However caution should be taken when different average value",
   "         for a covariate exists across groups.",
   "         3) Within-subject covariates are better modeled with 3dLME.\n",
             sep = '\n'
             ) ),

     '-qVarCenters' = apl(n=c(1,100), d=NA, h = paste(
   "-qVarCenters VALUES: Specify centering values for quantitative variables",
   "         identified under -qVars. Multiple centers are separated by ",
   "         commas (,) within (single or double) quotes. The order of the",
   "         values should match that of the quantitative variables in -qVars.",
   "         Default (absence of option -qVarsCetners) means centering on the",
   "         average of the variable across ALL subjects regardless their",
   "         grouping. If within-group centering is desirable, center the",
   "         variable first before the values are fed into -dataTable.\n",
             sep = '\n'
                     ) ),

     '-num_glt' = apl(n=1, d=0, h = paste(
   "-num_glt NUMBER: Specify the number of general linear tests (GLTs). A glt",
   "         is a linear combination of a factor levels. See details in ",
   "         -gltLabel.\n", sep = '\n'
             ) ),
                     
     '-gltLabel' = apl(n=c(2,1000), d=NA, h = paste(
   "-gltLabel k label: Specify the label for the k-th general linear test",
   "         (GLT). A symbolic coding for the GLT is assumed to follow with",
   "         each -gltLabel.\n", sep = '\n'
                     ) ),

     '-gltCode' = apl(n=c(2,1000), d=NA, h = paste(
   "-gltCode k CODING: Specify the k-th general linear test (GLT) through a",
   "         weighted combination among factor levels. The symbolic coding has",
   "         to be within (single or double) quotes. For example, the following",
   "         'Condition : 2*House -3*Face Emotion : 1*positive '",
   "         requests for a test of comparing 2 times House condition",
   "         with 3 times Face condition while Emotion is held at positive",
   "         valence.",
   "         NOTICE:",
   "         1) The absence of a categorical variable in a coding means the",
   "         levels of that factor are averaged (or collapsed) for the GLT.",
   "         2) When a quantitative variable is present, other effects are",
   "         tested at the center value of the covariate.",
   "         3) The effect for a quantitative variable can be specified with,",
   "         for example, 'Group : 1*Old Age : ', or ",
   "         'Group : 1*Old - 1*Young Age : '\n", sep = '\n'
             ) ),


     '-dataTable' = apl(n=c(1, 10000), d=NA, h = paste(
   "-dataTable TABLE: List the data structure with a header as the first line.",
   "         The first column is fixed with 'Subj', and the last is reserved",
   "         for 'InputFile' Each row should contain only one input file in",
   "         the table in a long format. The levels of a factor should contain",
   "         at least one character. Input files can be in NIfTI or AFNI",
   "         format with sub-brick selector with a number or label. Unequal",
   "         number of subjects across groups is allowed, but situations",
   "         with missing data for a within-subject factor should be handled",
   "         with 3dLME.\n", 
             sep = '\n'
                     ) ),

      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" ),
   
       '-cio' = apl(n=0, h = "-cio: Use AFNI's C io functions, which is default.\n"),
       '-Rio' = apl(n=0, h = "-Rio: Use R's io functions\n")
      
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
      lop$qVars  <- NULL
      lop$qVarCenters    <- NA
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
             maskFN = lop$maskFN <- ops[[i]],
             jobs   = lop$nNodes <- ops[[i]],
             model  = lop$model  <- ops[[i]],
             wsVars = lop$withinSubjVar  <- ops[[i]],
             qVars  = lop$qVars <- ops[[i]],
             qVarCenters = lop$qVarCenters <- ops[[i]],
             num_glt = lop$num_glt <- ops[[i]],
             gltLabel = lop$gltLabel <- ops[[i]],
             gltCode  = lop$gltCode <- ops[[i]],
             dataTable  = lop$dataTable <- ops[[i]],
             
             help = help.MVM.opts(params, adieu=TRUE),

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
   if(file.exists(paste(lop$outFN,"+tlrc.HEAD", sep="")) || 
         file.exists(paste(lop$outFN,"+tlrc.HEAD", sep=""))) {
         errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
         return(NULL)
   }      
   
   #Make sure new io must be used with anything but BRIK format
   an <- parse.AFNI.name(lop$outFN)
   if(an$type != 'BRIK' && lop$iometh != 'clib') 
      errex.AFNI(c('Must of use -cio option with any input/output ',
                   'format other than BRIK'))

   lop$QV <- strsplit(lop$qVars, '\\+')[[1]]


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
      for(ii in 1:wd) 
         lop$dataStr <- data.frame(cbind(lop$dataStr, lop$dataTable[seq(wd+ii, len, wd)]))
      names(lop$dataStr) <- lop$dataTable[1:wd]
      for(jj in lop$QV) lop$dataStr[,jj] <- as.numeric(lop$dataStr[,jj])
   }

   
   if (lop$num_glt > 0) {
      lop$gltList    <- vector('list', lop$num_glt)
      lop$slpList    <- vector('list', lop$num_glt)
      for (n in 1:lop$num_glt) {
         if(any(lop$QV %in% lop$gltCode[[n]])) {
            QVpos <- which(lop$gltCode[[n]] %in% lop$QV)
            lop$gltList[[n]]   <- gltConstr(lop$gltCode[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
            lop$slpList[[n]] <- lop$gltCode[[n]][QVpos]   
         } else lop$gltList[[n]] <- gltConstr(lop$gltCode[[n]], lop$dataStr)
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


runAOV <- function(inData, dataframe, ModelForm, pars, tag) {
   Stat <- rep(0, pars[[1]])
   #browser()
   #options(warn = -1)
   if (!all(abs(inData) < 10e-8)) {        
      dataframe$Beta<-inData
      try(fm <- aov.car(ModelForm, data=dataframe, return='lm'), tag <- 1)
      if(tag == 0) {
         #browser()
         if(pars[[6]]) try(Stat[1:pars[[2]]] <- univ(fm[[1]])[2:(1+pars[[2]]),3], tag<-1) else
            try(Stat[1:pars[[2]]] <- unname(univ(fm[[1]])[[1]][-1,5]), tag<-1)
         if(tag == 0) for(ii in 1:pars[[3]]) {
	         try(glt <- testInteractions(fm[[2]], custom=pars[[4]][[ii]], slope=pars[[5]][[ii]], adjustment="none", idata = fm[["idata"]]), tag<-1) 
	         if(tag == 0) {
               Stat[pars[[2]]+2*ii-1] <- glt[1,1]
	            Stat[pars[[2]]+2*ii]   <- sign(glt[1,1]) * sqrt(glt[1,4])  # convert F to t
            }
         }
      }
   }
   return(Stat)
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
   lop$QVcenter <- unlist(strsplit(scanLine(modFile, lnNo=8), "\\*"))
   #lop$QVcenter <- as.numeric(strsplit(lop$QVcenter, '\\,')[[1]])

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
   browser()
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



lop$QVcenter <- as.numeric(strsplit(lop$qVarCenters, '\\,')[[1]])


require("afex")
require("phia")

#comArgs <- commandArgs()

#if(length(comArgs)<6) modFile <- "model.txt" else
#modFile <- comArgs[6]
#paste(commandArgs())


# even if lop$wsVars is NA (no within-subject factors), it would be still OK for Error(Subj/NA)
if(is.na(lop$wsVars)) ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj)')) else
   ModelForm <- as.formula(paste("Beta ~", lop$model, '+Error(Subj/(', lop$wsVars, '))')) 
   

# Line 12 and the next few: pair-wise contrasts

# Maybe not list for these two, or yes?
lop$dataStr$Subj <-  as.factor(lop$dataStr$Subj)
lop$dataStr$InputFile <-  as.character(lop$dataStr$InputFile)

# center on user-speficied value or mean
if(any(!is.na(lop$QV))) if(all(is.na(lop$QVcenter))) lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=TRUE, scale=F) else
   lop$dataStr[,lop$QV] <- scale(lop$dataStr[,lop$QV], center=lop$QVcenter, scale=F)



# Assume the last column is input files
#FileCol <- length(colnames(lop$dataStr))
FileCol <- dim(lop$dataStr)[2]

#Number of input files
NoFile <- dim(lop$dataStr[1])[1]

# Repeated-measures (use lme) or not (use lm)
#if (length(unique(lop$dataStr$Subj)) != length(lop$dataStr$Subj)) RM <- TRUE else RM <- FALSE

# Read in the 1st input file so that we have the dimension information
inData <- read.AFNI(lop$dataStr[1, FileCol])
dimx <- inData$dim[1]
dimy <- inData$dim[2]
dimz <- inData$dim[3]
myHist <- inData$header$HISTORY_NOTE; myOrig <- inData$origin; myDelta <- inData$delta

inData <- unlist(lapply(lapply(lop$dataStr[,FileCol], read.AFNI), '[[', 1))
dim(inData) <- c(dimx, dimy, dimz, NoFile)

if (!is.na(lop$maskFN)) {
	Mask <- read.AFNI(lop$maskFN)$brk[,,,1]
	inData <- array(apply(inData, 4, function(x) x*read.AFNI(lop$maskFN)$brk[,,,1]), dim=c(dimx,dimy,dimz,NoFile))
}

# try out a few voxels and see if the model is OK, and find out the number of F tests and DF's 
# for t tests (and catch potential problems as well)
ii<-dimx%/%3; jj<-dimy%/%3; kk<-dimz%/%3

###############################

for(ii in 1:dimx) for(jj in 1:dimy) for(kk in 1:dimz) {
   if(all(abs(inData[ii,jj,kk,])>10e-7)) {
      xinit <- ii; yinit <- jj; zinit <- kk; break }
}

ii <- xinit; jj <- yinit; kk <- zinit

tag<-1
gltRes <- vector('list', lop$num_glt)

while (tag == 1) {
   tag<-0
   lop$dataStr$Beta<-inData[ii, jj, kk,]
        
   try(fm <- aov.car(ModelForm, data=lop$dataStr, return='lm'), tag <- 1)
   if(tag == 0) if (lop$num_glt > 0) try(for (n in 1:lop$num_glt) 
      gltRes[[n]] <- testInteractions(fm[[2]], custom=lop$gltList[[n]], slope=lop$slpList[[n]], 
         adjustment="none", idata = fm[["idata"]]) )
   if (tag == 1) if(ii<dimx) ii<-ii+1 else if(jj<dimy) {ii<-xinit; jj <- jj+1} else if(kk<dimz) {
      ii<-xinit; jj <- yinit; kk <- kk+1 } else {
      errex.AFNI("Something is not quite right during testing!")
      #break
   }
}

if(tag == 0)  {
   print("Good, test passed...")
   #if (NoConst) NoF <- nrow(anova(fm)) else NoF <- nrow(anova(fm))-1  # Just assume an intercept in the model
        #NoF <- nrow(anova(fm))
   #for (n in 1:lop$num_glt) contrDF[n] <- temp[n]$df
   } else { 
      errex.AFNI(sprintf("Insoluble model! Formula %s on the 4th line in model.txt \n",
                "might be inappropriate, or there are incorrect specifications in model.txt \n",
                "such as contrasts, factor levels, or other obscure problems.", lop$model))
      #break; next # won't run the whole brain analysis if the test fails
}


#fm <- aov.car(ModelForm, data=Model, return='lm')
#bm <- c(1,-1)%*%t(aa$lm$coefficients)
#Vm <- c(1,-1)%*%vcov(aa$lm)%*%c(1,-1)
#t(bm)%*%solve(Vm)%*%bm

# number of terms (or F-stats): the output structure is different without within-subject variables
nF <- ifelse(is.na(lop$wsVars), dim(univ(fm[[1]]))[1]-2, dim(univ(fm[[1]])$anova)[1]-1)
NoBrick <- nF + 2*lop$num_glt

# Initialization
Stat <- array(0, dim=c(dimx, dimy, dimz, NoBrick))

###############################

pars <- vector("list", 6)
pars[[1]] <- NoBrick
pars[[2]] <- nF
pars[[3]] <- lop$num_glt
pars[[4]] <- lop$gltList
pars[[5]] <- lop$slpList
pars[[6]] <- is.na(lop$wsVars)


#runAOV(inData[ii, jj, kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0)

print(sprintf("Start to compute %s slices along Z axis. You can monitor the progress", dimz))
print("and estimate the total run time by opening this file from time to time.")
print(format(Sys.time(), "%D %H:%M:%OS3"))

###############################
#options(warn = -1) # suppress warnings!
#getOption('warn')

#for(ii in 1:dimx) for(jj in 1:dimy) 
#   aa<-runAOV(inData[ii,jj,kk,], dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0)

if (lop$nNodes==1) for (kk in 1:dimz) {
   if(NoBrick > 1)
      Stat[,,kk,] <- aperm(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1)) else
      Stat[,,kk,] <- array(apply(inData[,,kk,], c(1,2), runAOV, dataframe=lop$dataStr, 
         ModelForm=ModelForm, pars=pars, tag=0), dim=c(dimx, dimy, 1))      
   cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
} 


if (lop$nNodes>1)    {
   library(snow)
   cl <- makeCluster(lop$nNodes, type = "SOCK")
   clusterEvalQ(cl, library(afex)); clusterEvalQ(cl, library(phia)) 
   for (kk in 1:dimz) {
      if(NoBrick > 1) Stat[,,kk,] <- aperm(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0), c(2,3,1)) else
         Stat[,,kk,] <- array(parApply(cl, inData[,,kk,], c(1,2), runAOV, 
            dataframe=lop$dataStr, ModelForm=ModelForm, pars=pars, tag=0), dim=c(dimx, dimy, 1))
      cat("Z slice ", kk, "done: ", format(Sys.time(), "%D %H:%M:%OS3"), "\n")
   } 
   stopCluster(cl)
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

if(is.na(lop$wsVars)) outLabel <- paste(dimnames(univ(fm[[1]]))[[1]][2:(1+nF)], " F") else
   outLabel <- paste(dimnames(univ(fm[[1]])[[1]])[[1]][-1], " F")
for(ii in 1:lop$num_glt) {
   outLabel <- append(outLabel, sprintf("%s", lop$gltLabel[ii]))
   outLabel <- append(outLabel, sprintf("%s:t", lop$gltLabel[ii]))
}   

statpar <- "3drefit"
for(ii in 1:nF) statpar <- paste(statpar, " -substatpar ", ii-1, " fift ",
   ifelse(is.na(lop$wsVars), univ(fm[[1]])[ii+1, 'Df'], unname(univ(fm[[1]])[[1]][ii+1,'num Df'])), " ",
   ifelse(is.na(lop$wsVars), univ(fm[[1]])[2+nF, 'Df'], unname(univ(fm[[1]])[[1]][ii+1,'den Df'])) )
for(ii in 1:lop$num_glt) statpar <- paste(statpar, " -substatpar ", nF+2*ii-1, " fitt", 
   ifelse(is.na(lop$wsVars), gltRes[[ii]][2,2], gltRes[[ii]][,6]))    
statpar <- paste(statpar, " -addFDR -newid ", lop$outFN)

write.AFNI(lop$outFN, Stat, outLabel, note=myHist, origin=myOrig, delta=myDelta, idcode="whatever")
system(statpar)
print(sprintf("Congratulations! You've got output %s+tlrc.*", lop$outFN))

# set save defaults using option:
#options(save.defaults=list(ascii=TRUE, safe=FALSE))
#save.image()
#unlink(".RData")

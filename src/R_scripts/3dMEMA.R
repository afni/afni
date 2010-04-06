#!/usr/bin/env AFNI_Batch_R

#########
##  type 4 (two groups with heteroskedasticity) should NOT be used when modeling with different slope across groups
##  Outliers can be modeled now with EFS of Laplace assumption regarding cross-subjects variability! 
#########

#Clean up
rm(list = ls())

libLoad <- function(myLib) {
   sucLoad <- FALSE
   sucCheck <- FALSE
   try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
   if (sucLoad) {
      print(sprintf("Package %s successfully loaded!", myLib)); sucCheck <- TRUE
   } else {
	  	try(install.packages(myLib))
      try(sucLoad <- library(myLib, character.only = TRUE, 
                              logical.return = TRUE))
      if (sucLoad) print(sprintf("Package %s successfully loaded...", myLib)) 
   	}
}


first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))

#################################################################################
##################### Begin MEMA Input functions ################################
#################################################################################

#A function to parse all interactive user input to computing function
read.MEMA.opts.interactive <- function (verb = 0) {
   lop <- list()  #List to hold all options from user input
   lop$verb <- verb
   
   outFNexist <- TRUE
   while (outFNexist) {
      lop$outFN <- 
         prefix.AFNI.name(readline("Output file name (just prefix, no view+suffix needed, e.g., myOutput): "))
      if(file.exists(paste(lop$outFN,"+orig.HEAD", sep="")) || 
         file.exists(paste(lop$outFN,"+tlrc.HEAD", sep=""))) {
         print("File exists! Try a different name.")
         outFNexist <- TRUE
      } else {
         outFNexist <- FALSE 
      }
   }
   lop$outFN <- 
      paste(lop$outFN, "+orig", sep="") # write.AFNI doesn't handle tlrc yet


   print("On a multi-processor machine, parallel computing will speed up the program significantly.")
   print("Choose 1 for a single-processor computer.")
   lop$nNodes <- 
      as.integer(readline("Number of parallel jobs for the running (e.g., 2)? "))
   
   lop$nGrp <- as.integer(readline("Number of groups (1 or 2)? "))

   lop$grpName <- vector('list', lop$nGrp)
   lop$testName <- vector('list', lop$nGrp)
   lop$nSubj <- vector('integer', lop$nGrp)
   lop$nFiles <- vector('integer', lop$nGrp) 
                        # number of input files for each group
   
   print("-----------------")
   print("The following types of group analysis are currently available:")
   print("1: one condition with one group;")
   print("2: one condition across 2 groups with homoskedasticity (same variability);")
   print("3: two conditions with one group;")
   print("4: one condition across 2 groups with heteroskedasticity (different variability).")
   lop$anaType <- as.integer(readline("Which analysis type (1, 2, 3, 4): "))
   
   if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4) {
      lop$bFN <- vector('list', lop$nGrp)
      lop$tFN <- vector('list', lop$nGrp)
      lop$subjLab <- vector('list', lop$nGrp)
      lop$bList <- vector('list', lop$nGrp)
      tList <- vector('list', lop$nGrp) 
      lop$varList <- vector('list', lop$nGrp)
   
      for(ii in 1:lop$nGrp) {

  	      if(lop$nGrp==1) 
            lop$grpName[[ii]] <- readline("Label for the test? ") 
         else 
            lop$grpName[[ii]] <- readline(sprintf("Label for group %i? ", ii))
  	      
         lop$testName[[ii]] <- lop$grpName[[ii]]

         lop$nSubj[ii] <- 
            as.integer(readline(sprintf("Number of subjects in group %s (e.g., 12)? ", lop$grpName[[ii]])))
  	      lop$nFiles[ii] <- 2*lop$nSubj[ii]
         lop$subjLab[[ii]] <- vector('list', lop$nSubj[ii])
  	      lop$bFN[[ii]] <- vector('integer', lop$nSubj[ii]) 
                                                # list of beta file names
  	      lop$tFN[[ii]] <- vector('integer', lop$nSubj[ii]) 
                                                # list of t-stat file names
  	      #print("Provide beta or linear combination of betas files first. Only one sub-brick input files are accepted!")
      
         for(jj in 1:lop$nSubj[ii]) 
            lop$subjLab[[ii]][[jj]] <- 
               readline(sprintf("No. %i subject label in group %s: ", 
                                 jj, lop$grpName[[ii]]))
      
         for(jj in 1:lop$nSubj[ii]) {
            lop$bFN[[ii]][[jj]] <- 
               readline(sprintf("No. %i subject (%s) file for beta or linear combination of betas in group %s: ",
                                jj, lop$subjLab[[ii]][[jj]], 
                                lop$grpName[[ii]]))
            # print("Now provide the corresponding t-statistic files in SAME subject order as beta files. Only one sub-brick input files are accepted!")
            lop$tFN[[ii]][[jj]] <- 
               readline(sprintf("No. %i subject (%s) file for the corresponding t-statistic in group %s: ", 
                                 jj, lop$subjLab[[ii]][[jj]], 
                                 lop$grpName[[ii]]))
            print("-----------------")
         }
         lop$bList[[ii]] <- lapply(lop$bFN[[ii]], read.AFNI); 
         lop$tList[[ii]] <- lapply(lop$tFN[[ii]], read.AFNI);
         if(ii==1) { 
            lop$myNote=lop$bList[[1]][[1]]$header$HISTORY_NOTE; 
            lop$myOrig=lop$bList[[1]][[1]]$origin; 
            lop$myDelta=lop$bList[[1]][[1]]$delta; 
            lop$myDim <- lop$bList[[1]][[1]]$dim;
         }
         lapply(lapply(lop$bList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch among the beta input files!") )
         lapply(lapply(lop$tList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch between beta and t-statistic files!"))

      } # for(ii in 1:lop$nGrp)
   } # if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4)
   
   if(lop$anaType==3) {
      lop$nLevel <- 2   
      lop$bFN <- vector('list', lop$nLevel)
      lop$tFN <- vector('list', lop$nLevel)
      lop$conLab <- vector('list', lop$nLevel)
      lop$subjLab <- vector('list', 1)
      lop$bList <- vector('list', lop$nLevel)
      lop$tList <- vector('list', lop$nLevel)
      lop$varList <- vector('list', lop$nLevel)
      print("Since the contrast between the 2 conditions will be the 2nd minus the 1st, choose")
      print("an appropriate order between the 2 conditions to get the desirable contrast.")
      lop$grpName[[1]] <- readline("Label for the contrast? ")
      lop$testName[[1]] <- lop$grpName[[1]]

      lop$nSubj[1] <- as.integer(readline("Number of subjects: "))
      lop$nFiles[1] <- 2*lop$nSubj[1]  
                        # 2 because of 1 beta and 1 t-statistic
      for(jj in 1:lop$nSubj[1]) 
         lop$subjLab[[1]][[jj]] <- 
            readline(sprintf("No. %i subject label: ", jj))      
      for(ii in 1:lop$nLevel) {
         lop$conLab[[ii]] <- readline(sprintf("Label for condition %i? ", ii))
         lop$bFN[[ii]] <- vector('integer', lop$nSubj[1]); 
         lop$tFN[[ii]] <- vector('integer', lop$nSubj[1]);
         for(jj in 1:lop$nSubj[1]) {
            lop$bFN[[ii]][[jj]] <- 
               readline(sprintf("No. %i subject (%s) file for beta or linear combination of betas with condition %s: ", 
                        jj, lop$subjLab[[1]][[jj]], lop$conLab[[ii]]))
            lop$tFN[[ii]][[jj]] <- 
               readline(sprintf("No. %i subject (%s) file for the corresponding t-statistic with condition %s: ", 
                        jj, lop$subjLab[[1]][[jj]], lop$conLab[[ii]]))
            print("-----------------")
         }
         lop$bList[[ii]] <- lapply(lop$bFN[[ii]], read.AFNI); 
         lop$tList[[ii]] <- lapply(lop$tFN[[ii]], read.AFNI);
         if(ii==1) {
            lop$myNote=lop$bList[[1]][[1]]$header$HISTORY_NOTE; 
            lop$myOrig=lop$bList[[1]][[1]]$origin; 
            lop$myDelta=lop$bList[[1]][[1]]$delta; 
            lop$myDim <- lop$bList[[1]][[1]]$dim
         }
         lapply(lapply(lop$bList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch among the beta input files!"))
         lapply(lapply(lop$tList[[ii]], function(x) x$dim), 
               function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch between beta and t-statistic files!"))

      } # for(ii in 1:lop$nLevel)  
   
   } # if(lop$anaType==3)
   
   #print("-----------------")
   print("There may have missing or zero t values at some voxels in some subjects.")
   print("The following threshold would allow the program not to waste runtime on those")
   print("voxels where most (e.g., 3/4 of total subjects) subjects have zero t-values. ")
   lop$nNonzero <- 
      as.integer(readline( sprintf(
      "Minimum number of subjects with non-zero t-statistic? (0-%i, e.g., %i) ",
                           sum(lop$nSubj), round(0.75*sum(lop$nSubj)) )))
   # Hartung-Knapp method with t-test?
   print("-----------------")
   print("The Hartung-Knapp adjustment makes the output t-statistic a little more")
   print("conservative but may be more robust when the number of subjects is")
   print("relatively small.")
   lop$KHtest <- 
      as.logical(as.integer(
                  readline("Hartung-Knapp adjustment for the output t-statistic? (0: No; 1: Yes) ")))
   #lop$KHtest <- FALSE
   
   print("-----------------")
   print("Masking is optional, but will alleviate unnecessary penalty on q values of FDR correction.")
   masked <- as.integer(readline("Any mask (0: no; 1: yes)? "))
   if(masked) {
      lop$maskFN <- 
         readline("Mask file name (suffix unnecessary, e.g., mask+tlrc): "); 
         lop$maskData <- read.AFNI(lop$maskFN)$brk
   }else {
      lop$maskFN <- NULL;
   }
   if(!is.null(lop$maskFN)) 
      if(!all(dim(lop$maskData[,,,1])==lop$myDim[1:3])) 
         stop("Mask dimensions don't match the input files!")

   if(lop$nGrp==1) lop$xMat <- rep(1, sum(lop$nSubj))      
   if(lop$nGrp==2) 
      lop$xMat <- cbind(rep(1, sum(lop$nSubj)), 
                    c(rep(0, lop$nSubj[1]), rep(1, lop$nSubj[2]))) 
                                          # dummy variable for two groups
   print("-----------------")   
   print("Covariates are continuous variables (e.g., age, behavioral data) that can be partialled out in the model.")
   anyCov <- as.logical(as.integer(readline("Any covariates (0: no; 1: yes)? ")))
   if(anyCov) {
      lop$nCov <- as.integer(readline("Number of covariates (e.g., 1)? "))
      #print("Each subject is assumed to have one number per covariate. Covariates as input files can be")
      #print("in multi-column or one-column format. Header with the 1st line as labels is optional in")
      #print("multi-column files. The vertical sequence in each column has to follow the same subject order")
      #print("as the beta/t-statistic input files listed above.")
      print("Each subject is assumed to have one number per covariate. All covariates should be put into one")
      print("file in a table format. It's required that both row (subject ID) and column (covariate) be put in")
      print("the table, for instance,")
      print("  subj  age   weight")
      print("  Jane   25   300")
      print("  Joe    22   313")
      print("  ...    ..   ...")
      
      #print("Header at the 1st line as labels is optional. The vertical")
      #print("sequence in each column has to follow exactly the same subject order as the beta/t-statistic")
      #print("input files listed above.")
      #covForm <- as.integer(readline("Covariates data type (0: MULTIPLE one-column files; 1: ONE single/multi-column file)? "))
      covForm <- 1
      if (covForm) {
         lop$covFN <- readline("Covariates file name: ")
         #covHeader <- as.integer(readline(
         #"Does this multi-column file have a header (0: no; 1: yes)? "))
         covHeader <- 1
         if(covHeader == 1) {
            #lop$covData <- read.table(lop$covFN, header=TRUE)
            lop$covData <- read.AFNI.matrix(lop$covFN, userrownames=unlist(lop$subjLab))
            lop$covName <- colnames(lop$covData)            
            #tmp <- read.table(lop$covFN, header=TRUE)
            #lop$covData <- cbind(tmp[,-1])
            #lop$covName <- colnames(tmp)[-1]
            #rm(tmp)
         } else {
            lop$covData <- read.table(lop$covFN, header=FALSE)
            if(lop$nCov!=dim(lop$covData)[2]) {
               stop(sprintf(
         "Mismatch: file %s has %i column while you said %i covariate(s)!", 
                     lop$covFN, dim(lop$covData)[2], lop$nCov)) 
            } else {
               for (ii in 1:lop$nCov) 
                  names(lop$covData)[ii] <- 
                     readline(sprintf("Name for covariate number %i? ", ii))
            }
            lop$covName <- names(lop$covData)
         } # if(covHeader == 1)
         #lop$covName <- names(lop$covData)
      } else {
         lop$covData <- 
            data.frame(matrix(data=NA, nrow=sum(lop$nSubj), 
                              ncol=lop$nCov, dimnames = NULL))
         lop$covData <- 
            readMultiFiles(lop$nCov, 1, "covariate")  # 1: assuming no header  
      }

      print("Each covariate should be centered around its mean or some other meaningful value so that group effect")
      print("can be interpreted with the covariate being at the mean or other user-specified value. Otherwise")
      print("the interpretation would be with the covariate being at 0!")
      lop$centerType <- 
         as.integer(readline(
"What value will covariate(s) be centered around (0: mean; 1: other value)? "))
      #browser()
      if(lop$nGrp == 1) {
         #browser()
         if(lop$centerType == 0) lop$covData <- apply(lop$covData, 2, scale, scale=F)  # center around mean for each covariate (column)
         if(lop$centerType == 1) {  # center around a user-specified value
            lop$centerVal <- vector(mode = "numeric", length = lop$nCov)
            for(ii in 1:lop$nCov) lop$centerVal[ii] <- as.numeric(readline(sprintf("Centering value for covariate no. %i (%s): ? ", ii, names(lop$covData)[ii])))
            lop$covData <- t(as.matrix(apply(lop$covData, 1, "-", 
                                             lop$centerVal)))
         }
      } # if(lop$nGrp == 1)

      if (lop$nGrp == 2) {
         lop$centerType2 <- 
            as.integer(readline("How to model covariate(s) across groups (0: same center & slope; 1: same center but different slope; 
            2: different center but same slope; 3: different center & slope)? "))
         #browser()
         if(lop$centerType2 == 0 | lop$centerType2 == 1) {   # same center 
            if(lop$centerType == 0) 
               lop$covData <- apply(lop$covData, 2, scale, scale=F)  
                                 # center around mean for each covariate (column)
            if(lop$centerType == 1) {  
                        # center around other value for each covariate (column)
               lop$centerVal <- vector(mode = "numeric", length = lop$nCov)
               for(jj in 1:lop$nCov) lop$centerVal[jj] <- as.numeric(readline(sprintf(
                  "Centering value for covariate no. %i (%s) for both groups: ? ", jj, names(lop$covData)[jj])))
               lop$covData <- t(as.matrix(apply(lop$covData, 1, "-", 
                                                lop$centerVal)))
            }
         } # if(lop$centerType2 == 0 | lop$centerType2 == 1)

         if(lop$centerType2 == 2 | lop$centerType2 == 3) {  # different center 
            if(lop$centerType == 0) lop$covData <- rbind(apply(as.matrix(lop$covData[1:lop$nSubj[1],]), 2, scale, scale=F), 
               apply(as.matrix(lop$covData[(lop$nSubj[1]+1):(lop$nSubj[1]+lop$nSubj[2]),]), 2, scale, scale=F))
            if(lop$centerType == 1) {
               covList <- vector('list', lop$nGrp)               
               for(ii in 1:lop$nGrp) {
                  lop$centerVal <- vector(mode = "numeric", length = lop$nCov)
                  for(jj in 1:lop$nCov)  {
                     lop$centerVal[jj] <- 
                        as.numeric(readline(sprintf(
                "Centering value for covariate no. %i (%s) in group %i (%s): ? ",
                                             jj, names(lop$covData)[jj], 
                                             ii, lop$grpName[[ii]])))
                  }
                  covList[[ii]] <- t(apply(as.matrix(lop$covData[((ii-1)*lop$nSubj[1]+1):(lop$nSubj[1]+(ii-1)*lop$nSubj[2]),]), 1, "-", lop$centerVal))
               }
               #browser()
               #lop$covData <- rbind(t(covList[[1]]), t(covList[[2]]))
               lop$covData <- rbind(covList[[1]], covList[[2]])
            } # if(lop$centerType == 1)
         } # if(lop$centerType2 == 3)
         
         if(lop$centerType2 == 1 | lop$centerType2 == 3) { # different slope
            lop$covData <- cbind(lop$covData, lop$covData*lop$xMat[,2])  
                                 # add one column per covariate for interaction
            lop$nCov <- 2*lop$nCov 
                              # double number of covariates due to interactions
            lop$covName<-c(lop$covName, paste(lop$covName, "X", sep=''))  
                              # add names for those interactions
         }
      }

      #browser()
      lop$xMat <- as.matrix(cbind(lop$xMat, lop$covData))           
   } else {lop$nCov <- 0; lop$xMat <- as.matrix(lop$xMat)}

   print("-----------------")
   print("If outliers exist at voxel/subject level, a special model can be adopted to account for outliers")
   print("in the data, leading to increased statistical power at slightly higher computation cost.")
   lop$lapMod <- as.integer(readline("Model outliers (0: no; 1: yes)? "))
   print("-----------------")
   print("The Z-score of residuals for a subject indicates the significance level the subject is an outlier at a voxel.")
   print("Turn off this option and select 0 if memory allocation problem occurs later on.")
   lop$resZout <- as.integer(readline("Want residuals Z-score for each subject (0: no; 1: yes)? "))
   if(lop$resZout==1) {
      lop$icc_FN  <- paste(strsplit(lop$outFN, "\\+")[[1]][1], 
                           "_ICC+orig", sep="")
      lop$resZ_FN <- paste(strsplit(lop$outFN, "\\+")[[1]][1], 
                           "_resZ+orig", sep="") 
                  # write.AFNI doesn't handle tlrc yet
   }
   
   return(lop)
}# end of read.MEMA.opts.interactive

#Handle the -set option string
MEMA.parse.set <- function (lop, op) {
   #length of op should be = 1 + Nsubj*3
   if ((length(op)-1)%%3) {
      warning(paste('Length of op must be 3*N+1\n',
                    'Make sure what follows -set is of the form:\n',
                    ' -set SETNAME <subj BetaDset TstatDset>',
                    '<subj BetaDset TstatDset>, ...\n', 'What I have is:',
                    paste(op, collapse=' ')),
                immediate.=TRUE);
      return(NULL);
   }
   
   #set name
   nn <- op[1];
   
   if (length(which(nn == names(lop$bFN)))) {
      warning(paste('Repeat combination of ', nn, condname),
               immediate.=TRUE);
      return(NULL);
   }

   ii<-2
   lop$bFN[[nn]] <- vector('character')
   lop$tFN[[nn]] <- vector('character')
   lop$subjLab[[nn]]<- vector('character')
   while (ii < length(op)) {
      lop$subjLab[[nn]] <- append(lop$subjLab[[nn]], op[ii]); ii<-ii+1;
      lop$bFN[[nn]] <- append(lop$bFN[[nn]], op[ii]); ii<-ii+1;
      lop$tFN[[nn]] <- append(lop$tFN[[nn]], op[ii]); ii<-ii+1;
   }
   
   return(lop);
}

#Handle the -covariates option string
MEMA.parse.covariates_center <- function (lop, op) {
   if (is.character(op)) {
      ss <- strsplit(op,' ')[[1]];
      lop$centerType <- -1
      lop$centerVal <- NULL
      dd <- as.num.vec(paste(op, collapse=' '), addcount = FALSE)
      if (is.null(dd)) {
         if (length(ss) == 1) {
            if (ss == 'mean' || ss == 'avg') {
               lop$centerType <- 0
            } else {
               warning(paste('Bad value for -covariates_center',
                             '  Have "', ss,'" \n',sep=''),
                        immediate.=TRUE);
               return(lop);
            }
         }
      } else {
         lop$centerVal <- dd
         if (is.null(lop$centerVal)) {
            warning(paste('Failed to get centerVal from ', ss, sep=''),
                  immediate.=TRUE);
            return(lop);
         }
         lop$centerType <- 1
      }
   } else { # already changed to numbers 
      lop$centerVal <- op;
      lop$centerType <- 1
   }
   
   return(lop);
}

#Handle the -covariates_model option
MEMA.parse.covariates_model <- function (lop, op) {
   lop$centerType2 <- -1
   if (is.null(vv <- as.char.vec(paste(op,collapse=' ')))) return(lop);
   if (is.na(slp <- vv['slope'])) {
      warning(paste('Could not find "slope=" in -covariates_model option.\n',
                    '  Have ', paste(op,collapse=' '), '\n'),
            immediate.=TRUE);
      return(lop);
   }
   if (is.na(cen <- vv['center'])) {
      warning(paste('Could not find "center=" in -covariates_model option.\n',
                    '  Have ', paste(op,collapse=' '), '\n'),
            immediate.=TRUE);
      return(lop);
   }
   
   if (length(grep('sa', slp))) slp <- 1
   else if (length(grep('dif',slp))) slp <- 0
   else {
      warning(paste('Bad value for "slope=" in -covariates_model option.\n',
                    '  Have "', paste(slp,collapse=' '), '"\n',
                    '  Use either "slope=same" or "slope=different"\n', sep=''),
            immediate.=TRUE);
      return(lop);
      slp <- -1
   }
   
   if (length(grep('sa', cen))) cen <- 1
   else if (length(grep('dif',cen))) cen <- 0
   else {
      warning(paste('Bad value for "center=" in -covariates_model option.\n',
                    '  Have "', paste(cen,collapse=' '), '"\n',
                    '  Use either "center=same" or "center=different"\n', 
                    sep=''),
            immediate.=TRUE);
      return(lop);
      cen <- -1
   }
   
   if (slp == 1 && cen == 1) lop$centerType2 <- 0
   else if (slp == 0 && cen == 1) lop$centerType2 <- 1
   else if (slp == 1 && cen == 0) lop$centerType2 <- 2
   else if (slp == 0 && cen == 0) lop$centerType2 <- 3
   else lop$centerType2 <- -1
   
   return(lop);
}

#Get a list of all subjects
AllSubj.MEMA <- function (subjLab) {
   allsubj <- subjLab[[1]]
   if (length(subjLab) > 1) {
      for (i in 2:length(subjLab)) allsubj <- c(allsubj, subjLab[[i]])
   }

   allsubj <- unique(allsubj);
   return(allsubj)
}  

greeting.MEMA <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 3dMEMA.R ==================          
             AFNI Mixed-Effects Meta-Analysis Modeling Package!
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 0.1.6,  March 10, 2010
Author: Gang Chen (gangchen@mail.nih.gov)
Website - http://afni.nimh.nih.gov/sscc/gangc/MEMA.html
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      
reference.MEMA <- function ()
   return(
"#######################################################################
Please consider citing the following if this program is useful for you:

   Gang Chen et al., hopefully something coming soon...
   http://afni.nimh.nih.gov/sscc/gangc/MEMA.html
   
#######################################################################"
   )


#The help function for 3dMEMA batch (command line mode)
help.MEMA.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
Usage:
------ 
 3dMEMA is a program for performing Mixed Effects Meta Analysis at group level 
 that models both within- and across- subjects variablity, thereby requiring
 both regression coefficients, or general linear contrasts among them, and the 
 corresponding t-statistics from each subject as input. It\'s required to install 
 R (http://www.r-project.org/), plus \'snow\' package if parallel computing is
 desirable. Version 0.1.6 (March 10, 2010). See more details at
 
 http://afni.nimh.nih.gov/sscc/gangc/MEMA.html'
   
   ex1 <- 
"
Example 1 --- One-sample type (one regression coefficient or general linear 
contrast from each subject in a group):
--------------------------------
      3dMEMA   -prefix ex1  \\
               -jobs 4      \\
               -set  happy  \\
                  ac   ac+tlrc\'[14]\'   ac+tlrc\'[15]\'  \\
                  ejk  ejk+tlrc\'[14]\'  ejk+tlrc\'[15]\' \\
                  ...
                  ss   ss+tlrc\'[14]\'   ss+tlrc\'[15]\' \\
               -max_zeros 4    \\
               -model_outliers \\        
               -residual_Z        

      3dMEMA   -prefix ex1  \\
               -jobs 4      \\
               -set  happy  \\
                  ac   ac+tlrc\'[happy#0_Coef]\'   ac+tlrc\'[happy#0_Tstat]\'  \\
                  ejk  ejk+tlrc\'[happy#0_Coef]\'  ejk+tlrc\'[happy#0_Tstat]\' \\
                  ...
                  ss   ss+tlrc\'[happy#0_Coef]\'   ss+tlrc\'[happy#0_Tstat]\' \\
               -max_zeros 4    \\
               -HKtest         \\        
               -model_outliers \\        
               -residual_Z     \n"   

   ex2 <-
"Example 2 --- Paired type (two regression coefficients or general linear 
contrasts from each subject in a group with the constrast is the 2nd set 
subtracing the 1st one):
---------------------------------
   3dMEMA   -prefix ex2  \\
            -jobs 4      \\
            -conditions happy sad \\
            -set   happy \\
                ac   ac_hap_B+tlrc   ac_hap_T+tlrc   \\
                ejk  ejk_hap_B+tlrc  ejk_hap_T+tlrc  \\
                ...
                ss   ss_hap_B+tlrc   ss_hap_T+tlrc   \\
            -set   sad   \\
                ac   ac_sad_B+tlrc   ac_sad_T+tlrc   \\
                ejk  ejk_sad_B+tlrc  ejk_sad_T+tlrc  \\
                ...
                ss   ss_sad_B+tlrc   ss_sad_T+tlrc   \\
            -n_nonzero 10  \n"
   
   ex3 <- 
"Example 3 --- Two-sample type (one regression coefficient or general linear
contrast from each subject in two groups with the constrast being the 2nd group 
subtracing the 1st one), homoskedasticity (different cross-subjects variability 
between the two groups), outlier modeling, covariates centering, no payment no 
interest till Memorial Day next year:
-------------------------------------------------------------------------
   3dMEMA   -prefix ex3  \\
            -jobs 4      \\
            -groups horses goats  \\
            -unequal_variance     \\
            -set   healthy_horses \\
                ac   ac_sad_B+tlrc.BRIK   ac_sad_T+tlrc.BRIK  \\
                ejk  ejk_sad_B+tlrc.BRIK  ejk_sad_T+tlrc.BRIK \\
                ...
                ss   ss_sad_B+tlrc.BRIK   ss_sad_T+tlrc.BRIK  \\
            -set   healthy_goats \\
                jp   jp_sad_B+tlrc.BRIK   jp_sad_T+tlrc.BRIK  \\
                mb   mb_sad_B+tlrc.BRIK   mb_sad_T+tlrc.BRIK  \\
                ...
                trr  trr_sad_B+tlrc.BRIK  trr_sad_T+tlrc.BRIK \\
            -max_zeros 6    \\
            -HKtest         \\
            -model_outliers \\
            -residual_Z     \\
            -covariates CovFile.txt \\
            -covariates_center age = 25 13 weight = 100 150  \\
            -covariates_model center=different slope=same   
   
   where file CovFile.txt looks something like this:  
   
      name  age  weight
      ejk   93    117
      jcp   3     34
      ss    12    200   
      ac    12    130
      jp    65    130
      mb    25    630
      trr   18    187
      delb  9     67
      tony  12    4000
\n"

   parnames <- names(params)
   ss <- vector('character')
   if (alpha) {
       parnames <- sort(parnames)   
      ss <- paste('Options in alphabetical order:\n',
                  '------------------------------\n', sep='')
   } else {
      ss <- paste('Options:\n',
                  '--------\n', sep='')
   }
   for (ii in 1:length(parnames)) {
      op <- params[parnames[ii]][[1]]
      if (!is.null(op$help)) {
         ss <- c(ss , paste(itspace, op$help, sep=''));
      } else {
         ss <- c(ss, paste(itspace, parnames[ii], 
                           '(no help available)\n', sep='')); 
      }
   }
   ss <- paste(ss, sep='\n');
   cat(intro, ex1, ex2, ex3, ss, reference.MEMA(), sep='\n');
   
   if (adieu) exit.AFNI();
}

#Change command line arguments into an options list
read.MEMA.opts.batch <- function (args=NULL, verb = 0) {
   params <- list (
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output prefix (just prefix, no view+suffix needed)\n"
                     ) ),
      
      '-jobs' = apl(n = 1, d = 1, h = paste(
   "-jobs NJOBS: On a multi-processor machine, parallel computing will speed ",
   "             up the program significantly.",
   "             Choose 1 for a single-processor computer.\n", sep = '\n'
                     ) ),
      
      '-groups' = apl(n = c(1,2), d = 'G1', h = paste(
   "-groups GROUP1 [GROUP2]: Name of 1 or 2 groups.\n",
   "                         Default is one group named 'G1'\n"
                     ) ),
                     
      '-set' = apl (n = c(4, Inf), d = NA, dup = TRUE, h = paste (
   "-set SETNAME                         \\\n",
   "              SUBJ_1 BETA_DSET T_DSET \\\n",
   "              SUBJ_2 BETA_DSET T_DSET \\\n",
   "              ...   ...       ...     \\\n",
   "              SUBJ_N BETA_DSET T_DSET \\\n",
   "     Specify the data for one of two test variables (either group,\n",
   "             conditions/tasks/GLTs) A & B. \n",
   "     SETNAME is the name assigned to the set. \n",
   "     SUBJ_K is the label for the subject K whose datasets will be \n",
   "            listed next\n",
   "     BETA_DSET is the name of the dataset of the beta coefficient or GLT.\n",
   "     T_DSET is the name of the dataset containing the Tstat \n", 
   "            corresponding to BETA_DSET. \n",
   "        To specify BETA_DSET, and T_DSET, you can use the standard AFNI \n",
   "        notation, which, in addition to sub-brick indices, now allows for\n",
   "        the use of sub-brick labels as selectors\n",
   "     e.g: -set Placebo Jane pb05.Jane.Regression+tlrc'[face#0_Beta]'  \\\n",
   "                            pb05.Jane.Regression+tlrc'[face#0_Tstat]' \\\n" 
                        ) ),
                        
      '-conditions' = apl(n = c(1,2), h = paste (
   "-conditions COND1 [COND2]: Name of 1 or 2 conditions, tasks, or GLTs.\n",
   "                           Default is one condition named 'c1'\n"   
                  ) ),
                  
      '-max_zeros' = apl(n = 1, d = 0, h = paste(
   "-max_zeros MM: Do not compute statistics at any voxel that has \n",
   "               more than MM zero beta coefficients or GLTs. Voxels around\n",
   "               the edges of the group brain will not have data from\n",
   "               some of the subjects. Therefore, some of their beta\'s or\n",
   "               GLTs and t-stats are masked with 0. 3dMEMA can handle\n",
   "               missing data at those voxels but obviously too much\n",
   "               missing data is not good. Setting -max_zeros to 0.25\n",
   "               means process data only at voxels where no more than 1/4\n",
   "               of the data is missing.\n",
   "               The default value is 0 (no missing values allowed).\n",
   "               MM can be a positive integer less than the number of\n",
   "               subjects, or a fraction beteen 0 and 1.\n" 
                           ) ),
                            
      '-n_nonzero' = apl(n = 1, d = -1, h = paste(
   "-n_nonzero NN: Do not compute statistics at any voxel that has \n",
   "               less than NN non-zero beta values. This options is\n",
   "               complimentary to -max_zeroes, and matches an option in\n",
   "               the interactive 3dMEMA mode. NN is basically (number of\n",
   "               unique subjects - MM). \n"
                           )),
                            
      '-HKtest' = apl(0 , d = NA, h = paste(
   "-HKtest: Perform Hartung-Knapp adjustment for the output t-statistic. \n",
   "         This approach is more robust when the number of subjects\n",
   "         is small. However it is a little more conservative.\n",
   "         -no_KHtest is the default with t-statistic output.\n"
                     ) ),
                      
      '-no_HKtest' = apl(0, d=NA, h = paste(
   "-no_HKtest: Do not make the Hartung-Knapp adjustment (Default).\n"
                     ) ),
                     
      '-mask' = apl(1, h = paste(
   "-mask MASK: Process voxels inside this mask only.\n",
   "            Default is no masking.\n"
                     ) ),
                     
      '-model_outliers' = apl(0, h = paste(
   "-model_outliers: Model outlier betas with a Laplace distribution of\n",
   "                 of subject-specific error.\n",
   "                 Default is -no_model_outliers\n"
                     ) ),
                      
      '-no_model_outliers' = apl(0, h = paste(
   "-no_model_outliers: No modeling of outlier betas/GLTs (Default).\n"   
                     ) ),
                     
      '-residual_Z' = apl(0, h = paste(
   "-residual_Z: Output residuals and their Z values used in identifying\n",
   "             outliers at voxel level.\n",
   "             Default is -no_residual_Z\n"
                     ) ),
                      
      '-no_residual_Z' = apl(0, h = paste(
   "-no_residual_Z: Do not output residuals and their  Z values (Default).\n"
                     ) ),
                     
      '-unequal_variance' = apl(0, h = paste(
   "-unequal_variance: Model cross-subjects variability difference between\n",
   "                   GROUP1 and GROUP2 (heteroskedasticity). Default is\n",
   "                   -equal_variance (homoskedasticity).\n"
                     ) ),
                      
      '-equal_variance' = apl(0, h = paste(
   "-equal_variance: Assume same cross-subjects variability between GROUP1\n",
   "                 and GROUP2 (homoskedasticity). (Default)\n"
                     ) ), 
      
      '-covariates' = apl(n=1, d=NA, h=paste(
   "-covariates COVAR_FILE: Specify the name of a text file containing\n",
   "                        a table for the covariate(s). Each column in the\n",
   "                        file is treated as a separate covariate, and each\n",
   "                        row contains the values of these covariates for\n",
   "                        each subject. \n",
   "     To avoid confusion, it is best you format COVAR_FILE in this manner\n",
   "     with BOTH row and column names: \n",
   "        subj  age   weight\n",
   "        Jane   25   300\n",
   "        Joe    22   313\n",
   "        ...    ..   ...\n",
   "     This way, there is no amiguity as to which values are attributed to\n",
   "     which subject, nor to the label of the covariate(s). The word 'subj'\n",
   "     must be the first word of the first row. You can still get at the  \n",
   "     values of the columns of such a file with AFNI's 1dcat -ok_text, \n",
   "     which will treat the first row, and first column, as all 0s.\n",
   "     Alternate, but less recommended ways to specify the covariates:\n",
   "     (column names only)\n",
   "        age   weight\n",
   "        25   300\n",    
   "        22   313\n",
   "        ..   ...\n",  
   "     or\n",
   "     (no row and column names)\n",
   "        25   300\n",    
   "        22   313\n",
   "        ..   ...\n" 
                                 ) ),
                                 
      '-covariates_center' = apl(c(1,Inf),  h = paste(
   "-covariates_center COV_1=CEN_1 [COV_2=CEN_2 ... ]: (for 1 group) \n",' ',
   "-covariates_center COV_1=CEN_1.A CEN_1.B [COV_2=CEN_2.A CEN_2.B ... ]: \n",
   "                                                    (for 2 groups) \n",
   "    where COV_K is the name assigned to the K-th covariate, \n",
   "    either from the header of the covariates file, or from the option\n",
   "    -covariates_name. This makes clear which center belongs to which\n",
   "    covariate. When two groups are used, you need to specify a center for\n",
   "    each of the groups (CEN_K.A, CEN_K.B).\n",
   "    Example: If you had covariates age, and weight, you would use:\n",
   "           -covariates_center age = 78 55 weight = 165 198\n",
   "    If you want all covariates centered about their own mean, \n",
   "    just use -covariates_center mean. Be alert: Default is no centering,\n",
   "    which is NOT recommended unless you know what you're doing!!!\n"
               ) ),
               
      '-covariates_model' = apl(c(2),  h = paste(
   "-covariates_model center=different/same slope=different/same:\n",
   "         Specify whether to use the same or different intercepts\n",
   "         for each of the covariates. Similarly for the slope.\n"
               ) ),
               
      '-covariates_name' = apl(c(1,Inf),  h = paste(
   "-covariates_name COV_1 [... COV_N]: Specify the name of each of the N\n",
   "             covariates. This is only needed if the covariates' file \n",
   "             has no header. The default is to name the covariates\n",
   "             cov1, cov2, ... \n"
               ) ),
      '-contrast_name: ' = apl(1),
      '-verb' = apl(n=1, d = 0, h = paste(
   "-verb VERB: VERB is an integer specifying verbosity level.\n",
   "            0 for quiet (Default). 1 or more: talkative.\n"
                        ) ),
      '-help' = apl(n=0, h = '-help: this help message\n'),
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" )
      
         );
                     
   ops <- parse.AFNI.args(args,  params,
                          other_ok=FALSE
                          )
   if (verb) show.AFNI.args(ops, verb=0, hstr='');
   if (is.null(ops)) {
      errex.AFNI('Error parsing arguments. See 3dMEMA -help for details.');
   }
   
   #Parse dems options
   #initialize with defaults
      lop <- list ()
      lop$nNodes <- 1
      lop$nNonzero <- -1
      lop$nMaxzero <- -1
      lop$KHtest <- FALSE
      lop$maskFN <- NULL
      lop$covFN <- NULL
      lop$lapMod <- 0
      lop$resZout <- 0
      lop$homo <- 1
      lop$nLevel <- 0
      lop$test <- NULL
      lop$grpName <- 'G1'
      lop$conLab <- 'c1'
      lop$testName <- NULL
      lop$nCov <- 0
      lop$covFN <- NULL
      lop$covMatrix <- NULL
      lop$covData <- NULL
      lop$covName <- NULL
      lop$centerType <- 0
      lop$centerVal <- NULL
      lop$centerType <- 0
      lop$centerType2 <- 0
      lop$contrastName <- NULL
      lop$verb <- 0
   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             prefix = lop$outFN  <- prefix.AFNI.name(ops[[i]]),
             jobs   = lop$nNodes <- ops[[i]],
             groups = lop$grpName <- ops[[i]],
             conditions = lop$conLab <- ops[[i]],
             set  = lop <- MEMA.parse.set(lop, ops[[i]]),
             n_nonzero = lop$nNonzero <- ops[[i]],
             max_zeros = lop$nMaxzero <- ops[[i]],
             HKtest = lop$KHtest <- TRUE,
             no_HKtest = lop$KHtest <- FALSE,
             mask = lop$maskFN <- ops[[i]],
             model_outliers = lop$lapMod <- 1,
             no_model_outliers = lop$lapMod <- 0,
             residual_Z = lop$resZout <- 1,
             no_residual_Z = lop$resZout <- 0,
             unequal_variance = lop$homo <- 0,
             equal_variance = lop$homo <- 1,
             test = lop$test <- ops[[i]],
             covariates = lop$covFN <- ops[[i]],
             covariates_center = 
                  lop <- MEMA.parse.covariates_center(lop, ops[[i]]),
             covariates_model = 
                  lop <- MEMA.parse.covariates_model(lop, ops[[i]]),
             covariates_name = lop$covName <- ops[[i]],
             contrast_name  = lop$contrastName <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             help = help.MEMA.opts(params, adieu=TRUE),
             show_allowed_options = show.AFNI.args(ops, verb=0, 
                                              hstr="3dMEMA's",adieu=TRUE)

             )
   }

   #No figure out some other variables 
   lop$nGrp <- length(lop$grpName)
   lop$nSubj <- vector('numeric')
   for (ii in 1:lop$nGrp) {
      lop$nSubj <- c(lop$nSubj,length(lop$subjLab[[ii]]))
   } 
   names(lop$nSubj) <- lop$grpName
   
   if (lop$nGrp != 1 && lop$nGrp != 2) {
     warning(paste('You must have either one or two groups'),
               immediate.=TRUE);
     return(NULL); 
   }
   if (lop$nGrp != 1 && lop$nGrp != 2) {
     warning(paste('You must have either one or two groups'),
               immediate.=TRUE);
     return(NULL); 
   }
   lop$nLevel <- length(lop$conLab)
   
   lop$anaType <- -1
   if (lop$nGrp == 1) {
      if (lop$nLevel == 1) lop$anaType <- 1
      else if (lop$nLevel == 2) {
         for (i in 1:lop$nSubj) {
            if (lop$subjLab[[1]][i] != lop$subjLab[[2]][i]) {
               warning(paste( 'mismatch in subject labels for ',
                              'within group test'),
               immediate.=TRUE);
               return(NULL); 
            }
         }
         lop$anaType <- 3
      }
   } else if (lop$nGrp == 2) {
      if (lop$nLevel == 1 && lop$homo)lop$anaType <- 2
      else if (lop$nLevel == 1 && !lop$homo)lop$anaType <- 4
   }
   if (lop$anaType < 0) {
      warning(paste('Could not set analysis type'),
               immediate.=TRUE);
     return(NULL); 
   }
   if (lop$centerType < 0 || lop$centerType2 < 0) {
     warning(paste('Could not set covariates centering, or model'),
               immediate.=TRUE);
     return(NULL); 
   }

   #Set the testnames
   if (is.null(lop$testName)) {
      if(lop$nGrp==1) {
         if (lop$nLevel==1) {
            lop$testName[[1]] <- names(lop$bFN)[1] 
         } else {
            lop$testName[[1]] <- 
                     sprintf('%s-%s', names(lop$bFN)[2], names(lop$bFN)[1]) 
         }
      } else { 
         lop$testName <- lop$grpName
  	   }
   }
   #one more sanity test
   if (lop$anaType==1 | lop$anaType==2 | lop$anaType==4) {
      if (length(lop$testName) != length(lop$grpName)) {
         err.AFNI(paste("Bad testName of ", paste(lop$testName, collapse=' '), 
                  "for group", 
                  paste(lop$grpName, collapse=' ')) );
         return(NULL);
      }
   } else {
      if (length(lop$testName) != 1) {
         err.AFNI("Need only one string in testName");
         return(NULL);
      }
   }
   
   #Get a unique list of all subjects 
   allsubj <- AllSubj.MEMA (lop$subjLab)
   
   #Get the covariates
   if (!is.null(lop$covFN)) {
      
      lop$covMatrix <- read.AFNI.matrix(lop$covFN, lop$covName, allsubj)
   
      #retrieve some components
      lop$covName <- colnames(lop$covMatrix)
      lop$nCov <- length(lop$covName)
      #It would be better if all references to lop$covMatrix,
      #lop$covName, and lop$nCov were obtained live from
      #lop$covData beyond this point. If that were to be done
      #them covMatrix, covName, and nCov would be local to thi
      #function only. But that requires more changes to the interactive
      #input function
      lop$covData <- data.frame((lop$covMatrix)) 
      
      if (lop$centerType == 1) {
         if ( (length(lop$covName) != length(lop$centerVal)) &&
              (length(lop$covName)*lop$nGrp != length(lop$centerVal)) ) {
            warning(paste( 'Mismatch between number of covariates "',
                           paste(lop$covName, collapse=','), '"(',
                           length(lop$covName),'*',lop$nGrp,
                           ')and number of center values',
                           length(lop$centerVal),'.'),
                    immediate.=TRUE);
            return(NULL); 
         }
         if (is.null(names(lop$centerVal))) {
            warning(paste( 'Assuming following centers\n   ',
                           paste(lop$centerVal,collapse=', '),
                           '\n for covariates \n   ',
                           paste(lop$covName, collapse=', '), sep=''),
                     immediate.=TRUE);
            if (length(lop$covName) < length(lop$centerVal)) {
               ww <- vector('character');
               for (i in 1:lop$covName) {
                  ww <- c(ww, rep(lop$covName[i],lop$nGrp))
               }
               names(lop$centerVal) <- ww
            } else {
               names(lop$centerVal) <- lop$covName
            }
         } else {
            #Make sure covariates center match covariate names
            dd <- lop$covName[!(lop$covName %in% names(lop$centerVal))]
            if (length(dd)) {
               warning (paste('Covariates ', paste(dd,collapse=' '),
                           ' do not match a covariate centering entry.\n'),
                     immediate.=TRUE);
               return(NULL);
            }
         }   
      } else if (lop$centerType == 0) {
         warning('Covariates will be centered around their respective means',
                  immediate.=TRUE);
      }
       
   }

   if (lop$nNonzero >= 0 && lop$nMaxzero >= 0) {
      warn.AFNI('Cannot set both of -n_nonzero and -max_zeros');
      return(NULL)
   }
   

   return(lop)
}# end of read.MEMA.opts.batch

#Change options list to 3dMEMA variable list 
process.MEMA.opts <- function (lop, verb = 0) {
   if (file.exists(paste(lop$outFN,"+orig.HEAD", sep="")) || 
         file.exists(paste(lop$outFN,"+tlrc.HEAD", sep=""))) {
         warning((paste("File ", lop$outFN, "exists! Try a different name.\n")),
                  immediate.=TRUE);
         return(NULL);
   }      
   lop$outFN <- 
      paste(lop$outFN, "+orig", sep="") # write.AFNI doesn't handle tlrc yet


   if (lop$nNodes < 1) lop$nNodes <- 1
    
   if (lop$nGrp < 1) lop$nGrp <- 1
   
   if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4) {
      if (length(lop$grpName) != lop$nGrp) {
         stop ('bad length for grpName');
      }
      if (length(lop$nSubj) != lop$nGrp) {
         stop ('bad length for nSubj');
      }
      if (length(lop$subjLab) != lop$nGrp) {
         stop ('bad length for subjLab');
      }
      lop$bList <- vector('list', lop$nGrp)
      lop$tList <- vector('list', lop$nGrp) 
      lop$varList <- vector('list', lop$nGrp)
   
      for(ii in 1:lop$nGrp) {

  	      lop$nFiles[ii] <- 2*lop$nSubj[ii]
         if (length(lop$subjLab[[ii]]) != lop$nSubj[ii]) {
            stop (cat('bad length for subjLab for ', ii,'\n'));
         }      
         if (length(lop$bFN[[ii]]) != lop$nSubj[ii]) {
            stop (cat('bad length for bFN for ', ii,'\n'));
         }      
         if (length(lop$tFN[[ii]]) != lop$nSubj[ii]) {
            stop (cat('bad length for tFN for ', ii,'\n'));
         }
          
         if (verb) {
            cat ('Beta Filenames:', paste(lop$bFN[[ii]], collapse=''), '\n') 
         }
         lop$bList[[ii]] <- lapply(lop$bFN[[ii]], read.AFNI); 
         lop$tList[[ii]] <- lapply(lop$tFN[[ii]], read.AFNI);
         if(ii==1) { 
            lop$myNote=lop$bList[[1]][[1]]$header$HISTORY_NOTE; 
            lop$myOrig=lop$bList[[1]][[1]]$origin; 
            lop$myDelta=lop$bList[[1]][[1]]$delta; 
            lop$myDim <- lop$bList[[1]][[1]]$dim;
         }
         lapply(lapply(lop$bList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch among the beta input files!") )
         lapply(lapply(lop$tList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch between beta ",
                          " and t-statistic files!"))

      } # for(ii in 1:lop$nGrp)
   } else { # if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4)
      if (is.null(lop$contrastName)) {
         lop$contrastName <- paste(lop$conLab[c(2,1)],collapse='-');
      }
   } # if(lop$anaType==3)
   
   if (!is.null(lop$covFN) && 
       ( (lop$centerType2 == 1 || lop$centerType2 == 3) && !lop$homo) ) {
      warning (paste('Cannot use -unequal_variance with slope=different \n',
                     '  in -covariates_model'),
                     immediate.=TRUE);
      return(NULL); 
   }

   if (lop$centerType > 0) {
      #Now be sure the orders match
      init_centerVal <- lop$centerVal
      lop$centerVal <- vector('numeric')
      for (i in 1:length(lop$covName)) {#must do it like this
                                        #Can have multiple values
                                        #per covariates!
         lop$centerVal <- c(lop$centerVal,                              
               init_centerVal[lop$covName[i]==names(init_centerVal)])
      }
   }      

   if(lop$anaType==3) {
      lop$bList <- vector('list', lop$nLevel)
      lop$tList <- vector('list', lop$nLevel)
      lop$varList <- vector('list', lop$nLevel)

      if (verb) {
         cat ('Have grp Name:', lop$grpName[[1]],'\n');
         cat ('Have nSubj:', lop$nSubj[1],'\n');
      }
      lop$nFiles[1] <- 2*lop$nSubj[1]  
                        # 2 because of 1 beta and 1 t-statistic
      for(jj in 1:lop$nSubj[1]) {
         if (verb) {
            cat ('Have lop$subjLab[[1]][[jj]]', lop$subjLab[[1]][[jj]], '\n')
         }
      }
      for(ii in 1:lop$nLevel) {
         if (verb) {
            cat ('Have lop$conLab[[ii]]', lop$conLab[[ii]], '\n')
         }
         for(jj in 1:lop$nSubj[1]) {
            if (verb) {
               cat ('Have lop$bFN[[ii]][[jj]]', lop$bFN[[ii]][[jj]], '\n')
               cat ('Have lop$tFN[[ii]][[jj]]', lop$bFN[[ii]][[jj]], '\n')
            }
         }
         lop$bList[[ii]] <- lapply(lop$bFN[[ii]], read.AFNI); 
         lop$tList[[ii]] <- lapply(lop$tFN[[ii]], read.AFNI);
         if(ii==1) {
            lop$myNote=lop$bList[[1]][[1]]$header$HISTORY_NOTE; 
            lop$myOrig=lop$bList[[1]][[1]]$origin; 
            lop$myDelta=lop$bList[[1]][[1]]$delta; 
            lop$myDim <- lop$bList[[1]][[1]]$dim
         }
         lapply(lapply(lop$bList[[ii]], function(x) x$dim), 
                function(x) 
                  if(!all(x==lop$myDim)) 
                     stop("Dimension mismatch among the beta input files!"))
         lapply(lapply(lop$tList[[ii]], function(x) x$dim), 
               function(x) 
                  if(!all(x==lop$myDim)) 
                     stop( "Dimension mismatch between beta ",
                           "and t-statistic files!"))

      } # for(ii in 1:lop$nLevel)  
   
   } # if(lop$anaType==3)

   
   if (verb) {
      cat ('Have lop$KHtest', lop$KHtest, '\n');
   }
   if (verb) {
      cat ('have lop$maskFN', lop$maskFN, '\n');
   }
   if(!is.null(lop$maskFN)) {
      if (verb) {
         cat ("Will read ", lop$maskFN,'\n');
      }
      if (is.null(mm <- read.AFNI(lop$maskFN))) {
         warning("Failed to read mask", immediate.=TRUE);
         return(NULL);
      }
      lop$maskData <- mm$brk
      if (verb) cat ("Done read ", lop$maskFN,'\n');
   }
   if(!is.null(lop$maskFN)) 
      if(!all(dim(lop$maskData[,,,1])==lop$myDim[1:3])) 
         stop("Mask dimensions don't match the input files!")

   if(lop$nGrp==1) lop$xMat <- rep(1, sum(lop$nSubj))      
   if(lop$nGrp==2) 
      lop$xMat <- cbind(rep(1, sum(lop$nSubj)), 
                    c(rep(0, lop$nSubj[1]), rep(1, lop$nSubj[2]))) 
                                          # dummy variable for two groups
   if(!is.null(lop$covFN)) {
      if (1) {
         if (verb) {
            cat ('have lop$covFN=', lop$covFN, lop$covName, '\n');
         }
      }
      
       cat ('Have lop$centerType', lop$centerType, '\n');
      #browser()
      if(lop$nGrp == 1) {
         if(lop$centerType == 0) {
            lop$covData <- apply(lop$covData, 2, scale, scale=F)  
                     # center around mean for each covariate (column)
         }
         if(lop$centerType == 1) {  # center around a user-specified value
            #centerVal <- vector(mode = "numeric", length = lop$nCov)
            for(ii in 1:lop$nCov) {
               cat ( 'Have lop$centerVal[ii]', lop$centerVal[ii], 
                     'for', names(lop$covData)[ii], '\n');
            }
            lop$covData <- t(as.matrix(apply(lop$covData, 1, "-", 
                                             lop$centerVal)))
         }
      } # if(lop$nGrp == 1)

      if (lop$nGrp == 2) {
         cat ('Have lop$centerType2', lop$centerType2, '\n');
         if(lop$centerType2 == 0 | lop$centerType2 == 1) {   # same center 
            if(lop$centerType == 0) 
               lop$covData <- apply(lop$covData, 2, scale, scale=F)  
                                 # center around mean for each covariate (column)
            if(lop$centerType == 1) {  
                        # center around other value for each covariate (column)
               for(jj in 1:lop$nCov) {
                  cat ('Have lop$centerVal[jj]', lop$centerVal[jj], '\n');
               }
               lop$covData <- t(as.matrix(
                                 apply(lop$covData, 1, "-", lop$centerVal)))
            }
         } # if(lop$centerType2 == 0 | lop$centerType2 == 1)
        
         if(lop$centerType2 == 2 | lop$centerType2 == 3) {  # different center 
            if(lop$centerType == 0) 
               lop$covData <- 
                        rbind(apply(as.matrix(lop$covData[1:lop$nSubj[1],]), 
                                    2, scale, scale=F),
                              apply(as.matrix(lop$covData[
                                 (lop$nSubj[1]+1):(lop$nSubj[1]+lop$nSubj[2]),]),
                                    2, scale, scale=F))
            if(lop$centerType == 1) {
               covList <- vector('list', lop$nGrp)               
               for(ii in 1:lop$nGrp) {
                  centerVal <- 
                      lop$centerVal[seq(ii,length(lop$centerVal),lop$nGrp)]
                  cat ('Have lop$centerVal', 
                        paste(centerVal, collapse=','), 
                        'for group ',ii,'\n');
                  covList[[ii]] <- t(apply(as.matrix(
                                 lop$covData[((ii-1)*lop$nSubj[1]+1):
                                             (lop$nSubj[1]+(ii-1)*lop$nSubj[2]),]
                                                      ), 1, "-", centerVal))
               }
               lop$covData <- rbind(covList[[1]], covList[[2]])
            } # if(lop$centerType == 1)
         } # if(lop$centerType2 == 3)
         
         if(lop$centerType2 == 1 | lop$centerType2 == 3) { # different slope
            lop$covData <- cbind(lop$covData, lop$covData*lop$xMat[,2])  
                                 # add one column per covariate for interaction
            lop$nCov <- 2*lop$nCov 
                              # double number of covariates due to interactions
            lop$covName<-c(lop$covName, paste(lop$covName, "X", sep=''))  
                              # add names for those interactions
         }
      }
      #browser()
      lop$xMat <- as.matrix(cbind(lop$xMat, lop$covData))           
   } else {lop$nCov <- 0; lop$xMat <- as.matrix(lop$xMat)}


   if (verb) cat ('Have lop$lapMod', lop$lapMod,'\n');
   if (verb) cat ('Have lop$resZout', lop$resZout, '\n');
   if(lop$resZout==1) {
      lop$icc_FN  <- paste(strsplit(lop$outFN, "\\+")[[1]][1], 
                           "_ICC+orig", sep="")
      lop$resZ_FN <- paste(strsplit(lop$outFN, "\\+")[[1]][1], 
                           "_resZ+orig", sep="") 
                  # write.AFNI doesn't handle tlrc yet
   }
   
   #Get the total number of unique subjects
   allsubj <- AllSubj.MEMA (lop$subjLab)
   if (lop$nNonzero < 0) { 
      if (lop$nMaxzero >= 0) {
         if (lop$nMaxzero > 0 && lop$nMaxzero < 1) { #fraction
            lop$nMaxzero <- round(lop$nMaxzero * length(allsubj))
            if (lop$nMaxzero > length(allsubj)) lop$nMaxzero <- length(allsubj) 
         }
         lop$nNonzero <- length(allsubj) - lop$nMaxzero
      } else {
         lop$nNonzero <- length(allsubj)
      }
   } 
   if (lop$nNonzero > 0 && lop$nNonzero < 1) {
      lop$nNonzero <- round(lop$nNonzero*length(allsubj))
      if (lop$nNonzero > length(allsubj)) lop$nNonzero <- length(allsubj)
   }
   
   if (lop$nNonzero < 0.75 * length(allsubj)) {
      warn.AFNI(c(
   'You are allowing computations at voxels which may be missing \n',
   'more than 1/3 of the data!') ) 
   }
   
   if (verb) {
      cat ('Have lop$nNonzero', lop$nNonzero, '\n');
   }
   return(lop)
}

#Check of DOF constraints
DOF.check.MEMA <- function (lop)  {
   #str(dim(lop$xMat))
   if (dim(lop$xMat)[1] <= dim(lop$xMat)[2]) {
      warning (paste("Too few subjects for options selected."),
               immediate.=TRUE);
      return(FALSE);
   } else if (dim(lop$xMat)[1] <= 2*dim(lop$xMat)[2]) {
      warning (paste("Number of parameters to estimate is more than ",
                     "half the number of subjects. Consider increasing",
                     "the number of subjects.", sep='\n'),
               immediate.=TRUE);
      return(TRUE);
   }
   return(TRUE);
}



#################################################################################
################# Begin MEMA Computation functions ##############################
#################################################################################

### modified rma version 0.562 by Wolfgang Viechtbauer, Department of Methodology and Statistics, University of Maastricht
# Further modifications

# handles one group or two groups with homogeneity assumption
rmaB <- function( yi, vi, n, p, X, resOut, lapMod, 
                  knha=FALSE, con=list(thr=10^-8, maxiter=50, thrZ=1.3)) {

# yi: vector of dependent variable values
# vi: corresponding variances of yi 
# n: number of rows
# p: number of regressors (columns in design matrix X) including covariates   
# X: fixed effects matrix including covariates 
# knha: adopting knha t-test?
# con$thr: converging threshold for REML
# con$maxiter: maximum # of converging iterations for REML

	Y <- as.matrix(yi)
	   
	tr <- function(X) sum(diag(X))

   W0     <- diag(1/vi)
   tmp0   <- t(X) %*% W0
   #P0     <- W0 - t(tmp0) %*% solve(tmp0 %*% X) %*% tmp0
   # catch the case when matrix tmp0 %*% X is singular 
   continue <- TRUE
   tryCatch(P0 <- W0 - t(tmp0) %*% solve(tmp0 %*% X) %*% tmp0, error = function(w) continue <<- FALSE)
   
   if(continue) {
   QE     <- t(Y) %*% P0 %*% Y   
   
   collect <- function(Y, vb, R, P, n, p, knha, con) {
      # force t(Y) %*% P %*% Y to be 0 in case it's numerically 0 but negative
      if(knha) vb <- max((c( t(Y) %*% P %*% Y ) / (n-p)), 0) * vb      
      se <- sqrt(diag(vb))
       b    <- R %*% Y
      z <- ifelse(se>con$thr, b/se, 0)
      out <- list(se, b, z)
      names(out) <- c("se", "b", "z")
      out
   }

   MoM <- function(Y, vi, n, p, X, P0, QE, knha, con) {
      #RSS    <- t(Y) %*% P0 %*% Y
      tau2   <- ( QE - (n-p) ) / tr(P0)  # RSS = QE
      tau2 <- max(0, c(tau2))
      W   <- diag(1/(vi + tau2))     
      tmp <- t(X) %*% W
         
      vb  <- solve(tmp %*% X)
      R   <- vb %*% tmp  # projection of Y to space spanned by X
      P   <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out <- list(stat$se, stat$b, stat$z, tau2, P, 0, 0)
      names(out) <- c("se", "b", "z", "tau2", "P", "meth", "iter")
      out
   }                 

   
   Laplace <- function(Y, vi, n, p, X, knha, con=list(thr=2*10^-5, maxiter=50)) {
   ### looks like you can't get the precision lower than con$thr = 10^-5!!!

   conv		<- 1
	change_b	<- 1000
   change_nu	<- 1000
	iter		<- 0
   
   nu       <- sqrt(max(0, var(Y) - mean(vi)))/sqrt(2)
   if(nu < 10^-10) nu <- sqrt(max(0, var(Y) - mean(vi)/3))
   W		   <- diag(1/(vi + 2*nu^2))  # need to deal with 0s' here?
   tmp  <- t(X) %*% W   
   b    <- solve(tmp %*% X) %*% tmp %*% Y
   visr <- sqrt(vi)

	# seems two ways to update beta (b): one through iterative WLS, while the other via its own iterations
   
   if(nu > 10^-10) {
   while (any(change_b > con$thr) & (change_nu > con$thr)) {
		iter     <- iter + 1
      if (iter > con$maxiter) {  # Laplace fails
			conv    <- 0
			break
		}
		b.old    <- b
      nu.old   <- nu    # scalor
      ei <- Y - X %*% b.old    # n X 1 vector
      Ei <- exp(ei/nu)       # n X 1 vector
      temp1 <- visr/nu; temp2 <- ei/visr  # n X 1 vector: dealing 0s for visr?
      Phi1 <- pnorm(-temp1 - temp2)   # n X 1 vector
      Phi2 <- pnorm(temp2 - temp1)    # n X 1 vector
      phi1 <- dnorm(-temp1 - temp2)   # n X 1 vector
      phi2 <- dnorm(temp2 - temp1)    # n X 1 vector
      Gi <- Phi1 * Ei + Phi2 / Ei     # n X 1 vector: dealing 0s for Ei?
      
      dLb <- vector(mode="numeric", length=p)
      H <- matrix(data = 0, nrow = p+1, ncol = p+1, byrow = FALSE, dimnames = NULL)
      g <- matrix(data = 0, nrow = p+1, ncol = 1, byrow = FALSE, dimnames = NULL)
      for(ii in 1:n) { 
         dLb  <- (phi1[ii]*t(X[ii,])*Ei[ii]/visr[ii] - t(X[ii,])*Ei[ii]*Phi1[ii]/nu
                 + (t(X[ii,])*Phi2[ii]/Ei[ii])/nu - (phi2[ii]*t(X[ii,])/visr[ii])/Ei[ii])/Gi[ii]
         dLnu <- -1/nu - vi[ii]/nu^3 + (Ei[ii]*ei[ii]*Phi1[ii]/nu^2 + visr[ii]*Ei[ii]*phi1[ii]/nu^2
                 - (ei[ii]*Phi2[ii]/Ei[ii])/nu^2 + (visr[ii]*phi2[ii]/Ei[ii])/nu^2)/Gi[ii]        
         ttemp <- t(dLb)*dLnu
         #H <- H + rbind(cbind(dLb %*% t(dLb), t(ttemp)), cbind(ttemp, dLnu^2))
         H <- H + rbind(cbind(t(dLb) %*% dLb, ttemp), cbind(t(ttemp), dLnu^2))
         g <- g + rbind(t(dLb), dLnu)
      }      
      
      if(any(is.infinite(H)) | any(is.infinite(H)) | any(is.infinite(g)) | any(is.infinite(g)) |
         any(is.nan(H)) | any(is.nan(H)) | any(is.nan(g)) | any(is.nan(g))) { # failed because of infinity issue
         conv    <- 0
         break
		}
      
      suc <- TRUE
      tryCatch(adj <- solve(H) %*% g, error = function(w) suc <<- FALSE)   # (p+1) X 1 vector
      if(!suc) {  # Laplace fails
			conv    <- 0
			break
		}
      
      b  <- b.old + adj[1:p]
      nu <- nu + adj[p+1]
      change_b	   <- abs(b.old - b)
      change_nu	<- abs(nu.old - nu)
	}
   } else conv <- 0

   if(conv==0) {
      out <- list(conv)
      names(out) <- c("conv")
   } else {   # Laplace succeeded
      W	  <- diag(1/(vi + 2*nu^2))
      tmp  <- t(X) %*% W
      vb   <- solve(tmp %*% X)   # variance-covariance matrix
      R    <- vb %*% tmp  # projection of Y to space spanned by X
      P    <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      tau2 <- 2*nu^2
      
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out  <- list(stat$se, stat$b, stat$z, tau2, P, 1, 1, iter)
      names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
   }
   out
   } # end of Laplace
   
   REML <- function(Y, X, v, knha, n, p, con) {
      conv		<- 1
      adj	   <- 1000
      iter		<- 0
      tau2     <- max(0, var(Y) - mean(v))  # tau^2 for group 1
      W		   <- diag(1/(v + tau2))
      tmp      <- t(X) %*% W
      vb       <- solve(tmp %*% X)   # variance-covariance matrix
      R        <- vb %*% tmp  # projection of Y to space spanned by X
      P        <- W - t(tmp) %*% R
      while(abs(adj) > con$thr) {
         iter     <- iter + 1  # iteration counter
         if (iter > con$maxiter) {
            conv    <- 0
            break
         }
         tau2.old <- tau2
         py    <- P%*%Y
         adj	<- solve( tr(P%*%P) ) %*% ( t(py)%*%py - tr(P) )
         while (tau2 + adj < 0) adj <- adj / 2
         tau2		<- tau2 + adj
         W		   <- diag(1/(v + tau2))
         tmp <- t(X) %*% W
         vb  <- solve(tmp %*% X)   # variance-covariance matrix
         R   <- vb %*% tmp  # projection of Y to space spanned by X
         P <- W - t(tmp) %*% R
      }

      if(conv==0) { # REML failed
         out <- list(conv)
         names(out) <- c("conv")
      } else {   # REML succeeded         
         stat <- collect(Y, vb, R, P, n, p, knha, con)
         out <- list(stat$se, stat$b, stat$z, tau2, P, 1, 2, iter)
         names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
      }
      out
   }  # end of REML

   # try MoM first: if not significant for sure, then not worth trying other costy methods
   outMoM <- MoM(Y, vi, n, p, X, P0, QE, knha, con)
   se   <- outMoM$se
   b    <- outMoM$b
   z    <- outMoM$z
   tau2 <- outMoM$tau2
   P    <- outMoM$P
   meth <- outMoM$meth
   iter <- outMoM$iter   
   
   if(lapMod==1) {
      resZ <- P %*% Y / sqrt(diag(P %*% tcrossprod(diag(tau2+vi), P)))
      # try Laplace only if either likely significant or suspicious of outliers
      #browser()
      noMoM <- any(abs(outMoM$z) > con$thrZ) | any(abs(resZ) > con$thrZ)
      if(noMoM) {
         outLap <- Laplace(Y, vi, n, p, X, knha)
         if(outLap$conv == 1) {
            se   <- outLap$se
            b    <- outLap$b
            z    <- outLap$z
            tau2 <- outLap$tau2
            P    <- outLap$P
            meth <- outLap$meth
            iter <- outLap$iter
         } else lapMod <- 0  # switch to REML first here when Laplace fails
      } 
   }
   
   if(lapMod==0) { # if Laplace is not selected or fails, try REML
      # try REML only if likely significant
      noMoM <- any(abs(outMoM$z) > con$thrZ) 
      if(noMoM) {
         outREML <- REML(yi, X, vi, knha, n, p, con)
         if(outREML$conv == 1) {
            se   <- outREML$se
            b    <- outREML$b
            z    <- outREML$z
            tau2 <- outREML$tau2
            P    <- outREML$P
            meth <- outREML$meth
            iter <- outREML$iter
         }
      }
   }
   
   if(resOut==1) { # residual statistics requested
      vTot <- tau2+vi  # total variance
      lamc <- ifelse(vTot>con$thr, vi/vTot, 0)  # Like ICC, lamda shows percent of variation at group level   
      # need to scale this for Knapp & Hartung method as done above by s2w <- c( t(Y) %*% P %*% Y ) / (n-p)?
      if((lapMod==0) | noMoM) resZ <- P %*% Y / sqrt(diag(P %*% tcrossprod(diag(vTot), P)))
   
      res         <- list(b, se, z, tau2, QE, lamc, resZ, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "lamc", "resZ", "meth", "iter")
   } else {  # no residual statistics
      res         <- list(b, se, z, tau2, QE, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "meth", "iter")
   }
   res
   } else return(NULL) # if(continue)
}

# two groups with heterogeneity assumption
rmaB2 <- function(yi, vi, n1, nT, p, X, resOut, lapMod, 
                  knha=FALSE, con=list(thr=10^-8, maxiter=50, thrZ=1.3)) {

	Y <- as.matrix(yi)
   Y1 <- as.matrix(Y[1:n1,]); Y2 <- as.matrix(Y[(n1+1):nT,])
   v1 <- vi[1:n1]; v2 <- vi[(n1+1):nT]
   n2 <- nT-n1
   X1 <- as.matrix(X[1:n1,-2]); X2 <- as.matrix(X[(n1+1):nT,-2])   
	   
	tr <- function(X) sum(diag(X))

   W01	 <- diag(1/v1)
   W02    <- diag(1/v2)
   tmp01   <- t(X1) %*% W01
   tmp02   <- t(X2) %*% W02
   #P01    <- W01 - t(tmp01) %*% solve(tmp01 %*% X1) %*% tmp01
   #P02    <- W02 - t(tmp02) %*% solve(tmp02 %*% X2) %*% tmp02
   # don't waste time on computing if tmp01 %*% X1 or tmp02 %*% X2 is singular 
   continue <- TRUE
   tryCatch(P01 <- W01 - t(tmp01) %*% solve(tmp01 %*% X1) %*% tmp01, error = function(w) continue <<- FALSE)
   tryCatch(P02 <- W02 - t(tmp02) %*% solve(tmp02 %*% X2) %*% tmp02, error = function(w) continue <<- FALSE)
   
   if(continue) {
   
   QE     <- c(t(Y1) %*% P01 %*% Y1, t(Y2) %*% P02 %*% Y2)

   collect <- function(Y, vb, R, P, n, p, knha, con) {
      # force t(Y) %*% P %*% Y to be 0 in case it's numerically 0 but negative
      if(knha) vb <- max((c( t(Y) %*% P %*% Y ) / (n-p)), 0) * vb
      se <- sqrt(diag(vb))
      b    <- R %*% Y
      z <- ifelse(se>con$thr, b/se, 0)
      out <- list(se, b, z)
      names(out) <- c("se", "b", "z")
      out
   }
   
   MoM <- function(Y, vi, n, p, X, P0, QE, knha, con) {
      #RSS    <- t(Y) %*% P0 %*% Y
      tau2   <- ( QE - (n-p) ) / tr(P0)  # RSS = QE
      tau2 <- max(0, c(tau2))
      W   <- diag(1/(vi + tau2))
      tmp <- t(X) %*% W
      vb  <- solve(tmp %*% X)
      R   <- vb %*% tmp  # projection of Y to space spanned by X
      P   <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out <- list(stat$se, stat$b, stat$z, tau2, P, 0, 0)
      names(out) <- c("se", "b", "z", "tau2", "P", "meth", "iter")
      out
   }                 

   Laplace <- function(Y, vi, n, p, X, knha, con=list(thr=2*10^-5, maxiter=50)) {
   ### looks like you can't get the precision lower than con$thr = 10^-5!!!

   conv		<- 1
	change_b	<- 1000
   change_nu	<- 1000
	iter		<- 0
   
   nu       <- sqrt(max(0, var(Y) - mean(vi)))/sqrt(2)
   if(nu < 10^-10) nu <- sqrt(max(0, var(Y) - mean(vi)/3))
   W		   <- diag(1/(vi + 2*nu^2))  # need to deal with 0s' here?
   tmp  <- t(X) %*% W
   b    <- solve(tmp %*% X) %*% tmp %*% Y
   visr <- sqrt(vi)

	# seems two ways to update beta (b): one through iterative WLS, while the other via its own iterations
   
   if(nu > 10^-10) {
   while (any(change_b > con$thr) & (change_nu > con$thr)) {
		iter     <- iter + 1
      if (iter > con$maxiter) {  # Laplace fails
			conv    <- 0
			break
		}
		b.old    <- b
      nu.old   <- nu    # scalor
      ei <- Y - X %*% b.old    # n X 1 vector
      Ei <- exp(ei/nu)       # n X 1 vector
      temp1 <- visr/nu; temp2 <- ei/visr  # n X 1 vector: dealing 0s for visr?
      Phi1 <- pnorm(-temp1 - temp2)   # n X 1 vector
      Phi2 <- pnorm(temp2 - temp1)    # n X 1 vector
      phi1 <- dnorm(-temp1 - temp2)   # n X 1 vector
      phi2 <- dnorm(temp2 - temp1)    # n X 1 vector
      Gi <- Phi1 * Ei + Phi2 / Ei     # n X 1 vector: dealing 0s for Ei?
      
      dLb <- vector(mode="numeric", length=p)
      H <- matrix(data = 0, nrow = p+1, ncol = p+1, byrow = FALSE, dimnames = NULL)
      g <- matrix(data = 0, nrow = p+1, ncol = 1, byrow = FALSE, dimnames = NULL)
      for(ii in 1:n) { 
         dLb  <- (phi1[ii]*t(X[ii,])*Ei[ii]/visr[ii] - t(X[ii,])*Ei[ii]*Phi1[ii]/nu
                 + (t(X[ii,])*Phi2[ii]/Ei[ii])/nu - (phi2[ii]*t(X[ii,])/visr[ii])/Ei[ii])/Gi[ii]
         dLnu <- -1/nu - vi[ii]/nu^3 + (Ei[ii]*ei[ii]*Phi1[ii]/nu^2 + visr[ii]*Ei[ii]*phi1[ii]/nu^2
                 - (ei[ii]*Phi2[ii]/Ei[ii])/nu^2 + (visr[ii]*phi2[ii]/Ei[ii])/nu^2)/Gi[ii]        
         ttemp <- t(dLb)*dLnu
         #H <- H + rbind(cbind(dLb %*% t(dLb), t(ttemp)), cbind(ttemp, dLnu^2))
         H <- H + rbind(cbind(t(dLb) %*% dLb, ttemp), cbind(t(ttemp), dLnu^2))
         g <- g + rbind(t(dLb), dLnu)
         
      }      
      
      if(any(is.infinite(H)) | any(is.infinite(H)) | any(is.infinite(g)) | any(is.infinite(g)) |
         any(is.nan(H)) | any(is.nan(H)) | any(is.nan(g)) | any(is.nan(g))) { # failed because of infinity issue
         conv    <- 0
         break
		}
      
      suc <- TRUE
      tryCatch(adj <- solve(H) %*% g, error = function(w) suc <<- FALSE)   # (p+1) X 1 vector
      if(!suc) {  # Laplace fails
			conv    <- 0
			break
		}
      
      b  <- b.old + adj[1:p]
      nu <- nu + adj[p+1]
      change_b	   <- abs(b.old - b)
      change_nu	<- abs(nu.old - nu)
	}
   } else conv <- 0

   if(conv==0) {
      out <- list(conv)
      names(out) <- c("conv")
   } else {   # Laplace succeeded
      W	  <- diag(1/(vi + 2*nu^2))
      tmp  <- t(X) %*% W
      vb   <- solve(tmp %*% X)   # variance-covariance matrix
      R    <- vb %*% tmp  # projection of Y to space spanned by X
      P    <- W - t(tmp) %*% R  # projection of Y to space spanned by residuals
      tau2 <- 2*nu^2
      
      stat <- collect(Y, vb, R, P, n, p, knha, con)
      out  <- list(stat$se, stat$b, stat$z, tau2, P, 1, 1, iter)
      names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
   }
   out
   } # end of Laplace
 
   REML <- function(Y, X, v, knha, n, p, con) {
      conv		<- 1
      adj	   <- 1000
      iter		<- 0
      tau2     <- max(0, var(Y) - mean(v))  # tau^2 for group 1
      W		   <- diag(1/(v + tau2))
      tmp      <- t(X) %*% W
      vb       <- solve(tmp %*% X)   # variance-covariance matrix
      R        <- vb %*% tmp  # projection of Y to space spanned by X
      P        <- W - t(tmp) %*% R
      while(abs(adj) > con$thr) {
         iter     <- iter + 1  # iteration counter
         if (iter > con$maxiter) {
            conv    <- 0
            break
         }
         tau2.old <- tau2
         py    <- P%*%Y
         adj	<- solve( tr(P%*%P) ) %*% ( t(py)%*%py - tr(P) )
         while (tau2 + adj < 0) adj <- adj / 2
         tau2		<- tau2 + adj
         W		   <- diag(1/(v + tau2))
         tmp <- t(X) %*% W
         vb  <- solve(tmp %*% X)   # variance-covariance matrix
         R   <- vb %*% tmp  # projection of Y to space spanned by X
         P <- W - t(tmp) %*% R
      }

      if(conv==0) { # REML failed
         out <- list(conv)
         names(out) <- c("conv")
      } else {   # REML succeeded         
         stat <- collect(Y, vb, R, P, n, p, knha, con)
         out <- list(stat$se, stat$b, stat$z, tau2, P, 1, 2, iter)
         names(out) <- c("se", "b", "z", "tau2", "P", "conv", "meth", "iter")
      }
      out
   }  # end of REML

   # try MoM first: if not significant for sure, then not worth trying other costy methods
   # Be careful: p-1 here because each group has p-1 columns in the design matrix
   outMoM1 <- MoM(Y1, v1, n1, p-1, X1, P01, QE[1], FALSE, con) # force knha FASLE so that modification happens later
   outMoM2 <- MoM(Y2, v2, n2, p-1, X2, P02, QE[2], FALSE, con) # force knha FASLE
   
   se   <- c(outMoM1$se, outMoM2$se)
   b    <- c(outMoM1$b, outMoM2$b)
   z    <- c(outMoM1$z, outMoM2$z)
   tau2 <- c(outMoM1$tau2, outMoM2$tau2)
   #tau2 <- outMoM$tau2
   P1    <- outMoM1$P; P2 <- outMoM2$P
   meth <- outMoM1$meth
   iter <- c(outMoM1$iter, outMoM2$iter)   
   
   if(lapMod==1) {
      resZ1 <- P1 %*% Y1 / sqrt(diag(P1 %*% tcrossprod(diag(tau2[1]+v1), P1)))
      resZ2 <- P2 %*% Y2 / sqrt(diag(P2 %*% tcrossprod(diag(tau2[2]+v2), P2)))
      noMoM <- (any(abs(z) > con$thrZ)) | any(abs(resZ1) > con$thrZ) | any(abs(resZ2) > con$thrZ)
      if(noMoM) {
         #browser()
         outLap1 <- Laplace(Y1, v1, n1, p-1, X1, FALSE) # force knha FASLE
         outLap2 <- Laplace(Y2, v2, n2, p-1, X2, FALSE) # force knha FASLE
         if(outLap1$conv == 1 & outLap2$conv == 1) {
            se   <- c(outLap1$se, outLap2$se)
            b    <- c(outLap1$b, outLap2$b)
            z    <- c(outLap1$z, outLap2$z)
            tau2 <- c(outLap1$tau2, outLap2$tau2)
            P1   <- outLap1$P; P2 <- outLap2$P
            meth <- outLap1$meth
            iter <- c(outLap1$iter, outLap2$iter)
        } else lapMod <- 0  # switch to REML first here when Laplace fails
      }
   }

   if(lapMod==0) { # if Laplace is not selected or fails, try REML      
	   noMoM <- (any(abs(z) > con$thrZ))
      if(noMoM) {
         outREML1 <- REML(Y1, X1, v1, FALSE, n1, p-1, con)  # force knha FASLE
         outREML2 <- REML(Y2, X2, v2, FALSE, n2, p-1, con)  # force knha FASLE
         if(outREML1$conv == 1 & outREML2$conv == 1) {
            se   <- c(outREML1$se, outREML2$se)
            b    <- c(outREML1$b, outREML2$b)
            z    <- c(outREML1$z, outREML2$z)
            tau2 <- c(outREML1$tau2, outREML2$tau2)
            P1   <- outREML1$P; P2 <- outREML2$P
            meth <- outREML1$meth
            iter <- c(outREML1$iter, outREML2$iter)
         }
      }
      #browser()
   } # if(lapMod==0)
   
   if(knha) { # scaling done in runRMA
      #browser()
      if (is.nan(pp1 <- sqrt((c( t(Y1) %*% P1 %*% Y1 ) / (n1-p+1)))) | 
          is.nan(pp2 <- sqrt((c( t(Y2) %*% P2 %*% Y2 ) / (n2-p+1)))) ) scl <- NA else {
      #pp1 <- sqrt((c( t(Y1) %*% P1 %*% Y1 ) / (n1-p+1)))
      #scl <- c(pp1,
      #   sqrt((c( t(Y2) %*% P2 %*% Y2 ) / (n2-p+1))))
      #if(!exists(as.character(substitute(pp1)))) browser()
      
      scl <- c(pp1, pp2)
      W <- diag(c(1/(v1 + tau2[1]), 1/(v2 + tau2[2])))
      tmp <- t(X) %*% W
      P <- W - t(tmp) %*% solve(tmp %*% X) %*% tmp
      scl <- c(scl, sqrt((c( t(Y) %*% P %*% Y ) / (nT-p))))
      }
   } else scl <- NA

   if(resOut==1) {  # residual statistics requested
      vTot1 <- tau2[1]+v1  # total variance for group1
      vTot2 <- tau2[2]+v2  # total variance for group1
      # Like ICC, lamda shows percent of variation at group lev
      lamc <- c(ifelse(vTot1>con$thr, v1/vTot1, 0),
         ifelse(vTot2>con$thr, v2/vTot2, 0))
      
      resZ <- c(P1 %*% Y1/sqrt(diag(P1 %*% tcrossprod(diag(vTot1), P1))),
         P2 %*% Y2/sqrt(diag(P2 %*% tcrossprod(diag(vTot2), P2))))
 
      res         <- list(b, se, z, tau2, QE, scl, lamc, resZ, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "scl", "lamc", "resZ", "meth", "iter")
   } else {  # no residual statistics requested
      res         <- list(b, se, z, tau2, QE, scl, meth, iter)
      names(res)  <- c("b", "se", "z", "tau2", "QE", "scl", "meth", "iter")
   }
   res
   } else return(NULL) # if(continue)
}


readMultiFiles <- function(nFiles, dim, type) {
   inFile <- vector('list', nFiles) # list of file names with path attached
	fn <- vector('list', nFiles)     # list of file names
	if (dim==1) inData <-  matrix(data = NA, nrow = dim(read.table(inFile[[ii]], header=FALSE))[1], ncol = nFiles)
   if (dim==2) inData <- vector('list', nFiles)
   for (ii in 1:nFiles) {
	   inFile[[ii]] <- readline(sprintf("No. %i %s file name: ", ii, type))  
		fn[[ii]] <- strsplit(inFile[[ii]], "/")[[1]][length(strsplit(inFile[[ii]], "/")[[1]])]
		print(sprintf("No. %i file just read in: %s", ii, fn[[ii]]))
		if (dim==1) inData[,ii] <- read.table(inFile[[ii]], header=FALSE)
		if (dim==2) inData[[ii]] <- read.table(inFile[[ii]], header=TRUE)
	}
	return(inData)
}

runRMA <- function(  inData, nGrp, n, p, xMat, outData, 
                     mema, lapMod, KHtest, nNonzero, 
                     nCov, nBrick, anaType, resZout, tol) {  
   
   # n[1]: # subjects in group1; n[2]: total # subjects in both groups; n[2]-n[1]: # subjects in group2   
   fullLen <- length(inData); halfLen <- fullLen/2  # remove this line by providing halfLen as function argument later?
   Y <- inData[1: halfLen]; V <- inData[(halfLen +1): fullLen]
   if(sum(abs(Y)>tol) >= nNonzero) {  # run only when there are more than 2 non-zeros in both Y and V
   tag <- TRUE
   if(anaType==4) try(resList <- mema(Y, V, n[1], n[2], p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE) else
      if(length(n)==1) try(resList <- mema(Y, V, n, p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE) else
      try(resList <- mema(Y, V, n[2], p, X=xMat, resZout, lapMod, knha=KHtest), tag <- FALSE)  # for the case of 2 groups with homoskedasticiy
   
   #if(is.null(resList)) tag <- FALSE  # stop here if singularity occurs
    
   if(tag & !is.null(resList)) {
   if(nGrp==1) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      
      #if(abs(outData[2]) > 100) browser()
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+1] <- resList$b[ii+1]
         outData[2*ii+2] <- resList$z[ii+1]   
      } # for(ii in 1:nCov)
   } # if (nGrp==1)
   
   #browser()
   if(anaType==2) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      outData[5] <- resList$b[2]  # beta of group2-group1
      outData[6] <- resList$z[2]  # z score of group2-group1
      outData[3] <- outData[1]+outData[5]  # beta of group2
      tmp <- sqrt(resList$se[2]^2-resList$se[1]^2)
      outData[4] <- ifelse(tmp>tol, outData[3]/tmp, 0) # z-score of group2
      
      if(nCov>0) for(ii in 1:nCov) {  # covariates
         outData[2*ii+5] <- resList$b[ii+2]
         outData[2*ii+6] <- resList$z[ii+2]   
      } # for(ii in 1:nCov)
   } # if (nGrp==2)

   if(anaType==4) {
      outData[1] <- resList$b[1]  # beta of group1, intercept
      outData[2] <- resList$z[1]  # z score of group1
      outData[3] <- resList$b[2]  # beta of group2
      outData[4] <- resList$z[2]  # z-score of group2
      outData[5] <- resList$b[2]-resList$b[1]  # beta of group2-group1
      tmp <- sqrt(resList$se[1]^2+resList$se[2]^2)
      outData[6] <- ifelse(tmp>tol, outData[5]/tmp, 0) #z score of group2-group1
      #browser()
      #if(KHtest) {
      #   outData[2] <- outData[2]/resList$scl[1]
      #   outData[4] <- outData[4]/resList$scl[2]
      #   outData[6] <- outData[6]/resList$scl[3]
      #}
      
      # more concise than above
      #if(KHtest)  outData[c(2,4,6)] <- outData[c(2,4,6)]/resList$scl[c(1,2,3)]
      if(KHtest)  outData[c(2,4,6)] <- outData[c(2,4,6)]/resList$scl
   
      if(resZout==0) {
         outData[nBrick-5] <- resList$tau2[1]
         outData[nBrick-3]   <- resList$tau2[2]
         outData[nBrick-4] <- resList$QE[1]
         outData[nBrick-2]   <- resList$QE[2]
         outData[nBrick-1] <- ifelse(resList$tau2[2] > tol, resList$tau2[1]/resList$tau2[2], 0)
         outData[nBrick]   <- ifelse(resList$tau2[1] > tol, resList$tau2[2]/resList$tau2[1], 0)
      } else {
   
      outData[nBrick-2*n[2]-5] <- resList$tau2[1]
      outData[nBrick-2*n[2]-3]   <- resList$tau2[2]
      outData[nBrick-2*n[2]-4] <- resList$QE[1]
      outData[nBrick-2*n[2]-2]   <- resList$QE[2]
      outData[nBrick-2*n[2]-1] <- ifelse(resList$tau2[2] > tol, resList$tau2[1]/resList$tau2[2], 0)
      outData[nBrick-2*n[2]]   <- ifelse(resList$tau2[1] > tol, resList$tau2[2]/resList$tau2[1], 0)
   
      for(ii in 1:n[1]) {
         outData[nBrick-2*(n[2]-ii)-1] <- resList$lamc[ii]   # lamda = 1-I^2
         outData[nBrick-2*(n[2]-ii)]   <- resList$resZ[ii]    # Z-score for residuals
      } # from nBrick-2*n[2]+1 to nBrick-2*(n[2]-n[1])
   
      for(ii in 1:(n[2]-n[1])) {
         outData[nBrick-2*(n[2]-n[1]-ii)-1] <- resList$lamc[ii+n[1]]   # lamda = 1-I^2
         outData[nBrick-2*(n[2]-n[1]-ii)]   <- resList$resZ[ii+n[1]]    # Z-score for residuals
      } # from nBrick-2*(n[2]-n[1])+1 to nBrick
      } # if(resZout==0)
   } else {  # not anaType==4
   if(resZout==0) {
      outData[nBrick-1] <- resList$tau2
      outData[nBrick]   <- resList$QE
   } else {
      outData[nBrick-2*n-1] <- resList$tau2
      outData[nBrick-2*n]   <- resList$QE
      for(ii in 1:n) {
      outData[nBrick-2*(n-ii)-1] <- resList$lamc[ii]   # lamda = 1-I^2
      outData[nBrick-2*(n-ii)]   <- resList$resZ[ii]    # Z-score for residuals
   }
   }  # if(resZout==0)
   }  # if(anaType==4)
   }  # if(tag)
   }  # not all 0's
   return(outData)
} # end of runRMA


#################################################################################
########################## Begin MEMA main ######################################
#################################################################################


tolL <- 1e-7 # bottom tolerance for avoiding division by 0 and for avioding analyzing data with most 0's
tolU <- 1e8  # upper tolerance for those variances of 0
tTop <- 100   # upper bound for t-statistic

#options(show.error.messages = FALSE)  # suppress error message when running with single processor

   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      save(args, file=".3dMEMA.dbg.AFNI.args", ascii = TRUE) 
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      cat(greeting.MEMA(),
          reference.MEMA(),
      "Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
      sep='\n')
      if (is.null(lop <- read.MEMA.opts.interactive())) {
         stop('Error parsing interactive input');
      }
      if (0) { #Too much output, big dump of header structs of input dsets..
         str(lop);
      }
   } else {
      if (!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }  
      if (is.null(lop <- read.MEMA.opts.batch(args, verb = 0))) {
         stop('Error parsing input');
      }
      #str(lop);
      if (is.null(lop <- process.MEMA.opts(lop, verb = lop$verb))) {
         stop('Error processing input');
      }
   }
   
   if (lop$verb > 1) { 
      #Too much output, big dump of header structs of input dsets..
      str(lop);
   }
   
   #dump xmat
   if (lop$verb) {
      xmout <- paste(prefix.AFNI.name(lop$outFN),".RxMat", sep='')
      cat ('Writing xMat to', xmout, '\n')
      write.table(lop$xMat,xmout);
   }
   
   #Check on DOF problems
   if (!DOF.check.MEMA(lop)) {
      stop('');
   }
   
   exclude <- TRUE  # exclude those voxels with 0 variance
   
   # Maybe I should avoid doing this below part for those voxels in the mask!!!
   if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4) {
      for(ii in 1:lop$nGrp) {
         lop$bList[[ii]] <- lapply(lop$bList[[ii]], function(x) x$brk); 
         lop$tList[[ii]] <- lapply(lop$tList[[ii]], function(x) x$brk)
         lop$varList[[ii]] <- mapply(function(x, y) 
               ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), 
               lop$bList[[ii]], lop$tList[[ii]], SIMPLIFY = FALSE)  # variances
         if(exclude) {
            for (jj in 1:lop$nSubj[ii]) { 
               lop$varList[[ii]][[jj]][lop$varList[[ii]][[jj]] < tolL] <- tolU  
                              # replace those 0 variances with a big number
            }
         }
      }
   }
   
   if(lop$anaType==3) {
      for(ii in 1:lop$nLevel) {
         lop$bList[[ii]] <- lapply(lop$bList[[ii]], function(x) x$brk); 
         lop$tList[[ii]] <- lapply(lop$tList[[ii]], function(x) x$brk)
         lop$varList[[ii]] <- 
            mapply(function(x, y) 
                   ifelse((abs(x)<tolL) | (abs(y)<tolL), 0, (x/y)^2), 
                   lop$bList[[ii]], lop$tList[[ii]], SIMPLIFY = FALSE)  # variance
         if(exclude) {
            for (jj in 1:lop$nSubj[1]) 
               lop$varList[[ii]][[jj]][lop$varList[[ii]][[jj]] < tolL] <- tolU  
                                    # replace those 0 variances with a big number
         }
      }
      # 2nd minus 1st: keep consistent with 3dttset and the two-sample types 2 and 4
      contrBList <- mapply("-", lop$bList[[2]], lop$bList[[1]], SIMPLIFY = FALSE)
      contrVarList <- mapply("+", lop$varList[[1]], lop$varList[[2]], 
                              SIMPLIFY = FALSE)
      
      # if one of the 2 conditions has 0 t-value, force the contrast beta ZERO here
      # even if the other beta is nonzerio since it's not worth considering a contrast for this voxel      
      if(exclude) for (jj in 1:lop$nSubj[1])
         contrBList[[jj]] <- ifelse(contrVarList[[jj]]>=tolU, 0, contrBList[[jj]])                     
   }
   
   #rm(lop$tList)
   lop$tList <- list();
   
   if(!identical(grep("orig", lop$bFN[[1]][1]), integer(0))) 
      dataView <- "orig"  # input files in orig view
   if(!identical(grep("tlrc", lop$bFN[[1]][1]), integer(0))) 
      dataView <- "tlrc"  # input files in tlrc view
   dataOrient <- system(sprintf("@GetAfniOrient %s", lop$bFN[[1]][1]), 
                        intern = TRUE)

   anyCov <- !is.null(lop$covFN);
   
   # each subject has two number: one for lambda, and the other, deviation, 
   # for outlier identificaiton - need to do the same for type 4
   
   nBrick0 <- 4*lop$nGrp+(anyCov)*2*lop$nCov   
                        # no. sub-bricks in the main output
   nBrick <- 4*lop$nGrp+(anyCov)*2*lop$nCov+2*sum(lop$nSubj)*lop$resZout  
                        # total sub-bricks in all output
   if(lop$anaType==4) {
      nBrick0 <- nBrick0+4; nBrick <- nBrick+4
   }  # two more for lop$anaType==4
      
   if(lop$anaType==1 | lop$anaType==2 | lop$anaType==4) {
      comArr <- array(c(unlist(c(lop$bList)), unlist(c(lop$varList))), 
                        dim=c(lop$myDim[1:3], sum(lop$nFiles)))
   }
   if(lop$anaType==3) {
      comArr <- array(c(unlist(c(contrBList)), unlist(c(contrVarList))), 
                        dim=c(lop$myDim[1:3], sum(lop$nFiles)))
   }
   
   lop$bList <- list();
   lop$varList <- list();
   
   # mask out the junk: one slice at a time due to potential memory issue  
   if(!is.null(lop$maskFN)) { 
      #str(lop$maskData)
      #str(comArr)
      for (kk in 1:lop$myDim[3]) {
         comArr[,,kk,] <- 
            array(apply(comArr[,,kk,], 3, 
                        function(x) x*(abs(lop$maskData[,,kk,1])>tolL)), 
                        dim=c(lop$myDim[1:2],sum(lop$nFiles)))
      }
      #rm(lop$maskData)
      lop$maskData <- list();
   }

   outArr <- array(0, dim=c(lop$myDim[1:3], nBrick))
   outData<-vector(mode="numeric", length= nBrick)  
                     # initialization for use in runRMA: 3 beta's + 3 z-scores

   #if (lop$verb) {
      print("-----------------")
      print(sprintf("Totally %i slices in the data.", lop$myDim[3]))
      print("-----------------")
      print("Starting to analyze data slice by slice...")
      print("-----------------")
   #}

   # single processor
   
   if(lop$anaType==4) {
      if(lop$nNodes==1) 
         for (ii in 1:lop$myDim[3]) {
            outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runRMA, 
               nGrp=lop$nGrp, n=c(lop$nSubj[1], sum(lop$nSubj)), 
               p=dim(lop$xMat)[2], xMat=lop$xMat, outData=outData, 
               mema=rmaB2, lapMod=lop$lapMod, KHtest=lop$KHtest, 
               nNonzero=lop$nNonzero, nCov=lop$nCov, nBrick=nBrick, 
               anaType=lop$anaType, resZout=lop$resZout, tol=tolL), 
               c(2,3,1))
            cat(  "Z slice #", ii, "done: ", 
                  format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
   
      # multi-processing
      if(lop$nNodes>1) {
         libLoad('snow')
         cl <- makeCluster(lop$nNodes, type = "SOCK")
         for(ii in 1:lop$myDim[3]) {
            outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), 
                  runRMA, nGrp=lop$nGrp, n=c(lop$nSubj[1], sum(lop$nSubj)), 
                  p=dim(lop$xMat)[2], xMat=lop$xMat, outData=outData, 
                  mema=rmaB2, lapMod=lop$lapMod, KHtest=lop$KHtest, 
                  nNonzero=lop$nNonzero, nCov=lop$nCov, nBrick=nBrick, 
                  anaType=lop$anaType, resZout=lop$resZout, tol=tolL), 
                  c(2,3,1))
            cat("Z slice #", ii, "done: ", 
                  format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      }  # if(lop$nNodes>1)
   } else {
      if(lop$nNodes==1) for (ii in 1:lop$myDim[3]) {
         outArr[,,ii,] <- aperm(apply(comArr[,,ii,], c(1,2), runRMA, 
               nGrp=lop$nGrp, n=sum(lop$nSubj), 
               p=dim(lop$xMat)[2], xMat=lop$xMat, 
               outData=outData, mema=rmaB, lapMod=lop$lapMod, 
               KHtest=lop$KHtest, nNonzero=lop$nNonzero, nCov=lop$nCov, 
               nBrick=nBrick, anaType=lop$anaType, resZout=lop$resZout, 
               tol=tolL), c(2,3,1))
         cat("Z slice #", ii, "done: ", 
               format(Sys.time(), "%D %H:%M:%OS3"), "\n")
      }
   
      # multi-processing
      if(lop$nNodes>1) {
         libLoad('snow')
         cl <- makeCluster(lop$nNodes, type = "SOCK")
         for(ii in 1:lop$myDim[3]) {
            outArr[,,ii,] <- aperm(parApply(cl, comArr[,,ii,], c(1,2), runRMA, 
                  nGrp=lop$nGrp, n=sum(lop$nSubj), p=dim(lop$xMat)[2], 
                  xMat=lop$xMat, 
                  outData=outData, mema=rmaB, lapMod=lop$lapMod, 
                  KHtest=lop$KHtest, nNonzero=lop$nNonzero, nCov=lop$nCov, 
                  nBrick=nBrick, anaType=lop$anaType, resZout=lop$resZout, 
                  tol=tolL), c(2,3,1))
            cat("Z slice #", ii, "done: ", 
                  format(Sys.time(), "%D %H:%M:%OS3"), "\n")
         }
         stopCluster(cl)
      }  # if(lop$nNodes>1)
   } # if(lop$anaType==4)

   print(sprintf("Analysis finished: %s", format(Sys.time(), "%D %H:%M:%OS3")))
   
   print("#++++++++++++++++++++++++++++++++++++++++++++")

   for(ii in 1:lop$nGrp) {
      if(ii==1) outLabel <- paste(sprintf("%s:b", lop$testName[[ii]])) else
         outLabel <- append(outLabel, sprintf("%s:b", lop$testName[[ii]]))
      outLabel <- append(outLabel, sprintf("%s:t", lop$testName[[ii]]))    
      #if(lop$KHtest) {
      #   outLabel <- append(outLabel, sprintf("%s:t", lop$testName[[ii]])) 
      #} else {
      #  outLabel <- append(outLabel, sprintf("%s:Z", lop$testName[[ii]]))
      #}
   }
   if (lop$nGrp==2) {
      outLabel <- append(outLabel, sprintf("%s-%s:b", 
                         lop$testName[[2]],lop$testName[[1]]))
      outLabel <- append(outLabel, sprintf("%s-%s:t", lop$testName[[2]],
                         lop$testName[[1]]))
      #if(lop$KHtest) {
      #   outLabel <- append(outLabel, sprintf("%s-%s:t", 
      #                      lop$testName[[2]],lop$testName[[1]])) 
      #} else {
      #   outLabel <- append(outLabel, sprintf("%s-%s:Z", 
      #                      lop$testName[[2]],lop$testName[[1]]))
      #}
   } # if (lop$nGrp==2)
   
   if(anyCov) for(ii in 1:lop$nCov) {
      outLabel <- append(outLabel, sprintf("%s:b", lop$covName[ii]))
      outLabel <- append(outLabel, sprintf("%s:t", lop$covName[ii]))
      #if(lop$KHtest) {
      #   outLabel <- append(outLabel, sprintf("%s:t", lop$covName[ii])) 
      #} else {
      #   outLabel <- append(outLabel, sprintf("%s:Z", lop$covName[ii]))
      #}
   } 
   
   if(lop$anaType==4) {
      for(ii in 1:lop$nGrp) {
         outLabel <- append(outLabel, sprintf("%s:tau^2", lop$testName[[ii]]))
         outLabel <- append(outLabel, sprintf("%s:QE", lop$testName[[ii]]))
      }
      outLabel <- append(outLabel, "tau1^2>tau2^2")
      outLabel <- append(outLabel, "tau2^2>tau1^2")
   } else {
      outLabel <- append(outLabel, "tau^2")
      outLabel <- append(outLabel, "QE:Chisq")  
   }
   
   if(lop$resZout==1) for(ii in 1:lop$nGrp) for(jj in 1:lop$nSubj[ii]) {
      if(ii==1 & jj==1) {
         iccLabel  <- paste(sprintf("%s-lambda", lop$subjLab[[ii]][[jj]])) 
         resZLabel <- paste(sprintf("%s-Res:Z", lop$subjLab[[ii]][[jj]]))
      } else {
         iccLabel  <- append(iccLabel, sprintf("%s-lambda", 
                              lop$subjLab[[ii]][[jj]]))
         resZLabel <- append(resZLabel, sprintf("%s-Res:Z", 
                              lop$subjLab[[ii]][[jj]]))
      }  
   }
   
   nDF <- sum(lop$nFiles)/2-lop$nGrp-lop$nCov
   
   #statpar <- "3drefit"
   #if (lop$KHtest) {
   #   for (ii in 1:(2*lop$nGrp-1)) 
   #      statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fitt ", nDF) 
   #} else {
   #   for (ii in 1:(2*lop$nGrp-1)) 
   #      statpar <- paste(statpar, " -substatpar ", 2*(ii-1)+1, " fizt")
   #}
   
   statpar <- "3drefit"
   for (ii in 1:(2*lop$nGrp-1)) statpar <- paste(statpar, " -substatpar ",
                                                 2*(ii-1)+1, " fitt ", nDF)
   if(anyCov) for(ii in 1:lop$nCov) statpar <- paste( statpar, " -substatpar ",
       nBrick0-3-2*(lop$nCov-ii), " fitt ", nDF)
   
   #if(anyCov) {
   #   if (lop$KHtest) {
   #      for(ii in 1:lop$nCov) 
   #         statpar <- paste( statpar, " -substatpar ", 
   #                           nBrick0-3-2*(lop$nCov-ii), " fitt ", nDF)
   #   } else {
   #      for(ii in 1:lop$nCov) 
   #         statpar <- paste(statpar, " -substatpar ", 
   #                          nBrick0-3-2*(lop$nCov-ii), " fizt")
   #   }
   #}
   if(lop$anaType==4) {
      statpar <- paste(statpar, " -substatpar ", nBrick0-5, " fict ", 
                       lop$nSubj[1]-lop$nCov) # Chi-sq for QE: group 1
      statpar <- paste(statpar, " -substatpar ", nBrick0-3, " fict ", 
                       lop$nSubj[2]-lop$nCov) # Chi-sq for QE: group 2
   } else {
      statpar <- paste(statpar, " -substatpar ", nBrick0-1, " fict ", nDF)
   }
   
   # last brick: QE with chi-sq
 
   # Z-score for residuals
   if(lop$resZout==1) {
      statparICC <- "3drefit"; 
      statparResZ <- "3drefit"
      for(ii in 1:sum(lop$nSubj)) 
         statparResZ <- paste(statparResZ, " -substatpar ", ii-1, " fizt")
   }

   #rm(comArr)
   outArr[outArr > tTop] <- tTop  # Avoid outflow!!!!
   outArr[outArr < (-tTop)] <- -tTop  # Avoid outflow!!!!
   if (lop$verb) cat ('outLabel', outLabel,'\n');
   write.AFNI(lop$outFN, outArr[,,,1:nBrick0], 
                  outLabel, note=lop$myNote, origin=lop$myOrig, 
                  delta=lop$myDelta, idcode="whatever")
   if (dataView=="tlrc") statpar <- paste(statpar, " -view ", dataView)     
   statpar <- paste( statpar, " -addFDR -newid -orient ", 
                     dataOrient, " ", lop$outFN)                   
   if (lop$verb) cat ('statpar', statpar, '\n')
   system(statpar)

   if(lop$resZout==1) {
      write.AFNI(lop$icc_FN, outArr[,,,seq((nBrick0+1), nBrick, by=2)], iccLabel,
                 note=lop$myNote, origin=lop$myOrig, delta=lop$myDelta, 
                 idcode="whatever")
      write.AFNI(lop$resZ_FN, outArr[,,,seq((nBrick0+2), nBrick, by=2)], 
                 resZLabel, note=lop$myNote, origin=lop$myOrig, 
                 delta=lop$myDelta, idcode="whatever")
      if (dataView=="tlrc") {
         statparICC  <- paste(statparICC, " -view ", dataView); 
         statparResZ <- paste(statparResZ, " -view ", dataView) 
      } 
      statparICC  <- paste(statparICC, " -newid -orient ", 
                           dataOrient, " ", lop$icc_FN)
      statparResZ <- paste(statparResZ, " -addFDR -newid  -orient ", 
                           dataOrient, " ", lop$resZ_FN)
      system(statparICC)
      system(statparResZ)
   }

      
   geterrmessage()




#An R API for the setup of group analysis 
#
# 

require('XML');

################################################################################
# Function to read two xml files used to perform a group analysis,
#  subjects contains the subjects' information
#  test contains the instructions for the analysis
################################################################################
read.level2 <- function(subjects, test, verb=0) {

#First form the dataframe containing information about subjects   
   nel <- xmlInternalTreeParse(subjects);
      
   #Mode 1
   #create an XMLNodeSet of subjects nodes (named <s>)
   sbs = xpathApply(nel, "//s");
   if (verb) cat ('Have ', length(sbs), 'subject nodes\n');
   tbl = sbs2subjframe(sbs);
   fr.show(tbl);
   
   #Mode 2
   #create an XMLNodeSet of table form (named <table>)
   tbs = xpathApply(nel, "//table");
   if (verb) cat ('Have ', length(tbs), 'table nodes\n');
   if (length(tbs)>0) {
      tbl2 = table2subjframe(tbs);
      fr.show(tbl2);
   }

#Now see what kind of test is wanted   
   nel <- xmlInternalTreeParse(test);
   tst = xpathApply(nel, "//test");
   if (length(tst) != 1) {
      warning(paste("Have no test element"),
              immediate. = TRUE);
      return(NULL);
   }
   
#depending on the analysis method, perform some checks
   #What method?
   meth <- xpathApply(tst[[1]], "method");
   if (match(str.deblank(xmlValue(meth[[1]])),"mema")==1) {
      if (is.null(lvars <- setup.test.mema(tst,tbl))) {
         warning(paste("Failed setting up for test"),
              immediate. = TRUE);
         return(NULL);
      }
      cat('List of variables for use by 3dMEMA.R:\n');
      str(lvars);
   } else {
      warning(paste("Don't know about method", xmlValue(meth)),
              immediate. = TRUE);
      return(NULL);
   }
   
   #return with variables list  
   return(lvars);
}

################################################################################
# Function to turn a set of <s> nodes into a subjects' frame
################################################################################
sbs2subjframe <- function(sbs, verb=0) {
   #first we need a full list of all the elements (attributes) in all nodes
   allcols <- vector(length=0, mode='character');
   for (ss in sbs) {
      #Append all the element names for each subject
      allcols <- c(allcols, tolower(attr(xmlSApply(ss,xmlName), "names")))
   } 
   allcols <- unique(allcols);
   if (verb) cat('Full set of', length(allcols) ,'columns is:\n', allcols, '\n');
   
   
   #initialize data frame
   fr <- data.frame(matrix(data=NA, nrow=length(sbs), ncol=length(allcols)));
   colnames(fr) <- allcols;
   
   #Fill data frame
   for (irow in 1:length(sbs)) {
      ss = sbs[[irow]];
      for (icol in 1:length(allcols)) {
         nn = allcols[icol];
         #get the value of nn 
         nnel <- xpathApply(ss,nn);
         if (length(nnel) == 1) {
            val <- str.deblank(xmlValue(nnel[[1]])); 
            if ( !is.na(match(nn,"name"))) {  
               if (!is.na(match(val,fr[,icol])))  {
               warning(paste("Name", val, 'already in list.'),
                        immediate. = TRUE);
               return(NULL);
               }
            }
            fr[irow,icol] <- val;
         }else {
            # if 0 then OK, but otherwise should complain
         } 
      }
   }
   
   #Setup some default attributes
   fr = fr.attr.set(fr);
   return (fr);
}

################################################################################
# Function to turn a <table> node into a subjects' frame
################################################################################
table2subjframe <- function(tbs, verb = 0) {
   if (length(tbs) != 1) {
      warning(paste("More than 1 table not allowed. Have", length(tbs),'\n'),
              immediate. = TRUE);
      return(NULL);
   }
   #Get the table attributes 
   attrs = xmlAttrs(tbs[[1]]);
   #find column titles
   icl = grep("col_titles", attr(attrs,"names"));
   allcols <- vector(length=0, mode='character');
   if (length(icl)) {
      allcols <- tolower(strsplit(attrs[icl], ' ')[[1]]);
   }else {
      warning(paste("No col_titles attribute in", attr(attrs,"names"), "\n"),
              immediate. = TRUE);
      return(NULL);
   }
   if (verb) cat('Full set of', length(allcols) ,'columns is:\n', allcols, '\n');
   
   #initialize data frame
   fr <- data.frame(matrix(data=NA, nrow=0, ncol=length(allcols)));
   colnames(fr) <- allcols;
   
   #fill data frame
   tv = strsplit(xmlValue(tbs[[1]]), '\n')[[1]];
   iblankrow<-0;
   irow <- 0;
   for (ss in tv) {
      iblankrow <- iblankrow+1;
      ss = sub('[[:space:]]+$','',ss);
      ss = sub('^[[:space:]]+','',ss);
      ss = strsplit(ss, '[[:space:]]+')[[1]];
      if (length(ss) > 0) {
         #Foreach non empty row, enter values into table
         if (length(ss) != length(allcols)) {
            warning(paste("Row", iblankrow-1, "has", length(ss), 
                          "columns. But we need", length(allcols), "\n"),
              immediate. = TRUE);
            return(NULL);
         }
         irow <- irow + 1;
         for (icol in 1:length(ss)) {
            if (length(grep('NA', ss[icol]))==0) {
               #only copy non empty entries
               fr[irow,icol] <- ss[icol];
            }
         }
      }
   }
   #create some default attributes
   fr = fr.attr.set(fr);
   
   return(fr);
}


################################################################################
#          Functions that setup variables for 3dMEMA.R's use
################################################################################

# Fetches dataset names from subjects' frame in a way convenient to 3dMEMA.R
get.dsets.mema <- function( fr, dsetcolname, 
                           grp = NA, cond = NA, condqual="_Coef") {
   if (length(grp)==1 && is.na(grp)) {
      igrp <- 1:1:length(fr$group);
   } else {
      igrp = vector('numeric', 0);
      for (ig in 1:1:length(grp)) {
         igrp <- c(igrp, grep(grp[ig],fr$group));
      } 
   }

   s <- fr.col(fr, dsetcolname)[igrp];
   
   #Now add the conditions
   if (is.na(cond)) {
      cond <- fr$conditions;
   }
   sc <- vector('character', 0);
   for (ic in 1:1:length(cond)) {
      sel <- paste('[',cond[ic],condqual,']',sep='',collapse='');
      sc <- c(sc, paste(s,sel,sep=''));
   }
   
   #Check on results
   for (ic in 1:1:length(sc)) {
      com <- paste ('3dAttribute BRICK_LABS', sc[ic], '>& /dev/null',
                     collapse ='');
      if (try(system(com)) != 0) {
         warning(paste("Failed to execute:\n   ", com, "\ndataset missing ?"),
                        immediate. = TRUE);
         #return(NULL);
      }
   }
   return(sc)
}

#This function takes the 'test' XMLInternalDocument object
#   and sets up the list of variables for 3dMEMA.R   
setup.test.mema <- function (tst,fr) {
   #The prefix
   lmema <- list(prefix=get.child.val(tst, "prefix", "memaout"));
   #The groups
   lmema <- c( lmema, 
               list("groups" = (get.child.val(tst,"groups"))));
   if (length(lmema$groups) > 2 || length(lmema$groups) < 1) {
      warning(paste("One or two groups allowed"),
              immediate. = TRUE);
         return(NULL);
   }
   
   #The conditions
   lmema <- c( lmema, 
               list("conditions" = c(get.child.val(tst,"conditions"))));
   if (length(lmema$conditions) > 2 || length(lmema$conditions) < 1) {
      warning(paste("One or two conditions allowed"),
              immediate. = TRUE);
         return(NULL);
   }
   
   #Variance modeling
   lmema <- c( lmema, 
               list("variance" = c(get.child.val(tst,"variance","homo"))));
   if (is.na(match(lmema$variance,"hetero")) &&
       is.na(match(lmema$variance,"homo")) ) {
      warning(paste("Only 'homo' or 'hetero' allowed for variance"),
              immediate. = TRUE);
         return(NULL);
   }
   
   #The jobs
   lmema <- c( lmema, 
               list("jobs" = c(get.child.nval(tst,"N_jobs",1))));
   if (is.na(lmema$jobs)) {
      warning(paste("Bad value for N_jobs"),
              immediate. = TRUE);
         return(NULL);
   }
   
   #The test label
   lmema <- c( lmema, 
               list("test_label" = c(get.child.val(tst,"test_label",
                                                   "mematest"))));
   
   if (is.na(match(lmema$variance,"hetero")) &&
       is.na(match(lmema$variance,"homo")) ) {
      warning(paste("Only 'homo' or 'hetero' allowed for variance"),
              immediate. = TRUE);
         return(NULL);
   }
   
   #How many subjects in each group ?
   lmema$N_subjects = as.vector(mode='numeric', length(lmema$groups));
   for (isb in 1:length(lmema$groups)) {
      lmema$N_subjects[isb] = fr.sbj.num(fr,thisgrp=lmema$groups[isb]);
      if (lmema$N_subjects[isb] < 1) {
         warning(paste("No subjects for group",lmema$groups[isb],'\n',
                       "Available groups:", paste(fr.grp.lst(fr), collapse=' ')),
              immediate. = TRUE);
         return(NULL);
      }
   }
   
   #names of subjects in each group 
   lmema$subject_names = vector('list', length(lmema$groups));
   for (igr in 1:1:length(lmema$groups)) {
     lmema$subject_names[[igr]] = fr.sbj.lst(fr, thisgrp=lmema$groups[igr]); 
   }
   
   #Get the covariates 
   cc <- get.child.val(tst,"covariates")
   lmema$covariates = vector('list', length(cc));
   for ( ic in 1:length(cc)) {
      lmema$covariates[[ic]] <- fr.col(fr, cc[ic], numeric=TRUE)
   }
   
   #Model decision
   if (length(lmema$groups) == 2 &&
       length(lmema$conditions) == 1 &&
       !is.na(match(lmema$variance,"homo")) ){
      lmema <- c( lmema, list("model_type" = 2) );
   } else if (length(lmema$groups) == 2 &&
       length(lmema$conditions) == 1 &&
       !is.na(match(lmema$variance,"hetero")) ) {
      lmema <- c( lmema, list("model_type" = 4) );
   } else if (length(lmema$groups) == 1 &&
       length(lmema$conditions) == 1 ) {
       lmema <- c( lmema, list("model_type" = 1) );   
   } else if (length(lmema$groups) == 1 &&
       length(lmema$conditions) == 2 ) {
       lmema <- c( lmema, list("model_type" = 3) );   
   } else {
      warning(paste("Bad groups/conditions/variance combo"),
              immediate. = TRUE);
         return(NULL);
   }  
   
   #Get the datasets for each group
   lmema$dsetsb = get.dsets.mema (fr,"reg_dset", 
                                lmema$groups, lmema$conditions,"_Coef");
   lmema$dsetst = get.dsets.mema (fr,"reg_dset", 
                                lmema$groups, lmema$conditions,"_Tstat");                  

   return(lmema)
}

################################################################################
#  Functions to act on subjects' frame
################################################################################
#Get the list of group names
fr.grp.lst <- function (fr,reuse=TRUE) {
   #get the list of groups
   ugrp = NULL;
   if (reuse) {
      ugrp <- attr(fr,"group_list"); #already stored?
   }
   if (is.null(ugrp)) {
      ugrp<-unique(fr$group);
      attr(fr,"group_list") <- ugrp;
      assign('fr', fr, envir=parent.frame()); #Oh those high level languages
   }
   return(ugrp);
}

#Get the list of subject names
fr.sbj.names <- function (fr,reuse=TRUE) {
   #get the list of groups
   ugrp = NULL;
   if (reuse) {
      ugrp = attr(fr,"group_list"); #already stored?
   }
   if (i.null(ugrp)) {
      ugrp=unique(fr$group);
      attr(fr,"group_list") <- ugrp;
      assign('fr', fr, envir=parent.frame()); #Oh those high level languages
   }
    
   return(ugrp);
}

#Get the number of groups
fr.grp.num <- function (fr,reuse=TRUE) {
   n=NULL;
   if (reuse) {
      n = attr(fr,"N_groups");
   }
   if (is.null(n)) {
      n = length(fr.grp.lst(fr));
      attr(fr,"N_groups") <- n;
      assign('fr', fr, envir=parent.frame()); #Oh those high level languages
   }
   return(n);
}

#Return the number of subjects in each group
#  if 'thisgrp' is set, return the number of subjects
#  in 'thisgrp' only.
fr.sbj.num <- function (fr,reuse=TRUE,thisgrp=NA) {
   #Number of subjects in each group
   nsubj = NULL;
   if (reuse) { # Allow use of pre computed result
      nsubj = attr(fr,"N_subjects"); 
   } 
   if (is.null(nsubj)) {
      ugrp <- fr.grp.lst(fr);
      nsubj <- vector(length(ugrp), mode='numeric');
      for (isubj in 1:length(ugrp)) {
         nsubj[isubj] = length(grep(ugrp[isubj],fr$group));
      }
      attr(fr,"N_subjects") <- nsubj;
      assign('fr', fr, envir=parent.frame()); #Oh those high level languages
   }
   
   #number for a particular grp?
   if (!is.na(thisgrp)) {
      igrp = grep(thisgrp, fr.grp.lst(fr));
      if (length(igrp)) {
         nsubj <- nsubj[igrp];
      } else {
         nsubj <- 0;
      }
   }
    
   return(nsubj);
}

#Get a named column from subject's frame
fr.col <- function (fr, colname, reuse=TRUE, thisgrp=NA, numeric=FALSE) {
   icol = grep(colname,colnames(fr));
   if (icol < 1) {
      warning(paste("Have no such column"),
              immediate. = TRUE);
      return(NULL);
   }
   if (is.na(thisgrp)) {
      #all subjects
      v <- fr[,icol];
   } else {
      imatch = grep(thisgrp,fr$group);
      v <- fr[imatch, icol];
   }
   
   if (numeric) {
      v <- as.numeric(v);
   }  
   
   return(v); 
}

#Get the list of subjects in the frame
# use 'thisgrp' to select a subset
# See fr.col() for more generic form
fr.sbj.lst <- function (fr,reuse=TRUE,thisgrp=NA) {
   if (is.na(thisgrp)) {
      #all subjects
      return(fr$name);
   } else {
      imatch = grep(thisgrp,fr$group);
      return(fr$name[imatch]);
   }   
}

#Set attributes that are added to the subjects' frame
#  for convenience.
fr.attr.set <- function (fr){
   
   #do we have a group column?
   if (length(fr$group) == 0) { #Have no group column, assume 1 group 
    fr$group <- sample('NA', dim(fr)[1], replace=TRUE);
   } else {
      #replace NA group with string NA
      fr$group[is.na(fr$group)]='NA'
   }
   
   #do we have a subject name column?
   if (length(fr$name) == 0) { #Have no name column, naming 
    fr$name <- paste('sbj', 1:1:dim(fr)[1], sep='');
   }
   
   fr.grp.lst(fr);  
   fr.sbj.num(fr);
   fr.grp.num(fr);

   return(fr);
}

#Show the subjects' frame
fr.show <- function (fr, showall=FALSE, 
                        attrlist=c('group_list', 'N_groups', 'N_subjects') ) {
   if (showall) {
      print(attributes(fr));  #Show all attributes
   } else {
      for (nn in attrlist) {
         cat ('  ', nn, '= "', attr(fr,nn),'"\n');
      }
   }
   print.data.frame(fr);
}

################################################################################
#  Functions to return the values of an XML node named 'name' in an 
#  XMLInternalDocument object 'tst'
################################################################################
get.child.val <- function (tst, name, default=NA, split=TRUE, numeric = FALSE) {
   node = xpathApply(tst[[1]], name);
   if (length(node)) {
      s = str.deblank(xmlValue(node[[1]]), middle=TRUE);
      if (split) {
         s = strsplit(s,' ')[[1]];
      }
   } else {
      s = default;
   }
   if (numeric) {
      return(as.numeric(s));   
   } else {
      return(s);  
   }
}

#Return numeric value of node 'name'
get.child.nval <- function (tst, name, default=NA, split=TRUE) {
   return(get.child.val(tst,name, default,split,numeric=TRUE));
}

################################################################################
######################generic stuff, move out ##################################
################################################################################

################################################################################
# Generic deblanking function. Move out of here someday
################################################################################
str.deblank <- function(s, start=TRUE, end=TRUE, middle=FALSE) {
   if (end) {
      s = sub('[[:space:]]+$','',s);
   }
   if (start) {
      s = sub('^[[:space:]]+','',s);
   }
   if (middle) {
      s = sub('[[:space:]]+',' ',s); 
   }
   return(s);
}



#####################################
## 09/2017 Justin Rajendra
## Cluster Explorer
## global

### todo ###

library(shiny)
library(data.table)
library(shinydashboard)
library(plotly)
library(psych)
library(RColorBrewer)
library(afex)

#################
## change this stuff from the template
prefix <- 'prefix_replace'


#################
## load the stat functions and afni drivers
source('stat_functions.R')
source('afni_funk.R')
source('lib_Rafni.R')

## find out where afni is
afni.path <- dirname(system('which afni',intern=TRUE))

####################################################
## subjects and setup

## read in stat info
SI.df <- read.csv(paste0('data/',prefix,'_StatInfo.csv'))

## selected p value
p_val <- as.character(SI.df$p_val)

## table of extracted regions
ClustFile <- paste0(prefix,'_p_uncor_',p_val,'_clusters.csv')
clust_all.df <- read.csv(paste0('data/',ClustFile))

## group data from the history file
GroupFile <-paste0(prefix,'_GroupTable.csv')
group.df  <- read.csv(paste0('data/',GroupFile))

## mean data extracted per subject
MeanFile <- paste0(prefix,'_p_uncor_',p_val,'_mean.csv')
mean.df  <- read.csv(paste0('data/',MeanFile),check.names=FALSE)

## reshape to long and filter by subjects included from analysis
mean.df <- melt(mean.df,variable.name='coord',id.vars=c('Subj','InputFile'))
mean.df <- subset(mean.df,mean.df$InputFile %in% levels(group.df$InputFile))

## merge with the group data
mean.df <- merge(mean.df,group.df,by=c('InputFile','Subj'))

## peak data extracted per subject
PeakFile <- paste0(prefix,'_p_uncor_',p_val,'_peak.csv')
peak.df  <- read.csv(paste0('data/',PeakFile),check.names=FALSE)

## reshape to long and filter by subjects included from analysis
peak.df <- melt(peak.df,variable.name='coord',id.vars=c('Subj','InputFile'))
peak.df <- subset(peak.df,peak.df$InputFile %in% levels(group.df$InputFile))

## merge with the group data
peak.df <- merge(peak.df,group.df,by=c('InputFile','Subj'))

##### add check for subjects in group match #####

## images
underImage <- paste0('data/',prefix,'_master.nii.gz')
threshImage <- paste0('data/',prefix,'_p_uncor_',p_val,'.nii.gz')
maskImage <- paste0('data/',prefix,'_p_uncor_',p_val,'_mask.nii.gz')
peakImage <- paste0('data/',prefix,'_p_uncor_',p_val,'_peak_mask.nii.gz')
atlas.name <- SI.df$atlas

####################################################
## cluster lists

## cluster location, center of mass
clust.cm.lab <- ifelse(is.na(clust_all.df$label_cm),
                       as.character(clust_all.df$x_y_z_cm),
                       as.character(clust_all.df$label_cm))

clust.cm.list <- as.list(as.character(clust_all.df$x_y_z_cm))
names(clust.cm.list) <- clust.cm.lab

## cluster location, peak
clust.pk.lab <- ifelse(is.na(clust_all.df$label_peak),
                       as.character(clust_all.df$x_y_z_peak),
                       as.character(clust_all.df$label_peak))

clust.pk.list <- as.list(as.character(clust_all.df$x_y_z_peak))
names(clust.pk.list) <- clust.pk.lab

####################################################
## need these later
wsVars <- NA
qVars <- NA
mvm.vars <- NA
no.interaction <- TRUE

####################################################
## mvm info
if(SI.df$test == "3dMVM"){

  ## get the full model and split out the variables
  mvm.model <- gsub("'", '', SI.df$model)
  mvm.vars <- unlist(tstrsplit(mvm.model, '[*+/-]'))

  ## get categorical variables (first and last are Subj and InputFile)
  catVars <- c(names(group.df)[sapply(group.df,is.factor)])
  if(length(catVars) > 2){
    catVars <- catVars[3:length(catVars)-1]
    if(length(catVars) >= 2){ no.interaction <- FALSE }
  } else {
    catVars <- "None"
  }  ## end catVars

  ## quantitative variables
  qVars <- c(names(group.df)[sapply(group.df,is.numeric)])
  if(length(qVars) == 0){ qVars <- NA }

  ######################
  ## verify within subject factors
  if(!("None" %in% catVars)){

    wsVars.model <- NA

    ## custom function, takes data frame and position of the 'Subj' column
    wsVars <- wsVarCatFinder(group.df,1)
    if(!anyNA(wsVars)){

      ## get rid of "InputFile"
      wsVars <- wsVars[1:length(wsVars)-1]

      ## remove the wsVars from catVars
      catVars <- catVars[which(!(catVars%in%wsVars))]
      if(length(catVars) < 1){ catVars <- "None" }

      ## get the first one from the 3dMVM code to the top
      if(!is.na(SI.df$wsVars)){
        ## break down wsVars model and split
        wsVars.model <- gsub("'", '', SI.df$wsVars)
        wsVars.spec <- unlist(tstrsplit(wsVars.model, '[*+/-]'))
        wsVars <- c(wsVars.spec[1],wsVars[wsVars != wsVars.spec[1]])
      }
    }
  }   ## end wsVars

} else if(SI.df$test == "Ttest"){

  ## match the order
  if(SI.df$model == "AB"){
    lab.a <- as.character(SI.df$setA)
    lab.b <- as.character(SI.df$setB)
  } else if(SI.df$model == "BA"){
    lab.a <- as.character(SI.df$setB)
    lab.b <- as.character(SI.df$setA)
  }

  ## reorder the factor levels
  mean.df$Group <- factor(mean.df$Group,levels=c(lab.a,lab.b),ordered=TRUE)
  peak.df$Group <- factor(peak.df$Group,levels=c(lab.a,lab.b),ordered=TRUE)

  catVars <- paste0(lab.a,'-',lab.b)
  names(catVars) <- catVars

}   ## end variable names

## launch afni
afni_launch(underImage,threshImage,maskImage,peakImage)

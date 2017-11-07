#####################################
## 11/2017 Justin Rajendra
## 3dMVM validator
## other functions

library(psych)
library(RColorBrewer)
library(afex)
library(phia)

## todo

## add line for output of the script as text
## Takes a character string and a data frame or matrix with ONE column
## Returns the data frame with the new line at the bottom
add.line <- function(scr.in,str.in){
  return(paste(scr.in,paste0(str.in,' \\'),sep="\n"))
}   ## end add line

## white space check. Takes a string and checks for ANY white spaces
## useful for verifying prefixes etc
white_space <- function(char.in){
  if(length(grep("[[:blank:]]",char.in)) > 0){
    return(TRUE)
  } else { return(FALSE) }
}

## check for a necessary characters in a list of strings
chars_needed <- function(list.in,needed.str){
  print(list.in)
  foundStr <- lapply(list.in, function(x) grep(needed.str,as.character(x)))
  allStr <- sapply(foundStr, function(x) length(x) > 0)
  if(all(allStr)){ return(TRUE) } else { return(FALSE) }
}

## check if any in a list of strings ends in any of a vector of single chars
ends_with <- function(list.in,end.strs){
  lastChars <- unlist(lapply(list.in, function(x) {
    substr(as.character(x),nchar(x),nchar(x))
  }))
  print(lastChars)
  if(any(lastChars %in% end.strs)){ return(TRUE) } else { return(FALSE) }
}

################################################
## make voxel file name and coordinate string
vox.file.name <- function(vox.roi,out.dir,coord.mat,srad){

  ## base name
  coord.str <- paste(coord.mat[3,1],coord.mat[3,2],coord.mat[3,3],sep='_')

  ## add the seed radius for an ROI
  if(vox.roi == "ROI"){ coord.str <- paste0(coord.str,'_',srad) }

  ## make and return a list
  vox.file <- paste0(out.dir,"/",coord.str,'_temp_vox.1D')
  vox.coord <- paste('ijk',coord.str,sep='_')
  return(list(vox.file=vox.file,vox.coord=vox.coord))
}   ## end vox.file.name

################################################
## return model string
## takes left hand side, main model (bsVars), and within subject model
## assumes 'Subj' as the Error term.
## if no wsVars, pass an NA
ModelMaker <- function(lhs.in,bsVars.in,wsVars.in){
  if(!is.na(wsVars.in)){
    model.out <- paste0(lhs.in," ~ ",bsVars.in," + Error(Subj/",
                        wsVars.in,")")
  } else {
    model.out <- paste0(lhs.in," ~ ",bsVars.in," + Error(Subj)")
  }
  return(model.out)
}   ## end model maker

################################################
## return vector of categorical within subject variable names
## takes a data frame
wsVarCatFinder <- function(data.in,subj.col){

  ## check inputs
  if(!is.data.frame(data.in)){
    print("Input is not a data frame")
    return(NA)
  }
  if(!is.numeric(subj.col)){
    print(paste(subj.col,"is not numeric"))
    return(NA)
  }

  ## see how many per subject and give error is all not the same
  subj.freq <- data.frame(table(data.in[[subj.col]]))$Freq
  if(!all(subj.freq[1] == subj.freq)) {
    print("Missing subject levels")
    return(NA)
  }

  ## get the categorical
  catVars <- c(names(data.in)[sapply(data.in,is.factor)])
  if(length(catVars) == 0){
    print("No categorical variables")
    return(NA)
  }

  ## number of observations per subject
  subj.df <- subset(data.in,
                    data.in[[subj.col]] == data.in[1,subj.col],select=catVars)
  subj.reps <- nrow(subj.df)

  ## there must be some wsVars
  if(subj.reps > 1){
    wsVars <- c()
    ## get rid of empty factor levels (and the Subj and InputFile)
    subj.df <- as.data.frame(lapply(subj.df,function(x)
      if(is.factor(x)) factor(x) else x))

    ## see if it is not the same for each row, and add to wsVars
    for(i in 1:length(subj.df)){
      if(length(levels(subj.df[[i]])) > 1){
        wsVars <- c(wsVars,names(subj.df[i]))
      }
    }
    return(wsVars)

  } else {
    print("No categorical within subject variables")
    return(NA)
  }
}   ## end wsVarCatFinder

################################################
## check the GLT inputs
## takes the current label, currently selected glt variable levels,
## the weights and a table of the previous glts
## returns a vector of error strings or empty for all good
gltCheckFun <- function(glt_lab,glt_lvl,glt_wt,glt_list){

  ## start empty
  bad.out <- c()

  ## check the label
  if(glt_lab == ""){
    bad.out <- rbind(bad.out,"Need a label!")
  } else {
    if(!grepl('^[A-Za-z0-9|+|_|.|-]+$',glt_lab)){
      bad.out <- rbind(bad.out,paste("label:",glt_lab,
                                     "has invalid characters!"))
    }
    if(any(startsWith(glt_lab,c("+","-")))){
      bad.out <- rbind(bad.out,paste("label:",glt_lab,
                                     "starts with invalid characters!"))
    }
  }

  ## get the weights and check for numeric
  glt.lvl.wt <- as.numeric(unlist(tstrsplit(glt_wt,',')))
  if(any(is.na(glt.lvl.wt))){
    bad.out <- rbind(bad.out,"All weights must be numeric!")
  }

  ## get the levels of the current variable and check if the numbers match
  if(!is.null(glt_lvl)){
    if(glt_lvl == "Quantitative, enter one number." & length(glt.lvl.wt) != 1){
      bad.out <- rbind(bad.out,"Need only 1 weight!")
    }
    if(length(glt.lvl.wt) != length(glt_lvl)){
      bad.out <- rbind(bad.out,
                       paste(length(glt.lvl.wt),'weight(s) do not match',
                             length(glt_lvl),'level(s)!'))
    }
  } else {
    bad.out <- rbind(bad.out,"One of the levels is not correct")
  }

  ## check if the label has been used before
  if(glt_list != "" & glt_lab != ""){
    ## get the table, select the odd rows (labels) and split
    glt.tab <- unlist(strsplit(glt_list,split='\n'))
    glt.labs <- glt.tab[c(TRUE,FALSE)]
    glt.labs <- sapply(strsplit(as.character(glt.labs)," "),"[[",3)

    ## add the current label and check for dups
    glt.labs <- c(glt.labs,glt_lab)
    if(anyDuplicated(glt.labs) != 0){
      bad.out <- rbind(bad.out,paste(glt_lab,"is already a label!"))
    }
  }
  return(bad.out)
}  ## end gltCheckFun

################################################
## change non specified qVars to factors
num2fact_fun <- function(qnt_vars_in,qntVar,vox.df){
  if(qnt_vars_in != ""){
    qVar.spec <- unlist(tstrsplit(qnt_vars_in,','))
    qVar.2.fac <- setdiff(qntVar,qVar.spec)
    vox.df[,qVar.2.fac] <- data.frame(apply(vox.df[qVar.2.fac],2,as.factor))
  } else {
    vox.df[,qntVar] <- data.frame(apply(vox.df[qntVar],2,as.factor))
  }
  return(vox.df)
}   ## end num2fact_fun

#####################################
## gltConstr:
## returns a list of weights named by variables or NULL on error
## takes a space separated list of the glt/f code and the data frame
## will convert any numeric variables to factor,
## so remove those first if they are specified in -qVars
gltConstr <- function(cStr, dataStr) {

  ## make sure there are some : to split variables
  pos <- which(cStr == ":")
  if(length(pos) < 1){
    print("ERROR: Missing variable separator ':'")
    return(NULL)
  }

  ## find all of the variable names and make sure they are in the data frame
  vars  <- cStr[pos-1]
  varsOK <- vars %in% colnames(dataStr)
  if(!all(varsOK)) {
    print(paste0("ERROR: Variables not in the data frame: ",
                 vars[which(!varsOK)]))
    return(NULL)
  }

  ## (not sure what this does) add an artificial one for convenient usage below
  pos <- c(pos, length(cStr)+2)

  ## make a list and loop through the variables
  gltList <- vector('list',0)
  slpList <- vector('list',0)
  slp.ind <- glt.ind <- 1
  slp.names <- glt.names <- c()
  for(ii in 1:length(vars)) {

    ## if it is numeric, force to factor (because it could be)
    if(!is.factor(dataStr[,vars[ii]])){
      dataStr[,vars[ii]] <- factor(dataStr[,vars[ii]])
    }

    ## assume numeric with no weights
    if(!grepl('\\*',cStr[(pos[ii]+1):(pos[ii+1]-2)])){
      ## get the single number, name, and increment
      slpList[[slp.ind]] <- as.numeric(cStr[(pos[ii]+1):(pos[ii+1]-2)])
      slp.names[[slp.ind]] <- vars[ii]
      slp.ind <- slp.ind+1

    } else {   ## factor

      ## get the levels for the factor and make 0's
      lvl <- levels(dataStr[,vars[ii]])
      gltList[[glt.ind]] <- rep(0, length(lvl))

      ## split the weights for levels by *
      sepTerms <- unlist(lapply(cStr[(pos[ii]+1):(pos[ii+1]-2)],strsplit,'\\*'))

      ## get all the levels and make sure they are in the variable
      lvlInv <- sepTerms[seq(2,length(sepTerms),2)]
      if(!all(lvlInv %in% lvl)){
        print(paste0("ERROR: Levels not in ",vars[ii],": ",
                     lvlInv[which(!(lvlInv %in% lvl))]))
        return(NULL)
      }

      ## match the level order and add the weights to the list
      sq <- match(lvlInv, lvl)
      gltList[[glt.ind]][sq] <- as.numeric(sepTerms[seq(1,length(sepTerms),2)])

      ## name the entry and increment
      glt.names[[glt.ind]] <- vars[ii]
      glt.ind <- glt.ind+1
    }
  }   ## end var loop

  names(slpList) <- slp.names
  names(gltList) <- glt.names

  OutList <- list(slpList=slpList,gltList=gltList)
  ## name the list and return
  # names(gltList) <- vars
  return(OutList)
}   ## end gltConstr

#####################################
## center qVars
qVar_center_fun <- function(qVars,qVar.centers,center.df){

  ## center by selections
  if(!is.null(qVar.centers)){
    for(i in 1:length(qVars)){
      center.df[,qVars[i]] <- scale(center.df[[qVars[i]]],
                                    center=qVar.centers[i][1],scale=FALSE)
    }
  } else {   ## center to mean
    for(i in 1:length(qVars)){
      center.df[,qVars[i]] <- scale(center.df[[qVars[i]]],
                                    center=TRUE,scale=FALSE)
    }
  }
  return(center.df)
}   ## end qVar_center_fun





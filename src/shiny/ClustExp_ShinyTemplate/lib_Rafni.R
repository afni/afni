#####################################
## 09/2017 Justin Rajendra
## "library" of helper functions for R

## necessary libraries
library(afex)
library(plotly)

#####################################
## return all permutations of all lengths of a vector pasted together
## takes a vector and a string to paste ("_","*",etc.)
combn_paste_fun <- function(vector.in,collapse.in){
  if(length(vector.in) > 1){
    vector.out <- vector.in
    for(i in 2:length(vector.in)){
      temp <- t(combn(vector.in,i))
      vector.out <- c(vector.out,apply(temp,1,paste,collapse=collapse.in))
    }
  } else { vector.out <- vector.in }   ## only one so return that
  return(vector.out)
}

#####################################
## return model string
## takes left hand side, main model (bsVars),
## within subject model, and the Error variable as strings
## if no wsVars, pass an NULL
ModelMaker <- function(lhs.in,bsVars.in,wsVars.in,err.str){
  if(!is.null(wsVars.in)){
    model.out <- paste0(lhs.in," ~ ",bsVars.in," + Error(Subj/", wsVars.in,")")
  } else {
    model.out <- paste0(lhs.in," ~ ",bsVars.in," + Error(Subj)")
  }
  return(model.out)
}   ## end model maker

#####################################
## return list of a fit object, R and p from standard lm
## takes a data frame and the x,y variables as characters
lmCalc <- function(data.in,x.in,y.in){

  ## make model and do the fit
  mod.in <- as.formula(paste(y.in,"~",x.in))
  fit <- lm(mod.in,data.in)

  ## get the R and p
  rValue <- sqrt(summary(fit)$r.squared)
  if(sign(fit$coefficients[2]) != 1) { rValue <- rValue * -1 }
  pValue <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],
               summary(fit)$fstatistic[3],lower.tail=FALSE)
  fit.out <- list(fit=fit,R=rValue,p=pValue)
  return(fit.out)
}

#####################################
## return vector of categorical within subject variable names
## takes a data frame and the column index for "subj"
wsVarCatFinder <- function(data.in,subj.col){

  ## check inputs
  if(!is.data.frame(data.in)){
    print("Input is not a data frame.")
    return(NA)
  }
  if(!is.numeric(subj.col)){
    print(paste(subj.col,"is not numeric."))
    return(NA)
  }

  ## see how many per subject and give error is all not the same
  subj.freq <- data.frame(table(data.in[[subj.col]]))$Freq
  if(!all(subj.freq[1] == subj.freq)) {
    print("Missing subject levels.")
    return(NA)
  }

  ## get the categorical
  catVars <- c(names(data.in)[sapply(data.in,is.factor)])
  if(length(catVars) == 0){
    print("No categorical variables.")
    return(NA)
  }

  ## empty data frame with the catVars
  subj.reps <- data.frame(matrix(ncol=length(catVars),nrow=0))
  colnames(subj.reps) <- catVars

  ## for each subj
  for(s in levels(data.in[[subj.col]])){

    ## subset and get rid of empty factor levels
    subj.temp <- subset(data.in,data.in[[subj.col]] == s,select=catVars)
    subj.temp <- as.data.frame(lapply(subj.temp,function(x)
      if(is.factor(x)) factor(x) else x))

    ## get the number of levels per variable and add to output
    rep.temp <- as.data.frame(lapply(subj.temp,function(x) length(levels(x))))
    subj.reps <- rbind(subj.reps,rep.temp)
  }

  ## are there any wsVars?
  if(any(subj.reps > 1)){
    wsVars <- c()
    ## if any subject has more than one level for the variable, it is a wsVar
    for(i in 1:length(subj.reps)){
      if(max(subj.reps[[i]]) > 1){
        wsVars <- c(wsVars,names(subj.reps[i]))
      }
    }
    return(wsVars)
  } else {
    print("No categorical within subject variables.")
    return(NA)
  }
}   ## end wsVarCatFinder

#####################################
## standard error with no extra library
std.err.mean <- function(x){ sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x))) }

#####################################
## return empty plotly with some message as the title
plotly_error <- function(message.in){
  plot_ly(x=1,y=1,alpha=0) %>%
    layout(title=message.in,font=list(color='red'),margin=list(t=80),
           yaxis=list(visible=FALSE),xaxis=list(visible=FALSE))
}



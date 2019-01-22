########################################
## 05/2018 Justin Rajendra
## Gang bayesian something or other 

## todo

## check for libraries  (add to @afni_R_package_install)
brms.test <- require(brms)
if(!brms.test){
  print('missing brms library')
  q()
}

########################################
## read arguments
args <- commandArgs(trailingOnly=TRUE)
InFile <- args[1]
y_var <- args[2]
x_var <- unlist(strsplit(args[3],' '))   ## split list by space
OutFile <- args[4]
NumIter <- as.numeric(args[5])
NumChains <- as.numeric(args[6])
ControlList <- args[7]
InSeed <- as.numeric(args[8])
BaseFolder <- args[9]
plot_or_not <- as.logical(args[10])
StanPlots <- unlist(strsplit(args[11],' '))
save_RData <- as.logical(args[12])
NoCenter <- as.logical(args[13])

########################################
## plot parameters
line.width <- 2
trans <- 0.6
sig.col <- "darkred"
sig.bg <- rgb(1,0,0,alpha=trans)
may.col <- "purple4"
may.bg <- rgb(0.6,0.2,1,alpha=trans)

########################################
## go to folder and set some options
setwd(BaseFolder)
options(mc.cores = parallel::detectCores())
set.seed(InSeed)

########################################
## read in the data and center variables
data.df <- read.table(InFile,header=TRUE)

## center the explanatory variables (skip first x_var for intercept)
## add scale true later
if(!NoCenter & length(x_var) > 1){
  for(x in x_var[2:length(x_var)]){
    data.df[,x] <- scale(data.df[,x],center=TRUE,scale=FALSE)
  }
}

########################################
## make the formula

## start
FormulaStr <- paste0(y_var," ~ ",x_var[1])
if(length(x_var) > 1){
  for(i in 2:length(x_var)){
    FormulaStr <- paste0(FormulaStr,"+",x_var[i])
  }
}

## middle
FormulaStr <- paste0(FormulaStr,"+(1|Subj)+(")
FormulaStr <- paste0(FormulaStr,x_var[1])
if(length(x_var) > 1){
  for(i in 2:length(x_var)){
    FormulaStr <- paste0(FormulaStr,"+",x_var[i])
  }
}

## end
FormulaStr <- paste0(FormulaStr,"|ROI)")

## make function and print out
FormulaFunc <- as.formula(FormulaStr)
cat("\nModel is:\n")
print(FormulaFunc)
cat("\n")

########################################
## run the model

## get the control options
if(ControlList != ""){
  cntrl_list <- eval(parse(text=paste('list(',ControlList,')')))
} else {
  cntrl_list <- NULL
}

## save the output
brmsFit <- brm(FormulaFunc,data=data.df,chains=NumChains,iter=NumIter,
               control=cntrl_list)

## save the summary
capture.output(summary(brmsFit),file=paste0(OutFile,"_summary.txt"))

########################################
## assemble the outputs

## Population-Level Estimates
PopEff <- fixef(brmsFit,summary=FALSE)

## Extract Group-Level (or random-effect) Estimate
GrpEff <- ranef(brmsFit,summary=FALSE)

## check for intercept (double check)
if(x_var[1] == "1"){ x_var[1] <- "Intercept" }

########################################
## loop through the variables for output and plots
for(x in x_var){
  
  ## split output for current variable and calculate stuff
  PostSamp <- apply(GrpEff[['ROI']][,,x],2,'+',PopEff[,x])
  
  ## median, standard deviation, HPD? interval
  MedianEst <- apply(PostSamp,2,mean)  
  StdDev <- apply(PostSamp,2,sd)
  QuantInt <- apply(PostSamp,2,quantile, c(0.025,0.05,0.5,0.95,0.975))
  
  ## get table of outputs and write out
  out.tab <- t(rbind(MedianEst,StdDev,QuantInt))
  write.csv(round(out.tab,3),file=paste0(OutFile,"_",x,"_table.csv"))
  
  ########################################
  ## plots
  if(plot_or_not){
    
    ## place holders for colors
    box.color <- rep(0,length(row.names(out.tab)))
    box.border <- rep(1,length(row.names(out.tab)))
    axis.sig <- axis.may <- axis.not <-  c()
    
    ## get sigificance colors
    for(i in 1:length(row.names(out.tab))){
      if(out.tab[i,3] > 0 & out.tab[i,7] > 0){
        box.color[i] <- sig.bg
        box.border[i] <- sig.col
        axis.sig <- c(axis.sig,i)
      } else if(out.tab[i,3] < 0 & out.tab[i,7] < 0){
        box.color[i] <- sig.bg
        box.border[i] <- sig.col
        axis.sig <- c(axis.sig,i)
      } else if(out.tab[i,4] > 0 & out.tab[i,6] > 0){
        box.color[i] <- may.bg
        box.border[i] <- may.col
        axis.may <- c(axis.may,i)
      } else if(out.tab[i,4] < 0 & out.tab[i,6] < 0){
        box.color[i] <- may.bg
        box.border[i] <- may.col
        axis.may <- c(axis.may,i)
      } else {
        axis.not <- c(axis.not,i)
      }
    }
    
    ## fake boxplot list from fake quantiles
    box.tab <- list(stats=rbind(MedianEst,StdDev,QuantInt)[3:7,],
                    n=rep(1,length(row.names(out.tab))))
    
    ## plot boxes and 0 line
    png(file=paste0(OutFile,"_",x,"_box.png"),w=20,h=10,res=100,units="in")
    par(mar=c(10,4,4,2))
    bxp(box.tab,axes=FALSE,boxfill=box.color,border=box.border,
        lwd=line.width-0.2,main=x,cex.main=2,font.main=2)
    abline(h=0,lwd=line.width)
    
    ## y label
    mtext(x,side=2,line=2.5,font=2,cex=2)
    
    ## add axis colors
    if(length(axis.not) > 0){
      axis(1,at=axis.not,labels=row.names(out.tab)[axis.not],
           srt=45,las=2,col.axis='black',font=2)
    }
    if(length(axis.sig) > 0){
      axis(1,at=axis.sig,labels=row.names(out.tab)[axis.sig],
           srt=45,las=2,col.axis=sig.bg,font=2)
    }
    if(length(axis.may) > 0){
      axis(1,at=axis.may,labels=row.names(out.tab)[axis.may],
           srt=45,las=2,col.axis=may.bg,font=2)
    }
    axis(2,font=2)
    box()
    dev.off()
    
    ## fit plot
    png(file=paste0(OutFile,"_",x,"_fit.png"),w=10,h=10,res=100,units="in")
    plot(brmsFit,ask=FALSE)
    dev.off()
    
    ## ppc
    pp_check(brmsFit)
    ggsave(file=paste0(OutFile,"_",x,"_postPredCheck.png"),
           w=10,h=10,dpi=100,units="in")
    
    ## stan plots
    for(s in StanPlots){
      stanplot(brmsFit,type=s)
      ggsave(file=paste0(OutFile,"_",x,"_",s,".png"),
             w=10,h=10,dpi=100,units="in")
    }
  }   ## end plots
}   ## end x_var loop

########################################
## rhat output table, no loopy

## empty
rhats.df <- data.frame(var=NULL,effect=NULL,rhat=NULL)

## for each random effect in the list
for(i in 1:length(summary(brmsFit)$random)){
  
  ## names 
  effect.names <- row.names(summary(brmsFit)$random[[i]])
  
  ## assemble values and add to output data frame
  temp.df <- data.frame(variable=rep(names(summary(brmsFit)$random)[i],
                                     length(effect.names)),
                        effect=row.names(summary(brmsFit)$random[[i]]),
                        rhat=round(summary(brmsFit)$random[[i]][,6],3))
  rhats.df <- rbind(rhats.df,temp.df)
}

## population level
effect.names <- row.names(summary(brmsFit)$fixed)
variable <- rep('PopulationLevel',length(effect.names))
rhat <- round(summary(brmsFit)$fixed[,6],3)
temp.df <- data.frame(variable=variable,effect=effect.names,rhat=rhat)
rhats.df <- rbind(rhats.df,temp.df)

## family specific
effect.names <- row.names(summary(brmsFit)$spec_pars)
variable <- rep('FamilySpecific',length(effect.names))
rhat <- round(summary(brmsFit)$spec_pars[,6],3)
temp.df <- data.frame(variable=variable,effect=effect.names,rhat=rhat)
rhats.df <- rbind(rhats.df,temp.df)

## save
write.csv(rhats.df,file=paste0(OutFile,"_rhats.csv"),row.names=FALSE)

########################################
## save workspace?
if(save_RData){
  save.image(file=paste0(OutFile,".RData"))
}

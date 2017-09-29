#####################################
## 09/2017 Justin Rajendra
## Cluster Explorer
## plot and stat functions

library(psych)
library(RColorBrewer)
library(afex)
library(plotly)

################################################
## helper functions

################
## make a vector of variable depending on the split
## takes some categorical variables and how to combine them
varCombine <- function(bsVars.in,wsVars.in,bs_ws.split){
  model.final <- NA
  if(!is.null(bs_ws.split)){
    if(bs_ws.split == "Within"){
      if(!is.null(wsVars.in)){
        model.final <- wsVars.in
      } else { print("variables names are NULL") ; model.final <- NA }
    } else if(bs_ws.split == "Between"){
      if(!is.null(bsVars.in)){
        model.final <- bsVars.in
      } else { print("variables names are NULL") ; model.final <- NA }
    } else if(bs_ws.split == "Both"){
      if(!is.null(bsVars.in) & !is.null(wsVars.in)){
        model.final <- c(bsVars.in,wsVars.in)
      } else { print("variables names are NULL") ; model.final <- NA }
    }
  } else {
    if(!all(is.na(bsVars.in)) & bsVars.in[1] != "None"){
      model.final <- bsVars.in
    } else if(!is.na(wsVars.in)){
      model.final <- wsVars.in
    }
  }
  return(model.final)
}   ## end varCombine

################
## return data frame of categorical interaction models
## takes the data, some categorical variables and how to combine them
InteractionFrame <- function(data.in,bsVars.in,wsVars.in,bs_ws.split){

  ## check inputs
  if(!is.data.frame(data.in)){
    print("Input is not a data frame")
    return(NA)
  }
  if(is.null(bsVars.in) & is.null(wsVars.in)){
    print("Need something to make a model")
    return(NA)
  }

  ## get all of the model vars
  model.final <- varCombine(bsVars.in,wsVars.in,bs_ws.split)

  ## combine the groups if necessary
  if(length(model.final) > 1){
    data.in$Groups <- apply(data.in[,model.final],1,paste,collapse=".")
    data.in$Groups <- factor(data.in$Groups)
  } else {
    data.in$Groups <- data.in[[model.final]]
  }
  return(data.in)
}   ## end InteractionFrame

################
## return data frame with summary stats for by level
## takes a data frame, the continuous variable and a categorical variable
InteractionCalc <- function(data.in,cont.var,fact.var){

  ## stat by level
  mean.df <- aggregate(data.in[[cont.var]],list(data.in[[fact.var]]),mean)
  sd.df <- aggregate(data.in[[cont.var]],list(data.in[[fact.var]]),sd)
  se.df <- aggregate(data.in[[cont.var]],list(data.in[[fact.var]]),std.err.mean)
  n.df <- aggregate(data.in[[cont.var]],list(data.in[[fact.var]]),length)

  ## combine, rename, and return
  out.df <- cbind(mean.df,sd.df$x,se.df$x,n.df$x)
  names(out.df) <- c(fact.var,"Mean","SD","SE","n")
  return(out.df)
}  ## end InteractionCalc



################################################
stat_plot_fun <- function(plot.df,prefix,p_val,stat.info,clust.lab,
                          bsVarsCat.sel,vox.vol,fixed.range,custom.range,
                          qVars.sel,wsVars.sel,one.plot,col.pal,marker.size,
                          marker.opacity,box.points,line.width,box.mean,
                          bs_ws.split,qVars.center.sel,box.scatter,
                          jit,error.bars){

  ## return empty on waiting
  if(nrow(plot.df) == 0){ return(plotly_empty()) }

  ## change box.mean to logical if needed
  if(!is.null(box.mean)){
    if(box.mean != 'sd'){ box.mean <- as.logical(box.mean) }
  }

  ## y axis zoom
  if(fixed.range){
    y.axis <- list(title='',range=custom.range)
  } else {
    y.axis <- list(title='')
  }

  ## make color list for everything but ttest
  if(stat.info != "Ttest"){
    if(is.null(col.pal)){
      col.list <- brewer.pal(200,"Dark2")
    } else {
      col.list <- brewer.pal(200,col.pal)
    }
  }

  ## modify plotly window
  plotly.layout <- list("toImage","hoverCompareCartesian",
                        "hoverClosestCartesian", "toggleSpikelines")

  ################################################
  if(stat.info == "Ttest"){

    ## make color list
    if(is.null(col.pal)){
      col.list <- c('red','blue')
    } else if(col.pal == 'RedBlue'){
      col.list <- c('red','blue')
    } else {
      col.list <- brewer.pal(3,col.pal)
    }

    ## plotly
    plot.ly <- plot_ly(plot.df,x=~Group,y=~value,color=~Group,type='box',
                       colors=col.list,showlegend=FALSE,
                       boxpoints=box.points,boxmean=box.mean)
    layout(plot.ly,title=paste0(clust.lab," (",vox.vol," voxels)"),
           yaxis=y.axis,margin=list(t=80)) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=plotly.layout)

    ################################################
    ## box plots
  } else if(stat.info == "3dMVM" & box.scatter == "Box"){

    ## return empty if nothing here
    if(is.null(bsVarsCat.sel) & bs_ws.split == "Between"){
      return(plotly_error("Must have at least one categorical variable!"))
    }
    if(is.null(wsVars.sel) & bs_ws.split == "Within"){
      return(plotly_error("Must have at least one categorical variable!"))
    }
    if(is.null(bsVarsCat.sel) & is.null(wsVars.sel)){
      return(plotly_error("Must have at least one categorical variable!"))
    }
    if(!is.null(bsVarsCat.sel) & is.null(wsVars.sel)){
      if(bsVarsCat.sel == "None"){
        return(plotly_error("Must have at least one categorical variable!"))
      }
    }

    ## get the combinations of categorical variables
    plot.df <- InteractionFrame(plot.df,bsVarsCat.sel,wsVars.sel,bs_ws.split)

    ## plot
    plot.ly <- plot_ly(plot.df,x=~Groups,y=~value,type="box",color=~Groups,
                       colors=col.list,boxpoints=box.points, boxmean=box.mean)
    layout(plot.ly,title=paste0(clust.lab," (",vox.vol," voxels)"),
           yaxis=y.axis,margin=list(t=80)) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=plotly.layout)


    ################################################
    ## interaction plots
  } else if(stat.info == "3dMVM" & box.scatter == "Interaction"){

    ## return empty if nothing here
    if(is.null(bsVarsCat.sel) & is.null(wsVars.sel)){
      return(plotly_error("Must have at least one categorical variable!"))
    }
    if(bsVarsCat.sel == "None" & is.null(wsVars.sel)){
      return(plotly_error("Must have at least one categorical variable!"))
    }

    ## get all of the model vars
    model.final <- varCombine(bsVarsCat.sel,wsVars.sel,bs_ws.split)

    ## no more than 2 categorical variables
    if(length(model.final) != 2 ){
      return(plotly_error("Need exactly 2 categorical variables!"))
    }

    ## split by first variable and do the stats by the second
    lvl.list <- split(plot.df,plot.df[[model.final[1]]])
    stat.list <- lapply(lvl.list,
                        function(x) InteractionCalc(x,"value",model.final[2]))

    ## plot parameters
    if(is.null(marker.size)){ marker.size <- 12 ; marker.opacity <- 0.8 }
    x.lvl <- levels(plot.df[[model.final[2]]])
    x.axis <- list(title=model.final[2],autotick=FALSE,tickmode='array',
                   tickvals=c(1:length(x.lvl)),ticktext=x.lvl,
                   zeroline=FALSE)
    y.axis$zeroline <- FALSE
    if(error.bars == 'no'){
      error.list <- list(visible=FALSE)
    } else if(error.bars == 'SD') {
      error.list <- list(type="data",array=~SD,thickness=line.width)
    } else if(error.bars == 'SE') {
      error.list <- list(type="data",array=~SE,thickness=line.width)
    }

    ## plot
    plot.ly <- plot_ly()
    for(i in 1:length(stat.list)){

      ## colors
      marker.list <- list(size=marker.size,color=col.list[i],
                          opacity=marker.opacity)
      line.list <- list(color=col.list[i],width=line.width)

      ## add jitter and plot
      x.jit <- jitter(c(1:length(x.lvl)),jit)
      plot.ly <- add_trace(plot.ly,data=stat.list[[i]],x=x.jit,y=~Mean,
                           type='scatter',mode='markers+lines',
                           name=names(stat.list)[i],text=~n,
                           error_y=error.list,hoverinfo="y+text",
                           marker=marker.list,line=line.list)
    }
    layout(plot.ly,title=paste0(clust.lab," (",vox.vol," voxels)"),
           yaxis=y.axis,xaxis=x.axis,margin=list(t=80)) %>%
      config(displaylogo=FALSE,modeBarButtonsToRemove=plotly.layout)

    ## end interaction
    ################################################
    ## has categorical and quantitative variables
  } else if(stat.info == "3dMVM" & box.scatter == "Scatter"){

    ## temp markers
    if(is.null(marker.size)){ marker.size <- 8 ; marker.opacity <- 0.8 }

    ## return empty if nothing here
    if(bsVarsCat.sel == "None" & is.null(wsVars.sel)){ return(plotly_empty()) }

    ## get the combinations of categorical variables
    plot.df <- InteractionFrame(plot.df,bsVarsCat.sel,wsVars.sel,bs_ws.split)

    ## levels for looping
    n.lvl <- length(levels(plot.df$Groups))
    names.lvl <- levels(plot.df$Groups)

    ## check for qVars and center them to mean or specified
    if(qVars.center.sel){
      ## center by mean
      if(is.na(SI.df$qVarCenters)){
        center.str <- "mean"
        for(i in 1:length(qVars)){
          plot.df[,qVars[i]] <- scale(plot.df[[qVars[i]]],center=TRUE,
                                      scale=FALSE)
        }
      } else {   ## center by value
        center.str <- SI.df$qVarCenters
        qVars.cent <- as.numeric(unlist(tstrsplit(SI.df$qVarCenters,',')))
        for(i in 1:length(qVars)){
          plot.df[,qVars[i]] <- scale(plot.df[[qVars[i]]],
                                      center=qVars.cent[[i]],scale=FALSE)
        }
      }
    }   ## end centering

    ## one plot or subplots
    if(!one.plot){   ## overlay all plots
      plot.ly <- plot_ly()
      ## loop through the levels of the selected variable
      for(i in 1:n.lvl){

        ## subset and make a new data frame for easy names
        temp.df <- subset(plot.df,plot.df$Groups == names.lvl[i])
        plot.temp <- data.frame(x=temp.df[[qVars.sel]],y=temp.df$value,
                                Subj=temp.df$Subj)

        ## do the fit with the custom function
        fit.calc <- lmCalc(plot.temp,"x","y")
        plot.fit <- fit.calc$fit
        rValue <- paste0('R = ',signif(fit.calc$R,digits=2))
        pValue <- paste("p = ",signif(fit.calc$p,digits=2))
        legend.text <- paste0(rValue,' (',pValue,')')

        ## axis property lists
        x.axis <- list(title=qVars.sel)
        marker.list <- list(size=marker.size,color=col.list[i],
                            opacity=marker.opacity)
        line.list <- list(color=col.list[i],width=line.width)
        ## plot
        plot.ly <- add_trace(plot.ly,data=plot.temp,x=~x,y=~y,type='scatter',
                             mode='markers',text=~Subj,name=names.lvl[i],
                             marker=marker.list)
        plot.ly <- add_trace(plot.ly,x=plot.temp$x,y=fitted(plot.fit),
                             name=legend.text,line=line.list,
                             type='scatter',mode='lines',inherit=FALSE)
      }
      layout(plot.ly,title=paste0(clust.lab," (",vox.vol," voxels)"),
             yaxis=y.axis,xaxis=x.axis,margin=list(t=80)) %>%
        config(displaylogo=FALSE,modeBarButtonsToRemove=plotly.layout)

      ################################################
    } else {   ## subplots
      ## make a list to save all of the plots
      plot.list <- list()

      ## loop through the levels of the selected variable
      for(i in 1:n.lvl){

        ## subset and make a new data frame for easy names
        temp.df <- subset(plot.df,plot.df$Groups == names.lvl[i])
        plot.temp <- data.frame(x=temp.df[[qVars.sel]],y=temp.df$value,
                                Subj=temp.df$Subj)

        ## do the fit with the custom function
        fit.calc <- lmCalc(plot.temp,"x","y")
        plot.fit <- fit.calc$fit
        rValue <- paste0('R = ',signif(fit.calc$R,digits=2))
        pValue <- paste("p = ",signif(fit.calc$p,digits=2))
        legend.text <- paste0(rValue,' (',pValue,')')

        ## axis property lists
        x.axis <- list(title=qVars.sel)
        marker.list <- list(size=marker.size,color=col.list[i],
                            opacity=marker.opacity)
        line.list <- list(color=col.list[i],width=line.width)

        ## plot
        plot.ly <- plot_ly(plot.temp,x=~x,y=~y,type='scatter',mode='markers',
                           text=~Subj,name=names.lvl[i], marker=marker.list)
        plot.ly <- add_trace(plot.ly,x=plot.temp$x,y=fitted(plot.fit),
                             name=legend.text,line=line.list,
                             type='scatter',mode='lines',inherit=FALSE)
        plot.ly <- layout(plot.ly,yaxis=y.axis,xaxis=x.axis,margin=list(t=80),
                          title=paste0(clust.lab," (",vox.vol," voxels)")) %>%
          config(displaylogo=FALSE,modeBarButtonsToRemove=plotly.layout)

        ## add to plot list
        plot.list[[i]] <- plot.ly

      }   ## end level loop
      ## plot all in list with 2 per row
      if(n.lvl == 1){ plot.rows <- 1 } else { plot.rows <- ceiling(n.lvl/2) }
      subplot(plot.list,shareX=TRUE,shareY=TRUE,nrows=plot.rows)
    }   ## end overplot ancova
  }   ## end different plots
}   ## end stat_plot_fun

################################################
## function for statistics summary
stat_fun <- function(plot.df,prefix,p_val,stat.info,t.paired,clust.lab,
                     bsVarsCat.sel,vox.vol,qVars.sel,wsVars.sel,orig.plot,
                     bs_ws.split,box.scatter){

  ## make sure there is something there there
  if(nrow(plot.df) == 0){ return("please wait") }

  ## return ttest for paired or not (same format as the anova summary)
  if(stat.info == "Ttest"){
    if(t.paired){
      return(summary(aov(value ~ Group + Error(Subj),plot.df)))
    } else {
      return(summary(aov(value ~ Group,plot.df)))
    }
  }

  ## stat for whatever it was originally
  if(orig.plot == "orig" & !is.na(wsVars.model)){
    model.final <- paste0("value ~ ",mvm.model," + Error(Subj/",wsVars.model,")")
  }
  if(orig.plot == "orig" & is.na(wsVars.model)){
    model.final <- paste0("value ~ ",mvm.model," + Error(Subj)")
  }

  #####################
  ## stats for box plots as selected
  if(orig.plot == 'plot' & box.scatter == "Box"){
    if(bs_ws.split == "Between"){
      if(is.null(bsVarsCat.sel)){
        return(cat("Error: No between subjects variable selected!"))
      }
      model.vars <- combn_paste_fun(bsVarsCat.sel,"*")
      model.vars <- model.vars[length(model.vars)]
      model.final <- paste0("value ~ ",model.vars," + Error(Subj)")

    } else if(bs_ws.split == "Within"){
      if(is.null(wsVars.sel)){
        return(cat("Error: No within subjects variable selected!"))
      }
      model.vars <- combn_paste_fun(wsVars.sel,"*")
      model.vars <- model.vars[length(model.vars)]
      model.final <- paste0("value ~ 1"," + Error(Subj/",model.vars,")")

    } else if(bs_ws.split == "Both"){
      if(is.null(bsVarsCat.sel) | is.null(wsVars.sel)){
        return(cat("Error: Need between AND within subjects variables!"))
      }
      model.vars <- varCombine(bsVarsCat.sel,wsVars.sel,bs_ws.split)
      model.vars <- combn_paste_fun(model.vars,"*")
      model.vars <- model.vars[length(model.vars)]
      model.ws   <- combn_paste_fun(wsVars.sel,"*")
      model.ws   <- model.ws[length(model.ws)]
      model.final <- paste0("value ~ ",model.vars," + Error(Subj/",model.ws,")")
    }
  }   ## end box

  #####################
  ## stats for interaction plots as selected
  if(orig.plot == 'plot' & box.scatter == "Interaction"){
    if(bs_ws.split == "Between"){
      if(length(bsVarsCat.sel) != 2){
        return(cat("Error: Need EXACTLY 2 categorical variables!!"))
      }
      model.vars <- combn_paste_fun(bsVarsCat.sel,"*")
      model.vars <- model.vars[length(model.vars)]
      model.final <- paste0("value ~ ",model.vars," + Error(Subj)")

    } else if(bs_ws.split == "Within"){
      if(length(wsVars.sel) != 2){
        return(cat("Error: Need EXACTLY 2 categorical variables!!"))
      }
      model.ws <- combn_paste_fun(wsVars.sel,"*")
      model.ws <- model.ws[length(model.ws)]
      model.final <- paste0("value ~ ",model.ws," + Error(Subj/",model.ws,")")

    } else if(bs_ws.split == "Both"){
      model.vars <- varCombine(bsVarsCat.sel,wsVars.sel,bs_ws.split)
      if(length(model.vars) != 2){
        return(cat("Error: Need EXACTLY 2 categorical variables!!"))
      }
      model.vars <- combn_paste_fun(model.vars,"*")
      model.vars <- model.vars[length(model.vars)]
      model.ws   <- combn_paste_fun(wsVars.sel,"*")
      model.ws   <- model.ws[length(model.ws)]
      model.final <- paste0("value ~ ",model.vars," + Error(Subj/",model.ws,")")
    }
  }   ## end interaction

  #####################
  ## stats for scatter plots as selected

  ## check for qVars and center them to mean or specified
  if(stat.info == "3dMVM" & !is.na(qVars)){
    if(is.na(SI.df$qVarCenters)){
      center.str <- "mean"
      for(i in 1:length(qVars)){
        plot.df[,qVars[i]] <- scale(plot.df[[qVars[i]]],center=TRUE,scale=FALSE)
      }
    } else {   ## center by value
      center.str <- SI.df$qVarCenters
      qVars.cent <- as.numeric(unlist(tstrsplit(SI.df$qVarCenters,',')))
      for(i in 1:length(qVars)){
        plot.df[,qVars[i]] <- scale(plot.df[[qVars[i]]],
                                    center=qVars.cent[[i]],scale=FALSE)
      }
    }
  }   ## end centering

  if(orig.plot == 'plot' & box.scatter == "Scatter"){
    if(bs_ws.split == "Between"){
      model.vars <- combn_paste_fun(bsVarsCat.sel,"*")
      model.vars <- model.vars[length(model.vars)]
      model.final <- paste0("value ~ ",model.vars,"*",qVars.sel," + Error(Subj)")

    } else if(bs_ws.split == "Within"){
      model.ws <- combn_paste_fun(wsVars.sel,"*")
      model.ws <- model.ws[length(model.ws)]
      model.final <- paste0("value ~ ",model.ws,"*",qVars.sel," + Error(Subj/",model.ws,")")

    } else if(bs_ws.split == "Both"){
      model.vars <- varCombine(bsVarsCat.sel,wsVars.sel,bs_ws.split)
      model.vars <- combn_paste_fun(model.vars,"*")
      model.vars <- model.vars[length(model.vars)]
      model.ws   <- combn_paste_fun(wsVars.sel,"*")
      model.ws   <- model.ws[length(model.ws)]
      model.final <- paste0("value ~ ",model.vars,"*",qVars.sel," + Error(Subj/",model.ws,")")
    }

  }   ## end scatter

  ## print out
  cat(paste0(clust.lab,'\n\n'))
  cat(paste0('Full Model = ',model.final,'\n\n'))
  if(stat.info == "3dMVM" & box.scatter == "Scatter"){
    cat(paste0(SI.df$qVars,' centered to ',center.str,'\n\n'))
  }
  model.final <- as.formula(model.final)
  anova.aov <- aov_car(model.final,plot.df,factorize=FALSE,type=SI.df$SS_type)
  return(anova(anova.aov))
}   ## end stat_fun

## function for statistics table
stat_desc <- function(plot.df,prefix,p_val,stat.info,clust.lab,
                      bsVarsCat.sel,vox.vol,fixed.range,custom.range,
                      qVars.sel,one.plot,col.pal,marker.size){

  if(bsVarsCat.sel == "None"){
    return(data.frame(message="No factor variables in main model."))
  }
  ## make sure there is something there there (and fill empty for reactive)
  if(nrow(plot.df) == 0){ return("Please wait.") }
  table.out <- data.frame(message="Please wait.")

  if(stat.info == "Ttest"){
    plot.df <- model.frame(value ~ Group,data=plot.df)
    table.out <- describeBy(plot.df$value,plot.df$Group,digits=5,mat=TRUE)
    table.out <- subset(table.out,
                        select=-c(vars,item,trimmed,range,skew,kurtosis))

  } else if(stat.info == "3dMVM" & bsVarsCat.sel != "None"){

    ## make the model as text and do the anova
    bsVarsCat.sel <- as.formula(paste0("value ~ ",bsVarsCat.sel))

    ## get the data frame by the model
    plot.df <- model.frame(bsVarsCat.sel,data=plot.df)

    ## get summary stats by list of grouping variables
    grp.names <- names(plot.df[2:length(plot.df)])
    grp.list <- as.list(plot.df[2:length(plot.df)])
    table.out <- describeBy(plot.df$value,grp.list,digits=5,mat=TRUE)
    table.out <- subset(table.out,
                        select=-c(vars,item,trimmed,range,skew,kurtosis))

    ## rename the "group1" etc
    for(i in 1:length(grp.names)){
      names(table.out)[i] <- grp.names[i]
    }
  }   ## end 3dMVM
  return(table.out)
}   ## end stat description

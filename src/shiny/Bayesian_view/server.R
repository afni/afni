## top ##################################
## 01/2022 Justin Rajendra
## Gang RBA 
## Server 

## server top #####################################
shinyServer(function(input,output,session) {
  options(warn = -1)
  session$onSessionEnded(function() { cat('\nAll done!\n') ; stopApp() })
  
  ## get data #########################################
  getROIs <- reactive({
    attach(input$fileSel)
    roi.temp <- as.data.frame(ps0)
    detach(paste0('file:',input$fileSel),character.only=TRUE)
    return(roi.temp)
  })   ## end getROIs
  
  
  ## update UI elements ###############################
  observeEvent(input$fileSel,{
    temp.rois <- getROIs()
    rois.list <- as.list(names(temp.rois))
    names(rois.list) <- names(temp.rois)
    
    updateSelectInput(session,"roiSel",choices=rois.list,selected=rois.list)
  })
  
  ## get stats ################################
  getStats <- reactive({
    
    ## get data remove unselected rois
    data <- getROIs()
    
    validate(need(input$roiSel,'more rois'))
    data <- subset(data,select=input$roiSel)
    
    data$X <- NULL
    nobj=dim(data)[1]
    # rename columns with ROI list
    # print(summary(data))
    rois <- names(data)
    colnames(data) <- rois
    data_stats <- data.frame(1:length(rois))
    
    # create ROI column instead of numerics to match threat table above
    data_stats$ROI <- rois
    data_stats$mean <- colMeans(data)  # median: quantile(x, probs=.5)
    data_stats$P <- colSums(data > 0)/nobj
    data_stats$Pn <- ifelse(data_stats$P < .5, 1-data_stats$P, data_stats$P)
    # this will order the distributions correctly
    data_stats <- data_stats[order(data_stats$mean),]
    
    data_trans <- as.data.frame(t(as.matrix(data)))
    # add two more columns
    data_trans <- tibble::rownames_to_column(data_trans, "ROI")
    data_trans$X <- 1:nrow(data_trans)
    
    # merge values & stats into one table by ROI
    data_merge <- merge(data_stats, data_trans, by = "ROI")
    data_merge <- data_merge[order(data_merge$X),]
    #browser()
    # Transform data into long form: Melt dataframe by ROI
    data_long <- reshape2::melt(data_trans, id=c("ROI","X"))
    data_long <- data_long[order(data_long$X),]
    
    #clunky, but for now stats by ensuring orders are all the same and repeating each value nobj times. no success for alternatives. 
    data_long$mean <- rep(data_merge$mean, each = nobj)
    data_long$P <- rep(data_merge$P, each =nobj)
    data_long$Pn <- rep(data_merge$Pn, each =nobj)
    data_long$gray.vals <- rep(data_merge$gray.vals, each =nobj)
    
    # print(tail(data_long))
    # 
    # ## calculate some stats
    # roi.stats <- data.frame(matrix(ncol=0,nrow=length(rois.df)))
    # 
    # # create ROI column instead of numerics to match threat table above
    # roi.stats$ROI <- as.factor(names(rois.df))
    # roi.stats$mean <- colMeans(rois.df)  # median: quantile(x, probs=.5)
    # roi.stats$P <- colSums(rois.df > 0) / length(rois.df)
    # roi.stats$Pn <- ifelse(roi.stats$P < .5, 1-roi.stats$P, roi.stats$P)
    # 
    # # this will order the distributions correctly
    # roi.stats <- roi.stats[order(roi.stats$mean),]
    # 
    # 
    data.out <- list(data_stats,data_long)
    return(data.out)
    
    
  })
  
  
  # observeEvent(input$roiSel,{
  #   roi.temp <- getStats()
  #   # roi.temp <- subset(roi.temp,select=input$roiSel)
  #   # print(summary(roi.temp))
  #   
  # })
  ##  ###############################
  output$gangPlot <- renderPlot({
    plot.list <- getStats()
    data_stats <- plot.list[[1]]
    data_long <- plot.list[[2]]
    rois <- data_stats$ROI
    
    labx <- input$plotTitle
    xlim <- input$plotRange
    
    
    
    y.axis.labs <- data_stats$ROI                              # y axis labels
    sec.y.axis.labs <- round(data_stats$P,3)                   # second y axis labels (probabilities) - Rounded to 2 decimals
    
    ################# X AXIS LABELS ###########################################################
    # X AXIS LABELS NEED TO CHANGE TO CORRESPOND TO DATA SET! UNCOMMENT WHICHEVER MATCHES
    x.axis.labs <- NULL                                # x axis labels  INTERACTION, not sure what to put.
    x.labs.pos <- NULL                                 # a axis position INTERACTION, change when labels decided
    
    ######## T I T L E S #############################################################
    #data.name <- tl
    graph.title <- "Interaction (% signal change)"                                   # graph title 
    legend.title <- "P+"                              # legend title
    y.axis.title <- NULL                                       # for now ...
    x.axis.title <- NULL                                       # for now...
    
    ########################## D A T A  ##############################################################
    # GRAPH DATA 
    dataset <- data_long
    x.values <- data_long$value                               # x values
    y.values <- data_long$ROI                                 # y values
    y.values.RO <- data_long$value                            # values to reorder Y by
    distrib.fill <- data_long$P                       # fill graph with probabilities
    group <- data_long$ROI
    
    ######################### S A V E  ################################################
    # SAVE SETTINGS -- Currently low res and jpeg to make it easier to share
    # adjusting height + width will throw font sizes out of wack: need change (see other aspects below) 
    
    dpi <- 300
    units <- "in"                                           # "in", "cm", or "mm"
    height <- 5
    width <- 9
    file.type <- ".jpeg"                   # can be ".jpeg",".pdf",".png",".bmp",".tiff",etc
    
    ############################### O T H E R  #################################################
    #gradient.colors<-c("#41245C","yellow","gray","gray","blue","#C9182B") # change gradient colors  
    
    if( input$colPal == "Gang" ){
      gradient.colors <- c("blue","cyan","gray","gray","yellow","#C9182B")
    } else if( input$colPal == "Dark2" ){
      gradient.colors <- brewer.pal(6,"Dark2")
    } else if( input$colPal == "Set1" ){
      gradient.colors <-  brewer.pal(6,"Set1")
    } else if( input$colPal == "Accent" ){
      gradient.colors <- brewer.pal(6,"Accent")
    } else if( input$colPal == "Rainbow" ){
      gradient.colors <- rainbow(6)[c(1,3,4,5,6)]
    } else {
      gradient.colors <- c("blue","cyan","gray","gray","yellow","#C9182B")  # change gradient colors here 
    }
    
    ROI.label.size <- input$ROI_label_size                # adjust ROI and probability y-axis font size
    P.label.size <- input$P_label_size
    title.size <- input$title_size                         # adjust graph title size 
    x.axis.size <- input$x_axis_size                                        # adjust x-axis label sizes
    
    ##################  G R A P H  ########################################
    # change information about the graph and add other characteristics using ggplot and ggridges
    ggplot(dataset, aes(x = x.values, 
                        y = as.numeric(reorder(y.values, y.values.RO)), 
                        fill = distrib.fill, 
                        group = group))   +
      guides(fill = guide_colorbar(barwidth = 1,             #legend characteristics
                                   barheight = 20,
                                   nbin = 100, # can change # bins to play around with color gradient
                                   frame.colour = "black",
                                   frame.linewidth = 1.5,
                                   ticks.colour = "black",
                                   title.position = "top",
                                   title.hjust = 0.5)) +
      #geom_density_ridges() +                            # scale = spacing, alpha = transparency
      stat_density_ridges(quantile_lines = TRUE,         # divide into two quantiles (show mean)
                          quantiles = 2,
                          size = .6,
                          alpha = .8,
                          scale = 2,
                          color = "black") +
      geom_vline(xintercept = 0,                        #create line at X = 0
                 linetype="solid",
                 alpha = 1,
                 size = 1,
                 color = "green3") +
      scale_fill_gradientn(
        colors = gradient.colors,                       # set gradient
        limits = c(0,1),                                # scale size
        name = legend.title,
        breaks = c(0,0.05,0.1,0.9,0.95,1),
        expand = expansion(0),
        labels = c("0","0.05","0.1","0.9", "0.95","1")
      ) +
      scale_y_continuous(breaks = 1:length(rois), # A VERY HACK-Y WAY TO HAVE TWO Y AXES W DISCRETE DATA
                         labels = y.axis.labs,   # Trick ggplot into thinking data is continuous...
                         sec.axis = sec_axis(~.,  # Second axis to show probabilities
                                             breaks = 1:length(rois),
                                             labels = sec.y.axis.labs)) +
      theme_ridges(font_size = ROI.label.size, grid = TRUE, center_axis_labels = TRUE) +  # theme info
      #ggtitle(graph.title)+                                                   # graph title
      annotate("text", x=0.06, y=1.5, label=labx, size=11)+
      theme(
        plot.title = element_text(vjust = -0.5, size = title.size),   # plot title size and position
        #axis.title.x=element_text(vjust=0, hjust=0.5),
        axis.text.y.left = element_text(size=ROI.label.size),        # y-axis text size
        axis.text.y.right = element_text(size = P.label.size),       # y-axis info for right axis
        axis.text.x = element_text(size = x.axis.size),   # x-axis text size/face
        #axis.text.x = element_text(size = x.axis.size, face = "bold"),   # x-axis text size/face
        axis.title=element_text(size=28), #,face="bold"),
        legend.title.align = 5,
        #legend.text = element_text(face = "bold"),
        legend.title = element_text(size = 24))+
      #legend.title = element_text(face = "bold", size = 15))+
      labs(
        #x = 'interaction (% signal change)',                 # Add or not add X and Y labels
        x = NULL,
        y = NULL) +
      scale_x_continuous(labels=waiver(), limits = xlim) +
      xlab(labx)
    
  })
  
  
  
  
  
  
  
  
  
  ## time plot prep ##########################################
  output$time_out <- renderPlotly({
    
    showNotification("Loading data...",id="loading",duration=NULL,type="error")
    
    ## make sure we have something
    if( input$showHome ){
      validate(need(input$scoreSel,'Need at least 1 Home Measurement!!'))
      validate(need(input$InterviewType,'Need at least 1 time of day!!'))
    }
    
    ## get the data
    plot.df <- getOne()
    plot.df <- plot.df[order(plot.df$DateTime),]
    
    
    ## ranges #################################
    
    ## x
    x.range <- range(plot.df$DateTime,na.rm=TRUE)
    
    ## starting y's
    y.score.range <- y.cat.range <-  c(100,-100)
    
    
    
    ## plot by time #####################################
    plot.ly <- plot_ly(type="scatter",mode="markers+lines")
    plot.ly <- config(plot.ly,displayModeBar=FALSE)
    
    ## home scales #######################################
    
    plot.ly <- add_trace(
      plot.ly,connectgaps=TRUE,
      x=chron.df$DateTime,
      y=chron.df[[input$scoreSel[s]]],
      marker=list(size=marker.size,color=c.col,
                  line=list(color=c.col,width=line.wd)),
      line=list(color=c.col,width=line.wd),
      name=s.name,
      hoverinfo="text",
      text=paste0(
        s.name," ",
        round(chron.df[[input$scoreSel[s]]],2),"<br>",
        chron.df$InterviewType,"<br>",
        wday(chron.df$DateTime,label=TRUE)," ",
        format(chron.df$DateTime,"%m-%d-%Y")," ",
        format(chron.df$DateTime,"%H:%M"),"<br>",
        chron.df$Phase)
    )
    
    
    temp.df <- subset(chron.df,!is.na(chron.df[[input$scoreSel[s]]]))
    smo <- loess(temp.df[[input$scoreSel[s]]] ~ 
                   as.numeric(temp.df$DateTime),span=input$span)
    plot.ly <- add_lines(plot.ly,x=temp.df$DateTime,y=predict(smo),
                         name=paste0(s.name,'\nLOESS (',
                                     input$span,')'),
                         type='scatter',mode='lines',
                         line=list(width=line.wd+1.5,color=c.col))
    rm(list="temp.df")
    
    
    
    
    
    ## end home scales
    
    
    
    ## decorations ###########################
    x.axis <- list(range=x.range,title='Date')
    
    if( input$showLab ){
      if( input$showHome | input$showCat ){
        y.axis=list(range=y.left.range,title='Home')
        y2.axis=list(range=y.rate.range,title='Lab',overlaying="y",side="right") 
        l.enged <- list(x=1.1)
      } else {
        y.axis=list(range=y.rate.range,title='Lab')
        y2.axis=list()
        l.enged <- list()
      }
    } else {
      y.axis=list(range=y.left.range,title='Home') 
      y2.axis=list()
      l.enged <- list()
    }
    
    plot.ly <- layout(plot.ly,title=main.title,legend=l.enged,
                      xaxis=x.axis,yaxis=y.axis,yaxis2=y2.axis,
                      shapes=c(event.lines,phase.bars))
    removeNotification(id="loading")
    plot.ly
  })   ## end time plot
  
  ## box plot prep ##########################################
  output$box_out <- renderPlotly({
    
    showNotification("Loading data...",id="loading",
                     duration=NULL,type="error")
    
    ## make sure we have something
    validate(need(input$scoreSelBox,'Need at least 1 score!!'))
    
    ## get the data and get rid of observations with fake zeros
    plot.df <- getOne()
    plot.df <- plot.df[!(is.na(plot.df$MoodAssessmentStart)),]
    
    
    ## title
    main.title <- paste0(input$subject,"\nLast updated: ",
                         format(last.date,format="%m/%d/%Y"))
    
    
    
    ## box plot #####################################################
    plot.ly <- plot_ly(type="box")
    plot.ly <- config(plot.ly,displayModeBar=FALSE)
    
    plot.ly <- add_trace(
      plot.ly,
      x=~plot.df$fac.var,
      y=plot.df[[input$scoreSelBox[s]]],
      boxpoints=b.points,jitter=jit,boxmean=show.mean,
      fillcolor=c.col,
      marker=list(size=marker.size,color=c.col,
                  line=list(color=c.col,width=line.wd)),
      line=list(color="black",width=line.wd),
      name=s.name,
      hoverinfo=input$box_info,hoveron="boxes+points",
      text=paste0(
        s.name," ",
        round(plot.df[[input$scoreSelBox[s]]],2),"<br>",
        plot.df$InterviewType,"<br>",
        wday(plot.df$DateTime,label=TRUE)," ",
        format(plot.df$DateTime,"%m-%d-%Y")," ",
        format(plot.df$DateTime,"%H:%M"),"<br>",
        plot.df$fac.var)
    )
    
    
    
    ## decorations
    plot.ly <- layout(plot.ly,yaxis=list(range=y.range,title='Score'),
                      xaxis=list(range='',title=''),title=main.title,
                      boxmode="group")
    removeNotification(id="loading")
    plot.ly
    
  })   ## end box plot
  
  ## variable tables #####################
  output$var_tab <- renderTable(colnames=FALSE,{rate.names})
  
  output$bad_vars <- renderTable(colnames=FALSE,{
    
    ## get the text and split the vars
    model.vars <- unlist(tstrsplit(input$model_in,'[*+-/^]'))
    # model.vars <- gsub("[[:space:]]", "",model.vars)
    bad.vars <- setdiff(as.character(model.vars),rate.names)
    bad.vars
  })
  
  output$meta_tab <- renderPrint({
    validate(need(input$model_in,"Please specify model!"))
    model.vars <- unlist(tstrsplit(input$model_in,'[*+-/^]'))
    bad.vars <- model.vars[ ! (model.vars %in% rate.names) ]
    
    if( length(bad.vars) > 0 ){ return("Check variable names!") }
    mod.df <- getOne()
    meta.calc <- try(parse(text=paste0("with(mod.df,",input$model_in,")")),
                     silent=TRUE)
    meta.out <- try(eval(meta.calc),silent=TRUE)
    summary(meta.out)
  })
  
})   ## end server ###########################




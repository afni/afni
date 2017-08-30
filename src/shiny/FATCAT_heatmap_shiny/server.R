#####################################
## 06/08/2017 Justin Rajendra
## 3dNetCorr heatmap
## Server

## need libraries
# library(shiny)
# library(plotly)
# library(data.table)
# library(gplots)

######################################################################################
## code for server for input and output
shinyServer(function(input,output,session) {
  options(warn =-1)
  session$onSessionEnded(stopApp)

  ############################################
  ## get the rois and info

  observe({
    ## get the selected file and info
    num.rois <- as.numeric(strsplit(readLines(input$net_file,n=1),
                                    split=' ')[[1]][2])
    num.mat <- as.numeric(strsplit(readLines(input$net_file,n=2)[2],
                                   split=' ')[[1]][2])

    ## rois for choices
    roi.lab <- as.matrix(read.table(input$net_file,nrows=1,header=FALSE))
    if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }

    stat.list <- c()
    stat.names <- c()
    start.line <- 5
    withProgress(message = 'Finding stat types.', value = 0, {
      for(i in 1:num.mat){

        ## cut out the stat and add to list
        stat.type <- as.character(scan(input$net_file,what='character',n=2,
                                       skip=start.line,nlines=1,
                                       quiet=TRUE)[2][1])
        stat.list <- rbind(stat.list,stat.type[1])

        ## get the pretty name and add to name list
        name.temp <- ifelse(stat.type[1] %in% stat.df$label,
                            stat.df$description[stat.df$label == stat.type[1]],
                            stat.type[1])
        stat.names <- rbind(stat.names,name.temp)

        start.line <- 5 + (num.rois*i+1*i)
        incProgress(1/num.mat,detail=stat.type[[1]])
      }
      names(stat.list) <- stat.names
      print(stat.list)
      updateSelectInput(session,'stat_sel',choices=stat.list)
    })
    ## if there are too many regions...
    updateSelectInput(session,'rois',choices=roi.lab,selected=roi.lab)
  })   ## end observe for rois

  ## threshold
  observe({
    t.min <- round(input$range_min,2)
    t.max <- round(input$range_max,2)
    updateSliderInput(session,'cor_thresh',value=c((t.max+t.min)/2-0.1,
                                                   (t.max+t.min)/2+0.1),
                      min=input$range_min,
                      max=input$range_max)
  })

  ############################################
  ## read matrices
  mat_read <- reactive({
    if(length(input$rois) > 1){
      ## get info
      num.rois <- as.numeric(strsplit(readLines(input$net_file,n=1),split=' ')
                             [[1]][2])
      num.mat <- as.numeric(strsplit(readLines(input$net_file,n=2)[2],split=' ')
                            [[1]][2])
      roi.lab <- as.matrix(read.table(input$net_file,nrows=1,header=FALSE))
      if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }

      ## read in data
      net.mat <- as.matrix(fread(input$net_file,skip=input$stat_sel,
                                 nrows=num.rois,header=FALSE))
      if(is.na(sum(net.mat[,ncol(net.mat)]))){
        net.mat <- net.mat[,1:ncol(net.mat)-1]
      }
      rownames(net.mat) <- colnames(net.mat) <- roi.lab

      data.range <- range(net.mat)
      updateNumericInput(session,'range_min',value=data.range[1])
      updateNumericInput(session,'range_max',value=data.range[2])
      updateNumericInput(session,'thresh_min',
                         value=(data.range[1]+data.range[2])/2-0.1)
      updateNumericInput(session,'thresh_max',
                         value=(data.range[1]+data.range[2])/2+0.1)
      return(net.mat)
    }
  })   ## end mat_read


  ############################################
  heat_data <- reactive({

    if(length(input$rois) < 2){ stop('ERROR: select more than one ROI!') }

    ## get data
    net.mat <- mat_read()
    net.mat <- net.mat[rownames(net.mat) %in% input$rois, ]
    net.mat <- net.mat[, colnames(net.mat) %in% input$rois]

    ## cluster if you want
    if(input$h_clust != 'none'){
      if(input$dist_meth == "canberra" & min(net.mat,na.rm=TRUE) <= 0){
        stop('Canberra does not work with none positive values!')
      }
      row.order <- hclust(dist(net.mat,method=input$dist_meth),
                          method=input$h_clust)$order
      col.order <- hclust(dist(t(net.mat),method=input$dist_meth),
                          method=input$h_clust)$order
      net.mat <- net.mat[row.order, col.order]
    }

    ## threshold and remove zeros
    if(input$thresh_yn){
      net.mat[net.mat > input$thresh_min & net.mat < input$thresh_max] <- NA
    }
    if(input$zero_yn){  net.mat[net.mat == 0] <- NA }

    ## select upper/lower/full
    if(input$tri_sel == 'Upper'){
      net.mat[upper.tri(net.mat,diag=TRUE)] <- NA
    } else if(input$tri_sel == 'Lower'){
      net.mat[lower.tri(net.mat,diag=TRUE)] <- NA
    }
    return(net.mat)
  })   ## end net_data

  ## read mat 2
  heat_data2 <- reactive({

    if(length(input$rois) < 2){ stop('ERROR: select more than one ROI!') }

    ## get data
    net.mat <- mat_read()
    net.mat <- net.mat[rownames(net.mat) %in% input$rois, ]
    net.mat <- net.mat[, colnames(net.mat) %in% input$rois]
    if(length(net.mat) < 2){ stop('select more than one ROI!') }

    ## suppress the stupid extra pdf
    if(length(dev.list()) > 0 ){ graphics.off() }
    pdf(NULL)

    ## cluster if you want
    if(input$h_clust != 'none'){
      if(input$dist_meth == "canberra" & min(net.mat,na.rm=TRUE) <= 0){
        stop('Canberra does not work with none positive values!')
      }
      hm2 <- heatmap.2(net.mat,Rowv=TRUE,Colv='Rowv',
                       trace='none',symm=TRUE,revC=TRUE,
                       distfun=function(c) dist(c,method=input$dist_meth),
                       hclustfun=function(c) hclust(c,method=input$h_clust),
                       breaks=seq(input$range_min,input$range_max,
                                  length.out=101))
    } else {
      hm2 <- heatmap.2(net.mat,Rowv=FALSE,symm=TRUE,revC=FALSE,trace='none',
                       breaks=seq(input$range_min,input$range_max,
                                  length.out=101))
    }  ## end cluster check

    ## threshold and remove zeros
    if(input$thresh_yn){
      hm2$carpet[hm2$carpet > input$thresh_min &
                   hm2$carpet < input$thresh_max] <- NA
    }
    if(input$zero_yn){  hm2$carpet[hm2$carpet == 0] <- NA }

    return(hm2$carpet)

  })   ## end heat_data2
  ############################################
  ## outputs

  ## plot heat map
  output$cor_heatmap_plot <- renderPlotly({

    ## get data and make sure it is enough
    heat.mat <- heat_data2()
    if(length(heat.mat) < 2){ stop('select more than one ROI!') }

    ## suppress the stupid extra pdf
    if(length(dev.list()) > 0 ){ graphics.off() }
    pdf(NULL)

    ## get title checking if clustered
    if(input$h_clust != 'none'){
      if(input$dist_meth == "canberra" & min(heat.mat,na.rm=TRUE) <= 0){
        stop('Canberra does not work with none positive values!')
      }
      plot.title <- paste(basename(input$net_file),
                          stat.df$description[stat.df$label == input$stat_sel],
                          paste0('clust: ',input$h_clust,
                                 ', dist: ',input$dist_meth),
                          sep='\n')
    } else {
      plot.title <- paste(basename(input$net_file),
                          stat.df$description[stat.df$label == input$stat_sel],
                          sep='\n')
    }

    ## threshold and remove zeros
    if(input$thresh_yn){
      heat.mat[heat.mat > input$thresh_min &
                 heat.mat < input$thresh_max] <- NA
    }
    if(input$zero_yn){  heat.mat[heat.mat == 0] <- NA }

    ## select upper/lower/full
    if(input$tri_sel == 'Upper'){
      heat.mat <- heat.mat[,rev(seq.int(ncol(heat.mat)))]
      heat.mat[(upper.tri(heat.mat,diag=TRUE))] <- NA
      heat.mat <- heat.mat[,rev(seq_len(ncol(heat.mat)))]
    } else if(input$tri_sel == 'Lower'){
      heat.mat <- heat.mat[,rev(seq.int(ncol(heat.mat)))]
      heat.mat[(lower.tri(heat.mat,diag=TRUE))] <- NA
      heat.mat <- heat.mat[,rev(seq_len(ncol(heat.mat)))]
    }

    ## make sure something is left
    if(all(is.na(heat.mat))){
      stop('Nothing survives thresholding!')
    }

    ## plot margins and color
    lab.width <- max(strwidth(rownames(heat.mat),'inches')) * 96
    if(lab.width < 125){ lab.width <- 125 }
    m <- list(l=lab.width,r=lab.width,b=lab.width,t=lab.width,pad=4)
    col.ramp <- colorRampPalette(c(input$col1,input$col2,input$col3))

    ## plot
    plot.ly <- plot_ly(x=~colnames(heat.mat),y=~rownames(heat.mat),
                       z=~heat.mat,
                       type="heatmap",colors=col.ramp(100),
                       colorbar=list(title=''),
                       zmin=input$range_min,zmax=input$range_max)
    layout(plot.ly,xaxis=list(title=''),yaxis=list(title=''),margin=m,
           title=plot.title)
  })   ## end heat map

  ############################################
  ## download static version
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(basename(input$net_file)),'_',
                                input$stat_sel,'.png') },

    content = function(file) {

      ## get data and make sure it is enough and select rois
      net.mat <- mat_read()
      net.mat <- net.mat[rownames(net.mat) %in% input$rois, ]
      net.mat <- net.mat[, colnames(net.mat) %in% input$rois]
      if(length(net.mat) < 2){ stop('select more than one ROI!') }

      ## get the margin size from the names
      lab.width <- max(nchar(rownames(net.mat))) / 3
      if(lab.width < 10){ lab.width <- 10 }

      ## suppress the stupid extra pdf
      pdf(NULL)

      ############################################
      ## calculate first

      col.ramp <- colorRampPalette(c(input$col1,input$col2,input$col3))
      stat.lab <- stat.df$description[stat.df$label == input$stat_sel]

      ## cluster if you want
      if(input$h_clust != 'none'){
        if(input$dist_meth == "canberra" & min(net.mat,na.rm=TRUE) <= 0){
          stop('Canberra does not work with none positive values!')
        }
        hm2 <- heatmap.2(net.mat,Rowv=TRUE,Colv='Rowv',
                         breaks=seq(input$range_min,input$range_max,
                                    length.out=101),
                         trace='none',symm=TRUE,revC=TRUE,
                         distfun=function(c) dist(c,method=input$dist_meth),
                         hclustfun=function(c) hclust(c,method=input$h_clust))
        plot.title <- paste(basename(input$net_file),stat.lab,
                            paste0('clust: ',input$h_clust,
                                  ', dist: ',input$dist_meth),
                            sep='\n')
      } else {
        hm2 <- heatmap.2(net.mat,Rowv=FALSE,
                         breaks=seq(input$range_min,input$range_max,
                                    length.out=101),
                         trace='none',symm=TRUE,revC=FALSE)
        plot.title <- paste(basename(input$net_file),stat.lab,sep='\n')
      }

      ## threshold and remove zeros
      if(input$thresh_yn){
        hm2$carpet[hm2$carpet > input$thresh_min &
                     hm2$carpet < input$thresh_max] <- NA
      }
      if(input$zero_yn){  hm2$carpet[hm2$carpet == 0] <- NA }

      ############################################
      ## plot

      ## open graphics device
      graphics.off()
      png(file,width=20,height=20,units='in',res=input$static_dpi)

      ## different orders depending on clustering
      if(input$h_clust != 'none'){
        ## select upper/lower/full
        if(input$tri_sel == 'Upper'){
          hm2$carpet <- hm2$carpet[,rev(seq.int(ncol(hm2$carpet)))]
          hm2$carpet[(upper.tri(hm2$carpet,diag=TRUE))] <- NA
          hm2$carpet <- hm2$carpet[,rev(seq_len(ncol(hm2$carpet)))]
        } else if(input$tri_sel == 'Lower'){
          hm2$carpet <- hm2$carpet[,rev(seq.int(ncol(hm2$carpet)))]
          hm2$carpet[(lower.tri(hm2$carpet,diag=TRUE))] <- NA
          hm2$carpet <- hm2$carpet[,rev(seq_len(ncol(hm2$carpet)))]
        }

        ## hide/show dendro
        if(input$dendro){
          show_dendro <- 'column'
        } else {
          show_dendro <- 'none'
        }
        revRowInd <- match(c(1:length(hm2$rowInd)), hm2$rowInd)
        revColInd <- match(c(1:length(hm2$colInd)), hm2$colInd)
        heatmap.2(hm2$carpet[revRowInd,revColInd],
                  col=col.ramp(100),breaks=seq(input$range_min,input$range_max,
                                               length.out=101),
                  trace='none',symm=TRUE,revC=TRUE,dendrogram=show_dendro,
                  Rowv=hm2$rowDendrogram, Colv=hm2$colDendrogram,
                  key.title=NA,key.xlab=stat.lab,keysize=0.5,
                  density.info='none',
                  key.par=list(mar=c(4,2,4,2)),
                  main=plot.title,
                  margins=c(lab.width,lab.width),
                  sepcolor="lightgrey",sepwidth=c(0.01,0.01),
                  colsep=seq(1,ncol(hm2$carpet),2),
                  rowsep=seq(1,nrow(hm2$carpet),2))
      } else {
        ## no clustering
        if(input$tri_sel == 'Lower'){
          hm2$carpet <- hm2$carpet[,rev(seq.int(ncol(hm2$carpet)))]
          hm2$carpet[(upper.tri(hm2$carpet,diag=TRUE))] <- NA
          hm2$carpet <- hm2$carpet[,rev(seq_len(ncol(hm2$carpet)))]
        } else if(input$tri_sel == 'Upper'){
          hm2$carpet <- hm2$carpet[,rev(seq.int(ncol(hm2$carpet)))]
          hm2$carpet[(lower.tri(hm2$carpet,diag=TRUE))] <- NA
          hm2$carpet <- hm2$carpet[,rev(seq_len(ncol(hm2$carpet)))]
        }

        revRowInd <- match(c(1:length(hm2$rowInd)), hm2$rowInd)
        revColInd <- match(c(1:length(hm2$colInd)), hm2$colInd)

        heatmap.2(t(hm2$carpet[revRowInd, revColInd]),
                  col=col.ramp(100),breaks=seq(input$range_min,input$range_max,
                                               length.out=101),
                  trace='none',symm=TRUE,revC=FALSE,Rowv=FALSE,
                  key.title=NA,key.xlab=stat.lab,keysize=0.5,
                  density.info='none',
                  key.par=list(mar=c(4,2,4,2)),
                  main=plot.title,
                  margins=c(lab.width,lab.width),
                  sepcolor="lightgrey",sepwidth=c(0.01,0.01),
                  colsep=seq(1,ncol(hm2$carpet),2),
                  rowsep=seq(1,nrow(hm2$carpet),2))
      }  ## end cluster check

      dev.off()
    })   ## end static heatmap

  ############################################
  ## plot histogram
  output$cor_hist_plot <- renderPlotly({
    ## get data and make sure it is enough
    cor.in <- heat_data2()
    if(length(cor.in) < 2){ stop('select more than one ROI!') }

    ## select upper/lower/full
    if(input$tri_sel == 'Upper'){
      cor.in <- cor.in[,rev(seq.int(ncol(cor.in)))]
      cor.in[(upper.tri(cor.in,diag=TRUE))] <- NA
      cor.in <- cor.in[,rev(seq_len(ncol(cor.in)))]
    } else if(input$tri_sel == 'Lower'){
      cor.in <- cor.in[,rev(seq.int(ncol(cor.in)))]
      cor.in[(lower.tri(cor.in,diag=TRUE))] <- NA
      cor.in <- cor.in[,rev(seq_len(ncol(cor.in)))]
    }

    ## as vector
    cor.in <- c(cor.in)

    ## plot margins and color
    stat.lab <- stat.df$description[stat.df$label == input$stat_sel]
    m <- list(l=100,r=100,b=100,t=125,pad=4)

    ## plot
    plot.ly <- plot_ly(x=~cor.in,type="histogram",
                       marker=list(color='lightblue',
                                   line=list(width=1,color='darkblue')))
    layout(plot.ly,yaxis=list(title='Count'),xaxis=list(title=stat.lab),
           title=paste(basename(input$net_file),stat.lab,sep='\n'),
           margin=m)
  })   ## end histogram

  ############################################
  ## help links
  output$hclust_link <- renderUI({
    tags$a(target='_blank',
           href='https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html',
           'Cluster descriptions link')
  })
  output$dist_link <- renderUI({
    tags$a(target='_blank',
           href='https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html',
           'Distance descriptions link')
  })

})   ## end server

#####################################
## 11/2017 Justin Rajendra
## FATCAT matrix plot
## Server

###########################################################################
## misc functions

## get number of ROIs and matrices and the ROI labels
mat_info_fun <- function(in.file){
  num.rois <- as.numeric(strsplit(readLines(in.file,n=1),
                                  split=' ')[[1]][2])
  num.mat <- as.numeric(strsplit(readLines(in.file,n=2)[2],
                                 split=' ')[[1]][2])
  
  ## rois labels or add "roi" to just numeric ones
  roi.lab <- as.matrix(read.table(in.file,nrows=1,header=FALSE))
  if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }
  
  return(list(num.rois=num.rois,num.mat=num.mat,roi.lab=roi.lab))
}   ## end mat_info_fun

###########################################################################
## code for server for input and output
shinyServer(function(input,output,session) {
  
  ## hide some warnings and quit app when browser/tab closes
  options(warn =-1)
  session$onSessionEnded(stopApp)
  
  ############################################
  ## get the rois and info
  observe({
    
    ## see if it is a .grid or .netcc file
    if( ! file_ext(input$net_file) %in% c('grid','netcc')){
      
      ## get roi labels and prepend 'roi_' if only numbers
      roi.lab <- colnames(data.frame(fread(input$net_file,header=TRUE),
                                     row.names=1,check.names=FALSE))
      roi.lab <- type.convert(roi.lab,as.is=TRUE)
      if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }
      
      ## no stat because we don't know what it is
      stat.list <- 'none'
      
    } else { 
      
      ## get the selected file and info
      mat.info <- mat_info_fun(input$net_file)
      num.rois <- mat.info$num.rois
      num.mat <- mat.info$num.mat
      roi.lab <- mat.info$roi.lab
      
      ## build the stat list
      stat.list <- c() ; stat.names <- c() ; start.line <- 5
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
      })   ## end progress
      
    }
    ## update stats and rois
    updateSelectInput(session,'stat_sel',choices=stat.list)
    updateSelectInput(session,'rois',choices=roi.lab,selected=roi.lab)
  })   ## end observe for rois
  
  ############################################
  ## color min/max reset
  observeEvent(input$col_thresh,{
    if(length(input$rois) > 1){
      if(input$col_thresh == 'No'){
        data.range <- range(mat_read())
        updateNumericInput(session,'range_min',value=data.range[1])
        updateNumericInput(session,'range_max',value=data.range[2])
      }
    }
  })
  
  ############################################
  ## read matrices
  mat_read <- reactive({
    if(length(input$rois) > 1){
      
      ## change read in based on file extension
      if( ! file_ext(input$net_file) %in% c('grid','netcc')){
        net.mat <- as.matrix(data.frame(fread(input$net_file,header=TRUE),
                                        row.names=1,check.names=FALSE))
        
        ## fix labels if they are all numeric
        roi.lab <- colnames(net.mat)
        roi.lab <- type.convert(roi.lab,as.is=TRUE)
        if(is.numeric(roi.lab)){ roi.lab <- paste0('roi_',roi.lab) }
        rownames(net.mat) <- colnames(net.mat) <- roi.lab
        
      } else { 
        ## get the selected file and info
        mat.info <- mat_info_fun(input$net_file)
        num.rois <- mat.info$num.rois
        num.mat <- mat.info$num.mat
        roi.lab <- mat.info$roi.lab
        
        ## read in data (get rid of empty column?)
        net.mat <- as.matrix(fread(input$net_file,skip=input$stat_sel,
                                   nrows=num.rois,header=FALSE))
        if(is.na(sum(net.mat[,ncol(net.mat)]))){
          net.mat <- net.mat[,1:ncol(net.mat)-1]
        }
        rownames(net.mat) <- colnames(net.mat) <- roi.lab
      }
      
      ## get range and update choices
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
  ## get the data reorganized with the clustering
  heat_data <- reactive({
    
    if(length(input$rois) < 2){ stop('ERROR: select more than one ROI!') }
    
    ## get the data
    net.mat <- mat_read()
    net.mat <- net.mat[rownames(net.mat) %in% input$rois, ]
    net.mat <- net.mat[, colnames(net.mat) %in% input$rois]
    
    if(length(net.mat) < 2){ stop('ERROR: select more than one ROI!') }
    
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
  })   ## end heat_data
  
  ############################################
  ## plot heat map
  output$cor_heatmap_plot <- renderPlotly({
    
    ## get data and make sure it is enough
    heat.mat <- heat_data()
    if(length(heat.mat) < 2){ stop('select more than one ROI!') }
    
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
                                 ', dist: ',input$dist_meth),sep='\n')
    } else {
      plot.title <- paste(basename(input$net_file),
                          stat.df$description[stat.df$label == input$stat_sel],
                          sep='\n')
    }
    
    ## make sure something is left
    if(all(is.na(heat.mat))){ stop('Nothing survives thresholding!') }
    
    ## plot margins and color
    lab.width <- max(strwidth(rownames(heat.mat),'inches')) * 96
    if(lab.width < 125){ lab.width <- 125 }
    m <- list(l=lab.width,r=lab.width,b=lab.width,t=lab.width,pad=4)
    if(input$col_2_3 == 2){
      col.ramp <- colorRampPalette(c(input$col1,input$col3),alpha=FALSE)
    } else {
      col.ramp <- colorRampPalette(c(input$col1,input$col2,input$col3),
                                   alpha=FALSE)
    }
    
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
    filename = function(){
      paste0(basename(input$net_file),'_',input$stat_sel,'_heatmap.png')
    },
    content = function(file) {
      ## get data and make sure it is enough and select rois
      net.mat <- mat_read()
      net.mat <- net.mat[rownames(net.mat) %in% input$rois, ]
      net.mat <- net.mat[, colnames(net.mat) %in% input$rois]
      if(length(net.mat) < 2){ 
        showNotification("select more than one ROI!",type="error",duration=3)
        stop('select more than one ROI!') 
      }
      
      ## get the margin size from the names
      lab.width <- max(nchar(rownames(net.mat))) / 3
      if(lab.width < 10){ lab.width <- 10 }
      
      ## suppress the stupid extra pdf
      pdf(NULL)
      
      ## colors and labels
      if(input$col_2_3 == 2){
        col.ramp <- colorRampPalette(c(input$col1,input$col3),alpha=FALSE)
      } else {
        col.ramp <- colorRampPalette(c(input$col1,input$col2,input$col3),
                                     alpha=FALSE)
      }
      stat.lab <- stat.df$description[stat.df$label == input$stat_sel]
      
      ############################################
      ## calculate first
      if(input$h_clust != 'none'){
        if(input$dist_meth == "canberra" & min(net.mat,na.rm=TRUE) <= 0){
          showNotification("Canberra does not work with none positive values!",
                           type="error",duration=3)
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
                                   ', dist: ',input$dist_meth),sep='\n')
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
      ## open graphics device (close any old ones just in case)
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
    cor.in <- heat_data()
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
  ## circos
  output$downloadCircos <- downloadHandler(
    filename = function(){
      paste0(basename(input$net_file),'_',input$stat_sel,'_circos.png') 
    },
    content = function(file){
      
      ## read in data, check it and mirror it
      net.mat <- heat_data()
      if(length(net.mat) < 2){ stop('select more than one ROI!') }
      net.mat <- net.mat[,c(nrow(net.mat):1),drop=FALSE] 
      
      ## get all of the roi labels
      roi.lab <- t(rownames(net.mat))
      
      ## show the wait
      showNotification("Creating Circos PNG.",type="error",
                       id="circos",duration=0)
      
      ## get the matrix as a dataframe long with the correct format
      net.upper <- upper.tri(net.mat,diag=FALSE)
      map.df <- data.frame(seg1=rownames(net.mat)[row(net.mat)[net.upper]],
                           start1=0,end1=1,
                           seg2=rownames(net.mat)[col(net.mat)[net.upper]],
                           start2=0,end2=1,
                           value=(net.mat)[net.upper])
      
      ## get the max number of connections for any region
      max.con <- max(by(map.df$value,map.df$seg1,
                        function(x) length(which(!is.na(x)))))
      
      ## remove NA rows and reorder by region and get rid of empty levels
      map.df <- map.df[complete.cases(map.df),]
      map.df <- map.df[order(map.df$seg1,map.df$seg2),]
      map.df$seg1 <- factor(map.df$seg1)
      map.df$seg2 <- factor(map.df$seg2)
      
      ## get the max number of connections for any region
      map.long <- c(as.character(map.df$seg1),as.character(map.df$seg2))
      map.long <- factor(map.long)
      max.con <- max(tapply(map.long,map.long,length))
      
      ## make an indexer to keep track of mapping numbers
      map.ind <- data.frame(roi=t(roi.lab),ind=0)
      
      ## make the connection mapping by the number of connections
      if(max.con > 1){
        for(i in 1:nrow(map.df)){
          ## seg1 start
          start1 <- map.ind$ind[map.ind$roi == as.character(map.df$seg1[i])]
          map.ind$ind[map.ind$roi == as.character(map.df$seg1[i])] <- start1+1
          
          ## seg2 start
          start2 <- map.ind$ind[map.ind$roi == as.character(map.df$seg2[i])]
          map.ind$ind[map.ind$roi == as.character(map.df$seg2[i])] <- start2+1
          
          ## put the connection in
          map.df$start1[i] <- start1
          map.df$end1[i] <- start1 + 1
          map.df$start2[i] <- start2
          map.df$end2[i] <- start2 + 1
        }   ## end row loop
      }
      
      ## make segments for rois 
      roi.seg <- data.frame(seg.name=NULL,seg.Start=NULL,
                            seg.End=NULL,the.v=NULL,NO=NULL)
      for(i in 1:length(roi.lab)){
        temp.seg <- data.frame(seg.name=rep(roi.lab[i],max.con),
                               seg.Start=c(0:(max.con-1)),seg.End=c(1:max.con),
                               the.v=NA,NO=NA)
        roi.seg <- rbind(roi.seg,temp.seg)
      }
      ## get the angles from the segments
      roi.seg.cust <- segAnglePo(roi.seg,seg=roi.lab)
      
      ## colors and labels
      if(input$col_2_3 == 2){
        col.ramp <- colorRampPalette(c(input$col1,input$col3),alpha=TRUE)
      } else {
        col.ramp <- colorRampPalette(c(input$col1,input$col2,input$col3),
                                     alpha=TRUE)
      }
      color_fun <- function(x,n=10000){ col.ramp(n)[cut(x,n)] }
      stat.lab <- stat.df$description[stat.df$label == input$stat_sel]
      plot.title <- paste(basename(input$net_file),stat.lab,sep='\n')
      
      ## everything relative to the image dimensions
      circ.center <- input$circos_dim*100/2
      
      graphics.off()
      png(file,width=input$circos_dim,height=input$circos_dim,
          units='in',res=input$circos_dpi)
      par(mar=c(2,2,2,2))
      
      ## empty plot
      plot(c(1,input$circos_dim*100),c(1,input$circos_dim*100),type="n",
           axes=FALSE,xlab="",ylab="",main=plot.title)
      
      ## segments with labels
      circos(R=circ.center*0.7,xc=circ.center,yc=circ.center,cir=roi.seg.cust,
             type="chr",print.chr.lab=TRUE,W=10,cex=1,col='black')
      ## connections
      circos(R=circ.center*0.68,xc=circ.center,yc=circ.center,cir=roi.seg.cust,
             W=40,mapping=map.df,type="link.pg",lwd=1,
             col=color_fun(map.df$value))
      dev.off()
      
      removeNotification(id="circos")
      
    })   ## end circos
  
  ############################################
  ## log file download
  output$downloadLog <- downloadHandler(
    filename = function(){
      paste0(basename(input$net_file),'_',input$stat_sel,'_log.csv')
    },
    content = function(file){
      
      ## read in data, check it and mirror it
      heat.mat <- heat_data()
      if(length(heat.mat) < 2){ stop('select more than one ROI!') }
      
      ## mirror then reverse rows
      heat.mat <- heat.mat[,c(nrow(heat.mat):1),drop=FALSE]
      heat.mat <- heat.mat[nrow(heat.mat):1,]
      
      ## select upper/lower/full (reversed to match above)
      if(input$tri_sel == 'Lower'){
        heat.mat <- heat.mat[,rev(seq.int(ncol(heat.mat)))]
        heat.mat[(upper.tri(heat.mat,diag=TRUE))] <- NA
        heat.mat <- heat.mat[,rev(seq_len(ncol(heat.mat)))]
      } else if(input$tri_sel == 'Upper'){
        heat.mat <- heat.mat[,rev(seq.int(ncol(heat.mat)))]
        heat.mat[(lower.tri(heat.mat,diag=TRUE))] <- NA
        heat.mat <- heat.mat[,rev(seq_len(ncol(heat.mat)))]
      }
      
      ## un-mirror and add labels as the first column
      heat.mat <- heat.mat[,c(nrow(heat.mat):1),drop=FALSE]
      out.mat <- data.frame(Label=row.names(heat.mat))
      out.mat <- cbind(out.mat,heat.mat)
      
      ## paste as comma separated strings per row
      out.data <- apply(out.mat,1,paste,collapse=",")
      
      ## main header description
      header.lab <- paste0("FileName,Stat,LowPass,HighPass,Min,Max,Matrix,",
                           "RemoveZero,ClusterLinkage,DistanceMethod,",
                           "ColorLow,ColorMid,ColorHigh")
      
      ## some checks
      if(input$thresh_yn){
        t.min <- input$thresh_min
        t.max <- input$thresh_max
      } else {
        t.min <- t.max <- NA
      }
      if(input$h_clust == 'none'){ 
        dist.meth <- "none" 
      } else {
        dist.meth <- input$dist_meth
      }
      if(input$col_2_3 == 2){ 
        col.2 <- "none" 
      } else {
        col.2 <- input$col2 
      }
      
      header.info <- paste(c(input$net_file,input$stat_sel,t.min,t.max,
                             input$range_min,input$range_max,input$tri_sel,
                             input$zero_yn,input$h_clust,dist.meth,
                             input$col1,col.2,input$col3),
                           collapse=",")
      
      ## open file and write out
      datafile <- file(file, open = 'wt')
      writeLines(header.lab,con=datafile)
      writeLines(header.info,con=datafile)
      writeLines(" ",con=datafile)
      writeLines(paste(t(colnames(out.mat)),collapse=","),con=datafile)
      writeLines(out.data,con=datafile)
      close(datafile)
    })
  
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

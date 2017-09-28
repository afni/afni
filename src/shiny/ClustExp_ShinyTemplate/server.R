#####################################
## 09/2017 Justin Rajendra
## Cluster Explorer
## Server


######################################################################################
## code for server for input and output
shinyServer(function(input,output,session) {

  options(warn = -1)
  session$onSessionEnded(function() {
    afni_close()
    cat('\nAll done!\n')
    stopApp
  })

  ############################################
  ## change the cluster list
  observe({
    if(input$mean_peak == 'Mean'){
      updateSelectInput(session,"clusters",choices=clust.cm.list)
    } else if(input$mean_peak == 'Peak'){
      updateSelectInput(session,"clusters",choices=clust.pk.list)
    }
  })   ## end update cluster list

  ############################################
  ## change the color list
  observe({
    if(SI.df$test == 'Ttest'){
      col.list <-  c('Red/Blue'='RedBlue','Dark2','Set1','Set2','Set3',
                     'Accent','Paired','Pastel1','Pastel2')
      updateSelectInput(session,"col_pal",choices=col.list)
    }
  })   ## end update color list

  ############################################
  ## set the slider range
  observe({
    if(input$mean_peak == 'Mean'){
      val.range <- range(mean.df$value,na.rm=TRUE)
    } else if(input$mean_peak == 'Peak'){
      val.range <- range(peak.df$value,na.rm=TRUE)
    }
    range.diff <- (val.range[2]-val.range[1])*0.1
    set.range <- c(val.range[1]-range.diff,val.range[2]+range.diff)
    range.add <- (val.range[2]-val.range[1])*1.25
    max.range <- c(val.range[1]-range.add,val.range[2]+range.add)
    updateSliderInput(session,"custom_range",min=max.range[1],max=max.range[2],
                      value=set.range)
  })   ## end update cluster list

  ###########################################
  # change the coordinate location of selected cluster
  observeEvent(input$clusters,{

    ## xyz for drive afni and data and name
    if(input$mean_peak == 'Mean' & !is.null(input$clusters)){
      clust.x <- clust_all.df$x_cm[clust_all.df$x_y_z_cm == input$clusters]
      clust.y <- clust_all.df$y_cm[clust_all.df$x_y_z_cm == input$clusters]
      clust.z <- clust_all.df$z_cm[clust_all.df$x_y_z_cm == input$clusters]
      temp.name <- input$clusters
      clust.name <- names(clust.cm.list[clust.cm.list == temp.name])
    } else if(input$mean_peak == 'Peak' & !is.null(input$clusters)){
      clust.x <- clust_all.df$x_peak[clust_all.df$x_y_z_peak == input$clusters]
      clust.y <- clust_all.df$y_peak[clust_all.df$x_y_z_peak == input$clusters]
      clust.z <- clust_all.df$z_peak[clust_all.df$x_y_z_peak == input$clusters]
      temp.name <- input$clusters
      clust.name <- names(clust.pk.list[clust.pk.list == temp.name])
    }
    ## go to the xyz
    afni_move(clust.x,clust.y,clust.z)

  })   ## end go to cluster

  ###########################################
  clustPlot <- reactive({
    clust.in <- clustData()
    if(exists("clust.in")){
      stat_plot_fun(clust.in[["clust.df"]],prefix,p_val,SI.df$test,
                    clust.in[["clust.name"]],input$var_sel,
                    clust.in[["vox.vol"]],input$fixed_range,
                    input$custom_range,input$qVars_sel,input$wsVars_sel,
                    input$OverPlot,input$col_pal,input$marker_size,
                    input$marker_opacity,input$box_points,input$line_w,
                    input$box_mean,input$split_bs_ws,input$qVars_center,
                    input$box_scatter,input$jit_sel,input$error_y_sel)
    }
  })   ## end clust_plot

  ###########################################
  clustStat <- reactive({
    clust.in <- clustData()
    stat_fun(clust.in[["clust.df"]],prefix,p_val,SI.df$test,SI.df$paired,
             clust.in[["clust.name"]],input$var_sel,
             clust.in[["vox.vol"]],input$qVars_sel,input$wsVars_sel,
             input$orig_stat,input$split_bs_ws,input$box_scatter)
  })   ## end clustStat

  ###########################################
  clustDesc <- reactive({
    clust.in <- clustData()
    stat_desc(clust.in[["clust.df"]],prefix,p_val,SI.df$test,
              clust.in[["clust.name"]],input$var_sel,
              clust.in[["vox.vol"]],input$fixed_range,
              input$custom_range,input$qVars_sel,
              input$OverPlot,input$col_pal,input$marker_size)
  })   ## end clustStat

  ############################################
  ## load the selected cluster data
  clustData <- reactive({

    ## make sure something is selected
    if(!is.null(input$clusters)){

      ## xyz for drive afni and data and name
      if(input$mean_peak == 'Mean'){
        clust.x <- clust_all.df$x_cm[clust_all.df$x_y_z_cm == input$clusters]
        clust.y <- clust_all.df$y_cm[clust_all.df$x_y_z_cm == input$clusters]
        clust.z <- clust_all.df$z_cm[clust_all.df$x_y_z_cm == input$clusters]
        vox.vol <- clust_all.df$Voxels[clust_all.df$x_y_z_cm == input$clusters]
        clust.df <- subset(mean.df,mean.df$coord == input$clusters)
        temp.name <- input$clusters
        clust.name <- names(clust.cm.list[clust.cm.list == temp.name])
      } else if(input$mean_peak == 'Peak'){
        clust.x <- clust_all.df$x_pk[clust_all.df$x_y_z_peak == input$clusters]
        clust.y <- clust_all.df$y_pk[clust_all.df$x_y_z_peak == input$clusters]
        clust.z <- clust_all.df$z_pk[clust_all.df$x_y_z_peak == input$clusters]
        vox.vol <- clust_all.df$Voxels[clust_all.df$x_y_z_peak == input$clusters]
        clust.df <- subset(peak.df,peak.df$coord == input$clusters)
        temp.name <- input$clusters
        clust.name <- names(clust.pk.list[clust.pk.list == temp.name])
      }
      clust.out <- list(clust.df=clust.df,x=clust.x,y=clust.y,z=clust.z,
                        clust.name=clust.name,vox.vol=vox.vol)
      return(clust.out)
    }   ## end input$clusters check
  })   ## end clustData reactive

  ############################################
  ## outputs

  DT.options <- list(paging=FALSE,info=FALSE,searching=FALSE)
  DT.options2 <- list(paging=TRUE,info=FALSE,searching=FALSE,scrollX=TRUE,
                      pageLength=25)

  output$clust_plot_orig <- renderPlotly({clustPlot()})
  output$clust_stat_orig <- reactivePrint(function(){clustStat()})
  output$clust_stat_desc <- renderDataTable({clustDesc()},options=DT.options)
  output$stat_info_table <- renderTable({t(SI.df)},rownames=TRUE,colnames=FALSE)

  output$data_table <- renderDataTable(options=DT.options2,{
    clust.in <- clustData()
    return(clust.in[["clust.df"]])
  })
  output$summary_table <- renderPrint({
    clust.in <- clustData()
    clust.df <- clust.in[["clust.df"]]
    summary(clust.df)
  })

  ############################################
  ## make or update some UI
  output$qVars_input <- renderUI({
    if(!is.na(qVars)){
      selectInput('qVars_sel','Quantitative variable:',qVars)
    } })
  if(any(qVars %in% mvm.vars) & !is.na(mvm.vars) & !is.na(qVars)){
    updateRadioButtons(session,'box_scatter',inline=TRUE,
                       choices=c('Box','Interaction','Scatter'),
                       selected='Scatter')
  }
  if(SI.df$test == "Ttest"){
    updateRadioButtons(session,'box_scatter',inline=TRUE,
                       choices='Box',selected='Box')
    updateRadioButtons(session,'split_bs_ws','Factor by:',
                       c('None'),inline=TRUE)
    updateRadioButtons(session,'orig_stat','Which model for summary?',
                       c('Original Model'='orig'),inline=TRUE)
  }
  output$wsVar_input <- renderUI({
    if(!is.na(wsVars)){
      selectInput('wsVars_sel','Within subject factor:',wsVars,multiple=TRUE,
                  selected=wsVars[1])
    } })

  if(SI.df$test != "Ttest"){
    if(!("None" %in% catVars) & !is.na(wsVars)){
      updateRadioButtons(session,'split_bs_ws','Factor by:',
                         c('Between','Within','Both'),inline=TRUE)
    } else if(!("None" %in% catVars) & is.na(wsVars)){
      updateRadioButtons(session,'split_bs_ws','Factor by:',
                         c('Between'),inline=TRUE)
    } else if("None" %in% catVars & !is.na(wsVars)){
      updateRadioButtons(session,'split_bs_ws','Factor by:',
                         c('Within'),inline=TRUE)
    }
    if(is.na(wsVars) & "None" %in% catVars){
      updateRadioButtons(session,'box_scatter',inline=TRUE,
                         choices=c('Scatter'),selected='Scatter')
    }
  }

})   ## end server

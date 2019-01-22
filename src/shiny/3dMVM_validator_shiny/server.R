#####################################
## 11/2017 Justin Rajendra
## 3dMVM validator
## Server

## persistent variables to go between reactive functions
coord.mat <- matrix(nrow=3,ncol=3)
aov.obj <- c()
model.static <- "There is yet to be a model."
glt.good <- FALSE

###########################################################
## code for server for input and output
shinyServer(function(input,output,session) {

  options(warn = -1)
  session$onSessionEnded(function() {
    unlink(out.dir,recursive=TRUE)
    afni_close()
    cat('\nAll done!\n')
    stopApp
  })

  ## start up
  showNotification("Choose a coordinate to get started.",type="message",
                   id="onLoad",duration=0)

  ############################################
  ## get coordinates and change table
  observeEvent(input$get_coord,{
    coord.mat <<- get_ijk_xyz()
    output$coord_tab <- renderTable({coord.mat},colnames=FALSE,rownames=TRUE)
    removeNotification(id="onLoad")
    showNotification("Coordinate chosen. Now extract data.",
                     type="message",duration=0,id="coordDone")
  })

  ##########################
  ## make ROI mask
  observeEvent(input$make_roi,{
    if(!is.na(coord.mat)){
      removeNotification(id="coordDone")
      afni_roi(master.dset,input$srad_num,coord.mat[3,1],
               coord.mat[3,2],coord.mat[3,3])
      afni_add_mask(coord.mat[3,1],coord.mat[3,2],coord.mat[3,3],input$srad_num)
      showNotification("ROI set. Now extract data.",
                       type="message",duration=0,id="maskDone")
    } else {
      showNotification("Get a coordinate first!!",type="error",duration=5)
    }
  })   ## end make mask

  ## remove ROI mask
  observeEvent(input$vox_roi,{
    if(input$vox_roi == 'Voxel'){ afni_hide_OLAY() }
  })

  ##########################
  ## extract the data
  observeEvent(input$extract_data,{
    if(!is.na(coord.mat)){
      removeNotification(id="coordDone")
      removeNotification(id="maskDone")

      ## generate names
      vox.name.list <- vox.file.name(input$vox_roi,out.dir,coord.mat,
                                     input$srad_num)
      vox.file <- vox.name.list$vox.file

      ## extract if not there already
      if(!file.exists(vox.file)){
        showNotification(paste("Wait until the terminal window reports 'DONE!'",
                               "before continuing."),
                         type="error",duration=NULL,id="extract")
        voxel_extract(InputFile.str,coord.mat[3,1],coord.mat[3,2],
                      coord.mat[3,3],vox.file,input$vox_roi,input$srad_num)
        removeNotification(id="extract")
        showNotification("You can now load the data.",type="message",
                         duration=0,id="extractDone")
      } else {
        showNotification("Voxel already extracted. You can now load the data.",
                         type="message")
      }
    } else {
      showNotification("Get a coordinate first!!",type="error",duration=5)
    }
  })  ## end extract data event

  ##########################
  ## voxel extraction data load
  data_load <- eventReactive(c(input$load_data,input$qnt_vars_in),{
    removeNotification(id="extractDone")

    ## generate names
    vox.name.list <- vox.file.name(input$vox_roi,out.dir,coord.mat,
                                   input$srad_num)

    ## no data?
    if(!file.exists(vox.name.list$vox.file)){
      return(cat(paste("No data yet.")))
    }

    ## get data
    vox.df <- read.table(vox.name.list$vox.file)
    names(vox.df) <- vox.name.list$vox.coord
    vox.df <- cbind(vox.df,data.df)

    ## change non specified qVars to factors
    vox.df <- num2fact_fun(input$qnt_vars_in,qntVar,vox.df)

    showNotification("Data Loaded! Specify a model.",type="message",
                     duration=8,id="loadDone")
    return(vox.df)
  })   ## end load data

  ############################################
  ## if they type something wrong, report it
  badVars <- reactive({
    ## get the text and split the vars
    model.vars <- unlist(tstrsplit(input$model_in,'[*+-/]'))

    ## same for the within subject if there
    if(input$ws_vars_in != ""){
      ws.vars <- unlist(tstrsplit(input$ws_vars_in,'[*+-/]'))
      model.vars <- c(model.vars,ws.vars)
    }

    ## get the ones not listed in table
    missing.vars <- setdiff(model.vars,allVars)

    ## check quantitative are numeric
    if(input$qnt_vars_in != ""){
      qnt.vars <- unlist(tstrsplit(input$qnt_vars_in,','))
      missing.qntVars <- setdiff(qnt.vars,qntVar)
      if(length(missing.qntVars) > 0){
        missing.qntVars <- paste(missing.qntVars,'is non-numeric.')
        missing.vars <- c(missing.vars,missing.qntVars)
      }
    }
    ## return the bad ones
    return(missing.vars)
  })   ## end badVars

  ############################################
  ## do the actual anova
  calcModel <- reactive({
    removeNotification(id="loadDone")
    ## load data and check for bad variables
    vox.df <- data_load()
    stopVars <- badVars()

    ## no data?
    if(is.null(vox.df)){ return(cat("")) }

    ## generate names
    vox.name.list <- vox.file.name(input$vox_roi,out.dir,coord.mat,
                                   input$srad_num)
    vox.file <- vox.name.list$vox.file
    vox.coord <- vox.name.list$vox.coord

    ## make sure there is a model and no bad variables
    if(input$model_in == "" | length(stopVars) > 0){
      aov.obj <<- c()
      model.static <<- "Enter a valid model."
      return(cat("Enter a valid model."))
    }
    ## make sure they finished typing something
    last.chr <- substr(as.character(input$model_in),nchar(input$model_in),
                       nchar(input$model_in))
    if(last.chr %in% c("*","+","-","/")){
      aov.obj <<- c()
      model.static <<- "Enter a valid model."
      return(cat("Enter a valid model."))
    }

    if(! (vox.coord %in% names(vox.df))){   ## same voxel
      ## extract if not there already
      if(file.exists(vox.file)){
        return(cat(
          paste("Load the data again for your new voxel coordinates.")))
      } else {
        return(cat(
          paste("Extract the voxel for your new voxel coordinates.")))
      }
    }

    ############################################
    ## do the work

    ## rename the variable for negative doesn't work
    names(vox.df)[names(vox.df)==vox.coord] <- 'value'

    ## check for qVars and center them to mean or specified
    if(input$qnt_vars_in != ""){
      qnt_var_sel <- unlist(tstrsplit(input$qnt_vars_in,','))

      if(input$qnt_vars_center != ""){
        qnt_var_cent_sel <- as.numeric(
          unlist(tstrsplit(input$qnt_vars_center,',')))
        if(length(qnt_var_cent_sel) != length(qnt_var_sel)){
          return(cat(
            paste("Number of qVars and centers do not match!")))
        }
        vox.df <- qVar_center_fun(qnt_var_sel,qnt_var_cent_sel,vox.df)
        qVar.cnt.str <- paste(input$qnt_vars_in,"centered on",
                              input$qnt_vars_center,'\n\n')
      } else {   ## center by mean
        vox.df <- qVar_center_fun(qnt_var_sel,NULL,vox.df)
        qVar.cnt.str <- paste(input$qnt_vars_in,"centered on mean\n\n")
      }
    }   ## end qntVars check

    ## make model from custom function, within subject.
    if(input$ws_vars_in != ""){
      model.sel <- ModelMaker("value",input$model_in,input$ws_vars_in)
    } else {   ## no wsVars
      model.sel <- ModelMaker("value",input$model_in,NA)
    }

    ## print stuff
    cat(paste("RAI =",coord.mat[1,1],coord.mat[1,2],coord.mat[1,3],'\n'))
    if(input$qnt_vars_in != ""){ cat(qVar.cnt.str) }
    cat(paste0('Full Model: ',model.sel,'\n\n'))

    ## run and return stat
    anova.aov <- tryCatch(
      aov_car(as.formula(model.sel),vox.df,factorize=FALSE,
              type=input$SS_type),error=function(e) NA)

    if(!is.na(anova.aov)){
      aov.obj <<- anova.aov ## update global vars
      model.static <<- model.sel
      return(anova(anova.aov))
    } else {
      model.static <<- "Model Fail! Try again..."
      return(cat("Model Fail! Try again..."))
    }

  })   ## end calc model

  ############################################
  ## mvm script output
  mvmScript <- reactive({

    ## line by line the script
    mvm.scr <- '#!/bin/tcsh'
    mvm.scr <- paste(mvm.scr,' ',sep="\n")
    mvm.scr <- paste(mvm.scr,'################################',sep="\n")
    mvm.scr <- paste(mvm.scr,paste("##",Sys.Date()),sep="\n")
    mvm.scr <- paste(mvm.scr,"## Created with 3dMVM_validator",sep="\n")
    mvm.scr <- paste(mvm.scr,paste("## from:",table.file),sep="\n")
    mvm.scr <- paste(mvm.scr,' ',sep="\n")
    mvm.scr <- add.line(mvm.scr,'3dMVM')
    mvm.scr <- add.line(mvm.scr,paste('-jobs',input$n.jobs))
    if(input$mvm.prefix != ""){
      mvm.scr <- add.line(mvm.scr,paste('-prefix',input$mvm.prefix))
    }
    if(input$mvm.mask != ""){
      mvm.scr <- add.line(mvm.scr,paste('-mask',input$mvm.mask))
    }
    mvm.scr <- add.line(mvm.scr,paste('-SS_type',input$SS_type))
    if(input$GES){ mvm.scr <- add.line(mvm.scr,'-GES') }
    if(input$SC){ mvm.scr <- add.line(mvm.scr,'-SC') }
    if(input$robust){ mvm.scr <- add.line(mvm.scr,'-robust') }
    if(input$wsE2){ mvm.scr <- add.line(mvm.scr,'-wsE2') }
    if(input$wsMVT){ mvm.scr <- add.line(mvm.scr,'-wsMVT') }
    if(input$dbgArgs){ mvm.scr <- add.line(mvm.scr,'-dbgArgs') }

    mvm.scr <- add.line(mvm.scr,paste0('-bsVars "',input$model_in,'"'))
    if(input$ws_vars_in != ""){
      mvm.scr <- add.line(mvm.scr,paste0('-wsVars "',input$ws_vars_in,'"'))
    }
    if(input$qnt_vars_in != ""){
      mvm.scr <- add.line(mvm.scr,paste0('-qVars "',input$qnt_vars_in,'"'))
    }
    if(input$qnt_vars_center != ""){
      mvm.scr <- add.line(mvm.scr,paste0('-qVarCenters "',
                                         input$qnt_vars_center,'"'))
    }
    ## GLT's
    if(input$glt_list != ""){
      glt.split <- unlist(strsplit(input$glt_list,split='\n'))
      mvm.scr <- add.line(mvm.scr,paste('-num_glt',length(glt.split)/2))
      for(g in 1:length(glt.split)){
        glt.line <- substr(glt.split[g],1,nchar(glt.split[g])-2)
        mvm.scr <- add.line(mvm.scr,glt.line)
      }
    }
    ## end with data table
    mvm.scr <- add.line(mvm.scr,'-dataTable')
    mvm.scr <- add.line(mvm.scr,paste(names(data.str),collapse=" "))
    for(i in 1:(nrow(data.str)-1)){
      d.tab.line <- paste(data.str[i,],collapse=" ")
      mvm.scr <- add.line(mvm.scr,d.tab.line)
    }
    d.tab.line <- paste(data.str[nrow(data.str),],collapse=" ")
    mvm.scr <- paste(mvm.scr,d.tab.line,sep="\n")
    return(mvm.scr)

  })   ## end MVM script

  ## download the script give run instructions
  observeEvent(input$downloadScript,{

    ## check if file has a path
    if(!grepl('\\/',input$script_name)){
      ScriptOut <- paste0(cur.dir,"/",input$script_name)
    } else {
      ScriptOut <- input$script_name
    }

    ## check for overwrite
    if(file.exists(ScriptOut) & !input$OverwriteScript){
      showModal(modalDialog(title="File Exists!",
                            paste0(normalizePath(ScriptOut),
                                   " is taken!! Choose a different name!")
      ))
    } else {
      ## write out file and show confirmation
      tryCatch(
        {fileConn <- file(ScriptOut)
        writeLines(mvmScript(),fileConn)
        close(fileConn)},
        error=function(e) NA)
      if(file.exists(ScriptOut)){
        showModal(modalDialog(title = "Script Downloaded",
                              paste0("Script saved as ",
                                     normalizePath(ScriptOut))
        ))
      } else {
        showModal(modalDialog(title = "Script Download Failed!",
                              paste0("Script NOT saved as ",
                                     normalizePath(ScriptOut))
        ))
      }
    }
  })   ## end download script

  output$exec_script <- renderText({
    paste0('tcsh ',input$script_name)
  })

  ############################################
  ## GLTs

  ## get the variables from the model
  glt_get_vars <- observeEvent(
    c(input$ws_vars_in,input$qnt_vars_in,
      input$model_in,input$load_data),
    {
      ## force the static model to update
      junk <- calcModel()

      ## split by space to make sure there is something
      mod.parts <- unlist(strsplit(model.static,split=' '))
      if(mod.parts[1] != "value"){
        print("Model Fail! Try again...")
      } else {

        ## split the error terms and the main terms
        err.term <- unlist(strsplit(model.static,split="Error\\(Subj/"))[2]
        err.term <- substr(err.term,1,nchar(err.term)-1)
        err.term <- unlist(tstrsplit(err.term,'[*+-/]'))
        main.term <- unlist(tstrsplit(mod.parts[3],'[*+-/]'))

        ## combine and take the unique ones
        all.terms <- c(main.term,err.term)
        all.terms <- all.terms[!is.na(all.terms)]
        all.terms <- unique(all.terms)

        ## update list
        updateSelectInput(session,'glt_vars',choices=all.terms)
      }
    })   ## end get glt vars

  ## return the possible levels
  glt_get_lvls <- reactive({

    if(input$glt_vars == ""){ return("") }
    vox.df <- data.df
    ## change non specified qVars to factors
    if(!is.na(qntVar)){
      vox.df <- num2fact_fun(input$qnt_vars_in,qntVar,vox.df)
    }

    if(is.factor(vox.df[,input$glt_vars])){
      return(levels(vox.df[,input$glt_vars]))
    } else {
      return("Quantitative, enter one number.")
    }
  })   ## end glt_get_lvls
  observeEvent(input$glt_vars,{
    updateSelectInput(session,'glt_lvl',choices=glt_get_lvls())
  })

  ## make the glt code, assumes that there are no errors
  observeEvent(input$add_glt_var,{

    ## check for badness
    bad.out <- gltCheckFun(input$glt_label,input$glt_lvl,input$glt_weights,
                           input$glt_list)

    if(length(bad.out) == 0){
      ## get the current code and add a space at the end if not there
      glt.current <- input$glt_code
      code.last.char <- substr(glt.current,nchar(glt.current)-1,
                               nchar(glt.current))
      if(code.last.char != " "){ glt.current <- paste0(glt.current," ") }

      ## get new var level and weights
      glt.lvl <- input$glt_lvl
      glt.wt <- as.numeric(unlist(tstrsplit(input$glt_weights,',')))

      if(glt.lvl == "Quantitative, enter one number."){
        glt.lvl.wt <- glt.wt
      } else {
        glt.lvl.wt <- paste(glt.wt,glt.lvl,sep='*')
        glt.lvl.wt <- paste(glt.lvl.wt,collapse=' ')
      }

      ## paste all of the strings and output
      new.glt.code <- paste0(glt.current,input$glt_vars,' : ',glt.lvl.wt,' ')
      updateTextInput(session,'glt_code',value=new.glt.code)
      glt.good <<- FALSE
    }
  })

  ## verify GLTs
  observe({
    bad.out <- gltCheckFun(input$glt_label,input$glt_lvl,input$glt_weights,
                           input$glt_list)
    output$bad_glt <- renderTable({bad.out},colnames=FALSE)
  })

  ## test the current glt code
  observeEvent(input$test_glt,{

    ## clear old and check for badness
    output$glt_test_out <- renderTable("",colnames=FALSE)
    bad.out <- gltCheckFun(input$glt_label,input$glt_lvl,input$glt_weights,
                           input$glt_list)
    if(length(bad.out) > 0){
      output$glt_test_out <- renderTable({"Model Fail! Try again..."},
                                         colnames=FALSE)
      glt.good <<- FALSE
    }

    ## load data and check for bad variables
    vox.df <- data_load()
    junk <- calcModel()

    ## no data or other badness
    if(is.null(vox.df)){ return(cat("")) }
    if(is.null(aov.obj)){ return(cat("")) }

    ## generate names
    vox.name.list <- vox.file.name(input$vox_roi,out.dir,coord.mat,
                                   input$srad_num)
    vox.file <- vox.name.list$vox.file
    vox.coord <- vox.name.list$vox.coord
    names(vox.df)[names(vox.df)==vox.coord] <- 'value'

    ## check for qVars and center them to mean or specified
    if(input$qnt_vars_in != ""){
      qnt_var_sel <- unlist(tstrsplit(input$qnt_vars_in,','))
      if(input$qnt_vars_center != ""){
        qnt_var_cent_sel <- as.numeric(
          unlist(tstrsplit(input$qnt_vars_center,',')))
        if(length(qnt_var_cent_sel) != length(qnt_var_sel)){
          return(cat(
            paste("Number of qVars and centers do not match!")))
        }
        vox.df <- qVar_center_fun(qnt_var_sel,qnt_var_cent_sel,vox.df)
      } else {   ## center by mean
        vox.df <- qVar_center_fun(qnt_var_sel,NULL,vox.df)
      }
    }   ## end qntVars check

    ## cut off the trailing space and space separate
    code.last.char <- substr(input$glt_code,nchar(input$glt_code)-1,
                             nchar(input$glt_code))
    if(code.last.char == " "){
      cur.code <- substr(input$glt_code,1,nchar(input$glt_code)-1)
      code.list <- strsplit(cur.code,split=' ')
    } else {
      code.list <- strsplit(input$glt_code,split=' ')
    }

    ## return a list of the glt's and/or slopes
    glt.terms <- gltConstr(code.list[[1]],vox.df)

    ## check to see if the glt code is valid
    if(is.null(glt.terms)){
      output$glt_test_out <- renderTable({"Model Fail! Try again..."},
                                         colnames=FALSE)
      glt.good <<- FALSE
      return()
    }

    ## split the pieces
    test.slp <- glt.terms$slpList
    test.glt <- glt.terms$gltList
    if(length(glt.terms$slpList) == 0){ test.slp <- NULL }
    if(length(glt.terms$gltList) == 0){ test.glt <- NULL }

    ## do the actual test
    test.int <- tryCatch(
      testInteractions(aov.obj$lm,custom=test.glt,
                       idata=aov.obj$data$idata,adjustment="none",
                       slope=test.slp,pairwise=NULL),error=function(e) NA)
    if(!is.na(test.int)){
      output$glt_test_out <- renderTable({data.frame(test.int)},rownames=TRUE)
      glt.good <<- TRUE
    } else {
      output$glt_test_out <- renderTable({"Model Fail! Try again..."},
                                         colnames=FALSE)
      glt.good <<- FALSE
    }
  })   ## end test glt

  ## clear the current glt code
  observeEvent(input$clear_glt,{
    updateTextInput(session,'glt_code',value="")
    output$glt_test_out <- renderTable("",colnames=FALSE)
    updateTextInput(session,'glt_weights',value="")
    updateTextInput(session,'glt_label',value="")
  })

  ## clear the all glt code
  observeEvent(input$clear_all_glt,{
    updateTextInput(session,'glt_code',value="")
    output$glt_test_out <- renderTable("",colnames=FALSE)
    updateTextInput(session,'glt_weights',value="")
    updateTextInput(session,'glt_label',value="")
    updateTextAreaInput(session,'glt_list',value="")
  })

  ## add the full GLT label and code to the list
  observeEvent(input$add_glt,{

    if(!glt.good){
      showNotification("Test the GLT first!",type="error",
                       duration=5)
      output$glt_test_out <- renderTable("Test the GLT first!",colnames=FALSE)
      return()
    }
    ## check for badness
    bad.out <- gltCheckFun(input$glt_label,input$glt_lvl,input$glt_weights,
                           input$glt_list)

    if(input$glt_label != "" & input$glt_code != "" & length(bad.out) == 0){

      ## cut off the trailing space and starting space
      cur.code <- substr(input$glt_code,1,nchar(input$glt_code)-1)
      cur.code <- substr(cur.code,2,nchar(cur.code))
      print(paste0("f",cur.code))
      ## if first one
      if(input$glt_list == ""){
        glt.tab <- paste("-gltLabel 1",input$glt_label,'\\')
        glt.tab <- paste0(glt.tab,'\n',paste0("-gltCode  1 '",cur.code,"' \\"))
      } else {
        ## get the number of them
        glt.n <- length(unlist(strsplit(input$glt_list,split='\n')))/2+1
        new.glt.l <- paste0("-gltLabel ",glt.n," ",input$glt_label," \\")
        new.glt.c <- paste0("-gltCode  ",glt.n," '",cur.code,"' \\")
        glt.tab <- paste(input$glt_list,new.glt.l,new.glt.c,sep='\n')
      }
      ## update and clear all of the things
      updateTextAreaInput(session,'glt_list',value=glt.tab)
      updateTextInput(session,'glt_label',value="")
      updateTextInput(session,'glt_code',value="")
      updateTextInput(session,'glt_weights',value="")
      updateSelectInput(session,'glt_lvl',selected="")
      output$glt_test_out <- renderTable("",colnames=FALSE)
      glt.good <<- FALSE
    }
  })

  ## clear the GLT codes if the model changes
  observeEvent(c(input$model_in,input$ws_vars_in,
                 input$qnt_vars_in,input$qnt_vars_center),{
    updateTextAreaInput(session,'glt_list',value="")
  })

  ############################################
  ## outputs

  output$cat_var_tab <- renderTable({
    tab.out <- catVar
    len.vec <- length(tab.out)
    if(len.vec > 4){
      n.row <- ceiling(len.vec/4)
      for(i in (len.vec+1):(4*n.row)){ tab.out[i] <- " " }
      dim(tab.out) <- c(n.row,4)
      return(tab.out)
    } else if(len.vec > 0){
      return(t(tab.out))
    } else { return(" ") }
  },colnames=FALSE)

  output$qnt_var_tab <- renderTable({
    tab.out <- qntVar
    len.vec <- length(tab.out)
    if(len.vec > 4){
      n.row <- ceiling(len.vec/4)
      for(i in (len.vec+1):(4*n.row)){ tab.out[i] <- " " }
      dim(tab.out) <- c(n.row,4)
      return(tab.out)
    } else if(len.vec > 0){
      return(t(tab.out))
    } else { return(" ") }
  },colnames=FALSE)

  ## long list of options
  dataTableOptions <- list(paging=TRUE,info=FALSE,searching=FALSE,scrollX=TRUE,
                           pageLength=25)

  ## many outputs
  output$mvm_table <- renderDataTable({data_load()},options=dataTableOptions)
  output$bad_vars  <- renderTable({badVars()},colnames=FALSE)
  output$model_summary <- reactivePrint(function(){calcModel()})
  output$mvm_script <- renderText({ mvmScript() })
  output$glt_cur_mod <- renderText({
    junk <- calcModel()   ## update the model.static global (bad disco)
    as.character(model.static)
  })

  ## update the summary when you select that tab
  observeEvent(input$tabs,{
    if(input$tabs == "Data Summary"){
      output$summary_table <- renderPrint({
        vox.df <- data_load()
        if(is.null(vox.df)){ return(cat("")) }

        ## change non specified qVars to factors
        if(input$qnt_vars_in != ""){
          qVar.spec <- unlist(tstrsplit(input$qnt_vars_in,','))
          qVar.2.fac <- setdiff(qntVar,qVar.spec)
          vox.df[,qVar.2.fac] <- data.frame(apply(vox.df[qVar.2.fac],2,
                                                  as.factor))
        }
        summary(vox.df[,1:(length(vox.df)-1)])
      })
    }
  })   ## end summary tab

  ## help page embed
  output$mvm_help <- renderUI({
    tags$iframe(src="https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dMVM.html",
                height=800,width="100%")
  })

})   ## end server

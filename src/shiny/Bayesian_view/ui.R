## top ##################################
## 01/2022 Justin Rajendra
## Gang RBA 
## UI

header <- dashboardHeader(title=paste("RBA"),titleWidth=290)

## sidebar top #####################
sidebar <- dashboardSidebar(
  width=290,
  sidebarMenu(
    id="tabs",
    
    ## main #############################################
    menuItem(
      "Data",icon=icon('database'),selected=TRUE,startExpanded=TRUE,
      
      selectInput('fileSel',"Data File",file.list,multiple=FALSE),
      
      selectInput('roiSel','Select ROIs',NULL,NULL,multiple=TRUE),
  
      checkboxInput('ordered','Ordered?',TRUE),
      
      sliderInput('plotRange','Range:',min=-0.5,max=0.5,value=c(-0.05,0.05),
                  step=0.01),

      br()
    ),   ## end subject and date
    
    ## decorations menu item #######################
    menuItem(
      "Decorations",icon=icon('palette'),selected=FALSE,startExpanded=FALSE,
      
      selectInput('colPal','Color Pallet',col.list),
      textInput('plotTitle','Title','Gang Plot'),
      sliderInput('ROI_label_size','ROI Label Size',min=3,max=50,value=10),
      sliderInput('P_label_size','P Label Size',min=3,max=50,value=10),
      sliderInput('x_axis_size','X Axis Size',min=3,max=50,value=10),
      
      
      # sliderInput('title_size','Title Size',min=3,max=50,value=10),
      
      br()
    ),
 
    ## output menu item #######################
    menuItem(
      "Output",icon=icon('image'),selected=FALSE,startExpanded=FALSE,
      
      selectInput('outputFormat','File Format',c('png','pdf')),
      sliderInput('outputHeight','Height (in)',min=3,max=20,value=5),
      sliderInput('outputWeight','Weight (in)',min=3,max=20,value=5),
      
      
      br()
    ),
    
    br()
  )   ## end sidebar menu                  
)   ## end sidebar

## body top #######################################################################
body <-  dashboardBody(
  
  tabsetPanel(
    id='tabs',
    tabPanel(
      'Plots',icon=icon("chart-line"),
      br(),
      fluidRow(
        plotOutput('gangPlot')

      ),
      br()
    ),
    # tabPanel(
    #   'Meta Variable',icon=icon("asterisk"),br(),
    #   
    #   # fluidRow(column(
    #   #   width=12,
    #   #   textInput('model_label','Model Label (no spaces and must 
    #   #             start with a letter):',
    #   #             value="",width="100%"))
    #   # ),br(),
    #   # 
    #   # fluidRow(column(
    #   #   width=12,
    #   #   textInput('model_in','Specify Model (no spaces):',
    #   #             value="",width="100%"))
    #   # ),br(),
    #   # 
    #   # fluidRow(
    #   #   box(title='Invalid Variable Names:',width=6,
    #   #       tableOutput('bad_vars'),
    #   #       tags$head(tags$style("#bad_vars{color: red;}"))
    #   #   )
    #   # ),br()
    #   
    #   
    # )
    br()
  )
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)


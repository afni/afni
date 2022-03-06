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
      "Main",icon=icon('user'),selected=TRUE,startExpanded=TRUE,
      
      selectInput('fileSel',"Data File",file.list,multiple=FALSE),
      
      ## 
      selectInput('roiSel','Select ROIs',NULL,NULL,multiple=TRUE),
  
      # checkboxInput('ordered','Ordered?',TRUE),
      
      sliderInput('plotRange','Range:',min=-0.5,max=0.5,value=c(-0.05,0.05),
                  step=0.01),
      
      selectInput('colPal','Color Pallet',col.list),
      
      textInput('plotTitle','Title','Gang Plot'),
      
      sliderInput('ROI_label_size','ROI Label Size',min=3,max=50,value=10),
      
      sliderInput('P_label_size','P Label Size',min=3,max=50,value=10),
      sliderInput('x_axis_size','X Axis Size',min=3,max=50,value=10),
      
      # sliderInput('plotHeight','Height (in)',min=3,max=20,value=5),
      
      
      # sliderInput('title_size','Title Size',min=3,max=50,value=10),
      
      
      # ),
      # conditionalPanel(
      #   'input.dateSel == "Phase"',
      #   selectInput('phaseSel',"Phase",phase.list,
      #               selected=phase.list[!(phase.list %in% c("Pre"))],
      #               multiple=TRUE)
      # ),
      # 
      # checkboxInput('showPhase',"Show Phases",FALSE),
      # 
      # ## what to plot
      # selectInput('plotType','Plot Type',
      #             c("Time","Home Data Box Plots"="Box"),selected="Time"),
      br()
    ),   ## end subject and date
    
    ## home scales menu item #######################
    # menuItem(
    #   "Home Scales",icon=icon('home'),selected=FALSE,startExpanded=FALSE,
    #   
    #   ## choices for time #########################
    #   conditionalPanel(
    #     'input.plotType == "Time"',
    #     
    #     checkboxInput('showHome','Plot Home Scales',value=TRUE),
    #     selectInput('scoreSel','Measures',score.list,multiple=TRUE),
    #     
    #     selectInput('InterviewType','Time of Day',
    #                 c('Everything'='Chronological','Morning','Evening',
    #                   'Life Event'='LifeEvent','CATMH'),multiple=TRUE,
    #                 selected=c('Morning','Evening')),
    #     
    #     checkboxGroupInput('LineType','What to Plot',
    #                        c('Points'='Points',
    #                          'LOESS'='LOESS',
    #                          'Moving Average'='MovAve'),
    #                        selected='Points',inline=FALSE),
    #     
    #     conditionalPanel("input.LineType.includes('LOESS')",
    #                      sliderInput('span','LOESS Span',
    #                                  value=0.2,min=0.1,max=1,step=0.1)
    #     ),
    #     conditionalPanel("input.LineType.includes('MovAve')",
    #                      sliderInput('DaysAve','Moving Average Days',
    #                                  value=5,min=3,max=30,step=1)
    #     ),
    #     checkboxInput('revScores','Reverse Left Y Axis Scale')
    #   ),  ## end time type
    # ),
    # 
    # ## lab scales #################################
    # menuItem(
    #   "Lab Scales",icon=icon('flask'),selected=FALSE,startExpanded=FALSE,
    #   
    #   checkboxInput('showLab','Plot Lab Scales',value=FALSE),
    #   selectInput('rateSel','Lab Measures',rate.list,multiple=TRUE,
    #               selected=rate.list[1]),
    #   checkboxInput('revScoresLab','Reverse Lab Y Axis Scale'),
    #   checkboxInput('fixRangeLab','Keep Max Range',FALSE),
    #   checkboxInput('useMeta','Use Meta Variable',FALSE),
    #   
    #   
    #   br(),br()
    # )
    
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


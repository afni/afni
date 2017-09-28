#####################################
## 09/2017 Justin Rajendra
## Cluster Explorer
## UI

#########################################################################
header <- dashboardHeader(title=paste("Cluster Explorer"),titleWidth=275)
sidebar <- dashboardSidebar(width=275,sidebarMenu(

  menuItem("Clusters",tabName="clusters",icon=icon("area-chart"),
           selected=TRUE,startExpanded=TRUE,

           ## Mean of clusters or peak
           radioButtons('mean_peak','Plot the peak voxel or cluster mean?',
                        c('Peak','Mean'),inline=TRUE),

           ## list of clusters from the read function
           selectInput('clusters',paste('Atlas:',atlas.name),NULL,NULL),

           ## list of groups
           selectInput('var_sel','Between subjects factor:',choices=catVars,
                       selected=catVars[1],multiple=TRUE),

           ## wsVars and qVars
           # uiOutput('bsVar_input'),
           uiOutput('wsVar_input'),
           uiOutput('qVars_input'),

           ## show original stat output?
           radioButtons('orig_stat','Which model for summary?',
                        c('Original Model'='orig','Plotted Model'='plot'),
                        inline=TRUE),

           ## plot options
           radioButtons('split_bs_ws','Factor by:',c(NA),inline=TRUE),
           radioButtons('box_scatter','Plot type:',c('Box','Interaction'),
                        inline=TRUE),

           ## scatter only
           conditionalPanel('input.box_scatter == "Scatter"',
                            checkboxInput('OverPlot',
                                          'Plot separately for each level?'),
                            checkboxInput('qVars_center','Plot centered?')
           ),br()


  ),   ## end clusters tab
  menuItem("Plot Settings",tabName="settings",icon=icon("gears"),
           selected=FALSE,startExpanded=FALSE,

           ## min and max
           checkboxInput('fixed_range','Lock data range?'),
           conditionalPanel('input.fixed_range',
                            sliderInput('custom_range','Min:Max',sep='',
                                        step=0.001,value=c(-1,1),
                                        min=NULL,max=NULL) ),

           ## only show for box
           conditionalPanel('input.box_scatter == "Box"',
                            radioButtons('box_points','Add subject data points?',
                                         c('all','outliers only'='outliers'),
                                         inline=TRUE),
                            radioButtons('box_mean','Add mean or sd?',
                                         inline=TRUE,
                                         c('no'='FALSE','mean'='TRUE',
                                           'mean and sd'='sd'))
           ),
           ## only show for scatters:
           conditionalPanel('input.box_scatter != "Box"',
                            numericInput('marker_size','Marker size:',value=10,
                                         min=2,step=1),
                            sliderInput('marker_opacity','Marker opacity:',0.8,
                                         min=0.05,max=1,step=0.05),
                            numericInput('line_w','Line width:',value=2,
                                         min=0.5,step=0.5)
           ),
           conditionalPanel('input.box_scatter == "Interaction"',
                            sliderInput('jit_sel','Jitter:',min=0,max=1,
                                        value=0.3,step=0.1),
                            radioButtons('error_y_sel','Show error bars:',
                                          choices=c('no','SD','SE'),
                                          inline=TRUE)
           ),
           ## show color pallet, if ttest add red/blue via update
           selectInput('col_pal','Color pallete:',
                       choices=c('Dark2','Set1','Set2','Set3','Accent','Paired',
                                 'Pastel1','Pastel2')),
           br()
  )
)   ## end side bar menu
)   ## end sidbar

#########################################################################
body <-  dashboardBody(
  tabsetPanel(
    tabPanel('Main',icon=icon("beer"),br(),
             fluidRow(
               column(width=12,plotlyOutput('clust_plot_orig',height="100%"))
             ),br(),
             fluidRow(
               h4('Model Summary',align='center'),
               column(width=12,verbatimTextOutput('clust_stat_orig'))
             ),br(),
             fluidRow(
               h4("Descriptive Statistics for Cluster",align='center'),
               column(width=12,dataTableOutput('clust_stat_desc'))
             )
    ),   ## end main panel
    #################################
    tabPanel('Data Table',icon=icon("table"),br(),
             fluidRow(column(width=12,br(),
                             dataTableOutput('data_table'),br()) )
    ),   ## end full data table panel
    #################################
    tabPanel('Data Summary',icon=icon("calculator"),br(),
             fluidRow(column(width=12,br(),
                             verbatimTextOutput('summary_table'),br()) )
    ),   ## end data table summary panel
    #################################
    tabPanel('Inputs',icon=icon("inbox"),br(),
             fluidRow(column(width=12,br(),
                             tableOutput('stat_info_table'),br()) )
    )   ## end data table summary panel
  )   ## end tabset panel
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)

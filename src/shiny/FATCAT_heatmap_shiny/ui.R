#####################################
## 06/08/2017 Justin Rajendra
## 3dNetCorr heatmap
## UI

## packages
# library(shiny)
# library(shinydashboard)
# library(plotly)
# library(colourpicker)

#########################################################################
header <- dashboardHeader(title='FATCAT Heatmap',titleWidth=300)

#########################################################################
sidebar <- dashboardSidebar(width=300,sidebarMenu(
  tags$style(type="text/css", ".tab-content { overflow: visible !important; }"),
  tags$head(
    tags$style(".learn_link{margin-left:15px;}")),
  menuItem("FATCAT",tabName="stuff",icon=icon("cogs"),selected=TRUE,
           startExpanded=TRUE,

           selectInput('net_file','File:',file.list),
           selectInput('stat_sel','Stat:',NULL),
           radioButtons('tri_sel','Matrix:',inline=TRUE,
                        c('Full','Upper','Lower')),

           fluidRow(
             column(width=4,
                    checkboxInput('thresh_yn','Threshold')),
             column(width=8,
                    checkboxInput('zero_yn','Remove zeros'))
           ),
           conditionalPanel('input.thresh_yn',
                            fluidRow(
                              column(width=6,
                                     numericInput('thresh_min',
                                                  'Threshold Lower:',
                                                  -1,step=0.1)),
                              column(width=6,
                                     numericInput('thresh_max',
                                                  'Threshold Upper:',
                                                  1,step=0.1))
                            ) ),

           selectInput('h_clust','Cluster linkage:',
                       c('none','complete','single','average','median',
                         'centroid','mcquitty')),

           conditionalPanel('input.h_clust != "none"',
                            h5('Clustering is calculated FIRST (no NAs!)',
                               class='learn_link'),
                            htmlOutput('hclust_link',class='learn_link'),
                            selectInput('dist_meth','Distance Method:',
                                        c("euclidean","maximum","manhattan",
                                          "canberra","binary","minkowski")),
                            htmlOutput('dist_link',class='learn_link')
           ),

           checkboxInput('hist_yn','Show histogram below'),
           conditionalPanel('input.hist_yn',
                            h5('Histogram is after all thresholding.',
                               class='learn_link')),br()

  ), ## end files tab
  menuItem("ROIs",tabName="ROIs",icon=icon("code-fork"),
           selectInput('rois','Select ROIs:',multiple=TRUE,choices=NULL,
                       selected=NULL,selectize=TRUE),br()
  ),  ## end ROIs tab

  menuItem("Colors",tabName="colors",icon=icon("edit"),
           fluidRow(
             column(width=6,numericInput('range_min',
                                         'Color Min:',-1,
                                         step=0.1)),
             column(width=6,numericInput('range_max',
                                         'Color Max:',1,
                                         step=0.1))
           ),
           colourInput('col1','Lower',value='blue'),
           colourInput('col2','Middle',value='white'),
           colourInput('col3','Upper',value='red')
  ),   ## end colors
  menuItem("Create PNG",tabName="png",icon=icon("file-image-o"),
           numericInput('static_dpi','DPI (min 72):',value=200,min=72),
           downloadButton('downloadPlot', 'Download Plot',
                          class='d_button'),
           tags$head(tags$style(".d_button#downloadPlot{
                                margin-left: 12px;}")),br(),
           conditionalPanel('input.h_clust != "none"',
                            checkboxInput('dendro','Show dendrogram on png?',
                                          value=TRUE)
           ),
           br()
  ),  ## end png tab


  h6("If you can't scroll down, resize the browser window.",
     style="margin-left: 12px; margin-bottom: 0px;")
)   ## end sidebar menu
)   ## end sidbar

#########################################################################
body <-  dashboardBody(
  fluidRow(
    plotlyOutput('cor_heatmap_plot',height=680,width=680),

    conditionalPanel('input.hist_yn',br(),br(),
                     plotlyOutput('cor_hist_plot',height=680,width=680)
    )
  )
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)

#####################################
## 11/2017 Justin Rajendra
## FATCAT matrix plot
## UI

#########################################################################
header <- dashboardHeader(title='FATCAT Matrix Plot',titleWidth=300)

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
                                                  'Low Pass:',
                                                  -1,step=0.1)),
                              column(width=6,
                                     numericInput('thresh_max',
                                                  'High Pass:',
                                                  1,step=0.1))
                            ) ),

           selectInput('h_clust','Cluster linkage (before thresholding):',
                       c('none','complete','single','average','median',
                         'centroid','mcquitty')),

           conditionalPanel('input.h_clust != "none"',
                            htmlOutput('hclust_link',class='learn_link'),
                            selectInput('dist_meth','Distance Method:',
                                        c("euclidean","maximum","manhattan",
                                          "canberra","binary","minkowski")),
                            htmlOutput('dist_link',class='learn_link')
           ),

           checkboxInput('hist_yn','Show histogram below',value=TRUE),
           conditionalPanel('input.hist_yn',
                            h5('Histogram is after all thresholding.',
                               class='learn_link')),br()

  ), ## end files tab
  menuItem("ROIs",tabName="ROIs",icon=icon("code-fork"),
           selectInput('rois','Select ROIs:',multiple=TRUE,choices=NULL,
                       selected=NULL,selectize=TRUE),br()
  ),  ## end ROIs tab

  menuItem("Colors",tabName="colors",icon=icon("edit"),br(),
           h5("Color min/max also thresholds the data!",
              class='learn_link'),
           fluidRow(
             column(width=6,numericInput('range_min',
                                         'Color Thresh Min:',-1,
                                         step=0.1)),
             column(width=6,numericInput('range_max',
                                         'Color Thresh Max:',1,
                                         step=0.1))
           ),
           colourInput('col1','Lower',allowTransparent=TRUE,
                       value=rgb(0,0,1,alpha=0.4)),
           colourInput('col2','Middle',allowTransparent=TRUE,
                       value=rgb(1,1,1,alpha=0.7)),
           colourInput('col3','Upper',allowTransparent=TRUE,
                       value=rgb(1,0,0,alpha=0.4)),
           h5("Transparency only applies to the Circos.",
              class='learn_link'),
           h5("Circos only takes the lower and upper colors",
              class='learn_link'),br()
  ),   ## end colors
  menuItem("Heatmap PNG",tabName="png",icon=icon("file-image-o"),
           numericInput('static_dpi','DPI (min 72):',value=200,min=72),
           downloadButton('downloadPlot', 'Download Heatmap',
                          class='d_button'),
           tags$head(tags$style(".d_button#downloadPlot{margin-left: 12px;} 
                                .d_button#downloadPlot{color:black;}")),br(),
           conditionalPanel('input.h_clust != "none"',
                            checkboxInput('dendro','Show dendrogram on png?',
                                          value=TRUE)
           ),br()

  ),  ## end png tab
  menuItem("Circos PNG",tabName="circos",icon=icon("times-circle-o"),br(),
           h5("Longer labels may need larger image size.",
              class='learn_link'),
           sliderInput('circos_dim','Image size (inches):',value=8,
                        min=4,step=4,max=32),
           numericInput('circos_dpi','DPI (min 72):',value=200,min=72),
           downloadButton('downloadCircos', 'Download Circos',
                          class='d_button'),
           tags$head(tags$style(".d_button#downloadCircos{margin-left: 12px;} 
                                .d_button#downloadCircos{color:black;}")),
           br(),br()
  ),

  h6("If you can't scroll down, resize the browser window.",
     style="margin-left: 12px; margin-bottom: 0px;")
)   ## end sidebar menu
)   ## end sidbar

#########################################################################
body <-  dashboardBody(
  fluidRow(
    plotlyOutput('cor_heatmap_plot',height=700,width=700),
    conditionalPanel('input.hist_yn',br(),br(),
                     plotlyOutput('cor_hist_plot',height=700,width=700)
    )
  )
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)
